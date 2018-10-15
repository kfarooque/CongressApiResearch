#!/usr/bin/env python
# -*- utf-8 -*-

import os
import re
import json
import requests
import datetime as dt
import numpy as np
import pandas as pd
from bs4 import BeautifulSoup


# GENERAL #


def extract_value_from_file(file, variable):
    """
    Extract value from a file of variable definitions.
    :param file: plaintext file with variables defined on each line as "variable = value"
    :param variable: name of variable whose value to extract
    :return: string with variable value
    """
    regex = f"(^|\n)({variable} *= *)(.*)($|\n)"
    with open(file, "r", encoding='utf8') as file:
        value = re.search(regex, file.read()).group(3)
    return value


def fix_json_escapes(infile, outfile):
    """
    Fix specific escape-related errors in JSON file and write out new file. Fixes the following:
        any \ without something being escaped, any unescaped ".
    :param infile: string, path of JSON file to read in and fix
    :param outfile: string, path of JSON file to write out (can be same as infile)
    :return: none (creates new file)
    """
    newlines = []
    with open(infile, "r", encoding='utf8') as file:
        for line in file:
            content = re.sub(r'[^:]*:(.*)', r'\1', line)
            content = content.strip()
            content = re.sub(r',($|\n)', '', content)
            content = content.strip()
            content = re.sub(r'^[\"\']|[\"\']$', '', content)
            content = content.strip()
            if len(content) <= 1:
                replacement = content
            elif content[0] + content[-1] not in ['()', '{}', '[]']:
                replacement = content
                # fix errant \s
                replacement = re.sub(r'\\ ', ' ', replacement)
                # fix unescaped "s and 's
                replacement = re.sub(r'([^\\])\"', r'\1\\"', replacement)
            else:
                replacement = content
            newline = line.replace(content, replacement)
            newlines.append(newline)
    with open(outfile, "w", encoding='utf8') as file:
        file.write(''.join(newlines))
    return None


def search_directory_files(scan, filepattern):
    """
    Scan a directory and its subdirectories for files with the given pattern.
    :param scan: string, directory to scan for files
    :param filepattern: r-string, regex pattern of file to scan (remember to escape your \. - e.g., r'file\.txt')
    :return: list of full paths where the file was found
    """
    files_found = []
    for root, directories, filenames in os.walk(scan):
        for filename in filenames:
            if re.search(filepattern, filename) is not None:
                files_found.append(os.path.join(root, filename))
    files_found = [re.sub(r'\\', '/', file) for file in files_found]
    return files_found


def import_multiple_files(files, sep='\t'):
    """
    Build combined dataframe of results files.
    :param files: string or list of strings, full path of text file(s) to read in
    :param sep: string, separator character to use for delimiters
    :return: data frame with all files read in, plus extra field 'sourcefile' that is the file that was read in
    """
    if type(files) == str:
        files = [files]
    df_all = pd.DataFrame()
    for file in files:
        if os.path.isfile(file):
            with open(file, 'r', encoding='utf8') as fileopen:
                df = pd.read_table(fileopen, sep=sep)
            df['sourcefile'] = file
        else:
            df = pd.DataFrame({'sourcefile': file}, index=[0])
        df_all = df_all.append(df)
    return df_all


# DOWNLOADING FROM API #


def format_congress_api_query(query=None):
    """
    Format query for requests for the ProPublica Congress API.
    :param query: string or list of strings, containing search query/queries
                  if string, will be treated as one search term,
                             e.g., "tax shelter" searches for the entire phrase
                  if list, each item will be treated as a separate one-or-multi-word search
                           e.g., ["tax shelter", "loophole"] searches for "tax shelter" or "loophole"
    :return: string, with query to use for searches
    """
    if query is not None:
        if type(query) == list:
            api_query = ' '.join(['"' + item + '"' for item in query])
        elif type(query) == str:
            if query[0] == "'" and query[-1] == "'":
                api_query = re.sub("^\'|\'$", '"', query)
            elif query[0] == '"' and query[-1] == '"':
                api_query = query
            else:
                api_query = '"' + query + '"'
        else:
            api_query = '"' + str(query) + '"'
    else:
        api_query = None
    return api_query


def define_congress_api_url(path="bills/latest.json", query=None, offset=0, version="v1"):
    """
    Define URL for accessing the ProPublica Congress API.
    :param path: string, path after version and up to and including .json part, for details see
        https://projects.propublica.org/api-docs/congress-api/.
    :param query: string, optional query parameters, use quotes for multi-word strings
    :param offset: integer, offset for results (default 0)
    :param version: string, version of API (currently v1)
    :return: string, URL for Congress API request
    """
    base = "https://api.propublica.org/congress"
    if re.search("\.json\?", path):
        path = path + "&"
    elif re.search("\.json$", path):
        path = path + "?"
    else:
        path = path + ".json?"
    api_url = base + "/" + version + "/" + path
    if query is not None:
        api_url = api_url + 'query=' + query + "&"
    api_url = api_url + "offset=" + str(offset)
    return api_url


def download_congress_api_results(api_key, api_url, max_results=20, iterate_by=20, tempfile="~temp_api_results.json"):
    """
    Submit request and download results from ProPublica Congress API request.
    Uses function: fix_json_escapes
    :param api_key: string, API key
    :param api_url: string, request URL in full (for details see https://projects.propublica.org/api-docs/congress-api/)
                if there is an offset=X parameter, then this is overwritten to enable request to loop through
    :param max_results: integer, maximum number of results (rounded up to next multiple of 20)
    :param iterate_by: integer, number of results to iterate by between each request (20 for most requests)
    :param tempfile: string, path and file to temporarily store results from each request
    :return: tuple with status message, copyright message, and data (as a list of dictionaries),
             if status is an error message, then the tempfile is also retained for debugging.
    """
    loops = int(np.ceil(max_results/iterate_by))
    number = 0
    data = []
    temp_status = ""
    temp_copyright = ""
    for i in range(0, loops):
        offset_i = int(np.floor(number/iterate_by) * iterate_by)
        url_i = re.sub("offset=\\d+$", "offset=" + str(offset_i), api_url)
        result_i = requests.request('GET', url_i, headers={'X-API-Key': api_key})
        with open(tempfile, "w", encoding='utf8') as output:
            output.write(result_i.text)
            # json.dump(result_i.json(), output, indent=4)
        fix_json_escapes(tempfile, tempfile)
        with open(tempfile, "r", encoding='utf8') as file:
            temp_dict_full = json.loads(file.read())
            temp_status = temp_dict_full['status']
            temp_copyright = temp_dict_full['copyright']
            temp_results = temp_dict_full['results']
            if type(temp_results) == list:
                temp_results = temp_results[0]
            temp_results_keys = list(temp_results.keys())
            temp_results_types = [type(temp_results[x]) for x in temp_results_keys]
            if 'num_results' in temp_results_keys:
                temp_number = temp_results['num_results']
            else:
                temp_number = 0
            if list in temp_results_types:
                temp_results_datakey = temp_results_keys[temp_results_types.index(list)]
                temp_data = temp_results[temp_results_datakey]
            else:
                temp_data = []
        if temp_status == 'OK':
            number = number + temp_number
            data = data + temp_data
        else:
            break
        if temp_number < iterate_by:
            break
    status = temp_status
    copyright = temp_copyright
    if status == 'OK':
        os.remove(tempfile)
    return status, copyright, data


# OUTPUT API RESULTS #


def define_api_output_path(root, endpoint, query=None, add_date=False, create_path=True):
    """
    Define output path based on structure of initial API request.
    :param root: string, root path for output
    :param endpoint: string, endpoint used in request (path after version and up to and including .json part)
    :param query: string, optional query parameters used in request
    :param add_date: boolean, whether to add the current date (as YYYYMMDD) as a final subfolder
    :param create_path: boolean, whether to also create the output path if it does not already exist
    :return: string, with full path for output and ending in a / (does not include filename part)
    """
    endpoint_formatted = re.sub('\.json$', '', endpoint)
    number_search = re.search('^(\d+)\/', endpoint)
    if number_search:
        number = number_search.group(1)
        endpoint_formatted = re.sub('^(\d+)\/', '', endpoint_formatted) + "/" + number
    endpoint_formatted = re.sub("[\"\'\!\$\%\&\(\)\-\{\}\[\]\:\;\<\>\,\.\?]", "", endpoint_formatted)
    endpoint_formatted = re.sub("[ \\\/\|\*\+\^@#]", "_", endpoint_formatted).lower()
    if query is not None:
        query_formatted = re.sub("[\"\'\!\$\%\&\(\)\-\{\}\[\]\:\;\<\>\,\.\?]", "", query)
        query_formatted = re.sub("[ \\\/\|\*\+\^@#]", "_", query_formatted).lower()
    else:
        query_formatted = ""
    if add_date:
        date_formatted = dt.datetime.today().strftime("%Y%m%d")
    else:
        date_formatted = ""
    full_path = root + "/" + endpoint_formatted + "/" + query_formatted + "/" + date_formatted + "/"
    full_path = re.sub("\/+", "/", full_path)
    if create_path:
        os.makedirs(full_path, exist_ok=True)
    return full_path


def output_api_data_to_json(api_data, outfile):
    """
    Output API data (list of dictionaries) to JSON file.
    :param api_data: list of dictionaries, from API results
    :param outfile: string, path and file to store results
    :return: None, results written to file
    """
    with open(outfile, "w", encoding='utf8') as file:
        json.dump(api_data, file, indent=4)
    return None


def output_api_data_to_txt(api_data, outfile, sep=','):
    """
    Output API data (dataframe or list of dictionaries) to text file formatted from a dataframe
    :param api_data: dataframe or list of dictionaries, from API results
    :param outfile: string, path and file to store results
    :param sep: string, separator to use between fields
    :return: None, results written to file
    """
    out_dataframe = pd.DataFrame(api_data)
    with open(outfile, "w", encoding='utf8') as file:
        out_dataframe.to_csv(file, sep, header=True, index=False, encoding="utf-8")
    return None


def define_api_result_message(results, api_url):
    """
    Define message with Congress API results summary.
    :param results: tuple with status message, copyright message, and data (output of download_congress_api_results())
    :param api_url: string, request URL in full (output of define_congress_api_url())
    :return: list of strings with API results message
    """
    out_message = [
        "ProPublica Congress API Results",
        results[1],
        "Time: " + dt.datetime.today().strftime("%Y-%m-%d at %H:%M"),
        "Results: " + "Status=" + results[0] + " Count=" + str(len(results[2])),
        "Request URL: " + api_url
    ]
    return out_message


# DOWNLOADING FROM OTHER URLS #


def download_from_url_govtrack_bill_text(url):
    """
    Download bill text from a govtrack URL.
    :param url: string, URL from which to download bill text
    :return: string with full text of bill taken from govtrack
    """
    try:
        html_request = requests.get(url)
        html_soup = BeautifulSoup(html_request.text, features="html.parser")
        html_main = html_soup.find('div', {'id': 'main_text_content'})
        if html_main is not None:
            html_text = html_main.find_all(text=True)
            html_text_combined = ''.join(html_text)
            html_text_combined = re.sub(r'\n+', '\n', html_text_combined)
            html_text_combined = re.sub(r' +', ' ', html_text_combined)
            html_text_combined = re.sub(r'\t', '    ', html_text_combined)
            html_text_combined = re.sub(r'^[\n\t ]+|[\n\t ]+$', '', html_text_combined)
            html_text_combined = re.sub(r'\"', '\'', html_text_combined)
        else:
            html_text_combined = '[ERROR] Could not obtain text.'
            print('ERROR WHEN TRYING TO OBTAIN TEXT IN: ' + url)
    except requests.exceptions.RequestException as e:
        html_text_combined = '[ERROR] ' + str(e)
        print('ERROR WHEN TRYING TO REQUEST FROM: ' + url)
        print(e)
    return html_text_combined
