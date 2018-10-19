#!/usr/bin/env python
# -*- utf-8 -*-

import config as c
import functions as f
import os
import re

# PRINT PARAMETERS #

print("Parameters:")
print("bills_path = " + c.bills_path)
print("existing bills " + ("will" if c.bills_overwrite else "will not ") + "be replaced")
print("-" * 64)

# LOAD FILES #

files_results = f.search_directory_files(c.bills_path, r'results\.txt$')
df_results_headers = f.scan_file_headers(files_results, seps='\t')
check_bills = ['bill_id' in headers.split('\t') for headers in df_results_headers['headers']]
check_url = ['govtrack_url' in headers.split('\t') for headers in df_results_headers['headers']]
files_results_use = [file for file, bill, url in zip(files_results, check_bills, check_url) if bill and url]

df_results = f.import_multiple_files(files_results_use)
df_results['bill_url'] = [url + '/text' for url in df_results['govtrack_url']]
df_results = df_results[['sourcefile', 'bill_id', 'bill_url']]
df_results = df_results.drop_duplicates(['sourcefile', 'bill_id'])

# DOWNLOAD FILES #

for sourcefile in df_results['sourcefile'].unique():
    outpath = re.sub(r'results\.txt$', 'bills/', sourcefile)
    outindex = outpath + "_index_.txt"
    df_bills = df_results.loc[df_results['sourcefile'] == sourcefile, ['bill_id', 'bill_url']]
    df_bills['filename'] = [re.sub('[^A-Za-z0-9\\-]', '', id) + '.txt' for id in df_bills['bill_id']]
    df_bills['downloaded'] = ['Y' if os.path.isfile(outpath + filename) else 'N' for filename in df_bills['filename']]
    if not os.path.isdir(outpath):
        os.makedirs(outpath)
    for index, row in df_bills.iterrows():
        text_path = outpath + row['filename']
        text_content = ''
        if not os.path.isfile(text_path) or c.bills_overwrite:
            text_content = f.download_from_url_govtrack_bill_text(row['bill_url'])
            if text_content == '':
                print('ERROR WHEN TRYING TO DOWNLOAD FROM URL: ' + row['bill_url'])
                df_bills.loc[index, 'downloaded'] = 'N'
            elif text_content[0:7] == '[ERROR]':
                print(text_content)
                df_bills.loc[index, 'downloaded'] = 'N'
            else:
                with open(text_path, "w", encoding='utf8') as file:
                    file.write(text_content)
                df_bills.loc[index, 'downloaded'] = 'Y'
    with open(outindex, "w", encoding='utf8') as file:
        df_bills.to_csv(file, sep="\t", header=True, index=False, encoding="utf-8")
