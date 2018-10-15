# Congress API - Download Results

Generalized method for extracting information from ProPublica's Congress API and GovTrack, related to US Congress bills, committees, lobbying data, etc. 
This uses ProPublica's Congress API to build a data set based on specific keyword(s) searches and separated by type of data.
This also uses GovTrack to pull text for bills that were found during the keyword(s) searches.

Currently, this works well for bills (e.g., using endpoints `bills/search.json`, `115/senate/bills/enacted.json`, or `bills/upcoming/house.json`). It does *not* work well with lobbying (e.g., using endpoints `lobbying/search.json` or `lobbying/latest.json`).

### Update Notes
|Date|Updates|
|----|-------|
|2018-10-08|Completed initial version.|
|2018-10-15|Built steps to pull text for bills from GovTrack.|

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [GovTrack: Bills and Resolutions](https://www.govtrack.us/congress/bills/)
* [Python-Congress Python Client](https://propublica-congress.readthedocs.io/en/latest/)
* [GitHub: eyeseast/propublica-congress](https://github.com/eyeseast/propublica-congress)

# Contents

### Scripts
* `get_api_results.py` downloads data from Congress API into `results.txt` files
* `get_text_bills.py` downloads bill text from GovTrack into `bills.txt` files
* `config.py` defines operational parameters and filenames
* `functions.py` contains all user-defined functions

### Other Files
* `requirements.txt` requirements for running process
* `license.txt` license information
* `.keys` (not in repository) plaintext file with key(s) needed to access API.

### Inputs
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/) (not part of repository): API used for queries; see documentation for structure of requests.
* [GovTrack: Bills and Resolutions](https://www.govtrack.us/congress/bills/) (not part of repository): Site from which bill text is pulled; see site documentation for details.

### Outputs
* `data/<sub1>/<sub2>`: results of request are downloaded and saved to subfolder.
    * `<sub1>` subfolder name based on request URL (e.g., bills_search)
    * `<sub2>` subfolder name based on query (e.g., tax_shelter_loophole)
    * `message.txt`: message with date, request, query, and number of results
    * `results.json`: JSON file with combined results of request
    * `results.txt`: tab-delimited text file with combined results of request
    * `bills.txt`: tab-delimited text file with full text of any bills contained in `results.txt`

# Operation

## Initial Setup
1. Clone this repository to your local computer.
2. Request a key for the ProPublica Congress API ([instructions](https://projects.propublica.org/api-docs/congress-api/)).
3. If necessary, use `pip install` to install the packages: `requests`, `datetime`, `numpy`, `pandas`, `bs4`.
4. Create a plaintext file `.keys` in the root folder, with the contents:
    ```
    api_key = <put your Congress API key here>
    ```

## Get API Results (`get_api_results.py`)

#### Option 1: Run from command line with arguments
1. Run `get_api_results.py` from the command line with arguments for the number of results and the request, in the format:
    ```
    python get_api_results.py <max_results> <endpoint> [query...]
    ```
    * `<max_results>` = integer; maximum number of results to return.
    * `<endpoint>` = string; endpoint for request (part of URL *after* version number and *up to and including* .json part).
    * `[query...]` = string or strings (optional); terms for search queries.
        * Leave blank to omit any query (e.g., when searching for latest bills).
        * Use spaces to separate multiple search terms (e.g., `budget taxes`).
        * Use `""` to enclose multi-word phrases (e.g., `"tax shelter"`).
        * Combine the above for multiple one-or-multi-word search terms (e.g., `"tax shelter" "loophole"`)
2. Check new output in the `data` subfolder (if status is OK), or check the temporary `~*.json` file in the root directory (if there was an error).

#### Option 2: Run from python interpreter
1. Update `config.py` parameters with your request:
    * `infile_keys` = location of the `.keys` file with your API key (default `".keys"`).
    * `outpath_data` = root path for outputs (default `"data/"`).
    * `max_results` = integer; maximum number of results to return.
    * `endpoint` = string; endpoint for request (part of URL *after* version number and *up to and including* .json part).
    * `query` = string or list of strings (optional); terms for search queries.
        * Leave as `""` to omit any query (e.g., when searching for latest bills).
        * Use list of strings for multiple search terms (e.g., `["budget", "taxes"]`).
        * Use single string for multi-word phrases (e.g., `"tax shelter"`).
        * Combine the above for multiple one-or-multi-word search terms (e.g., `["tax shelter", "loophole"]`).
2. Run the `get_api_results.py` script from a python interpreter.
3. Check new output in the `data` subfolder (if status is OK), or check the temporary `~*.json` file in the root directory (if there was an error).

#### Examples
* Search for up to 400 bills containing "tax shelter" or "loophole".
    ```
    python get_api_results.py 400 bills/search.json "tax shelter" "loophole"
    ```
* Return latest 60 enacted bills in 115th Senate. (Alternatively: `introduced`, `updated`, `active`, `passed`, `enacted`, or `vetoed`.)
    ```
    python get_api_results.py 60 115/senate/bills/enacted.json
    ```
* Return up to 40 upcoming House bills.
    ```
    python get_api_results.py 40 bills/upcoming/house.json
    ```
* (UNTESTED) Search for up to 100 lobbying requests related to "net neutrality".
    ```
    python get_api_results.py 100 lobbying/search.json "net neutrality"
    ```
* (UNTESTED) Return latest 40 lobbying actions.
    ```
    python get_api_results.py 40 lobbying/latest.json
    ```
* Return up to 120 Senate members in 115th Senate.
    ```
    python get_api_results.py 120 115/senate/members.json
    ```
* Return up to 20 Joint (House and Senate) committees in 115th Congress.
    ```
    python get_api_results.py 20 115/joint/committees.json
    ```
* Return latest 100 House votes.
    ```
    python get_api_results.py 100 house/votes/recent.json
    ```
* (UNTESTED) Return latest 60 nominees received by 115th Senate. (Alternatively: `received`, `updated`, `confirmed`, or `withdrawn`.)
    ```
    python get_api_results.py 60 115/nominees/received.json
    ```
* (UNTESTED) Return latest 20 nominees from New York considered by 115th Senate.
    ```
    python get_api_results.py 20 115/nominees/state/ny.json
    ```
* (UNTESTED) Return latest 20 official Congressional statements.
    ```
    python get_api_results.py 20 statements/latest.json
    ```

## Get Text of Bills (`get_text_bills.py`)

#### Option 1: Run from command line
1. Run `get_text_bills.py` from the command line (this does not require any arguments):
    ```
    python get_text_bills.py
    ```
    * This will scan every `results.txt` file in the the output path (see `config.py` parameter `outpath_data`) for bills to download.
    * For each bill where there is not a `bills.txt` file in the output path, or there is one but the bill text was not downloaded, this will download and place output in the existing or a new `bills.txt` file.
2. Check new output in the `data` subfolder.
3. (Optional) You may need to re-run this program if it could not download all bills' text the first time (it will only re-download anything that it failed to download the first time, unless the `config.py` parameter `bills_overwrite` is `True`).

#### Option 2: Run from python interpreter
1. Update `config.py` parameters if necessary:
    * `infile_keys` = location of the `.keys` file with your API key (default `".keys"`).
    * `outpath_data` = root path for outputs, and path that will be scanned for any `results.txt` files (default `"data/"`).
    * `bills_overwrite` = whether existing `bills.txt` files found in output will be overwritten even if they already contain bill text.
2. Run the `get_text_bills.py` script from a python interpreter.
    * This will scan every `results.txt` file in the the output path (see `config.py` parameter `outpath_data`) for bills to download.
    * For each bill where there is not a `bills.txt` file in the output path, or there is one but the bill text was not downloaded, this will download and place output in the existing or a new `bills.txt` file.
3. Check new output in the `data` subfolder.
4. (Optional) You may need to re-run this program if it could not download all bills' text the first time (it will only re-download anything that it failed to download the first time, unless the `config.py` parameter `bills_overwrite` is `True`).
