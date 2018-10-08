# Congress API - Download Results

Generalized method for extracting information from ProPublica's Congress API, related to US Congress bills, committees, lobbying data, etc. This builds a data set based on specific keyword(s) and separated by type of data, in a format easier to analyze.

Currently, this works well for bills (e.g., using endpoints `bills/search.json`, `115/senate/bills/enacted.json`, or `bills/upcoming/house.json`). It does *not* work well with lobbying (e.g., using endpoints `lobbying/search.json` or `lobbying/latest.json`).

### Update Notes
|Date|Updates|
|----|-------|
|2018-10-08|Completed initial version.|

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [Python-Congress Python Client](https://propublica-congress.readthedocs.io/en/latest/)
* [GitHub: eyeseast/propublica-congress](https://github.com/eyeseast/propublica-congress)

# Contents

### Scripts
* `main.py` downloads and saves data from Congress API
* `config.py` defines operational parameters and filenames
* `functions.py` contains all user-defined functions

### Other Files
* `requirements.txt` requirements for running process
* `license.txt` license information
* `.keys` (not in repository) plaintext file with key(s) needed to access API.

### Inputs
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/) (not part of repository): API used for queries; see documentation for structure of requests.

### Outputs
* `data/<sub1>/<sub2>`: results of request are downloaded and saved to subfolder.
    * `<sub1>` subfolder name based on request URL (e.g., bills_search)
    * `<sub2>` subfolder name based on query (e.g., tax_shelter_loophole)
    * `results.json`: JSON file with combined results of request
    * `results.txt`: tab-delimited text file with combined results of request
    * `message.txt`: message with date, request, query, and number of results

# Operation

### Initial Setup
1. Clone this repository to your local computer.
2. Request a key for the ProPublica Congress API ([see instructions here](https://projects.propublica.org/api-docs/congress-api/)).
3. If necessary, use `pip install` to install the packages: `requests`, `datetime`, `numpy`, `pandas`.
4. Create a plaintext file `.keys` in the root folder, with the contents:
    ```
    api_key = <put your Congress API key here>
    ```

### Get Request

#### Option 1: Run from command line with arguments
1. Run `main.py` from the command line with arguments for the number of results and the request, in the format:
    ```
    python main.py <max_results> <endpoint> [query...]
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
    * `max_results` = integer; maximum number of results to return.
    * `endpoint` = string; endpoint for request (part of URL *after* version number and *up to and including* .json part).
    * `query` = string or list of strings (optional); terms for search queries.
        * Leave as `""` to omit any query (e.g., when searching for latest bills).
        * Use list of strings for multiple search terms (e.g., `["budget", "taxes"]`).
        * Use single string for multi-word phrases (e.g., `"tax shelter"`).
        * Combine the above for multiple one-or-multi-word search terms (e.g., `["tax shelter", "loophole"]`).
2. Run the `main.py` script from a python interpreter.
3. Check new output in the `data` subfolder (if status is OK), or check the temporary `~*.json` file in the root directory (if there was an error).

# Examples

#### Bill Search

Search for up to 400 bills containing "tax shelter" or "loophole".
```
python main.py 400 bills/search.json "tax shelter" "loophole"
```

Return latest 60 enacted bills in 115th Senate. (Alternatively: `introduced`, `updated`, `active`, `passed`, `enacted`, or `vetoed`.)
```
python main.py 60 115/senate/bills/enacted.json
```

Return up to 40 upcoming House bills.
```
python main.py 40 bills/upcoming/house.json
```

#### Lobbying Search *(not fully tested)*

Search for up to 100 lobbying requests related to "net neutrality".
```
python main.py 100 lobbying/search.json "net neutrality"
```

Return latest 40 lobbying actions.
```
python main.py 40 lobbying/latest.json
```

#### Congress Structure

Return up to 120 Senate members in 115th Senate.
```
python main.py 120 115/senate/members.json
```

Return up to 20 Joint (House and Senate) committees in 115th Congress.
```
python main.py 20 115/joint/committees.json
```

Return latest 100 House votes.
```
python main.py 100 house/votes/recent.json
```

#### Nominees *(not fully tested)*

Return latest 60 nominees received by 115th Senate. (Alternatively: `received`, `updated`, `confirmed`, or `withdrawn`.)
```
python main.py 60 115/nominees/received.json
```

Return latest 20 nominees from New York considered by 115th Senate.
```
python main.py 20 115/nominees/state/ny.json
```

#### Statements *(not fully tested)*

Return latest 20 official Congressional statements.
```
python main.py 20 statements/latest.json
```
