# Congress API Research - Graph: Keyword Trends (R version)

Build dashboard with graphs and information on bills downloaded via ProPublica's Congress API. This uses tab-delimited data based on the JSON files obtained via the API (see the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults)), optionally flags keywords in addition to other general bill characteristics (sponsor, partisan lean, subject, etc.), automatically extracts keywords and highlights features, and generates graphs.

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [Congress API - Download Results](https://github.com/kfarooque/CongressApiDownloadResults)

# Contents

### Scripts
* `01_ImportData.R` imports data and builds stoplist.
* `02_DescribeBills.R` flags keywords and extracts all features based on bill characteristics.
* `03_DisplayResults.R` creates graphs and builds dashboard.
* `config.R` defines parameters for files and operation.
* `functions.R` contains all user-defined functions.

### Other Files
* `license.txt` license information.
* `*.Rproj` R project file.

### Inputs
* `resources/stoplist.txt` text file with optional manual stop list to incorporate.
* `resources/keywords.txt` (not saved in repository) text file with keywords to flag as another characteristic (see **Operation** section for details).
* `../*/results.txt` (not part of repository) tab-delimited text files with data obtained from the Congress API for bills, based on output of the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults). Any number of text files can be read in.

### Outputs
* From Step 01, output to `results/*`:
  * `dfResults.RData, dfBills.RData` imported metadata, summary contents, and full text, from bills read in from separate project separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults).
  * `stoplist_full.txt` stop list of words based on manual stop list and automatic parsing of too rare or too common terms.
* From Step 02, output to `results/*`:
  * `dfGroups.RData` all bills with identified features and keyword flags.
  * `dfKeywordsByGroup.RData` extracted keywords to describe each group of data.
  * `dfFeaturesByGroup.RData` notable features based on comparing each group to the rest of population.
* From Step 03, output to `results/*/dashboard`:
  * `dashboard*.html` combined dashboards with topic descriptors and images.
  * `*.png` various images that graph bills by various characteristics.

# Operation

### Initial Setup
1. Clone this repository to your local computer.
2. *(Suggested)* Run the first 10 lines of `functions.R` to install any necessary packages.
3. Run the related project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults) for one or more specific queries, and note where the `results.txt` and individual bill text file(s) are saved.
4. (Optional) Create manual stop list: Create the file `stoplist.txt` in the `resources` subfolder. Contents should contain one row per term to be excluded from use when extracting keywords. Example content:
  ```
  house
  senate
  congress
  act
  acts
  acted
  acting
  ```
5. (Optional) Define keywords to flag in bills: Create the file `keywords.txt` in the `resources` subfolder. Contents should contain one keyword and the terms used to flag them per row, in the format `keyword label = term 1, term 2, term 3, ...`. Example content:
  ```
  taxes = tax, taxes, taxation, fico, payroll
  jobs = job, jobs, employee, employees, employer, employers, employment
  ```

### Run Project
1. Define parameters in `config.py`:
  * `TITLE` = title to use for dashboard
  * `LABEL` = label to use for output subfolder
  * `FILTER_ACTIVE`, `FILTER_VOTED`, `FILTER_PASSED`, `FILTER_ENACTED` = whether to filter inputs to just active, active + voted, active + voted + passed, or active + voted + passed + enacted bills (each of these is more restrictive, and the most restrictive filter will be applied)
  * `INPUT_ROOT` = root path with input results.txt files and bills in deeper subfolders (often `../DownloadResults/data/*`)
  * `STOPLIST_FILE` = full path of manual stoplist file, usually `resources/stoplist.txt`
  * `KEYWORD_FILE` = full path of keyword flagging file, usually `resources/keywords.txt`
  * `OUTPUT_ROOT` = root path for output results
2. Run remaining steps in order:
  ```
  source("01_ImportData.R")
  source("02_DescribeBills.R")
  source("03_DisplayResults.R")
  ```
3. Review outputs in `results/*/dashboard/dashboard*.html`.
