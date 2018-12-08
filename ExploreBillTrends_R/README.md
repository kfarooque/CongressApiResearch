# Congress API Research - Explore Bill Trends (R version)

Explore trends in bills based on data downloaded via ProPublica's Congress API. This uses tab-delimited data based on the JSON files obtained via the API (see the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults)), performs LDA topic modeling, and outputs a dashboard with a summary of topics.

**This project is in development and outputs may be expanded.**

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [Congress API - Download Results](https://github.com/kfarooque/CongressApiDownloadResults)

# Contents

### Scripts
* `00_CreateStopList.R` creates an automatic stop list.
* `01_ImportData.R` imports data.
* `02_ModelTopics.R` builds terms/docs, trains or applies LDA model, and defines topics.
* `03_DescribeTopics.R` describes topics modeled based on terms and representative documents.
* `config.R` defines parameters for files and operation.
* `functions.R` contains all user-defined functions.

### Other Files
* `license.txt` license information.
* `*.Rproj` R project file.

### Inputs
* `resources/stop_list_manual.txt` text file with optional manual stop list to incorporate.
* `../*/results.txt` (not part of repository) tab-delimited text files with data obtained from the Congress API for bills, based on output of the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults). Any number of text files can be read in.

### Outputs
* Frop Step 00, output to `resources`:
  * `stoplist_full.txt` stop list with manual, automatic, and rare terms.
* From Step 01, output to `model/*` or `results/*`:
  * `dfInformation.RData, dfContent.RData, dfBills.RData` imported metadata, summary contents, and full text, from bills read in from separate project separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults).
* From Step 02, output to `model/*` or `results/*`:
  * `labels.txt` (model training only) text file with topic labels - *this can be modified by the user to apply labels to future outputs*.
  * `ldaTrain.RData`, `ldaBetas.RData`, `ldaGammas.RData` (model training only) parameters for LDA topic modeling.
  * `ldaTopics.RData` results of LDA topic model application.
* From Step 03, output to `model/*/dashboard` or `results/*/dashboard`:
  * `dashboard.html` combined dashboard with topic descriptors and images.
  * `summary.html` description of each topic based on key terms and representative docs.
  * `*.png` various images that graph topics by other characteristics.

# Operation

### Initial Setup
1. Clone this repository to your local computer.
2. *(Suggested)* Run the first 10 lines of `functions.R` to install any necessary packages.
3. Run the related project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults) for one or more specific queries, and note where the `results.txt` and individual bill text file(s) are saved.

### Run Project
1. (Only need to do this once for bills to be read in) Run step 0 to create stop list:
  ```
  source("00_CreateStopList.R")
  ```
2. Define parameters in `config.py`:
  * `TRAIN_MODEL` = whether to train model (output to `model/*` folder) or apply existing model to new data (output to `results/*` folder)
  * `NUMBER_TOPICS` = number of topics to model in LDA training or apply from LDA model
  * `RANDOM_SEED` = random seed to use for LDA model training
  * `FILTER_ACTIVE`, `FILTER_VOTED`, `FILTER_PASSED`, `FILTER_ENACTED` = whether to filter inputs to just active, active + voted, active + voted + passed, or active + voted + passed + enacted bills (each of these is more restrictive, and the most restrictive filter will be applied)
  * `STOPLIST_FOLDER` = folder where input manual stoplist and output full stoplist are stored
  * `INPUT_ROOT` = root path with input results.txt files and bills in deeper subfolders (often `../DownloadResults/data/*`)
  * `TRAIN_ROOT` = root path for LDA model results (will append subfolder `/k#`, where # is number of clusters used)
  * `OUTPUT_ROOT` = root path for output scored results
3. Run steps in order (step 01 can be skipped if files are already imported):
  ```
  source("01_ImportData.R")
  source("02_ModelTopics.R")
  source("03_DescribeTopics.R")
  ```
4. If training a model (`TRAIN_MODEL = TRUE`):
  * Review the results in the `model/*/dashboard` folder.
  * Adjust and re-run steps 2-4 with different parameters if necessary.
  * Edit the `model/*/labels.txt` file with topic labels.
  * Re-run `source("03_DescribeTopics.R")` to regenerate dashboard with the new topic labels.
5. If applying an existing model (`TRAIN_MODEL = FALSE`):
  * Review the results in the `results/*/dashboard` folder.
  * Adjust and re-run steps 2-4 with different parameters if necessary.
