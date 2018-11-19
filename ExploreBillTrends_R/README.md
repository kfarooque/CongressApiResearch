# Congress API Research - Explore Bill Trends (R version)

Explore trends in bills based on data downloaded via ProPublica's Congress API. This uses tab-delimited data based on the JSON files obtained via the API (see the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults)), performs LDA topic modeling, and outputs a dashboard with a summary of topics.

**This project is in development and outputs may be expanded.**

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [Congress API - Download Results](https://github.com/kfarooque/CongressApiDownloadResults)

# Contents

### Scripts
* `01_ImportData.R` imports data and creates stop list.
* `02_ModelTopics.R` builds terms/docs, trains LDA model, and applies topics.
* `03_DescribeTopics.R` describes topics modeled based on terms and representative documents.
* `config.R` defines parameters for files and operation
* `functions.R` contains all user-defined functions

### Other Files
* `license.txt` license information
* `*.Rproj` R project file

### Inputs
* `resources/stop_list_manual.txt` text file with optional manual stop list to incorporate
* `../*/results.txt` (not part of repository) tab-delimited text files with data obtained from the Congress API for bills, based on output of the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults). Any number of text files can be read in.

### Outputs
* From Step 01, output to `results`:
  * `dfInformation.RData, dfContent.RData, dfBills.RData` imported metadata, summary contents, and full text, from bills read in from separate project separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults).
  * `stoplist.txt` stop list with manual, automatic, and rare terms.
* From Step 02, output to `results`:
  * `docTokens.RData, docTermMatrix.RData` tokens and document-term matrix used in topic modeling.
  * `ldaTrain.RData, ldaBetas.RData, ldaGammas.RData, ldaTopics.RData` results of LDA topic modeling.
* From Step 03, output to `results/dashboard`:
  * `dashboard.html` combined dashboard with topic descriptors and images.
  * `summary.html` description of each topic based on key terms and representative docs.
  * `*.png` various images that graph topics by other characteristics.

# Operation

### Initial Setup
1. Clone this repository to your local computer.
2. *(Suggested)* Run the first 10 lines of `functions.R` to install any necessary packages.
3. Run the related project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults) for one or more specific queries, and note where the `results.txt` and individual bill text file(s) are saved.

### Run Project
1. Define parameters in `config.py`:
  * `INPUT_ROOT` = path for input results.txt files and bill (often `../DownloadResults/data/*`)
  * `INPUT_STLOPLIST` = path for optional manual stop list to add to automatic one
  * `OUTPUT_ROOT` = path for output results
  * `OUTPUT_TOPIC_FILESTEM` = name of output topic description file (omit file extension)
  * `OUTPUT_TOPIC_SUBTITLE` = subtitle to use in output topic description file (optional)
  * `RANDOM_SEED` = random seed to use for LDA model training
  * `NUMBER_TOPICS` = number of topics to model in LDA training
2. Run steps in order (step 01 can be skipped if files are already imported):
  ```
  source("01_ImportData.R")
  source("02_ModelTopics.R")
  source("03_DescribeTopics.R")
  ```
3. Review outputs in `results/*` folder.
