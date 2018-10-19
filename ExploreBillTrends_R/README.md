# Congress API Research - Explore Bill Trends (R version)

Explore trends in bills based on data downloaded via ProPublica's Congress API. This uses tab-delimited data based on the JSON files obtained via the API (see the separate project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults)), parses and mines text and data, and generates results and visuals based on identified trends.

**Currently this is in development and the exploration stage, so the methodology and outputs are subject to change.**

### References
* [ProPublica Congress API](https://projects.propublica.org/api-docs/congress-api/)
* [Congress API - Download Results](https://github.com/kfarooque/CongressApiDownloadResults)

# Contents

### Scripts
* `01_BuildData.R` builds and formats results data
* {}
* `config.R` defines parameters for files and operation
* `functions.R` contains all user-defined functions

### Other Files
* `license.txt` license information
* `*.Rproj` R project file

### Inputs
* `*/results.txt` (not part of repository) tab-delimited text files with data obtained from the Congress API for bills, based on output of the separate project [CongressApi_DownloadResults](https://github.com/kfarooque/CongressApi_DownloadResults). Any number of text files can be read in.

### Outputs
* {}

# Operation

### Initial Setup
1. Clone this repository to your local computer.
2. *(Suggested)* Run the first 10 lines of `functions.R` to install any necessary packages.
3. Run the related project [CongressApiResearch/DownloadResults](https://github.com/kfarooque/CongressApiResearch/tree/master/DownloadResults) for one or more specific queries, and note where the `results.txt` and individual bill text file(s) are saved.

### Run Project
1. {}
