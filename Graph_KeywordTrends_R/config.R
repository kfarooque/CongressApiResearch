#' Define parameters for project
#' Inputs: n/a
#' Outputs: n/a

#### PARAMETERS ####

TITLE <- ""
FILTER_ACTIVE <- FALSE # only active
FILTER_VOTED <- FALSE # only active and voted on
FILTER_PASSED <- FALSE # only active, voted on, and passed
FILTER_ENACTED <- FALSE # only active, voted on, passed, and enacted
KEYWORD_FILE <- "keywords.txt"

#### INPUTS/OUTPUTS ####

INPUT_ROOT <- file.path("..", "DownloadResults", "data", "bills_introduced")
STOPLIST_FOLDER <- file.path("resources")
OUTPUT_FOLDER <- file.path("results")
