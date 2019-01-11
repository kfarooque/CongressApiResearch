#' Define parameters for project
#' Inputs: n/a
#' Outputs: n/a

#### PARAMETERS ####

TITLE <- "Explore trends in bills since 2016"
LABEL <- "bills_all"
FILTER_ACTIVE <- FALSE # only active
FILTER_VOTED <- FALSE # only active and voted on
FILTER_PASSED <- FALSE # only active, voted on, and passed
FILTER_ENACTED <- FALSE # only active, voted on, passed, and enacted

#### INPUTS/OUTPUTS ####

INPUT_ROOT <- file.path("..", "DownloadResults", "data", "bills_introduced")
STOPLIST_FILE <- file.path("resources", "stoplist.txt")
KEYWORD_FILE <- file.path("resources", "keywords.txt")
OUTPUT_FOLDER <- file.path("results", LABEL)
