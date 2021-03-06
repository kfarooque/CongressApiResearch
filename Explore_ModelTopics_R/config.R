#' Define parameters for project
#' Inputs: n/a
#' Outputs: n/a

#### PARAMETERS ####

TITLE <- "Topic Model with Security-Related Bills"
TRAIN_MODEL <- FALSE
NUMBER_TOPICS <- 20
RANDOM_SEED <- 582
FILTER_ACTIVE <- FALSE # only active
FILTER_VOTED <- FALSE # only active and voted on
FILTER_PASSED <- FALSE # only active, voted on, and passed
FILTER_ENACTED <- FALSE # only active, voted on, passed, and enacted

#### INPUTS/OUTPUTS ####

STOPLIST_FOLDER <- file.path("resources")

INPUT_ROOT <- file.path("..", "DownloadResults", "data", "bills_search")
TRAIN_ROOT <- file.path("model")
OUTPUT_ROOT <- file.path("results", "bills_search")

if (TRAIN_MODEL) {
  TRAIN_FOLDER <- file.path(TRAIN_ROOT, paste0("k", NUMBER_TOPICS))
  OUTPUT_FOLDER <- TRAIN_FOLDER
} else {
  TRAIN_FOLDER <- file.path(TRAIN_ROOT, paste0("k", NUMBER_TOPICS))
  OUTPUT_FOLDER <- OUTPUT_ROOT
}
