#' Define parameters for project
#' Inputs: n/a
#' Outputs: n/a

#### INPUTS/OUTPUTS ####

INPUT_ROOT <- file.path("..", "DownloadResults", "data", "bills_introduced")
INPUT_STOPLIST <- file.path("resources", "stop_list_manual.txt")

TRAIN_ROOT <- file.path("model")

OUTPUT_ROOT <- file.path("results", "bills_search")

#### PARAMETERS ####

TRAIN_MODEL <- TRUE
RANDOM_SEED <- 654
NUMBER_TOPICS <- 10

if (TRAIN_MODEL) {
  OUTPUT_ROOT <- TRAIN_ROOT
}
