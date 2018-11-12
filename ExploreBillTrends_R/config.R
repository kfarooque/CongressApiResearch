#' Define parameters for project
#' Inputs: n/a
#' Outputs: n/a

#### INPUTS/OUTPUTS ####

INPUT_ROOT <- file.path("..", "DownloadResults", "data", "bills_search")
INPUT_STOPLIST <- file.path("resources", "stop_list_manual.txt")

OUTPUT_ROOT <- file.path("results", "bills_search")
OUTPUT_TOPIC_FILESTEM <- "topics_summary"
OUTPUT_TOPIC_TITLE <- "Topic Model Using Summary Field"

#### PARAMETERS ####

RANDOM_SEED <- 654
NUMBER_TOPICS <- 10
