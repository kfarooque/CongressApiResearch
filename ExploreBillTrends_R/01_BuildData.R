#' Load and format data.
#' Inputs: results.txt, bills/_index_.txt, and bills/*.txt file(s) 
#'         from the CongressApiResearch/DownloadResults project
#' Outputs: {}

source("config.R")
source("functions.R")

#### IMPORT DATA ####

listInputResults <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern="results.txt", recursive=TRUE))
dfResultsFull <- ImportCongressApiResults(listInputResults)
listResults <- FormatResultsFull(dfResultsFull)
dfResultsInformation <- listResults$information
dfResultsContent <- listResults$content
rm(listResults, dfResultsFull)

listInputAll <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern=".*\\.txt", recursive=TRUE))
listInputAllEndings <- gsub("(.*/)(.+/.+)($)", "\\2", listInputAll)
listInputBillIndexes <- listInputAll[grep("bills/_index_.txt", listInputAllEndings)]
listInputBillText <- listInputAll[grepl("bills/.*.txt", listInputAllEndings) & !grepl("bills/_index_.txt", listInputAllEndings)]
dfBillIndex <- ImportMultipleResults(listInputBillIndexes, addSource=FALSE)[c('bill_id', 'filename')]
dfBillIndex <- dfBillIndex[!duplicated(dfBillIndex$filename), ]
dfBillText <- ImportMultipleFilesToDf(listInputBillText, addSource=TRUE)[c('text', 'source_file')]
dfBillText <- dfBillText[!duplicated(dfBillText$source_file), ]
dfResultsBillText <- inner_join(dfBillIndex, dfBillText, by=c('filename'='source_file'))
rm(listInputAll, listInputAllEndings, listInputBillIndexes, listInputBillText, dfBillIndex, dfBillText)

### PARSE TEXT ###

# TODO: parse out text similar to what was done in previous twitter project
# - use dfResultsContent$title to experiment with
# - parse out to get key useful terms, topic modeling, topic clustering
# - assign that info back to bill ID -- get a dataframe with just that info and the bill ID (can join later)
# - now try doing this with longer fields like dfResultsContent$summary
# - ...and eventually try to do this with bill text downloaded from uri
