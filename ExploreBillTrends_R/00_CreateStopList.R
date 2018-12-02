#' Create stop list.
#' Inputs: results.txt, bills/_index_.txt, and bills/*.txt file(s) from the CongressApiResearch/DownloadResults project,
#'         stoplist_manual.txt (optional) from the  resources/ subfolder
#' Outputs: stoplist_full.txt (saved to resources folder),

source("config.R")
source("functions.R")

#### IMPORT DATA ####

listInputResults <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern="results.txt", recursive=TRUE))
dfResults <- ImportCongressApiResults(listInputResults)
dflistInformationContent <- FormatResultsFull(dfResults)
dfInformation <- dflistInformationContent$information
dfContent <- dflistInformationContent$content
rm(listInputResults, dflistInformationContent, dfResults)

#### BUILD STOP LIST ####

stoplistManual <- file.path(STOPLIST_FOLDER, "stoplist_manual.txt")
if (!file.exists(stoplistManual)) {
  stoplistManual <- NULL
}

dfTokenWords <- BuildTokensTfidf(dfContent$summary, dfContent$bill_id, ngram=1)
listTopWords <- BuildCommonRareTerms(dfTokenWords, nCommon=0.01, nRare=0)
stoplistWords <- BuildStopList(vectors=listTopWords$common, manual=stoplistManual, auto=TRUE)
rm(dfTokenWords, listTopWords)

#### OUTPUT ####

write(stoplistWords, file.path(STOPLIST_FOLDER, "stoplist_full.txt"))
