#' Import and format data.
#' Inputs: results.txt, bills/_index_.txt, and bills/*.txt file(s) from the CongressApiResearch/DownloadResults project
#' Outputs: dfInformation, dfContent, dfBills (saved to TRAIN or OUTPUT folder)

source("config.R")
source("functions.R")

#### IMPORT DATA ####

listInputResults <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern="results.txt", recursive=TRUE))
dfResults <- ImportCongressApiResults(listInputResults)
dflistInformationContent <- FormatResultsFull(dfResults)
dfInformation <- dflistInformationContent$information
dfContent <- dflistInformationContent$content
rm(listInputResults, dflistInformationContent, dfResults)

listInputBills <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern=".*\\.txt", recursive=TRUE))
listInputBillsEndings <- gsub("(.*/)(.+/.+)($)", "\\2", listInputBills)
listInputBillsIndexes <- listInputBills[grep("bills/_index_.txt", listInputBillsEndings)]
listInputBillsText <- listInputBills[grepl("bills/.*.txt", listInputBillsEndings) & !grepl("bills/_index_.txt", listInputBillsEndings)]
if (length(listInputBillsIndexes) > 0) {
  dfBillsIndex <- ImportMultipleResults(listInputBillsIndexes, addSource=FALSE)[c('bill_id', 'filename')]
  dfBillsIndex <- dfBillsIndex[!duplicated(dfBillsIndex$filename), ]
  dfBillsText <- ImportMultipleFilesToDf(listInputBillsText, addSource=TRUE)[c('text', 'source_file')]
  dfBillsText <- dfBillsText[!duplicated(dfBillsText$source_file), ]
  dfBills <- inner_join(dfBillsIndex, dfBillsText, by=c('filename'='source_file'))
  rm(dfBillsIndex, dfBillsText)
} else {
  dfBills <- data.frame()
}
rm(listInputBills, listInputBillsEndings, listInputBillsIndexes, listInputBillsText)

if (nrow(dfInformation) == nrow(dfContent)) {
  print(paste0(nrow(dfInformation), " bills loaded."))
  print("Loaded information (metadata), content (short descriptors).")
} else {
  print("WARNING: There was some mismatch in the contents of bills that were loaded.")
}
if (nrow(dfInformation) == nrow(dfBills)) {
  print("Loaded bills (full text).")
} else {
  print("NOTE: Full bill text was not loaded for all bills.")
}

#### FILTER DATA ####

#TODO: add parameters for filtering by votes/enacted (i.e., drop junk bills), then apply here

#### OUTPUT ####

if (!dir.exists(OUTPUT_FOLDER)) {
  dir.create(OUTPUT_FOLDER, recursive=TRUE)
}
save(dfInformation, file=file.path(OUTPUT_FOLDER, "dfInformation.RData"))
save(dfContent, file=file.path(OUTPUT_FOLDER, "dfContent.RData"))
save(dfBills, file=file.path(OUTPUT_FOLDER, "dfBills.RData"))
