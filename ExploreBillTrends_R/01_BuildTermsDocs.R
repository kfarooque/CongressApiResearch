#' Import and format data, create stop list, and build terms/docs data.
#' Inputs: results.txt, bills/_index_.txt, and bills/*.txt file(s) 
#'         from the CongressApiResearch/DownloadResults project
#' Outputs: dfInformation, dfContent, dfBills (saved to results/ folder),
#'          stop_list.txt (saved to results/ folder),
#'          dfTokens, dtmWords (saved to results/ folder)

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
dfBillsIndex <- ImportMultipleResults(listInputBillsIndexes, addSource=FALSE)[c('bill_id', 'filename')]
dfBillsIndex <- dfBillsIndex[!duplicated(dfBillsIndex$filename), ]
dfBillsText <- ImportMultipleFilesToDf(listInputBillsText, addSource=TRUE)[c('text', 'source_file')]
dfBillsText <- dfBillsText[!duplicated(dfBillsText$source_file), ]
dfBills <- inner_join(dfBillsIndex, dfBillsText, by=c('filename'='source_file'))
rm(listInputBills, listInputBillsEndings, listInputBillsIndexes, listInputBillsText, dfBillsIndex, dfBillsText)

if (nrow(dfInformation) == nrow(dfContent) & nrow(dfInformation) == nrow(dfBills)) {
  print(paste0(nrow(dfInformation), " bills loaded."))
  print("Loaded information (metadata), content (short descriptors), and bills (full text).")
} else {
  print("WARNING: There was some mismatch in the contents of bills that were loaded.")
}

#### BUILD STOP LIST ####

dfTokenWords <- BuildTokensTfidf(dfContent$summary, dfContent$bill_id, ngram=1)
listTopWords <- BuildCommonRareTerms(dfTokenWords, nCommon=0.01, nRare=0)
stoplistWords <- BuildStopList(vectors=listTopWords$common, manual=INPUT_STOPLIST, auto=TRUE)
rm(dfTokenWords, listTopWords)

#### BUILD TOKENS ####

dfTokens <- BuildTokensCleaned(dfContent$summary, id=dfContent$bill_id, ngram=1, stoplist=stoplistWords)
dtmWords <- cast_dtm(dfTokens, id, term, n)

#### OUTPUT ####

if (!dir.exists(OUTPUT_ROOT)) {
  dir.create(OUTPUT_ROOT, recursive=TRUE)
}

save(dfInformation, file=file.path(OUTPUT_ROOT, "dfInformation.RData"))
save(dfContent, file=file.path(OUTPUT_ROOT, "dfContent.RData"))
save(dfBills, file=file.path(OUTPUT_ROOT, "dfBills.RData"))

write(stoplistWords, file.path(OUTPUT_ROOT, "stop_list.txt"))

save(dfTokens, file=file.path(OUTPUT_ROOT, "dfTokens.RData"))
save(dtmWords, file=file.path(OUTPUT_ROOT, "dtmWords.RData"))

