#' Import and format data, build stop list.
#' Inputs: results.txt, bills/_index_.txt, and bills/*.txt file(s) from the CongressApiResearch/DownloadResults project,
#'         stoplist_manual.txt (optional) in the "resources" folder
#' Outputs: dfResults, dfBills (saved to OUTPUT folder),
#'          stoplist_auto.txt (saved to STOPLIST folder)

source("config.R")
source("functions.R")

#### IMPORT DATA ####

listInputResults <- file.path(INPUT_ROOT, list.files(INPUT_ROOT, pattern="results.txt", recursive=TRUE))
dfResults <- ImportCongressApiResults(listInputResults)
dfResults <- FormatCongressApiResults(dfResults)
rm(listInputResults)

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

if (nrow(dfResults) > 0) {
  print(paste0(nrow(dfResults), " bills loaded."))
  print("Loaded information (metadata), content (short descriptors).")
} else {
  print("WARNING: There was some problem with loading bill data.")
}
if (nrow(dfResults) == nrow(dfBills)) {
  print("Loaded bills (full text).")
} else {
  print("NOTE: Full bill text was not loaded for all bills.")
}

#### FILTER DATA ####

dfFilter <- cbind(dfResults[, c("bill_id", "active")],
                  data.frame(lapply(dfResults[, c("last_vote", "house_passage", "senate_passage", "enacted")], function(x) {!is.na(x)})))
if (FILTER_ENACTED) {
  bills <- dfFilter[dfFilter$enacted, "bill_id"]
} else if (FILTER_PASSED) {
  bills <- dfFilter[(dfFilter$house_passage | dfFilter$senate_passage) | dfFilter$enacted, "bill_id"]
} else if (FILTER_VOTED) {
  bills <- dfFilter[dfFilter$last_vote | (dfFilter$house_passage | dfFilter$senate_passage) | dfFilter$enacted, "bill_id"]
} else if (FILTER_ACTIVE) {
  bills <- dfFilter[dfFilter$active | dfFilter$last_vote | (dfFilter$house_passage | dfFilter$senate_passage) | dfFilter$enacted, "bill_id"]
} else {
  bills <- dfFilter[, "bill_id"]
}
rm(dfFilter)

if ("bill_id" %in% colnames(dfResults) & length(bills) != nrow(dfResults)) {
  dfResults <- dfResults[dfResults$bill_id %in% bills, ]
}
if ("bill_id" %in% colnames(dfBills) & length(bills) != nrow(dfBills)) {
  dfBills <- dfBills[dfBills$bill_id %in% bills, ]
}

print(paste0("There were ", length(bills), " bills kept after filtering."))

#### BUILD STOP LIST ####

stoplistManual <- file.path(STOPLIST_FOLDER, "stoplist_manual.txt")
if (!file.exists(stoplistManual)) {
  stoplistManual <- NULL
}

dfTokenWords <- BuildTokensTfidf(dfResults$summary, dfResults$bill_id, ngram=1)
listTopWords <- BuildCommonRareTerms(dfTokenWords, nCommon=0.01, nRare=0)
stoplistWords <- BuildStopList(vectors=listTopWords$common, manual=stoplistManual, auto=TRUE)
rm(dfTokenWords, listTopWords)

#### OUTPUT ####

if (!dir.exists(OUTPUT_FOLDER)) {
  dir.create(OUTPUT_FOLDER, recursive=TRUE)
}
save(dfResults, file=file.path(OUTPUT_FOLDER, "dfResults.RData"))
save(dfBills, file=file.path(OUTPUT_FOLDER, "dfBills.RData"))
write(stoplistWords, file.path(STOPLIST_FOLDER, "stoplist_full.txt"))
