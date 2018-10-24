#' Train data and build tokens, document-term matrices.
#' Inputs: dfInformation, dfContent, dfBills, stop_list_auto.txt
#' Outputs: {}

source("config.R")
source("functions.R")

#### LOAD DATA ####

if (!exists("dfInformation")) {
  load(file.path(OUTPUT_ROOT, "dfInformation.RData"))
}
if (!exists("dfContent")) {
  load(file.path(OUTPUT_ROOT, "dfContent.RData"))
}
if (!exists("dfBills")) {
  load(file.path(OUTPUT_ROOT, "dfBills.RData"))
}

stoplistWords <- read.table(file.path(OUTPUT_ROOT, "stop_list.txt"), 
                            header=FALSE, stringsAsFactors=FALSE,
                            sep="", quote="")
stoplistWords <- unlist(stoplistWords)
names(stoplistWords) <- NULL

#### BUILD TOKENS/DTMS ####

dfTokens <- BuildTokensCleaned(dfContent$summary, id=dfContent$bill_id, ngram=1, 
                               stoplist=stoplistWords)
dtmWords <- cast_dtm(dfTokens, id, term, n)


# TODO: join the above to the previous step, rename the previous step, remove some section breaks in functions.R


#if (tolower(RUN_TYPE) == "train") {
#  dfTokensTrain <- BuildTokensFromTweets(select(dfTweetsTrain, doc_id, doc_time, author_id, text), n=1, stoplist=stoplist)
#}
#dfTokensValidate <- BuildTokensFromTweets(select(dfTweetsValidate, doc_id, doc_time, author_id, text), n=1, stoplist=stoplist)
#
#if (tolower(RUN_TYPE) == "train") {
#  dtmTrain <- cast_dtm(dfTokensTrain, doc_id, term, n)
#}
#dtmValidate <- cast_dtm(dfTokensValidate, doc_id, term, n)
#
#if (tolower(RUN_TYPE) == "train") {
#  save(dfTokensTrain, file=file.path(PATH_DATA, "dfTokensTrain.RData"))
#  save(dtmTrain, file=file.path(PATH_DATA, "dtmTrain.RData"))
#}
#save(dfTokensValidate, file=file.path(PATH_DATA, "dfTokensValidate.RData"))
#save(dtmValidate, file=file.path(PATH_DATA, "dtmValidate.RData"))



# TODO: parse out text similar to what was done in previous twitter project
# - use dfContent$title to experiment with
# - parse out to get key useful terms, topic clustering, topic modeling
# - assign that info back to bill ID -- get a dataframe with just that info and the bill ID (can join later)
# - now try doing this with longer fields like dfResultsContent$summary
# - ...and eventually try to do this with bill text downloaded from uri

# practical use:
# - define topics, then can relate to other variables (who introduced, what party, passage status, over time)
# - extract general keywords, then can highlight top keywords and relate them to other variables
# - extract proper noun keywords, then can highlight top items and relate them to other variables
