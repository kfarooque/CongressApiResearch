#' Model topics of bills.
#' Inputs: dfInformation, dfContent, dfBills, stoplist.txt
#' Outputs: docTokens, docTermMatrix (saved to results/ folder)
#'          ldaTrain, ldaBetas, ldaGammas, ldaTopics (saved to results/ folder)

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
if (!exists("stoplistWords")) {
  stoplistWords <- read.table(file.path(OUTPUT_ROOT, "stoplist.txt"), header=FALSE, sep="", stringsAsFactors=FALSE)
  stoplistWords <- unlist(stoplistWords)
}

#### BUILD TOKENS ####

docTokens <- BuildTokensCleaned(dfContent$summary, id=dfContent$bill_id, ngram=1, stoplist=stoplistWords, stemWords=TRUE, dropNumbers=TRUE)
docTermMatrix <- cast_dtm(docTokens, id, term, n)

#### BUILD LDA MODEL ####

ldaTrain <- LDA(docTermMatrix, k=NUMBER_TOPICS, control=list(seed=RANDOM_SEED))
ldaBetas <- tidy(ldaTrain, matrix="beta") %>% arrange(term, topic)
ldaGammas <- tidy(ldaTrain, matrix="gamma") %>% arrange(document, topic)
ldaTopics <- ApplyTopicToDocuments(docTokens, ldaBetas, weightByN=TRUE) %>% arrange(id)

#### OUTPUT ####

save(docTokens, file=file.path(OUTPUT_ROOT, "docTokens.RData"))
save(docTermMatrix, file=file.path(OUTPUT_ROOT, "docTermMatrix.RData"))

save(ldaTrain, file=file.path(OUTPUT_ROOT, "ldaTrain.RData"))
save(ldaBetas, file=file.path(OUTPUT_ROOT, "ldaBetas.RData"))
save(ldaGammas, file=file.path(OUTPUT_ROOT, "ldaGammas.RData"))
save(ldaTopics, file=file.path(OUTPUT_ROOT, "ldaTopics.RData"))
