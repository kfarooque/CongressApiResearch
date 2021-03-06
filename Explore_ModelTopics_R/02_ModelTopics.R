#' Estimate or apply topic model to bills.
#' Inputs: dfInformation, dfContent, dfBills, stoplist_full.txt,
#'         ldaBetas (if not training model)
#' Outputs: labels.txt, ldaTrain, ldaBetas, ldaGammas (saved to TRAIN folder if training model)
#'          ldaTopics (saved to OUTPUT folder)

source("config.R")
source("functions.R")

#### LOAD DATA ####

if (!exists("dfInformation")) {
  load(file.path(OUTPUT_FOLDER, "dfInformation.RData"))
}
if (!exists("dfContent")) {
  load(file.path(OUTPUT_FOLDER, "dfContent.RData"))
}
# if (!exists("dfBills")) {
#   load(file.path(OUTPUT_FOLDER, "dfBills.RData"))
# }
if (!exists("stoplistWords")) {
  stoplistWords <- read.table(file.path(STOPLIST_FOLDER, "stoplist_full.txt"), header=FALSE, sep="", stringsAsFactors=FALSE)
  stoplistWords <- unlist(stoplistWords)
}

#### BUILD LDA MODEL ####

docTokens <- BuildTokensCleaned(dfContent$summary, id=dfContent$bill_id, ngram=1, stoplist=stoplistWords, stemWords=FALSE, dropNumbers=TRUE)

if (TRAIN_MODEL) {
  docTermMatrix <- cast_dtm(docTokens, id, term, n)
  ldaTrain <- LDA(docTermMatrix, k=NUMBER_TOPICS, control=list(seed=RANDOM_SEED))
  ldaBetas <- tidy(ldaTrain, matrix="beta") %>% arrange(term, topic)
  ldaGammas <- tidy(ldaTrain, matrix="gamma") %>% arrange(document, topic)
} else {
  load(file.path(TRAIN_FOLDER, "ldaBetas.RData"))
}
ldaTopics <- ApplyTopicToDocuments(docTokens, ldaBetas, weightByN=TRUE) %>% arrange(id)

#### OUTPUT ####

if (TRAIN_MODEL) {
  save(ldaTrain, file=file.path(TRAIN_FOLDER, "ldaTrain.RData"))
  save(ldaBetas, file=file.path(TRAIN_FOLDER, "ldaBetas.RData"))
  save(ldaGammas, file=file.path(TRAIN_FOLDER, "ldaGammas.RData"))
  if (!file.exists(file.path(TRAIN_FOLDER, "labels.txt"))) {
    CreateDummyLabelsFile(NUMBER_TOPICS, file=file.path(TRAIN_FOLDER, "labels.txt"))
  }
}
save(ldaTopics, file=file.path(OUTPUT_FOLDER, "ldaTopics.RData"))
