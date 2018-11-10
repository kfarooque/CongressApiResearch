#' Model topics of bills.
#' Inputs: dfInformation, dfContent, dfBills,
#'         ldaBetas, ldaGammas, ldaTopics 
#' Outputs: topics_summary_*.txt (saved to results/ folder)

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
if (!exists("ldaBetas")) {
  load(file.path(OUTPUT_ROOT, "ldaBetas.RData"))
}
if (!exists("ldaGammas")) {
  load(file.path(OUTPUT_ROOT, "ldaGammas.RData"))
}
if (!exists("ldaTopics")) {
  load(file.path(OUTPUT_ROOT, "ldaTopics.RData"))
}

#### BUILD TOPIC DESCRIPTORS ####

dfTextTopics <- left_join(dfContent, ldaTopics, by=c("bill_id"="id"))
exampleTermCount <- 8
exampleDocumentCount <- 5

termsTopicTop <- ExtractTopicsTopTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
termsTopicDistinct <- ExtractTopicsDistinctTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
documentsTopicTop <- ExtractTopicsTopDocuments(dfTextTopics, ldaGammas, idcol="bill_id", textcol="summary", n=exampleDocumentCount) %>%
  select(topic, bill_id, title, summary)

#### OUTPUT SUMMARY ####

title <- paste0("Topic Modeling Results with ", NUMBER_TOPICS, " Topics")
subtitle <- OUTPUT_TOPIC_SUBTITLE
sepTopic <- "================================"
sepLine <- ""
exampleCharLimit <- 256

lines <- c(title, subtitle, sepLine, sepTopic)
for (t in unique(termsTopicTop$topic)) {
  lineHeader <- paste0("Topic #", t)
  lineDocs <- paste0("Documents: ", sum(dfTextTopics$topic == t))
  lineTermsTop <- paste0("Top Terms: ", unlist(termsTopicTop[termsTopicTop$topic == t, "term"]))
  lineTermsDistinct <- paste0("Distinct Terms: ", unlist(termsTopicDistinct[termsTopicDistinct$topic == t, "term"]))
  examplesHeader <- paste0("Topic #", t, " - Representative Documents: ")
  examplesList <- documentsTopicTop[documentsTopicTop$topic == t, c("bill_id", "title")] %>%
    unite(documents, bill_id, title, sep="\n\t") %>%
    mutate(documents = substr(documents, 1, exampleCharLimit))
  examplesList <- paste0(unlist(examplesList), collapse="\n")
  newlines <- c(lineHeader, sepLine, lineDocs, lineTermsTop, lineTermsDistinct, 
                examplesHeader, examplesList, sepLine, sepTopic)
  lines <- c(lines, newlines)
}

write(lines, file.path(OUTPUT_ROOT, paste0(OUTPUT_TOPIC_FILESTEM, "_", NUMBER_TOPICS, ".txt")))
