#' Model topics of bills.
#' Inputs: dfInformation, dfContent, dfBills, dfTokens, dtmWords
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
if (!exists("dfTokens")) {
  load(file.path(OUTPUT_ROOT, "dfTokens.RData"))
}
if (!exists("dtmWords")) {
  load(file.path(OUTPUT_ROOT, "dtmWords.RData"))
}

#### BUILD LDA MODEL ####

# TRAIN AND DEFINE TOPICS #

ldaTrain <- LDA(dtmWords, k=NUMBER_TOPICS, control=list(seed=RANDOM_SEED))
ldaBetas <- tidy(ldaTrain, matrix="beta") %>% arrange(term, topic)
ldaGammas <- tidy(ldaTrain, matrix="gamma") %>% arrange(document, topic)
ldaTopics <- ApplyTopicToDocuments(dfTokens, ldaBetas, weightByN=TRUE) %>% arrange(id)

dfInformationTopics <- left_join(dfInformation, ldaTopics, by=c("bill_id"="id"))
dfContentTopics <- left_join(dfContent, ldaTopics, by=c("bill_id"="id"))
dfBillsTopics <- left_join(dfBills, ldaTopics, by=c("bill_id"="id"))

# DESCRIBE TOPICS #

termsTopicTop <- ExtractTopicsTopTerms(ldaBetas, betaFilter=0.001, n=8) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
termsTopicDistinct <- ExtractTopicsDistinctTerms(ldaBetas, betaFilter=0.001, n=8) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
documentsTopicTop <- ExtractTopicsTopDocuments(dfContentTopics, ldaGammas, idcol="bill_id", textcol="summary", n=5) %>%
  select(topic, bill_id, title, summary)

for (t in unique(termsTopicTop$topic)) {
  sepTopic <- "================================"
  sepLine <- ""
  lineHeader <- paste0("Topic #", t)
  lineDocs <- paste0("Documents: ", sum(dfContentTopics$topic == t))
  lineTermsTop <- paste0("Top Terms: ", unlist(termsTopicTop[termsTopicTop$topic == t, "term"]))
  lineTermsDistinct <- paste0("Distinct Terms: ", unlist(termsTopicDistinct[termsTopicDistinct$topic == t, "term"]))
  #lineDocuments <- paste0("Representative Documents: ")
  #listDocuments <- documentsTopicTop[documentsTopicTop$topic == t, c("bill_id", "title")]
  print(c(lineHeader, sepLine, lineDocs, sepLine, lineTermsTop, sepLine, lineTermsDistinct, sepTopic))
}


# TODO: parse out text similar to what was done in previous twitter project
# > update stoplist with more numbers if possible (1, 2, etc.)
# x use dfContent$title to experiment with
# x parse out to get key useful terms, topic clustering, topic modeling
# x assign that info back to bill ID -- get a dataframe with just that info and the bill ID (can join later)
# > now try doing this with longer fields like dfResultsContent$summary
# - ...and eventually try to do this with bill text downloaded from uri
# GOALS: practical uses of model...
# - define topics, then can relate to other variables (who introduced, what party, passage status, over time)
# - extract general keywords, then can highlight top keywords and relate them to other variables
# - extract proper noun keywords, then can highlight top items and relate them to other variables
