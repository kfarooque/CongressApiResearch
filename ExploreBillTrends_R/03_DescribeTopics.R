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

#### BUILD BILL DESCRIPTORS ####

dfDescription <- dfContent[, c("bill_id", "primary_subject", "title", "summary")] %>%
  left_join(dfInformation[, c("bill_id", "introduced_date", "latest_major_action_date", "enacted", "house_passage", "senate_passage",
                              "sponsor", "cosponsors_dem", "cosponsors_rep", "cosponsors_ind")],
            by="bill_id") %>%
  left_join(ldaTopics[, c("id", "topic", "probability")], by=c("bill_id"="id")) %>%
  filter(latest_major_action_date > "2000-01-01") %>%
  mutate(description = ifelse(!is.na(summary), summary, title),
         description = ifelse(!is.na(primary_subject), paste0("(", primary_subject, ") ", description), description),
         passed = max(house_passage, senate_passage),
         flag_enacted = (!is.na(enacted)),
         flag_passed = (!is.na(house_passage) | !is.na(senate_passage)),
         sponsor_party = gsub("^.*\\((.)-.*\\)$", "\\1", sponsor),
         sponsor_party_num = ifelse(sponsor_party == "D", -1, 
                                    ifelse(sponsor_party == "R", 1, 0)),
         cosponsors_dem = cosponsors_dem + (sponsor_party == "D"),
         cosponsors_rep = cosponsors_rep + (sponsor_party == "R"),
         cosponsors_ind = cosponsors_ind + (sponsor_party == "I"),
         cosponsors_sum = cosponsors_dem + cosponsors_rep + cosponsors_ind,
         cosponsor_party = ifelse(cosponsors_sum == 0, sponsor_party,
                                  ifelse(cosponsors_dem/cosponsors_sum >= 0.7, "D",
                                         ifelse(cosponsors_rep/cosponsors_sum >= 0.7, "R",
                                                "B"))),
         cosponsor_party_num = round((cosponsors_rep - cosponsors_dem) / cosponsors_sum, 2)) %>%
  select(bill_id, description, introduced_date, latest_major_action_date, flag_enacted, flag_passed, 
         sponsor_party, sponsor_party_num, cosponsor_party, cosponsor_party_num,
         topic, probability)

#### BUILD TOPIC DESCRIPTORS ####

exampleTermCount <- 10
exampleDocumentCount <- 5

termsTopicTop <- ExtractTopicsTopTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
termsTopicDistinct <- ExtractTopicsDistinctTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
documentsTopicTop <- ExtractTopicsTopDocuments(dfDescription, ldaGammas, idcol="bill_id", textcol="description", n=exampleDocumentCount) %>%
  select(topic, bill_id, description)

textTopicSummary <- DescribeTopicExamples(
  dfDescription, xTermsTop=termsTopicTop, xTermsDistinct=termsTopicDistinct, xDocumentsTop=documentsTopicTop, 
  title="Topic Model Using Summary Field"
)

#### PLOT TOPIC GRAPHS ####

plotTopics <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic"
)

plotTopicsEnacted <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$flag_enacted, labelCategory="Enacted into Law"
)
plotTopicsPassed <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$flag_passed, labelCategory="Passed in Congress"
)
plotTopicsSponsor <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$sponsor_party, labelCategory="Sponsor Party"
)
plotTopicsCosponsor <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$cosponsor_party, labelCategory="Cosponsor Party Lean"
)

plotTopicsByIntroduction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesTime=dfDescription$introduced_date, labelTime="Year Introduced", timeFreq="Y"
)
plotTopicsByAction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesTime=dfDescription$latest_major_action_date, labelTime="Year of Latest Action", timeFreq="Y"
)

plotsTopicsEnactedByAction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$flag_enacted, labelCategory="Enacted into Law",
  seriesTime=dfDescription$latest_major_action_date, labelTime="Year of Latest Action", timeFreq="Y"
)
plotsTopicsPassedByAction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$flag_passed, labelCategory="Passed in Congress",
  seriesTime=dfDescription$latest_major_action_date, labelTime="Year of Latest Action", timeFreq="Y"
)
plotsTopicsSponsorByIntroduction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$sponsor_party, labelCategory="Sponsor Party",
  seriesTime=dfDescription$introduced_date, labelTime="Year Introduced", timeFreq="Y"
)
plotsTopicsCosponsorByIntroduction <- PlotTopicGraphs(
  seriesGroup=dfDescription$topic, labelGroup="Topic",
  seriesCategory=dfDescription$cosponsor_party, labelCategory="Cosponsor Party Lean",
  seriesTime=dfDescription$introduced_date, labelTime="Year Introduced", timeFreq="Y"
)

#### OUTPUT ####

write(lines, file.path(OUTPUT_ROOT, "topics.txt"))

#TODO: function to print either one plot or list of plots to a file - use PrintMultiplePlots()
#      then call and output to OUTPUT_ROOT/topics...png (name is same as object minus 'plot' part)
# plotTopics
# plotTopicsEnacted
# plotTopicsPassed
# plotTopicsSponsor
# plotTopicsCosponsor
# plotsTopicsEnactedByAction
# plotsTopicsPassedByAction
# plotsTopicsSponsorByIntroduction
# plotsTopicsCosponsorByIntroduction
