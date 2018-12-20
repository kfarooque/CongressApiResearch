#' Describe bill content and flag keywords.
#' Inputs: dfResults, dfBills, stoplist_full.txt (from Import Data step),
#'         KEYWORD_FILE (usually in root folder)
#' Outputs: ??? (saved to OUTPUT folder)

source("config.R")
source("functions.R")

#### LOAD DATA ####

labeledKeywords <- ImportKeywordFile(KEYWORD_FILE)

if (!exists("dfResults")) {
  load(file.path(OUTPUT_FOLDER, "dfResults.RData"))
}
# if (!exists("dfBills")) {
#   load(file.path(OUTPUT_FOLDER, "dfBills.RData"))
# }
if (!exists("stoplistWords")) {
  stoplistWords <- read.table(file.path(STOPLIST_FOLDER, "stoplist_full.txt"), header=FALSE, sep="", stringsAsFactors=FALSE)
  stoplistWords <- unlist(stoplistWords)
}

#### FLAG KEYWORDS ####

dfScanText <- dfResults %>%
  mutate(text = ifelse(!is.na(summary), summary,
                       ifelse(!is.na(summary_short), summary_short,
                              ifelse(!is.na(title), title,
                                     ifelse(!is.na(short_title), short_title,
                                            "")))),
         text_short = ifelse(!is.na(summary_short), summary_short,
                             ifelse(!is.na(title), title,
                                    ifelse(!is.na(short_title), short_title,
                                           "")))
  ) %>%
  select(bill_id, text, text_short)

dfScanTextFlags <- FlagTextKeywords(dfScanText$text, labels=names(labeledKeywords), keywords=labeledKeywords)
names(dfScanTextFlags) <- paste0("keyword_", names(dfScanTextFlags))
dfScanTextFlags <- cbind(dfScanText["bill_id"], dfScanTextFlags)

#### DESCRIBE FEATURES ####

dfDefineGroups <- left_join(dfResults, dfScanTextFlags, by="bill_id") %>%
  mutate(primary_subject = ifelse(is.na(primary_subject), "Undefined", primary_subject),
         introduced_year = as.character(year(introduced_date)),
         sponsor_party = gsub("^.*\\((.).*-.*\\)$", "\\1", sponsor),
         cosponsors_dem = cosponsors_dem + (sponsor_party == "D"),
         cosponsors_rep = cosponsors_rep + (sponsor_party == "R"),
         cosponsors_ind = cosponsors_ind + (sponsor_party == "I"),
         cosponsors_sum = cosponsors_dem + cosponsors_rep + cosponsors_ind,
         cosponsor_party = ifelse(cosponsors_sum == 0, sponsor_party,
                                  ifelse(cosponsors_dem/cosponsors_sum >= 0.7, "D",
                                         ifelse(cosponsors_rep/cosponsors_sum >= 0.7, "R",
                                                "B"))),
         action_year = as.character(year(latest_major_action_date)),
         top_action = ifelse(!is.na(enacted), "ENACTED",
                             ifelse(!is.na(house_passage) | !is.na(senate_passage), "PASSED",
                                    ifelse(!is.na(last_vote), "VOTED",
                                           ifelse(!is.na(active), "ACTIVE",
                                                  "NONE"))))) %>%
  select(bill_id, primary_subject, bill_type, introduced_year, action_year, top_action, cosponsor_party, sponsor, starts_with("keyword_")) %>%
  mutate(primary_subject_x_bill_type = paste(primary_subject, bill_type, sep=" - "),
         primary_subject_x_introduced_year = paste(primary_subject, introduced_year, sep=" - "),
         primary_subject_x_action_year = paste(primary_subject, action_year, sep=" - "),
         primary_subject_x_top_action = paste(primary_subject, top_action, sep=" - "),
         primary_subject_x_cosponsor_party = paste(primary_subject, cosponsor_party, sep=" - "),
         primary_subject_x_sponsor = paste(primary_subject, sponsor, sep=" - "))

#' TODO:
#'   figure out if/how to join keyword flags with primary_subject 
#'   repeat ExtractKeywordsByGroup() by each of the groups defined earlier, new df each time
#'     e.g., ExtractKeywordsByGroup(dfScanText$text, group=dfResults$primary_subject, n=10, stoplist=stoplistWords)


# TODO: (hard)
# - function to define stand-out features by a group
# - use ^ to define features by category of sponsor, party, passage

#### OUTPUT ####

# TODO: output files based on the above... will need many by different units of observation
