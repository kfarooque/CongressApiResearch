#' Describe bill content and flag keywords.
#' Inputs: dfResults, dfBills, stoplist_full.txt (from Import Data step),
#'         KEYWORD_FILE (usually in the "resources" folder)
#' Outputs: dfGroups, dfKeywordsByGroup, dfFeaturesByGroup (saved to OUTPUT folder)

source("config.R")
source("functions.R")

#### LOAD DATA ####

if (file.exists(KEYWORD_FILE)) {
  labeledKeywords <- ImportKeywordFile(KEYWORD_FILE)
} else {
  labeledKeywords <- NULL
}

if (!exists("dfResults")) {
  load(file.path(OUTPUT_FOLDER, "dfResults.RData"))
}
# if (!exists("dfBills")) {
#   load(file.path(OUTPUT_FOLDER, "dfBills.RData"))
# }
if (!exists("stoplistWords")) {
  stoplistWords <- read.table(file.path(OUTPUT_FOLDER, "stoplist_full.txt"), header=FALSE, sep="", stringsAsFactors=FALSE)
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
dfScanTextFlags <- ConvertFlagsToName(dfScanTextFlags, falsechar="none")
if (ncol(dfScanTextFlags) > 0) {
  names(dfScanTextFlags) <- paste0("keyword_", names(dfScanTextFlags))
}
dfScanTextFlags <- cbind(dfScanText["bill_id"], dfScanTextFlags)

#### DESCRIBE FEATURES ####

dfGroups <- left_join(dfResults, dfScanTextFlags, by="bill_id") %>%
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
  select(bill_id, primary_subject, bill_type, introduced_year, action_year, top_action, sponsor_party, cosponsor_party, sponsor, starts_with("keyword_")) %>%
  mutate(primary_subject_x_bill_type = paste(primary_subject, bill_type, sep=" / "),
         primary_subject_x_introduced_year = paste(primary_subject, introduced_year, sep=" / "),
         primary_subject_x_action_year = paste(primary_subject, action_year, sep=" / "),
         primary_subject_x_top_action = paste(primary_subject, top_action, sep=" / "),
         primary_subject_x_sponsor_party = paste(primary_subject, sponsor_party, sep=" / "),
         primary_subject_x_cosponsor_party = paste(primary_subject, cosponsor_party, sep=" / "))
for (keyword in names(select(dfScanTextFlags, starts_with("keyword_")))) {
  dfGroups <- unite_(dfGroups, paste0("primary_subject_x_", keyword), 
                     c("primary_subject", keyword), sep=" / ", remove=FALSE)
}

dfKeywordsByGroup <- data.frame()
groupcols <- names(dfGroups)[-1]
for (groupcol in groupcols) {
  df <- ExtractKeywordsByGroup(
    dfScanText$text, group=dfGroups[[groupcol]], 
    n=10, stoplist=stoplistWords
  )
  if (grepl("^keyword_", groupcol)) {
    df <- df[!grepl("^none$|^none / ", df$group), ]
  }
  if (grepl("^.*_x_keyword_", groupcol)) {
    df <- df[!grepl("^none$| / none$", df$group), ]
  }
  df <- spread(df, rank, keywords, fill="") %>% 
    unite(keywords, -group, sep=", ") %>%
    mutate(category = groupcol) %>%
    select(category, group, keywords)
  dfKeywordsByGroup <- rbind(dfKeywordsByGroup, df)
}
rm(df)

dfFeaturesByGroup <- data.frame()
groupcols <- names(dfGroups)[-1]
features <- names(dfGroups)[-1]
for (groupcol in groupcols) {
  df <- ExtractFeaturesByGroup(
    dfGroups, features=features[!(features %in% groupcol)], group=dfGroups[[groupcol]]
  )
  if (grepl("^keyword_", groupcol)) {
    df <- df[!grepl("^none$|^none / ", df$group), ]
  }
  if (grepl("^.*_x_keyword_", groupcol)) {
    df <- df[!grepl("^none$| / none$", df$group), ]
  }
  df <- df[!(grepl("^keyword_", df$feature) & grepl("^none$|^none / ", df$value)), ]
  df <- df[!(grepl("^.*_x_keyword_", df$feature) & grepl("^none$| / none$", df$value)), ]
  df$category <- groupcol
  df <- select(df, category, group, feature, value, comparison)
  dfFeaturesByGroup <- rbind(dfFeaturesByGroup, df)
}
rm(df)

#### OUTPUT ####

if (!dir.exists(OUTPUT_FOLDER)) {
  dir.create(OUTPUT_FOLDER, recursive=TRUE)
}
save(dfGroups, file=file.path(OUTPUT_FOLDER, "dfGroups.RData"))
save(dfKeywordsByGroup, file=file.path(OUTPUT_FOLDER, "dfKeywordsByGroup.RData"))
save(dfFeaturesByGroup, file=file.path(OUTPUT_FOLDER, "dfFeaturesByGroup.RData"))
