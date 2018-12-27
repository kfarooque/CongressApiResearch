#' Display contents and features.
#' Inputs: dfResults, dfGroups, dfKeywordsByGroup, dfFeaturesByGroup
#' Outputs: graphs/tables saved to results/ folder

source("config.R")
source("functions.R")

#### LOAD DATA ####

if (!exists("dfResults")) {
  load(file.path(OUTPUT_FOLDER, "dfResults.RData"))
}
if (!exists("dfGroups")) {
  load(file.path(OUTPUT_FOLDER, "dfGroups.RData"))
}
if (!exists("dfKeywordsByGroup")) {
  load(file.path(OUTPUT_FOLDER, "dfKeywordsByGroup.RData"))
}
if (!exists("dfFeaturesByGroup")) {
  load(file.path(OUTPUT_FOLDER, "dfFeaturesByGroup.RData"))
}

#### BUILD TEXT SUMMARY ####

dfTextSummary <- data.frame()
exampleText <- paste0(dfResults[["number"]], ": ", dfResults[["title"]])
groupcols <- names(dfGroups)[-1]
for (groupcol in groupcols) {
  df <- BuildTextSummary(
    group=dfGroups[[groupcol]], text=exampleText,
    df_keywords=dfKeywordsByGroup[dfKeywordsByGroup$category == groupcol,],
    df_features=dfFeaturesByGroup[dfFeaturesByGroup$category == groupcol,]
  )
  if (grepl("^keyword_", groupcol)) {
    df <- df[!grepl("^none$|^none / ", df$group), ]
  }
  if (grepl("^.*_x_keyword_", groupcol)) {
    df <- df[!grepl("^none$| / none$", df$group), ]
  }
  df <- df[df$count > 5,]
  df$category <- groupcol
  df <- select(df, category, group, count, share, examples, keywords, features)
  dfTextSummary <- rbind(dfTextSummary, df)
}
rm(df)

#### DECIDE DYNAMIC GRAPHS ####

dfGraphInstructions <- data.frame()
groupcols <- names(dfGroups)[-1]
for (groupcol in groupcols) {
  features <- dfFeaturesByGroup[dfFeaturesByGroup$category == groupcol, "feature"]
  features <- unique(features)
  features_combo <- features[grepl("^.*_x_.*$", features)]
  features_combo <- unlist(strsplit(features_combo, "_x_"))
  features_normal <- features[!grepl("^.*_x_.*$", features)]
  features <- unique(c(features_combo, features_normal))
  features <- features[order(features)]
  df <- rbind(data.frame(stringsAsFactors=FALSE, category=groupcol, var1=groupcol, var2=as.character(NA)),
              data.frame(stringsAsFactors=FALSE, category=groupcol, var1=groupcol, var2=features))
  dfGraphInstructions <- rbind(dfGraphInstructions, df)
}
rm(df)

#### TEXT OUTPUT ####

listTextSummary <- list()
categories <- c("primary_subject", "bill_type", "introduced_year", "action_year", "top_action", "cosponsor_party")
for (category in categories) {
  vectorTextSummary <- WriteGroupDescriptions(dfTextSummary[dfTextSummary$category == category, ], title=category)
  listTextSummary[[category]] <- vectorTextSummary
}
rm(vectorTextSummary)

#### GRAPH OUTPUT - MAIN ####

namesOld <- c("primary_subject", "bill_type", "introduced_year", "action_year", "top_action", "cosponsor_party")
namesNew <- c("Primary Subject", "Bill Type", "Year Introduced", "Year of Latest Action", "Furthest Action", "Cosponsor Party Lean")
dfGroupsGraph <- dfGroups %>%
  mutate(introduced_year = as.numeric(introduced_year),
         action_year = as.numeric(action_year))

listGraphMain <- list()

#TODO: graph output with predefined features
# - decide which of the below are worth keeping
# - put in loop with namesOld/namesNew being used to rename variables and actions
# - within each iteration of the loop, add to listGraphMain

GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject")
GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject", gcol="bill_type")
GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="primary_subject", gcol="cosponsor_party")

GraphBarLineGroups(dfGroupsGraph, xcol="bill_type")
GraphBarLineGroups(dfGroupsGraph, xcol="bill_type", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="bill_type", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="bill_type", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="bill_type", gcol="cosponsor_party")

GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="primary_subject")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="bill_type")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="introduced_year", gcol="cosponsor_party")

GraphBarLineGroups(dfGroupsGraph, xcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="primary_subject")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="bill_type")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="action_year", gcol="cosponsor_party")

GraphBarLineGroups(dfGroupsGraph, xcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="top_action", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="top_action", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="top_action", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="top_action", gcol="cosponsor_party")

GraphBarLineGroups(dfGroupsGraph, xcol="cosponsor_party")
GraphBarLineGroups(dfGroupsGraph, xcol="cosponsor_party", gcol="introduced_year")
GraphBarLineGroups(dfGroupsGraph, xcol="cosponsor_party", gcol="action_year")
GraphBarLineGroups(dfGroupsGraph, xcol="cosponsor_party", gcol="top_action")
GraphBarLineGroups(dfGroupsGraph, xcol="cosponsor_party", gcol="cosponsor_party")

#### GRAPH OUTPUT - DYNAMIC ####

#TODO: graph output with dynamic features from dfGraphInstructions, using dfGraphInstructions

#### BUILD DASHBOARD ####

output_dashboard <- file.path(OUTPUT_FOLDER, "dashboard")
if (!dir.exists(output_dashboard)) {
  dir.create(output_dashboard, recursive=TRUE)
}

#TODO: build dashboards using text output, graph output - main, graph output - dynamic
