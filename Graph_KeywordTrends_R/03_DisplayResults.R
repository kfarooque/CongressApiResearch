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

#TODO: text output using dfTextSummary, filtered to selected categories

#### GRAPH OUTPUT - MAIN ####

#TODO: graph output with predefined features

#### GRAPH OUTPUT - DYNAMIC ####

#TODO: graph output with dynamic features from dfGraphInstructions, using dfGraphInstructions

#### BUILD DASHBOARD ####

#TODO: build dashboards using text output, graph output - main, graph output - dynamic
