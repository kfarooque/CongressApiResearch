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

#### FORMAT DATA ####

dfGroupsGraph <- dfGroups %>%
  mutate(introduced_year = as.numeric(introduced_year),
         action_year = as.numeric(action_year))
namesList <- c(
  "primary_subject"="Primary Subject",
  "bill_type"="Bill Type",
  "introduced_year"="Year Introduced",
  "action_year"="Year of Latest Action",
  "top_action"="Furthest Action",
  "sponsor_party"="Sponsor Party",
  "cosponsor_party"="Cosponsor Party Lean",
  "primary_subject_x_bill_type"="Primary Subject + Bill Type",
  "primary_subject_x_introduced_year"="Primary Subject + Year Introduced",
  "primary_subject_x_action_year"="Primary Subject + Year of Latest Action",
  "primary_subject_x_top_action"="Primary Subject + Furthest Action",
  "primary_subject_x_sponsor_party"="Primary Subject + Sponsor Party",
  "primary_subject_x_cosponsor_party"="Primary Subject + Cosponsor Party Lean"
)

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
  if (nrow(df) > 0) {
    df$category <- groupcol
    df <- select(df, category, group, count, share, examples, keywords, features)
    dfTextSummary <- rbind(dfTextSummary, df)
  }
  rm(df)
}
rm(groupcol, groupcols, exampleText)

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
  if (length(features) > 0) {
    df <- rbind(data.frame(stringsAsFactors=FALSE, category=groupcol, var1=groupcol, var2=as.character(NA)),
                data.frame(stringsAsFactors=FALSE, category=groupcol, var1=groupcol, var2=features))
    dfGraphInstructions <- rbind(dfGraphInstructions, df)
    rm(df)
  }
  rm(features, features_combo, features_normal)
}
rm(groupcol, groupcols)

#### TEXT OUTPUT ####

textSummary <- list()
categories <- c("primary_subject", "bill_type", "introduced_year", "action_year", "top_action", "sponsor_party", "cosponsor_party")
for (category in categories) {
  if (category %in% names(namesList)) {
    title <- paste0(namesList[category], " Description")
  } else {
    title <- paste0(category, " Description")
  }
  vectorTextSummary <- WriteGroupDescriptions(dfTextSummary[dfTextSummary$category == category, ], 
                                              title=title, name_list=namesList)
  textSummary[[category]] <- vectorTextSummary
  rm(title, vectorTextSummary)
}
rm(category, categories)

#### GRAPH OUTPUT ####

graphsBars <- LoopGraphBarLineGroups(
  dfGroupsGraph, name_list=namesList,
  xcol_list="primary_subject",
  gcol_list=c(NA, "bill_type", "top_action", "sponsor_party", "cosponsor_party")
)
graphsLines <- LoopGraphBarLineGroups(
  dfGroupsGraph, name_list=namesList,
  xcol_list=c("introduced_year", "action_year"),
  gcol_list=c("primary_subject", "cosponsor_party")
)
graphsLineGroups <- LoopGraphBarLineGroups(
  dfGroupsGraph, name_list=namesList, bygroup="primary_subject",
  xcol_list="introduced_year",
  gcol_list=c("top_action", "cosponsor_party")
)

graphsDynamic <- list()
categories <- unique(dfGraphInstructions$category)
for (category in categories) {
  dfVars <- dfGraphInstructions[dfGraphInstructions$category == category, c("var1", "var2")]
  graphs <- LoopGraphBarLineGroups(
    dfGroupsGraph, name_list=namesList,
    xcol_list=unique(dfVars$var1), 
    gcol_list=unique(dfVars$var2)
  )
  graphsDynamic[[category]] <- graphs
  rm(dfVars, graphs)
}
rm(category, categories)

#### BUILD DASHBOARD ####

output_dashboard <- file.path(OUTPUT_FOLDER, "dashboard")
if (!dir.exists(output_dashboard)) {
  dir.create(output_dashboard, recursive=TRUE)
}

SavePlotToFile(graphsBars, file=file.path(output_dashboard, "main.png"), height=640, width=1280)
SavePlotToFile(graphsLines, file=file.path(output_dashboard, "main.png"), height=640, width=1280)
SavePlotToFile(graphsLineGroups, file=file.path(output_dashboard, "group.png"), height=320, width=640)
for (graphsDynamicSubset in graphsDynamic) {
  SavePlotToFile(graphsDynamicSubset, file=file.path(output_dashboard, "dynamic.png"), height=640, width=1280)
}

SaveCombinedDashboard(
  output_dashboard, "dashboard1_text.html", 
  title=paste0(TITLE, " - Text Summary of Groups/Categories"),
  textTitle="All Groups/Categories", textContent=textSummary
)
SaveCombinedDashboard(
  output_dashboard, "dashboard2_subjects.html", 
  title=paste0(TITLE, " - Summary of Primary Subjects"),
  textTitle="Main Summary", textContent=textSummary[["primary_subject"]],
  graphs1Title="Main Graphs", graphs1Prefix="main", graphs1Content=c(graphsBars, graphsLines),
  graphs2Title="Group Graphs", graphs2Prefix="group", graphs2Content=graphsLineGroups
)
for (name in names(graphsDynamic)) {
  if (name %in% names(textSummary)) {
    textShown <- textSummary[[name]]
  } else {
    textShown <- "<p>No text summary available</p>"
  }
  if (name %in% names(namesList)) {
    varLabel <- namesList[name]
  } else {
    varLabel <- name
  }
  SaveCombinedDashboard(
    output_dashboard, paste0("dashboard3_dynamic_", name, ".html"), 
    title=paste0(TITLE, " - Specific Information on: ", varLabel),
    textTitle="", textContent=textShown,
    graphs1Title="", graphs1Prefix="dynamic", graphs1Content=graphsDynamic[[name]]
  )
}

