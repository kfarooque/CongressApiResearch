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

#### BUILD MODEL ####

#{}#


# TODO: parse out text similar to what was done in previous twitter project
# - use dfContent$title to experiment with
# - parse out to get key useful terms, topic clustering, topic modeling
# - assign that info back to bill ID -- get a dataframe with just that info and the bill ID (can join later)
# - now try doing this with longer fields like dfResultsContent$summary
# - ...and eventually try to do this with bill text downloaded from uri
# GOALS: practical uses of model...
# - define topics, then can relate to other variables (who introduced, what party, passage status, over time)
# - extract general keywords, then can highlight top keywords and relate them to other variables
# - extract proper noun keywords, then can highlight top items and relate them to other variables
