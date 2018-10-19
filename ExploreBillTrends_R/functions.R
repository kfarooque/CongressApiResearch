#' Define functions for project
#' Inputs: n/a
#' Outputs: n/a

#### LOAD PACKAGES ####

# Load packages
reqPackages <- c("readxl", "dplyr", "tidyr", "readr", "tidytext", "topicmodels", "ggplot2", "ggraph", "igraph")
lapply(reqPackages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))
rm(reqPackages)


#### BUILD DATA ####


ImportMultipleResults <- function(files, header=TRUE, sep="\t", quote="", addSource=TRUE) {
  #' Import multiple text files with same headers.
  #' Args:
  #'   files: vector of files to import, use full paths
  #'          each file should have same headers and represent similar content (e.g., bills)
  #'   header: whether to read in header (default TRUE)
  #'   sep: separation character between fields (default tab or "\t")
  #'   quote: quote character(s) to use (default "")
  #'   addSource: whether to add source file name as a column (default TRUE)
  #' Returns:
  #'   data frame based on all files imported, with added column for path of each file (optional)
  dfResults <- data.frame()
  for (f in 1:length(files)) {
    file <- files[f]
    dfTemp <- read.table(file, header=header, sep=sep, quote=quote, na.strings="", comment.char="",
                         fileEncoding="UTF-8", strip.white=TRUE, stringsAsFactors=FALSE)
    if (addSource) {
      parts <- unlist(strsplit(file, split="[\\/]"))
      dfTemp['source_path'] = file
      dfTemp['source_file'] = parts[length(parts)]
    }
    dfResults <- rbind(dfResults, dfTemp)
  }
  dfResults
}


ImportMultipleFilesToDf <- function(files, addSource=TRUE) {
  #' Import multiple text files, each one as an entire text string, and place into a dataframe.
  #' Args:
  #'   files: vector of files to import, use full paths
  #'   addSource: whether to add source file name as a column (default TRUE)
  #' Returns:
  #'   data frame with one row per file imported, with added column for path of each file (optional)
  contents <- c()
  filenames <- c()
  for (f in 1:length(files)) {
    contents[f] <- read_file(files[f])
    if (addSource) {
      parts <- unlist(strsplit(files[f], split="[\\/]"))
      filenames[f] <- parts[length(parts)]
    }
  }
  if (addSource) {
    dfResults <- data.frame(stringsAsFactors=FALSE, contents, files, filenames)
    names(dfResults) <- c("text", "source_path", "source_file")
  } else {
    dfResults <- data.frame(stringsAsFactors=FALSE, contents)
    names(dfResults) <- c("text")
  }
  dfResults
}


ImportCongressApiResults <- function(files, header=TRUE, sep="\t", quote="") {
  #' Import results text file from Congress API 'get_api_results' step.
  #' Args:
  #'   files: vector of files to import, use full paths
  #'          each file should have same headers and represent similar content (e.g., bills)
  #'   header: whether to read in header (default TRUE)
  #'   sep: separation character between fields (default tab or "\t")
  #'   quote: quote character(s) to use (default "")
  #' Returns:
  #'   data frame based on all files imported, with added columns for path of each file
  dfResults <- data.frame()
  for (f in 1:length(files)) {
    file <- files[f]
    parts <- unlist(strsplit(file, split="[\\/]"))
    dfTemp <- read.table(file, header=header, sep=sep, quote=quote, na.strings="", comment.char="",
                         fileEncoding="UTF-8", strip.white=TRUE, stringsAsFactors=FALSE)
    dfTemp['search_type'] <- ifelse(length(parts) > 2, parts[length(parts)-2], "")
    dfTemp['search_query'] <- ifelse(length(parts) > 1, parts[length(parts)-1], "")
    dfResults <- rbind(dfResults, dfTemp)
  }
  dfResults
}


ConvertColumnsCharToOther <- function(df, colsDates=NULL, colsBoolean=NULL, colsNumeric=NULL) {
  #' Convert dataframe columns from character to date, boolean, and numeric
  #' Args:
  #'   df: data frame
  #'   colsDates: vector of names of columns to convert to date
  #'   colsBoolean: vector of names of columns to convert to TRUE/FALSE type
  #'   colsNumeric: vector of names of columns to convert to numeric
  #' Returns:
  #'   dataframe with same dimensions as input but with some columns converted to another type
  dfOut <- df
  if (!is.null(colsDates)) {
    for (c in 1:length(colsDates)) {
      dfOut[colsDates[c]] <- as.Date(unlist(dfOut[colsDates[c]]), format="%Y-%m-%d")
    }
  }
  if (!is.null(colsBoolean)) {
    for (c in 1:length(colsBoolean)) {
      value <- toupper(unlist(dfOut[colsBoolean[c]]))
      dfOut[colsBoolean[c]] <- ifelse(value == "TRUE", TRUE,
                                      ifelse(value == "FALSE", FALSE,
                                             NA))
      rm(value)
    }
  }
  if (!is.null(colsNumeric)) {
    for (c in 1:length(colsNumeric)) {
      dfOut[colsNumeric[c]] <- as.numeric(unlist(dfOut[colsNumeric[c]]))
    }
  }
  dfOut
}


ExtractNumberAfterString <- function(text, string) {
  #' Extract first number that occurs after a string within a text field.
  #' Args:
  #'   text: text or vector of text values that will be scanned
  #'   string: string that will be searched for within text, after which first number is extracted
  #' Returns:
  #'   value or vector of values taken from whatever first follows the string in each text entry
  pattern <- paste0(string, "\\D*(\\d+)")
  matches <- regexpr(pattern, text)
  results <- ifelse(matches > 0, regmatches(text, matches), "0")
  numbers <- as.numeric(gsub("\\D", "", results))
  numbers
}


FormatResultsFull <- function(df) {
  #' Format data frame of Congress API results (output of ImportCongressApiResults()),
  #' filtering to unique records and keeping/combining selected fields.
  #' Uses functions: ConvertColumnsCharToOther(), ExtractNumberAfterString()
  #' Args:
  #'   df: data frame with Congress API results (output of ImportCongressApiResults())
  #' Returns:
  #'   list with $information = data frame with bill info, $content = data frame wtih bill text
  #'   data frames have one row per bill_id (using latest action date)
  # Filter rows
  dfResults <- arrange(df, desc(latest_major_action_date), desc(introduced_date), bill_id)
  dfResults <- dfResults[!duplicated(dfResults['bill_id']),]
  # Format columns
  dfResults <- ConvertColumnsCharToOther(
    dfResults,
    colsDates=c("introduced_date", "latest_major_action_date", "last_vote", 
                "enacted", "vetoed", "house_passage", "senate_passage"),
    colsBoolean=c("active"),
    colsNumeric=c("cosponsors"))
  dfResults["search_query"] <- gsub("_", " ", unlist(dfResults["search_query"]))
  # Combine/split columns
  dfResults$cosponsors_dem <- ExtractNumberAfterString(dfResults$cosponsors_by_party, string="D")
  dfResults$cosponsors_rep <- ExtractNumberAfterString(dfResults$cosponsors_by_party, string="R")
  dfResults$cosponsors_ind <- ExtractNumberAfterString(dfResults$cosponsors_by_party, string="I")
  dfResults$sponsor <- paste0(dfResults$sponsor_name, " (", dfResults$sponsor_party, "-", dfResults$sponsor_state, ")")
  # Final output
  colsId <- c('bill_id', 'bill_type', 'number')
  colsReference <- c('bill_uri', 'govtrack_url')
  colsIntroduction <- c('committees', 'introduced_date', 'sponsor', 
                        'cosponsors', 'cosponsors_dem', 'cosponsors_rep', 'cosponsors_ind')
  colsPassage <- c('active', 'latest_major_action_date', 'latest_major_action', 'last_vote', 
                   'enacted', 'vetoed', 'house_passage', 'senate_passage')
  colsContent <- c('primary_subject', 'short_title', 'title', 'summary_short', 'summary')
  colsSearch <- c('search_type', 'search_query')
  list(
    information=dfResults[c(colsId, colsReference, colsIntroduction, colsPassage)],
    content=dfResults[c(colsId, colsContent, colsSearch)]
  )
}

