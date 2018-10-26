#' Define functions for project
#' Inputs: n/a
#' Outputs: n/a

#### LOAD PACKAGES ####

# Load packages
reqPackages <- c("readxl", "dplyr", "tidyr", "readr", "tidytext", "topicmodels", "ggplot2", "ggraph", "igraph")
lapply(reqPackages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))
rm(reqPackages)


#### IMPORT DATA ####


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


SpreadJoinColumn <- function(df, colId, colSpread, sep=" ") {
  #' Spread value of one variable and join them together into single column, by an identifier.
  #' Args:
  #'   df: data frame with identifier column and column to spread-and-rejoin
  #'   colId: name of identifier column
  #'   colSpread: name of column to spread and rejoin by identifier
  #'   sep: character(s) used to separate values of column to spread for each identifier
  #' Returns:
  #'   data frame with colId and colSpread, and one identifier per row
  dfOutput <- df[, c(colId, colSpread)]
  names(dfOutput) <- c("id", "value")
  dfOutput <- group_by(dfOutput, id) %>%
    mutate(key = paste0("var", sprintf("%04d", row_number()))) %>%
    spread(key, value, fill="") %>%
    unite(value, -id, sep=sep) %>%
    ungroup()
  dfOutput$value <- gsub(paste0("^", sep), "", dfOutput$value)
  dfOutput$value <- gsub(paste0(sep, "$"), "", dfOutput$value)
  dfOutput$value <- gsub(paste0(sep, "+"), sep, dfOutput$value)
  dfOutput$value <- trimws(dfOutput$value)
  names(dfOutput) <- c(colId, colSpread)
  dfOutput
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
  #' Uses functions: SpreadJoinColumn(), ConvertColumnsCharToOther(), ExtractNumberAfterString()
  #' Args:
  #'   df: data frame with Congress API results (output of ImportCongressApiResults())
  #' Returns:
  #'   list with $information = data frame with bill info, $content = data frame wtih bill text
  #'   data frames have one row per bill_id (using latest action date)
  # Filter rows
  dfResults <- arrange(df, desc(latest_major_action_date), desc(introduced_date), bill_id) %>%
    distinct(bill_id, .keep_all=TRUE) %>%
    select(-search_type, -search_query)
  dfSearchType <- SpreadJoinColumn(df, "bill_id", "search_type", sep=" ")
  dfSearchQuery <- SpreadJoinColumn(df, "bill_id", "search_query", sep=" ")
  dfSearchQuery$search_query <- gsub("_", " ", dfSearchQuery$search_query)
  dfResults <- left_join(dfResults, dfSearchType, by="bill_id") %>%
    left_join(dfSearchQuery, by="bill_id")
  # Format columns
  dfResults <- ConvertColumnsCharToOther(
    dfResults,
    colsDates=c("introduced_date", "latest_major_action_date", "last_vote", 
                "enacted", "vetoed", "house_passage", "senate_passage"),
    colsBoolean=c("active"),
    colsNumeric=c("cosponsors"))
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


#### BUILD STOPLIST ####


BuildTokensTfidf <- function(text, id=NULL, ngram=1) {
  #' Build dataframe of tokens, counts, and tf-idf (term-freq/inverse-doc-freq) measures.
  #' Args:
  #'   text: vector of text entries to use to create tokens
  #'   id: vector of identifiers for text (if blank then will use numbers)
  #'   ngram: number of words to use in tokens (e.g., 1 = words, 2 = bigrams, 3 = trigrams)
  #' Returns:
  #'   dataframe with id, term, n (count), tf, idf, and tf_idf columns
  if (is.null(id)) {
    id <- 1:length(text)
  }
  df <- data.frame(stringsAsFactors=FALSE, id=id, text=text)
  if (ngram == 1) {
    tokens <- unnest_tokens(df, term, text, token="words")
  } else {
    tokens <- unnest_tokens(df, term, text, token="ngrams", n=ngram)
  }
  counts <- group_by(tokens, id) %>%
    count(term) %>%
    ungroup() %>%
    arrange(id, desc(n)) %>%
    bind_tf_idf(term, id, n)
  counts
}


BuildCommonRareTerms <- function(df, nCommon=0.01, nRare=0.01, filterMinDocs=2, filterNumbers=TRUE) {
  #' Build list of most and least common terms from dataframe with tf-idf measures.
  #' Args:
  #'   df: dataframe with columns "term", "n", and "tf_idf" 
  #'       (output by dfTokens() or by unnest_tokens() and bind_tf_idf())
  #'   nCommon: number (if >1) or percent (if <1) of terms to include in 'common' list
  #'   nRare: number (if >1) or percent (if <1) of terms to include in 'rare' list
  #'   filterMinDocs: minimum number of docs a term needs to be in for either list (default 2)
  #'   filterNumbers: whether to filter out numbers
  #' Returns:
  #'   list of two vectors: $common = vector of common terms, $rare = vector of rare terms
  dfSum <- group_by(df, term) %>%
    summarize(n=sum(n), tf_idf=mean(tf_idf))
  if (filterMinDocs > 0) {
    dfSum <- dfSum[dfSum$n > filterMinDocs, ]
  }
  if (filterNumbers) {
    dfSum <- dfSum[!grepl("^.{,3}\\d+", dfSum$term), ]
    dfSum <- dfSum[!grepl("^m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})$", dfSum$term), ]
  }
  indexCommon <- order(dfSum$tf_idf, decreasing=FALSE)
  indexRare <- order(dfSum$tf_idf, decreasing=TRUE)
  if (is.na(nCommon)) {
    termsCommon <- c()
  } else if (nCommon == 0) {
    termsCommon <- c()
  } else if (nCommon < 1) {
    nCommon <- round(nrow(dfSum) * nCommon)
    termsCommon <- unlist(dfSum[indexCommon[1:nCommon], "term"])
    names(termsCommon) <- NULL
  } else {
    termsCommon <- unlist(dfSum[indexCommon[1:nCommon], "term"])
    names(termsCommon) <- NULL
  }
  if (is.na(nRare)) {
    termsRare <- c()
  } else if (nRare == 0) {
    termsRare <- c()
  } else if (nRare < 1) {
    nRare <- round(nrow(dfSum) * nRare)
    termsRare <- unlist(dfSum[indexRare[1:nRare], "term"])
    names(termsRare) <- NULL
  } else {
    termsRare <- unlist(dfSum[indexRare[1:nRare], "term"])
    names(termsRare) <- NULL
  }
  list(common=termsCommon, rare=termsRare)
}


BuildStopList <- function(vectors=NULL, manual=NULL, auto=TRUE) {
  #' Build a stop list based on automatic sources, vector(s) of terms, and manual import.
  #' Args:
  #'   vectors: vector or list of vectors with stop words
  #'   manual: string with path of file to import with stop words (one term per line)
  #'   auto: boolean, whether to use the automatic stop list from the tidytext package
  #' Returns:
  #'   vector of terms that can be used as stop list
  if (!is.null(vectors)) {
    stopwordsBuilt <- unlist(vectors)
  } else {
    stopwordsBuilt <- c()
  }
  if (!is.null(manual)) {
    if (file.info(INPUT_STOPLIST)$size != 0) {
      stopwordsImported <- read.table(manual, header=FALSE, stringsAsFactors=FALSE,
                                      sep="\n", quote="", na.strings="")
      stopwordsImported <- unlist(stopwordsImported)
      stopwordsImported <- stopwordsImported[gsub("\\s*", "", stopwordsImported) != ""]
    } else {
      stopwordsImported <- c()
    }
  } else {
    stopwordsImported <- c()
  }
  if (auto) {
    data(stop_words)
    stopwordsDefault <- stop_words[stop_words$lexicon == "SMART", "word"]
  } else {
    stopwordsDefault <- c()
  }
  stopwords <- unique(unlist(c(stopwordsBuilt, stopwordsImported, stopwordsDefault)))
  stopwords[order(stopwords)]
}


#### BUILD TOKENS ####


BuildTokensCleaned <- function(text, id=NA, ngram=1, stoplist=NULL) {
  #' Build dataframe of tokens, cleaned, for further parsing.
  #' Args:
  #'   text: vector of text entries to use to create tokens
  #'   id: vector of identifiers for text (if blank then will use numbers)
  #'   ngram: number of words to use in tokens (e.g., 1 = words, 2 = bigrams, 3 = trigrams)
  #'   stoplist = vector of terms in stop list
  #' Returns:
  #'   dataframe with id, term (cleaned), n (count)
  # Clean terms
  cleaned <- iconv(text, to="UTF-8", sub="?")
  #TODO: add more cleaning steps here
  # Build initial data
  if (is.null(id)) {
    id <- 1:length(text)
  }
  df <- data.frame(stringsAsFactors=FALSE, id=id, text=cleaned)
  # Build tokens
  if (ngram == 1) {
    tokens <- unnest_tokens(df, term, text, token="words")
  } else if (ngram >= 2) {
    tokens <- unnest_tokens(df, term, text, token="ngrams", n=n)
  }
  if (!is.null(stoplist)) {
    tokens <- filter(tokens, !(term %in% stoplist))
  }
  tokenCounts <- group_by(tokens, id) %>%
    count(term) %>%
    ungroup()
  tokenCounts
}
