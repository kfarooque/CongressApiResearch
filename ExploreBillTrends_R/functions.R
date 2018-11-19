#' Define functions for project
#' Inputs: n/a
#' Outputs: n/a

#### LOAD PACKAGES ####

# Load packages
reqPackages <- c("readxl", "dplyr", "tidyr", "readr", "tidytext", "topicmodels", "corpus", "SnowballC", "ggplot2", "ggraph", "igraph", "lubridate")
lapply(reqPackages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))
rm(reqPackages)


#### GENERAL ####


JoinValuesByGroup <- function(x, group, value, sep=";") {
  #' Spread and join one column in a dataframe by a group. Useful for term lists by group.
  #' Args:
  #'   x: dataframe with group column and value column to spread
  #'   group: name of group column
  #'   value: name of column with values to spread
  #'   sep: separator to use between entries in value column per group
  #' Returns:
  #'   dataframe with just group and joined value columns
  dfLong <- group_by(x[, c(group, value)], .dots=group) %>%
    rename("group"=group, "value"=value) %>%
    mutate(key = paste0("key", sprintf("%02d", row_number())),
           value = ifelse(is.na(value), "", value))
  dfWide <- spread(dfLong, "key", "value", fill="") %>%
    unite("key", -group, sep=sep) %>%
    mutate(key = gsub(paste0("(^", sep, ")|(", sep, "$)"), "", key),
           key = gsub(paste0("(", sep, ")+"), sep, key))
  retVal <- ungroup(dfWide)[, c("group", "key")]
  names(retVal) <- c(group, value)
  retVal
}


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


StemWordsHunspell <- function(text) {
  #' Apply stemming to vector of text entries or individual terms. Uses hunspell dictionary.
  #' Args:
  #'   text: vector of text entries or individual terms
  #' Returns:
  #'   vector with each word replaced by stem
  temp_Stemmer <- function(term) {
    #' Stemmer, based on https://cran.r-project.org/web/packages/corpus/vignettes/stemmer.html
    stems <- hunspell::hunspell_stem(term)[[1]] # look up term in dictionary
    if (length(stems) == 0) { # if there are no stems, use the original term
      stem <- term
    } else { # if there are multiple stems, use the last one
      stem <- stems[[length(stems)]]
    }
    stem
  }
  stemmedList <- text_tokens(text, stemmer=temp_Stemmer)
  stemmedVector <- c()
  for (i in 1:length(stemmedList)) {
    entry <- paste(stemmedList[[i]], collapse=" ")
    entry <- gsub("([\\(\\$#@]) ", "\\1", entry) # remove space after this punctuation
    entry <- gsub(" ([\\):;,\\.\\?!%])", "\\1", entry) # remove space before this punctuation
    entry <- gsub("'s", "", entry)
    entry <- gsub("s'", "s", entry)
    stemmedVector[i] <- entry
  }
  stemmedVector
}


BuildTokensCleaned <- function(text, id=NA, ngram=1, stoplist=NULL, stemWords=FALSE, dropNumbers=FALSE) {
  #' Build dataframe of tokens, cleaned, for further parsing.
  #' Uses functions: StemWordsHunspell()
  #' Args:
  #'   text: vector of text entries to use to create tokens
  #'   id: vector of identifiers for text (if blank then will use numbers)
  #'   ngram: number of words to use in tokens (e.g., 1 = words, 2 = bigrams, 3 = trigrams)
  #'   stoplist: vector of terms in stop list
  #'   stemWords: whether to apply word stemming
  #'   dropNumbers: whether to drop numbers from tokens
  #' Returns:
  #'   dataframe with id, term (cleaned), n (count)
  # Clean terms
  cleaned <- iconv(text, to="UTF-8", sub="?")
  if (stemWords) {
    cleaned <- StemWordsHunspell(cleaned)
  }
  if (dropNumbers) {
    cleaned <- gsub("[\\(]*\\d+[\\)]*", "", cleaned)
    cleaned <- gsub("(^| |\\()m{0,4}(cm|cd|d?c{0,3})(xc|xl|l?x{0,3})(ix|iv|v?i{0,3})($| |\\))", "", cleaned)
  }
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


#### MODELING (LDA) ####


ApplyTopicToDocuments <- function(x, betas, groupvars=NULL, filterBetas=TRUE, weightByN=TRUE) {
  #' Apply LDA model's term betas to a tokenized data frame's terms to identify most likely topic.
  #' This is a shortcut to re-running LDA model with new data added, and may yield different results.
  #' Args:
  #'   x: tokenized data frame with one row per term per document, and following columns:
  #'      group var(s) (including document id), "term" (term present), and "n" (count of term),
  #'   betas: betas matrix from LDA object with: "term" (terms used), "beta" (beta coefficient), 
  #'          and "topic" (topic identifier)
  #'   groupvars: vector of the names of the grouping var(s) in x
  #'              (if left blank then it will use everything in x except "term" and "n")
  #'   filterBetas: flag for whether to filter out low-value betas from affecting classification
  #'                (sets to 0 any betas below the lowest one for a top-ranked term)
  #'   weightByN: flag for whether to weight terms by number of occurrences in each document
  #'              (if FALSE then resets term count to 1 per document)
  #' Returns:
  #'   Data frame with one row per document, including grouping variables, topic identifier, 
  #'   and probability.
  if (is.null(groupvars)) {
    groupvars <- names(x)[!(names(x) %in% c("term", "n"))]
  }
  if (filterBetas) {
    topBetas <- group_by(betas, term) %>%
      arrange(term, desc(beta)) %>%
      filter(row_number() == 1)
    betasThreshold <- min(topBetas$beta, na.rm=TRUE)
    betas$beta <- ifelse(betas$beta < betasThreshold, 0, betas$beta)
  }
  documentsTopics <- left_join(x, betas, by="term") %>%
    mutate(n = ifelse(weightByN, n, 1)) %>%
    mutate(nbeta = ifelse(!is.na(beta), n * beta, 0)) %>%
    group_by_(.dots=c(groupvars, "topic")) %>%
    summarize(nbeta = sum(nbeta, na.rm=TRUE))
  documentsTotal <- group_by(documentsTopics, .dots=c(groupvars)) %>%
    summarize(total = sum(nbeta, na.rm=TRUE))
  documentsProbabilities <- left_join(documentsTopics, documentsTotal, by=c(groupvars)) %>%
    mutate(probability = ifelse(!is.na(total), nbeta / total, 0)) %>%
    select(-nbeta, -total) %>%
    arrange(desc(probability)) %>%
    filter(row_number() == 1)
  documentsProbabilities
}


ExtractTopicsTopTerms <- function(x, betaFilter=0.001, n=10) {
  #' Extract top terms per topic, using an LDA betas data frame with topic, term, and beta.
  #' Args:
  #'   x: betas data frame with "topic", "term", and "beta" columns.
  #'   betaFilter: minimum allowable beta value
  #'   n: number of top terms per topic
  #' Returns:
  #'   Data frame with topic, term, and beta, for just the top terms per topic.
  retVal <- group_by(x, topic) %>%
    arrange(topic, desc(beta))
  if (!is.null(betaFilter)) {
    retVal <- filter(retVal, beta >= betaFilter)
  }
  if (!is.null(n)) {
    retVal <- group_by(retVal, topic) %>%
      arrange(topic, desc(beta)) %>%
      filter(row_number() <= n) %>%
      ungroup()
  }
  ungroup(retVal)
}


ExtractTopicsDistinctTerms <- function(x, betaFilter=0.001, n=10) {
  #' Extract most distinct terms per topic, using an LDA betas data frame with topic, term, and beta.
  #' Args:
  #'   x: betas data frame with "topic", "term", and "beta" columns.
  #'   betaFilter: minimum allowable beta value
  #'   n: number of distinct terms per topic
  #' Returns:
  #'   Data frame with topic, term, and beta, for just the most distinct terms per topic.
  # topics <- unique(x$topic)[order(unique(x$topic))]
  # retVal <- mutate(x, topic = paste0("topic", topic))
  retVal <- x
  if (!is.null(betaFilter)) {
    termsDrop <- group_by(retVal, term) %>%
      summarize(max = max(beta)) %>%
      filter(max < betaFilter) %>%
      select(-max) %>%
      ungroup()
    retVal <- anti_join(retVal, termsDrop, by="term")
  }
  betaSums <- group_by(retVal, term) %>%
    summarize(betaSum = sum(beta))
  betaEach <- left_join(retVal, betaSums, by="term") %>%
    mutate(logratio = log2(beta / (betaSum-beta)),
           absratio = abs(logratio))
  retVal <- group_by(betaEach, topic) %>%
    arrange(topic, desc(absratio))
  if (!is.null(n)) {
    retVal <- group_by(retVal, topic) %>%
      arrange(topic, desc(absratio)) %>%
      filter(row_number() <= n) %>%
      ungroup()
  }
  select(retVal, topic, term, beta) %>%
    ungroup()
}


ExtractTopicsTopDocuments <- function(x, gammas=NULL, idcol="id", textcol="text", n=10) {
  #' Extract most illustrative documents per topic, using a results and optional LDA gammas data frame.
  #' Args:
  #'   x: results data frame from joining topics and original document data, with columns:
  #'      idcol, textcol, "topic", and "probability"
  #'   gammas: optional gammas data frame with "topic", "document", and "gamma" columns.
  #'   idcol: name of identifier column in x
  #'   textcol: name of text column to return as results in x
  #'   n: number of illustrative documents per topic
  #' Returns:
  #'   Data frame with all fields for just the most illustrative documents per topic.
  docsContent <- group_by(x[,c(idcol, textcol, "topic", "probability")], topic) %>%
    arrange(topic, desc(probability)) %>%
    rename("id"=idcol, "text"=textcol)
  if (!is.null(gammas)) {
    docsGammas <- mutate(gammas, id = document) %>%
      select(id, topic, gamma)
    docsContent <- left_join(docsContent, docsGammas, by=c("id", "topic")) %>%
      group_by(topic) %>%
      arrange(topic, desc(probability), desc(gamma))
  }
  docsContent <- docsContent[!duplicated(docsContent$text), ]
  if (!is.null(n)) {
    docsContent <- filter(docsContent, row_number() <= n)
  }
  retVal <- left_join(docsContent[, "id"], x, by=c("id"=idcol))
  names(retVal)[names(retVal) == "id"] <- idcol
  retVal
}


#### TABLES AND GRAPHS ####


DescribeTopicExamples <- function(x, xTermsTop=NULL, xTermsDistinct=NULL, xDocumentsTop=NULL, title=NULL) {
  #' Describe topics using key terms and examples.
  #' Args:
  #'   x: dataframe with all documents, must have columns: "topic"
  #'   xTermsTop: dataframe with top terms per topic, must have columns: "topic", "term" (optional)
  #'   xTermsDistinct: dataframe with distinct terms per topic, must have columns: "topic", "term" (optional)
  #'   xDocumentsTop: dataframe with top documents per topic, must have columns: "topic", and an ID and description field (optional)
  #'   title: string with title for results (optional)
  #' Returns:
  #'   vector of lines with topic descriptions
  # Headers and separators
  exampleCharLimit <- 256
  topics <- unique(x$topic)[order(unique(x$topic))]
  if (is.null(title)) {
    title <- "Topic Descriptions"
  }
  header <- paste0("<h1>", title, " (", length(topics), " topics)", "</h1>")
  topicStart <- "<table border=0><tr><td>"
  topicEnd <- "</td></tr></table>"
  # Build lines
  lines <- c(header, "<p></p>")
  for (t in topics) {
    lineHeader <- paste0("<b>Topic #", t, "</b>", "<br />")
    lineDocs <- paste0("<b>Documents:</b> ", sum(x$topic == t), "<br />")
    if (!is.null(xTermsTop)) {
      lineTermsTop <- paste0("<b>Top Terms:</b> ", unlist(xTermsTop[xTermsTop$topic == t, "term"]), "<br />")
    } else {
      lineTermsTop <- NULL
    }
    if (!is.null(xTermsDistinct)) {
      lineTermsDistinct <- paste0("<b>Distinct Terms:</b> ", unlist(xTermsDistinct[xTermsDistinct$topic == t, "term"]), "<br />")
    } else {
      lineTermsDistinct <- NULL
    }
    if (!is.null(xDocumentsTop)) {
      examplesHeader <- paste0("<b>Representative Documents:</b>", "<br />")
      examplesList <- select(xDocumentsTop[xDocumentsTop$topic == t,], -topic)
      names(examplesList) <- c("id", "description")
      examplesList <- examplesList[, c("id", "description")] %>%
        mutate(line = paste0("<li>", "<b>", id, ":</b> ", substr(description, 1, exampleCharLimit), "</li>"))
      lineExamples <- c(examplesHeader, "<ul>", examplesList$line, "</ul><br />")
    } else {
      lineExamples <- NULL
    }
    newlines <- c(topicStart, lineHeader, lineDocs, lineTermsTop, lineTermsDistinct, lineExamples, topicEnd)
    lines <- c(lines, newlines)
  }
   lines
}


PlotTopicGraphs <- function(seriesGroup, seriesCategory=NULL, seriesTime=NULL, timeFreq="", 
                            labelGroup="Group", labelCategory="Category", labelTime="Time") {
  #' Plot graphs of topics optionally by a category and/or time parameter.
  #' Args:
  #'   seriesGroup: vector with group values (usually topic)
  #'   seriesCategory: (optional) vector with category values (e.g., party, passage flag)
  #'   seriesTime: (optional) vector with time values (e.g., date introduced)
  #'   timeFreq: (optional) frequency by which to summarize seriesTime values
  #'             valid values: "" (none), "W" (weekly), "M" (monthly), "Y" (yearly)
  #'   labelGroup: (optional) label to use for group values
  #'   labelCategory: (optional) label to use for category values
  #'   labelTime: (optional) label to use for time values
  #' Returns:
  #'   Plot object of bar chart(s) or line graph(s) depending on use of group/category/time parameters,
  #'   or list of plot objects if multiple plots are generated.
  # Define colors
  paletteBars <- c("orchid", "skyblue", "orangered", "springgreen")
  paletteLines <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928')
  # Define data
  if (!is.null(seriesGroup)) {
    df <- data.frame(stringsAsFactors=FALSE, group=as.character(seriesGroup))
  } else {
    df <- data.frame(stringsAsFactors=FALSE, group=rep("1", max(length(seriesCategory), length(seriesTime))))
  }
  if (!is.null(seriesCategory)) {
    df["category"] <- as.character(seriesCategory)
  } else {
    df["category"] <- "1"
  }
  if (!is.null(seriesTime)) {
    if (tolower(timeFreq) == "y") {
      df["time"] <- floor_date(seriesTime, unit="year")
    } else if (tolower(timeFreq) == "m") {
      df["time"] <- floor_date(seriesTime, unit="month")
    } else if (tolower(timeFreq) == "w") {
      df["time"] <- floor_date(seriesTime, unit="week")
    } else {
      df["time"] <- seriesTime
    }
  } else {
    df["time"] <- 1
  }
  dfg <- summarize(group_by(df, group, category, time), value = n())
  grpRange <- unique(dfg$group)[order(unique(dfg$group))]
  grpPalette <- c(paletteLines, rainbow(length(grpRange) - length(paletteLines)))
  catValues <- unique(dfg$category)
  catPalette <- c(paletteBars, rainbow(length(catValues) - length(paletteBars)))
  # Histogram
  if (is.null(seriesCategory) & is.null(seriesTime)) {
    plot <- ggplot(dfg, aes(x=group, y=value)) +
      geom_bar(stat="identity", width=0.75, color="blue", fill="lightblue") +
      scale_x_discrete(breaks=grpRange, labels=grpRange) +
      labs(title=paste0(labelGroup, " Counts"), x=labelGroup, y="Count") +
      theme_minimal()
  }
  # Histogram by category
  if (!is.null(seriesCategory) & is.null(seriesTime)) {
    plot <- ggplot(dfg, aes(x=group, y=value, fill=category)) +
      geom_bar(stat="identity", width=0.75, color="black", position=position_dodge()) +
      scale_x_discrete(breaks=grpRange, labels=grpRange) +
      scale_fill_manual(values=catPalette) +
      labs(title=paste0(labelGroup, " Counts by ", labelCategory), x=labelGroup, y="Count", fill=labelCategory) +
      theme_minimal() + theme(legend.position="bottom")
  }
  # Line graphs
  if (is.null(seriesCategory) & !is.null(seriesTime)) {
    plot <- ggplot(dfg, aes(x=time, y=value, group=group, color=group)) +
      geom_line() + geom_point() +
      scale_color_manual(name=labelGroup, values=grpPalette) +
      labs(title=paste0(labelGroup, " Counts by ", labelTime), x=labelTime, y="Count", group=labelGroup) +
      theme_minimal() + theme(legend.position="bottom")
  }
  # Line graphs list
  if (!is.null(seriesCategory) & !is.null(seriesTime)) {
    plot <- list()
    for (g in grpRange) {
      dfgTemp <- dfg[dfg$group == g, ]
      plot[[g]] <- ggplot(dfgTemp, aes(x=time, y=value, group=category, color=category)) +
        geom_line() + geom_point() +
        scale_color_manual(name=labelCategory, values=grpPalette) +
        labs(title=paste0(labelGroup, " ", labelCategory, " by ", labelTime, " - ", labelGroup, " ", g), 
             x=labelTime, y="Count", group=labelCategory) +
        theme_minimal() + theme(legend.position="bottom")
    }
  }
  plot
}


SavePlotToFile <- function(plot, file, width=640, height=480, res=100) {
  #' Save plot or list of plots to .png file(s).
  #' Args:
  #'   plot: plot object or list of plot objects to save to file(s)
  #'   file: string, full path of output file (plot name is appended to end if saving a list)
  #'   width: integer, width of output in pixels
  #'   height: integer, height of output in pixels
  #'   res: integer, resolution
  #' Returns:
  #'   NULL (saves output to file)
  filestem <- gsub("\\.\\w{1,5}$", "", file)
  if (class(plot)[1] != "list") {
    filename <- paste0(filestem, ".png")
    outfile <- png(filename, width=width, height=height, res=res)
    capture.output(plot)
    dev.off()
  } else {
    plotnames <- names(plot)
    for (p in plotnames) {
      filename <- paste0(filestem, "_", p, ".png")
      outfile <- png(filename, width=width, height=height, res=res)
      capture.output(plot[p])
      dev.off()
    }
  }
  NULL
}


SaveCombinedDashboard <- function(outpath, outfile, fileDescription, fileSingle, filestemMultiple, tableColumns=3) {
  #' Save combined output from summary file, single images, and multiple images, as single dashboard file.
  #' Args:
  #'   outpath: string, path where all outputs are stored and where dashboard will be saved
  #'   outfile: string, name of output file
  #'   fileDescription: string, name of file with summary info (summary.html)
  #'   fileSingle: vector of strings, list of all individual image files (in outpath) to put in dashboard
  #'   filestemMultiple: vector of strings, list of initial part of files (in outpath) to put in tables in dasboard
  #'   tableColumns: integer, number of columns to use for images defined by filestemMultiple
  #' Return:
  #'   null; output dashboard html file is output to location given in outpath
  validFiles <- list.files(outpath)
  fileStart <- c("<html>", "<head>Topic Descriptions Dashboard</head>", "<body>")
  fileEnd <- c("</body>", "</html>")
  sepSection <- "<hr />"
  # description section
  linesDescription <- read_file(file.path(outpath, fileDescription))
  linesDescription <- unlist(strsplit(linesDescription, "\n"))
  linesDescription <- gsub("\r", "", linesDescription)
  # single images
  linesSingleImages <- c()
  for (file in fileSingle) {
    linesSingleImages <- c(linesSingleImages, paste0("<img src='", file, "'>", "<br />"))
  }
  # image groups
  linesMultipleImages <- c()
  for (filestem in filestemMultiple) {
    filestem <- gsub("\\.\\w{1,5}$", "", filestem)
    files <- validFiles[grep(filestem, validFiles)]
    imgTable <- c("<table border=0>")
    for (f in 1:(ceiling(length(files) / tableColumns) * tableColumns)) {
      if ((f-1) %% tableColumns == 0) {
        startRow <- "<tr>"
      } else {
        startRow <- ""
      }
      if ((f-1) %% tableColumns == (tableColumns-1)) {
        endRow <- "</tr>"
      } else {
        endRow <- ""
      }
      if (f <= length(files)) {
        cell <- paste0(startRow, "<td><img src='", files[f], "'>", "</td>", endRow)
      } else {
        cell <- paste0(startRow, "<td>&nbsp;</td>", endRow)
      }
      imgTable <- c(imgTable, cell)
    }
    imgTable <- c(imgTable, "</table><br /><hr />")
    linesMultipleImages <- c(linesMultipleImages, imgTable)
  }
  # complete file
  lines <- c(fileStart, sepSection, linesDescription, sepSection, linesSingleImages, sepSection, linesMultipleImages, fileEnd)
  write(lines, file.path(outpath, outfile))
  NULL
}
