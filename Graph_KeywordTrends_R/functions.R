#' Define functions for project
#' Inputs: n/a
#' Outputs: n/a

#### LOAD PACKAGES ####

# Load packages
reqPackages <- c("readxl", "dplyr", "tidyr", "tidytext", "corpus", "tm", "SnowballC", "lubridate", "ggplot2")
lapply(reqPackages, function(x) if(!require(x, character.only = TRUE)) install.packages(x))
rm(reqPackages)


#### GENERAL ####


StatsTTest <- function(m1, m2, s1, s2, n1, n2, m0=0, equal.variance=FALSE) {
  #' T-test for difference given sample statistics instead of raw data.
  #' Adapted from User Macro at https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
  #' Args:
  #'   m1, m2: sample means
  #'   s1, s2: sample standard deviations
  #'   n1, n2: sample sizes
  #'   m0: null value for difference in means, default 0
  #'   equal.variance: whether to assume equal variance, default FALSE
  #' Returns:
  #'   Named vector with difference, std err, t-stat, and p-value
  if (equal.variance==FALSE) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) )
    df <- n1+n2-2
  }
  t <- (m1-m2-m0)/se 
  dat <- c(m1-m2, se, t, 2*pt(-abs(t),df))    
  names(dat) <- c("Difference of means", "Std Error", "t", "p-value")
  return(dat) 
}


#### IMPORT DATA ####


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


FormatCongressApiResults <- function(df) {
  #' Format data frame of Congress API results (output of ImportCongressApiResults()),
  #' filtering to unique records and keeping/combining selected fields.
  #' Uses functions: SpreadJoinColumn(), ConvertColumnsCharToOther(), ExtractNumberAfterString()
  #' Args:
  #'   df: data frame with Congress API results (output of ImportCongressApiResults())
  #' Returns:
  #'   data frame with bill info and brief text, with one row per bill_id (using latest action date)
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
  dfResults[c(colsId, colsReference, colsIntroduction, colsPassage, colsContent, colsSearch)]
}


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
    if (file.exists(file)) {
      dfTemp <- read.table(file, header=header, sep=sep, quote=quote, na.strings="", comment.char="",
                           fileEncoding="UTF-8", strip.white=TRUE, stringsAsFactors=FALSE)
      if (addSource) {
        parts <- unlist(strsplit(file, split="[\\/]"))
        dfTemp['source_path'] = file
        dfTemp['source_file'] = parts[length(parts)]
      }
      dfResults <- rbind(dfResults, dfTemp)
    }
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


ImportKeywordFile <- function(file, sepLabel="=", sepWords=",") {
  #' Import keyword file, with rows in the format "label = keyword1, keyword2, ...".
  #' Args:
  #'   file: string, path and name of file to read in
  #'   sepLabel: character, separator between label and keyword(s) on each row
  #'   sepWords: character, separator between keyword(s) on a row
  #' Returns:
  #'   named vector, where names are labels and elements are regex string with keywords
  df <- read.table(file, header=FALSE, sep=sepLabel, quote="", stringsAsFactors=FALSE)
  labels <- c()
  for (i in 1:nrow(df)) {
    label <- df[i, 1]
    label <- trimws(label)
    label <- tolower(label)
    label <- gsub("[^a-z0-9]", "_",label)
    labels[i] <- label
  }
  keywords <- c()
  for (i in 1:nrow(df)) {
    keyword <- unlist(strsplit(df[i, 2], sepWords))
    for (j in 1:length(keyword)) {
      word <- keyword[j]
      word <- trimws(word)
      word <- tolower(word)
      word <- gsub('"\\|', '', word)
      keyword[j] <- word
    }
    keywords[i] <- paste0("(", paste0(keyword, collapse="|"), ")")
  }
  retVal <- keywords
  names(retVal) <- labels
  retVal
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
    if (file.info(manual)$size != 0) {
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


#### DESCRIBE TEXT ####


FlagTextKeywords <- function(text, labels=NULL, keywords="") {
  #' Flag keyword(s) contained in text entries.
  #' Args:
  #'   text: vector of strings, each one a text entry to be scanned for keywords
  #'   labels: vector of strings, labels to apply to each keyword set scanned,
  #'           will be used for variable names (must be same length as keywords)
  #'   keywords: vector of strings, regex strings representing each keyword set scanned,
  #'             will be used to flag text entries (must be same length as labels)
  #' Returns:
  #'   data frame with columns for each keyword set being scanned (named using labels arg);
  #'   columns are boolean and indicate which text entries have the corresponding keyword set
  if (is.null(labels)) {
    labels <- paste0("keyword", 1:length(keywords))
  }
  retVal <- data.frame(stringsAsFactors=FALSE, rows=1:length(text))
  for (i in 1:length(keywords)) {
    label <- labels[i]
    keyword <- keywords[i]
    retVal[label] <- grepl(paste0("\\b", keyword, "\\b"), text, ignore.case=TRUE)
  }
  retVal <- select(retVal, -rows)
  retVal
}


ConvertFlagsToName <- function(df, cols=NULL, falsechar="") {
  #' Convert column(s) with TRUE/FALSE into character where TRUE = colname, FALSE = blank.
  #' Args:
  #'   df: data frame with columns to convert
  #'   cols: character vector, column names to convert to character, leave blank to apply to all logical columns
  #'   falsechar: string, character(s) to use to replace FALSE values
  #' Returns:
  #'   data frame with boolean columns replaced with character
  if (is.null(cols)) {
    classes <- unlist(lapply(df, class))
    cols <- names(df)[classes == "logical"]
  }
  if (length(cols) > 0) {
    for (col in cols) {
      df[col] <- ifelse(unlist(df[col]), col, falsechar)
    }
  }
  df
}


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
  cleaned[is.na(cleaned)] <- ""
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


ExtractKeywordsViaDtm <- function(df, n=10, stoplist=NULL, dropNumbers=TRUE, stemWords=TRUE) {
  #' Extract keywords from text entries using document-term matrix.
  #' Args:
  #'   df: data frame with doc_id and text columns
  #'   n: integer, number of keywords to extract
  #'   stoplist: (optional) vector of strings, words to disregard when extracting keywords
  #'   dropNumbers: boolean, whether to drop numbers from results
  #'   stemWords: boolean, whether to apply word stemming
  #' Returns:
  #'   vector of top keywords
  # Build corpus
  docs <- Corpus(DataframeSource(df))
  # Clean text
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  if (!is.null(stoplist)) {
    docs <- tm_map(docs, removeWords, stoplist)
  }
  if (dropNumbers) {
    docs <- tm_map(docs, removeNumbers)
  }
  if (stemWords) {
    docs <- tm_map(docs, stemDocument)
  }
  docs <- tm_map(docs, stripWhitespace)
  # Build matrix
  dtm <- TermDocumentMatrix(docs)
  dtm_matrix <- as.matrix(dtm)
  dtm_vector <- sort(rowSums(dtm_matrix), decreasing=TRUE)
  dtm_words <- data.frame(stringsAsFactors=FALSE, word=names(dtm_vector), freq=dtm_vector)
  retVal <- dtm_words$word[1:n]
  retVal
}


ExtractKeywordsByGroup <- function(text, group=NULL, n=10, stoplist=NULL, dropNumbers=TRUE, stemWords=TRUE) {
  #' Extract keywords from text entries, optionally separated by a group.
  #' Uses functions: ExtractKeywordsViaDtm()
  #' Args:
  #'   text: vector of strings, text entries to extract keywords from
  #'   group: (optional) vector, group values to use for text, vector must be same length as text
  #'   n: integer, number of keywords to extract per group
  #'   stoplist: (optional) vector of strings, words to disregard when extracting keywords
  #'   dropNumbers: boolean, whether to drop numbers from results
  #'   stemWords: boolean, whether to apply word stemming
  #' Returns:
  #'   data frame with keywords and rank for each group
  # Build data
  df <- data.frame(stringsAsFactors=FALSE, doc_id=1:length(text), text=text)
  if (is.null(group)) {
    df$group <- "1"
    group_list <- "1"
  } else {
    df$group <- group
    df$group[is.na(df$group)] <- "NA"
    group_list <- unique(df$group)
  }
  # Extract keywords
  retVal <- data.frame()
  for (g in 1:length(group_list)) {
    group_g <- group_list[g]
    if (sum(df$group == group_g) > 5) {
      keywords_g <- ExtractKeywordsViaDtm(df[df$group == group_g, c("doc_id", "text")],
                                          n=n, stoplist=stoplist, dropNumbers=dropNumbers, stemWords=stemWords)
      newrows_g <- data.frame(stringsAsFactors=FALSE, group=group_g, keywords=keywords_g, rank=1:length(keywords_g))
    } else {
      keywords_g <- ""
      newrows_g <- data.frame(stringsAsFactors=FALSE, group=group_g, keywords=as.character(NA), rank=as.character(NA))
    }
    retVal <- rbind(retVal, newrows_g)
  }
  # Remove group if not needed
  if (is.null(group)) {
    retVal$group <- NULL
  }
  retVal
}


ExtractFeaturesByGroup <- function(x, features, group) {
  #' Extract features from data frame columns, comparing groups to overall population to identify standout features.
  #' Args:
  #'   x: data frame with columns from which to extract features
  #'   features: vector of column names to use for extracting features,
  #'             numeric columns -> mean-based, logical/categorical columns -> frequency-based
  #'   group: vector, group values to use for features, vector must be same length as number of columns in df
  #' Returns:
  #'   data frame with features that are outside-the-dataframe-norm for each group,
  #'   contains columns: group (each group name), feature (each feature variable name), 
  #'   value (statistic or feature name being compared), comparison (values compared between in/out group)
  # Build data
  dfAll <- x[features]
  dfAll$group <- group
  dfAll$group[is.na(dfAll$group)] <- "NA"
  group_list <- unique(dfAll$group)
  
  # Extract notable features for each group value
  groupFeatures <- data.frame()
  for (g in 1:length(group_list)) {
    group_g <- group_list[g]
    group_flag <- dfAll$group == group_g
    
    # Loop through features
    featureFlagAll <- data.frame()
    for (feature in features) {
      values <- dfAll[[feature]]
      featureFlag <- NULL
      
      if (class(values) == "character") {
        # Character -> flag values with higher/lower proportions in group
        if (length(unique(values)) > 256 | length(unique(values))/length(values) > 0.10) {
          featureFlag <- NULL
        } else {
          # build comparison of shares of each value
          trialsIn <- as.data.frame(stringsAsFactors=FALSE, table(values[group_flag]))
          trialsOut <- as.data.frame(stringsAsFactors=FALSE, table(values[!group_flag]))
          trials <- full_join(trialsIn, trialsOut, by="Var1", suffix=c(".in", ".out"))
          names(trials) <- c("value", "countIn", "countOut")
          trials[is.na(trials)] <- 0
          trials$totalIn <- sum(group_flag)
          trials$totalOut <- sum(!group_flag)
          trials$shareIn <- trials$countIn / trials$totalIn
          trials$shareOut <- trials$countOut / trials$totalOut
          trials$stdIn <- sqrt(((trials$shareIn) * (1-trials$shareIn))/trials$countIn)
          trials$stdOut <- sqrt(((trials$shareOut) * (1-trials$shareOut))/trials$countOut)
          # flag which values have a significant difference (rough estimate)
          valuesFlagged <- c()
          for (r in 1:nrow(trials)) {
            row <- trials[r, ]
            # flag only if more than 5 in/out of group
            if (row$countIn > 5 & row$countOut > 5) {
              diff <- row$shareIn - row$shareOut
              diff_stdIn <- abs(diff / row$stdIn)
              diff_stdOut <- abs(diff / row$stdOut)
              if (!any(is.na(c(diff, diff_stdIn, diff_stdOut)))) {
                # flag only if diff more than 2.326 sd apart
                if (min(diff_stdIn, diff_stdOut) > 2.326) {
                  valuesFlagged <- c(valuesFlagged, row$value)
                }
              }
              rm(diff, diff_stdIn, diff_stdOut)
            }
          }
          rm(r, row)
          # define flagged group characteristics
          if (length(valuesFlagged) > 0) {
            trialsFlagged <- trials[trials$value %in% valuesFlagged, ] %>%
              mutate(description = paste0(
                round(shareIn, 2) * 100, "% (n=", totalIn, ")",
                " vs ",
                round(shareOut, 2) * 100, "% (n=", totalOut, ")"
              ))
            featureFlag <- trialsFlagged$description
            names(featureFlag) <- trialsFlagged$value
            rm(trialsFlagged)
          } else {
            featureFlag <- NULL
          }
          # cleanup
          rm(trialsIn, trialsOut, trials, valuesFlagged)
        }

      } else if (class(values) == "numeric") {
        # Numeric -> flag group if significantly different mean
        if (sum(group_flag) > 5 & sum(!group_flag) > 5) {
          ttest <- t.test(values[group_flag], values[!group_flag])
          if (ttest$p.value <= 0.01) {
            description <- paste0(
              round(ttest$estimate[1], 4), " (n=", sum(group_flag), ")",
              " vs ",
              round(ttest$estimate[2], 4), " (n=", sum(!group_flag), ")"
            )
            featureFlag <- description
            names(featureFlag) <- "mean"
            rm(description)
          } else {
            featureFlag <- NULL
          }
          rm(ttest)
        } else {
          featureFlag <- NULL
        }
        
      } else if (class(values) == "logical") {
        # Logical -> flag group if significantly different proportion
        if (sum(group_flag) > 5 & sum(!group_flag) > 5) {
          ttest <- t.test(values[group_flag], values[!group_flag])
          if (ttest$p.value <= 0.01) {
            description <- paste0(
              round(ttest$estimate[1], 2) * 100, "% (n=", sum(group_flag), ")",
              " vs ",
              round(ttest$estimate[2], 2) * 100, "% (n=", sum(!group_flag), ")"
            )
            featureFlag <- description
            names(featureFlag) <- "proportion"
            rm(description)
          } else {
            featureFlag <- NULL
          }
          rm(ttest)
        } else {
          featureFlag <- NULL
        }
        
      }
      
      # Add onto list of feature flags
      if (!is.null(featureFlag)) {
        featureFlagAll <- rbind(featureFlagAll, 
                                data.frame(stringsAsFactors=FALSE, 
                                           feature=feature, 
                                           value=names(featureFlag), 
                                           comparison=featureFlag))
      }
      rm(featureFlag)
      
    }
    
    # Add onto list of group features
    if (nrow(featureFlagAll) > 0) {
      groupFeatures <- rbind(groupFeatures,
                             data.frame(stringsAsFactors=FALSE, 
                                        group=group_g, 
                                        feature=featureFlagAll$feature, 
                                        value=featureFlagAll$value,
                                        comparison=featureFlagAll$comparison))
    }
    rm(featureFlagAll)
  }
  
  groupFeatures
}


#### TABLES AND GRAPHS ####


BuildTextSummary <- function(group, text=NULL, df_keywords=NULL, df_features=NULL) {
  #' Build text summary by group values, using additional information from keywords and features data.
  #' Args:
  #'   group: vector, contains values for each group in the category to be examined, used for counts
  #'   text: (optional) vector of strings, contains text description of each document, used for displaying examples,
  #'         must be same length as group if defined
  #'   df_keywords: (optional) data frame from output of ExtractKeywordsByGroup, 
  #'                must have group and keyword columns
  #'   df_features: (optional) data frame from output of ExtractFeaturesByGroup, 
  #'                must have group, feature, value, and comparison columns
  #' Returns:
  #'   data frame with row for each group, and columns group (group name), count, share,
  #'   and optionally examples (up to 3 examples), keywords, and/or features
  retVal <- data.frame()
  group[is.na(group)] <- "NA"
  grouplist <- unique(group)
  for (g in 1:length(grouplist)) {
    group_g <- grouplist[g]
    summary_g <- data.frame(stringsAsFactors=FALSE,
                            group = group_g,
                            count = sum(group == group_g),
                            share = round(sum(group == group_g) / length(group), 2))
    if (!is.null(text)) {
      examples <- text[group == group_g]
      if (length(examples) > 3) {
        examples <- sample(examples, 3)
      }
      examples <- substr(examples, 1, 96)
      examples <- paste0(examples, collapse="\n")
      summary_g$examples <- examples
      rm(examples)
    }
    if (!is.null(df_keywords)) {
      keywords <- df_keywords[df_keywords$group == group_g, "keywords"]
      if (length(keywords) == 0) {
        keywords <- ""
      }
      summary_g$keywords <- keywords
      rm(keywords)
    }
    if (!is.null(df_features)) {
      features_all <- df_features[df_features$group == group_g, c("feature", "value", "comparison")]
      if (nrow(features_all) > 0) {
        features_all$comparison <- gsub(" \\(n=(\\d|\\.|,)+\\)", "", features_all$comparison)
        features_list <- paste0(features_all$feature, " ", features_all$value, " (", features_all$comparison, ")")
        features <- paste0(features_list, collapse="\n")
        rm(features_list)
      } else {
        features <- ""
      }
      summary_g$features <- features
      rm(features_all, features)
    }
    retVal <- rbind(retVal, summary_g)
  }
  retVal
}


WriteGroupDescriptions <- function(x, title) {
  #' Describe groups in data using columns with count, share, keywords, examples, and features.
  #' Args:
  #'   x: dataframe with group descriptions, must have columns: count, share, keywords, examples, and features,
  #'      usually is output of BuildTextSummary() function.
  #'   title: (optional) string with title for descriptions
  #' Returns:
  #'   vector of lines of HTML for group descriptions
  # Headers and separators
  exampleCharLimit <- 320
  if (is.null(title)) {
    title <- "Group Descriptions"
  } else {
    title <- paste0(title, " Descriptions")
  }
  header <- paste0("<h1>", title, " (", nrow(x), " groups)", "</h1>")
  rowStart <- "<table border=0><tr><td>"
  rowEnd <- "</td></tr></table>"
  rowIndent <- "&nbsp; &nbsp; &nbsp; "
  # Build lines
  lines <- c(header, "<p></p>")
  for (r in 1:nrow(x)) {
    row <- x[r, ]
    lineHeader <- paste0("<b>Group #", r, ":</b> ", row$group, "<br />")
    lineCounts <- paste0("<b>Records:</b> ", row$count, " (", row$share * 100, "%)", "<br />")
    lineKeywords <- paste0("<b>Keywords:</b> ", "<br />", rowIndent, row$keywords, "<br />")
    lineFeatures <- paste0("<b>Notable Features:</b> ", "<br />",
                           gsub("\n", "<br />", row$features, fixed=TRUE),
                           "<br />")
    lineExamples <- paste0("<b>Document Examples:</b> ", "<br />",
                           gsub("\n", "<br />", substr(row$examples, 1, exampleCharLimit), fixed=TRUE),
                           "<br />")
    newlines <- c(rowStart, 
                  "<p>", lineHeader, lineCounts, lineKeywords, "</p>",
                  "<p>", lineFeatures, "</p>",
                  "<p>", lineExamples, "</p>",
                  rowEnd)
    lines <- c(lines, newlines)
  }
  lines
}

