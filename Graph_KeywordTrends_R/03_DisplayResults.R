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

testdata <- cbind(dfGroups[c("introduced_year", "primary_subject", "bill_type", "top_action", "cosponsor_party")],
                  dfResults[c("introduced_date", "latest_major_action_date", "cosponsors")]) %>%
  mutate(testValue = cosponsors,
         testTime = introduced_date,
         testCat1 = cosponsor_party,
         testCat2 = top_action) %>%
  select(testValue, testTime, testCat1, testCat2)

x=testdata
ycol="testValue" # "testValue", NULL
xcol="testCat1"  # "testCat1", "testTime" <- this will change shape of graph
gcol="testCat2"  # "testCat2", NULL <- this will change whether one or multiple categories
ycolStat="count"
xcolTime="y"

GraphCategories <- function(x, ycol=NULL, xcol=NULL, gcol=NULL, ycolStat="count", xcolTime=NULL) {
  #' Graph data using lines/bars separated by groups. Graph type depends on input data types.
  #' Args:
  #'   x: input data frame with all records to be graphed, must have columns named in ycol/xcol/gcol
  #'   ycol: name of column corresponding to y-axis values, leave blank to use n (count),
  #'         must be numeric, will use mean if multiple values in group
  #'   xcol: name of column corresponding to x-axis values,
  #'         if category/logical then will create bar graph, if numeric/date then will create line graph
  #'   gcol: name of column corresponding to group values, leave blank to not use groups
  #'         must be character or coerced to character, will generate multiple bars or lines with legend
  #'   ycolStat: statistic to use to summarize ycol (if defined) if there are multiple values per group;
  #'             valid values: "count", "sum", "mean", "median", "min", "max"
  #'   xcolTime: character string indicating how xcol should be translated to date, leave blank if not needed;
  #'             "y" = convert xcol to year, "ym" = convert to year-month, "ymd" = convert to year-month-day
  #' Returns:
  #'   plot object
  # Define data
  df <- x[NULL]
  if (!is.null(gcol)) {
    df$g <- as.character(x[[gcol]])
  } else {
    df$g <- ""
  }
  if (!is.null(xcol)) {
    df$x <- x[[xcol]]
  } else {
    df$x <- 1
  }
  if (!is.null(ycol)) {
    df$y <- as.numeric(x[[ycol]])
  } else {
    df$y <- 1
  }
  if (!is.null(xcolTime) & is.Date(df$x)) {
    if (toupper(xcolTime) == "Y") {
      df$x <- as.Date(format(df$x, "%Y-01-01"))
    } else if (toupper(xcolTime) == "YM") {
      df$x <- as.Date(format(df$x, "%Y-%m-01"))
    } else if (toupper(xcolTime) == "YMD") {
      df$x <- as.Date(format(df$x, "%Y-%m-%d"))
    }
  }
  if (!is.null(ycolStat) & !is.null(ycol)) {
    df <- group_by(df, g, x)
    if (toupper(ycolStat) == "COUNT") {
      df <- summarize(df, y=n())
    } else if (toupper(ycolStat) == "SUM") {
      df <- summarize(df, y=sum(y, na.rm=TRUE))
    } else if (toupper(ycolStat) == "MEAN") {
      df <- summarize(df, y=mean(y, na.rm=TRUE))
    } else if (toupper(ycolStat) == "MEDIAN") {
      df <- summarize(df, y=median(y, na.rm=TRUE))
    } else if (toupper(ycolStat) == "MIN") {
      df <- summarize(df, y=min(y, na.rm=TRUE))
    } else if (toupper(ycolStat) == "MAX") {
      df <- summarize(df, y=max(y, na.rm=TRUE))
    }
  } else {
    df <- group_by(df, g, x)
    df <- summarize(df, y=n())
  }

  # Define graph features
  if (class(df$x) %in% c("character", "logical")) {
    graphShape <- "bar"
  } else {
    graphShape <- "line"
  }
  if (!is.null(gcol)) {
    graphMulti <- TRUE
  } else {
    graphMulti <- FALSE
  }
  if (!is.null(ycol)) {
    ylabel <- ycol
  } else {
    ylabel <- "count"
  }
  if (!is.null(xcol)) {
    xlabel <- xcol
  } else {
    xlabel <- ""
  }
  if (!is.null(gcol)) {
    glabel <- gcol
    gvalues <- unique(df$g)
    gpalette <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928')
    gpalette <- gpalette[1:length(gvalues)]
    #TODO: refine palette colors for specific values, and whether bar or line
  } else {
    glabel <- ""
    gvalues <- ""
    gpalette <- c("#666666")
  }
  #TODO: revise ymin/ymax

  # Generate graph
  if (graphShape == "bar" & !graphMulti) {
    plot <- ggplot(df, aes(x=x, y=y)) +
      geom_bar(stat="identity", width=0.75, color="black", position=position_dodge()) +
      scale_x_discrete(breaks=NULL, labels=gvalues) +
      scale_fill_manual(values=gpalette) +
      labs(title=paste0(ylabel, " by ", xlabel), x=xlabel, y=ylabel) +
      theme_minimal()
  }
  if (graphShape == "bar" & graphMulti) {
    plot <- ggplot(df, aes(x=x, y=y, fill=g)) +
      geom_bar(stat="identity", width=0.75, color="black", position=position_dodge()) +
      scale_x_discrete(breaks=gvalues, labels=gvalues) +
      scale_fill_manual(values=gpalette) +
      labs(title=paste0(glabel, " by ", xlabel), x=xlabel, y=ylabel, fill=glabel) +
      theme_minimal() + theme(legend.position="right")
  }
  if (graphShape == "line" & !graphMulti) {
    plot <- ggplot(df, aes(x=x, y=y)) +
      geom_line() + geom_point() +
      labs(title=paste0(ylabel, " by ", xlabel), x=xlabel, y=ylabel) +
      scale_color_manual(name=NULL, values=gpalette) +
      theme_minimal()
  }
  if (graphShape == "line" & graphMulti) {
    plot <- ggplot(df, aes(x=x, y=y, group=g, color=g)) +
      geom_line() + geom_point() +
      labs(title=paste0(glabel, " by ", xlabel), x=xlabel, y=ylabel, group=glabel) +
      scale_color_manual(name=glabel, values=gpalette) +
      theme_minimal() + theme(legend.position="right")
  }
  #TODO: test the above and fill in missing label elements (e.g., when x values get cut off)
  plot
}


#TODO: graph output with predefined features

#### GRAPH OUTPUT - DYNAMIC ####

#TODO: graph output with dynamic features from dfGraphInstructions, using dfGraphInstructions

#### BUILD DASHBOARD ####

output_dashboard <- file.path(OUTPUT_FOLDER, "dashboard")
if (!dir.exists(output_dashboard)) {
  dir.create(output_dashboard, recursive=TRUE)
}

#TODO: build dashboards using text output, graph output - main, graph output - dynamic
