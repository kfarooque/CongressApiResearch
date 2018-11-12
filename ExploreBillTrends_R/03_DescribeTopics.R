#' Model topics of bills.
#' Inputs: dfInformation, dfContent, dfBills,
#'         ldaBetas, ldaGammas, ldaTopics 
#' Outputs: topics_summary_*.txt (saved to results/ folder)

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
if (!exists("ldaBetas")) {
  load(file.path(OUTPUT_ROOT, "ldaBetas.RData"))
}
if (!exists("ldaGammas")) {
  load(file.path(OUTPUT_ROOT, "ldaGammas.RData"))
}
if (!exists("ldaTopics")) {
  load(file.path(OUTPUT_ROOT, "ldaTopics.RData"))
}

#### BUILD BILL DESCRIPTORS ####

dfDescription <- dfContent[, c("bill_id", "primary_subject", "title", "summary")] %>%
  left_join(dfInformation[, c("bill_id", "introduced_date", "latest_major_action_date", "enacted", "house_passage", "senate_passage",
                              "sponsor", "cosponsors_dem", "cosponsors_rep", "cosponsors_ind")],
            by="bill_id") %>%
  left_join(ldaTopics[, c("id", "topic", "probability")], by=c("bill_id"="id")) %>%
  mutate(description = ifelse(!is.na(summary), summary, title),
         description = ifelse(!is.na(primary_subject), paste0("(", primary_subject, ") ", description), description),
         passed = max(house_passage, senate_passage),
         flag_enacted = (!is.na(enacted)),
         flag_passed = (!is.na(house_passage) | !is.na(senate_passage)),
         sponsor_party = gsub("^.*\\((.)-.*\\)$", "\\1", sponsor),
         sponsor_party_num = ifelse(sponsor_party == "D", -1, 
                                    ifelse(sponsor_party == "R", 1, 0)),
         cosponsors_dem = cosponsors_dem + (sponsor_party == "D"),
         cosponsors_rep = cosponsors_rep + (sponsor_party == "R"),
         cosponsors_ind = cosponsors_ind + (sponsor_party == "I"),
         cosponsors_sum = cosponsors_dem + cosponsors_rep + cosponsors_ind,
         cosponsor_party = ifelse(cosponsors_sum == 0, sponsor_party,
                                  ifelse(cosponsors_dem/cosponsors_sum >= 0.7, "D",
                                         ifelse(cosponsors_rep/cosponsors_sum >= 0.7, "R",
                                                "B"))),
         cosponsor_party_num = round((cosponsors_rep - cosponsors_dem) / cosponsors_sum, 2)) %>%
  select(bill_id, description, introduced_date, latest_major_action_date, flag_enacted, flag_passed, 
         sponsor_party, sponsor_party_num, cosponsor_party, cosponsor_party_num,
         topic, probability)

#### BUILD TOPIC DESCRIPTORS ####

exampleTermCount <- 10
exampleDocumentCount <- 5

termsTopicTop <- ExtractTopicsTopTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
termsTopicDistinct <- ExtractTopicsDistinctTerms(ldaBetas, betaFilter=0.001, n=exampleTermCount) %>%
  JoinValuesByGroup("topic", "term", sep=", ")
documentsTopicTop <- ExtractTopicsTopDocuments(dfDescription, ldaGammas, idcol="bill_id", textcol="description", n=exampleDocumentCount) %>%
  select(topic, bill_id, description)

textTopicSummary <- DescribeTopicExamples(
  dfDescription, xTermsTop=termsTopicTop, xTermsDistinct=termsTopicDistinct, xDocumentsTop=documentsTopicTop, 
  title=OUTPUT_TOPIC_TITLE
)

#### PLOT TOPIC GRAPHS ####


PlotTopicGraphs <- function(seriesGroup, seriesCategory=NULL, seriesTime=NULL, timeFreq="", labelGroup=NULL, labelCategory=NULL, labelTime=NULL) {
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
  #'   Plot object of bar chart(s) or line graph(s) depending on use of group/category/time parameters.
  # Define labels
  if (is.null(labelGroup)) {
    labelGroup <- "Group"
  }
  if (is.null(labelCategory)) {
    labelCategory <- "Category"
  }
  if (is.null(labelTime)) {
    labelTime <- "Time"
  }
  paletteBars <- c("orchid", "skyblue", "orangered", "springgreen")
  paletteLines <- c('#A6CEE3','#1F78B4','#B2DF8A','#33A02C','#FB9A99','#E31A1C','#FDBF6F','#FF7F00','#CAB2D6','#6A3D9A','#FFFF99','#B15928')
  # Define data
  if (!is.null(seriesGroup)) {
    df <- data.frame(stringsAsFactors=FALSE, group=as.character(seriesGroup))
  } else {
    df <- data.frame(stringsAsFactors=FALSE, group=rep("1", max(length(seriesCategory), length(seriesTime))))
  }
  if (!is.null(seriesCategory)) {
    df["category"] <- seriesCategory
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
      scale_x_continuous(breaks=grpRange, labels=grpRange) +
      labs(title=paste0(labelGroup, " Counts"), x=labelGroup, y="Count") +
      theme_minimal()
  }
  # Histogram by category
  if (!is.null(seriesCategory) & is.null(seriesTime)) {
    plot <- ggplot(dfg, aes(x=group, y=value, fill=category)) +
      geom_bar(stat="identity", width=0.75, color="black", position=position_dodge()) +
      scale_x_continuous(breaks=grpRange, labels=grpRange) +
      scale_fill_manual(values=catPalette) +
      labs(title=paste0(labelGroup, " Counts by ", labelCategory), x=labelGroup, y="Count", fill=labelCategory) +
      theme_minimal() + theme(legend.position="bottom")
  }
  # Line graphs
  if (is.null(seriesCategory) & !is.null(seriesTime)) {
    plot <- ggplot(dfg, aes(x=time, y=value, group=group, color=group)) +
      geom_line() + geom_point() +
      scale_color_manual(values=grpPalette) +
      labs(title=paste0(labelGroup, " Counts by ", labelTime), x=labelTime, y="Count", group=labelGroup) +
      theme_minimal() + theme(legend.position="bottom")
  }
  # Line graphs grid
  if (!is.null(seriesCategory) & !is.null(seriesTime)) {
    plots <- list()
    for (g in grpRange) {
      dfgTemp <- dfg[dfg$group == g, ]
      plots[[g]] <- ggplot(dfgTemp, aes(x=time, y=value, group=category, color=category)) +
        geom_line() + geom_point() +
        scale_color_manual(values=grpPalette) +
        labs(title=paste0(labelGroup, " ", g), x=labelTime, y="Count", group=labelCategory) +
        theme_minimal() + theme(legend.position="bottom")
    }
    #TODO: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    #      use multiplot() function from here
    #      then call: plot <- multiplot(plotlist=plots, cols=3)
  }
  plot
}

seriesGroup = dfDescription$topic
seriesCategory = dfDescription$cosponsor_party
seriesTime = dfDescription$introduced_date
labelGroup = "Topic"
labelCategory = "Cosponsor Party"
labelTime = "Introduced Date"
timeFreq = "Y"


# plot <- ggplot(x, aes(date, level)) +
#   geom_line() +
#   facet_wrap(~ group, scales="free_y") +
#   labs(x = "Date", y = "Weighted Level") +
#   scale_x_date(date_labels="%Y-%m") +
#   theme(text = element_text(size=20))
# plot


#### OUTPUT SUMMARY ####



write(lines, file.path(OUTPUT_ROOT, paste0(OUTPUT_TOPIC_FILESTEM, "_", NUMBER_TOPICS, ".txt")))
