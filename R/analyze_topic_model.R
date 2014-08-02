# Title:          analyze_topic_model.R
# Description:    Do stuff with the topic model
# Author:         Andrew Heiss
# Last updated:   2014-08-01
# R version:      ≥3.0

# Load libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)

# Load data
load("../Output/topic_model.RData")
load("../Output/mentions_data.RData")

# MAYBE: Normalize per organization, not overall (or maybe not normalize?)

# Add publication name
topic.docs$publication <- factor(regmatches(row.names(topic.docs), regexpr("^[^_]+", row.names(topic.docs))), 
                                 labels=c("Al-Ahram English", "Daily News Egypt", "Egypt Independent"), ordered=TRUE)

rownames(topic.docs) <- gsub("\\.txt", "", rownames(topic.docs))


blah <- melt(all.mentions[,-ncol(all.mentions)], id.vars=c("organization", "publication"), na.rm=TRUE)

blah1 <- blah %>%
  group_by(organization) %>%
  filter(value == TRUE)



# Calculate summaries of normalized proportions
topic.means.wide <- topic.docs.norm %>%
  group_by(publication) %>%
  summarise_each(funs(mean))  # Or funs(mean, sd) to get both

topic.means.long <- melt(topic.means.wide, id="publication", variable.name="topic", value.name="proportion")
# topic.means.long$label <- factor(topic.means.long$topic, labels=topic.keys.result$short.names)

# Get reverse topic order for correct plotting
# topic.order <- topic.keys.result[order(topic.keys.result$dirichlet, decreasing=FALSE), "short.names"]
# topic.means.long$label <- factor(topic.means.long$label, levels=topic.order, ordered=TRUE)


