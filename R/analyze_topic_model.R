# Title:          analyze_topic_model.R
# Description:    Do stuff with the topic model
# Author:         Andrew Heiss
# Last updated:   2014-08-06
# R version:      ≥3.0

# Load libraries
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(grid)
library(XLConnect)

# Load topic and mentions data
load("../Data/topic_model.RData")
load("../Data/mentions_data.RData")

# Load NGO data
# TODO: Don't use Excel, because lame
org.sources.wb <- loadWorkbook("../Data/source_list.xlsx")
setMissingValue(org.sources.wb, ".")
org.sources.full <- readWorksheet(org.sources.wb, sheet="Sheet1")

# Add primary keys
topic.docs <- topic.docs %>%
  mutate(article_id = gsub("\\.txt", "", rownames(topic.docs)))

org.sources <- org.sources.full %>%
  mutate(article_id = paste(publication, id_article, sep="_"))

ngos <- ngos %>%
  mutate(name.lc = tolower(search.name))


# Make mega data frame, listed by organization rows
org.source.topics <- merge(topic.docs, org.sources, by="article_id") %>%
  mutate(publication = factor(publication, 
                              labels=c("Al-Ahram English", "Daily News Egypt", 
                                       "Egypt Independent"), 
                              ordered=TRUE)) %>%  # Make a bunch of factors
  mutate(used_as_source = factor(used_as_source)) %>%
  mutate(author = factor(author)) %>%
  mutate(source_type = factor(source_type)) %>%
  mutate(source_who = factor(source_who)) %>%
  mutate(name.lc = organization) %>%  # Make primary key
  left_join(ngos, by="name.lc") %>%  # Bring in nice NGO names
  mutate(organization = clean.name) %>%  # Rename organization variable
  select(-c(sentence_number, verbs, sentence_before, sentence, 
            sentence_after, url, name.lc, search.name, 
            id_article, clean.name, alternate)) %>%  # Remove extra columns
  select(c(1, 22:28, 2:21))  # Reorder

# MAYBE: Normalize per organization, not overall (or maybe not normalize?)

blah <- melt(all.mentions[,-ncol(all.mentions)], id.vars=c("organization", "publication"), na.rm=TRUE)

blah1 <- blah %>%
  group_by(organization) %>%
  filter(value == TRUE)



# Calculate summaries of normalized proportions
topic.means.wide <- topic.docs %>%
  group_by(publication) %>%
  summarise_each(funs(mean))  # Or funs(mean, sd) to get both

topic.means.long <- melt(topic.means.wide, id="publication", variable.name="topic", value.name="proportion")
# topic.means.long$label <- factor(topic.means.long$topic, labels=topic.keys.result$short.names)

# Get reverse topic order for correct plotting
# topic.order <- topic.keys.result[order(topic.keys.result$dirichlet, decreasing=FALSE), "short.names"]
# topic.means.long$label <- factor(topic.means.long$label, levels=topic.order, ordered=TRUE)


