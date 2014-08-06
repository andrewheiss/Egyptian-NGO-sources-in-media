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
  select(c(1, 22:28, 2:21))  # Reorder columns


# summarise_each wipes out existing columns, so get the counts first
# TODO: Subset by used_as_source once that data's done
ngo.counts <- org.source.topics %>% 
  group_by(organization, publication) %>% summarise(count=n()) %>% arrange(desc(count))

org.topics <- org.source.topics %>%
  group_by(organization, publication) %>%
  summarise_each_q(funs(mean), 9:28) %>%  # Or funs(mean, sd) to get both
  left_join(ngo.counts, by=c("organization", "publication")) %>%
  arrange(desc(count))

org.topics.long <- melt(org.topics, id.vars=c("organization", "publication"), 
                        variable.name="topic") %>%
  filter(topic != "count")

# BUG: The rows with 0 counts are missing, leading to goofy bar widths
p <- ggplot(data=org.topics.long, aes(x=organization, y=value, fill=publication))
p + geom_bar(stat="identity", position="dodge") + 
  labs(x=NULL, y="Average proportion") + 
  theme_bw(10) + 
  theme(legend.position="bottom", legend.key.size=unit(.7, "line"), 
        legend.key=element_blank()) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#e6ab02"), name="") + 
  coord_flip() + facet_wrap(~ topic)
