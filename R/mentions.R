# Title:          mentions.R
# Description:    Analyze NGO mentions
# Author:         Andrew Heiss
# Last updated:   2014-08-01
# R version:      ≥3.0

# Load packages
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
library(grid)

# Load data
load("../Data/mentions_data.RData")

# Find which organizations are cited the most
orgs.mentioned <- all.mentions %>%
  group_by(organization) %>%
  summarize(mentions=sum(num.mentions)) %>%
  arrange(desc(mentions)) %>%
  filter(mentions > 10)
# BUG: Look through list and see why there are so many 0s, like Hisham Mubarak

# Disaggregate top organizations by publication
mention.summary <- all.mentions %>%
  group_by(publication, organization) %>%
  summarize(mentions=sum(num.mentions)) %>%
  arrange(desc(mentions)) %>%
  ungroup() %>%
  filter(organization %in% orgs.mentioned$organization) %>%
  mutate(organization = ordered(organization, levels=rev(unique(organization)))) %>%
  mutate(publication = factor(publication, labels=c("Al-Ahram  ", "Daily News Egypt  ", "Egypt Independent")))

# Make a pretty graph
p <- ggplot(mention.summary, aes(x=organization, y=mentions, fill=publication))
p + geom_bar(stat="identity", position="dodge") + coord_flip() + 
  labs(x=NULL, y="Articles where mentioned") + 
  theme_bw(10) + 
  theme(legend.position="bottom", legend.key.size=unit(.7, "line"), 
        legend.key=element_blank()) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#e6ab02"), name="")



# List of organizations by publication
library(reshape2)
dcast(all.mentions, organization ~ publication, value.var="num.mentions")
