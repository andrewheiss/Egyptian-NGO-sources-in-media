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
library(pander)
library(vcd)

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
  filter(ignore_duplicate == "No", byline == "No") %>%
  mutate(used_as_source = factor(used_as_source)) %>%
  mutate(source_type = factor(source_type)) %>%
  mutate(source_who = factor(source_who)) %>%
  mutate(individual = factor(individual)) %>%
  mutate(name.lc = organization) %>%  # Make key
  left_join(ngos, by="name.lc") %>%  # Bring in nice NGO names
  mutate(organization = clean.name) %>%  # Rename organization variable
  select(-c(sentence_number, verbs, sentence_before, sentence, byline,
            sentence_after, url, name.lc, search.name, ignore_duplicate, 
            id_article, clean.name, alternate)) %>%  # Remove extra columns
  select(c(1, 22:28, 2:21))  # Reorder columns


# # Find articles where one organization is mentioned twice. 
# # Manually create ignore_duplicate variable in the Excel file
# # Direct quote > Paraphrase > Passing reference > Report/Statement > None
# multiple.mentions.ids <- org.source.topics %>%
#   group_by(organization, article_id) %>%
#   filter(row_number() > 1)
# multiple.mentions.ids$article_id


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


# Export table of counts by publication
mention.summary <- dcast(ngo.counts, organization ~ publication, value.var="count") %>%
  group_by(organization) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

# MAYBE: Would be nice to put this in the main dplyr chain
# Oooh dplyr chain nested in the mutate() call...
mention.summary <- mention.summary %>% 
  mutate(Total = mention.summary %.% select(2:4) %.% rowSums()) %>%
  arrange(desc(Total))

# Export to Markdown
cat(pandoc.table.return(mention.summary, split.tables=Inf, digits=3,
                        justify="left", caption="Number of organization mentions in each publication"),
    file="../Output/mention_summary.md")


# Crosstabs!
# Only look at articles where NGOs are used as sources
just.sources <- org.source.topics %>% filter(used_as_source == "Yes")

# Collapse levels
levels(just.sources$source_type) <- c("Direct quote", "Paraphrase", 
                                      "Paraphrase", "Statement", "Statement")

# Build contingency table
source.type.pub <- xtabs(~ publication + source_type, data=just.sources)

# Statistical tests for contingency tables
chisq.test(source.type.pub)
coindep_test(source.type.pub, n=5000)

# Build mosaic plots
mosaic(source.type.pub, pop=FALSE, 
       labeling_args=list(set_varnames=c(source_type="Type of source", 
                                         publication="Publication")), 
       gp=gpar(fill=matrix(c("#e41a1c", "#377eb8", "#e6ab02"), 3, 3)))
labeling_cells(text=source.type.pub)(source.type.pub)  # Add counts

assoc(source.type.pub, labeling_args=list(set_varnames=c(
  source_type="Type of source", publication="Publication")))


# Output nice proportion table
nice.table <- as.data.frame(addmargins(prop.table(source.type.pub, 2), 1)) %.%
  mutate(Freq = paste(round(Freq*100, 2), "%", sep=""))
nice.table <- dcast(nice.table, publication ~ source_type, value.var="Freq")
cat(pandoc.table.return(nice.table, justify="left", 
                        caption="Percent of source type per publication"),
    file="../Output/source_publication_table.md")
