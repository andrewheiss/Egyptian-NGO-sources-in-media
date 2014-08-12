# Title:          analyze_sources_orgs.R
# Description:    Do stuff with sources and organizations
# Author:         Andrew Heiss
# Last updated:   2014-08-11
# R version:      ≥3.0

#--------------------------
# Load libraries and data
#--------------------------
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


#-------------------------
# Initial data wrangling
#-------------------------
# Add primary keys to topics
topic.docs <- topic.docs %>%
  mutate(article_id = gsub("\\.txt", "", rownames(topic.docs)))

# Add primary keys to organizations
org.sources <- org.sources.full %>%
  mutate(article_id = paste(publication, id_article, sep="_"))

# Add lower case column since the names are lc in org.sources
ngos <- ngos %>% mutate(name.lc = tolower(search.name))

# Make mega data frame of organizations + sources + topics, listed by organization rows
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


# Get the order and counts of mentioned NGOs
# summarise_each wipes out existing columns, so get the counts first
# TODO: Subset by used_as_source once that data's done
ngo.counts <- org.source.topics %>% 
  group_by(organization, publication) %>% summarise(count=n()) %>% arrange(desc(count))

# # Find articles where one organization is mentioned twice. 
# # Manually create ignore_duplicate variable in the Excel file
# # Direct quote > Paraphrase > Passing reference > Report/Statement > None
# multiple.mentions.ids <- org.source.topics %>%
#   group_by(organization, article_id) %>%
#   filter(row_number() > 1)
# multiple.mentions.ids$article_id


#----------------------------------------
# Analyze sourced vs. mentioned sources
#----------------------------------------
source.ratio <- org.source.topics %>%
  group_by(publication, used_as_source) %>%
  summarise(count = n()) %>%
  dcast(formula = publication ~ used_as_source, value.var="count") %>%
  mutate(mentions = Yes + No) %>%
  mutate(ratio = round(Yes / mentions, 2)) %>%
  select(publication, mentions, Yes, No, ratio)
colnames(source.ratio) <- c("Publication", "Mentions", "Used as source", 
                            "Only mentioned", "Source ratio")

cat(pandoc.table.return(source.ratio, justify="left", 
                        caption="NGO mentions by publication"),
    file="../Output/source_mention_ratio.md")


#----------------------------------------
# Export table of counts by publication
#----------------------------------------
mention.summary <- dcast(ngo.counts, organization ~ publication, value.var="count") %>%
  group_by(organization) %>%
  summarise_each(funs(sum(., na.rm=TRUE)))

# MAYBE: Would be nice to put this with the previous dplyr chain
mention.summary <- mention.summary %>% 
  # Oooh dplyr chain nested in the mutate() call...
  mutate(Total = mention.summary %>% select(2:4) %>% rowSums()) %>%
  arrange(desc(Total))

# Export to Markdown
cat(pandoc.table.return(mention.summary, split.tables=Inf, digits=3, justify="left", 
  caption="Number of organization mentions in each publication"),
    file="../Output/mention_summary.md")


#------------------------------------
# Analyze source type + publication
#------------------------------------
# Make "no source" a source_type. Name is confusing; sorry (not sorry)
source.no.source <- org.source.topics %>% 
  mutate(source_type = factor(ifelse(used_as_source == "No", 
                              "No", as.character(source_type))))

# Collapse levels and reorder
levels(source.no.source$source_type) <- c("Direct quote", "Paraphrase", "No source",
                                      "Paraphrase", "Statement", "Statement")
source.no.source$source_type <- factor(source.no.source$source_type, 
                                   levels=c("Direct quote", "Paraphrase", 
                                            "Statement", "No source"))

# Build contingency table
source.type.pub <- xtabs(~ publication + source_type, data=source.no.source)

# Statistical tests for contingency tables
chisq.test(source.type.pub)
coindep_test(source.type.pub, n=5000)


# Build mosaic plot
cell.colors <- cbind(matrix(c("#e41a1c", "#377eb8", "#e6ab02"), 3, 3),
                     c("#7c1116", "#193a51", "#7f5c03"))

pdf("../Output/mosaic.pdf", width=4, height=4)
mosaic(source.type.pub, pop=FALSE,
       labeling_args=list(set_varnames=c(source_type="Type of source", 
                                         publication="Publication"),
                          gp_labels=(gpar(fontsize=7))), 
       gp=gpar(fill=cell.colors), gp_varnames=gpar(fontsize=9, fontface=2))

# Add counts
labeling_cells(text=source.type.pub, gp_text=gpar(fontsize=8))(source.type.pub)
dev.off()

# assoc(source.type.pub, labeling_args=list(set_varnames=c(
#   source_type="Type of source", publication="Publication")))

# # Add individuals, just for fun
# mosaic(xtabs(~ publication + source_type + individual, data=source.no.source))

# Output nice proportion table
nice.table <- as.data.frame(addmargins(prop.table(source.type.pub, 2), 1)) %>%
  mutate(Freq = paste(round(Freq*100, 2), "%", sep=""))
nice.table <- dcast(nice.table, publication ~ source_type, value.var="Freq")
cat(pandoc.table.return(nice.table, justify="left", 
                        caption="Percent of source type per publication"),
    file="../Output/source_publication_table.md")


#----------------------------------------------------
# Analyze organizations + source type + publication
#----------------------------------------------------
# Copy data frame for more wrangling
source.orgs <- org.source.topics

# Collapse levels
levels(source.orgs$source_type) <- c("Direct quote", "Paraphrase", 
                                     "Paraphrase", "Statement", "Statement")

# Find organizations with more than 5 sourced mentions of any type
top.orgs.raw <- as.data.frame(xtabs(~ organization + source_type, 
                                    data=source.orgs)) %>%
  group_by(organization) %>%
  filter(Freq > 5) %>%
  arrange(desc(Freq))

# Save to single ordered factor
top.orgs <- unique(factor(top.orgs.raw$organization, 
                          levels=unique(top.orgs.raw$organization), ordered=TRUE))

# Filter full data to only include top sourced articles
more.than.five <- source.orgs %>%
  filter(organization %in% top.orgs) %>%
  mutate(organization = factor(organization, levels=rev(top.orgs), ordered=TRUE))

# Crosstab and convert to dataframe for plotting
org.source.pub.tab <- xtabs(~ organization + publication + source_type, 
                            data=more.than.five)
org.source.pub.tab.prop <- prop.table(org.source.pub.tab, c(1,3))
chisq.test(org.source.pub.tab)

# Pretty pictures
plot.data <- as.data.frame(org.source.pub.tab)
p <- ggplot(plot.data, aes(x=organization, y=Freq, fill=publication))
p + geom_bar(stat="identity", position="dodge") + coord_flip() + 
  labs(x=NULL, y="Articles where used as source") + 
  theme_bw(10) + 
  theme(legend.position="bottom", legend.key.size=unit(.7, "line"), 
        legend.key=element_blank()) + 
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#e6ab02"), name="") + 
  facet_wrap(~ source_type)


#-------------------------------------------------------
# Analyze average topics by organization + publication
#-------------------------------------------------------
# Find average topics for each organization in each publication
org.topics <- org.source.topics %>%
  group_by(organization, publication) %>%
  summarise_each_q(funs(mean), 9:28) %>%  # Or funs(mean, sd) to get both
  left_join(ngo.counts, by=c("organization", "publication")) %>%
  arrange(desc(count))

# Reshape for plotting
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


#--------------------------------------------------
# Analyze all the things
# topics + organizations + sources + publication 
#--------------------------------------------------
# TODO: Topics + organization
# TODO: Topics with more NGO sources by type
# TODO: Which topics use direct quotes more