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
library(lubridate)
library(scales)
library(grid)
library(readxl)
library(pander)
library(vcd)
library(Cairo)

# Load topic and mentions data
load("../Data/topic_model.RData")
load("../Data/mentions_data.RData")

# Load NGO data
org.sources.full <- read_excel("../Data/source_list.xlsx", sheet="Sheet1", na=".", 
                               col_types=c("numeric", "text", 
                                           "numeric", rep("text", 13)))


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

# Deal with topic labels and dirichlet params
short.names.actual <- c("Post-revolutionary Egypt (catch-all)", "Legislation and governance", "Muslim Brotherhood and constitution", "Military trials", "Protestors and activism", "Public economics", "Religious issues", "Police arrests", "Environmental issues", "Human rights and civil society", "Sexual violence", "Protests and clashes", "Egyptian workers", "Elections", "Trials", "Sectarian issues", "Media and censorship", "Morsi", "Police torture", "Regime politics")

topics <- topic.keys.result %>%
  mutate(topic = factor(paste0("X", key))) %>%
  mutate(label = short.names.actual) %>%
  mutate(topic.words = as.character(topic.words)) %>%
  arrange(desc(dirichlet)) %>%
  mutate(label = factor(label, levels=label, ordered=TRUE)) %>%
  mutate(label.rev = factor(label, levels=rev(label), ordered=TRUE)) %>%
  select(-short.names)


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


# Get the order and counts of sourced NGOs
# summarise_each wipes out existing columns, so get the counts first
ngo.counts <- org.source.topics %>% 
  group_by(organization, publication) %>% filter(used_as_source == "Yes") %>%
  summarise(count=n()) %>% arrange(desc(count))


# # Find articles where one organization is mentioned twice. 
# # Manually create ignore_duplicate variable in the Excel file
# # Direct quote > Paraphrase > Passing reference > Report/Statement > None
# multiple.mentions.ids <- org.source.topics %>%
#   group_by(organization, article_id) %>%
#   filter(row_number() > 1)
# multiple.mentions.ids$article_id


# Create a data frame with collapsed source levels. 
# Use this for all calls to xtabs() to maintain empty categories before melting
# Necessary because dplyr drops empty combinations when using summarize(), 
#   which leaves missing plot pieces in ggplot
melt.base <- org.source.topics
levels(melt.base$source_type) <- c("Direct quote", "Paraphrase", 
                                   "Paraphrase", "Statement", "Statement")

# Example of how to subset with xtabs + dplyr, pre-melt
# All combinations
all.combinations <- melt.base %>%
  xtabs(formula = ~ organization + publication + source_type) %>% 
  as.data.frame()

# Get the average of each topic for each org + source + publication
topics.avg <- melt.base %>%
  group_by(organization, source_type, publication) %>%
  summarise_each(funs(mean), 9:28)  # Or funs(mean, sd) to get both

# Merge average topics into the df of all combinations, set missing to 0
all.combinations.topics <- all.combinations %>%
  left_join(topics.avg, by=c("organization", "source_type", "publication"))


# Choose top organizations for plotting subset of organizations
top.organizations <- factor(c("The Egyptian Initiative for Personal Rights", 
                              "Arab Network for Human Rights Information", 
                              "The Egyptian Organization for Human Rights",
                              "The Hisham Mubarak Law Center"), ordered=TRUE)


# Find organizations with more than 5 sourced mentions of any type
top.orgs.raw <- as.data.frame(xtabs(~ organization + source_type, 
                                    data=org.source.topics)) %>%
  group_by(organization) %>%
  filter(Freq > 5) %>%
  arrange(desc(Freq))

# Save to single ordered factor
top.orgs <- unique(factor(top.orgs.raw$organization, 
                          levels=unique(top.orgs.raw$organization), ordered=TRUE))


#---------------------
# Plotting variables
#---------------------
# publication.colors <- c("#e41a1c", "#377eb8", "#e6ab02")
publication.colors <- c("#000000", "#636363", "#cccccc")
# source.colors <- c("#a741e4", "#219758", "#e86d24")
source.colors <- c("#000000", "#636363", "#cccccc")

# validation.color <- "#8e6010"
validation.color <- "#252525"

theme_clean <- function(base_size=12, base_family="Gill Sans") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Gill Sans", face="bold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size=0.25, colour="grey90"),
          axis.ticks=element_blank(),
          legend.position="bottom", 
          axis.title=element_text(size=rel(0.8), 
                                  family="Gill Sans", face="bold"),
          strip.text=element_text(size=rel(0.9), 
                                  family="Gill Sans", face="bold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin=unit(1, "lines"), legend.key.size=unit(.7, "line"),
          legend.key=element_blank())
  
  ret
}

theme_dotplot <- theme(panel.grid.major.y=element_line(size=0.25))

add.padding <- function(x) {
  first.part <- paste0(levels(x)[-length(levels(x))], "   ")
  last.part <- levels(x)[length(levels(x))]
  levels(x) <- c(first.part, last.part)
  x
}


#----------------------
# Topic model summary
#----------------------
topic.summary <- topics %>% select(dirichlet, topic.words, label)
colnames(topic.summary) <- c("Dirichlet α", " Top ten words", " Short name")
rownames(topic.summary) <- paste(" ", 1:nrow(topic.summary))  # To trick pander into thinking these are real rownames...

cat(pandoc.table.return(topic.summary, split.tables=Inf, digits=3,
                        justify="left", caption="Topic model summary {#tbl:tm_summary}"), 
    file="../Output/table_topic_model.md")


#---------------------
# Article validation
#---------------------
set.seed(1234)
validation <- melt.base %>% group_by(publication) %>% sample_n(1) 

output.table <- validation %>% ungroup() %>%
  select(article_id, publication, title)
colnames(output.table) <- c("Article ID", "Publication", "Article title")
rownames(output.table) <- NULL

cat(pandoc.table.return(output.table, split.tables=Inf, 
                        justify="left", caption="Summary of mentions and sourcing by publication {#tbl:article_validation}"),
    file="../Output/table_article_validation.md")

plot.data <- validation %>%
  select(article_id, publication, organization, 9:28) %>%
  melt(variable.name="topic",
       id.vars=c("article_id", "publication", "organization")) %>%
  filter(topic != "X0") %>%
  left_join(topics, by="topic")

p <- ggplot(plot.data, aes(x=label.rev, y=value, fill=publication))
article.validation <- p + geom_bar(stat="identity") + 
  geom_vline(xintercept=seq(length(unique(plot.data$topic)) + 3.5 - 3, 0, by=-3), 
             colour="grey80", linetype="dotted", size=0.25) + 
  coord_flip() + facet_wrap(~ article_id) + 
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values=publication.colors, guide=FALSE) + 
  labs(y="Proportion of topic in article", x=NULL) + theme_clean(7)

# TODO: ggsave outputs PNGs that are inexplicably gigantic. See http://mcfromnz.wordpress.com/2013/09/03/ggplot-powerpoint-wall-head-solution/
ggsave(article.validation, filename="../Output/plot_article_validation.png", 
       width=4.7, height=3, units="in")
ggsave(article.validation, filename="../Output/plot_article_validation.pdf", 
       width=4.7, height=3, units="in", device=cairo_pdf)


#----------------------------------
# Topic + organization validation
#----------------------------------
comb.org.pub.topic <- melt.base %>%
  filter(organization %in% top.orgs) %>%
  mutate(organization = factor(organization, levels=rev(top.orgs), ordered=TRUE)) %>%
  xtabs(formula = ~ organization + publication) %>% 
  as.data.frame() %>% select(-Freq)

# Get the average of each topic for each org + publication
topics.avg <- melt.base %>%
  group_by(organization) %>%
  summarise_each(funs(mean), 9:28)  # Or funs(mean, sd) to get both

# Merge average topics into the df org + publication
comb.org.pub.topics <- comb.org.pub.topic %>%
  left_join(topics.avg, by=c("organization")) %>%
  select(-publication)

plot.data <- melt(comb.org.pub.topics, id.vars=c("organization"), 
                  variable.name="topic") %>%
  filter(topic %in% c("X10", "X16")) %>%
  left_join(topics, by="topic") 

p <- ggplot(data=plot.data, aes(x=organization, y=value))
org.topic.validation <- p + 
  geom_bar(stat="identity", position="dodge", fill=validation.color) + 
  geom_vline(xintercept=seq(length(unique(plot.data$organization)) + 3.5 - 3, 0, by=-3), 
             colour="grey80", linetype="dotted", size=0.25) + 
  labs(x=NULL, y="Mean proportion of topic in corpus") + 
  coord_flip() + facet_wrap(~ label) + 
  theme_clean(7) + scale_y_continuous(labels=percent)

ggsave(org.topic.validation, filename="../Output/plot_org_topic_validation.png", 
       width=4.7, height=2.5, units="in")
ggsave(org.topic.validation, filename="../Output/plot_org_topic_validation.pdf", 
       width=4.7, height=2.5, units="in", device=cairo_pdf)


#----------------------------------------
# Analyze sourced vs. mentioned sources
#----------------------------------------
publication.counts <- read.csv("../Data/publication_counts.csv") %>%
  mutate(publication = factor(publication, 
                              labels=levels(org.source.topics$publication), 
                              ordered=TRUE)) %>%
  select(publication, total_count = count)

nice.percent <- function(x) {
  paste(formatC(100 * x, format="f", digits=2), "%", sep = "")
}

source.ratio <- org.source.topics %>%
  group_by(publication, used_as_source) %>%
  summarise(count = n()) %>%
  dcast(formula = publication ~ used_as_source, value.var="count") %>%
  left_join(publication.counts, by="publication") %>%
  mutate(mentions = Yes + No) %>%
  mutate(source.ratio = nice.percent(Yes / mentions)) %>%
  mutate(source.ratio.all = nice.percent(Yes / total_count)) %>%
  select(publication, total_count, mentions, Yes, No, source.ratio, source.ratio.all)
colnames(source.ratio) <- c("Publication", "Total articles", "Articles with NGO mention", "NGO used as source", 
                            "NGO only mentioned", "Source to mention ratio", "Source to total ratio")

cat(pandoc.table.return(source.ratio, justify=c("left", rep("center", 6)),
                        big.mark=",", digits=4, split.tables=Inf,
                        caption="NGO mentions and sourced mentions by publication {#tbl:source_mention_ratio}"),
    file="../Output/table_source_mention_ratio.md")


#--------------------
# Sources over time
#--------------------
load("../Data/full_text.RData")

ahram.articles <- ahram.articles %>%
  mutate(article_id = paste("ahram", id_article, sep="_")) %>%
  mutate(publication = "ahram") %>%
  select(article_id, publication, month)

dne.articles <- dne.articles %>%
  mutate(article_id = paste("dne", id_article, sep="_")) %>%
  mutate(publication = "dne") %>%
  select(article_id, publication, month)

egind.articles <- egind.articles %>%
  mutate(article_id = paste("egypt_independent", id_article, sep="_")) %>%
  mutate(publication = "egind") %>%
  select(article_id, publication, month)

article.dates <- rbind(ahram.articles, dne.articles, egind.articles) %>%
  mutate(article_id = factor(article_id)) %>%
  mutate(publication = factor(publication, 
                              labels=c("Al-Ahram English", "Daily News Egypt", 
                                       "Egypt Independent"), ordered=TRUE))

month.year.counts <- read.csv("../Data/month_year_counts.csv") %>%
  mutate(publication = factor(publication, 
                              labels=levels(org.source.topics$publication), 
                              ordered=TRUE)) %>%
  mutate(actual_date = as.POSIXct(ymd(paste0(date, "-01")), tz="EET")) %>%
  mutate(month = floor_date(actual_date, "month")) %>%
  mutate(month_factor = factor(month)) %>%
  select(publication, total_count = count, month_factor)

month.year.sources <- org.source.topics %>%
  filter(used_as_source == "Yes") %>%
  mutate(article_id = as.character(article_id)) %>%
  select(article_id) %>%
  left_join(article.dates, by="article_id") %>%
  group_by(publication, month) %>%
  summarize(source_count = n()) %>%
  mutate(month_factor = factor(month))

plot.data <- month.year.sources %>% ungroup() %>%
  left_join(month.year.counts, by=c("publication", "month_factor")) %>%
  mutate(prop = source_count / total_count) %>%
  mutate(prop = ifelse(prop > 0.2, 0, prop)) %>%  # Remove outliers
  mutate(publication = add.padding(publication))

p <- ggplot(aes(x=as.Date(month), y=prop, colour=publication), data=plot.data)
sources.over.time <- p + geom_line(size=1) + 
  scale_x_date(breaks="3 months", labels=date_format("%B %Y")) + 
  scale_y_continuous(labels=percent) + labs(x=NULL, y="Proportion of articles that source NGOs") + 
  scale_colour_manual(values=publication.colors, name="") + 
  theme_clean(7) + theme(legend.key = element_blank(),
                         panel.grid.major.y = element_line(size=0.25, colour="grey90"))

ggsave(plot=sources.over.time, filename="../Output/plot_sources_time.png", 
       width=4.7, height=2.5, units="in")
ggsave(plot=sources.over.time, filename="../Output/plot_sources_time.pdf", 
       width=4.7, height=2.5, units="in", device=cairo_pdf)


#----------------------------------------
# Export table of counts by publication
#----------------------------------------
source.summary <- dcast(ngo.counts, organization ~ publication, value.var="count") %>%
  group_by(organization) %>%
  summarise_each(funs(sum(., na.rm=TRUE))) %>%
  select(Organization=organization, 2:4)

# MAYBE: Would be nice to put this with the previous dplyr chain
source.summary <- source.summary %>% 
  # Oooh dplyr chain nested in the mutate() call...
  mutate(Total = source.summary %>% select(2:4) %>% rowSums()) %>%
  mutate(Percent = nice.percent(Total / sum(Total))) %>%
  mutate(Organization = as.character(Organization)) %>%
  arrange(desc(Total))

plot.data <- select(source.summary, -c(Total, Percent)) %>%
  melt(id.vars="Organization") %>%
  filter(Organization %in% top.orgs) %>%
  mutate(Organization = factor(Organization, levels=rev(source.summary$Organization), ordered=TRUE)) %>%
  mutate(variable = add.padding(variable))

p <- ggplot(plot.data, aes(x=Organization, y=value, fill=variable)) 
plot.source.organization <- p + geom_bar(stat="identity", position="dodge") + 
  coord_flip() + labs(x=NULL, y="Number of articles") + 
  scale_fill_manual(values=publication.colors, name="") + 
  theme_clean(7) + theme(legend.key = element_blank())

ggsave(plot=plot.source.organization, filename="../Output/plot_source_organization.png", 
       width=4.7, height=3, units="in")
ggsave(plot=plot.source.organization, filename="../Output/plot_source_organization.pdf", 
       width=4.7, height=3, units="in", device=cairo_pdf)

source.summary <- rbind(source.summary, c("Total", sum(source.summary[,2]), 
                                          sum(source.summary[,3]), 
                                          sum(source.summary[,4]), 
                                          sum(source.summary[,5]), "&nbsp;"))

# Export to Markdown
cat(pandoc.table.return(source.summary, split.tables=Inf, digits=3, 
                        justify=c("left", rep("center", 5)), 
                        caption="Frequency of use as a source in each publication {#tbl:source_organization}"),
    file="../Output/table_source_organization.md")


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
set.seed(1234)
coindep_test(source.type.pub, n=5000)


# Build mosaic plot
cell.colors <- cbind(matrix(publication.colors, 3, 3),
                     c("#000000", "#444646", "#9b9b9c"))
                     #c("#7c1116", "#193a51", "#7f5c03"))

# Save the mosaic plot to PDF
# I wish this were easier, like ggsave!
# dev.control(...) is needed to plot noninteractively
cairo_pdf("../Output/plot_mosaic.pdf", width=3, height=3)
dev.control(displaylist="enable")
mosaic(source.type.pub, pop=FALSE,
       labeling_args=list(set_varnames=c(source_type="Type of source", 
                                         publication="Publication"),
                          gp_labels=(gpar(fontsize=7, 
                                          fontfamily="Gill Sans"))), 
       gp=gpar(fill=cell.colors, col="#ffffff"), margins=unit(c(2, 0, 0, 0), "lines"),
       gp_varnames=gpar(fontsize=8, fontface=2, fontfamily="Gill Sans"))

# Add counts
labeling_cells(text=source.type.pub, 
               gp_text=gpar(fontsize=7, fontfamily="Gill Sans", col="#ffffff"))(source.type.pub)
final.mosaic <- recordPlot()
dev.off()

# Save as PNG
png("../Output/plot_mosaic.png", width=3, height=3, units="in", res=300)
final.mosaic
dev.off()

# assoc(source.type.pub, labeling_args=list(set_varnames=c(
#   source_type="Type of source", publication="Publication")))

# # Add individuals, just for fun
# mosaic(xtabs(~ publication + source_type + individual, data=source.no.source))

# Output nice proportion table
nice.table <- as.data.frame(addmargins(prop.table(source.type.pub, 2), 1)) %>%
  mutate(Freq = paste(round(Freq*100, 2), "%", sep=""))
nice.table <- dcast(nice.table, source_type ~ publication, value.var="Freq") %>%
  rename(`Source Type` = source_type)
cat(pandoc.table.return(nice.table, justify=c("left", rep("center", 4)), 
                        split.tables=Inf, caption="Frequency of source type by publication {#tbl:source_publication}"),
    file="../Output/table_source_publication.md")


#-----------------------------------------
# Source type + individual + publication
#-----------------------------------------
plot.data <- melt.base %>%
  xtabs(formula = ~ individual + publication + source_type) %>% 
  as.data.frame() %>%
  mutate(publication = add.padding(publication)) %>%
  mutate(individual = factor(individual, labels=c("Organization", "Individual")))

p <- ggplot(plot.data, aes(x=individual, fill=publication))
plot.source.pub <- p + geom_bar(aes(y=(Freq)/sum(Freq)), 
                                stat="identity", position="dodge") + 
  labs(x=NULL, y="Proportion") + 
  scale_y_continuous(labels=percent) + 
  scale_fill_manual(values=publication.colors, name="") + 
  facet_wrap(~ source_type) + 
  theme_clean(7) + theme(panel.grid.major.y = element_line(size=0.25, colour="grey90"),
                         panel.grid.major.x = element_blank())

ggsave(plot.source.pub, filename="../Output/plot_source_pub.png", 
       width=4.7, height=2.5, units="in")
ggsave(plot.source.pub, filename="../Output/plot_source_pub.pdf", 
       width=4.7, height=2.5, units="in", device=cairo_pdf)


#--------------------------------------
# Plot average topics per publication
#--------------------------------------
plot.pub.topics <- org.source.topics %>%
  filter(used_as_source == "Yes") %>%
  group_by(publication) %>%
  summarise_each(funs(mean), 9:28) %>%  # Or funs(mean, sd) to get both
  select(-X0) %>%
  melt(measure.vars=2:20, id.vars=c("publication"), 
       variable.name="topic", value.name="proportion") %>%
  left_join(topics, by="topic") %>%
  arrange(publication, proportion)

ahram.order <- plot.pub.topics %>%
  filter(publication == "Al-Ahram English") %>%
  arrange(proportion) %>% select(label)

plot.pub.topics <- plot.pub.topics %>%
  mutate(publication = add.padding(publication)) %>%
  mutate(label.ahram = factor(label, levels=ahram.order$label, ordered=TRUE))

p <- ggplot(plot.pub.topics, aes(x=label.ahram, y=proportion, colour=publication))
plot.topic.pub <- p + geom_point(size=4, alpha=0.9) + 
  labs(x=NULL, y="Mean proportion of topic in articles that mention NGOs") + 
  coord_flip() + scale_y_continuous(labels=percent, 
                                    breaks=seq(0.02, 0.1, by=0.02)) + 
  scale_colour_manual(values=publication.colors, name="") + 
  # scale_size_continuous(range = c(2, 7), 
                        # name=expression(paste("Proportion (", alpha, ")"))) + 
  theme_clean(7) + theme_dotplot

ggsave(plot.topic.pub, filename="../Output/plot_topic_pub.png", 
       width=4.7, height=3.5, units="in")
ggsave(plot.topic.pub, filename="../Output/plot_topic_pub.pdf", 
       width=4.7, height=3.5, units="in", device=cairo_pdf)


#-------------------------------------------------
# Plot topics by each organization (or top ones)
#-------------------------------------------------
# Create org + publication data frame
comb.org.pub <- melt.base %>%
  filter(used_as_source == "Yes") %>%
  xtabs(formula = ~ organization + publication) %>% 
  as.data.frame()

# Get the average of each topic for each org + source + publication
topics.avg <- melt.base %>%
  filter(used_as_source == "Yes") %>%
  group_by(organization, publication) %>%
  summarise_each(funs(mean), 9:28)  # Or funs(mean, sd) to get both

# Merge average topics into the df org + publication
comb.org.pub.topics <- comb.org.pub %>%
  left_join(topics.avg, by=c("organization", "publication"))

plot.data <- melt(comb.org.pub.topics, measure.vars=4:23, 
                  id.vars=c("organization", "publication"), 
                  variable.name="topic", value.name="proportion") %>%
  filter(organization %in% top.organizations) %>%
  filter(topic != "X0") %>%
  mutate(organization = factor(organization, levels=top.organizations)) %>%
  mutate(proportion = ifelse(is.na(proportion), 0, proportion)) %>%
  left_join(topics, by="topic") %>%
  mutate(publication = add.padding(publication)) #%>%
#   mutate(label.ahram = factor(label, levels=ahram.order$label, ordered=TRUE))

p <- ggplot(plot.data, aes(x=label.rev, y=proportion, color=publication))
plot.topic.pub.org <- p + geom_point(size=2, alpha=0.9) + 
  labs(x=NULL, y="Mean proportion of topic in articles that mention NGOs") + 
  theme_clean(6) + theme_dotplot + 
  coord_flip() + scale_y_continuous(labels=percent) +
  scale_colour_manual(values=publication.colors, name="") + 
  # scale_size_continuous(range = c(2, 7), 
                        # name=expression(paste("Proportion (", alpha, ")"))) + 
  facet_wrap(~ organization)

ggsave(plot.topic.pub.org, filename="../Output/plot_topic_pub_org.png", 
       width=4.7, height=5, units="in")
ggsave(plot.topic.pub.org, filename="../Output/plot_topic_pub_org.pdf", 
       width=4.7, height=5, units="in", device=cairo_pdf)


#--------------------------------------
# Plot average topics per source type
#--------------------------------------
# Create org + publication data frame
comb.org.pub <- melt.base %>%
  filter(used_as_source == "Yes") %>%
  xtabs(formula = ~ organization) %>% 
  as.data.frame()

# Get the average of each topic for each org + source + publication
topics.avg <- melt.base %>%
  filter(used_as_source == "Yes") %>%
  group_by(organization) %>%
  summarise_each(funs(mean), 9:28)  # Or funs(mean, sd) to get both

plot.source.topics <- melt.base %>%
  filter(used_as_source == "Yes") %>%
  group_by(source_type) %>%
  summarise_each(funs(mean), 9:28) %>%  # Or funs(mean, sd) to get both
  select(-X0) %>%
  melt(measure.vars=2:20, id.vars=c("source_type"), 
       variable.name="topic", value.name="proportion") %>%
  left_join(topics, by="topic") %>%
  arrange(source_type, proportion)

quote.order <- plot.source.topics %>%
  filter(source_type == "Direct quote") %>%
  arrange(proportion) %>% select(label)

plot.source.topics <- plot.source.topics %>%
  mutate(source_type = add.padding(source_type)) %>%
  mutate(label.quote = factor(label, levels=quote.order$label, ordered=TRUE))

p <- ggplot(plot.source.topics, aes(x=label.quote, y=proportion, colour=source_type))
plot.topic.source <- p + geom_point(size=4, alpha=0.9) + 
  labs(x=NULL, y="Mean proportion of topic in articles that mention NGOs") + 
  theme_clean(7) + theme_dotplot + 
  coord_flip() + scale_y_continuous(labels=percent,
                                    breaks=seq(0.02, 0.1, by=0.02)) + 
  scale_colour_manual(values=source.colors, name="") #+ 
  # scale_size_continuous(range = c(2, 7), 
                        # name=expression(paste("Proportion (", alpha, ")")))

ggsave(plot.topic.source, filename="../Output/plot_topic_source.png", 
       width=4.7, height=3.5, units="in")
ggsave(plot.topic.source, filename="../Output/plot_topic_source.pdf", 
       width=4.7, height=3.5, units="in", device=cairo_pdf)
