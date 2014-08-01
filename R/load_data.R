# Title:          load_data.R
# Description:    Load all corpus data into R, clean it up, save as .RData file for later use
# Author:         Andrew Heiss
# Last updated:   2014-08-01
# R version:      ≥3.0

# Load packages
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(parallel))


#--------------
# Import data
#--------------
# Connect to the databases
drv <- dbDriver("SQLite")
egind.con <- dbConnect(drv, "../Corpora/egypt_independent.db")
ahram.con <- dbConnect(drv, "../Corpora/ahram.db")
dne.con <- dbConnect(drv, "../Corpora/dne.db")

# Query the databases
date.range <- "article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\'"
egind.articles <- dbGetQuery(egind.con, paste("SELECT * FROM articles WHERE article_word_count < 10000 AND", date.range))  # Ignore new constitution
ahram.articles <- dbGetQuery(ahram.con, paste("SELECT * FROM articles WHERE", date.range))
dne.articles <- dbGetQuery(dne.con, paste("SELECT * FROM articles WHERE", date.range))


#----------------
# Clean up data
#----------------
# There's probably a better way to do this, like with a function or something. But when I use
# add.dates <- function(dataset) {
#   dataset$actual_date <- as.POSIXct(dataset$article_date, tz="EET")
#   ...
#   return(dataset)
# }
# R hangs indefinitely, since it's copying the whole huge dataset into memory 1+ times.
# So in the meantime, lots of repetition! :)

# Add dates
egind.articles$actual_date <- as.POSIXct(egind.articles$article_date, tz="EET")
egind.articles$month <- floor_date(egind.articles$actual_date, "month")
egind.articles$week <- floor_date(egind.articles$actual_date, "week")
egind.articles$day <- floor_date(egind.articles$actual_date, "day")

ahram.articles$actual_date <- as.POSIXct(ahram.articles$article_date, tz="EET")
ahram.articles$month <- floor_date(ahram.articles$actual_date, "month")
ahram.articles$week <- floor_date(ahram.articles$actual_date, "week")
ahram.articles$day <- floor_date(ahram.articles$actual_date, "day")

dne.articles$actual_date <- as.POSIXct(dne.articles$article_date, tz="EET")
dne.articles$month <- floor_date(dne.articles$actual_date, "month")
dne.articles$week <- floor_date(dne.articles$actual_date, "week")
dne.articles$day <- floor_date(dne.articles$actual_date, "day")


#-----------------------------------
# Create subsets that mention NGOs
#-----------------------------------
ngos <- read.csv("../Data/ngos.csv")

# Need to pass the articles variable as a function so that parallel 
# can see it in the worker environment
find.mentions <- function(x, articles) {
  grepl(paste(tolower(x)), articles)
}

# Start cluster
cl <- makeCluster(getOption('cl.cores', detectCores()))

# Find every instance where an NGO is mentioned in an article
# Data frame returns a big matrix: 
#   row.names The.Cairo.Institute...  Misryon.Against...  
# 1 egind_193 FALSE                   FALSE 
# 2 egind_240 FALSE                   FALSE 
# 3 egind_241 FALSE                   FALSE
egind.mentions.raw <- data.frame(parSapply(cl, X=ngos$search.name, FUN=find.mentions, 
                                           egind.articles$article_content_no_punc))
rownames(egind.mentions.raw) <- egind.articles$id_article
colnames(egind.mentions.raw) <- ngos$clean.name

ahram.mentions.raw <- data.frame(parSapply(cl, X=ngos$search.name, FUN=find.mentions, 
                                           ahram.articles$article_content_no_punc))
rownames(ahram.mentions.raw) <- ahram.articles$id_article
colnames(ahram.mentions.raw) <- ngos$clean.name

dne.mentions.raw <- data.frame(parSapply(cl, X=ngos$search.name, FUN=find.mentions, 
                                         dne.articles$article_content_no_punc))
rownames(dne.mentions.raw) <- dne.articles$id_article
colnames(dne.mentions.raw) <- ngos$clean.name

# Close everything up
stopCluster(cl)


# Rearrange mention data
# Select rows where any of the organizations are mentioned
egind.mentions <- egind.mentions.raw[apply(egind.mentions.raw, MARGIN=1, function(x) any(x==TRUE)), ]

# Get the ids of mentioned articles
egind.mentions.ids <- rownames(egind.mentions)

# Make nicer row names
rownames(egind.mentions) <- paste0("egypt_independent_", rownames(egind.mentions))

# Transpose so that organizations become rows
egind.mentions <- data.frame(organization=colnames(egind.mentions),
                             publication="Egypt Independent", t(egind.mentions))


# Repeat for the other two publications
ahram.mentions <- ahram.mentions.raw[apply(ahram.mentions.raw, MARGIN=1, function(x) any(x==TRUE)), ]
ahram.mentions.ids <- rownames(ahram.mentions)
rownames(ahram.mentions) <- paste0("ahram_", rownames(ahram.mentions))
ahram.mentions <- data.frame(organization=colnames(ahram.mentions),
                             publication="Al-Ahram", t(ahram.mentions))

dne.mentions <- dne.mentions.raw[apply(dne.mentions.raw, MARGIN=1, function(x) any(x==TRUE)), ]
dne.mentions.ids <- rownames(dne.mentions)
rownames(dne.mentions) <- paste0("dne_", rownames(dne.mentions))
dne.mentions <- data.frame(organization=colnames(dne.mentions),
                           publication="Daily News Egypt", t(dne.mentions))


# Combine everything into one data frame
# * dplyr gets mad about one-level factors initially, but the first mutate() call 
#   fixes them anyway, so suppress the warnings
suppressWarnings(
  all.mentions <- rbind_all(list(egind.mentions, ahram.mentions, dne.mentions)) %>%
  mutate(publication = factor(publication)) %>%
  mutate(organization = gsub("\\.", " ", organization))
)
all.mentions$num.mentions <- rowSums(all.mentions[, c(-1, -2)], na.rm=TRUE)


# Add indicator variables to main data frame
egind.articles <- egind.articles %>%
  mutate(ngo.mention = ifelse(id_article %in% egind.mentions.ids, TRUE, FALSE))
ahram.articles <- ahram.articles %>%
  mutate(ngo.mention = ifelse(id_article %in% ahram.mentions.ids, TRUE, FALSE))
dne.articles <- dne.articles %>%
  mutate(ngo.mention = ifelse(id_article %in% dne.mentions.ids, TRUE, FALSE))


#-----------------
# Save for later
#-----------------
save(ngos, all.mentions,
     file="../Output/mentions_data.RData", compress="gzip")
save(ngos, egind.articles, ahram.articles, dne.articles,
     file="../Output/full_text.RData", compress="gzip")
