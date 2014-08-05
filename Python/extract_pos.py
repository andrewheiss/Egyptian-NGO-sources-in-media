#!/usr/bin/env python3

# Title:          extract_pos.py
# Description:    
# Author:         Andrew Heiss
# Last updated:   2014-08-05
# Python version: ≥3.0
# Usage:          


#-----------------
# Import modules
#-----------------
import nltk
import nltk.data
import sqlite3
import csv
import xlsxwriter
import re
import pickle


#---------------------------------
# Load files and configure stuff
#---------------------------------
finished_file = 'Output/source_list_WILL_BE_OVERWRITTEN.xlsx'

# Load lists of NGOs and article ids
with open('Data/ngos.csv', 'r') as f:
  ngo_list = csv.reader(f)
  next(ngo_list, None)  # Skip the header row (alternatively save to a variable)
  organizations = [org[0] for org in ngo_list]  # Read just the first column

with open('Data/ngo_mentions.obj', 'rb')  as p:
  db_ids = pickle.load(p)

# Load NLTK sentence tokenizer
sentence_detector = nltk.data.load('tokenizers/punkt/english.pickle')

# Compile big organization regex
org_regex = re.compile("(" + "|".join([org.lower() for org in organizations]) + ")")


#------------------------
# Initialize Excel file
#------------------------
# Excel can't handle Unicode in CSV files, so write all the data to an Excel file instead
global_row = 0  # xlsxwriter doesn't have a writerow(function), so we have to keep track of what row we're on
columns = ['id_article', 'publication', 'sentence_number', 'organization', 'verbs', 'sentence_before', 'sentence', 'sentence_after', 'title', 'url', 'source', 'source_type']
workbook = xlsxwriter.Workbook(finished_file)  # Create new spreadsheet
worksheet = workbook.add_worksheet()  # Make new worksheet
for col in range(0, len(columns)):  # Write the header row
  worksheet.write(global_row, col, columns[col])
global_row += 1


#-------------------------------
# Loop through all the corpora
#-------------------------------
for db in db_ids.keys():
  database = 'Corpora/{0}.db'.format(db)
  
  #------------------------------------
  # Connect to and query the database
  #------------------------------------
  conn = sqlite3.connect(database, detect_types=sqlite3.PARSE_DECLTYPES)
  conn.row_factory = sqlite3.Row  # Use a dictionary cursor
  c = conn.cursor()

  # Query using article ids
  sql_statement = ("""SELECT * FROM articles WHERE id_article IN ({0})""".format(', '.join('?' for _ in db_ids[db])))
  c.execute(sql_statement, db_ids[db])

  # Fetch the results
  ngo_mentions = c.fetchall()


  #-----------------------------------
  # Extract mentions and parse verbs
  #-----------------------------------
  # Loop through each article and extract sentences with NGO mentions, extract
  # all the verbs, and save everything to the Excel file
  for row in ngo_mentions:
    # NLTK's tagger returns the following parts of speech (not a complete list):
    #   noun (NN), adjective (JJ), determiner (DT), verb (VB), noun phrase (NP),
    #   sentence subject (SBJ), and prepositional noun phrase (PNP)
    article = row['article_content_no_tags']
    sentences = sentence_detector.tokenize(article)

    # Split the article into sentences
    sentences_lower = [sentence.lower() for sentence in sentences]
    
    # Get a dictionary of all the sentences that mention one of the organizations
    # {sentence_id: [org1, org2, ...]}
    org_sentences = {i:org for i, sent in enumerate(sentences_lower) for org in [org_regex.findall(sent)] if org}


    # Loop through all the sentences with NGO mentions
    for current_sentence in org_sentences.keys():
      # Tag parts of speech for the sentence
      tagged = nltk.pos_tag(nltk.word_tokenize(sentences[current_sentence]))
      verbs = [verb[0] for verb in tagged if 'VB' in verb[1]]

      # Get article information
      id_article = row['id_article']
      title = row['article_title']
      url = row['article_url']

      # Extract the organizations from the org_sentences dictionary
      orgs_mentioned = org_sentences.get(current_sentence)
      
      # Find the surrounding sentences
      sentence = sentences[current_sentence]
      sentence_before, sentence_after = "NA", "NA"

      if current_sentence > 0:
        sentence_before = sentences[current_sentence-1]

      if current_sentence < (len(sentences) - 1):
        sentence_after = sentences[current_sentence+1]

      # Empty columns
      source = "NA"
      source_type = "NA"

      # Write a row for each organiaztion mentioned in the sentence
      for org in orgs_mentioned:
        spreadsheet_row = [id_article, db, current_sentence+1, org, ", ".join(verbs), sentence_before, sentence, sentence_after, title, url, source, source_type]
        for col in range(0, len(columns)):
          worksheet.write(global_row, col, spreadsheet_row[col])
        global_row += 1


# My work here is done.
workbook.close()  
