#!/usr/bin/env python3

#---------------
# Load modules
#---------------
import sqlite3
import csv
import glob
import os
import pickle


#---------------------------
# Find ids of NGO mentions
#---------------------------
# Load list of NGOs from csv file
with open('../Data/ngos.csv', 'r') as f:
  ngo_list = csv.reader(f)
  next(ngo_list, None)  # Skip the header row (alternatively save to a variable)
  organizations = [org[0] for org in ngo_list]  # Read just the first column

# Load all corpora
databases = glob.glob('../Corpora/*.db')

# Start the dictionary
mentions = {}

# Query each database for mentions and save to dictionary
for db in databases:
  conn = sqlite3.connect(db, detect_types=sqlite3.PARSE_DECLTYPES)
  conn.row_factory = sqlite3.Row  # Use a dictionary cursor
  c = conn.cursor()

  # Query using the organization names
  org_sql = ['article_content_no_punc LIKE "%'+org.lower()+'%"' for org in organizations]
  sql_statement = 'SELECT id_article FROM articles WHERE article_word_count < 10000 AND ('+' OR '.join(org for org in org_sql) + ') AND article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\''
  c.execute(sql_statement)

  # Fetch the results
  results = c.fetchall()

  # Save results to dictionary
  publication = os.path.splitext(os.path.basename(db))[0]
  mentions[publication] = [row['id_article'] for row in results]


#------------------
# Save everything
#------------------
# As a pickle for Python stuff
mentions_pickle = open('../Output/ngo_mentions.obj', 'wb') 
pickle.dump(mentions, mentions_pickle)

# As a csv for R stuff
with open('../Output/ngo_mention_ids.csv', 'w') as f:
  csv_out = csv.writer(f, delimiter=',', quoting=csv.QUOTE_ALL)
  csv_out.writerow(['publication', 'id_article'])
  for pub in mentions.keys():
    for id_article in mentions.get(pub):
      row = (pub, id_article)
      csv_out.writerow(row)
