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
with open('Data/ngos.csv', 'r') as f:
  ngo_list = csv.reader(f)
  next(ngo_list, None)  # Skip the header row (alternatively save to a variable)
  organizations = [org[0] for org in ngo_list]  # Read just the first column

# Load all corpora
databases = glob.glob('Corpora/*.db')

# Start the dictionary
mentions = {}
counts = {}
month_counts = {}

# Query each database for mentions and save to dictionary
for db in databases:
  conn = sqlite3.connect(db, detect_types=sqlite3.PARSE_DECLTYPES)
  conn.row_factory = sqlite3.Row  # Use a dictionary cursor
  c = conn.cursor()

  # Get the number of articles in each publication
  count_sql = 'SELECT COUNT(id_article) AS n FROM articles WHERE article_word_count < 10000 AND article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\''
  c.execute(count_sql)
  count_results = c.fetchall()

  # Get the number of articles per month
  month_sql = 'SELECT strftime(\'%Y-%m\', article_date) as month_year, count(id_article) as n FROM articles WHERE article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\' GROUP BY strftime(\'%Y-%m\', article_date)'
  c.execute(month_sql)
  month_results = c.fetchall()

  # Search each corpus for mentions of NGOs
  org_sql = ['article_content_no_punc LIKE "%'+org.lower()+'%"' for org in organizations]
  sql_statement = 'SELECT id_article FROM articles WHERE article_word_count < 10000 AND ('+' OR '.join(org for org in org_sql) + ') AND article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\''
  c.execute(sql_statement)
  results = c.fetchall()

  # Save results to dictionary
  publication = os.path.splitext(os.path.basename(db))[0]
  mentions[publication] = [row['id_article'] for row in results]
  counts[publication] = [row['n'] for row in count_results]
  month_counts[publication] = [(row['month_year'], row['n']) for row in month_results]


#------------------
# Save everything
#------------------
# As a pickle for Python stuff
mentions_pickle = open('Data/ngo_mentions.obj', 'wb') 
pickle.dump(mentions, mentions_pickle)

# As a csv for R stuff
with open('Data/ngo_mention_ids.csv', 'w') as f:
  csv_out = csv.writer(f, delimiter=',', quoting=csv.QUOTE_ALL)
  csv_out.writerow(['publication', 'id_article'])
  for pub in mentions.keys():
    for id_article in mentions.get(pub):
      row = (pub, id_article)
      csv_out.writerow(row)

with open('Data/publication_counts.csv', 'w') as f:
  csv_out = csv.writer(f, delimiter=',', quoting=csv.QUOTE_ALL)
  csv_out.writerow(['publication', 'count'])
  for pub in counts.keys():
    for count in counts.get(pub):
      row = (pub, count)
      csv_out.writerow(row)

with open('Data/month_year_counts.csv', 'w') as f:
  csv_out = csv.writer(f, delimiter=',', quoting=csv.QUOTE_ALL)
  csv_out.writerow(['publication', 'date', 'count'])
  for pub in month_counts.keys():
    for month_count in month_counts.get(pub):
      row = (pub, month_count[0], month_count[1])
      csv_out.writerow(row)
