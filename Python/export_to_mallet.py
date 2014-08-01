#!/usr/bin/env python3

# Title:          export_to_mallet.py
# Description:    Create a folder of individual text files for every article 
#                 that mentions an NGO, to be used in MALLET 
# Author:         Andrew Heiss
# Last updated:   2014-08-01
# Python version: ≥3.0
# Arguments:
#   database       the path to the database to read
#   prefix         the prefix for the output article id (e.g.
#                  "egypt_independent" will result in
#                  "egypt_independent_001.txt")
#   output_folder  the path to save final text files


# Import modules
import argparse
import os
import sqlite3
import csv

# Get command line information
parser = argparse.ArgumentParser(description='Export all articles in a database to individual plain text files.')
parser.add_argument('database', type=str, 
                    help='the path to the database to read')
parser.add_argument('prefix', type=str, 
                    help='the prefix for the output article id (e.g. "egypt_independent" will result in "egypt_independent_001.txt")')
parser.add_argument('output_folder', type=str, 
                    help='the path to save final text files')
args = parser.parse_args()

# Save arguments
database = os.path.abspath(args.database)
prefix = args.prefix
output_folder = os.path.abspath(args.output_folder)


#------------------------------------
# Connect to and query the database
#------------------------------------
conn = sqlite3.connect(database, detect_types=sqlite3.PARSE_DECLTYPES)
conn.row_factory = sqlite3.Row  # Use a dictionary cursor
c = conn.cursor()


# List of signatory organizations in http://www.eipr.org/en/pressrelease/2013/05/30/1720
# Load list of NGOs from csv file
with open('Data/ngos.csv', 'r') as f:
  ngo_list = csv.reader(f)
  next(ngo_list, None)  # Skip the header row (alternatively save to a variable)
  organizations = [org[0] for org in ngo_list]  # Read just the first column


# Query using the organization names
org_sql = ['article_content_no_punc LIKE "%'+org.lower()+'%"' for org in organizations]
sql_statement = 'SELECT * FROM articles WHERE ('+' OR '.join(org for org in org_sql) + ') AND article_date BETWEEN \'2011-11-24 00:00:00\' AND \'2013-04-25 23:59:59\''
c.execute(sql_statement)

# Fetch the results
results = c.fetchall()


#------------------------------------
# Write each article to a text file
#------------------------------------
for row in results:
  filename = output_folder + '/' + prefix + '_' + str(row['id_article']) + '.txt'
  with open(filename, 'w') as f:
    f.write(row['article_title'] + '\n\n')
    if row['article_subtitle']:
      f.write(row['article_subtitle'] + '\n\n')
    f.write(row['article_content_no_tags'])
