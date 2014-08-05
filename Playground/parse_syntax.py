#!/usr/bin/env python3


import nltk

tree1 = nltk.Tree('NP', ['John'])
print(tree1)
tree2 = nltk.Tree('NP', ['the', 'man'])
tree3 = nltk.Tree('VP', ['saw', tree2])

# tree3.draw()

groucho_grammar = nltk.CFG.fromstring("""
S -> NP VP
PP -> P NP
NP -> Det N | Det N PP | 'I'
VP -> V NP | VP PP
Det -> 'an' | 'my'
N -> 'elephant' | 'pajamas'
V -> 'shot'
P -> 'in'
""")


sent = ['I', 'shot', 'an', 'elephant', 'in', 'my', 'pajamas']
parser = nltk.ChartParser(groucho_grammar)
for tree in parser.parse(sent):
    print(tree)