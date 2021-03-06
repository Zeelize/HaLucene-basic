# HaLucene-basic
Basic implementation of core Lucene functions in Haskell. Will be extended over time.

[![Build Status](https://travis-ci.com/Zeelize/HaLucene-basic.svg?token=1i9swdUXVgvPsqEuiqDd&branch=master)](https://travis-ci.com/Zeelize/HaLucene-basic)

## Description

HaLucene-basic project is divided into two parts. 

First part is to implement core of the Lucene library with StandardAnalyzer, IndexWriter, IndexSearcher and word Autocorrect. Each of these modules contains functions for HaLucene library to work with the simple examples. Everything will be done only for the English language. 

Second part is to show usage of the implemented HaLucene library with e-shop product example. Main goal is to load CSV file with product ids, product descriptions and keywords and for user query return top _X_ relevant products which suits best for the user input.

Both parts and their parts are specified in __Tasks__ section. 

## Tasks

1. HaLucene library
    * AutoCorrect of words and queries (misspelling up to mistakes of the second order)
    * StandardAnalyzer (case-sensitive/non-case sensitive)
        * Deleting stop-words
        * Basic stemmer
    * IndexWriter
        * Indexing of documents/terms
        * tf-idf calculations and storing (!!! NOT IMPLEMENTED - it is storing only terms and DOC where)
    * IndexSearcher
        * Word/Query searcher in already prepared indexed documents.
        * Returning relevant documents

2. E-shop product searcher
    * Loading of formatted CSV file with a products and their description and keywords
    * Read user query and usage of AutoCorrect
    * Use of StandardAnalyzer
    * Use of IndexWriter
    * Return *X* the most relevant products for user query.

## Usage

* For buiding HaLucene run: `stack build`
* For running tests: `stack test`
* For running example for Home-depot products run: `stack exec halucene`
* For documentation run: `stack haddock`

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE)
file for more details.
