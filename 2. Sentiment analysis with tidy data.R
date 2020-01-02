### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 2: SENTIMENT ANALYSIS WITH TIDY DATA

# 2.1 THE SENTIMENTS DATASET

library(tidytext)

# affin, bing & nrc are three general-purpose lexicons

get_sentiments("afinn")

# "affin" lexicon assigns words with a score that runs between -5 and 5, with negative...
# scores indicating negative sentiment and positive socres indicating positive sentiment.

get_sentiments("bing")

# "bing" lexicon categorizes words in a binary fashion into positive and negative categories.

get_sentiments("nrc")

# "nrc" lexicon categorizes words in a binary function ("yes/"no") into categories of... 
# positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust.

# 2.2 SENTIMENT ANALYSIS WITH INNER JOIN

library(janeaustenr)
library(dplyr)
library(stringr)

austen_books()

tidy_books <- austen_books() %>%                                               # from the austen_books data frame
  group_by(book) %>%                                                           # group the rows by book
  mutate(linenumber = row_number(),                                            # add a new column "linenumber" corresponding to the row number from austen_books
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",        # add a new column "chapter" (look chapter number on text colum from austen_books)
                                                 ignore_case = TRUE)))) %>%    # ignore_case = TRUE
  ungroup() %>%                                                                # ungroup the rows by book
  unnest_tokens(word, text)                                                    # tokenization   
tidy_books

# perform sentiment analysis


nrc_joy <- get_sentiments("nrc") %>%      # from the nrc lexicon
  filter(sentiment == "joy")              # filter for joy words

tidy_books %>%                      # from the tidy_books data frame
  filter(book == "Emma") %>%        # filter the "book" column to contain works by Emma
  inner_join(nrc_joy) %>%           # perform an innjer join with the nrc_join data frame
  count(word, sort = TRUE)          # get the words count and sort them

# sentiment analysis for Jane Austen books

library(tidyr)

jane_austen_sentiment <- tidy_books %>%                      # from the tidy_books data frame
  inner_join(get_sentiments("bing")) %>%                     # get the words from the "bing" lexicon
  count(book, index = linenumber %/% 80, sentiment) %>%      # count them, create an index and sentiment column
  spread(sentiment, n, fill = 0) %>%                         # spread the "sentiment" column into "positive" and "negative" columns
  mutate(sentiment = positive - negative)                    # add a new column "sentiment" where sentiment = positive - negative
jane_austen_sentiment
  
# visualization

library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +    # plot index vs sentiment, filled by book
  geom_col(show.legend = FALSE) +                                      # specify columns as the type of plot
  facet_wrap(~book, ncol =  2, scales = "free_x")                      # make a plot for each book, displayed in 2 columns
  
# 2.3 COMPARING THE THREE SENTIMENT DICTIONARIES

pride_prejudice <- tidy_books %>%          # from the tidy_books data frame
  filter(book == "Pride & Prejudice")      # filter the "book" column for the novel Pride and Prejudice
pride_prejudice

# sentiment analysis using "afinn" lexicon

affin <- pride_prejudice %>%                     # from the pride_prejudice Pride and Prejudice
  inner_join(get_sentiments("afinn")) %>%        # get the words from the "afinn" lexicon
  group_by(index = linenumber %/% 80) %>%        # group them by every 80 lines
  summarise(sentiment = sum(value)) %>%          # get the sentiments totals sums
  mutate(method = "AFINN")                       # add a new column "method" corresponding to the lexicon used
affin

# sentiment analysis using "bing" and "nrc" lexincons

# This analysis is performed simultaneously because the "bing" lexicon categorizes words by "positive" and "negative" which are two of the categories in which the lexicon "nrc" categorizes the words.

bing_and_nrc <- bind_rows(pride_prejudice %>%                                                   # from the pride_prejudice data frame
                            inner_join(get_sentiments("bing")) %>%                              # perform an inner join with the "bing" lexicon Pride and Prejudice
                            mutate(method = "Bing et al."),                                     # add a new column "method" corresponding to "Bing et al."
                          pride_prejudice %>%                                                   # from the pride_prejudice dataset
                            inner_join(get_sentiments("nrc") %>%                                # perform an inner join with the "bing"nrc" lexicon Pride and Prejudice
                                         filter(sentiment %in% c("positive","negative"))) %>%   # filter for "positive" and "negative"
                            mutate(method = "NRC")) %>%                                         # add a new column "method" corresponding to NRC
  count(method, index = linenumber %/% 80, sentiment) %>%                                       # count the method rows and perform sentimet for every 80 rows
  spread(sentiment, n, fill = 0) %>%                                                            # spread the sentiment column into positive and negative columns
  mutate(sentiment = positive - negative)                                                       # add a new column "sentiment" where sentiment = positive - negative

# visualization

bind_rows(affin,                                       # merge the rows from the affin data frame
          bing_and_nrc) %>%                            # with the rows from the bing_and_nrc data frame
  ggplot(aes(index, sentiment, fill = method)) +       # plot index vs sentiment, filled by method
  geom_col(show.legend = FALSE) +                      # specify columns as the type of plot
  facet_wrap(~method, ncol = 1, scales = "free_y")     # make a plot for each method, displayed in 1 column

# count how many positive and negative words are in these lexicons

get_sentiments("nrc") %>%                                # from the "nrc" lexicon data frame
  filter(sentiment %in% c("positive","negative")) %>%    # filter the sentiment column to contain "positive" and "negative"
  count(sentiment)                                       # get the sentiments count

get_sentiments("bing") %>%                               # from the "bing" lexicon data frame
  count(sentiment)                                       # get the sentiments count

# 2.4 MOST COMMON POSITIVE AND NEGATIVE WORDS

bing_word_counts <- tidy_books %>%                # from the tidy_books data frame
  inner_join(get_sentiments("bing")) %>%          # get the words from the "bing" lexicon
  count(word, sentiment, sort = TRUE) %>%         # count the word and sentiment, sort them in descending order
  ungroup()                                       # ungroup them
bing_word_counts

# visual representation of most common positive and negative words

bing_word_counts %>%                                   # from the bing_word_counts data frame
  group_by(sentiment) %>%                              # group them by sentiment
  top_n(10) %>%                                        # select the top 10 words
  ungroup() %>%                                        # ungroup them
  mutate(word = reorder(word, n)) %>%                  # add a new column "word" and reorder them in descending order
  ggplot(aes(word, n, fill = sentiment)) +             # plot word vs n, filled by sentiment
  geom_col(show.legend = FALSE) +                      # do not show the labels
  facet_wrap(~sentiment, scales = "free_y") +          # set the y_scale
               labs(y = "Contribution to sentiment",   # specify the y axis label
                    x = NULL) +                        # specify the x axis label 
  coord_flip()                                         # flip coordinates

# adding the word "miss" to a custom stop-words list

custom_stop_words <- bind_rows(tibble(word = c("miss"),         # add the word "miss"
                                      lexicon = c("custom")),   # with a "custom" lexicon
                               stop_words)                      # to the stop_words data frame
custom_stop_words

# 2.5 WORDCLOUDS

library(wordcloud)

tidy_books %>%                                  # from the tidy_books data frame
  anti_join(stop_words) %>%                     # remove stop words
  count(word) %>%                               # get the words count
  with(wordcloud(word, n, max.words = 100))     # create wordcloud

# sentiment analysis tagging positive and negative words using an inner join

library(reshape2)

tidy_books %>%                                             # from the tidy_books data frame
  inner_join(get_sentiments("bing")) %>%                   # get the words from the "bing" lexicon
  count(word, sentiment, sort = TRUE) %>%                  # count the words and sentiment, sort them in descending order
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%   # use the acast attribute
  comparison.cloud(colors = c("grey20","grey80"),          # create comparison cloud
                   max.words = 100)                        # specify maximum amount of words

# 2.6 LOOKING AT UNITS BEYOND JUST WORDS

# "tokenize" text by sentences instead of by words

PandP_senteces <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")
PandP_senteces$sentence[2]

# split the text of Jane Austen's novels into a data frame by chapter

austen_chapters <- austen_books() %>%                           # from the austen_books data frame
  group_by(book) %>%                                            # group them by book
  unnest_tokens(chapter, text, token = "regex",                 # "tokenize" the text   
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%     # using this pattern
  ungroup()                                                     # ungroup them

austen_chapters %>%                # from the austen_chapters data frame
  group_by(book) %>%               # group them by book
  summarise(chapters = n())        # get the chapters summares

# analysis to determine the most negative chapter in each of Jane Austen's novels:

# step #1: get the list of negative words from the Bing lexicon

bingnegative <- get_sentiments("bing") %>%    # from the "bing" sentiment data frame
  filter(sentiment == "negative")             # filter the sentiment column to contain "negative"

# step #2: make a dataset of how many words are in each chapter

wordcounts <-tidy_books %>%                   # from the tidy_books data frame
  group_by(book, chapter) %>%                 # group by book and chapter
  summarize(words = n())                      # summarize it and get the words count

# step #3: find the number of negative words in each chapter and divide by the total words in each chapter

tidy_books %>%                                            # from the tidy_books data frame
  semi_join(bingnegative) %>%                             # return all the rows and where there are matching values in bingnegative
  group_by(book, chapter) %>%                             # group them by book and chapter
  summarize(negativewords = n()) %>%                      # get the count of negativewords
  left_join(wordcounts, by = c("book", "chapter")) %>%    # return all columns from tidy_books, and all columns from tidy_books and wordcounts
  mutate(ratio = negativewords/words) %>%                 # add a new column "ratio" corresponding to negativewords/words
  filter(chapter != 0) %>%                                # filter to do not include chapter 0
  top_n(1) %>%                                            # select the top chapter with the highest ratio of negative words
  ungroup()                                               # ungroup them

