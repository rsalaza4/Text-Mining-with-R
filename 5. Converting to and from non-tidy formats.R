### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 5: CONVERTING TO AND FROM NON-TIDY FORMATS

# 5.1 Tidying a document-term matrix

library(tm)
library(broom)

data("AssociatedPress", package = "topicmodels")    # consider the collection of Associated Press newspapers articles included in the topicmodels package
AssociatedPress

# access the terms in the document

terms <- Terms(AssociatedPress)
head(terms)

library(dplyr)
library(tidytext)

# turn this data into a data frame with one-token-per-row

ap_td <- tidy(AssociatedPress)
ap_td

# perform sentiment analysis on these newspapers articles with the approach described in Chapter 2

ap_sentiments <- ap_td %>%                                       # from the ap_td data frame
  inner_join(get_sentiments("bing"), by = c(term = "word"))      # perform an inner join from the "word" column from the "bing" sentiment lexicon
ap_sentiments

# visualize the words from the AP articles contribute the most tothe positive or negtive sentiment

library(ggplot2)

ap_sentiments %>%                                            # from the ap_sentiments data frame
  count(sentiment, term, wt = count) %>%                     # count the total number of unique words
  ungroup() %>%                                              # ungroup them
  filter(n >= 200) %>%                                       # filter for the words where n >= 200   
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%     # if the sentiment is negative, change the n value to negative
  mutate(term = reorder(term, n)) %>%                        # reorder the "term" columm by termn and n
  ggplot(aes(term, n, fill = sentiment)) +                   # plot term vs n, filled by sentiment 
  geom_bar(stat = "identity") +                              # specify bars as the type of plot
  ylab("Contribution to sentiment") +                        # specify the y-axis label
  coord_flip()                                               # flip coordinates

# 5.2 Tidying dfm objects

library(quanteda)
data("data_corpus_inaugural")                                        # consider the corpus of presidential inauguration speeches
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)   
inaug_dfm

# convert them into a one-token-per-document-per-row table

inaug_td <- tidy(inaug_dfm)
inaug_td

# find the words most specific to each of the inaugural speeches by calculating the tf-idf of each term-speech pair

inaug_tf_idf <- inaug_td %>%                         # from the inaug_td data frame
  bind_tf_idf(term, document, count) %>%             # get the tf_idf
  arrange(desc(tf_idf))                              # sort it in descending order by tf_idf
inaug_tf_idf

# visualize the words most specific to each speech from 4 presidents

inaug_tf_idf %>%                                                   # from the inaug_tdf_idf data frame
  filter(document %in% c("1861-Lincoln",                           # filter the "document" column to contain 4 specific documents
                         "1933-Roosevelt",
                         "1961-Kennedy",
                         "2009-Obama")) %>% 
  arrange(desc(tf_idf)) %>%                                        # sort them in descending order by tf_idf
  mutate(term = factor(term, levels = rev(unique(term)))) %>%      # convert the elements in the "term" column into factors
  group_by(document) %>%                                           # group them by document
  top_n(10) %>%                                                    # select the top 10 words from each document
  ungroup %>%                                                      # ungroup them
  ggplot(aes(term, tf_idf, fill = document)) +                     # plot term vs tf_idf, filled by document  
  geom_col(show.legend = FALSE) +                                  # specify columns as the type of plot
  labs(x = NULL, y = "tf-idf") +                                   # specify the x-axis and y-axis labels
  facet_wrap(~document, ncol = 2, scales = "free") +               # create plots for each document, displayed in 2 columns
  coord_flip()                                                     # flip coordinates

# extract the year from each document's name and compute the total number of words within each year

library(tidyr)

year_term_count <- inaug_td %>%                                     # from the inaug_td data frame
  extract(document, "year", "(\\d+)", convert = TRUE) %>%           # from the "document" column, extract the year and add it to a new column "year"
  complete(year, term, fill = list(count = 0)) %>%                  # create a data frame with the count of "year" and "term" columns 
  group_by(year) %>%                                                # group them by year
  mutate(year_total = sum(count))                                   # add a new column "year_total" corresponding to the total number of words per year

# pick several words and visualize how they changed in frequency over time

year_term_count %>%                                                                            # from the year_term_count data frame
  filter(term %in% c("good", "america", "foreign", "union", "constitution", "freedom")) %>%    # filter the "term" columnm to contain only these 6 words
  ggplot(aes(year, count/year_total)) +                                                        # plot year vs count/year_total
  geom_point() +                                                                               # specifcy points as the type of plot
  geom_smooth() +                                                                              # specify smooth line  
  facet_wrap(~term, scales = "free_y") +                                                       # create plor for each term
  scale_y_continuous(labels = scales::percent_format()) +                                      # specify the y-axis scale    
  ylab("% frequency of word in inaugural address")                                             # specify the y-axis label

# 5.2 Casting tidy text data into a matrix

# take the ap_td data frame and cast it back into a document-term matrix

ap_td %>%
  cast_dtm(document, term, count)

# cast the table unto a dfm object from quanteda's dfm

ap_td %>%
  cast_dfm(document, term, count)

# tools that require a sparse matrix

library(Matrix)

# cast into a Matrix object

m <- ap_td %>%
  cast_sparse(document, term, count)
class(m)
dim(m)

# create a DTM of Jane Austen's books in just a few lines of code

library(janeaustenr)

austen_dtm <- austen_books() %>%      # from the austen_books dataset
  unnest_tokens(word, text) %>%       # "tokenize" the "text" column
  count(book, word) %>%               # get the words count
  cast_dtm(book, word, n)             # creat a DTM
austen_dtm

# the casting process allows for reading, filtering, and processinf to be done using dpylr ...
# and other tidy tools, after which the data can be converted into a document-term matrix  ...
# for machine learning applications.

# Tidying corpus objects with metadata

# "Corpus" are stored document collections before tokenization.

data("acq")    # the acq corpus contains 50 articles from the news service Reuters
acq

# first document

acq[[1]]

# a corpus document is structured like a list, with each item containing both text and metadata

# construct a table with ine riw per document, including the metadata (such as is and datetimestamp) as columns alongside tge text

acq_td <- tidy(acq)
acq_td

# find the most commons words accros the 50 Reuters articles (i.e. the ones most specific to each article)

acq_tokens <- acq_td %>%                    # from the acq_td dataset
  select(-places) %>%                       # select every column except "places"
  unnest_tokens(word, text) %>%             # "tokenize" the "text" column 
  anti_join(stop_words, by = "word")        # remove stop words

acq_tokens %>%                              # from the acq_tokens dataset
  count(word, sort = TRUE)                  # get the words count

# tf-idf

acq_tokens %>%                              # from the acq_tokens dataset
  count(id, word) %>%                       # get the words count  
  bind_tf_idf(word, id, n) %>%              # apply bind_tf_idf function  
  arrange(desc(tf_idf))                     # sort them in descending order by tf_idf
