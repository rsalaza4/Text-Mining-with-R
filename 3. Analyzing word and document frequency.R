### TEXT MINING WITH R - A Tidy Approach  ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 3: ANALYZING WORD AND DOCUMENT FREQUENCY: TF (TERM FREQUENCY) - IDF (INVERSE DOCUMENT FREQUENCYS)

# The statistics tf-idf is intended to measure how importanta word is to a document in a collection...
# (or corpus) of documents, for example, to one novel in a collection of novels or to one website...
# in a collection of websites.

# idf(term) = ln(n documents / n documents containing term)

# 3.1 TERM FREQUENCY IN JANE AUSTEN'S NOVELS

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%      # from the austen_books dataset
  unnest_tokens(word, text) %>%       # tokenize the words on the text column
  count(book, word, sort = TRUE)      # get the word count by book
book_words

total_words <- book_words %>%         # from the book_words dataset
  group_by(book) %>%                  # group rows by book
  summarize(total = sum(n))           # create a new column, name it "total" and count the number of words
total_words

book_words <- left_join(book_words, total_words)    # join the "total" column to the book_words dataset
book_words

library(ggplot2)

ggplot(book_words,                                 # from the book_words dataset, create a ggplot
       aes(n/total, fill = book)) +                # where the x-axis = n/total; bins filled by book
  geom_histogram(show.legend = FALSE) +            # specify histogram as the type of plot  
  xlim(NA, 0.0009) +                               # set the x-axis limit
  facet_wrap(~book, ncol = 2, scales = "free_y")   # create histograms for each book, displayed in 2 columns

# 3.2 ZIPF'S LAW

# Zipf's law states that the frequency that a word appears is inversely proportional to its rank.

freq_by_rank <- book_words %>%            # from the book_words dataset
  group_by(book) %>%                      # group rows by book
  mutate(rank = row_number(),             # add a new column "rank" where rank = row_number
         term_frequency = n/total)        # add a new column "term frequency" where term frequency = n/total   
freq_by_rank

# The rank column tells the rank of each word within the frequency table.
# Zipf's law is often visualized by plotting rank on the x-axis and term frequency on the y-axis, on log scales.

freq_by_rank %>%                                              # from the freq_by_rank dataset
  ggplot(aes(rank,term_frequency, color = book)) +            # plot rank vs term frequency, colored by book  
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +   # specify line setups    
  scale_x_log10() +                                           # specify x-axis scale to be logarithmic
  scale_y_log10()                                             # specify y-axis scale to be logarithmic

# Utilizing power law

rank_subset <- freq_by_rank %>%           # create a new tidy dataset named "rank_subset" from the freq_by_rank dataframe  
  filter(rank < 500,                      # filter rank to be less than 500
         rank > 10)                       # filter rank to be greater than 10

lm(log10(term_frequency) ~log10(rank), data = rank_subset)    # create a linear regresion model

freq_by_rank %>%                                                                   # from the freq_by_rank dataset
  ggplot(aes(rank, term_frequency, color = book)) +                                # plot rank vs term frequency, colores by book
  geom_abline(intercept = -0.62, slope = -1.1, color = "grey50", linetype = 2) +   # add the line obtained on the linear regression model
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +                        # specify line setups
  scale_x_log10() +                                                                # specify x-axis scale to be logarithmic
  scale_y_log10()                                                                  # specify y-axis scale to be logarithmic

# 3.3 THE bind_tf_idf FUNCTION

# The idea od tf-idf is to find the important words for the content of each document by decreasing the weight...
# for commonly used words and increasing the weights for words that are no used very much in a collection or...
# corpus of documents (i.e. words that are common by not too common).

book_words <- book_words %>%     # from the book_words dataset
  bind_tf_idf(word, book, n)     # apply the bind_tf_idf function
book_words

# As seen on the datasetm the inverse document frequency (idf) is very low (near zero) for words that occu in...
# many of the documents in a collection; this is how this approach decreases the wieght for common words. The...
# inverse document frequency will be a higher number for words that occur in fewer of the documents in the collection.

book_words %>%                   # from the book_words dataset
  select(-total) %>%             # select all the columns except the total column
  arrange(desc(tf_idf))          # and arrange it by tf_idf in descending order

book_words %>%                                                    # from the book_words dataset
  arrange(desc(tf_idf)) %>%                                       # arrange it by tf_idf in descending order
  mutate(word = factor(word, levels = rev(unique(word)))) %>%     # add a new column "word" where each word is a factor 
  group_by(book) %>%                                              # group rows by book
  top_n(15) %>%                                                   # select the top 15 words
  ungroup() %>%                                                   # ungroup rows
  ggplot(aes(word, tf_idf, fill = book)) +                        # plot word vs tf_idf, filled by book 
  geom_col(show.legend = FALSE) +                                 # specify columns as the type of plot  
  labs(x = NULL, y = "tf-idf") +                                  # specify the x-axis and y-axis labels 
  facet_wrap(~ book, ncol = 2, scales = "free") +                 # create plots for each book, displayed in 2 columns
  coord_flip()                                                    # flip coordinates

# 3.4 A CORPUS OF PHYSICS TEXTS

# Same approach with classic physics texts

library(gutenbergr)

physics <- gutenberg_download(c(37729, 14725, 13476, 30155),   # download the following physic books
                              meta_fields = "author")          # specified by author
physics

physics_words <- physics %>%            # from the austen_books dataset
  unnest_tokens(word, text) %>%         # tokenize the words on the text column
  count(author, word, sort = TRUE)      # get the word count by author
physics_words

library(forcats)

plot_physics <- physics_words %>%                                     # from the physics_words dataset
  bind_tf_idf(word, author, n) %>%                                    # apply the bind_tf_idf function
  mutate(word = fct_reorder(word, tf_idf)) %>%                        # add a new column "word" and reorder it by tf_idf
  mutate(author = factor(author, levels = c("Galilei, Galileo",       # add a new column "author" with each of the 4 authors as factors
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics

plot_physics %>%                                        # from the plot_physcis dataset
  group_by(author) %>%                                  # group rows by author
  top_n(15, tf_idf) %>%                                 # select the top 15 words based on tf_idf
  ungroup() %>%                                         # ungroup rows
  mutate(word = reorder(word, tf_idf)) %>%              # add a new column "word" and reorder it by tf_idf
  ggplot(aes(word, tf_idf, fill = author)) +            # plot word vs tf_idf, filled by author
  geom_col(show.legend = FALSE) +                       # specify columns as the type of plot
  labs(x = NULL, y = "tf-idf") +                        # specify the x-axis and y-axis labels
  facet_wrap(~ author, ncol = 2, scales = "free") +     # create plots for each author, displayed in 2 columns
  coord_flip()                                          # flip coordinates

# One interesting thing from the Einstein Plot is the "k" letter!

library(stringr)

physics %>%
  filter(str_detect(text, "_k_")) %>%
  select(text)

# Note: the unnest_tokenS() function separates arounf punctuation like hyphens by defeault (e.g. "co" "ordinate")!

# Remove less meaningful words to maje a better, more meaningfuk plot.

mystopwords <- tibble(word = c("eq","co","rc","ac","ak","bn","fig",
                               "file","cg","cb","cm","ab","_k","_k_","_x")) # define custom stop words (i.e. not relevant)

physics_words <-  anti_join(physics_words, mystopwords, by = "word") # remove custom stop words

plot_physics <- physics_words %>%                                    # from the physics_words dataset
  bind_tf_idf(word, author, n) %>%                                   # apply the bind_tf_idf function
  mutate(word = str_remove_all(word, "_")) %>%                       # add a new column "word" that removes words with "_"
  group_by(author) %>%                                               # group rows by author
  top_n(15, tf_idf) %>%                                              # select the top 15 words based on tf_idf
  ungroup() %>%                                                      # ungroup rows
  mutate(word = reorder_within(word, tf_idf, author)) %>%            # add a new column "word" and reorder it by tf_idf
  mutate(author = factor(author, levels = c("Galilei, Galileo",      # add a new column "author" with each of the 4 authors as factors
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
plot_physics


ggplot(plot_physics, aes(word, tf_idf, fill = author)) +             # plot word vs tf_idf, filled by author 
  geom_col(show.legend = FALSE) +                                    # specify columns as the type of plot
  labs(x = NULL, y = "tf-idf") +                                     # specify the x-axis and y-axis labels
  facet_wrap(~author, ncol = 2, scales = "free") +                   # create plots for each author, displayed in 2 columns
  coord_flip() +                                                     # flip coordinates
  scale_x_reordered()                                                # reorder x-axis scale
