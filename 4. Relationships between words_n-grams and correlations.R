### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 4: RELATIONSHIPS BETWEEN WORDS: N-GRAMS AND CORRELATIONS

# 4.1 Tokenizing by n-gram

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_books()
austen_bigrams <- austen_books() %>%                    # from the austen_books dataset
  unnest_tokens(bigram, text, token = "ngrams", n = 2)  # "tokenize" words from the "text" column in groups of 2   
austen_bigrams

# 4.1.1 Counting and filtering n-grams

austen_bigrams %>%                 # from the austen_bigrams dataset
  count(bigram, sort = TRUE)       # count the pairs of words from the "bigram" column

# Separating a single column into multiple columns:

library(tidyr)

bigrams_separated <- austen_bigrams %>%                 # from the austen_bigrams dataset
  separate(bigram, c("word1","word2"), sep = " ")       # separate the "bigram" column into two columns: "word1" and "word2" by a space
bigrams_separated

bigrams_filtered <- bigrams_separated %>%               # from the bigrams_separated dataset
  filter(!word1 %in% stop_words$word) %>%               # filter for the words in "word1" column that are not included in the stop_words dataset
  filter(!word2 %in% stop_words$word)                   # filter for the words in "word2" column that are not included in the stop_words dataset
bigrams_filtered

# New bigram counts:
bigram_counts <- bigrams_filtered %>%                   # from the austen_filtered dataset
  count(word1, word2, sort = TRUE)                      # count the words on the "word1" and "word2" columns
bigram_counts

# tidyr's unit() function is the inverse of separate(); recombine columns into one

bigrams_united <- bigrams_filtered %>%                  # from the bigrams_filtered dataset
  unite(bigram, word1, word2, sep = " ")                # unit the "word1" and "word2" columns into a new column named "bigram" separated by a space
bigrams_united

# 4.2 Analyzing bigrams

# Getting the most important streets
bigrams_filtered %>%                                    # from the bigrams_filtered dataset
  filter(word2 == "street") %>%                         # filter "word2" column to contain the word "street"
  count(book, word1, sort = TRUE)                       # get the total count of pairs of words, sorted in descending order

# Getting tf-idf from a bigram

bigram_tf_idf <- bigrams_united %>%                     # from the bigrams_united dataset
  count(book, bigram) %>%                               # get the count for each of the bigrams
  bind_tf_idf(bigram, book, n) %>%                      # apply the bind_tf_idf function
  arrange(desc(tf_idf))                                 # arrange by tf_idf in descending order
bigram_tf_idf

library(ggplot2)

bigram_tf_idf %>%                                       # from the bigram_tf_idf dataset
  group_by(book) %>%                                    # group rows by book
  top_n(12, tf_idf) %>%                                 # select the top 12 words based on tf_idf
  ungroup() %>%                                         # ungroup rows
  mutate(bigram = reorder(bigram, tf_idf)) %>%          # add a new column "bigram" and reorder it by tf_idf
  ggplot(aes(bigram, tf_idf, fill = book)) +            # plot word vs tf_idf, filled by book
  geom_col(show.legend = FALSE) +                       # specify columns as the type of plot
  labs(x = NULL, y = "tf-idf") +                        # specify the x-axis and y-axis labels
  facet_wrap(~ book, ncol = 2, scales = "free") +       # create plots for each book, displayed in 2 columns
  coord_flip()                                          # flip coordinates

# 4.1.3 Using bigrams to provide context in sentiment analysis

bigrams_separated %>%                     # from the bigrams_separated dataset
  filter(word1 == "not") %>%              # filter "word1" column to contain the word "street"
  count(word1, word2, sort = TRUE)        # get the total count of pairs of words, sorted in descending order

AFINN <- get_sentiments("afinn")          # get AFINN lexicon for sentiment analysis
AFINN

not_words <- bigrams_separated %>%                 # from the bigrams_separated dataset
  filter(word1 == "not") %>%                       # filter "word1" column to contain the word "not"  
  inner_join(AFINN, by = c(word2 = "word")) %>%    # get inner join with the AFINN dataset by the "word2" column
  count(word2, value, sort = TRUE)                 # get the total count of the "word2" and "value" columns
not_words

not_words %>%                                            # from the not_words dataset
  mutate(contribution = n * value) %>%                   # add a new column "contribution" with n * value
  arrange(desc(abs(contribution))) %>%                   # arrange the absolute value of contribution in descending order  
  head(20) %>%                                           # select the top 20 rows
  mutate(word2 = reorder(word2, contribution)) %>%       # add a new column "word2" and reorder it by word2 and contribution
  ggplot(aes(word2, n * value, fill = n * value > 0)) +  # plot word2 vs n * value, filled by n * value > 0  
  geom_col(show.legend = FALSE) +                        # specify columns as the type of plot   
  xlab("Words preceded by \"not\"") +                    # specify the x-axis label
  ylab("Sentiment value * number of occurences") +       # specify the y-axis label
  coord_flip()                                           # flip coordinates

# Common negation words:
negation_words <- c("not","no","never","without")        # set a new variable with custom negation words

negated_words <- bigrams_separated %>%                   # from the bigrams_separated dataset
  filter(word1 %in% negation_words) %>%                  # filter "word1" column to contain a word from negation_words  
  inner_join(AFINN, by = c(word2 = "word")) %>%          # get inner join with the AFINN dataset by the "word2" column
  count(word1, word2, value, sort = TRUE)                # get the total count of the "word2" and "value" columns
negated_words

negated_words %>%                                                                 # from the negated_words dataset
  mutate(contribution = n * value,                                                # add a new column "contribution"
         word2 = reorder(paste(word2, word1, sep ="__"), contribution)) %>%       # add a new column "word2"  
  group_by(word1) %>%                                                             # groups rows by word1
  top_n(12, abs(contribution)) %>%                                                # select top 12 words based on contribution
  ggplot(aes(word2, n * value, fill = n * value > 0)) +                           # plot word2 vs n * value, filled by n * value > 0
  geom_col(show.legend = FALSE) +                                                 # specify columns as the type of plot 
  facet_wrap(~ word1, scales = "free") +                                          # make a plot for every word1
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +                   # specify x-axis scale
  xlab("Words preceded by negation term") +                                       # specify the x-axis label
  ylab("Sentiment value * # of occurrences") +                                    # specify the y-axis label
  coord_flip()                                                                    # flip coordinates
  
# 4.1.4 Visualizing a network of bigrams with ggraph

library(igraph)

# Original counts
bigram_counts

# Filter for only relatively common combinations
bigram_graph <- bigram_counts %>%                        # from the bigram counts dataset
  filter(n > 20) %>%                                     # filter for the top 20 words
  graph_from_data_frame()                                # transform it into a graph
bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +                           # plot the graph, select layout
  geom_edge_link() +                                            # specificy the edge links
  geom_node_point() +                                           # specificy the node points
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)       # specify the nodes text

# Polishing operators to make a better looking graph

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))   # specify arrows characteristics

ggraph(bigram_graph, layout = "fr") +                             # plot the graph, select layout 
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,        # edge_alpha makes link transparent based on how common or rare a bigram is
                 arrow = a, end_cap = circle(0.07, "inches")) +   # add directionalilty with the arrow; end_cap tells the arrow to end before touching the node
  geom_node_point(color = "lightblue", size = 5) +                # specify the color and size of the nodes
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +       # specify the nodes text 
  theme_void()                                                    # set the theme 

# 4.1.5 Visualizing bigrams in other texts

# The following lines of code are a summary of what has been done so far but applied into functions to pass new dataset

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
  
# The King James version is book 10 on Project Gutenberg:
library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()

# Filter out rare combinations, as well as digits
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()  

# 4.2 Counting and correlation pairs of words with widyr package

# 4.2.1 Counting and correlating among sections

austen_section_words <- austen_books() %>%        # from the austen_books dataset
  filter(book == "Pride & Prejudice") %>%         # filter for the "Pride & Prejudice" book
  mutate(section = row_number() %/% 10) %>%       # add a new colummn "section" corresponding to the row number divided by 10
  filter(section > 0) %>%                         # filter for section greater than 0
  unnest_tokens(word, text) %>%                   # "tokenize" words form the text column
  filter(!word %in% stop_words$word)              # filter words that are considered stop words
austen_section_words

library(widyr)

# The pairwise_ means it will result in one row for each pair of words in the word variable.
# This let us count common pairs of words co-appearing within the same section (ecery 10 rows).

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "darcy")

# 4.2.2 Pairwise correlation

# The correlation between words indicates how often they appear together relative to how often they appear separately.

# The phi coefficient is equivalent to the Pearson correlation, which you may have heard of elsewhere, when it is applied to binary data).
# The pairwise_cor() function in widyr lets us find the phi coefficient between words based on how often they appear in the same section.

# Filter for at least relatively common words first

words_cors <- austen_section_words %>%          # from the austen_words dataset
  group_by(word) %>%                            # group rows by word
  filter(n() >= 20) %>%                         # filter n to be greater or equal than 20  
  pairwise_cor(word, section, sort = TRUE)      # get pairwise correlations
words_cors

# Find the words most correlated with a word like "pounds" using a filter operation:

words_cors %>%                                  # from the words_cors dataset
  filter(item1 == "pounds")                     # filter item1 to contain the word "pounds"

# Pick particular interesting words and find the other words most associated with them:

words_cors %>%                                                            # from the words_cors dataset
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%     # filter item1 to contain "elizabeth", "pounds", "married" or "pride"
  group_by(item1) %>%                                                     # group rows by word
  top_n(6) %>%                                                            # select top 6 words
  ungroup() %>%                                                           # ungroup rows
  mutate(item2 = reorder(item2, correlation)) %>%                         # add a new column "item2" and reorder it by correlation
  ggplot(aes(item2, correlation)) +                                       # plot item2 vs correlation
  geom_bar(stat = "identity") +                                           # specify columns as the type of plot 
  facet_wrap(~ item1, scales = "free") +                                  # make a plot for every item1
  coord_flip()                                                            # flip coordinates
  
set.seed(2016)

words_cors %>%                                                            # from the words_cors dataset
  filter(correlation > .15) %>%                                           # filter correlation to be greater than 0.15
  graph_from_data_frame() %>%                                             # transform it into a graph object   
  ggraph(layout = "fr") +                                                 # plot the graph, select layout 
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +    # edge_alpha makes link transparent based on how common or rare a bigram is
  geom_node_point(color = "lightblue", size = 5) +                        # specify the color and size of the nodes
  geom_node_text(aes(label = name), repel = TRUE) +                       # specify the nodes text 
  theme_void()                                                            # set the theme 
