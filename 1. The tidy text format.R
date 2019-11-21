### TEXT MINING WITH R - A Tidy Approach  ###

### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 1: THE TIDY TEXT FORMAT

# 1.1 CONTRASTING TIDY TEXT WITH OTHER DATA STRUCTURES

# 1.2 THE UNNEST_TOKENS FUNCTION

# Character vector
text <- c("Bacause I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# Turning character vector into a tidy dataset:

# Step #1 : put it into a dataframe
library(dplyr)
text_df <- tibble(line = 1:4, text = text)
# tibble(name of 1st column = content, name of 2nd column = content)
# "line" refers to the name of the first column; "text" refers to the name of the second column
text_df

# Tibble is a modern class of data frame within R that has a convenient print method...
# will not convert strings to factora, and does not use row names.

# Step #2: "tokenization" - convert each row into a one-token-per-document-per-row
library(tidytext)

# A token is a meaningful unit of text, most often a word, that we are interested in using for...
# further analysis, and tokenization is the process of splitting text into tokens.

text_df %>% unnest_tokens(word, text)
# the first argument in unnest_tokens is the column name we want to assign
# the second argument in unnest_tokens is the input column the text comes from (text column from text_df)

# 1.3 TIDYING THE WORKS OF JANE AUSTEN

library(janeaustenr)
library(dplyr)
library(stringr)

# Load dataset
original_books <- austen_books() %>%                                           # from the austen_books dataset
  group_by(book) %>%                                                           # group them by book
  mutate(linenumber = row_number(),                                            # add a new column ("linenumber") correspoding to row number
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",        # add a new column ("chapter") corresponding to the chapter number
                                                 igonre_case = TRUE)))) %>%
  ungroup()                                                                    # ungroup
original_books

# Restructure dataset into a one-token-per-row format
library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

# Remove stop words
data(stop_words)
tidy_books <- tidy_books %>% anti_join(stop_words)
tidy_books

# Find the most common words in all the books as a whole
tidy_books %>% count(word, sort = TRUE)

# Visulization of the most common words
library(ggplot2)

tidy_books %>%                            # from the tidy_books dataset
  count(word, sort = TRUE) %>%            # get the word count and sort them in descending order   
  filter(n > 600) %>%                     # filter for words that have more that appeared more than 600 times
  mutate(word = reorder(word,n)) %>%      # add a new column ("word") and reorder them in descending order
  ggplot(aes(word,n)) +                   # creat ggplot visualization
  geom_col() +                            # set columns
  xlab(NULL) +                            # specify x axis title
  coord_flip()                            # flip coordinates

# 1.4 THE GUTENBERGR PACKAGE

# 1.5 WORD FREQUENCIES

# Load library corresponding to the Project Gutenberg (multiple novels indexed)
library(gutenbergr)

# Specify which books to download written by H.G. Wells
hgwells <- gutenberg_download(c(35,36,5230,159))
hgwells

# "Tokenize" the words and remove stop words
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
tidy_hgwells

# What are the most common words on these four novels?
tidy_hgwells %>% count(word, sort = TRUE)

# Specify which books to download written by Bronte sisters
bronte <- gutenberg_download(c(1260,768,969,9182,767))
bronte

# "Tokenize" the words and remove stop words
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# What are the most common words on these five novels?
tidy_bronte %>% count(word, sort = TRUE)

# Calculation of the frequency for each word for the words of the three authors

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),     # add a new column ("author") to the tidy_bronte dataset
                       mutate(tidy_hgwells, author = "H.G. Wells"),        # add a new column ("author") to the tidy_bronte dataset
                       mutate(tidy_books, author = "Jane Austen")) %>%     # add a new column ("author") to the tidy_bronte dataset and combine all rows from abovw
  mutate(word = str_extract(word, "[a-z']+")) %>%                          # add a new column ("word") witt the words extracted
  count(author, word) %>%                                                  # get the count by author and word
  group_by(author) %>%                                                     # group authors
  mutate(proportion = n / sum(n)) %>%                                      # add a new column ("proportion") corresponding to n / sum(n)
  select(-n) %>%                                                           # select those words with negative count                                        
  spread(author, proportion) %>%                                           # spread word proportion by authors
  gather(author, proportion, 'Bronte Sisters':'H.G. Wells')                # gather 2 authors
frequency

# Visulization

library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# Quantify how similar and different these sets of word frequencies are using a correlation test

cor.test(data = frequency[frequency$author == "Bronte Sisters",],~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",],~ proportion + `Jane Austen`)
