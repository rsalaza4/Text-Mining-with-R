### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 1: THE TIDY TEXT FORMAT

# 1.1 CONTRASTING TIDY TEXT WITH OTHER DATA STRUCTURES

# 1.2 THE UNNEST_TOKENS FUNCTION

# character vector

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

# turning character vector into a tidy dataset

# step #1 : put it into a dataframe

library(dplyr)
text_df <- tibble(line = 1:4, text = text)

# tibble(name of 1st column = content, name of 2nd column = content)
# "line" refers to the name of the first column; "text" refers to the name of the second column

text_df

# tibble is a modern class of data frame within R that has a convenient print method will not convert strings to factora, and does not use row names

# step #2: "tokenization" - convert each row into a one-token-per-document-per-row

library(tidytext)

# a token is a meaningful unit of text, most often a word, that we are interested in using for further analysis, and tokenization is the process of splitting text into tokens.

text_df %>% unnest_tokens(word, text)

# the first argument in unnest_tokens is the column name we want to assign
# the second argument in unnest_tokens is the input column the text comes from (text column from text_df)

# 1.3 TIDYING THE WORKS OF JANE AUSTEN

library(janeaustenr)
library(dplyr)
library(stringr)

# Load data frame

original_books <- austen_books() %>%                                           # from the austen_books data frame
  group_by(book) %>%                                                           # group them by book
  mutate(linenumber = row_number(),                                            # add a new column "linenumber" correspoding to row number
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",        # add a new column "chapter" corresponding to the chapter number
                                                 igonre_case = TRUE)))) %>%
  ungroup()                                                                    # ungroup them
original_books

# restructure dataset into a one-token-per-row format

library(tidytext)
tidy_books <- original_books %>%      # from the original_books data frame
  unnest_tokens(word, text)           # "tokenize" the "text" column
tidy_books

# remove stop words

data(stop_words)                  # load stop_words data frame

tidy_books <- tidy_books %>%      # from the tidy_books data frame
  anti_join(stop_words)           # perform an anti join to remove stop words
tidy_books

# find the most common words in all the books as a whole

tidy_books %>% count(word, sort = TRUE)

# visulization of the most common words

library(ggplot2)

tidy_books %>%                            # from the tidy_books data frame
  count(word, sort = TRUE) %>%            # get the word count and sort them in descending order   
  filter(n > 600) %>%                     # filter for words that have appeared more than 600 times
  mutate(word = reorder(word,n)) %>%      # add a new column "word" and reorder them in descending order
  ggplot(aes(word,n)) +                   # plot word vs n
  geom_col() +                            # specify columns as the type of plot
  xlab(NULL) +                            # specify x-axis label
  coord_flip()                            # flip coordinates

# 1.4 THE GUTENBERGR PACKAGE

# 1.5 WORD FREQUENCIES

# load library corresponding to the Project Gutenberg (multiple novels indexed)

library(gutenbergr)

# specify which books to download written by H.G. Wells

hgwells <- gutenberg_download(c(35,36,5230,159))
hgwells

# "tokenize" the words and remove stop words

tidy_hgwells <- hgwells %>%           # from the hgwells data frame
  unnest_tokens(word, text) %>%       # "tokenize" the "text" column
  anti_join(stop_words)               # perform an anti join to remove stop words
tidy_hgwells

# which are the most common words on these four novels?

tidy_hgwells %>% count(word, sort = TRUE)

# specify which books to download written by Bronte sisters

bronte <- gutenberg_download(c(1260,768,969,9182,767))
bronte

# "tokenize" the words and remove stop words

tidy_bronte <- bronte %>%             # from the bronte data frame
  unnest_tokens(word, text) %>%       # "tokenize" the "text" column
  anti_join(stop_words)               # perform an anti join to remove stop words
tidy_bronte

# which are the most common words on these five novels?

tidy_bronte %>% count(word, sort = TRUE)

# calculation of the frequency for each word for the words of the three authors

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),     # add a new column "author" to the tidy_bronte data frame
                       mutate(tidy_hgwells, author = "H.G. Wells"),        # add a new column "author" to the tidy_bronte data frame
                       mutate(tidy_books, author = "Jane Austen")) %>%     # add a new column "author" to the tidy_bronte data frame and combine all rows from abovw
  mutate(word = str_extract(word, "[a-z']+")) %>%                          # add a new column "word" witt the words extracted
  count(author, word) %>%                                                  # get the count by author and word
  group_by(author) %>%                                                     # group by authors
  mutate(proportion = n / sum(n)) %>%                                      # add a new column "proportion" corresponding to n / sum(n)
  select(-n) %>%                                                           # select all columns but the "n" column                                          
  spread(author, proportion) %>%                                           # spread word proportion by authors
  gather(author, proportion, 'Bronte Sisters':'H.G. Wells')                # gather 2 authors
frequency

# visualization

library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +    # plot proportion vs Jane Austen, colored by abs(`Jane Austen` - proportion)
  geom_abline(color = "gray40", lty = 2) +                                                              # specify a diagonal line
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +                                     # specify scatter plot as the type of plot
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +                                     # specify text labels
  scale_x_log10(labels = percent_format()) +                                                            # specify x-axis scale to be logarithmic 
  scale_y_log10(labels = percent_format()) +                                                            # specify y-axis scale to be logarithmic
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +                 # specify the scale color gradient
  facet_wrap(~author, ncol = 2) +                                                                       # make a plot for each author
  theme(legend.position="none") +                                                                       # specify the plot themes
  labs(y = "Jane Austen", x = NULL)                                                                     # specify the x-axis and y-axis labels 

# quantify how similar and different these sets of word frequencies are using a correlation test

cor.test(data = frequency[frequency$author == "Bronte Sisters",],~ proportion + `Jane Austen`)
cor.test(data = frequency[frequency$author == "H.G. Wells",],~ proportion + `Jane Austen`)
