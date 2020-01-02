### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 6: TOPIC MODELING

# 6.1 Latent Dirichlet allocation (LDA)

library(topicmodels)

data("AssociatedPress") # consider the collection of Associated Press newspapers articles included in the topicmodels package
AssociatedPress

# Set a seed so that the output of the model is predictable
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))  # apply the LDA algoithm; k refers to the number of topics wanted
ap_lda

# 6.1.1 Word-topic probabilities

library(tidytext)

ap_topics <- tidy(ap_lda, matrix = "beta")   # extract the per-topic-per-word probabilities with the "beta" method
ap_topics

# find the 10 terms that are most common within eacg topic

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%    # from the ap_topics data set
  group_by(topic) %>%            # group by topic
  top_n(10, beta) %>%            # select the top 10 terms per topic based on the beta value
  ungroup() %>%                  # ungroup them
  arrange(topic, -beta)          # sort them first by topic and then by beta in descending order   

ap_top_terms %>%                                           # from the ap_top_terms data set
  mutate(term = reorder_within(term, beta, topic)) %>%     # add a new column "term" and reorder it before plotting it with faceting
  ggplot(aes(term, beta, fill = factor(topic))) +          # plot term vs beta, filled by topic (converted into a factor)
  geom_col(show.legend = FALSE) +                          # specify columns as the type of plot
  facet_wrap(~ topic, scales = "free") +                   # create plots for each topic
  coord_flip() +                                           # flip coordinates
  scale_x_reordered()                                      # specify x-axis scale

# get the terms that had the greatest difference in beta between topic 1 and topic 2.

library(tidyr)

beta_spread <- ap_topics %>%                               # from the ap_topics data set
  mutate(topic = paste0("topic", topic)) %>%               # add a new column "topic" with the topic and topic number
  spread(topic, beta) %>%                                  # spread the beta column into two new columns (one per topic)
  filter(topic1 > 0.001 | topic2 > 0.001) %>%              # filter topic1 and topic2 columns to contain values greater than 0.001
  mutate(log_ratio = log2(topic2 / topic1))                # add a new column "log_ratio" where log_ration = log2(topic2 / topic1)   
beta_spread

beta_spread %>%                                            # from the beta_spread data set
  mutate(abs_log = abs(log_ratio)) %>%                     # add a new column "abs_log" where abs_log = abs(log_ratio)
  mutate(rank = dense_rank(desc(abs_log))) %>%             # add a new column "rank" with a descending rank for the abs_log column
  filter(rank <= 20) %>%                                   # filter for the rank column to be <= 20
  ggplot(aes(reorder(term, log_ratio), log_ratio)) +       # plot term vs log_ratio
  geom_bar(stat = "identity") +                            # specify bars as the type of plot
  xlab("term") +                                           # specify the x-axis label
  ylab("Log2 ratio of beta in topic 2/ topic 1") +         # specify the y-axis label
  coord_flip()                                             # flip coordinates

# 6.1.2 Document-topic probabilitites

ap_documents <- tidy(ap_lda, matrix = "gamma")             # extract the per-document-per-topic probabilities with the "gamma" method
ap_documents

# take a closer look at document 6, where the proportion of words generated are mostly for topic 2

tidy(AssociatedPress) %>%                       # from the AssociatedPress data set (applying the tidy() function)
  filter(document == 6) %>%                     # filter for the "document" title to be equal to 6
  arrange(desc(count))                          # sort it in descending order by count  

# 6.2 Example: the great library heist

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%       # from the gutenberg_works data set, select the titles in the titles vector
  gutenberg_download(meta_fields = "title")           # specify meta_fields = "title" 
books
  
library(stringr)

# divide books into documents, each representing one chapter

by_chapter <- books %>%                                                                      # from the books data set
  group_by(title) %>%                                                                        # group by title  
  mutate(chapter = cumsum(str_detect(text, regex("^chapter", ignore_case = TRUE)))) %>%      # add a new column "chapter" corresponding to the chapter number
  ungroup() %>%                                                                              # ungroup them
  filter(chapter > 0) %>%                                                                    # filter the chapter column to contain values greater than 0
  unite(document, title, chapter)                                                            # unite the "title" and "chapter" columns into a single column named "document"
by_chapter

by_chapter_word <- by_chapter %>%            # from the by_chapter data set
  unnest_tokens(word, text)                  # apply "tokenization"
by_chapter_word

# Find document-word counts 

word_counts <- by_chapter_word %>%           # from the by_chapter_word data set   
  anti_join(stop_words) %>%                  # remove stop words
  count(document, word, sort = TRUE) %>%     # get the word count and sort them in descending order
  ungroup()                                  # ungroup them
word_counts

# 6.2.1 LDA on chapters

chapters_dtm <- word_counts %>%              # from the word_counts data set
  cast_dtm(document, word, n)                # convert it into a DocumentTermMatrix with the cast_dm() method
chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))    # apply the LDA algoithm; k refers to the number of topics wanted
chapters_lda    

chapter_topics <- tidy(chapters_lda, matriz = "beta")   # extract the per-topic-per-word probabilities with the "beta" method
chapter_topics

# find the top 5 terms within each topic

top_terms <- chapter_topics %>%     # from the chapter_topics data set
  group_by(topic) %>%               # group by topic
  top_n(5, beta) %>%                # select the top 10 terms per topic based on the beta value
  ungroup() %>%                     # ungroup them
  arrange(topic, -beta)             # sort them by beta in descending order
top_terms

library(ggplot2)

top_terms %>%                                               # from the tep_terms data set
  mutate(term = reorder_within(term, beta, topic)) %>%      # add a new column "term" and reorder it before plotting it with faceting
  ggplot(aes(term, beta, fill = factor(topic))) +           # plot term vs beta, filled by topic (converted into a factor)
  geom_col(show.legend = FALSE) +                           # specify columns as the type of plot
  facet_wrap(~ topic, scales = "free") +                    # create plots for each topic
  coord_flip() +                                            # flip coordinates
  scale_x_reordered()                                       # specify x-axis scale

# 6.2.2 Per-document classification

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")      # extract the per-document-per-topic probabilities with the "gamma" method
chapters_gamma

chapters_gamma <- chapters_gamma %>%                                     # from the chapters_gamma data set
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)   # separate the "document" column into two new columns ("title" and "chapter")
chapters_gamma

chapters_gamma %>%                                      # from the chapters_gamma data set 
  mutate(title = reorder(title, gamma * topic)) %>%     # add a new column "title" and reorder it before plotting it with faceting
  ggplot(aes(factor(topic), gamma)) +                   # plot topic (converted into a factor)
  geom_boxplot() +                                      # speficy boxplot as the type of plot
  facet_wrap(~ title)                                   # make a plot for each title

# find the topic that was most associated with each chapter using top_n()

chapter_classifications <- chapters_gamma %>%           # from the chapters_gamma data set
  group_by(title, chapter) %>%                          # group by title and chapter
  top_n(1, gamma) %>%                                   # select the top term per topic based on the gamma value
  ungroup()                                             # ungroup them
chapter_classifications

# compare each of the "consensus" topic for each book (the most common topic among its chapters), and see which were most often misidentified

books_topics <- chapter_classifications %>%      # from the chapter_classifications data set
  count(title, topic) %>%                        # get the topics count
  group_by(title) %>%                            # group them by title  
  top_n(1, n) %>%                                # get the top count per topic                
  ungroup() %>%                                  # ungroup them
  transmute(consensus = title, topic)            # transform the data set into a tibble with column names "consensus" and "topic"

chapter_classifications %>%                      # from the chapter_classifications data set
  inner_join(books_topics, by = "topic") %>%     # perform an inner join with the books_topics data set by topic
  filter(title != consensus)                     # filter for the rows where title is not equal to consensus

# 6.2.3 By word assignments: augment

assignments <- augment(chapters_lda, data = chapters_dtm)   # identify which words in each document were assigned to which topic
assignments

# find which words were incorrectly classified

assignments <- assignments %>%                                                # from the assingments data set
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%    # separate the "document" column into two new columns ("title" and "chapter")
  inner_join(books_topics, by = c(".topic" = "topic"))                        # perform an inner join with the books_topics data set by topic
assignments

# visualize confusion matrix

library(scales)

assignments %>%                                                     # from the assignmnets data set
  count(title, consensus, wt = count) %>%                           # get the count of title and consensus
  group_by(title) %>%                                               # group them by title
  mutate(percent = n / sum(n)) %>%                                  # add a new column "percent" where percent = n / sum(n)
  ggplot(aes(consensus, title, fill = percent)) +                   # plot consensus vs title, filled by percent
  geom_tile() +                                                     # add title
  scale_fill_gradient2(high = "red", label = percent_format()) +    # specify gradient fill
  theme_minimal() +                                                 # specify theme
  theme(axis.text.x = element_text(angle = 90, hjust = 1),          # set the x-axis angle
        panel.grid = element_blank()) +                             # set the panel grid
  labs(x = "Book words were assigned to",                           # specify the x-axis label  
       y = "nook words came from",                                  # specify the y-axis label
       fill = "% of assignments")                                   # specify the gill label

# what are the most commonly mistaken words?

wrong_words <- assignments %>%    # from the assignment data set
  filter(title != consensus)      # filter for the rows where title is not equal to consensus
wrong_words 

wrong_words %>%                                     # from the wrong_words data set
  count(title, consensus, term, wt = count) %>%     # get the term count
  ungroup() %>%                                     # ungroup them
  arrange(desc(n))                                  # sort n in descending order
