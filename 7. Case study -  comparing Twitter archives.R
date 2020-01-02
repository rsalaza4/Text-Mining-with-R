### TEXT MINING WITH R - A Tidy Approach ###
### By Julia Silge and David Robinson
### Book available at: https://www.tidytextmining.com/

# CHAPTER 7: CASE STUDY: COMPARING TWITTER ARCHIVES

# 7.1 Getting the data and distribution of tweets

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

tweets_julia <- read_csv("https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/tweets_julia.csv")
tweets_dave <- read_csv("https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/tweets_dave.csv")

tweets <- bind_rows(tweets_julia %>%                    # create a new data frame "tweets" with Julia's tweets
                      mutate(person = "Julia"),         # add a new column "person" where person = Julia 
                    tweets_dave %>%                     # include also David's tweets
                      mutate(person = "David")) %>%     # add a new column "person" where person = David
  mutate(timestamp = ymd_hms(timestamp))                # add a new column "timestamp" corresponding to the tweets' dates 
tweets

ggplot(tweets, aes(x = timestamp, fill = person)) +                           # plot timestamp
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +     # specify histogram as the type of plot
  facet_wrap(~person, ncol = 1)                                               # make a plot for each author

# 7.2 Word frequencies

library(tidytext)
library(stringr)

remove_reg <- "&amp;|&lt;|&gt"                                 # specify remove_reg variable with string "&amp;|&lt;|&gt"
tidy_tweets <- tweets %>%                                      # from the tweets data frame
  filter(!str_detect(text, "^RT")) %>%                         # filter the "text" column for those values that do not contain "RT"
  mutate(text = str_remove_all(text, remove_reg)) %>%          # add a new column "text" removing the string "&amp;|&lt;|&gt" on the "text" column
  unnest_tokens(word, text, token = "tweets") %>%              # "tokenize" the "text" column using the "tweets" token
  filter(!word %in% stop_words$word,                           # filter the "word" column to do not contain stop words
         !word %in% str_remove_all(stop_words$word, "'"),      # filter the "word" column to do not contain stop words with "'"
         str_detect(word, "[a-z]"))                            # filter the "word" column for words that start with a letter or @ or #
tidy_tweets

frequency <- tidy_tweets %>%                   # from the tidy_tweets data frame
  group_by(person) %>%                         # group it by person
  count(word, sort = TRUE) %>%                 # get the total count per word and sort it in descending order
  left_join(tidy_tweets %>%                    # perform a left join with the tidy_tweets data frame
              group_by(person) %>%             # group them by person
              summarise(total = n())) %>%      # add a new column "total" with the total number of words per author
  mutate(freq = n/total)                       # add a new column "freq" corresponding to n/total
frequency

# plot these frequencies in the x- and y-axes of a plot

library(tidyr)

frequency <- frequency %>%            # from the frequency data frame
  select(person, word, freq) %>%      # select the "person", "word" and "freq" columns
  spread(person, freq) %>%            # spread the "freq" column by "person" 
  arrange(Julia, David)               # sort by rows that contain values on both "Julia" and "David" columns
frequency
  
library(scales)

ggplot(frequency, aes(Julia, David)) +                                  # plot Julia vs David
  geom_jitter(alpha = 0.1, size = 2.5, width = 2.5, height = 2.5) +     # specify geom_jitter as the type of plot
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +     # specift text labels
  scale_x_log10(labels = percent_format()) +                            # specify x-axis scale
  scale_y_log10(labels = percent_format()) +                            # specify y-axis scale
  geom_abline(color = "red")                                            # specify diagonal line
  
# 7.3 Comparing word usage

tidy_tweets <- tidy_tweets %>%                   # from the tidy_tweets data frame
  filter(timestamp >= as.Date("2016-01-01"),     # filter for tweets sent after 2016-01-01
         timestamp < as.Date("2017-01-01"))      # and before 2017-01-01
tidy_tweets
  
word_ratios <- tidy_tweets %>%                                 # from the tidy_tweets data frame
  filter(!str_detect(word, "^@")) %>%                          # filter the "word" column for words that do not contain @
  count(word, person) %>%                                      # get the word count
  group_by(word) %>%                                           # group words
  filter(sum(n) >= 10) %>%                                     # filter for words where n >= 10     
  ungroup() %>%                                                # ungroup them
  spread(person, n, fill = 0) %>%                              # spread the "n" column by "person"
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%     # apply log odds ratio formula
  mutate(logratio = log(David / Julia)) %>%                    # add a new column "logratio" where logratio = log(David / Julia)
  arrange(desc(logratio))                                      # arrange "logratio" column in descending order
word_ratios                                                     
   
word_ratios %>%                                                
  arrange(abs(logratio))                                          # arrange the words_ratios data frame by the absolute value of "logratio" column
  
word_ratios %>%                                                   # from the word_ratios data frame
  group_by(logratio < 0) %>%                                      # group by logratio < 0
  top_n(15, abs(logratio)) %>%                                    # select the top 15 terms based on the absolute value of logratio
  ungroup() %>%                                                   # ungroup them
  mutate(word = reorder(word, logratio)) %>%                      # add a new column "word" reordered by logratio
  ggplot(aes(word, logratio, fill = logratio < 0)) +              # plot word vs logratio 
  geom_col(show.legend = FALSE) +                                 # specify columns as the type of plot 
  coord_flip() +                                                  # flip coordinates
  ylab("log odds ratio (David/Julia)") +                          # specify the y-axis label
  scale_fill_discrete(name = "", labels = c("David", "Julia"))    # set the scale fill discrete by author

# 7.4 Changes in word use
  
words_by_time <- tidy_tweets %>%                                        # from the tidy_tweets data frame
  filter(!str_detect(word, "^@")) %>%                                   # filter the "word" column to do not contain words with @
  mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%      # add a new column "time floor" with the time by month
  count(time_floor, person, word) %>%                                   # get the word count
  group_by(person, time_floor) %>%                                      # group by person and time_floor
  mutate(time_total = sum(n)) %>%                                       # add a new column "time_total" with the monthly word frequency
  group_by(person, word) %>%                                            # group by person and word
  mutate(word_total = sum(n)) %>%                                       # add a new column "word_total" with the yearly word frequency
  ungroup() %>%                                                         # ungroup them
  rename(count = n) %>%                                                 # rename the "n" column into "count"
  filter(word_total > 30)                                               # filter the "word_total" column to contain words used more than 30 times
words_by_time

nested_data <- words_by_time %>%                                        # from the words_by_time data frame
  nest(-word, -person)                                                  # nest all columns except "word" and "person"
nested_data

library(purrr)

nested_models <- nested_data %>%                                                # from the nested_data data frame
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, .,     # add a new column "models" with a glm
                                  family = "binomial")))                        # use family = "binomial"
nested_models

library(broom)

slopes <- nested_models %>%                           # from the nested_models data frame
  mutate(models = map(models, tidy)) %>%              # add a new column "models" where map(models, tidy)
  unnest(cols = c(models)) %>%                        # unnest and add the columns from the models data frame columns
  filter(term == "time_floor") %>%                    # filter the "term" column for time_floor only
  mutate(adjusted.p.value = p.adjust(p.value))        # add a new column "adjusted.p.value"
slopes

top_slopes <- slopes %>%                 # from the slopes data frame
  filter(adjusted.p.value < 0.05)        # filter for the "adjusted.p.value" to be lower than 0.05
top_slopes

words_by_time %>%                                               # from the words_by_time data frame
  inner_join(top_slopes, by = c("word", "person")) %>%          # perform an inner join with top_slopes by the "word" and "person" column
  filter(person == "David") %>%                                 # filter the "person" column to contain David
  ggplot(aes(time_floor, count/time_total, color = word)) +     # plot time_floor vs count/time_total, colored by word
  geom_line(size = 1.3) +                                       # specify geom_line as the type of plot
  labs(x = NULL, y = "Word frequency")                          # specify the y-axis label

words_by_time %>%                                               # from the words_by_time data frame
  inner_join(top_slopes, by = c("word", "person")) %>%          # perform an inner join with top_slopes by the "word" and "person" column
  filter(person == "Julia") %>%                                 # filter the "person" column to contain Julia
  ggplot(aes(time_floor, count/time_total, color = word)) +     # plot time_floor vs count/time_total, colored by word
  geom_line(size = 1.3) +                                       # specify geom_line as the type of plot
  labs(x = NULL, y = "Word frequency")                          # specify the y-axis label

# 7.5 Favorites and retweets

tweets_julia <- read_csv("https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/juliasilge_tweets.csv")
tweets_dave <- read_csv("https://raw.githubusercontent.com/dgrtwo/tidy-text-mining/master/data/drob_tweets.csv")

tweets <- bind_rows(tweets_julia %>%                    # create a new data frame "tweets" with Julia's tweets
                      mutate(person = "Julia"),         # add a new column "person" where person = Julia 
                    tweets_dave %>%                     # include also David's tweets
                      mutate(person = "David")) %>%     # add a new column "person" where person = David
  mutate(created_at = ymd_hms(created_at))              # add a new column "created_at" corresponding to the tweets' dates 
tweets

tidy_tweets <- tweets %>%                                               # from the tweets data frame
  filter(!str_detect(text, "^RT|@")) %>%                                # filter the "text" column to do not contain rows starting with RT or @
  mutate(text = str_remove_all(text, remove_reg)) %>%                   # add a new column "text" removing the string "&amp;|&lt;|&gt" on the "text" column
  unnest_tokens(word, text, token = "tweets", strip_url = TRUE) %>%     # "tokenize" the "text" column using the "tweets" token
  filter(!word %in% stop_words$word,                                    # filter the "word" column to do not contain stop words
         !word %in% str_remove_all(stop_words$word, "'"))               # filter the "word" column to do not contain stop words containing "´"
tidy_tweets

# note: words in the "word"column starting with \ represent tokenized emojis.

totals <- tidy_tweets %>%                   # from the tidy_tweets data frame
  group_by(person, id) %>%                  # group it by person and id
  summarise(rts = first(retweets)) %>%      # get the retweets count for each id
  group_by(person) %>%                      # group them by person
  summarise(total_rts = sum(rts))           # get the total count of retweets
totals

words_by_rts <- tidy_tweets %>%                         # from the tidy_tweets data frame   
  group_by(id, word, person) %>%                        # group it by id, word and person
  summarise(rts = first(retweets)) %>%                  # get the count of how many times each word was retweeted, for each tweet and person
  group_by(person, word) %>%                            # group them by person and word
  summarise(retweets = median(rts), uses = n()) %>%     # get the median number of retweets for each word and person and the count of number of times each word was used ever by each person
  left_join(totals) %>%                                 # perform a left join with the total data frame, joined by person
  filter(retweets != 0) %>%                             # filter the "retweets" column to contain values different than 0
  ungroup()                                             # ungroup them
words_by_rts

words_by_rts %>%                   # from the words_by_rts data frame
  filter(uses >= 5) %>%            # filter the "uses" colum to contain values greater or equal to 5
  arrange(desc(retweets))          # sort in descending order the "retweets" column

words_by_rts %>%                                                            # from the words_by_rts data frame
  filter(uses >= 5) %>%                                                     # filter the "uses" colum to contain values greater or equal to 5
  group_by(person) %>%                                                      # group them by person
  top_n(10, retweets) %>%                                                   # get the top 10 retweeted words per person
  arrange(retweets) %>%                                                     # sort the "retweets" column in ascending order                                           
  ungroup() %>%                                                             # ungroup them     
  mutate(word = factor(word, unique(word))) %>%                             # add a new column "word" where each word corresponds to a unique factor
  ungroup() %>%                                                             # ungroup them  
  ggplot(aes(word, retweets, fill = person)) +                              # plot word vs retweets, filled by person
  geom_col(show.legend = FALSE) +                                           # specify columns as the type of plot
  facet_wrap(~ person, scales = "free", ncol = 2) +                         # make a plot for each person, specify the axes scale and the number of columns
  coord_flip() +                                                            # flip coordinates
  labs(x = NULL,                                                            # specify the x-axis label
       y = "Median # of retweets for tweets containing each word")          # specify the y-axis label

# same approach with favorite word

totals <- tidy_tweets %>%                   # from the tidy_tweets data frame
  group_by(person, id) %>%                  # group it by person and id
  summarise(favs = first(favorites)) %>%    # get the favorites count for each id
  group_by(person) %>%                      # group them by person
  summarise(total_favs = sum(favs))         # get the total count of favorites
totals

words_by_favs <- tidy_tweets %>%                          # from the tidy_tweets data frame   
  group_by(id, word, person) %>%                          # group it by id, word and person
  summarise(favs = first(favorites)) %>%                  # get the count of how many times each word was favorited, for each tweet and person
  group_by(person, word) %>%                              # group them by person and word
  summarise(favorites = median(favs), uses = n()) %>%     # get the median number of favorites for each word and person and the count of number of times each word was used ever by each person
  left_join(totals) %>%                                   # perform a left join with the total data frame, joined by person
  filter(favorites != 0) %>%                              # filter the "favorites" column to contain values different than 0
  ungroup()                                               # ungroup them
words_by_favs

words_by_favs %>%                                                           # from the words_by_favs data frame
  filter(uses >= 5) %>%                                                     # filter the "uses" colum to contain values greater or equal to 5
  group_by(person) %>%                                                      # group them by person
  top_n(10, favorites) %>%                                                  # get the top 10 favoriteD words per person
  arrange(favorites) %>%                                                    # sort the "favorites" column in ascending order                                           
  ungroup() %>%                                                             # ungroup them     
  mutate(word = factor(word, unique(word))) %>%                             # add a new column "word" where each word corresponds to a unique factor
  ungroup() %>%                                                             # ungroup them  
  ggplot(aes(word, favorites, fill = person)) +                             # plot word vs favorites, filled by person
  geom_col(show.legend = FALSE) +                                           # specify columns as the type of plot
  facet_wrap(~ person, scales = "free", ncol = 2) +                         # make a plot for each person, specify the axes scale and the number of columns
  coord_flip() +                                                            # flip coordinates
  labs(x = NULL,                                                            # specify the x-axis label
       y = "Median # of favorites for tweets containing each word")         # specify the y-axis label
