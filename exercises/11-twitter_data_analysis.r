### if you need to merge tweets into a single file > the code is below

# library(tidyverse)
# library(lubridate)
# tweets_hillary=read_csv(file.choose())
# tweets_trump=read_csv(file.choose())
# tweets_hillary=tweets_hillary[,1:13]
# tweets_trump=tweets_trump[,1:13]
# 
# tweets=bind_rows(tweets_hillary, tweets_trump) %>%
#   mutate(created_at= ymd_hms(in_reply_to_screen_name))



# data and distribution of tweets from twitter archive

library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

tweets <- read_csv(file.choose())                        # choose hillary_N_trump
ggplot(tweets, aes(x = time, fill = handle)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~handle, ncol = 1)


#  Word frequencies

library(tidytext)
library(stringr)

replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# calculate word frequencies for each person

frequency <- tidy_tweets %>% 
  group_by(handle) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(handle) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency




library(tidyr)

frequency <- frequency %>% 
  select(handle, word, freq) %>% 
  spread(handle, freq) %>%
  arrange(HillaryClinton, realDonaldTrump)

frequency


library(scales)

ggplot(frequency, aes(HillaryClinton, realDonaldTrump)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")



######################  Comparing word usage

tidy_tweets <- tidy_tweets %>%
  filter(time >= as.Date("2016-01-01"),
         time < as.Date("2017-01-01"))


word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, handle) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(handle, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(HillaryClinton / realDonaldTrump)) %>%
  arrange(desc(logratio))


#words that have been about equally likely to come from Hillary or Trump’s account

word_ratios %>% 
  arrange(abs(logratio))


word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col() +
  coord_flip() +
  ylab("log odds ratio (Hillary/Trump)") +
  scale_fill_discrete(name = "", labels = c("Hillary", "Trump"))


###################### Changes in word use

#Which words’ frequencies have changed the fastest in our Twitter feeds? Or to state this #another way, which words have we tweeted about at a higher or lower rate as time has # passed

library(lubridate)

words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(time, unit = "1 month")) %>%
  count(time_floor, handle, word) %>%
  ungroup() %>%
  group_by(handle, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)

words_by_time


#We can use nest() from tidyr to make a data frame with a list column that contains little #miniature data frames for each word

nested_data <- words_by_time %>%
  nest(-word, -handle) 

nested_data


# “Was a given word mentioned in a given time bin? Yes or no? How does the count of word # mentions depend on time?”


library(purrr)

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

nested_models


library(broom)

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

# Which words have changed in frequency at a moderately 
# significant level in our tweets

top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)

top_slopes


words_by_time %>%
  inner_join(top_slopes, by = c("word", "handle")) %>%
  filter(handle == "HillaryClinton") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency of Clinton")


words_by_time %>%
  inner_join(top_slopes, by = c("word", "handle")) %>%
  filter(handle == "realDonaldTrump") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency of Trump")


###################### Favorites and retweets


#  Let’s remove all retweets and replies from this data set 
# so we only look at regular 
# tweets that Hillary and Trump have posted directly.

# let’s look at the number of times each of our tweets was retweeted. 
# Let’s find the total 
# number of retweets for each person.

totals <- tidy_tweets %>% 
  group_by(handle, id) %>% 
  summarise(rts = sum(retweet_count)) %>% 
  group_by(handle) %>% 
  summarise(total_rts = sum(rts))

totals



#Next, we can join this to the data frame of retweet totals. 
# Let’s filter() to only keep  
# words mentioned at least 5 times.

word_by_rts <- tidy_tweets %>% 
  group_by(id, word, handle) %>% 
  summarise(rts = first(retweet_count)) %>% 
  group_by(handle, word) %>% 
  summarise(retweet_count = median(rts), uses = n()) %>%
  left_join(totals) %>%
  filter(retweet_count != 0) %>%
  ungroup()

word_by_rts %>% 
  filter(uses >= 5) %>%
  arrange(desc(retweet_count))


# Let’s plot the words that have the highest median retweets 
# for each of our accounts

word_by_rts %>%
  filter(uses >= 5) %>%
  group_by(handle) %>%
  top_n(10, retweet_count) %>%
  arrange(retweet_count) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, retweet_count, fill = handle)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ handle, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of retweets for tweets containing each word")



#  to see which words led to more favorites

totals <- tidy_tweets %>% 
  group_by(handle, id) %>% 
  summarise(favs = sum(favorite_count)) %>% 
  group_by(handle) %>% 
  summarise(total_favs = sum(favs))

word_by_favs <- tidy_tweets %>% 
  group_by(id, word, handle) %>% 
  summarise(favs = first(favorite_count)) %>% 
  group_by(handle, word) %>% 
  summarise(favorites = median(favs), uses = n()) %>%
  left_join(totals) %>%
  filter(favorites != 0) %>%
  ungroup()


word_by_favs %>%
  filter(uses >= 5) %>%
  group_by(handle) %>%
  top_n(10, favorites) %>%
  arrange(favorites) %>%
  ungroup() %>%
  mutate(word = factor(word, unique(word))) %>%
  ungroup() %>%
  ggplot(aes(word, favorites, fill = handle)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ handle, scales = "free", ncol = 2) +
  coord_flip() +
  labs(x = NULL, 
       y = "Median # of favorites for tweets containing each word")
