library(tidyverse)
library(tidytext)


# choose bing lexicon
get_sentiments("bing")

# choose nrc lexicon
get_sentiments("nrc")


############################## Airlines sentiment analysis ########################################

airline_tweet=read.csv(file.choose())  # choose the airlines_sentiments tweet dataset > convert to tidy
names(airline_tweet)

airline_tweet=airline_tweet[,c(10,15)]
airline_tweet$line=1:nrow(airline_tweet)

airline_tweet$text=as.character(airline_tweet$text)

data(stop_words)
tweets2=airline_tweet %>% 
  unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  count(word, sort = TRUE)



#unnest_tokens_ is the "standard evaluation" version of 
# unnest_tokens, and allows you to pass in variable names 
# as strings

nrc=get_sentiments("nrc")

tweet_nrc= tweets2 %>% inner_join(nrc)

pos_words= tweet_nrc %>% filter(sentiment == "positive") %>%   # can be "sadness"/"joy"
               group_by(word) %>% summarize(freq = mean(n)) %>%
               arrange(desc(freq))


pos_words %>% top_n(20) %>% mutate(word = reorder(word, freq)) %>%
                            ggplot(aes(word, freq)) + geom_col() + coord_flip()



####### sentiment by airlines

tweets3=airline_tweet %>% group_by(airline) %>%
  unnest_tokens(word, text) %>% anti_join(stop_words) %>%
  count(word, sort = TRUE)

bing=get_sentiments("bing")

tweet_bing= tweets3 %>% inner_join(bing)


tweet_bing %>% group_by(airline, sentiment) %>% summarize(freq= mean(n)) %>%
                spread(sentiment, freq) %>% ungroup() %>%
                mutate(ratio= positive/negative, airlines=reorder(airline, ratio)) %>%
                ggplot(aes(airline,ratio)) + geom_point() + coord_flip()


### Challenge
# do a sentiment analysis as above with the hilter text



############################# William shakespeare #######################################

shakespeare=read.csv(file.choose())
shakespeare=shakespeare[,1:3]
shakespeare$linenumber=1:nrow(shakespeare)

# count how many times actor appears in play
shakespeare %>% count(Play, Actor)

library(tidytext)
shakespeare$text=as.character(shakespeare$text)

tidy_shakespeare= shakespeare %>% group_by(Play) %>%
                     unnest_tokens(word, text)%>% 
                     anti_join(stop_words) %>%
                     count(word, sort = TRUE)


tidy_shakespeare

shakespear_sentiment = tidy_shakespeare %>%
                       inner_join(get_sentiments("bing")) %>%
                         count(Play, sentiment) %>%
                         mutate(total = sum(nn), percent=nn/total)

shakespear_sentiment


## hamlet

tidy_hamlet_words = shakespeare %>% filter(Play=="Hamlet") %>%
                   group_by(Actor) %>%
                   unnest_tokens(word, text)%>% 
                   anti_join(stop_words) %>%
                   count(word, sort = TRUE) 

tidy_hamlet_words

tidy_hamlet_sentiment = tidy_hamlet_words %>%
                    inner_join(get_sentiments("bing")) %>%
                    mutate(total = sum(n), percent=n/total)

tidy_hamlet_sentiment


# we only want negative words
tidy_hamlet_sentiment_negative=tidy_hamlet_words %>%
                    inner_join(get_sentiments("bing")) %>%
                    mutate(total = sum(n), percent=n/total) %>%
                    filter(sentiment =="negative") %>%
                    arrange(percent)

tidy_hamlet_sentiment_negative




### group sentiments by Actor
tidy_hamlet_words %>%
  inner_join(get_sentiments("bing")) %>%
      group_by(Actor, sentiment)  %>% 
  summarize(freq= mean(n)) %>%
  spread(sentiment, freq) %>% ungroup() %>%
  mutate(ratio= positive/negative, Actor=reorder(Actor, ratio)) %>%
  ggplot(aes(Actor,ratio)) + geom_point() + coord_flip()


### top 10 negative and postive words

tidy_hamlet_words2 = shakespeare %>% filter(Play=="Hamlet") %>%
  group_by(Actor) %>%
  unnest_tokens(word, text)%>% 
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE)


top_words = tidy_hamlet_words2 %>%
            group_by(sentiment) %>%
            top_n(10) %>%
            ungroup %>%
            mutate(word=reorder(word, n))

top_words


ggplot(top_words, aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales="free") +
  coord_flip()


###### word contributions by plays

# negative sentiment contributions
tidy_shakespeare %>% count(Play, word, sort=TRUE) %>%
                    inner_join(get_sentiments("afinn")) %>%
                    filter(Play == "Hamlet" ,score<0)


sentiment_contributions =tidy_shakespeare %>% count(Play, word, sort=TRUE) %>%
                         inner_join(get_sentiments("afinn")) %>%
                         group_by(Play) %>%
                         mutate(contribution = score * nn/sum(nn)) %>%
                         ungroup()


sentiment_contributions

# filter by plays

sentiment_contributions %>% filter(Play== "Hamlet") %>%
                            arrange(contribution)


sentiment_contributions %>% filter(Play == "Merchant of Venice") %>%
  arrange(contribution)


### sentiment changes throughout play

# we break the play into chunks of 70 lines each
# analyse how the sentiment changes for each play

tidy_shakespeare2= shakespeare %>% group_by(Play,Actor,linenumber) %>%
  unnest_tokens(word, text)%>% 
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_shakespeare2 %>% inner_join(get_sentiments("bing")) %>%
                      count(Play, index=linenumber %/% 70, sentiment) %>%
                      spread(sentiment, nn, fill=0) %>%
                      mutate(sentiment=positive-negative) %>%
                      ggplot(aes(index, sentiment, fill=Play)) +
                      geom_col() +
                      facet_wrap(~Play, scales="free_x")

# for Merchant of Venice only > individual play


tidy_shakespeare2 %>% inner_join(get_sentiments("bing")) %>%
  count(Play, index=linenumber %/% 70, sentiment) %>%
  filter(Play == "Merchant of Venice") %>%
  spread(sentiment, nn, fill=0) %>%
  mutate(sentiment=positive-negative) %>%
  ggplot(aes(index, sentiment)) +
  geom_col()



################################# Guternbergr package #####################################

library(gutenbergr)

gutenberg_metadata %>% count(author, sort=TRUE)

# download william shakespeare
shakespear_meta <- gutenberg_metadata %>%
                              filter(author=="Shakespeare, William",
                              language == "en")


cesar=gutenberg_download(shakespear_meta$title == "The New Hudson Shakespeare: Julius Cæsar")


# download shelock homes
sherlock_holmes_metadata <- gutenberg_works() %>%
  filter(author == "Doyle, Arthur Conan") %>%
  semi_join(sherlock_holmes_subjects, by = "gutenberg_id")

holmes_books <- gutenberg_download(sherlock_holmes_metadata$gutenberg_id)
