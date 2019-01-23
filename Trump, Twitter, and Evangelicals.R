# New and Improved Project / Limited Tweets 

#Go to Twitter Developer Website to Learn More 
consumer_key <- "e5Jua8TSKjsNidJArQRp2bjee"
consumer_secret <- "NrL0sl9kMftTwx8SI69yomE1rmjFaGMTWVOMkJxU8Mm8nmzB6i"
access_token <-"374133211-eddPPJbiR2LACXnc4Yd8GxOcSZnee5iULGMGzcuF"
access_secret <- "WLZg6DS8Ttmo1RSOdoLudRnyRwUCaE9UdsZUj2Uo9Gfe6"
library(twitteR)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
1


library(stringr)
library(purrr)
library(tidytext)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)
library(broom)
library(ggplot2)
library(tm)


trump <- userTimeline("realDonaldTrump", n=3200, includeRts=T)
#Only 3193 collected on 11/26

graham <- userTimeline("Franklin_Graham", n=3200, includeRts=T)
#3200 collected on 11/26

jeffress <- userTimeline("robertjeffress", n=3200, includeRts=T)
#3200 collected on 11/26

burns <- userTimeline("pastormarkburns", n=3200, includeRts=T)
#3081 colected on 11/26

Scott <- userTimeline("PastorDScott", n=3200, includeRts=T)
#3199 collected  on 11/26

#dataframes 
df.trump <- twListToDF(trump)

df.graham <-twListToDF(graham)

df.jeffress <- twListToDF(jeffress)

df.burns <- twListToDF(burns)

df.Scott <- twListToDF(Scott)


#bindrows 

tweets <- bind_rows(
  df.trump %>% filter(isRetweet==F) %>%
    select(
      text, screenName, created, retweetCount, favoriteCount),
  df.graham %>% filter(isRetweet==F) %>%
    select(
      text, screenName, created, retweetCount, favoriteCount),
  df.jeffress %>% filter(isRetweet==F) %>%
    select(
      text, screenName, created, retweetCount, favoriteCount), 
  
  df.burns %>% filter(isRetweet==F) %>%
  select(
    text, screenName, created, retweetCount, favoriteCount),
  df.Scott %>% filter(isRetweet==F) %>%
  select(
    text, screenName, created, retweetCount, favoriteCount))


#13336 Tweets 

ggplot(tweets, aes(x = created, fill = screenName)) +
  geom_histogram(
    position = "identity", bins = 50, show.legend = TRUE) +
  facet_wrap(~screenName, ncol = 1) + 
  ggtitle("Tweet Activity 
             n = 10131 Tweets")

#Adjust Columns and Rows / Limit Jan 1 onwards/ New Dataframe 
attach(tweets)

tweets <- tweets %>%
  select(text, screenName, created, retweetCount, favoriteCount)%>%
  filter(created >= as.Date("2018-01-01"))


#Frequency of Tweets 

replace_reg <- "http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;"
unnest_reg  <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  mutate(id = row_number()) %>%
  unnest_tokens(
    word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

frequency <- tidy_tweets %>% 
  group_by(screenName) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(screenName) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

#Time and Frequency 
words_by_time <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  mutate(time_floor = floor_date(created, unit = "1 ")) %>%
  count(time_floor, screenName, word) %>%
  ungroup() %>%
  group_by(screenName, time_floor) %>%
  mutate(time_total = sum(n)) %>%
  group_by(word) %>%
  mutate(word_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(word_total > 30)
words_by_time 

words_by_time 

nested_data <- words_by_time %>%
  filter(screenName != "realdonaldTrump" | time_floor < ymd("20170101")) %>% 
  
  nest(-word, -screenName) 
nested_data

nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(
    cbind(count, time_total) ~ time_floor, ., 
    family = "binomial")))
nested_models

slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))
top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.1)
top_slopes

words_by_time %>%
  inner_join(top_slopes, by = c("word", "screenName")) %>%
  filter(screenName == "realDonaldTrump") %>%
  ggplot(aes(time_floor, count/time_total, color = word)) +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")

Extrarows <- words_by_time[6400:7869,]

words_by_time [7500, ]

write.csv(Extrarows, file = "Extrarows.csv")
