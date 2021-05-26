###DATES AND TIMES
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

data(brexit_polls)
brexit_polls$startdate <- as.Date(brexit_polls$startdate)

#How many polls had a start date (startdate) in April (month number 4)?
sum(month(brexit_polls$startdate) == 4)

#Use the round_date() function on the enddate column with the argument unit="week". 
#How many polls ended the week of 2016-06-12?
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

#Use the weekdays() function from lubridate to determine the weekday on which each poll ended.
#On which weekday did the greatest number of polls end?
table(weekdays(brexit_polls$enddate))


data(movielens)
#Data frame contains a set of about 100,000 movie reviews. 
#The timestamp column contains the review date as the number of seconds since 1970-01-01 (epoch time).

#Convert the timestamp column to dates using the lubridate as_datetime() function
movielens$timestamp <- as_datetime(movielens$timestamp)
#Which year had the most movie reviews?
max(table(year(movielens$timestamp)))
#Which hour of the day had the most movie reviews?
table(hour(movielens$timestamp))


###TEXT MINING
library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
install.packages("tidytext")
library(tidytext)
options(digits = 3)

#books and documents available in gutenbergr
gutenberg_metadata

#Use str_detect() to find the ID of the novel Pride and Prejudice
sum(str_detect(gutenberg_metadata$title, pattern = "Pride and Prejudice"), na.rm = TRUE)
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

#gutenberg_works() function filters this table to remove replicates and include only English language works. 
#Use this function to find the ID for Pride and Prejudice.
gutenberg_works(title=="Pride and Prejudice", languages = "en")
pap_id <- gutenberg_works(title == "Pride and Prejudice")$gutenberg_id

#Use the gutenberg_download() function to download the text for Pride and Prejudice. 
#Use the tidytext package to create a tidy table with all the words in the text.
words <- gutenberg_download(pap_id, mirror = "http://mirrors.xmission.com/gutenberg/") %>% 
        unnest_tokens(word, text)

#How many words are present in the book?
nrow(words)

#Remove stop words from the words object
wo_stopwords <- data %>% filter(!word %in% stop_words$word)
nrow(wo_stopwords)
#or
words <- words %>% anti_join(stop_words)
nrow(words)

#After removing stop words, detect and then filter out any token that contains a digit from words
#no_nums <- wo_stopwords[!str_detect(wo_stopwords$word, "[0-9]"),]
#nrow(no_nums)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

#words <- data.frame(table(words$word)) %>% rename(word = Var1)

#How many words appear more than 100 times in the book?
#sum(words$Freq > 100)
words %>%
  count(word) %>%
  filter(n > 100) %>%
nrow()

#What is the most common word in the book?
# words <- words[order(words$Freq, decreasing = TRUE), ]
# words[1,1]
words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

install.packages("textdata")
library(textdata)
#Define the afinn lexicon
afinn <- get_sentiments("afinn")

#Use this afinn lexicon to assign sentiment values to words
afinn_sentiments <- inner_join(words, afinn)

#How many elements of words have sentiments in the afinn lexicon?
nrow(afinn_sentiments)

colnames(words_sentiment)
#What proportion of words in afinn_sentiments have a positive value?
mean(afinn_sentiments$value > 0)

#How many elements of afinn_sentiments have a value of 4?
sum(afinn_sentiments$value == 4)
