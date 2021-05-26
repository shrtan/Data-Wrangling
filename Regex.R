library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_
European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

#only keeping rows that have a percent sign (%) in the remain column
polls <- polls[str_detect(polls$Remain, "%"), ]
colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", 
                     "poll_type", "notes")

nrow(polls)

#converts the remain vector to a proportion between 0 and 1
as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100

#Use a function from stringr to convert "N/A" in the undecided column to 0
str_replace(polls$undecided, "N/A", "0%")

#extract the end day and month from dates
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[A-Za-z]+$")
end_date <- sapply(temp, function(x) x[length(x)])                                 


