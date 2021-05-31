#In the extdata directory of the dslabs package, there's a PDF file containing daily mortality data 
#for Puerto Rico from Jan 1, 2015 to May 31, 2018
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

# create a tidy dataset with each row representing one observation. 
# The variables in this dataset will be year, month, day and deaths.

#install.packages("pdftools")
library("pdftools")
#Using the pdftools package to read in fn using the pdf_text() function
txt <- pdf_text(fn)
#txt contains a character string of length 12. Each entry represents the text in each page.
#The mortality is in there somewhere. 


library(stringr)
#Question 3: Extract the ninth page of the PDF file from the object txt. 
txt[9]
#Use the str_split() function from the stringr package so that each line is a different entry
x <- txt[9] %>% str_split(pattern = "\n")


#Question 4: Define s to be the first entry of the x object
s <- x[[1]]
length(s)


#Question 5: Trim the white space before and after the other characters
s <- s %>% str_trim(side = "both")
s[1]


#Question 6: preserve the string with the column header, which includes the month abbreviation
#find the row with the header

#Find thr rows that contain 2015: 3 and 25
header_index <- str_which(s, pattern = "2015")
#extract first row value 25
header_index <- header_index[1]
#replace multiple spaces between characters with one space 
header <- gsub("\\s+", " ", s[header_index])
#split the string by a space and convert into a matrux using the simplify arg
header <- str_split(header, " ", simplify = TRUE)

month <- header[1,1]
header <- header[1, -1]
header[3]


#Question 8: Notice that towards the end of the page defined by s there's a "Total" row followed by 
#rows with other summary statistics. Create an object called tail_index with the index of the "Total" entry
tail_index <- str_which(s, pattern = "Total")


#Question 9: The PDF page includes graphs with numbers, some of the rows have just one number 
#(from the y-axis of the plot) 
#Use the str_count() function to create an object n with the count of numbers in each row
n <- str_count(s, pattern = "[0-9]+")
#rows that have a single number in them
single_num <- str_which(n, "1")


#Question 10: Remove entries from rows that I know we don't need 
#The entry header_index and everything before it should be removed. 
#Entries for which n is 1 should also be removed, and the entry tail_index and everything that 
#comes after it should be removed as well
rem_rows <- c(1:header_index, single_num, tail_index:length(s))
s <- s[-rem_rows]
#entries that remain in s
length(s)


#Question 11: Remove all text that is not a digit or space
s <- str_remove_all(s, "[^\\d\\s]")


#Question 12: Convert s into a data matrix with just the day and death count data
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]
tab <- as.data.frame(s)

#Add column names to the matrix: the first column is day and the next columns should be the header
colnames(s) <- c("Day", header)
#mean number of deaths per day in September 2015
mean(as.numeric(s[,"2015"]))
#mean number of deaths per day in September 2016
mean(as.numeric(s[,"2016"]))

#Hurricane María hit Puerto Rico on September 20, 2017 
#mean number of deaths per day from September 1-19, 2017, before the hurricane hit
mean(as.numeric(s[1:19,"2017"]))
#mean number of deaths per day from September 20-30, 2017, after the hurricane hit
mean(as.numeric(s[20:30,"2017"]))


#Question 13: Changing tab to a tidy format
library(tidyverse)
tab <- tab %>% gather(year, deaths, -Day) %>%
  mutate(deaths = as.numeric(deaths))
tab
#gather fills the blank to generate a data frame with columns named "day", "year" and "deaths"


#Question 14: Plot of deaths versus day with color to denote year
tab_no2018 <- tab[tab$year != 2018,]
library(ggplot2)
tab %>% filter(year != 2018) %>% ggplot(mapping = aes(Day, deaths, color = year, group = year)) + 
  geom_point() +
  geom_line() + 
  geom_vline(xintercept = 20)
