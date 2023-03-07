#Installing data from github for library
devtools::install_github("bradleyboehmke/harrypotter")

#importing libraries
library(tidyverse) #for data manipulation & plotting 
library(stringr)   #for text cleaning & regular expressions
library(tidytext)  #for text mining tasks
library(harrypotter)  #for harry potter books


#viewing raw text
View(philosophers_stone[1:2])


#tibbling 
text_tb <- tibble(chapter = seq_along(philosophers_stone),text = philosophers_stone)


View(text_tb)

#applying unnest token (split to single words, Remove puncuation )
text_tb %>% unnest_tokens(word, text)
text_tb

#books names
titles <- c("Philosopher's Stone", "Chamber of secrets","Prisoner of Azkaban","Goblet of Fire","Order of the Phoenix","Half-Blood Prince","DEATHLY HALLOWS")

titles

# Books List

books <- list(philosophers_stone,chamber_of_secrets,prisoner_of_azkaban,goblet_of_fire,order_of_the_phoenix,half_blood_prince,deathly_hallows)

View(books)

#Tibbling & unnest tokens for all books

series <- tibble()


for(i in seq_along(titles))
{
  clean <- tibble(chapter= seq_along(books[[i]]),text =books [[i]]) %>%
    unnest_tokens(word,text) %>% mutate(book = titles[i]) %>%
     select(book,everything())
  series <- rbind(series,clean)}
View(clean)
View(series) 


#Setting books in order 
series$book <- factor(series$book, levels = rev(titles))
View(series)
