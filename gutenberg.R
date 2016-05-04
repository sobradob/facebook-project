#Gutenberg package installation to compare nchar of my messages vs other literature

install.packages("gutenbergr")
library(gutenbergr)
library(stringr)
library(dplyr)

gutenberg_works(title == "War and Peace")

war<-gutenberg_download(2600)

war %>% mutate(len = nchar(text)) %>% summarise(sum(len))

bible<-gutenberg_download(10)

bible %>% mutate(len = nchar(text)) %>% summarise(sum(len))


sophocles_books <- gutenberg_works(author == "Plato") %>%
  gutenberg_download(meta_fields = "title")

sophocles_books %>% mutate(len = nchar(text)) %>% summarise(sum(len))

plato_books <- gutenberg_works(author == "Plato") %>%
  gutenberg_download(meta_fields = "title")

plato_books %>% mutate(len = nchar(text)) %>% summarise(sum(len))
