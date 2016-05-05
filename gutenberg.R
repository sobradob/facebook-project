#Gutenberg package installation to compare nchar of my messages vs other literature

#install.packages("gutenbergr")
library(devtools)
devtools::install_github("dgrtwo/gutenbergr")


library(gutenbergr)
library(stringr)
library(dplyr)

gutenberg_works(title == "War and Peace")

war<-gutenberg_download(2600)


bible<-gutenberg_download(10)

aristotle_books <- gutenberg_works(author == "Aristotle") %>%
  gutenberg_download(meta_fields = "title")

sophocles_books <- gutenberg_works(author == "Sophocles") %>%
  gutenberg_download(meta_fields = "title")


plato_books <- gutenberg_works(author == "Plato") %>%
  gutenberg_download(meta_fields = "title")

nchar<-c(as.numeric(plato_books %>% mutate(len = nchar(text)) %>% summarise(sum(len))),
         as.numeric(bible %>% mutate(len = nchar(text)) %>% summarise(sum(len))),
         as.numeric(sophocles_books %>% mutate(len = nchar(text)) %>% summarise(sum(len))),
         as.numeric(war %>% mutate(len = nchar(text)) %>% summarise(sum(len))),
         as.numeric(df %>% filter(sender == "Boaz Sobrado") %>% select(msgLen) %>%sum()),
         as.numeric(df %>% filter(sender != "Boaz Sobrado") %>% select(msgLen) %>%sum()))

nnames<-c("Plato\nComplete works","King James\nBible","Sophocles\nComplete Works",
          "War & Peace\nTolstoy","Facebook messages\nI sent","Facebook messages\nI received")



plto<-data.frame(name = nnames, length = nchar, fb = c("F","F","F","F","T","T"))

ggplot(data=plto, aes(x=reorder(nnames,-length), y=length, fill = fb)) +
  geom_bar(width = 0.25, stat="identity")+theme_tufte(base_size=14, ticks=F)+  theme(axis.title=element_blank())  + 
  geom_hline(yintercept=seq(1, max(plto$length), 1000000), col="white", lwd=1)+
  scale_fill_manual(values=c("#6a9fb5","#b5806a"))+
  scale_y_continuous(labels=fancy_scientific)+ guides(fill=FALSE) +
  annotate("text", x = 6, y = 6000000, adj=1,  family="serif",
           label = c("Total number of characters\nbased on Project Gutenberg's libraries"))

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

#get the numbers in a vector