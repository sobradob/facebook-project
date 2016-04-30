library(tm)
library(stringr)
library(readr)
library(dplyr)
#create character vector where each thread is one element
#filter it to two people conversations in english
chat<-df %>% filter(language == "english") %>% 
  mutate(ppl =str_count(thread, pattern = "-")) %>%
  filter(ppl == 1) %>%
  select(thread,message) %>% group_by(thread) %>%
  summarise(crps = paste(message,sep=" ",collapse=""))
chat$thread<-gsub(pattern = "628590366@facebook.com",replacement = "Boaz Sobrado",x = chat$thread)

#quick fix and explore issues
chat$thread<-as.character(chat$thread)
summary(nchar(chat$crps))

#start cleaning of character vector to create a term document matrix
docs<-Corpus(VectorSource(chat$crps))
docs<-tm_map(docs, content_transformer(stringi::stri_trans_tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords('english'))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","‘ve ",
                 "‘re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot")
docs <- tm_map(docs, removeWords, myStopwords)

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
m<-as.matrix(dtm)

#compute cosine similarity
cosineSim <- function(x){
  as.dist(x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2)))))
}
cs <- cosineSim(m)

#arbitrary removing of unsimilar stuff
cs[cs < max(cs)/2] <- 0
cs <- round(cs,3)

write.csv(as.matrix(cs),file = "AdjacencyMatrix.csv")
write.csv(chat$thread,file="names.csv")

#periodically check stuff
writeLines(as.character(docs[[30]]))

#Gephi stuff is missing
#https://eight2late.wordpress.com/2015/12/02/a-gentle-introduction-to-network-graphs-using-r-and-gephi/