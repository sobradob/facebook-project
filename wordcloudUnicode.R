
library(tm)
library(SnowballC)
library(wordcloud)
library(stringi)
library(RColorBrewer)

df %>% filter(language == "english" & sender == "Boaz Sobrado") %>% select(message) ->v
war %>% select(text) ->wp

wordcloudify<-function(x){
  x <- Corpus(VectorSource(x[[1]]))
  x <- tm_map(x, PlainTextDocument)
  x <- tm_map(x, removePunctuation)
  x <- tm_map(x, removeWords, stopwords('english'))
  x <- tm_map(x, stemDocument)
  x<-tm_map(x, content_transformer(stringi::stri_trans_tolower))
  
  
  control<-list(tolower = F)
  
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
                   "great","lot","ill","the","said","dont")
  
  x <- tm_map(x, removeWords, myStopwords)
  return(x)
}

wc<-wordcloudify(v)
wp<-wordcloudify(wp)

myPa<-c("#6A9EB5","#b5816a","#6ab5a7","#6a79b5","#b56a79")

par(mfrow=c(1,2))
wordcloud(wc, max.words = 100, tolower = F,control=control,
          , random.order=FALSE, rot.per=0, use.r.layout=FALSE, colors=myPa)
title("My Facebook Messages")
wordcloud(wp, max.words = 100, tolower = F,control=control,
          , random.order=FALSE, rot.per=0, use.r.layout=FALSE, colors=myPa)
title("Tostoy's War & Peace")
dev.off()

#comparing corpora
library(devtools)
install_github("kasperwelbers/corpus-tools")

library(corpustools)

df %>% filter( language == "english" & sender != "Boaz Sobrado" & g_other =="female") %>% 
  select(message)->female

df %>% filter( language == "english" & sender != "Boaz Sobrado" & g_other =="male") %>% 
  select(message)->male


female<-wordcloudify(female)
male<-wordcloudify(male)

dtm.female <-DocumentTermMatrix(female)
dtm.male <-DocumentTermMatrix(male)

cmp <- corpora.compare(dtm.female, dtm.male)
cmp <- cmp[order(cmp$over), ]

#female compared to mael
cmp = cmp[order(cmp$over, decreasing=T), ]
head(cmp)

#wordcloud for female
female <- cmp[cmp$over > 1,]
dtm.wordcloud(terms = female$term, freqs = female$chi,pal = myPa,rot.per = 0,scale=c(3.5,1))

wordcloud(words = female$term, freqs = female$chi, max.words = 100, tolower = F,control=control,
          random.order=FALSE, rot.per=0, use.r.layout=FALSE, colors=myPa,scale=c(1,5))

#wordcloud for male
male <- cmp[cmp$over < 1,]
dtm.wordcloud(terms = male$term, freqs = male$chi,pal = myPa,rot.per = 0,scale=c(3.5,1))

#does not work as cannot allocate vector of size 5.3
m <- as.matrix(male_dtm)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)



par(mfrow=c(1,2))
wordcloud(female, max.words = 100, tolower = F,control=control,
          , random.order=FALSE, rot.per=0, use.r.layout=FALSE, colors=myPa)
title("Sent to females")
wordcloud(male, max.words = 100, tolower = F,control=control,
          , random.order=FALSE, rot.per=0, use.r.layout=FALSE, colors=myPa)
title("Sent to males")
dev.off()
