
library(tm)
library(SnowballC)
library(wordcloud)
library(stringi)
library(RColorBrewer)

wc <- Corpus(VectorSource(df[which(df$sender == "Zenab  China"),4]))
wc <- tm_map(wc, PlainTextDocument)
wc <- tm_map(wc, removePunctuation)
wc <- tm_map(wc, removeWords, stopwords('english'))
wc <- tm_map(wc, stemDocument)
wc<-tm_map(wc, content_transformer(stringi::stri_trans_tolower))


control<-list(tolower = F)

tflist <- mclapply(unname(content(wc)), termFreq, control)

termFreq(unname(content(wc)))

wordcloud(wc, max.words = 100, tolower = F,control=control,
          , random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
          