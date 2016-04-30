library(rvest)
library(dplyr)
#failed at this, used Python repo instead
rawMSG<-read_html(x = "C:\\Users\\user1\\Desktop\\Life\\RProjects\\facebookMsg\\facebook\\facebook-boazsobrado (1)\\html\\messages.htm",encoding = "UTF-8")

rawMSG %>% 
  html_nodes("div.thread:first-child") %>%
  html_text() %>% head()

nameDate<-rawMSG %>% 
  html_nodes("span") %>%
  html_text()

msg<-rawMSG %>% 
  html_nodes("p") %>%
  html_text()


allMSG<-data.frame(name = nameDate[seq(from = 1,to = length(nameDate),2)],
           date = nameDate[seq(from = 2,to = length(nameDate),2)],
           message = as.character(msg),stringsAsFactors = F)

Encoding(allMSG$message)<-"UTF-8"
Encoding(allMSG$name)<-"UTF-8"

#fix date
foo <- data.frame(do.call('rbind', strsplit(as.character(allMSG$date),' ',fixed=TRUE)))
foo$X3<-as.numeric(gsub(foo$X3,pattern = ",",replacement = ""))
date<-paste(foo[,3],foo[,2],foo[,4],foo$X6,sep ="-")
date<-strptime(date,format = "%e-%B-%Y-%I:%M%p")
allMSG$date<-date


allMSG[which(allMSG$name == "Fruzsina Mezei")[1]]

allMSG[284576:which(allMSG$name == "Fruzsina Mezei")[length(which(allMSG$name == "Fruzsina Mezei"))],]

freq<-data.frame(table(allMSG$name))
arrange(freq,Freq)

allMSG[383447]
