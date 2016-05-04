#Cluster analysis

#first step, try text categorisation
#install.packages("textcat")
library(textcat)
library(dplyr)
library(ggplot2)

df$language<-textcat(df$message)

#exploring
select(df,message,language) %>% filter(language == "basque")%>% 
  select(message)

#code something like that
lang.m<-select(df,thread,language) %>% group_by(thread,language) %>% summarise(count = n()) %>%
  top_n(1)

df<-merge(df,lang.m,all.x = T,by = "thread")
df<-df[,-6]
colnames(df)[6:7]<-c("language","thread_length")

df<-df %>% filter(language == "hungarian" |
              language == "english" |
              language == "spanish" |
              language == "german")#4k messages lost

ggplot(data = df, aes(x= language))+geom_bar(stat="count")#theme stuff is missing

ggplot(data = filter(df,sender == "Boaz Sobrado"),
       aes(x= language))+geom_bar(stat="count")#theme stuff is missing

#descriptives on language

#who do I speak to the most in each language?

df %>% filter(language == "english") %>% 
  group_by(thread) %>% summarise(count = n()) %>%
  top_n(10)

df %>% filter(language == "spanish") %>% 
  group_by(thread) %>% summarise(count = n()) %>%
  top_n(10)

df %>% filter(language == "hungarian") %>% 
  group_by(thread) %>% summarise(count = n()) %>%
  top_n(10)

df %>% filter(language == "german") %>% 
  group_by(thread) %>% summarise(count = n()) %>%
  top_n(10)

#how many times I mention another person's name

#how many times the other person mention's my name

#when did I speak which language predominantly