#DESCRIPTIVES TO SPICE THINGS UP

#clean up some variables and add a new column
library(stringi)
library(dplyr)
df$date<-as.POSIXct(strptime(df$date,format = "%A, %B %e, %Y at %I:%M%p"))
df$message<-as.character(df$message)
df$msgLen<-nchar(df$message)
df$sender<-gsub(pattern = "628590366@facebook.com",replacement = "Boaz Sobrado",x = df$sender)


select(df, sender, msgLen) %>%
  filter(sender == "Boaz Sobrado") %>% 
  select(msgLen) %>% summary()

filter(df, sender == "Libertad Sobrado") %>% select(thread) 

select(df, sender, msgLen) %>%
  filter(sender != "Boaz Sobrado") %>% 
  select(msgLen) %>% summary()

filter(l, Var1 == "-Zsófi Császár")

x<-"Libertad Sobrado"
y<-"Boaz Sobrado"
chatPlot<-function(x,y){
  require(ggplot2)
  require(dplyr)
  thread.selected<-paste0(x,"-",y)
  p<-select(df,sender, thread,date) %>% 
    filter(thread == thread.selected) %>%
    filter(sender == x) %>%
    arrange(date) %>% select(date) %>% 
    group_by(date) %>% summarise(count = n())
  
  p$who<-x
  
  p2<-select(df,sender, thread,date) %>% 
    filter(thread == thread.selected) %>%
    filter(sender != x) %>%
    arrange(date) %>% select(date) %>% 
    group_by(date) %>% summarise(count = n())
  
  p2$who<-y
  
  p<-rbind(p,p2)
  
  ggplot(data=p, aes(x=date,fill =who)) +
    geom_bar(stat="bin", alpha=.5, position="dodge")
}

chatPlot("Boaz Sobrado","Miguel Sobrado")


select(df,sender,thread,date,msgLen) %>% 
  filter(thread == thread.selected) %>%
  filter(sender == "Boaz Sobrado") %>%
  arrange(date) %>% select(date, msgLen) %>% 
  group_by(date) %>% summarise(count = sum(msgLen))

chatPlot2<-function(x,y){
  require(ggplot2)
  require(dplyr)
  thread.selected<-paste0(x,"-",y)
  p<-select(df,sender,thread,date,msgLen) %>% 
    filter(thread == thread.selected) %>%
    filter(sender == x) %>%
    arrange(date) %>% select(date, msgLen) %>% 
    group_by(date) %>% summarise(count = sum(msgLen))
  
  p$who<-x
  
  cat(paste0(x, " sent ", sum(p$count)," characters \n"))
  p2<-select(df,sender, thread,date,msgLen) %>% 
    filter(thread == thread.selected) %>%
    filter(sender != x) %>%
    arrange(date) %>% select(date, msgLen) %>% 
    group_by(date) %>% summarise(count = sum(msgLen))
  
  p2$who<-y

  cat(paste0(y, " sent ", sum(p2$count)," characters \n"))
  
  p<-rbind(p,p2)
    
  ggplot(data=p, aes(x=date, y=count, group = who, colour=who)) +
    geom_line(size =1) +
    geom_point()
}


chatPlot("Boaz Sobrado","Zenab  China")
chatPlot2("Boaz Sobrado","Zenab  China")

#things to do

#calculate moving average of messages sent/recieved for top 20 facebook chat mates
library(zoo)

topThreads<-arrange(data.frame(table(df$thread)),Freq) %>% tail(5) %>% select(Var1)
topThreads$Var1<-as.character(topThreads$Var1)

df1<-df[df$thread %in% topThreads$Var1[-nrow(topThreads)],]
df1$day<-as.Date(strftime(df1$date,format = "%F"))

df1$day<-strftime(df1$date,format = "%Y-%m")

df1$thread<-gsub(x=df1$thread,pattern = "Boaz Sobrado", replacement = "")
df1$thread<-gsub(x=df1$thread,pattern = "-", replacement = "")

plot<-select(df1,thread,day,msgLen) %>% 
  group_by(thread,day) %>% summarise(total = sum(msgLen))

plot$day<-as.POSIXct(strptime(paste0(plot$day,"-01"),format = "%F"))

ggplot(data=plot, aes(x=day, y=total, group=thread, colour=thread)) +
  geom_line() +
  geom_point()

#total messages sent per month

plot.t<-select(df, date, msgLen, sender) %>%
  filter(sender == "Boaz Sobrado") %>%
  mutate(day = as.character(strptime(paste0(strftime(date,format = "%Y-%m"),"-01"),format = "%F")))%>%
  group_by(day) %>% summarise(total = sum(msgLen))

plot.t$sender<-"Sent"

plot.t2<-select(df, date, msgLen, sender) %>%
  filter(sender != "Boaz Sobrado") %>%
  mutate(day = as.character(strptime(paste0(strftime(date,format = "%Y-%m"),"-01"),format = "%F")))%>%
  group_by(day) %>% summarise(total = sum(msgLen))

plot.t2$sender<-"Recieved"

plot.t<-rbind(plot.t, plot.t2)

plot.t$day<-as.Date(plot.t$day)

ggplot(data=plot.t, aes(x=day, y=total, group = sender, colour=sender)) +
  geom_line() +
  geom_point()

#create yearly boat race rank plot
x<-10

ranking<-select(df, date, sender) %>% 
  filter(sender != "Boaz Sobrado") %>%
  mutate(year = strftime(date, format = "%Y")) %>%
  group_by(year,sender) %>% summarise(total = n()) %>%
  top_n(x) %>% filter(year >2010) %>% arrange(year,-total)
  
ranking$rank<-1:x
ranking$year<-as.numeric(ranking$year)

r2<-ranking

for(i in 2011:2016){
  y.names<-ranking[ranking$year == i,"sender"]
  missing<-setdiff(ranking$sender,y.names$sender)
  temp<-data.frame(sender = missing,
                   year   = i,
                   total  = NA,
                   rank   = "Not Ranked")
  r2<-rbind(r2,temp)
}

r2$rank<-with(r2,factor(r2$rank,levels = as.character(c("Not Ranked",x:1))))
  
ggplot(data=r2, aes(x=year, y=rank, group = sender, colour=sender)) +
  geom_line(size =2) +
  geom_point()+geom_text(aes(label=ifelse(rank !="Not Ranked",as.character(sender),'')),hjust=0.2, vjust=-2)+ 
  theme(legend.position="none")

#language? sex?



#what time do I send/recieve messages
library(lubridate)

years<-2016

z<-df %>%
  filter(is.null(years) | year(date) %in% years) %>% select(date)

x<-data.frame(time = strptime(paste0("2016-04-09 ",strftime(z$date,format = "%T")),format ="%Y-%m-%d %H:%M:%S" ))

ggplot(data=x, aes(x=time)) +
  geom_bar(stat="count")

df$thread<-as.character(df$thread)

#who sends more messages, me or them
df1<-df[df$thread %in% topThreads$Var1,]
  
select(df1,sender,thread,msgLen) %>% 
  group_by(thread, sender) %>% summarise(sum(msgLen)) %>% View()

chatPlot2("Boaz Sobrado", "Branden Ceebs Chan")
chatPlot("Boaz Sobrado", "Branden Ceebs Chan")
x<-"Boaz Sobrado"
y<-"Zenab  China"


select(df, thread,message,msgLen,date) %>% 
       filter(thread == thread.selected) %>%
       filter(sender = x) %>%
       filter(grepl('ounce|weed|bag|MD|high|drug', message)) %>%
       arrange(date) %>% select(date, msgLen) %>% 
       group_by(date) %>% summarise(count = sum(msgLen))

z<-'ounce|weed|bag|MD|high|drug'
freqWordPlot<-function(x,y,z){
  require(ggplot2)
  require(dplyr)
  thread.selected<-paste0(x,"-",y)
  p<-select(df, thread,message,msgLen,date,sender) %>% 
    filter(thread == thread.selected) %>%
    filter(sender == x) %>%
    filter(grepl(z, message)) %>%
    arrange(date) %>% select(date, msgLen) %>% 
    group_by(date) %>% summarise(count = sum(msgLen))
  
  p$who<-x
  
  p2<-select(df,thread,message,msgLen,date,sender) %>% 
    filter(thread == thread.selected) %>%
    filter(sender != x) %>%
    filter(grepl(z, message)) %>%
    arrange(date) %>% select(date) %>% 
    group_by(date) %>% summarise(count = n())
  
  p2$who<-y
  
  p<-rbind(p,p2)
  
  ggplot(data=p, aes(x=date,fill =who)) +
    geom_bar(stat="bin", alpha=.5, position="dodge")
}


#count how many times 
p<-select(df, thread,message,msgLen,date,sender) %>% 
filter(thread == thread.selected) %>%
  filter(sender == x) %>%
  filter(grepl(z, message)) %>%
  arrange(date) %>% select(date, msgLen) %>% 
  group_by(date) %>% summarise(count = sum(msgLen))