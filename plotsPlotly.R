#Just plots and Plotly
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)
library(scales)
library(plotly)


#sign up to plotly

signup("sobradob","sobradob@gmail.com",save=T)
api_key<-"rlw8ozul8m"
p<- plotly(username="sobradob", key=api_key)

#what time of the day do I send/recieve messages

counts<-df %>% select(date) %>% filter(date > "2015-11-01")%>%
  mutate(rounded_hour  = floor_date(date,unit = "hour"),
         rounded_hour2 = as.POSIXct(paste0("2016-11-01 ",strftime(rounded_hour,format = "%T")),tz = "GMT")) %>%
  group_by(rounded_hour2) %>% summarise(hourly_total = n())

ggplot(data=counts, aes(x=rounded_hour2, y=hourly_total)) +
  geom_bar( fill="#6a9fb5", stat="identity")+theme_tufte(base_size=14, ticks=F)+
  scale_x_datetime(breaks=date_breaks("4 hour"), labels=date_format("%H:%M"))+  theme(axis.title=element_blank())  + 
  geom_hline(yintercept=seq(1, max(counts$hourly_total), 500), col="white", lwd=1)

#monthly message sending

counts_monthly<-df %>% select(date) %>% filter(date > "2010-01-01" & date <"2015-12-31")%>%
  mutate(Date  = floor_date(date,unit = "month")) %>%
  group_by(Date) %>% summarise(Total = n())

p<-ggplot(data=counts_monthly, aes(x=rounded, y=total)) +
  geom_bar(fill="#6a9fb5", stat="identity")+theme_tufte(base_size=14, ticks=F)+  theme(axis.title=element_blank())  + 
  geom_hline(yintercept=seq(1, max(counts_monthly$total), 2500), col="white", lwd=1)+ 
  scale_x_datetime(breaks=date_breaks("12 months"), labels=date_format("%Y"))

(gg <- ggplotly(p))
plotly_POST(gg, filename = "r-docs/fbmonthly")

#different languages
levels(df$language)

ggplot(data = filter(df,sender == "Boaz Sobrado"),
       aes(x= language))+
  geom_bar(fill="#6a9fb5", stat="count")+theme_tufte(base_size=14, ticks=F)+  
  theme(axis.title=element_blank())+ 
  geom_hline(yintercept=seq(1,150000, 25000), col="white", lwd=1)


#gutenberg

ggplot(data=plto, aes(x=reorder(nnames,-length), y=length, fill = fb)) +
  geom_bar(width = 0.25, stat="identity")+theme_tufte(base_size=14, ticks=F)+  theme(axis.title=element_blank())  + 
  geom_hline(yintercept=seq(1, max(plto$length), 1000000), col="white", lwd=1)+
  scale_fill_manual(values=c("#6a9fb5","#b5806a"))+
  scale_y_continuous(labels=fancy_scientific)+ guides(fill=FALSE) +
  annotate("text", x = 6, y = 6000000, adj=1,  family="serif",
           label = c("Total number of characters\nbased on Project Gutenberg's libraries"))