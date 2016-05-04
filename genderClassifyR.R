#Gender encoding
library(gender)
library(stringi)

#get all unique names
x<-unique(df$sender)

#split names into first name and the rest and pick first name
names<-data.frame(do.call('rbind', strsplit(as.character(x),' ',fixed=TRUE)))
gender<-data.frame(fullname = x, firstname = as.character(names[,1]), stringsAsFactors = F)

#get rid of accents
#gender$firstname<-iconv(gender$firstname, to='ASCII//TRANSLIT')#does not work
gender$firstname<-stri_trans_general(gender$firstname,"Latin-ASCII")

#genderize and check performance
x<-gender(unique(gender$firstname),method = "ssa",year=2012)
nrow(x)/length(unique(gender$firstname))#61%

gender<-merge(gender,x,by.x="firstname",by.y="name",all.x=T)
g<-gender[,c("fullname","gender")]

df$fullname<-df$sender

df<-merge(df,g, by="fullname",all.x=T,all.y=F)

#check NA's
y<-df %>% select(sender,gender) %>% filter(is.na(gender)) %>% unique() %>% select(sender)

#loss of 69k messages

#maybe only for english language

#explore
df %>% filter(sender != "Boaz Sobrado" & language == "english") %>%
  select(msgLen,gender) %>% group_by(gender) %>%
  summarise(meanLen = mean(msgLen),
            total_S = n())
