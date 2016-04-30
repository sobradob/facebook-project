#read and fix JSON file
library(rjson)

msgR<-fromJSON(file = "C:\\Users\\user1\\Desktop\\Life\\RProjects\\facebookMsg\\FB-Message-Parser\\messages.json",)
msg<-msgR[[1]]
rm(msgR)

threads<-sapply(msg,function(x) paste(x[[1]], sep ="", collapse = "-"))



thread<-vector()
sender<-vector()
date<-vector()
message<-vector()

for (i in 1:length(msg)){
  thread.length<-length(msg[[i]][[2]])
  for (a in 1:thread.length){
          thread[length(thread)+1]  <- threads[i]
         sender[length(sender)+1]   <- msg[[i]][[2]][[a]][[3]]
         date[length(date)+1]       <- msg[[i]][[2]][[a]][[2]]
         message[length(message)+1] <- msg[[i]][[2]][[a]][[1]]
  }
cat(paste0("thread ",i," involving ", threads[i]," has been completed ,
           ...its length was ", thread.length, "\n"))
}

df<-data.frame(thread,sender,date,message)
rm(message,msg,sender,thread,thread.length,threads, date)

df$date<-as.POSIXct(strptime(df$date,format = "%A, %B %e, %Y at %I:%M%p"))

df$thread<-as.character(df$thread)
df$sender<-as.character(df$sender)
df$message<-as.character(df$message)

df$message <- stri_encode(df$message, "", "UTF-8")

df$message<-stri_trans_general(df$message, "Latin-ASCII")
