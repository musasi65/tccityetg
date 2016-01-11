

library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggmap)
library(rjson)


install.packages("RJDBC")
library(RJDBC)

# ??RJDBC
#RJDBCcon <-odbcConnect("test2",uid="root",pwd="ceciits")
# ?dbListTables
# channel <- odbcDriverConnect("test1")

dev<-JDBC("com.mysql.jdbc.Driver",
          "C:\\jar\\mysql-connector-java-5.1.38\\mysql-connector-java-5.1.38-bin.jar"
)

conn<-dbConnect(dev,
                "jdbc:mysql://localhost:3306/etag",
                "root",
                "ceciits"
)
dbListTables(conn)

#   
#   x.dates<-c("17.03.2001", "16.03.2001", "15.03.2001")
#   x.times<-c("23:39:15", "23:50:00", "22:23:43")
#   x.datetime <- paste(x.dates,x.times)
#   str(strptime(x.datetime, "%d.%m.%Y %H:%M:%S")) # revise to %d.%m.%Y
#   Sys.setlocale("LC_TIME", lct)
#   

lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
str(z)

test_data<-dbGetQuery(conn,"select * from tce01_20151201")
test_data<-mutate(test_data,full_time=as.POSIXct(strptime(paste(date,time),"%Y-%m-%d %H:%M:%S")))
#str(test_data)

o_point<-"fetag-02"
d_point<-"fetag-05"

for()




kk_m<-mutate(kkk,full_time=as.POSIXct(strptime(paste(date,time),"%Y-%m-%d %H:%M:%S")))
str(kk_m)



kkk$time<-as.POSIXct(kkk$time)
str(kk_m)
dbWriteTable(conn,"test11",kkk)



dbDisconnect(conn)

