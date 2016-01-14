

library(xlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)
library(ggmap)
library(rjson)
library(rlist)
# 
#  install.packages("rlist")
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




dblist<-c("tce01_20160102","tce01_20160103" ,"tce01_20160104", "tce01_20160105", "tce01_20160106", "tce01_20160107",
"tce01_20160108" ,"tce01_20160109" ,"tce01_20160110", "tce01_20160111" ,"tce01_20160112")
total<-length(dblist)
# dblist[1]

#   
#   x.dates<-c("17.03.2001", "16.03.2001", "15.03.2001")
#   x.times<-c("23:39:15", "23:50:00", "22:23:43")
#   x.datetime <- paste(x.dates,x.times)
#   str(strptime(x.datetime, "%d.%m.%Y %H:%M:%S")) # revise to %d.%m.%Y
#   Sys.setlocale("LC_TIME", lct)
#   

# lct <- Sys.getlocale("LC_TIME")
# Sys.setlocale("LC_TIME", "C")
# str(z)

# 
# date<-"2013-11-14"
# date<-as.Date(date)
# # ss<- weekdays(date)
# 
# ?as.ITime
# (as.ITime("10:45"))
# 
# aa<-as.POSIXct(186480,origin="1970-1-1")
# aa

single_veicle_tr_info_finall<-list()

for(dd in 1:total){


sqlQuerytb<-paste("select * from",dblist[dd], sep=" " )



test_data<-dbGetQuery(conn,sqlQuerytb)


test_data<-mutate(test_data,full_time=as.POSIXct(strptime(paste(date,time),"%Y-%m-%d %H:%M:%S")))

date_start_time<-as.POSIXct(test_data$date[1])
date_start_time<-as.numeric(date_start_time)
date<-as.Date(test_data$date[1])
week<-weekdays(date)



pathnum<-nrow(od_patt)

weekdays=c("星期一","星期二","星期三","星期四","星期五")
holidays=c("星期六","星期日")

for(numm in 1:pathnum){
  
  

o_point<-as.character(od_patt$o_station[numm])
d_point<-as.character(od_patt$d_station[numm])

 # tt_epc_trt<-list()
#   o_point<-"etag-04"
#   d_point<-"etag-05"


for(ts in 1:48){ #30mins一個time range


timestep_start<-ts*30*60/3600

od_set<-filter(test_data,as.numeric(test_data$full_time) < date_start_time+30*60*ts 
               & as.numeric(test_data$full_time)>=date_start_time+30*60*(ts-1))


od_set_o<-filter(od_set,od_set$station==o_point)   #時間範圍內過o點的所有epc
if(nrow(od_set_o)==0){
  
  next;
}

else{
epc_list<-data.frame(od_set_o$epc,od_set_o$full_time)
colnames(epc_list)<-c("epc","ps_o_time")


for(i in 1:nrow(epc_list)){
  
  
  o_epc<-as.character(epc_list$epc[i])

  ps_o_time<-as.numeric(epc_list$ps_o_time[i]) #目標epc過o點時間

  od_set_o<-filter(test_data,epc==o_epc & station==d_point 
                   & as.numeric(full_time)>ps_o_time)
  if(nrow(od_set_o)==0){
    
    next;
    
  }
  
  else{
    ps_d_time<-as.numeric(sort(od_set_o$full_time)[1])#目標epc過d點時間
    epc_od_trt<-ps_d_time-ps_o_time
    vc_type<-substr(o_epc,start=6,stop=6)

#     single_epc_trt<-c(o_epc,epc_od_trt)
#     
#     tt_epc_trt<-c(tt_epc_trt,list(single_epc_trt))
     sg_v_data<-list(vc_type=vc_type,o_epc=o_epc,epc_od_trt=epc_od_trt)

     time_info<-list(week=week,date=date,time=timestep_start)
     roadpth_info<-list(o_point=o_point,d_point=d_point)
     single_veicle_tr_info<-list(roadpth_info=roadpth_info,time_info=time_info,sg_v_data=sg_v_data)
     single_veicle_tr_info_finall<-c(single_veicle_tr_info_finall,list(single_veicle_tr_info))
     
  }
  
}
}
}

}
}

single_veicle_tr_info_finall

head(single_veicle_tr_info_finall)

single_veicle_tr_info_finall[[27201]]


ltt<-List(single_veicle_tr_info_finall)
ltt
lttt<-List(ltt,single_veicle_tr_info_finall)
str(ltt)


 a<-c(1,2,3)
 b<-c(1)
 aa<-c(1,2,3,4,5)
is.element(b, aa)
 (is.element(aa, b))

pathnum<-nrow(od_patt)
s_car_total_data<-ltt$filter(sg_v_data$vc_type==3) #小車
l_car_total_data<-ltt$filter(sg_v_data$vc_type==4) #大車

sum_od_data_finall<-c()

for( tsp in 1:48){
  
#   ts=tsp/2
#   
  for(numm in 1:pathnum){
    
#     tsp=2
#     numm=1
    
    s_o_point<-as.character(od_patt$o_station[numm])
    s_d_point<-as.character(od_patt$d_station[numm])

    s_car_sum_trt_w<-s_car_total_data$filter(is.element(time_info$week,weekdays) &
                                           time_info$time== ts &
                                           roadpth_info$o_point==s_o_point &
                                           roadpth_info$d_point==s_d_point
                                           )
    
    l_car_sum_trt_w<-l_car_total_data$filter(is.element(time_info$week,weekdays)&
                                           time_info$time== ts &
                                           roadpth_info$o_point==s_o_point &
                                           roadpth_info$d_point==s_d_point
                                           )
    
    W_s_car_sum_trt_num=length(s_car_sum_trt_w$map(sg_v_data$epc_od_trt)[])
    W_l_car_sum_trt_num=length(l_car_sum_trt_w$map(sg_v_data$epc_od_trt)[])

    if(W_s_car_sum_trt_num!=0){    
      
      datalist<-as.numeric(s_car_sum_trt_w$map(sg_v_data$epc_od_trt)[])
      W_s_car_sum_trt_median=median(datalist)
    }
    
    else{
      
      W_s_car_sum_trt_median=NULL
    }
    
    if(W_l_car_sum_trt_num!=0){    
      
      datalist<-as.numeric(l_car_sum_trt_w$map(sg_v_data$epc_od_trt)[])
      W_l_car_sum_trt_median=median(datalist)    
    }
    
    else{
      
      W_l_car_sum_trt_median=NULL
    }        

    

    

    s_car_sum_trt_h<-s_car_total_data$filter(is.element(time_info$week,holidays) &
                                             time_info$time== ts &
                                             roadpth_info$o_point==s_o_point &
                                             roadpth_info$d_point==s_d_point
                                             )
    
    
    l_car_sum_trt_h<-l_car_total_data$filter(is.element(time_info$week,holidays) &
                                             time_info$time== ts &
                                             roadpth_info$o_point==s_o_point &
                                             roadpth_info$d_point==s_d_point
                                             )

    H_s_car_sum_trt_num=length(s_car_sum_trt_h$map(sg_v_data$epc_od_trt)[])
    H_l_car_sum_trt_num=length(l_car_sum_trt_h$map(sg_v_data$epc_od_trt)[])
    
    if(H_s_car_sum_trt_num!=0){    
      
      datalist<-as.numeric(s_car_sum_trt_h$map(sg_v_data$epc_od_trt)[])
      
      H_s_car_sum_trt_median=median(datalist)    
    }
    
    else{
      
      H_s_car_sum_trt_median=NULL
    }
    
    
    if(H_l_car_sum_trt_num!=0){    
      
      datalist<-as.numeric(l_car_sum_trt_h$map(sg_v_data$epc_od_trt)[])
      H_l_car_sum_trt_median=median(datalist)    
      
    }
    
    else{
      
      H_l_car_sum_trt_median=NULL
    }



    sum_od_data<-c(W_s_car_sum_trt_num,W_l_car_sum_trt_num,W_s_car_sum_trt_median,W_l_car_sum_trt_median,
                   H_s_car_sum_trt_num,H_l_car_sum_trt_num,H_s_car_sum_trt_median,H_l_car_sum_trt_median)            
                
                
    sum_od_data_finall<-cbind(sum_od_data_finall,sum_od_data)            
                
                
  }


}

#   tt_epc_trt
#   ddf<-as.data.frame(tt_epc_trt)
#     
#   f1<-c(1,2)
#   f2<-c(1,3)
#   f3<-list(f1=c(1,4))
#   
#   f_list1<-list(f1=f1,f2=f2)
#   
#   f_list1<-f_list1.app
#   f_list1
#   f_list2<-append(f_list1,f3)
#   
#   f_list2<-c(f_list1,f3)
#   f_list2
#   f_list1<-f_list1
#   
#   str(f_list2)
#   f_list2[[i]][2]
#   f_list2$
#   
#   dff<-as.data.frame(f_list3)
#   str(dff)
#   name1<-f_list2[[1]]
#   str(f_list2)
#   f_list2.name1


#     epc_ip=c()
#   trt_t=c()
#     dff<-list(epc_ip=c(),trt_t=c())
#   str(dff)
#   add_ip=1
#   
#   add_trt=23
#   
#   epc_ip=c(epc_ip,add_ip)
#   trt_t=c(trt_t,add_trt)
#   
#   dff<-list(epc_ip=epc_ip,trt_t=trt_t)
#   
#   dff
# 
#   (dff$trt_t==23)
#   }
#   



kkk$time<-as.POSIXct(kkk$time)
str(kk_m)
dbWriteTable(conn,"test11",kkk)


dbDisconnect(conn)

