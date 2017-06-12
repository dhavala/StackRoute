rm(list=ls(all=TRUE))
library(googleVis)
setwd('G:/MoocSim')

log.filename <- './logs//Raw_ClickStream_MoocSim.csv'
prog_log.filename <- './logs/log.txt'
proc_cstream.filename <- './logs/ClickStream_MoocSim.csv'
mod_cstream.filename <- './logs/Mod_ClickStream_MoocSim.csv'
student.filename <- './logs/StudentStore_MoocSim.csv'
object.filename <- './logs/ObjectStore_MoocSim.csv'

joined_df.filename <- './logs/joined_data_MoocSim.csv'
stu_week_dash.filename<- './logs/student_dash_by_week.csv'

geoloc.filename <- './data/IN_simple.csv'
env_init.filename <- './data/env_init_prob.csv'
env_tran.filename <- './data/env_tran_prob.csv'
env_to_act.filename <- './data/env_to_act_prob.csv'


obj_df <- read.csv(object.filename)
stu_df <- read.csv(student.filename)
dash_df <- read.csv(stu_week_dash.filename)

cs_df <- read.csv(mod_cstream.filename)
com_df <- read.csv(joined_df.filename)
stu_act <- read.csv('./logs/req_act_features.csv')
stu_act$uid <- stu_df$uid
stu_act <- merge(stu_act,stu_df,by="uid")


T <- gvisTable(obj_df, options = list(width = 700, height = 280,page="enable"))
plot(T)

T <- gvisTable(dash_df, options = list(width = 700, height = 280,page="enable"))
plot(T)

T <- gvisTable(stu_df, options = list(width = 700, height = 280,page="enable"))
plot(T)

Line4 <-  gvisLineChart(stu_df, "score_hist", c("score_curr"),
                        options=list(gvis.editor="Edit me!"))
plot(Line4)

stu_df2 <- data.frame(score=stu_df[,"score_curr"])
stu_df2$LatLong <- paste(stu_df$lat,stu_df$long,sep=":")
A <- gvisMap(stu_df2, "LatLong", 
                     options = list(showLine = FALSE, enableScrollWheel = TRUE, 
                                    mapType = "hybrid", useMapTypeControl = TRUE))
plot(A)


Geo=gvisGeoChart(stu_df2,locationvar="LatLong",colorvar="score", options=list(projection="kavrayskiy-vii",enableScrollWheel = TRUE,
                              gvis.editor="Edit me!"))
plot(Geo)
T <- gvisTable(Exports, options = list(width = 200, height = 280))
plot(T)
M <- gvisMotionChart(Fruits, 'Fruit', 'Year',
                   options=list(width=400, height=350))
plot(M)
mot_stu <- data.frame(skid=1:nrow(com_df),tm=rep(1,nrow(com_df)),tsp=com_df$time_spent,
                                            x=com_df$score_curr,y=com_df$score_hist)
mot_stu <- mot_stu[1:5,]
M <- gvisMotionChart(mot_stu,idvar="skid",timevar="tm",xvar="time_spent",yvar="score_hist",
                     options=list(width=400, height=350))


