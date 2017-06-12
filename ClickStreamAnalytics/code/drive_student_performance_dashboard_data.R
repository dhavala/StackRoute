rm(list=ls(all=TRUE))
library(ggplot2)
library(dplyr)
library(data.table)

#setwd('G:/MoocSim')

log.filename <- './logs//Raw_ClickStream_MoocSim.csv'
prog_log.filename <- './logs/log.txt'

proc_cstream.filename <- './logs/ClickStream_MoocSim.csv'
mod_cstream.filename <- './logs/Mod_ClickStream_MoocSim.csv'
student.filename <- './logs/StudentStore_MoocSim.csv'
object.filename <- './logs/ObjectStore_MoocSim.csv'

joined_df.filename <- './logs/joined_data_MoocSim.csv'

geoloc.filename <- './data/IN_simple.csv'
env_init.filename <- './data/env_init_prob.csv'
env_tran.filename <- './data/env_tran_prob.csv'
env_to_act.filename <- './data/env_to_act_prob.csv'


obj_df <- read.csv(object.filename)
stu_df <- read.csv(student.filename)
cs_df <- read.csv(mod_cstream.filename)

# merge the click stream
com_df <- merge(obj_df,cs_df,by="obj",all=F)
com_df <- merge(stu_df,com_df,by="uid",all=F)
write.csv(com_df,joined_df.filename,row.names=F)

# we have joined data.frame with all the meta-data
# produce dashboards

# total time spent on each object

sink('./logs/checkLog.txt')
cat("\n*******\n")
cat("mean TSP and total TSP on each OBJ_TYPE across all students and env\n")
cat(" *******\n")
summarise(group_by(com_df,type),mean(time_spent),sum(time_spent))
cat("\n")

cat("\n*******\n")
cat("mean TSP and total TSP  by OBJ_TYPE and ENV\n")
cat(" *******\n")
summarise(group_by(com_df,type,env),mean(time_spent),sum(time_spent))

cat("\n*******\n")
cat("mean TSP and total TSP  by UID\n")
cat(" *******\n")
summarise(group_by(com_df,uid),mean(time_spent),sum(time_spent))

cat("\n*******\n")
cat("mean TSP and total TSP  by UID on TYPE\n")
cat(" *******\n")
ag3 <- summarise(group_by(com_df,uid,type),mean(time_spent),sum(time_spent),length(unique(obj)))
names(ag3) <- c('student','avg TSP','total TSP','# of unique objects')
write.csv(data.frame(ag3),"./logs/TSPbyUid_Type.csv",row.names=F)

cat("\n*******\n")
cat("Number of unique users on any object \n")
cat(" *******\n")
summarise(group_by(com_df,obj),length(unique(uid)))
sink()

att_by_students <- data.frame(summarise(group_by(com_df,uid,type),sum(time_spent),length(unique(obj))))
names(att_by_students) <- c('student','env','total time spent (mins)','# of objects')
write.csv(att_by_students,'./logs/student_att_byWeek.csv',row.names=F)

att_by_obj <- data.frame(summarise(group_by(com_df,type,obj),mean(time_spent)))
names(att_by_obj) <- c('type','obj','time_spent')
att2_by_obj <- data.frame(summarise(group_by(att_by_obj,type),mean(time_spent),length(unique(obj))))
names(att2_by_obj) <- c('env','class: avg time spent in ENV','class: # of objects in that ENV')
write.csv(att2_by_obj,'./logs/obj_att_byWeek.csv',row.names=F)
att_by_week <- merge(att_by_students,att2_by_obj,by="env",all=F)
att_by_week <- att_by_week[order(att_by_week$student),c(2,1,3:6)]
write.csv(att_by_week,'./logs/att_by_week.csv',row.names=F)


perf_by_week <- data.frame(student=stu_df$uid,score=stu_df$score_curr,
                               class.avg=mean(stu_df$score_curr),rank=rank(-stu_df$score_curr))
                               

a.names <- paste('ATT:',names(att_by_week[-1]))
b.names <- paste('PERF:',names(perf_by_week[-1]))


comb_stu_dash <- merge(att_by_week,perf_by_week,by="student",all=F)
comb_stu_dash_write <- comb_stu_dash
names(comb_stu_dash_write) <- c('student',a.names,b.names)
write.csv(comb_stu_dash_write,"./logs/student_dash_by_week.csv",row.names=F)





