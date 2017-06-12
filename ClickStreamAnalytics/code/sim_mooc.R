rm(list=ls(all=TRUE))
setwd('G:/MoocSim/')

log.filename <- './logs/Raw_ClickStream_MoocSim.csv'
proc_cstream.filename <- './logs/ClickStream_MoocSim.csv'
mod_cstream.filename <- './logs/Mod_ClickStream_MoocSim.csv'

student.filename <- './logs/StudentStore_MoocSim.csv'
object.filename <- './logs/ObjectStore_MoocSim.csv'
geoloc.filename <- './data/IN_simple.csv'
env_init.filename <- './data/env_init_prob.csv'
env_tran.filename <- './data/env_tran_prob.csv'
env_to_act.filename <- './data/env_to_act_prob.csv'


#if (file.exists(log.filename)) file.remove(log.filename)

seed = 123
set.seed(seed)


env_n = 8
act_n = 20
obj_n = 50
N1 = 1000
N2 = 500
N = N1+N2

env_list = paste('e',formatC(1:env_n,width=nchar(env_n),flag='0'),sep='')
env_prob = rep(1,env_n)

act_list = paste('a',formatC(1:act_n,width=nchar(act_n),flag='0'),sep='')
act_prob = rep(1,act_n)

obj_list = paste('obj',formatC(1:obj_n,width=nchar(obj_n),flag='0'),sep='')
obj_prob = rep(1,obj_n)

bloom_n = 4
bloom_list <- paste('bloom',formatC(1:bloom_n,width=nchar(bloom_n),flag='0'),sep='')
bloom_prob <- rep(1,bloom_n)

diff_n = 5
diff_list <- paste('diff',formatC(1:diff_n,width=nchar(diff_n),flag='0'),sep='')
diff_prob <- seq(diff_n,1)

par_student_grade <- list(a=80,b=10)

init_env_prob <- data.frame(env=env_list,prob=matrix(runif(env_n),ncol=1));
init_env_prob$prob=init_env_prob$prob/sum(init_env_prob$prob)
write.csv(init_env_prob,env_init.filename,row.names=F)
init_env_prob <- matrix(init_env_prob$prob,ncol=1)

tran_env_prob <- matrix(runif(env_n*env_n),ncol=env_n);
tran_env_prob=sweep(tran_env_prob,1,rowSums(tran_env_prob),"/")
colnames(tran_env_prob) <- env_list
write.csv(tran_env_prob,env_tran.filename,row.names=F)

tran_env_to_act_prob <- matrix(runif(env_n*act_n),ncol=act_n);
tran_env_to_act_prob=sweep(tran_env_to_act_prob,1,rowSums(tran_env_to_act_prob),"/")
colnames(tran_env_to_act_prob) <- act_list
rownames(tran_env_to_act_prob) <- env_list
write.csv(tran_env_to_act_prob,env_to_act.filename,row.names=F)


geo_loc <- read.csv(geoloc.filename)
gender_prob <- c(0.8,0.2)
gender_list <- c("M","F")

# param list for generating time till next login (between session time or lull period) 
par_dur_lull = list(p=0.8,a=1,b=0.01,c=200)
# param list for generating time spent on an activity
par_dur_act = list(p=0.8,a=2,b=0.1,c=20)

# param list for generating number of session per user
par_num_sessions = list(p=0.8,a=1,b=NA,c=10)
# param list for generating number of actions in a given session
par_num_act = list(p=0.8,a=10,b=NA,c=100)


# simulate click_stream
source('./MoocSimCore.R')

sim_mooc(c(1:N1))

# turn 80% of cells in env_env off (very sparse)
tran_env_prob[sample.int(round(env_n*env_n*0.50))]=0
write.csv(tran_env_prob,paste(env_tran.filename,"_mod.csv",sep=''),row.names=T)

sim_mooc(c((N1+1):N))


# below simulate student meta-data
simulate_student_store(c(1:N1),header=T)
par_student_grade <- list(a=20,b=20)
simulate_student_store(c((N1+1):N),header=F)

# object store
obj_list <- c("QA","Notes","Discovery","LOB","Assessment")
obj_prob <- c(1,1,1,1,1)
obj_id = paste('obj',formatC(1:obj_n,width=nchar(obj_n),flag='0'),sep='')
object_store <- data.frame(obj=obj_id,type=sample(obj_list,replace=T,prob=obj_prob),
                           diff=sample(1:4,obj_n,replace=T),week=sample(1:6,obj_n,replace=T))

write.table(object_store,file=object.filename,row.names=F,sep=',',col.names=T)


# format headers
txt <- readLines(log.filename,n=1,warn=F)
substr <- unlist(strsplit(txt,','))
head <- sapply(substr,function(tmp) unlist(strsplit(tmp,':'))[1],USE.NAMES=F)
head_n <- length(head)

if (file.exists(proc_cstream.filename)) file.remove(proc_cstream.filename)
fpw <- file(proc_cstream.filename,'a')
writeLines(paste(head,collapse=','),fpw)


# parse all the lines
fp <- file(log.filename,'rt')

while ( length(txt <- readLines(fp,n=1,warn=F))>0 )
{
  #txt <- readLines(fp,n=1,warn=F)
  substr <- unlist(strsplit(txt,','))
  vals <-  sapply(substr,function(tmp) unlist(strsplit(tmp,':'))[2],USE.NAMES=F)
  #cat(txt);cat('\n\t')
  cat(vals);cat('\n')
  if(length(vals)!=head_n)  vals <- rep(NA,head_n)
  
  # for some reason writeLines is truncating decimal points (fixed width possibly)
  writeLines(paste(vals,collapse=','),fpw)
}
close(fp)
close(fpw)



# append time_spent along with time info
library(dplyr)
cs_df <- read.csv(proc_cstream.filename)

# get the clickstream and calculate times spent on each action
# # it is already in sorted order
# cs_sort <- cs_store[order(cs_store$uid,cs_store$session.id,cs_store$time),]
cs_sort = cs_df
rel_time <- c(diff(cs_sort$time),NA)
# generate time_spent in an activity
cs_sort$time_spent <- rel_time
cs_sort <- filter(cs_sort,env!="logout")
cs_sort <- droplevels(cs_sort)
write.csv(cs_sort,mod_cstream.filename,row.names=F)







