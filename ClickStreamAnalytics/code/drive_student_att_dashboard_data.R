rm(list=ls(all=TRUE))
library(dplyr)
library(reshape2)
args <- commandArgs(trailingOnly = TRUE)


input_filename <- args[1]
output_filename <- args[2]
w <- as.numeric(args[3])
#input_filename <- "./../data/input/input.csv"
#output_filename <- "./../data/input/output.csv"
  
#setwd('G:/MoocSim')
# select only studens, remove the entry point
# time_stamp is recorded at the begining of an activity


#df <- read.csv(input_filename,header=F)
#names(df) <- c("sid","time","name","course","role","env","act","uri","ip","ext.flag")
#df <- filter(df,role=="student")
#df <- df[,c(1:4,6)]

tryCatch({
  
  myCols <- rep("NULL",10); myCols[c(1:6)] <- NA;
  df <- read.table(input_filename,header=F,sep=",",colClasses = myCols)
  names(df) <- c("sid","time","name","course","role","env")
  df <- filter(df,role=="student")
  df <- df[,c(1:4,6)]
  
  # determine the last login
  df$last_time <-df$time
  
  df$time <- c(NA,diff(df$time))/3600/1000
  ind <- duplicated(df$sid)
  # we no longer need session id
  df <- df[ind,-1]


  # # much quicker way to compute tsp
  # # but need to loop thru to calculate engagement index anyway 
  # # tsp contains the time spent in minutes (in a given session)
  # # using the aggregae function, we can get time spent by
  # # a student in a course per environemtn
  # 
  # tdf <- aggregate(tsp~name+course+env,sum,data=df)
  # tdf$tsp <- tdf$tsp/1000/60
  # 
  # tdf <- aggregate(tsp~env+course+name,sum,data=df)
  # tdf$tsp <- tdf$tsp/1000/60
  # tdf <- tdf[,c("name","course","env","tsp")]

  # now for each student, in each course, calculate the
  # engagement index

  students <- levels(df$name)
  n.students <- length(students)
  courses <- levels(df$course)
  n.courses <- length(courses)
  env <- levels(df$env)
  n.env <- length(env)


  tsp <- matrix(0,n.env)
  rownames(tsp)<-env
  A <- matrix(0,n.env,n.env)
  rownames(A)<-env
  colnames(A)<-env

  get_tsp <- function(subdf,tsp_loc)
  {
    tf <- aggregate(time~env,sum,data=subdf)
    tsp_loc[tf$env,1] <- tf$time
    return(tsp_loc)
  }

  get_adj <- function(student_env,A)
  {
    edge_list_n1 = length(student_env)
    edge_list = cbind(student_env[-edge_list_n1],student_env[-1])
    edge_list_n2 = edge_list_n1-1
    A_local <- A
  
    for (jj in 1:edge_list_n2)
    {
      A_local[edge_list[jj,1],edge_list[jj,2]] <- A_local[edge_list[jj,1],edge_list[jj,2]]+1
    }
    return(A_local)
  }

  get_eng<- function(A_local,tsp_local)
  {
    # time should be in hrs per week
    x <- tsp_local*7/w
    y <- pexp(x,rate=0.4)
    A_local <- sweep(A_local,1,rowSums(A_local),'/')
    eng_env <- A_local%*%y
    eng_env[is.na(eng_env)]<-0
    eng <- c(eng_env,mean(eng_env))  
    return(eng)
  }

  k1 <- n.courses*n.students
  k2 <- 2+(n.env*2)+1+1
  eng.df <- data.frame(rep("student",k1),rep("course",k1),matrix(0,k1,k2-2))

  names(eng.df)<-c("name","course",paste("eng.",env,sep=""),paste("att.",env,sep=""),
                   "eng.ilimi","last.login")
  eng.df$name <- as.character(eng.df$name)
  eng.df$course <- as.character(eng.df$course)
  
  
  count=1
  for (ii in seq(1,n.students))
  {
    subdff <- filter(df,name==students[ii])
  
    for(jj in seq(1,n.courses)) 
    {
      subdf <- filter(subdff,course==courses[jj])
      eng.df[count,k2] <- max(subdf$last_time)
      subdf <- subdf[,c(1,4)]
      tsp_local <- get_tsp(subdf,tsp)
      #A_local <- createSequenceMatrix(subdf$env)
      A_local <- get_adj(subdf$env,A)
      eng_ind <- get_eng(A_local,tsp_local)
  
      eng.df[count,1]<-students[ii]
      eng.df[count,2]<-courses[jj]
      eng.df[count,3:(2+n.env)]<-tsp_local[1:n.env]
      eng.df[count,(2+n.env+1):(k2-2)]<-eng_ind[1:n.env]
      eng.df[count,k2-1] <- eng_ind[n.env+1]
      
      count = count+1
      print(count)
    }
  }
  write.table(eng.df,output_filename,row.names=F,col.names=T,sep=",")
  
  }, error=function(e) {
    #write.table(e,output_filename,row.names=F,col.names=F,sep=",") 
    print(e)
    quit(save = "no", status = 1, runLast = F)
  }
)


