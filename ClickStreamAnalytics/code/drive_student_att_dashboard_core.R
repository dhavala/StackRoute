library(dplyr)
library(reshape2)

get_tsp <- function(subdf,tsp_loc)
{
  tf <- aggregate(time~env,sum,data=subdf)
  tsp_loc[tf$env,1] <- tf$time
  return(tsp_loc)
}

get_adj <- function(student_env,A)
{
  
  #print(student_env);cat("length");print(length(student_env))
  A_local <- A
  edge_list_n1 = length(student_env)
  if(edge_list_n1>2)
  {
    # need at least two env to calculate 
    edge_list = cbind(student_env[-edge_list_n1],student_env[-1])
    edge_list_n2 = edge_list_n1-1
    for (jj in 1:edge_list_n2)
    {
      A_local[edge_list[jj,1],edge_list[jj,2]] <- A_local[edge_list[jj,1],edge_list[jj,2]]+1
    }
  }
  return(A_local)
}

get_eng<- function(A_local,tsp_local)
{
  # time should be in hrs per week
  w=7
  x <- tsp_local*7/w
  y <- pexp(x,rate=0.4)
  A_local <- sweep(A_local,1,rowSums(A_local),'/')
  eng_env <- A_local%*%y
  eng_env[is.na(eng_env)]<-0
  eng <- c(eng_env,mean(eng_env))  
  return(eng)
}


get_dash_summary <- function(input_filenames,day)
{
  
  myCols <- rep("NULL",10); myCols[c(1:6)] <- rep("character",6);
  myCols[2]<-"numeric"
  
  #input_filenames <- file.list_local
  #day = time.rep
  
  df <- lapply(input_filenames,read.table,header = F,sep=",",colClasses = myCols)
  df <- rbind_all(df)
  
  #df <- read.table(input_filename,header=F,sep=",",colClasses = myCols)
  names(df) <- c("sid","time","name","course","role","env")
  df <- filter(df,role=="student")
  
  # if no students presnet, then exit
  
  df <- df[,c(1:4,6)]
  
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

  students <- unique(df$name)
  n.students <- length(students)
  courses <- unique(df$course)
  n.courses <- length(courses)
  env <- unique(df$env)
  n.env <- length(env)


  tsp <- matrix(0,n.env)
  rownames(tsp)<-env
  A <- matrix(0,n.env,n.env)
  rownames(A)<-env
  colnames(A)<-env

  
  k <- n.courses*n.students*(n.env*2+2)
  eng.df <- data.frame(day=rep(day,k),name=rep("student",k),
                       course=rep("course",k),env=rep("env",k),
                       score=rep(NA,k))
  
  eng.df$name <- as.character(eng.df$name)
  eng.df$course <- as.character(eng.df$course)
  eng.df$env <- as.character(eng.df$env)
  
  env.vec <- c(paste("att.",env,sep=""),paste("eng.",env,sep=""),
               "ilimi","last.login")
  
  
  local_start = 1
  for (ii in seq(1,n.students))
  {
    
    subdff <- filter(df,name==students[ii])
    if(nrow(subdff)<1) next;
    
    # determine the last login
    subdff <- subdff[order(subdff$sid,subdff$time),]
    subdff$last_time <- subdff$time
    subdff$time <- c(NA,diff(subdff$time))/3600/1000
    ind <- duplicated(subdff$sid)
    # we no longer need session id and remove the session start rows
    subdff <- subdff[ind,-1]
    
    
    if(any(subdff$time<0)) {
      stop('time diff is negative');
    }
    # if not enough data, skip to next student
    if(nrow(subdff)<1) next;
    for(jj in seq(1,n.courses)) 
    {
      subdf <- filter(subdff,course==courses[jj])
      if(nrow(subdf) <1) next;
      # last login
      last.login <- max(subdf$last_time)
      subdf <- subdf[,c(1,4)]
      tsp_local <- get_tsp(subdf,tsp)
      #A_local <- createSequenceMatrix(subdf$env)
      A_local <- get_adj(subdf$env,A)
      eng_ind <- get_eng(A_local,tsp_local)
  
      
      local_end = local_start+(2*n.env+2-1)
      local_ind = local_start:local_end
      eng.df[local_ind,"name"] <- students[ii]
      eng.df[local_ind,"course"] <- courses[jj]
      eng.df[local_ind,"env"] <- env.vec
      eng.df[local_ind,"score"] <- c(tsp_local[1:n.env],eng_ind[1:(n.env+1)],last.login)
      local_start = local_end+1
    }
  }
  # there should be at least one record in the clickstreams
  stopifnot(local_end>1)
  eng.df <- eng.df[1:local_end,]
  # return eng.df after removing any rows with NA
  # eng.df <- na.omit(eng.df)
  return(eng.df)
  #write.table(eng.df,output_filename,row.names=F,col.names=T,sep=",")
}
