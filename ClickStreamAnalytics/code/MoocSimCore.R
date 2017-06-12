# param list for generating registration time (very first login)
par_dur = list(p=0.8,a=1,b=0.01,c=10800)
generate_duration <- function(n,par=par_dur)
{
  rg <- rgamma(n,par$a,par$b)
  ru <- par$c*runif(n)
  rb <- runif(n) < par$p
  start_time <- rg*rb + ru*(1-rb)
  start_time <- round(start_time*100)/100
  return(start_time)
}

# default param list for generating number of events
par_num = list(p=0.8,a=10,b=NA,c=100)
generate_num_events <- function(n,par=par_num)
{
  rp <- rpois(n,par$a)
  ru <- sample(par$c,n,replace=TRUE)
  rb <- runif(n) < par$p
  n_sessions <- round(rp*rb + ru*(1-rb))
  n_sessions[n_sessions>par$c] <- par$c
  n_sessions[n_sessions<1] <- 1
  return(n_sessions)
}

sim_mooc <- function(uid)
{
  
  #if (file.exists(log.filename)) file.remove(log.filename)
  fp = file(log.filename,'a')
  #sink(log.filename)
  for (ii in uid)
  {
    # generate M no. of session per user
    M = generate_num_events(1,par_num_sessions)
    count = 0
    start_time <-  0
    for (jj in seq(1,M))
    {
      count = count+1
      session_id <- paste(ii,count,sep='.')
      start_time <-  start_time + generate_duration(1,par_dur_lull)
      
      txt <- ''
      txt <- paste(txt,'session-id',sep='')
      txt <- paste(txt,session_id,sep=':')
      
      txt <- paste(txt,'uid',sep=',')
      txt <- paste(txt,ii,sep=':')
      
      # generate no. of actions in an env
      K = generate_num_events(1,par_num_sessions)
      
      # generate landing env
      env <- sample.int(env_n,1,prob=init_env_prob)
      
      #cat('\t');cat(c(M,K));cat('\n')
      for(kk in seq(1,K))
      {
        act <- sample(act_list,1,prob=tran_env_to_act_prob[env,])
        obj <- sample(obj_list,1)
        txt_in <- paste(txt,'env',sep=',')
        txt_in <- paste(txt_in,env_list[env],sep=':')
        txt_in <- paste(txt_in,'act',sep=',')
        txt_in <- paste(txt_in,act,sep=':')
        txt_in <- paste(txt_in,'obj',sep=',')
        txt_in <- paste(txt_in,obj,sep=':')
        
        txt_in <- paste(txt_in,'time',sep=',')
        txt_in <- paste(txt_in,start_time,sep=':')
        
        start_time <- start_time + generate_duration(1,par_dur_act)
        #cat(txt_in);cat('\n')
        writeLines(txt_in,con=fp)
        env <- sample.int(env_n,1,prob=tran_env_prob[env,])
      }
      # end of session
      # logout
      txt_in <- paste(txt,'env',sep=',')
      txt_in <- paste(txt_in,"logout",sep=':')
      
      txt_in <- paste(txt_in,'act',sep=',')
      txt_in <- paste(txt_in,'logout',sep=':')
      
      txt_in <- paste(txt_in,'obj',sep=',')
      txt_in <- paste(txt_in,'logout',sep=':')
      
      txt_in <- paste(txt_in,'time',sep=',')
      txt_in <- paste(txt_in,start_time,sep=':')
      
      writeLines(txt_in,con=fp)
      
      
    }
    
  }
  #sink()
  close(fp)
}


simulate_student_store <- function(uid,header=T,par_student=par_student_grade)
{
  N=length(uid)
  score_hist <- abs(rnorm(N,mean=par_student$a,sd=par_student$b))
  score_hist[score_hist>100] = 100
  gender <- factor(sample(gender_list,N,prob=gender_prob,replace=T))
  loc_ind <- sample(nrow(geo_loc),N,replace=T)
  city <- factor(geo_loc[loc_ind,1])
  lat <- geo_loc[loc_ind,2]
  long <- geo_loc[loc_ind,3]
  score_curr <- rnorm(score_hist)
  
  student_store <- data.frame(uid=uid,score_hist=score_hist,
                              gender=gender,city=city,
                              lat=lat,long=long,score_curr=score_curr)
  
  
  student_store_matrix <- data.matrix(student_store)
  student_store_matrix <- scale(student_store_matrix[,c(2,3,5,6)])
  beta <- matrix(c(1,0.5,0.5,-0.5))
  score_curr <- (student_store_matrix%*%beta)+rnorm(N,mean(score_hist),1)
  #plot(score_hist,score_curr)
  #summary.aov(lm(score_curr~score_hist+gender+long+lat))
  student_store$score_curr <- score_curr
  if(header)
  {
    write.table(student_store,file=student.filename,row.names=F,sep=',',col.names=header)
  } else {
    write.table(student_store,file=student.filename,row.names=F,append=TRUE,sep=',',col.names=header)
  }
}



