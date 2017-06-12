rm(list=ls(all=TRUE))

df <- read.csv('./../data/input/ilimi-original.log',header=F)
n <- nrow(df)
df <- cbind(df,rep("course-01",n))
df <- df[,c(1:3,10,4:9)]
# generate 10 file for simulation
p=10
n <- nrow(df)
origin <- paste("1971-01-",formatC(c(1:p),width=2,flag="0"),sep="")
fnames <- paste("./../data/input/sorted_stream_2014-11-",formatC(c(1:p),width=2,flag="0"),sep="")

for(ii in seq(1,p))
{
  df_sample <- df[sample(n),]
  epo <- as.numeric(as.POSIXct(df_sample[,2]/1000, origin=origin[ii]))*1000
  df_sample[,2] <- epo
  df_sample[,4] <- paste("Course-",formatC(ii,width=2,flag="0"),sep="")
  write.table(df_sample,paste(fnames[ii],".log",sep=""),row.names=F,col.names=F,sep=",")
}

