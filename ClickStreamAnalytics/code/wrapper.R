rm(list=ls(all=TRUE))
library(dplyr)
library(reshape2)
args <- commandArgs(trailingOnly = TRUE)

input_dir <- args[1]
output_filename <- args[2]
code_dir <- args[3]
upto_time = as.numeric(args[4])
window_time = as.integer(args[5])
resolution_time = as.integer(args[6])

# input_dir <- "/Users/soma/Dropbox (Canopus Consulting)/pidugu/aws/analytics/dashboards/data/input"
# output_filename <- "/Users/soma/Dropbox (Canopus Consulting)/pidugu/aws/analytics/dashboards/data/output/output.csv"
# code_dir <- "/Users/soma/Dropbox (Canopus Consulting)/pidugu/aws/analytics/dashboards/code"
# upto_time = as.integer(Sys.time())*1e3
# window_time = 30
# resolution_time = 7

#proj_dir <- "/Users/soma/Dropbox (Canopus Consulting)/pidugu/aws/analytics/dashboards/data/archive/temp_test"
days2ms <- 24*3600*1e3



tryCatch(
  {
    stopifnot(window_time>0)
    stopifnot(resolution_time>0)
    resolution_time = min(resolution_time,window_time)
    steps_time = ceiling(window_time/resolution_time)
    window_time = steps_time*resolution_time
    
    file.list <- list.files(input_dir,full.names=T)
    n <- length(file.list)
    
    # there should be at least one file
    if(n<1) stop("There are no clickstreams in the specified directory")
    
    # remove all those files which fall outside the window of interest
    file.info <- file.info(file.list)
    
    
    
    #upto_time_posix = as.POSIXlt(upto_time/1e3,origin="1970-01-01")
    #from_time_posix = upto_time_posix - (window_time*3600*24)
    
    from_time = upto_time - (window_time*days2ms)
    file.info[,"mtime"] <- as.integer(file.info[,"mtime"])*1e3
    


    file.selected <- which( (file.info[,"mtime"]<= upto_time) & 
                             (file.info[,"mtime"] > from_time) &
                              (file.info[,"size"] > 1))
    n <- length(file.selected)
  
    # there should be at least one file. if not exit
    if(n<1) stop("There are no clickstreams that are in the specified window")
    
    file.info <- file.info[file.selected,]
    file.list <- file.list[file.selected]
    
    
    
    # cluster the files according to the resolution
    
    breaks <- seq(upto_time,from_time,-(resolution_time*days2ms))
    vals <- file.info[,"mtime"]
    bins <- cut(vals,breaks,labels=F)
    unique_bins <- unique(bins)
    k <- length(unique_bins)
    
    source(paste(code_dir,"/drive_student_att_dashboard_core.R",sep=""))
    
    header_flag = T
    
    for (k in unique_bins)
    {
      
      ind <- (bins==k)
      
      
      file.list_local <- file.list[ind]
      # call the r routine which returns
      time.rep <- round(median(file.info[ind,"mtime"]))
      dash.df_local <- get_dash_summary(file.list_local,time.rep)
      if(header_flag)
      {
        # first time, write the header
        write.table(dash.df_local,output_filename,sep=",",col.names=T,row.names=F)
        header_flag=F
      } else {
        # append to existing file
        write.table(dash.df_local,output_filename,sep=",",col.names=F,row.names=F,append=T)
      }
      
    }
  
  }, error=function(e) {
    #write.table(e,output_filename,row.names=F,col.names=F,sep=",") 
    print(e)
    quit(save = "no", status = 1, runLast = F)
  }
)
