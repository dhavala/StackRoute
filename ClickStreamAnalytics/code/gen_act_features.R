rm(list=ls(all=TRUE))
setwd('G:/MoocSim')
library(igraph)
library(sna)
library(dplyr)


log.filename <- './logs//Raw_ClickStream_MoocSim.csv'
proc_cstream.filename <- './logs/ClickStream_MoocSim.csv'
mod_cstream.filename <- './logs/Mod_ClickStream_MoocSim.csv'
student.filename <- './logs/StudentStore_MoocSim.csv'
object.filename <- './logs/ObjectStore_MoocSim.csv'

req_act.filename <- './logs/req_act_features.csv'
opt_act.filename <- './logs/opt_act_features.csv'
graph_act.filename <- './logs/graph_act_features.csv'


geoloc.filename <- './data/IN_simple.csv'
env_init.filename <- './data/env_init_prob.csv'
env_tran.filename <- './data/env_tran_prob.csv'
env_to_act.filename <- './data/env_to_act_prob.csv'



seed = 123
set.seed(seed)


cs_store <- read.csv(mod_cstream.filename)

# # # it is already in sorted order
# # cs_sort <- cs_store[order(cs_store$uid,cs_store$session.id,cs_store$time),]
# cs_sort = cs_store
# rel_time <- c(NA,diff(cs_sort$time))
# rel_time[!duplicated(cs_sort$uid)] <- NA 
# # generate time_spent in an activity
# cs_sort$time_spent <- rel_time
# plot(density(cs_sort[,3]))

env_list <- levels(cs_store$env)
env_n <- length(env_list)

uid <- unique(cs_store$uid)
uid_n <- length(uid)


indeg.names <- paste("indeg",env_list,sep=".")
outdeg.names <- paste("outdeg",env_list,sep=".")
betw.names <- paste("betw",env_list,sep=".")
#strct.names <- paste("strct",env_list,sep=".")
close.names <- paste("close",env_list,sep=".")
conn.names <- paste("is.conn",env_list,sep=".")
clust.names <- paste("clust",env_list,sep=".")
pagerank.names <- paste("pagerank",env_list,sep=".")
cen.names.req <- paste("cen",c("flow","deg"),sep=".")
cen.names.opt <- paste("cen",c("betw","eig","load"),sep=".")


g_req <- matrix(NA,uid_n,((6*env_n)+2+9))
colnames(g_req) <- c(indeg.names,outdeg.names,cen.names.req,close.names,conn.names,clust.names,pagerank.names,"eff","hier",
                 "conn","lub","den","mut","nties","is.conn","clust")
                 
g_opt <- matrix(NA,uid_n,((1*env_n)+3+1))
colnames(g_opt) <- c(betw.names,cen.names.opt,"ncut")

g_act_graph <- matrix(NA,uid_n,env_n*env_n)




for (ii in seq(1,uid_n))
{
  tryCatch({
  
  cat(ii);cat('\n')
  
  #A <- get_AdjMatrix(uid[ii])
  A <- matrix(0,env_n,env_n)
  ind <- which(cs_store$uid==uid[ii])
  if(length(ind)>1)
  {
    student_env <- cs_store[ind,"env"]
    edge_list_n1 = length(student_env)
    edge_list = cbind(student_env[-edge_list_n1],student_env[-1])
    edge_list_n2 = edge_list_n1-1
    
    for (jj in 1:edge_list_n2)
    {
      A[edge_list[jj,1],edge_list[jj,2]] <- A[edge_list[jj,1],edge_list[jj,2]]+1
    }
    
  } 
  g_igraph <- graph.adjacency(A,mode="directed",weight=T)
  g_act_graph[ii,] <- as.vector(A)
  # in degree
  g_req[ii,indeg.names] <- sna:::degree(A,"indegree")
  # in degree
  g_req[ii,outdeg.names] <- sna:::degree(A,"outdegree")
  # is the graph isolte w.r.t to each node (ego)
  g_req[ii,conn.names] <- sna:::is.isolate(A,1:env_n)
  # degree centrality
  g_req[ii,"cen.deg"] <- sna:::centralization(A,FUN=degree)
  # flow centrality
  g_req[ii,"cen.flow"] <- sna:::centralization(A,FUN=flowbet)
  
  # local clustering
  g_req[ii,clust.names] <- igraph:::transitivity(g_igraph,type="barrat")
  g_req[ii,"clust"] <- igraph:::transitivity(g_igraph,type="global")
  
  # local clustering
  g_req[ii,pagerank.names] <- igraph:::page.rank(g_igraph)$vector
  
  # closness
  g_req[ii,close.names] <- sna:::closeness(A)
  # efficiency of a graph
  g_req[ii,"eff"] <- sna:::efficiency(A)
  # hierarchy
  g_req[ii,"hier"]<- sna:::hierarchy(A)
  # connectedness
  g_req[ii,"conn"] <- sna:::connectedness(A)
  # lubness
  g_req[ii,"lub"] <- sna:::lubness(A)
  # graph density 
  g_req[ii,"den"] <- sna:::gden(A)
  # is connected
  g_req[ii,"is.conn"] <-sna:::is.connected(A)
  # mutuality (# of dyands in the di-graph)
  g_req[ii,"mut"] <-sna:::mutuality(A)
  # number of ties in a graph
  g_req[ii,"nties"] <- sna:::nties(A)
  
  if(length(ind)>5)
  {
    # number of cutpoints
    g_opt[ii,"ncut"] <- length(sna:::cutpoints(A))
    
    
    
    # betweenness centrality of each node
    g_opt[ii,betw.names] <- sna:::betweenness(A)
        
    # structure w.r.t to each node
    #g[ii,strct.names] <- structure.statistics(A,1:env_n)
    
    # other centrality functions are possible
    g_opt[ii,"cen.eig"] <- sna:::centralization(A,FUN=evcent)
    g_opt[ii,"cen.load"] <- sna:::centralization(A,FUN=loadcent)
    g_opt[ii,"cen.betw"] <- sna:::centralization(A,FUN=betweenness)
  }  
  # e2 <- dyad.census(A)
  # # reachability matrix
  # e9 <- reachability(A2)
  }, error=function(e) print(paste(ii,e))
  )
}

write.csv(g_act_graph,graph_act.filename,row.names=F)
write.csv(g_req,req_act.filename,row.names=F)
write.csv(g_opt,opt_act.filename,row.names=F)

