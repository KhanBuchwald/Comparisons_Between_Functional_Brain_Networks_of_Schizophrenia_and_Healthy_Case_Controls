library(network)
#library(ggnet)
library(bnlearn)
library(ggplot2)
library(UpSetR)
library(networkDynamic)
library(igraph)
library(GGally)
library(brainconn)
library(tidyr)
library(cowplot)
library(svglite)
library(dbnR)
library(data.table)
library(corpcor)

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Linearity_Bootstrapped_Edges")
files1=list.files()
SE=grep("SE", files1, value=T)
SR=grep("SR", files1, value=T)
HE=grep("HE", files1, value=T)
HR=grep("HR", files1, value=T)

WG_CV_SE=data.frame()
WG_CV_SR=data.frame()
WG_CV_HE=data.frame()
WG_CV_HR=data.frame()

for(i in 1:length(SE)){
  CV=read.csv(SE[i], header=T)
  WG_CV_SE=rbind(WG_CV_SE, CV)
}

for(i in 1:length(SR)){
  CV=read.csv(SR[i], header=T)
  WG_CV_SR=rbind(WG_CV_SR, CV)
}

for(i in 1:length(HE)){
  CV=read.csv(HE[i], header=T)
  WG_CV_HE=rbind(WG_CV_HE, CV)
}

for(i in 1:length(HR)){
  CV=read.csv(HR[i], header=T)
  WG_CV_HR=rbind(WG_CV_HR, CV)
}

BSE=WG_CV_SE[,c(2,3)]
BSR=WG_CV_SR[,c(2,3)]
BHE=WG_CV_HE[,c(2,3)]
BHR=WG_CV_HR[,c(2,3)]
names(BSE)=c("from", "to")
names(BSR)=c("from", "to")
names(BHE)=c("from", "to")
names(BHR)=c("from", "to")
BSE$string=paste(BSE$from, BSE$to, sep=",")
BSR$string=paste(BSR$from, BSR$to, sep=",")
BHE$string=paste(BHE$from, BHE$to, sep=",")
BHR$string=paste(BHR$from, BHR$to, sep=",")

## Optimal Threshold
setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Linearity_Bootstrapped_Edges")
nodes=read.csv(SE[1], header=T)
nodes2=unique(c(nodes$from, nodes$to))

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/Linearity_Bootstrapped_Edges")

dat_cob_fun=function(dat){
  CV=read.csv(dat, header=T)
  arcs1=as.matrix(data.frame(from=CV$from, to=CV$to))
  return(arcs1)
}

list1=lapply(SE,dat_cob_fun)
Finally1=custom.strength(list1, nodes2)
averaged.network(Finally1)

list2=lapply(SR,dat_cob_fun)
Finally2=custom.strength(list2, nodes2)
averaged.network(Finally2)

list3=lapply(HE,dat_cob_fun)
Finally3=custom.strength(list3, nodes2)
averaged.network(Finally3)

list4=lapply(HR,dat_cob_fun)
Finally4=custom.strength(list4, nodes2)
averaged.network(Finally4)

comdat=rbind.data.frame(BSE, BSR, BHE, BHR)
comdat$dataframe=c(rep("BSE", nrow(BSE)), rep("BSR", nrow(BSR)), rep("BHE", nrow(BHE)), rep("BHR", nrow(BHR)))

## Centrality and Network results

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
#save.image("260424_Imported_Edges.RData")

load("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/241220_Imported_Edges.RData")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Manuscript_Document/ANZJP")

## Return_dataset with all edges at threshold, keep the strongest edge if edge is in both directions

return_dataset <- function(dat0, n) {
  dat <- comdat[comdat$dataframe == dat0, ]
  dat$weight=rep(1, nrow(dat))
  
  g <- graph_from_data_frame(cbind.data.frame(from = dat$from, to = dat$to), directed = TRUE)
  
  # Convert the igraph object to a data frame of edges
  
  edge_df0 <- get.data.frame(g, what = "edges")
  
  ## Obtain edge weights
  
  edge_df01 <- aggregate(cbind(weight = from) ~ from + to, data = edge_df0, FUN = function(x) length(x))
  edge_df01$weight=as.numeric(edge_df01$weight)
  names(edge_df01)[3] <- "weight"
  
  
  #  unique_edges <- retain_unique_edges(edge_df)
  
  filtered_edges <- edge_df01[edge_df01$weight > n, ]
  return(filtered_edges)
}

SE0=return_dataset("BSE", 0)
SR0=return_dataset("BSR", 0)
HE0=return_dataset("BHE", 0)
HR0=return_dataset("BHR", 0)

write.csv(SE0, "Edge_List_Schizophrenia_Encoding.csv", row.names = F)
write.csv(SR0, "Edge_List_Schizophrenia_Retrieval.csv", row.names = F)
write.csv(HE0, "Edge_List_Healthy_Controls_Encoding.csv", row.names = F)
write.csv(HR0, "Edge_List_Healthy_Controls_Retrieval.csv", row.names = F)


## Network Statistics

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Manuscript_Document/PLoS ONE/250131_Revision")

SE0=read.csv("Edge_List_Schizophrenia_Encoding.csv")
SR0=read.csv("Edge_List_Schizophrenia_Retrieval.csv")
HE0=read.csv("Edge_List_Healthy_Controls_Encoding.csv")
HR0=read.csv("Edge_List_Healthy_Controls_Retrieval.csv")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R_Output/260424_Network_Metrics")

## Transition edges

SE50=return_dataset("BSE", 49)
SR50=return_dataset("BSR", 49)
HE50=return_dataset("BHE", 49)
HR50=return_dataset("BHR", 49)

SE_T=SE50[grepl("t_1",SE50$from)&grepl("t_0",SE50$to),]
SR_T=SR50[grepl("t_1",SR50$from)&grepl("t_0",SR50$to),]
HE_T=HE50[grepl("t_1",HE50$from)&grepl("t_0",HE50$to),]
HR_T=HR50[grepl("t_1",HR50$from)&grepl("t_0",HR50$to),]

nrow(SE_T)/nrow(SE50)
nrow(SR_T)/nrow(SR50)
nrow(HE_T)/nrow(HE50)
nrow(HR_T)/nrow(HR50)

table(gsub("t_1", "", SE_T$from)==sub("t_0", "", SE_T$to))/nrow(SE_T)
table(gsub("t_1", "", SR_T$from)==sub("t_0", "", SR_T$to))/nrow(SR_T)
table(gsub("t_1", "", HE_T$from)==sub("t_0", "", HE_T$to))/nrow(HE_T)
table(gsub("t_1", "", HR_T$from)==sub("t_0", "", HR_T$to))/nrow(HR_T)

table(gsub("t_1", "", SE_T$from)==sub("t_0", "", SE_T$to))
table(gsub("t_1", "", SR_T$from)==sub("t_0", "", SR_T$to))
table(gsub("t_1", "", HE_T$from)==sub("t_0", "", HE_T$to))
table(gsub("t_1", "", HR_T$from)==sub("t_0", "", HR_T$to))

AutoSE=SE_T[gsub("t_1", "", SE_T$from)!=sub("t_0", "", SE_T$to),]
AutoSR=SR_T[gsub("t_1", "", SR_T$from)!=sub("t_0", "", SR_T$to),]
AutoHE=HE_T[gsub("t_1", "", HE_T$from)!=sub("t_0", "", HE_T$to),]
AutoHR=HR_T[gsub("t_1", "", HR_T$from)!=sub("t_0", "", HR_T$to),]

listInput <- list(PDS_Encoding=paste0(AutoSE[,1],AutoSE[,2]), PDS_Retrieval=paste0(AutoSR[,1],AutoSR[,2]), HC_Encoding=paste0(AutoHE[,1],AutoHE[,2]), HC_Retrieval=paste0(AutoHR[,1],AutoHR[,2]))
upset(fromList(listInput), order.by = "freq",text.scale = 1.25)

T_All=rbind.data.frame(cbind.data.frame(Group="SE", SE_T),cbind.data.frame(Group="SR", SR_T),cbind.data.frame(Group="HE", HE_T),cbind.data.frame(Group="HR", HR_T))
write.csv(T_All, "C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R Code/Matthieu_DBN/Khans_Results/261104_Transition_Network_Edges_Large_Network.csv")


## Create dataframes for data entry

mat.1<-mat.4<-mat.5<-mat.6<-mat.7<-cbind.data.frame(dat=c(rep("BSE", 100), rep("BSR", 100), rep("BHE", 100), rep("BHR",100)),PosVal=rep(seq(0,99,1),4))
mat.2<-cbind.data.frame(dat.1=c(rep("BSE", 100), rep("BSR", 100), rep("BSE", 100), rep("BHE",100)),dat.2=c(rep("BHE", 100), rep("BHR", 100), rep("BSR", 100), rep("BHR",100)),PosVal=rep(seq(0,99,1),4))
mat.3<-cbind.data.frame(dat.1=c(rep("BSE", 100), rep("BSR", 100)),dat.2=c(rep("BHE", 100), rep("BHR", 100)),PosVal=rep(seq(0,99,1),2))



## Number of arcs

Number_Arcs_N=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  return(nrow(filtered_edges))
}

mat.1$Number_Arcs<-mapply(Number_Arcs_N, mat.1[,1], mat.1[,2])

write.csv(mat.1, "240617_Number_Arcs.csv")

## percent shared edges

Shared_arcs2=function(dat.1, dat.2,n){
  dat1=return_dataset(dat.1,n) 
  dat2=return_dataset(dat.2,n)
  t1=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["TRUE"]]
  t2=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["FALSE"]]
  t3=round((t1/(t1+t2))*100,2)
  return(t3)
}

mat.2$result=mapply(Shared_arcs2, mat.2$dat.1, mat.2$dat.2, mat.2$PosVal)

write.csv(mat.2, "240617_Proportion_of_shared_edges.csv")

## Symmetric Difference of edges

Total_Arcs_Not_Shared=function(n,dat.1, dat.2){
  dat1=return_dataset(dat.1,n) 
  dat2=return_dataset(dat.2,n)
  t2=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["TRUE"]]
  t1=table(paste(dat1$from, dat1$to, sep="_") %in% paste(dat2$from, dat2$to, sep="_"))[["FALSE"]]
  t3=round((t1/(t1+t2))*100,2)
  return(t3)
}

mat.3$result=mapply(Total_Arcs_Not_Shared, mat.3$PosVal, mat.3$dat.1, mat.3$dat.2)

write.csv(mat.3, "240617_Proportion_of_nonshared_edges.csv")

## Betweenness

Closeness_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean(closeness(g, weights = NULL), na.rm=T))
}

mat.4$result=mapply(Closeness_fun, mat.4[,1], mat.4[,2])
write.csv(mat.4, "240617_Closeness.csv")

## Node degree

Degree_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean(strength(g, weights = NULL)))
}

mat.5$result=mapply(Degree_fun, mat.5[,1], mat.5[,2])
write.csv(mat.5, "240617_Degree.csv")

## Clustering coefficient

Cluster_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(transitivity(g, type = "global"))
}

mat.6$result=mapply(Cluster_fun, mat.6[,1], mat.6[,2])
write.csv(mat.6, "240617_Cluster_Coefficient.csv")

## Shortest Path Length (Unweighted)

Path_Length_fun=function(dat0, n){
  filtered_edges=return_dataset(dat0, n)
  filtered_edges2=cbind.data.frame(from=filtered_edges$from, to=filtered_edges$to)
  g <- graph_from_data_frame(filtered_edges2, directed = TRUE)
  return(mean_distance(g, directed = T, weights=NULL))
}

mat.7$result=mapply(Path_Length_fun, mat.7[,1], mat.7[,2])
write.csv(mat.7, "240617_Path_Length.csv")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data")
save.image("260424_Network_statistics.RData")

## Partial correlations

dat=read.csv("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Data/261120_Transformed_Responses_For_Corr_Network.csv")

SE50.1=cbind.data.frame(conditiongroup=rep("S.E", nrow(SE50)), SE50)
SR50.1=cbind.data.frame(conditiongroup=rep("S.R", nrow(SR50)), SR50)
HE50.1=cbind.data.frame(conditiongroup=rep("H.E", nrow(HE50)), HE50)
HR50.1=cbind.data.frame(conditiongroup=rep("H.R", nrow(HR50)), HR50)

bn_dat=rbind.data.frame(SE50.1, SR50.1,HE50.1, HR50.1)

vars_for_model <- function(dt) {
  setdiff(names(dt), c("X","Participant_id","inst_id","trialtype2","conditiongroup"))
}

GROUPS <- c("H.E","H.R","S.E","S.R")
results <- list()
for (grp in GROUPS) {
  d <- dat[dat$conditiongroup == grp,]
  vars <- vars_for_model(d)
  X <- as.matrix(d[, vars])
  C <- cor(X)
  R <- cor2pcor(C)
  dimnames(R) <- dimnames(C)
  n <- nrow(X)
  pairs <- combn(vars, 2)
  z <- atanh(R[t(pairs)]); se <- 1/sqrt(n-3)
  pval <- 2*pnorm(-abs(z/se)); padj <- p.adjust(pval, method="BH"); sig <- padj < 0.05
  corr_edges <- sum(sig)
  d2=bn_dat[bn_dat$conditiongroup == grp,]
  intra0 <- d2[grepl("_t_0$",d2$from) & grepl("_t_0$",d2$to),]
  base_from <- sub("_t_0$","",intra0$from); base_to <- sub("_t_0$","",intra0$to)
  skeleton <- unique(t(apply(cbind(base_from,base_to),1,sort)))
  dbn_edges <- nrow(skeleton)
  corr_pairs_sig <- t(pairs[,sig,drop=FALSE]); corr_pairs_sig <- unique(t(apply(corr_pairs_sig,1,sort)))
  corr_set <- apply(corr_pairs_sig,1,paste,collapse="|"); dbn_set <- apply(skeleton,1,paste,collapse="|")
  overlap <- length(intersect(corr_set,dbn_set)); jaccard <- overlap/length(union(corr_set,dbn_set))
  results[[grp]] <- data.frame(group=grp, correlation_edges=corr_edges, dbn_static_edges=dbn_edges,
                               overlap=overlap, jaccard=round(jaccard,3))
  cat(sprintf("%s: Pearson-FDR=%d | DBN static=%d | overlap=%d | Jaccard=%.3f\n", grp, corr_edges, dbn_edges, overlap, jaccard))
}
write.csv(do.call(rbind, results), "results/correlation_vs_dbn_comparison_full.csv", row.names=FALSE))
