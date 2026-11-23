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

comdat <- readRDS("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R Code/Matthieu_DBN/Khans_Data/strength_tabs_full.rds")

setwd("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/R/R Code/Matthieu_DBN/Khans_Results")

## Return_dataset with all edges at threshold, keep the strongest edge if edge is in both directions

return_dataset <- function(dat0, n) {
  dat <- as.data.frame(comdat[dat0])
  names(dat)=c("from", "to", "strength")
  dat$strength=100*dat$strength
  filtered_edges <- dat[dat$strength > n, ]
  return(filtered_edges)
}

SE50.1=return_dataset("S.E", 49)
SR50.1=return_dataset("S.R", 49)
HE50.1=return_dataset("H.E", 49)
HR50.1=return_dataset("H.R", 49)

write.csv(SE50.1, "261021_Edge_List_Schizophrenia_Encoding.csv", row.names = F)
write.csv(SR50.1, "261021_Edge_List_Schizophrenia_Retrieval.csv", row.names = F)
write.csv(HE50.1, "261021_Edge_List_Healthy_Controls_Encoding.csv", row.names = F)
write.csv(HR50.1, "261021_Edge_List_Healthy_Controls_Retrieval.csv", row.names = F)

## List fronto-parietal circuit

n1=c("Frontal_Inf_Oper_L_t_0",   "Frontal_Inf_Oper_L_t_1",   "Frontal_Inf_Oper_R_t_0",   "Frontal_Inf_Oper_R_t_1",
     "Frontal_Inf_Orb_L_t_0",    "Frontal_Inf_Orb_L_t_1",    "Frontal_Inf_Orb_R_t_0",    "Frontal_Inf_Orb_R_t_1",   
     "Frontal_Inf_Tri_L_t_0",    "Frontal_Inf_Tri_L_t_1",    "Frontal_Inf_Tri_R_t_0",    "Frontal_Inf_Tri_R_t_1",
     "Frontal_Mid_L_t_0",        "Frontal_Mid_L_t_1",        "Frontal_Mid_R_t_0",        "Frontal_Mid_R_t_1",       
     "Frontal_Mid_Orb_L_t_0",    "Frontal_Mid_Orb_L_t_1",    "Frontal_Mid_Orb_R_t_0",    "Frontal_Mid_Orb_R_t_1",
     "Temporal_Sup_L_t_0",       "Temporal_Sup_L_t_1",       "Temporal_Sup_R_t_0",       "Temporal_Sup_R_t_1"
     )           

## List Default Mode Network circuit

n2=c("Precentral_L_t_0",         "Precentral_L_t_1",        "Precentral_R_t_0",         "Precentral_R_t_1",
     "Precentral_R_t_0",         "Precentral_R_t_1",        "Frontal_Sup_R_t_0",        "Frontal_Sup_R_t_1",
     "Frontal_Sup_Orb_L_t_0",    "Frontal_Sup_Orb_L_t_1",   "Frontal_Sup_Orb_R_t_0",    "Frontal_Sup_Orb_R_t_1",
     "Frontal_Sup_Medial_L_t_0", "Frontal_Sup_Medial_L_t_1","Frontal_Sup_Medial_R_t_0", "Frontal_Sup_Medial_R_t_1",
     "Frontal_Med_Orb_L_t_0",    "Frontal_Med_Orb_L_t_1",   "Frontal_Med_Orb_R_t_0",    "Frontal_Med_Orb_R_t_1",
     "Cingulum_Ant_L_t_0",       "Cingulum_Ant_L_t_1",      "Cingulum_Ant_R_t_0",       "Cingulum_Ant_R_t_1"
) 

## List Sensorimotor-temporal circuit

n3=c("Rolandic_Oper_L_t_0",      "Rolandic_Oper_L_t_1",     "Rolandic_Oper_R_t_0",      "Rolandic_Oper_R_t_1",
     "Postcentral_L_t_0",        "Postcentral_L_t_1",       "Postcentral_R_t_0",        "Postcentral_R_t_1",
     "SupraMarginal_L_t_0",      "SupraMarginal_L_t_1",     "SupraMarginal_R_t_0",      "SupraMarginal_R_t_1",
     "Heschl_L_t_0",             "Heschl_L_t_1",            "Heschl_R_t_0",             "Heschl_R_t_1",
     "Temporal_Sup_L_t_0",       "Temporal_Sup_L_t_1",      "Temporal_Sup_R_t_0",       "Temporal_Sup_R_t_1"
) 

## Edges within FP circuit

Net_SE_FP_Network=NetSE50.1[NetSE50.1$from%in%n1,]
Net_SE_FP_Network2=Net_SE_FP_Network[Net_SE_FP_Network$to%in%n1,]

Net_HE_FP_Network=NetHE50.1[NetHE50.1$from%in%n1,]
Net_HE_FP_Network2=Net_HE_FP_Network[Net_HE_FP_Network$to%in%n1,]

Net_SR_FP_Network=NetSR50.1[NetSR50.1$from%in%n1,]
Net_SR_FP_Network2=Net_SR_FP_Network[Net_SR_FP_Network$to%in%n1,]

Net_HR_FP_Network=NetHR50.1[NetHR50.1$from%in%n1,]
Net_HR_FP_Network2=Net_HR_FP_Network[Net_HR_FP_Network$to%in%n1,]

## Edges within DMN circuit

Net_SE_DMN_Network=NetSE50.1[NetSE50.1$from%in%n2,]
Net_SE_DMN_Network2=Net_SE_DMN_Network[Net_SE_DMN_Network$to%in%n2,]

Net_HE_DMN_Network=NetHE50.1[NetHE50.1$from%in%n2,]
Net_HE_DMN_Network2=Net_HE_DMN_Network[Net_HE_DMN_Network$to%in%n2,]

Net_SR_DMN_Network=NetSR50.1[NetSR50.1$from%in%n2,]
Net_SR_DMN_Network2=Net_SR_DMN_Network[Net_SR_DMN_Network$to%in%n2,]

Net_HR_DMN_Network=NetHR50.1[NetHR50.1$from%in%n2,]
Net_HR_DMN_Network2=Net_HR_DMN_Network[Net_HR_DMN_Network$to%in%n2,]

## Edges within SMT circuit

Net_SE_SMT_Network=NetSE50.1[NetSE50.1$from%in%n3,]
Net_SE_SMT_Network2=Net_SE_SMT_Network[Net_SE_SMT_Network$to%in%n3,]

Net_HE_SMT_Network=NetHE50.1[NetHE50.1$from%in%n3,]
Net_HE_SMT_Network2=Net_HE_SMT_Network[Net_HE_SMT_Network$to%in%n3,]

Net_SR_SMT_Network=NetSR50.1[NetSR50.1$from%in%n3,]
Net_SR_SMT_Network2=Net_SR_SMT_Network[Net_SR_SMT_Network$to%in%n3,]

Net_HR_SMT_Network=NetHR50.1[NetHR50.1$from%in%n3,]
Net_HR_SMT_Network2=Net_HR_SMT_Network[Net_HR_SMT_Network$to%in%n3,]

## ALL atlas numbers

aal_numbers=read.table("C:/Users/khanb/OneDrive/Documents C/Documents/University/Massey University/161.893/Manuscripts/Disconnection Hypothesis/Manuscript_Document/PLoS ONE/Revisions/241206_Revision/aal116NodeNames.txt")
aal_numbers$V2=seq(1,116,1)

Obtain_adjacency_matrix_static=function(net){
  
  from <- lapply(net$from, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  to <- lapply(net$to, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  from_to=cbind.data.frame(from=unlist(from),to=unlist(to))
  
  ## rownumbers for each time period
  
  static=which(from_to$from=="t_1"&from_to$to=="t_1")
  #transition=as.numeric(rownames(from_to))[!as.numeric(rownames(from_to))%in%c(time1, time2)]
  
  ## t1 aal numbers
  
  aal_numbers2=aal_numbers
  aal_numbers2$V1=paste(aal_numbers$V1, "t_1", sep="_")
  
  Net_time1=net[static,]
  
  ## brain through brain region ID
  
  Net_SE_FP_Network3=merge(Net_time1, aal_numbers2, by.x="from", by.y="V1", all.x=T)
  Net_SE_FP_Network4=merge(Net_SE_FP_Network3, aal_numbers2, by.x="to", by.y="V1", all.x=T)
  
  ## Order dataset by brain region number
  
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$to, decreasing = T),]
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$from, decreasing = T),]
  
  ## Remove brain regions in character format
  
  Net_SE_FP=Net_SE_FP_Network4[,c(4,5,3)]
  Net_SE_FP1=Net_SE_FP[!is.na(Net_SE_FP$V2.x)&!is.na(Net_SE_FP$V2.y),]
  ## create wide dataframe
  
  Net_SE_FP2=as.data.frame(t(as.data.frame(pivot_wider(Net_SE_FP1, names_from ="V2.x", values_from = "strength", id_cols = "V2.y"))))
  
  ## Specify rownames
  
  names(Net_SE_FP2)=Net_SE_FP2[1,]
  
  ## delete rowname column
  
  Net_SE_FP2=Net_SE_FP2[-c(1),]
  
  # Identify names not included
  
  s1=seq(1,116,1)[!seq(1,116,1)%in%colnames(Net_SE_FP2)]
  s2=seq(1,116,1)[!seq(1,116,1)%in%rownames(Net_SE_FP2)]
  
  ## Create empty dataframe
  
  df=as.data.frame(matrix(rep(NA, nrow(Net_SE_FP2)*length(s1)), nrow = nrow(Net_SE_FP2), ncol=length(s1)))
  
  ## provide colnames
  
  names(df)=s1
  
  ## Combine dataframes
  
  Net_SE_FP3=cbind.data.frame(Net_SE_FP2, df)
  Net_SE_FP4=Net_SE_FP3
  
  ## Add rows
  
  Net_SE_FP4[(nrow(Net_SE_FP3)+1):116,]=NA
  
  ## rename rows
  
  rownames(Net_SE_FP4)[(nrow(Net_SE_FP3)+1):116]=s2
  
  ## Order rows and columns
  
  Net_SE_FP4=Net_SE_FP4[,order(as.numeric(colnames(Net_SE_FP4)))]
  Net_SE_FP4=Net_SE_FP4[order(as.numeric(rownames(Net_SE_FP4))),]
  
  ## Change NA to zero and make it unweighted
  
  Net_SE_FP4[is.na(Net_SE_FP4)]=0
  Net_SE_FP4[Net_SE_FP4>0]=1
  return(Net_SE_FP4)
}
Obtain_adjacency_matrix_transition=function(net){
  
  from <- lapply(net$from, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  to <- lapply(net$to, function(x) {
    split_name <- strsplit(x, "_")[[1]]
    time_point <- paste0("t_", split_name[length(split_name)])
    return(time_point)
  })
  
  from_to=cbind.data.frame(from=unlist(from),to=unlist(to))
  
  ## rownumbers for each time period
  
  #static=which(from_to$from=="t_1"&from_to$to=="t_1")
  transition=which(from_to$from=="t_1"& from_to$to=="t_0")
  
  ## t1 aal numbers
  
  aal_numbers2=aal_numbers
  aal_numbers2=rbind.data.frame(aal_numbers2,aal_numbers2)
  aal_numbers2$V1[1:116]=paste(aal_numbers2$V1[1:116], "t_1", sep="_")
  aal_numbers2$V1[117:232]=paste(aal_numbers2$V1[117:232], "t_0", sep="_")
  
  Net_time1=net[transition,]
  
  ## brain through brain region ID
  
  Net_SE_FP_Network3=merge(Net_time1, aal_numbers2, by.x="from", by.y="V1", all.x=T)
  Net_SE_FP_Network4=merge(Net_SE_FP_Network3, aal_numbers2, by.x="to", by.y="V1", all.x=T)
  
  ## Order dataset by brain region number
  
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$to, decreasing = T),]
  Net_SE_FP_Network4=Net_SE_FP_Network4[order(Net_SE_FP_Network4$from, decreasing = T),]
  
  Net_SE_FP_Network5=Net_SE_FP_Network4[!is.na(Net_SE_FP_Network4$V2.x)|!is.na(Net_SE_FP_Network4$V2.y),]
  Net_SE_FP_Network6=Net_SE_FP_Network5[gsub("t_0", "",Net_SE_FP_Network5$to)!=gsub("t_1", "",Net_SE_FP_Network5$from),]
  
  ## Remove brain regions in character format
  
  Net_SE_FP=Net_SE_FP_Network6[,c(4,5,3)]
  
  ## create wide dataframe
  
  Net_SE_FP2=as.data.frame(t(as.data.frame(pivot_wider(Net_SE_FP, names_from ="V2.x", values_from = "strength", id_cols = "V2.y"))))
  
  ## Specify rownames
  
  names(Net_SE_FP2)=Net_SE_FP2[1,]
  
  ## delete rowname column
  
  Net_SE_FP2=Net_SE_FP2[-c(1),]
  
  # Identify names not included
  
  s1=seq(1,116,1)[!seq(1,116,1)%in%colnames(Net_SE_FP2)]
  s2=seq(1,116,1)[!seq(1,116,1)%in%rownames(Net_SE_FP2)]
  
  ## Create empty dataframe
  
  df=as.data.frame(matrix(rep(NA, nrow(Net_SE_FP2)*length(s1)), nrow = nrow(Net_SE_FP2), ncol=length(s1)))
  
  ## provide colnames
  
  names(df)=s1
  
  ## Combine dataframes
  
  Net_SE_FP3=cbind.data.frame(Net_SE_FP2, df)
  Net_SE_FP4=Net_SE_FP3
  
  ## Add rows
  
  Net_SE_FP4[(nrow(Net_SE_FP3)+1):116,]=NA
  
  ## rename rows
  
  rownames(Net_SE_FP4)[(nrow(Net_SE_FP3)+1):116]=s2
  
  ## Order rows and columns
  
  Net_SE_FP4=Net_SE_FP4[,order(as.numeric(colnames(Net_SE_FP4)))]
  Net_SE_FP4=Net_SE_FP4[order(as.numeric(rownames(Net_SE_FP4))),]
  
  ## Change NA to zero and make it unweighted
  
  Net_SE_FP4[is.na(Net_SE_FP4)]=0
  Net_SE_FP4[Net_SE_FP4>0]=1
  return(Net_SE_FP4)
}

## Whole Brain

SE_Whole_Brain_Static=Obtain_adjacency_matrix_static(NetSE50.1)
SR_Whole_Brain_Static=Obtain_adjacency_matrix_static(NetSR50.1)
HE_Whole_Brain_Static=Obtain_adjacency_matrix_static(NetHE50.1)
HR_Whole_Brain_Static=Obtain_adjacency_matrix_static(NetHR50.1)

## DMN

SE_DMN_Static=Obtain_adjacency_matrix_static(Net_SE_DMN_Network2)
SR_DMN_Static=Obtain_adjacency_matrix_static(Net_SR_DMN_Network2)
HE_DMN_Static=Obtain_adjacency_matrix_static(Net_HE_DMN_Network2)
HR_DMN_Static=Obtain_adjacency_matrix_static(Net_HR_DMN_Network2)

## FPC

SE_FPC_Static=Obtain_adjacency_matrix_static(Net_SE_FP_Network2)
SR_FPC_Static=Obtain_adjacency_matrix_static(Net_SR_FP_Network2)
HE_FPC_Static=Obtain_adjacency_matrix_static(Net_HE_FP_Network2)
HR_FPC_Static=Obtain_adjacency_matrix_static(Net_HR_FP_Network2)

## SMT

SE_SMT_Static=Obtain_adjacency_matrix_static(Net_SE_SMT_Network2)
SR_SMT_Static=Obtain_adjacency_matrix_static(Net_SR_SMT_Network2)
HE_SMT_Static=Obtain_adjacency_matrix_static(Net_HE_SMT_Network2)
HR_SMT_Static=Obtain_adjacency_matrix_static(Net_HR_SMT_Network2)

## Whole Brain

pse0=brainconn(atlas = "aal116", conmat = SE_Whole_Brain_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
psr0=brainconn(atlas = "aal116", conmat = SR_Whole_Brain_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phe0=brainconn(atlas = "aal116", conmat = HE_Whole_Brain_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phr0=brainconn(atlas = "aal116", conmat = HR_Whole_Brain_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)

svglite("Whole_Brain_Static_SE.svg", width = 10, height=10)
plot_grid(
  pse0,
  ncol =1
)
dev.off()

svglite("Whole_Brain_Static_SR.svg", width = 10, height=10)
plot_grid(
  psr0,
  ncol =1
)
dev.off()

svglite("Whole_Brain_Static_HE.svg", width = 10, height=10)
plot_grid(
  phe0,
  ncol =1
)
dev.off()

svglite("Whole_Brain_Static_HR.svg", width = 10, height=10)
plot_grid(
  phr0,
  ncol =1
)
dev.off()

## DFN

pse0=brainconn(atlas = "aal116", conmat = SE_DMN_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
psr0=brainconn(atlas = "aal116", conmat = SR_DMN_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phe0=brainconn(atlas = "aal116", conmat = HE_DMN_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phr0=brainconn(atlas = "aal116", conmat = HR_DMN_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)

svglite("DFN_Static.svg", width = 10, height=10)
plot_grid(
  pse0,psr0,phe0,phr0,
  ncol =2,nrow=2
)
dev.off()

## FPC

pse0=brainconn(atlas = "aal116", conmat = SE_FPC_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
psr0=brainconn(atlas = "aal116", conmat = SR_FPC_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phe0=brainconn(atlas = "aal116", conmat = HE_FPC_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phr0=brainconn(atlas = "aal116", conmat = HR_FPC_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)

svglite("FPC_Static.svg", width = 10, height=10)
plot_grid(
  pse0,psr0,phe0,phr0,
  ncol =2,nrow=2
)
dev.off()

## SMT

pse0=brainconn(atlas = "aal116", conmat = SE_SMT_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
psr0=brainconn(atlas = "aal116", conmat = SR_SMT_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phe0=brainconn(atlas = "aal116", conmat = HE_SMT_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)
phr0=brainconn(atlas = "aal116", conmat = HR_SMT_Static, view="ortho", show.legend=FALSE, labels=FALSE, node.size = 1,edge.width = .25)

svglite("SMT_Static.svg", width = 10, height=10)
plot_grid(
  pse0,psr0,phe0,phr0,
  ncol =2,nrow=2
)
dev.off()

