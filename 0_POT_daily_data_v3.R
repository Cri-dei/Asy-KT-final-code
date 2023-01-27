###########  PAPER QQ ########
#### CODE FOR DAILY DISCHARGE POT ##########
######Author: Cristina Deidda ##########

# v:won:09/03/22: Compute POT  # 
# v: add length of data 12/04/2022 #
# V100622: Choose basins you want to investigate. Cleaned just for selected one
  #Input: 

#################################################################

######## IN THIS CODE ARE ################# 

rm(list = ls())


############################################################
##### Enjoy :) #############

library(date)
library(lubridate)
library(tidyverse)
library(gtools)
library(geosphere)
library(trend)
library(zoo)
library(rnrfa)
library(tictoc)

# CHOOSE DIRECTORY

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
#path<-c("C:/PROJECTS 2021/QQ")
#new computer

path<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed")

setwd(path)

#pc portatile
#path<-c("C:/Users/39349/Documents/Regional")


#############################0. IMPORT FILE ######################################

source("./Code/Functions/Randomization.R")
source("./Code/Functions/POT_annualmax.R")
source("./Code/Functions/POT_monthly_variable_max.R")
source("./Code/Functions/POT_eventmix_12.R")
source("./Code/Functions/POT_eventmix_21.R")
source("./Code/Functions/Event_independence_v2.R")

source("./Code/Functions/Asymmetric_KT.R")
### Set English language for date format

Sys.setlocale('LC_ALL','en_CA.utf-8');
Sys.setlocale('LC_ALL','English');

####### Load info coordinates ########

path0<-c(paste0(path,"/Data/"))

#Data_INFO<-read.csv("Catch_info_final.csv",sep=";")
M_coord<- read.table(paste0(path0,"nrfa-coords3.csv"), header = TRUE, sep=",")    #table excel with value


# Load daily data

load("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Data/Daily Data/Daily_data.RData")

#load("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/STSM/Code_Direction/Spey.RData")


 ########################## FROM GIS ######################################

#Load stations' codes that we want to investigate
# Import from GIS-> coordinates and names of stations that are in the basin considered

path_GIS<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/STSM/GIS/Selection_June/")


Basin<-read.csv(paste0(path_GIS,"Selected_points_1306.csv"))


################## RUN for Basins selected  ######################

# Basin selected

Available_daily<-na.omit(as.numeric(as.character(names(Daily_data))))
St_selected<- Basin$id

pos<-which(St_selected %in% Available_daily)
Available_st<-St_selected[pos]

comb_st<-data.frame(t(combn(Available_st,2)))
code<-seq(1:nrow(comb_st))
Allcomb<-data.frame(cbind(code,comb_st))
colnames(Allcomb)<-c("CODE","ID_Station_1","ID_Station_2")


#############################################################

Couples_investigate<- Allcomb

########## CHOOSE TYPE OF ANALYSIS ###############

TYPE_ANALYSIS<- "POT on daily data"
Lag_time<-0
pb_TH<-0.98

##################################################

POT_matrix<-as.data.frame(matrix(, nrow = nrow(Couples_investigate), ncol = 18))

colnames(POT_matrix)<-c("CODE", "ID_Station_1",  "ID_Station_2",
                        "Easting_1","Northing_1","Easting_2","Northing_2","Distance","Year_st","Year_end"
                        , "POT_KT_12", "POT_pvalue_12", "Syn_12",  "POT_KT_21", "POT_pvalue_21","Syn_21","N_point12",
                        "N_point21")


List_couple<- vector(mode = "list", length = nrow(Couples_investigate))

xx<-1
#xx<-193898
for (yy in 1: nrow(Allcomb))

#for (yy in 1: 3)
{
  ######
  ID_Station_1<-Allcomb$ID_Station_1[yy]
  ID_Station_2<-Allcomb$ID_Station_2[yy]
  
  #######
  
  D1<-which(names(Daily_data)== Allcomb$ID_Station_1[yy])
  D2<-which(names(Daily_data)== Allcomb$ID_Station_2[yy])
 
  DAILY_Station_1<- cbind(rownames(data.frame(Daily_data[[D1]])),data.frame(Daily_data[[D1]]))
  colnames(DAILY_Station_1)<-c("Data_1","Discharge_1")
  
  #Read daily discharge for Station 2
  
  DAILY_Station_2<- cbind(rownames(data.frame(Daily_data[[D2]])),data.frame(Daily_data[[D2]]))
  colnames(DAILY_Station_2)<-c("Data_2","Discharge_2")
  
  
  DAILY_Station_1$Data_1<-as.Date(as.POSIXct(DAILY_Station_1$Data_1, format="%Y-%m-%d",tz="UTC"))
  DAILY_Station_2$Data_2<-as.Date(as.POSIXct(DAILY_Station_2$Data_2, format="%Y-%m-%d",tz="UTC"))
  
  
  DAILY_Station_1$Year<-year(as.Date(DAILY_Station_1$Data_1, '%Y-%m-%d'))
  DAILY_Station_2$Year<-year(as.Date(DAILY_Station_2$Data_2, '%Y-%m-%d'))
   
  DAILY_Station_1$Month<-month(as.Date(DAILY_Station_1$Data_1, '%Y-%m-%d'))
  DAILY_Station_2$Month<-month(as.Date(DAILY_Station_2$Data_2, '%Y-%m-%d'))
  
  DAILY_Station_1$Juliand_1<-julian.Date(DAILY_Station_1$Data_1,origin=as.Date("1940-01-01"))
  DAILY_Station_2$Juliand_2<-julian.Date(DAILY_Station_2$Data_2,origin=as.Date("1940-01-01"))
  
  
  DAILY_merged<- merge(DAILY_Station_1,DAILY_Station_2, by.x = "Juliand_1", by.y = "Juliand_2")
  
  if(dim(DAILY_merged)[1]>0)
  {
  
  Y_St<-DAILY_merged$Juliand_1[1]
  Y_End<-DAILY_merged$Juliand_1[nrow(DAILY_merged)]  
  
  
  Stat_1_adj<-DAILY_Station_1[which(DAILY_Station_1$Juliand_1==Y_St):which(DAILY_Station_1$Juliand_1==Y_End),]
  Stat_2_adj<-DAILY_Station_2[which(DAILY_Station_2$Juliand_2==Y_St):which(DAILY_Station_2$Juliand_2==Y_End),]
  
  Daily_St_first<-Stat_1_adj
  Daily_St_second<-Stat_2_adj
  

  
  tic()
  
  Asy_KT<-Asymmetric_KT(Stat_1_adj,Stat_2_adj, pb_TH,Lag_time)
  
  toc()
  
  if(class(Asy_KT)!="character")
  {
  
  POT_matrix$POT_KT_12[xx]<- Asy_KT$`KT test 12`$estimate
  POT_matrix$POT_pvalue_12[xx]<- Asy_KT$`KT test 12`$p.value 
  POT_matrix$Syn_12[xx]<-Asy_KT$`Syn12`
  
  
  POT_matrix$POT_KT_21[xx]<- Asy_KT$`KT test 21`$estimate
  POT_matrix$POT_pvalue_21[xx]<- Asy_KT$`KT test 21`$p.value
  POT_matrix$Syn_21[xx]<-Asy_KT$`Syn21`     
  
  
  List_couple[[xx]]<-Asy_KT$Couples
  names(List_couple)[xx]<-yy
  names(List_couple[[xx]])<-c(ID_Station_1,ID_Station_2)
  
  names(Asy_KT$Couples)<-c(ID_Station_1,ID_Station_2)
  
  POT_matrix$CODE[xx]<-yy
  POT_matrix$ID_Station_1[xx]<-ID_Station_1
  POT_matrix$ID_Station_2[xx]<-ID_Station_2
  
  
  cc_l1<- which(M_coord$Station.number==ID_Station_1)
  cc_l2<- which(M_coord$Station.number==ID_Station_2)
  
  POT_matrix$Easting_1[xx]<-M_coord$Easting[cc_l1]
  POT_matrix$Northing_1[xx]<-M_coord$Northing[cc_l1]
  
  POT_matrix$Easting_2[xx]<-M_coord$Easting[cc_l2]
  POT_matrix$Northing_2[xx]<-M_coord$Northing[cc_l2]       
  
  POT_matrix$Distance[xx]<-distGeo(c(M_coord$Longitude[cc_l1], M_coord$Latitude[cc_l1]), c(M_coord$Longitude[cc_l2], M_coord$Latitude[cc_l2]))
  
  POT_matrix$Year_st[xx]<-DAILY_merged$Year.x[1]
  POT_matrix$Year_end[xx]<-DAILY_merged$Year.x[nrow(DAILY_merged)]
  
  POT_matrix$N_point12[xx]<-nrow(Asy_KT$Couples[[1]])
  POT_matrix$N_point21[xx]<-nrow(Asy_KT$Couples[[2]])
  }
  
  } 
  
  print(yy)
  xx<-xx+1
  
}



############# All combination between stations ########

#Allcomb<-data.frame(combinations(length(Available_st), 2, v=Available_st, set=TRUE, repeats.allowed=FALSE))
#save(Allcomb,file=paste0(path,"/Data/Allcombination.RData"))

print(paste0("You run the analysis:",TYPE_ANALYSIS))

na_pos<-which(is.na(POT_matrix$CODE))


POT_matrix_without_na<-POT_matrix[-na_pos,]

############ Saving results #################

## All possible combinations 

Path_saving<-paste0("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/All couples/Results_TH",pb_TH)
setwd(Path_saving)

save.image(paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(POT_matrix,POT_matrix_without_na,file=paste0("Final_matrix_DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(List_couple, file=paste0("DAILY_POT_List_couple_lag",Lag_time,"_TH",pb_TH,".RData"))
write.table(POT_matrix,paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".csv"), row.names=F, col.names=T)


print(paste0("Results has been saved here:",Path_saving,pb_TH))

print(paste0("You run the analysis:",TYPE_ANALYSIS," with TH: ", pb_TH))

#####################################################################################################################


## Selected basins 

#Path_saving<-paste0("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/1.Selected couples1306/Results_TH",pb_TH)
#setwd(Path_saving)

#setwd("D:/")
setwd("D:/Results_BasinSelected/TH0.98")



save.image(paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(POT_matrix,POT_matrix_without_na,file=paste0("Final_matrix_DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(List_couple, file=paste0("DAILY_POT_List_couple_lag",Lag_time,"_TH",pb_TH,".RData"))
write.table(POT_matrix,paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".csv"), row.names=F, col.names=T)


print(paste0("Results has been saved here:",Path_saving))

print(paste0("You run the analysis:",TYPE_ANALYSIS," with TH: ", pb_TH))


######################################################################################################################

#JOINT Spey and Basin 1277 results


setwd(paste0("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/Results_TH",pb_TH))

save.image(paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(POT_matrix,POT_matrix_without_na,file=paste0("Final_matrix_DAILY_POT_lag",Lag_time,"_TH",pb_TH,".RData"))
save(List_couple, file=paste0("DAILY_POT_List_couple_lag",Lag_time,"_TH",pb_TH,".RData"))
write.table(POT_matrix,paste0("DAILY_POT_lag",Lag_time,"_TH",pb_TH,".csv"), row.names=F, col.names=T)


print(paste0("Results has been saved here: C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/Results_TH",pb_TH))

print(paste0("You run the analysis:",TYPE_ANALYSIS," with TH: ", pb_TH))

########################################################################
  
  