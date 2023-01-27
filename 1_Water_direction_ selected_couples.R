#####################################################################################
###################CODE FOR DERIVING WATER DIRECTION#################################
#####################################################################################
######## AUTHOR: CRISTINA DEIDDA (cristina.deidda@polimi.it) ########################
#####################################################################################

# From ArcGis Information: Shapefile of subcatchment and ID outlet stations it
# derives the flow direction for each couple of stations 

# Update 19/04/2022: Type of connection: if hydro connected or not

# Update: 04/05/2022: Run for selected basin from GIS

#####################################################################################

######## Clear Workspace ################# 

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
library(sf)
library(rgdal)
library(ggplot2)
library(gridExtra)
library(dplyr)


path_function<- c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/Functions/")
source(paste0(path_function,"Direct_max_plot_v2.R"))
source(paste0(path_function,"UKcoord_to_LatLong.R"))
source(paste0(path_function,"LatLong_to_NorthEast.R"))
source(paste0(path_function,"Plot_cut_map.R"))

# CHOOSE DIRECTORY

#pc ufficio
#setwd("D:/PROJECTS/Regional/DISTANCE_selection/Data")
#path<-c("C:/PROJECTS 2021/QQ")
#new computer

path<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/STSM/Code_Direction/")

setwd(path)

################ INPUT FROM ARCGIS PRO #####################################
# Sel_basin -> subcatchemnt infor merged to outlet id
# Coord_point-> coordinates of outlet points


path_GIS<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/STSM/GIS/Selection_June/")
setwd(path_GIS)

#Coordinate points basin selected
Coordinates_all<-read.csv("Selected_points_1306.csv")
#Extracted from ArcGis stations and basin catchment info
Join_St_all<-read.csv("Subcatchments_1606.csv")
#Catch_included<-read.csv("Catchment_HA_Selected.csv")


# Comb_st_basin -> all stations and subcatchment ID area 

Comb_St_basin<-Join_St_all%>% select('station_1','OBJECTID') %>% 
  rename(Station = station_1)%>% 
  rename(Basin_ID = OBJECTID)

# Shapefile subcatchment
map0 = read_sf(paste0(path_GIS,"Subcatchment_1306.shp"))

# Convert coordinates from Latitude and Longitude to NORTH and EAST

Coordinates_NE<-LatLong_to_NorthEast(Coordinates_all)

#####################################################################################
############################### Load Results for Selected Couples ########################################

path_dati<-("D:/Results_BasinSelected")

setwd(path_dati)

################# Choose PTH ######################

pth<-0.98


load(paste0("D:/Results_BasinSelected/TH",pth,"/DAILY_POT_lag0_TH",pth,".RData"))
Matrix_basin<-POT_matrix

##################################################################################

####### Select combinations we want to analyze ##########
  
# All combination that we have after POT analysis

  Allcomb<-  Matrix_basin %>% select('CODE','ID_Station_1','ID_Station_2')
  colnames(Allcomb)<-c("CODE","Station_1","Station_2")

# Combination from catchment selected from GIS
  
  #Available_st<- Coordinates_all$id
 # comb_st<-data.frame(t(combn(Available_st,2)))
  #code<-seq(1:nrow(comb_st))
 # Allcomb<-data.frame(cbind(code,comb_st))
  #colnames(Allcomb)<-c("CODE","ID_Station_1","ID_Station_2")
  

  
    
#MAtrix with outlet station and basin area ID

Matrix_SB<-merge(Allcomb,Comb_St_basin, by.x="Station_1", by.y="Station")%>% 
  relocate(CODE, .before = Station_1)

xx<-match(Matrix_SB$Station_2,Comb_St_basin$Station)

Matrix_SB$Basin_2<-Comb_St_basin$Basin[xx]


Matrix_SB<- Matrix_SB  %>%
  rename(Basin_ST1 = Basin_ID)%>%   
  rename(Basin_ST2 = Basin_2)

#Matrix_SB<- Matrix_SB %>% relocate(Station_1, .before = Station_2) %>% 
#  rename(Basin_ST1 = 3) %>% 
#  rename(Basin_ST2 = 4)

#Matrix_SB


######################### Water Direction ######################################

####################### BASIN BELONG MATRIX ##########################
# Check for each outlet station in which basin areas belongs
# Input: catchment area for each outlet point

# Matrix_basin River
#map0 = read_sf(paste0(path,"Sub_basin.shp"))


pnts_sf <- st_as_sf(Coordinates_NE, coords = c('EAST','NORTH'), crs = st_crs(map0)) 
pnts_sf_overall<-pnts_sf

for (i in 1:nrow(map0))
{
  map<-map0[i,]
  
  
  pnts <- pnts_sf %>% mutate(
    intersection = as.integer(st_intersects(geometry, map))
    , area = if_else(is.na(intersection), '',as.character(map$OBJECTID[intersection]) )
  ) 
  
  pnts
  
  pnts_sf_overall<-cbind(pnts_sf_overall,pnts$area)
  
}

# MATRIX With direction values for each couple : 12,21,0
#pnts_sf_overall

Matrix_SB$Direction<-NA

for (i in 1:nrow(Matrix_SB))
{
  
  row_ST1<- which(pnts_sf_overall$id==Matrix_SB$Station_1[i])
  row_ST2<- which(pnts_sf_overall$id==Matrix_SB$Station_2[i])  
  
  Matrix_SB$Direction[i]<- ifelse(Matrix_SB$Basin_ST2[i]%in% pnts_sf_overall[row_ST1,], 12,
                                  ifelse(Matrix_SB$Basin_ST1[i]%in% pnts_sf_overall[row_ST2,],21,0))
  
}

# Just consider stations for which we have subcatchment info
#Matrix_basin<-POT_matrix

Stat_consid<-unique(c(Matrix_basin$ID_Station_1,Matrix_basin$ID_Station_2))
NotB<- Stat_consid[which(!(Stat_consid%in%map0$station_1))]

Matrix_basin_2<-Matrix_basin[-which(Matrix_basin$ID_Station_1%in%NotB | Matrix_basin$ID_Station_2%in%NotB),]
#Matrix_basin_2<-Matrix_basin[-which(Matrix_basin$ID_Station_1==NotB | Matrix_basin$ID_Station_2==NotB),]
Matrix_SB<- Matrix_SB[-which(Matrix_SB$Station_1%in%NotB | Matrix_SB$Station_2%in%NotB),]  
  
# Matrix with catchment type for each station

Info_catch<- data.frame(map0[,c("HA", "station_1")])[,-3]



#Matrix_SB
Matrix_SB_sorted <- Matrix_SB[order(Matrix_SB$CODE),] 

View(Matrix_SB_sorted)

Check<-all(Matrix_SB_sorted$CODE==Matrix_basin_2$CODE)

if(Check==FALSE){ print("ERROR: DIRECTIONS DOES NOT COMBINE! CHECK Matrix SB")}

Matrix_basin_2$Direction<-Matrix_SB_sorted$Direction

############ Checking if points are hydraulically connected or not ############

################### Checking Type of connection ###########################

xx1<-merge(x = Matrix_SB_sorted[,c("CODE","Station_1")] , y = Info_catch, by.x = "Station_1",by.y="station_1", all.x=TRUE)
colnames(xx1)<-c("Station_1","CODE","HA_1")

xx2<-merge(x = Matrix_SB_sorted[,c("CODE","Station_2")] , y = Info_catch, by.x = "Station_2",by.y="station_1", all.x=TRUE)
colnames(xx2)<-c("Station_2","CODE","HA_2")


Merged_hyd<-left_join(xx1,xx2, by = "CODE")
Merged_hyd<-Merged_hyd[order(Merged_hyd$CODE),] 


Matrix_SB_proc<-left_join(Matrix_SB_sorted,Merged_hyd[,c("CODE","HA_1","HA_2")], by = "CODE")

Matrix_SB_proc$Type<-ifelse(Matrix_SB_proc$HA_1!=Matrix_SB_proc$HA_2 & Matrix_SB_proc$Direction==0,
                            "Different Basins",
                            ifelse(Matrix_SB_proc$HA_1==Matrix_SB_proc$HA_2 & Matrix_SB_proc$Direction==0,
                                   "Same Basin No Hyd Connected", 
                                   ifelse(Matrix_SB_proc$HA_1==Matrix_SB_proc$HA_2 & Matrix_SB_proc$Direction!=0,
                                          "Same Basin Hyd Connected",NA)))



Matrix_basin_proc<-left_join(Matrix_basin_2,Matrix_SB_proc[,c("CODE","HA_1","HA_2","Type")], by = "CODE")



#pth<-0.95

Matrix_basin_proc$HA<-rowSums(Matrix_basin_proc[,c(20,21)])

################################################################################

setwd(paste0("D:/Results_BasinSelected/TH",pth))

save(Matrix_basin,Matrix_basin_proc,pth, path, Matrix_SB_proc, file="POT_matrix_processed.Rdata")
#save(Hydro_info,pv_th, file="Hydro_Info.Rdata")
#save(POT_matr_HYDRO,pv_th, file="POT_samebasin.Rdata")
save(pth,file="Pv_th.RData")

save(Matrix_SB,Matrix_SB_proc,Matrix_SB_sorted, Merged_hyd,Join_St_all, pnts, pnts_sf, 
     pnts_sf_overall, Merged_hyd, Info_catch,
     path, POT_matrix, file="POT_matrix_waterdirection.Rdata")

################################################################################

##################### SCATTERPLOT ############################


Sub1<-Matrix_basin_proc
Sub1$HA_1[which(Sub1$ID_Station_1%in%stat)]<-57
Sub1$HA_2[which(Sub1$ID_Station_2%in%stat)]<-57


Sub1$HA<-rowSums(Sub1[,c(20,21)])


Sub2<-Sub1[which(Sub1$Type=="Same Basin Hyd Connected"),]
Sub<-Sub2[-which(Sub2$HA==108 | Sub2$HA==110),]


#Sub<-Sub2[which(Sub2$HA==108),]
#Sub<-Sub2

Sub$Diff<-abs(Sub$POT_KT_12-Sub$POT_KT_21)


setwd("D:/Results_BasinSelected/TH0.98")

## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(HA)), size=3,data= Sub[which( Sub$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(HA)), size=3,data= Sub[which( Sub$Direction==21),])+
 theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")

# +xlim(0,1)+ylim(0,1)
# scale_color_gradientn(colours = rev(rainbow(5)))+
ggsave("Upstream- Downstream direction KT Scatt_ALLmeno108.jpeg", units="in",dpi=400, height=6,width =8)



Basin_map<-map0$geometry %>% 
  ggplot() +
  geom_sf() +
  theme_bw()

th_diff<-0.01


Sub$Difference<- Sub$POT_KT_12 - Sub$POT_KT_21


Sub$max_pos<-ifelse(Sub$Difference>0, 1, ifelse(Sub$Difference<0, 2, ifelse(Sub$Difference<= th_diff && Sub$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 

map0$HA[which(map0$station_1%in%stat)]<-57

Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Diff),
  data = Sub[which(Sub$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Diff),
    data =  Sub[which(Sub$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+ 
  ggtitle(paste("Directional plot of Differences"))

Plot_cut_map(Difference_plot,map0[which(map0$HA==54),],0)

m1<-map0[which(map0$HA==54),]
####################### Save Path ##############################################

pv_th<-pth













################################################################################


#### Process Matrix_basin matrix

load("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Workspace/Pv_th.RData")

#####################################################################################
####################### P value threshold  ##########################################



####################################################################################

 Matrix_basin$DEP_12<-ifelse( Matrix_basin$POT_pvalue_12<=pv_th,2,1)
 Matrix_basin$DEP_21<-ifelse( Matrix_basin$POT_pvalue_21<=pv_th,2,1)

nrow( Matrix_basin)

 Matrix_basin$Equality<-rowSums( Matrix_basin[,c("DEP_12","DEP_21")])


 Matrix_basin$Status<-ifelse( Matrix_basin$Equality==2,"Both IND",
                          ifelse( Matrix_basin$Equality==4,"Both DEP","Not equal")) 


100*length(which( Matrix_basin$Status=="Both DEP"))/nrow( Matrix_basin)
100*length(which( Matrix_basin$Status=="Both IND"))/nrow( Matrix_basin)
100*length(which( Matrix_basin$Status=="Not equal"))/nrow( Matrix_basin)


 Matrix_basin$Syn_Diff<- Matrix_basin$Syn_12- Matrix_basin$Syn_21
 
 Matrix_basin$KT_Diff<- Matrix_basin$POT_KT_12- Matrix_basin$POT_KT_21
 
 
Summary<-data.frame(matrix(NA,2,3))
colnames(Summary)<-c("Both Dependent","Both Independent","Not symmetric")
rownames(Summary)<-c("Number Couples", "Percentages [%]")

Summary$`Both Dependent`[1]<-length(which( Matrix_basin$Status=="Both DEP"))
Summary$`Both Independent`[1]<-length(which( Matrix_basin$Status=="Both IND"))
Summary$`Not symmetric`[1]<-length(which( Matrix_basin$Status=="Not equal"))

Summary$`Both Dependent`[2]<-round(100*length(which( Matrix_basin$Status=="Both DEP"))/nrow( Matrix_basin),2)
Summary$`Both Independent`[2]<-round(100*length(which( Matrix_basin$Status=="Both IND"))/nrow( Matrix_basin),2)
Summary$`Not symmetric`[2]<-round(100*length(which( Matrix_basin$Status=="Not equal"))/nrow( Matrix_basin),2)



####################################################################################################
########################################## PLOT ####################################################
####################################################################################################

####################################### Differences ################################################  


setwd(Path_results)

########## MAP: real FLOW direction #######################

Basin_map<-map0$geometry %>% 
  ggplot() +
  geom_sf() +
  theme_bw()


Real_flow<-map0$geometry %>% 
  ggplot() +
  geom_sf() +
  theme_bw()+ geom_segment(
    aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2),
    data =  Matrix_basin[which( Matrix_basin$Direction==12),],
    arrow = arrow(length = unit(0.50, "cm")), size = 0.8)+geom_point(aes(EAST, NORTH),data=Coordinates_NE, col="red")+ geom_segment(
      aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1),
      data =  Matrix_basin[which( Matrix_basin$Direction==21),],
      arrow = arrow(length = unit(0.50, "cm")), size = 0.8, col="blue")

ggsave("Real_flow.jpeg", units="in",dpi=400, height=7,width =10)


####### Differences of KT upstream and Downstream ######

Matrix_basin<-Matrix_basin %>% 
  rowwise() %>% 
  mutate(Mean_KT = mean(c(POT_KT_12, POT_KT_21), na.rm = TRUE))


min_diff<-min(Matrix_basin$Difference)
max_diff<- max(Matrix_basin$Difference)

Matrix_basin$Difference<- Matrix_basin$POT_KT_12 - Matrix_basin$POT_KT_21

th_diff<-0.01

Matrix_basin$max_pos<-ifelse(Matrix_basin$Difference>0, 1, ifelse(Matrix_basin$Difference<0, 2, ifelse(Matrix_basin$Difference<= th_diff && Matrix_basin$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 


Matrix_basin$max_pos[eq_zero]<-0

Matrix_basin$Abs_diff<-abs(Matrix_basin$Difference)
Matrix_basin$Group_diff<-cut(Matrix_basin$Abs_diff, c(-0.01,0.01, 0.05, 0.1, 0.19), labels = FALSE)


## Choose matrix ##
####################### Plot Diff Map ############################################

Matrix0<-Matrix_basin


Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_diff ),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff ),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)), 
                        limits = c(min_diff, max_diff))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff ),
    data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))




Basin_map+
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, colour= Abs_diff ),
    data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))

Basin_map+
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, colour= Abs_diff ),
    data = Matrix0[which(Matrix0$max_pos==1),])+
  ggtitle(paste("Directional plot of Differences"))





#####################################################################################


##################### SCATTERPLOT #############################

## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
# +xlim(0,1)+ylim(0,1)

ggsave("Upstream- Downstream direction KT Scatt.jpeg", units="in",dpi=400, height=6,width =8)


## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM and zero direction


ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), shape=15, size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
#+xlim(0,1)+ylim(0,1)

ggsave("Up- Down and zero direction KT Scatt.jpeg", units="in",dpi=400, height=6,width =7)


## JUST zero direction


ggplot()+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), shape=15, size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
#+xlim(0,1)+ylim(0,1)


ggsave("Zero direction KT Scatt.jpeg", units="in",dpi=400, height=6,width =7)

















  




Matrix0<-Matrix_basin[which(Matrix_basin$Mean_KT>0.3),]


hist(Matrix0$Difference)


Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")), size = 1.6)+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")), size = 1.6)+
  scale_color_gradientn(colours = rev(rainbow(5)), 
                        limits = c(min_diff, max_diff))+ 
  ggtitle(paste("Directional plot of Differences"))








Basin_area<-unique(map0$HA)

#map_single<- map0%>% filter(HA==Basin_area[1])

# ZOOM single basins
Plot_cut_map(Difference_plot,map0[which(map0$HA==Basin_area[1]),],10000)
ggsave(paste0("Real_flow_HA_",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Real_flow_HA_",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

########################################################################################



Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")), size = 1.6)+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")), size = 1.6)+
  scale_color_gradientn(colours = rev(rainbow(5)), 
                        limits = c(min_diff, max_diff))+ 
  ggtitle(paste("Directional plot of Differences"))





Matrix_basin$Diff<- ifelse( Matrix_basin$Direction==12, Matrix_basin$POT_KT_12 - Matrix_basin$POT_KT_21, 
                             ifelse( Matrix_basin$Direction==21, Matrix_basin$POT_KT_21 - Matrix_basin$POT_KT_12,NA))


Matrix0<-Matrix_basin








 Matrix_basin$IS_KT<- ifelse( Matrix_basin$Direction==12, Matrix_basin$POT_KT_12> Matrix_basin$POT_KT_21, 
                              ifelse( Matrix_basin$Direction==21, Matrix_basin$POT_KT_21> Matrix_basin$POT_KT_12,NA))
 
 Matrix_basin$IS_SYN<- ifelse( Matrix_basin$Direction==12, Matrix_basin$SYN_12> Matrix_basin$SYN_21, 
                               ifelse( Matrix_basin$Direction==21, Matrix_basin$SYN_21> Matrix_basin$SYN_12,NA))

 Matrix_basin$Distance<- Matrix_basin$Distance/1000



Basin_area<-unique(map0$HA)

#map_single<- map0%>% filter(HA==Basin_area[1])

# ZOOM single basins
Plot_cut_map(Real_flow,map0[which(map0$HA==Basin_area[1]),],10000)
ggsave(paste0("Real_flow_HA_",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Real_flow,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Real_flow_HA_",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

########################################################################################

# Directional Plots: Max Kendall's Tau

Max_plot_KT<-Direct_max_plot_v2( Matrix_basin,0.3, "Kendall's Tau",Basin_map)

Plot_KTALL<-Max_plot_KT[[1]]
Plot_KTmore<-Max_plot_KT[[2]]

ggsave("KT_Directional_plot_ALL.jpeg",Plot_KTALL, units="in",dpi=400, height=7,width =10)
ggsave("KT_Directional_plot_more than Th.jpeg", Plot_KTmore, units="in",dpi=400, height=7,width =10)

## Zoom directional plot

Plot_cut_map(Plot_KTmore,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("KT_Directional_HA_",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Plot_KTmore,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("KT_Directional_HA_",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


############################### Directional Plot: KT >0.5 ##########################################

Max_plot_KT2<-Direct_max_plot_v2( Matrix_basin,0.5, "Kendall's Tau",Basin_map)
Plot_KTmore2<-Max_plot_KT2[[2]]
ggsave("KT_Directional_plot_more than Th_higher.jpeg", Plot_KTmore2, units="in",dpi=400, height=7,width =10)


Max_plot_SYN<-Direct_max_plot_v2( Matrix_basin,0.5, "Synchrony",Basin_map)

Plot_SYNALL<-Max_plot_SYN[[1]]
Plot_SYNmore<-Max_plot_SYN[[2]]


ggsave("SYN_Directional_plot_ALL.jpeg",Plot_SYNALL, units="in",dpi=400, height=7,width =10)
ggsave("SYN_Directional_plot_more than Th.jpeg",Plot_SYNmore, units="in",dpi=400, height=7,width =10)


#Plot_cut_map(Plot_KTmore2,map0[which(map0$HA==Basin_area[1]),],20000)

Max_plot_SYN2<-Direct_max_plot( Matrix_basin,0.7, "Synchrony",Basin_map)
Plot_SYNmore2<-Max_plot_SYN2[[2]]
ggsave("SYN_Directional_plot_more than Th_higher.jpeg",Plot_SYNmore2, units="in",dpi=400, height=7,width =10)


######################### DEPENDENT AND INDEPENDENT ###############################

# INDEPENDENT

Basin_map+ geom_segment(aes(x=Easting_1, y=Northing_1, xend = Easting_2, yend = Northing_2, col=Status), size=0.8,data= Matrix_basin)

Basin_map+ geom_segment(aes(x=Easting_1, y=Northing_1, xend = Easting_2, yend = Northing_2, col=Status),
                        size=0.8,data= Matrix_basin[which(Matrix_basin$Status=="Both IND"),])+
          ggtitle("Independent couples")

ggsave("INDEPENDENT COUPLES.jpeg", units="in",dpi=400, height=7,width =10)


Matrix_basin<-Matrix_basin %>% 
  rowwise() %>% 
  mutate(Mean_KT = mean(c(POT_KT_12, POT_KT_21), na.rm = TRUE))

# DEPENDENT 

DEP_couple<-Basin_map+ geom_segment(aes(x=Easting_1, y=Northing_1, xend = Easting_2, yend = Northing_2, col=Mean_KT),
                        size=0.8,data= Matrix_basin[which(Matrix_basin$Status=="Both DEP"),])+
  ggtitle("Dependent couples")+
  scale_color_gradientn(colours = rev(rainbow(5)))

ggsave("DEPENDENT COUPLES.jpeg", units="in",dpi=400, height=7,width =10)

# DEPENDENT: ZOOM on sub basins

Plot_cut_map(DEP_couple,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("DEPENDENT COUPLES_HA",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(DEP_couple,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("DEPENDENT COUPLES_HA",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

# NOT EQUAL couples

NOT_EQ_couple<-Basin_map+ geom_segment(aes(x=Easting_1, y=Northing_1, xend = Easting_2, yend = Northing_2, col=Median_KT),
                                    size=0.8,data= Matrix_basin[which(Matrix_basin$Status=="Not equal"),])+
  ggtitle("Not equal couples")+
  scale_color_gradientn(colours = rev(rainbow(5)))

ggsave("NOT EQUAL COUPLES.jpeg", units="in",dpi=400, height=7,width =10)

Plot_cut_map(NOT_EQ_couple,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("NOT EQUAL COUPLES_HA",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(NOT_EQ_couple,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("NOT EQUAL COUPLES_HA",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


# DEPENDENT in KT>0.3

DEP_couple_higher<-Basin_map+ geom_segment(aes(x=Easting_1, y=Northing_1, xend = Easting_2, yend = Northing_2, col=Median_KT),
                                    size=0.8,data= Matrix_basin[which(Matrix_basin$Median_KT>=0.3),])+
  ggtitle("Dependent couples with median KT more than 0.3")+
  scale_color_gradientn(colours = rev(rainbow(5)))

ggsave("DEPENDENT COUPLES_ median KT03.jpeg", units="in",dpi=400, height=7,width =10)





## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM and zero direction
# COL: Direction

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(Direction)), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Direction)), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Direction)), shape=15, size=1.8,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
#+xlim(0,1)+ylim(0,1)


ggsave("ALL DIRECT KT Scatt.jpeg", units="in",dpi=400, height=6,width =7)

# COL: Status

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(Status)), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Status)), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Status)), shape=15, size=1.8,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
#+xlim(0,1)+ylim(0,1)


ggsave("Status Scatterplot.jpeg", units="in",dpi=400, height=6,width =7)



############ TRY SEBASTIAN PLOT IDEA ####################


## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM and zero direction


ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(Direction)), size=3,data= Matrix_basin)+
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Direction)), size=3,data= Matrix_basin)


ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), shape=15, size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
#+xlim(0,1)+ylim(0,1)



################ CHECK ERRORS #########################################


Matrix_basin1<-Matrix_basin[which(Matrix_basin$IS_KT==TRUE),]

Max_plot_KT<-Direct_max_plot_v2( Matrix_basin1,0.3, "Kendall's Tau",Basin_map)

Plot_KTALL<-Max_plot_KT[[1]]
Plot_KTmore<-Max_plot_KT[[2]]


Plot_cut_map(Plot_KTmore,map0[which(map0$HA==Basin_area[1]),],20000)


ggsave("ERRORS_majKT.jpeg", units="in",dpi=400, height=6,width =7)


Matrix_basin<-Matrix_basin %>% 
  rowwise() %>% 
  mutate(DIFF_KT = diff(c(POT_KT_12, POT_KT_21), na.rm = TRUE))


Code_er<-Matrix_basin1$CODE
List_coup_Error<-List_couple[Code_er]



ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=Distance), size=3,data= Matrix_basin1[which( Matrix_basin1$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin1[which( Matrix_basin1$Direction==21),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")

ggsave("Scatterplot KT_errors.jpeg", units="in",dpi=400, height=7,width =10)



Matrix_basin$KTmajor<-ifelse(Matrix_basin$POT_KT_12>Matrix_basin$POT_KT_21,12,
                             ifelse(Matrix_basin$POT_KT_12<Matrix_basin$POT_KT_21,21,0))

Matrix_basin$KTmajor<-ifelse(Matrix_basin$POT_KT_12>Matrix_basin$POT_KT_21,12,21)





########################################################################################

ggplot()+ geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)

ggsave("Scatterplot Max_KT.jpeg", units="in",dpi=400, height=7,width =10)


# DIRECTION EQUAL ZERO
ggplot()+ geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+ 
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ ggtitle("Scatterplot Direction = 0")

ggsave("Scatterplot direction 0.jpeg", units="in",dpi=400, height=7,width =10)


#Synchrony measures

ggplot()+ geom_point(aes(x=Syn_12, y=Syn_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=Syn_21, y=Syn_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ ggtitle("Syncrony difference")


ggsave("Scatterplot Syncrony difference.jpeg", units="in",dpi=400, height=7,width =10)


############# SUMMARY PLOTS #############################

# Histogram Difference of KT
jpeg(file="Hist_diff.jpeg")
hist( Matrix_basin$Diff, main="KT differences")
dev.off()

# BARPLOT ERRORS
jpeg(file="Error_Barplot_KT.jpeg")
barplot(prop.table(table( Matrix_basin$IS_KT)))
dev.off()

# Save Summary Table

png(paste0("Summary_",pth,".png"))
pS<-tableGrob(Summary)
grid.arrange(pS)
dev.off()


#Boxplot comparison

png("Boxplot_comparison.png")
boxplot(DAILY_POT_08$POT_KT_12,DAILY_POT_09$POT_KT_12,DAILY_POT_08$POT_KT_21,DAILY_POT_09$POT_KT_21, 
        names=c("TH 08 KT12","TH 09 KT12","TH 08 KT21","TH 09 KT21"))
dev.off()


print(paste0 ("END analysis for DAILY POT for PTH",pth))

# Save Workspace



#th 0.8

if(pth==0.8)
{
   Matrix_basin_DAILY_08_proc<- Matrix_basin
  save( Matrix_basin_DAILY_08_proc,file=paste0(" Matrix_basin_TH_",pth,".RData"))
  save.image(paste0("Code_direction_workspace_TH",pth,".RData"))

}  else
  
  #th 0.9
  if(pth==0.9)
  { 
   Matrix_basin_DAILY_09_proc<- Matrix_basin
  save( Matrix_basin_DAILY_09_proc,file=paste0(" Matrix_basin_TH_",pth,".RData"))
  save.image(paste0("Code_direction_workspace_TH",pth,".RData"))
  }
}

path11<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/")

save( Matrix_basin_DAILY_09_proc, Matrix_basin_DAILY_08_proc,file=paste0(path11,"Processed_ Matrix_basin_VARIOUS_TH.RData"))
save.image(paste0(path11,"Code_direction_workspace_VARIOUS_TH.RData"))

############### other stuff ###############
############## 15 03 22 #######################

Sel<-  Matrix_basin_DAILY_08_proc
Sel<-  Matrix_basin_DAILY_09_proc


ggplot()+ geom_point(aes(x=POT_KT_12, y=POT_KT_21, colour=factor(Direction)), 
                     size=3,data=Sel[which(sel)])+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)

ggplot()+ geom_point(aes(x=POT_KT_12, y=POT_KT_21, shape=factor(Direction), size=factor(Direction), colour=Distance), 
                     size=3,data=Sel)+xlim(0,1)+ylim(0,1)+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)


 Matrix_basin<- Matrix_basin_DAILY_09_proc

# SYNCHRONY #


ggplot()+ geom_point(aes(x=Syn_21, y=Syn_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=Syn_12, y=Syn_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+ 
  geom_point(aes(x=Syn_12, y=Syn_21, col=Distance), shape=15, size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("SYN Downstream -> Upstream")+
  ylab("SYN Upstream -> Downstream")


ggsave("SYN Upstream- Downstream direction .jpeg", units="in",dpi=400, height=6,width =7)





ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(Direction)), size=3,data= Matrix_basin)+
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(Direction)), size=3,data= Matrix_basin)+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")

+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), shape=15, size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")
























# DIRECTION EQUAL ZERO
ggplot()+ geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==0),])+ 
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ ggtitle("Scatterplot Direction = 0")

ggsave("Scatterplot direction 0.jpeg", units="in",dpi=400, height=7,width =10)


#Synchrony measures

ggplot()+ geom_point(aes(x=Syn_12, y=Syn_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
  geom_point(aes(x=Syn_21, y=Syn_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlim(0,1)+ylim(0,1)+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ ggtitle("Syncrony difference")
























# #Pvalue measures
# 
# ggplot()+ geom_point(aes(x=POT_pvalue_12, y=POT_pvalue_21, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==12),])+ 
#   geom_point(aes(x=POT_pvalue_21, y=POT_pvalue_12, col=Distance), size=3,data= Matrix_basin[which( Matrix_basin$Direction==21),])+
#   scale_color_gradientn(colours = rev(rainbow(5)))+
#   theme_classic()+ geom_abline(intercept = 0, slope = 1)


############## end Plot section #################################################################

#################################################################################################




#Plot
X<- Matrix_basin[which( Matrix_basin$Direction==12),]
Y<- Matrix_basin[which( Matrix_basin$Direction==21),]


plot(map0$geometry)
points(Coordinates_NE$EAST,Coordinates_NE$NORTH,col="red",pch=16)
arrows(X$Easting_1, X$Northing_1, X$Easting_2,X$Northing_2, col="red")
arrows(Y$Easting_2, Y$Northing_2, Y$Easting_1,Y$Northing_1, col="blue")

map<-map0[i,]
plot(map$geometry)
points(Coordinates_NE$EAST,Coordinates_NE$NORTH,col="red",pch=16)

X<- Matrix_basin[which( Matrix_basin$Direction==12),]
Y<- Matrix_basin[which( Matrix_basin$Direction==21),]

plot(Y$POT_KT_21,Y$POT_KT_12)
lines(0:1, 0:1, type="l")
points(X$POT_KT_12,X$POT_KT_21)



plot(X$POT_KT_12,X$POT_KT_21)
lines(0:1, 0:1, type="l")

################################### Trying to understand ##########################

YY<- Matrix_basin[which( Matrix_basin$IS==FALSE),]

#Plot
X<-YY[which(YY$Direction==12),]
Y<-YY[which(YY$Direction==21),]


plot(map0$geometry)
points(Coordinates_NE$EAST,Coordinates_NE$NORTH,col="red",pch=16)
arrows(X$Easting_1, X$Northing_1, X$Easting_2,X$Northing_2, col="red")
arrows(Y$Easting_2, Y$Northing_2, Y$Easting_1,Y$Northing_1, col="blue")


#############################################################################


