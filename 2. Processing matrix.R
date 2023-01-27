
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



#load("~/PROJECTS 2021/QQ_POT_mixed/Results/Differences/Workspace_init.RData")
load("D:/Results_BasinSelected/Processed_matrix.RData")

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

Matrix_basin0<-Matrix_basin

Matrix_basin<-Matrix_basin_2

Matrix_basin<-Matrix_basin %>% 
  rowwise() %>% 
  mutate(Mean_KT = mean(c(POT_KT_12, POT_KT_21), na.rm = TRUE))


Matrix_basin$Difference<- Matrix_basin$POT_KT_12 - Matrix_basin$POT_KT_21

min_diff<-min(Matrix_basin$Difference)
max_diff<- max(Matrix_basin$Difference)

th_diff<-0.01

Matrix_basin$max_pos<-ifelse(Matrix_basin$Difference>0, 1, ifelse(Matrix_basin$Difference<0, 2, ifelse(Matrix_basin$Difference<= th_diff && Matrix_basin$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 


Matrix_basin$max_pos[eq_zero]<-0

Matrix_basin$Abs_diff<-abs(Matrix_basin$Difference)
Matrix_basin$Group_diff<-cut(Matrix_basin$Abs_diff, c(-0.01,0.01, 0.05, 0.1, 0.19), labels = FALSE)





# Merged  Matrix_basin and 1277
map0 = read_sf(paste0(path_GIS,"Merged_Spey_1277.shp"))



############ Investigate Differences ###############################

####### Differences of KT upstream and Downstream ######

#Matrix_basin<-DAILY_POT_09

Matrix_basin<-POT_matrix

Matrix_basin<-Matrix_basin %>% 
  rowwise() %>% 
  mutate(Mean_KT = mean(c(POT_KT_12, POT_KT_21), na.rm = TRUE))

Matrix_basin$Difference<- Matrix_basin$POT_KT_12 - Matrix_basin$POT_KT_21

min_diff<-min(Matrix_basin$Difference)
max_diff<- max(Matrix_basin$Difference)


Matrix_basin$Abs_diff<-abs(Matrix_basin$Difference)
Matrix_basin$Group_diff<-cut(Matrix_basin$Abs_diff, c(-0.01,0.01, 0.05, 0.1, 0.19), labels = FALSE)


############# Threshold ##########################

th_diff<-0.07

#Max pos =1 12->21, maxpos=2 21->12

Matrix_basin$max_pos<-ifelse(Matrix_basin$Difference>0, 1, ifelse(Matrix_basin$Difference<0, 2, ifelse(Matrix_basin$Difference<= th_diff && Matrix_basin$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 


Matrix_basin$max_pos[eq_zero]<-0


## Choose matrix ##



############################################################################################################
################################## Diff and Kt mean value ################################


Matrix0<-Matrix_basin

Matrix0<-Matrix_basin[which(Matrix_basin$Mean_KT>0),]


## ALL
Difference_plotALL<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|>",th_diff))

ggsave(paste0("Diff_directions.jpeg"), units="in",dpi=400, height=7,width =10)


# Dir 0

Difference_plot0C<-Basin_map+geom_segment(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff))
 
ggsave(paste0("Zero directions.jpeg"), units="in",dpi=400, height=7,width =10)


############################ Plot cut map ####################################################

path_res1<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Results/Differences/RESULTS TH09/")

dir.create(paste0(path_res1,th_diff))

setwd(paste0(path_res1,th_diff))

#setwd(path_res)

# Diff ALL

Plot_cut_map(Difference_plotALL,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_directions_B",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)

Plot_cut_map(Difference_plotALL,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_directions_B",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

#Diff 0
Plot_cut_map(Difference_plot0C,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos0_B",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot0C,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos0_B",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

## FInal Plot diffenrece

#########################################################################################
######################### TH ON MEDIAN KT ################################################
#########################################################################################

TH_MKT<-0.1


Matrix0<-Matrix_basin[which(Matrix_basin$POT_KT_12>=TH_MKT & Matrix_basin$POT_KT_21>=TH_MKT),]

Matrix01<-Matrix0[which(Matrix0$max_pos==1 | Matrix0$max_pos==2),]

# FINAL PLOT DIFFERENCE WORKING WITH LEGEND 11 04 2022

Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21),col="red",shape=19,
  data = Matrix01)+
  ggtitle("Significant Kendall's Tau")+ geom_abline()+
  geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash")+ 
  geom_abline(intercept=-th_diff, linetype="longdash", size=0.5)+   geom_abline(intercept=th_diff, linetype="longdash", size=0.5)+ 
  xlim(0,1)+ylim(0,1)+geom_point(
    aes(x = POT_KT_12, y = POT_KT_21), col="blue", shape=4,
    data = Matrix0[which(Matrix0$max_pos==0),])+
  geom_point( aes(x = POT_KT_12, y = POT_KT_21, col="black"),
              data = Matrix_basin[which(Matrix_basin$POT_KT_12<TH_MKT | Matrix_basin$POT_KT_21<TH_MKT),], shape=4)+ 
  labs(x="Kendall's Tau 12", y="Kendall's Tau 21", colour="Legend") + 
  scale_color_manual(values = c( "Significant and asymmetric" = "red","Significant and symmetric" = "blue", "Not significant" = "black"))+
  theme_classic() +
  guides(shape ="none", colour = guide_legend(override.aes = list(shape = c(19,4, 4))))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(caption = paste("Kendall's Tau threshold = ",TH_MKT, "Difference threshold = ", th_diff))



ggsave("Significative Kendall's Tau.jpeg", units="in",dpi=400, height=7,width =10)


ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21,size=Distance,col=Direction),shape=19,
  data = Matrix01)

+
  geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash") +
  geom_area(aes(fill="blue"))


Diff1


############# Plot Diff ##############

colors <- c("Significant and symmetric" = "blue", "Significant and asymmetric" = "red", "Not significant" = "black")

Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21),col="red",shape=19,
  data = Matrix01)+
  ggtitle("Significant Kendall's Tau")+ geom_abline()+
  geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash")+ 
  geom_abline(intercept=-th_diff, linetype="longdash", size=0.5)+   geom_abline(intercept=th_diff, linetype="longdash", size=0.5)+ 
  xlim(0,1)+ylim(0,1)+geom_point(
    aes(x = POT_KT_12, y = POT_KT_21), col="blue", shape=4,
    data = Matrix0[which(Matrix0$max_pos==0),])+
  geom_point( aes(x = POT_KT_12, y = POT_KT_21, col="black"),
      data = Matrix_basin[which(Matrix_basin$POT_KT_12<TH_MKT | Matrix_basin$POT_KT_21<TH_MKT),], shape=4)+ 
  labs(x="Kendall's Tau 12", y="Kendall's Tau 21", colour="Legend") + 
  scale_color_manual(values = c( "Significant and asymmetric" = "red","Significant and symmetric" = "blue", "Not significant" = "black"))+
  theme_classic(plot.title = element_text(hjust = 0.5)) +
  guides(shape ="none", colour = guide_legend(override.aes = list(shape = c(19,4, 4))))




Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, size= Abs_diff, colour= Mean_KT),
  data = Matrix01)+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle("KT12 - KT21")+theme_classic()+ 
  geom_vline(xintercept = TH_MKT)+ geom_hline(yintercept = TH_MKT)+ geom_abline()+ 
  geom_abline(intercept=-th_diff)+   geom_abline(intercept=th_diff)+ xlim(0,1)+ylim(0,1)



Diff1+geom_point(
  aes(x = POT_KT_12, y = POT_KT_21),
  data = Matrix_basin, colour= "black")


#########################################

## ALL ##

Difference_plotALL1<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|>",th_diff,"And Mean KT>",TH_MKT))

ggsave(paste0("2_Diff_directions And Mean KT more",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plotALL1,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("2_Diff_directions_B",Basin_area[1],"And Mean KT",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)

Plot_cut_map(Difference_plotALL1,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("2_Diff_directions_B",Basin_area[2],"And Mean KT",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)


# Dir 0

Difference_plot0C1<-Basin_map+geom_segment(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff,"And Mean KT>",TH_MKT))

ggsave(paste0("2_Zero directions And Mean KT more",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)

#Diff 0
Plot_cut_map(Difference_plot0C1,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("2_Diff_pos0_B",Basin_area[1],"And Mean KT",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot0C1,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("2_Diff_pos0_B",Basin_area[2],"And Mean KT",TH_MKT,".jpeg"), units="in",dpi=400, height=7,width =10)

###################################################################################################################

######## Plot differences #########

Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, size= Abs_diff, colour= Mean_KT),
  data = Matrix_basin)+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle("KT12 - KT21")+theme_classic()+ 
  geom_vline(xintercept = 0.1)+ geom_hline(yintercept = 0.1)+ geom_abline()+ 
  geom_abline(intercept=-0.07)+   geom_abline(intercept=0.07)

Matb1<-Matrix_basin0[which(Matrix_basin0$max_pos==1 | Matrix_basin0$max_pos==2),]

###################################################################################################################




Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, size= Abs_diff, colour= Mean_KT),
  data = Matrix_basin)+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff,"And Mean KT>",TH_MKT))+theme_classic()+ 
  geom_vline(xintercept = 0.1)+ geom_hline(yintercept = 0.1)+ geom_abline()+ 
  geom_abline(intercept=-0.07)+   geom_abline(intercept=0.07)


Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, size= Abs_diff, colour= Mean_KT),
  data = Matrix_basin0)+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff,"And Mean KT>",TH_MKT))+theme_classic()+ 
  geom_vline(xintercept = 0.1)+ geom_hline(yintercept = 0.1)+ geom_abline()+ 
  geom_abline(intercept=-0.07)+   geom_abline(intercept=0.07)




Diff1<- ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, size= Abs_diff, colour= Mean_KT),
  data = Matrix_basin)+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff,"And Mean KT>",TH_MKT))+theme_classic()




Diff1<- ggplot() +geom_point(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("|KT12 - KT21|<",th_diff,"And Mean KT>",TH_MKT))

































#Diff 12

Plot_cut_map(Difference_plot12C,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos12_B",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot12C,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos12_B",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


#Diff 21

Plot_cut_map(Difference_plot21C,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos21_B",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot21C,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos21_B",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

################################################################################################
################################################################################################



# Dir 12

Difference_plot12C<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("Directional Differences"))


# Dir 21

Difference_plot21C<-Basin_map+geom_segment(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff, colour= Mean_KT),
  data = Matrix0[which(Matrix0$max_pos==2),],
  arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+
  ggtitle(paste("Direction 21"))


##################################################################################################










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


Plot_cut_map(Real_flow,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("RealFlow_B",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Real_flow,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("RealFlow_B",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


#####################################################################################################


















################### OLDDDDDDDDDDDDDDD######################################################
####################### Plot Diff Map ############################################

Matrix0<-Matrix_basin

##ALL
Difference_plot0<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_diff ),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff ),
    data = Matrix0[which(Matrix0$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)), 
                        limits = c(min_diff, max_diff))


# Dir 12

Difference_plot12<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2, size= Abs_dif),
  data = Matrix0[which(Matrix0$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))


# Dir 21

Difference_plot21<-Basin_map+geom_segment(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff),
  data = Matrix0[which(Matrix0$max_pos==2),],
  arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)), 
                        limits = c(min_diff, max_diff))

# Dir 0

Difference_plot0<-Basin_map+geom_segment(
  aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1, size= Abs_diff ),
  data = Matrix0[which(Matrix0$max_pos==0),])+
  ggtitle(paste("Directional plot of Differences"))


#################### Plot cut map

path_res<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Results/Differences")

setwd(path_res)

#Diff 0
Plot_cut_map(Difference_plot0,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos0_TH",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot0,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos0_TH",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)

#Diff 12

Plot_cut_map(Difference_plot12,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos12_TH",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot12,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos12_TH",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


#Diff 21

Plot_cut_map(Difference_plot21,map0[which(map0$HA==Basin_area[1]),],20000)
ggsave(paste0("Diff_pos21_TH",Basin_area[1],".jpeg"), units="in",dpi=400, height=7,width =10)


Plot_cut_map(Difference_plot21,map0[which(map0$HA==Basin_area[2]),],20000)
ggsave(paste0("Diff_pos21_TH",Basin_area[2],".jpeg"), units="in",dpi=400, height=7,width =10)


## old
Matrix_plot<-Matrix_basin

Matrix_plot$Type<-NA


Matrix_plot$Type[which(Matrix_plot$POT_KT_12>=TH_MKT & Matrix_plot$POT_KT_21>=TH_MKT)]<-"Significant symmetric"
Matrix_plot$Type[which(Matrix_plot$max_pos==1 | Matrix_plot$max_pos==2)]<-"Significant asymmetric"
Matrix_plot$Type[which(Matrix_plot$POT_KT_12<=TH_MKT | Matrix_plot$POT_KT_21<=TH_MKT)]<-"Not Significant"

ggplot(Matrix_plot, aes(x = POT_KT_12, y = POT_KT_21,col=Type,shape=Type))+geom_point()+
  ggtitle("Significant Kendall's Tau")+ geom_abline()+
  geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash")+ 
  geom_abline(intercept=-th_diff, linetype="longdash", size=0.5)+   geom_abline(intercept=th_diff, linetype="longdash", size=0.5)+ 
  xlim(0,1)+ylim(0,1)+ 
  theme(plot.title = element_text(hjust = 0.5))+ labs(x="Kendall's Tau 12", y="Kendall's Tau 21", colour="Legend")+
  scale_color_manual(labels = c("Significant and symmetric", "Significant and asymmetric","Not significant"), 
                     values = c("red","blue", "black"))+
  theme_classic()
