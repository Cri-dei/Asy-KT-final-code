
######## Clear Workspace ################# 

rm(list = ls())

############## FINAL CODE PLOT FOR PAPER ########################
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
library(ggpubr)
library(raster)
library(latex2exp)

path_function<- c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/Functions/")
path_GIS<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/STSM/GIS/")

source(paste0(path_function,"Direct_max_plot_v2.R"))
source(paste0(path_function,"UKcoord_to_LatLong.R"))
source(paste0(path_function,"LatLong_to_NorthEast.R"))
source(paste0(path_function,"Plot_cut_map.R"))



#CHoose PTh

pth<-0.98

load(paste0("D:/Results_BasinSelected/TH",pth,"/POT_matrix_processed.Rdata"))

# Shapefile subcatchment selected
map0 = read_sf(paste0(path_GIS,"Subcatchment_selected.shp"))



TH_MKT<-0.2
th_diff<-0.05


Sub<-Matrix_basin_proc


stat<-c(54102,54019,54111,54004,54114,54106,54007,54907,54024,54002,54036)

Sub$HA_1[which(Sub$ID_Station_1%in%stat)]<-57
Sub$HA_2[which(Sub$ID_Station_2%in%stat)]<-57

Sub$HA<-rowSums(Sub[,c(20,21)])

map0$HA[which(map0$station_1%in%stat)]<-57
Sub<-Sub[-which(Sub$HA==108 | Sub$HA==110),]



setwd("D:/Results_BasinSelected/TH0.98")


###################### Plot 1: Significant Kendall's Tau #####################################

ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, col= Type),shape=19,
  data = Sub)+
 # geom_abline()+
 # geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash")+ 
 # geom_abline(intercept=-th_diff, linetype="longdash", size=0.5)+   
#  geom_abline(intercept=th_diff, linetype="longdash", size=0.5) +
  theme_classic()+xlim(c(-0.35,1))+ylim(c(-0.35,1))+ 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+ 
  xlab("Tail Kendall's Tau 'U+03C4' XY")+ ylab("Tail Kendall's Tau YX")


ggsave("KT_selectedbasin.jpeg", units="in",dpi=400, height=5.3,width =8)


###################### Plot 2: Upstream and downstream KT #####################################

## KENDALL TAU UPSTREAM vs KENDALL TAU DOWNSTREAM

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(HA)), size=3,data= Sub[which( Sub$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(HA)), size=3,data= Sub[which( Sub$Direction==21),])+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")

ggsave("Upstream- Downstream direction KT Scatt_ALLmeno108.jpeg", units="in",dpi=400, height=6,width =8)


######################### Plot 3: Zoom map basin with the dam ##################################

Sub<-Sub[which(Sub$HA==114),]
Sub<-Sub[which(Sub$Type=="Same Basin Hyd Connected"),]

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(HA)), size=3,data= Sub2[which( Sub2$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(HA)), size=3,data= Sub2[which( Sub2$Direction==21),])+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")


Basin_map<-map0$geometry %>% 
  ggplot() +
  geom_sf() +
  theme_bw()+ 
  theme(panel.background = element_rect(fill = "white"))

th_diff<-0.01


Sub$Difference<- Sub$POT_KT_12 - Sub$POT_KT_21


Sub$max_pos<-ifelse(Sub$Difference>0, 1, ifelse(Sub$Difference<0, 2, ifelse(Sub$Difference<= th_diff && Sub$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 

Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Sub[which(Sub$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data =  Sub[which(Sub$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlab("Easting 1")+ylab("Northing 1")


Diff<-Plot_cut_map(Difference_plot,map0[which(map0$HA==57),],0)



Real_flow<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Sub[which(Sub$Direction==12),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data =  Sub[which(Sub$Direction==21),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+xlab("Easting 2")+ylab("Northing 2")


Realf<-Plot_cut_map(Real_flow,map0[which(map0$HA==57),],0)

ggarrange(Realf,Diff,ncol=2,nrow=1,
          common.legend = TRUE, 
          legend = "right",labels = c("a)","b)"))


ggsave("Map comparison_ basin 57.jpeg", units="in",dpi=400, height=3,width =8)

#################################################################################################################

































############################old ####################




Sub1<-Sub[which(Sub$HA==114),]
Sub1<-Sub1[which(Sub1$Type=="Same Basin Hyd Connected"),]

ggplot()+ geom_point(aes(x=POT_KT_21, y=POT_KT_12, col=factor(HA)), size=3,data= Sub1[which( Sub1$Direction==12),])+ 
  geom_point(aes(x=POT_KT_12, y=POT_KT_21, col=factor(HA)), size=3,data= Sub1[which( Sub1$Direction==21),])+
  theme_classic()+ geom_abline(intercept = 0, slope = 1)+ xlab("Kendall's Tau Downstream -> Upstream")+
  ylab("Kendall's Tau Upstream -> Downstream")


Basin_map<-map0$geometry %>% 
  ggplot() +
  geom_sf() +
  theme_bw()

th_diff<-0.01


Sub$Difference<- Sub1$POT_KT_12 - Sub1$POT_KT_21


Sub1$max_pos<-ifelse(Sub1$Difference>0, 1, ifelse(Sub1$Difference<0, 2, ifelse(Sub1$Difference<= th_diff && Sub1$Difference>= -th_diff, 0,0)))

eq_zero<-which(Matrix_basin$Difference<= th_diff & Matrix_basin$Difference>= -th_diff) 

Difference_plot<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Sub1[which(Sub1$max_pos==1),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data =  Sub1[which(Sub1$max_pos==2),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+ 
  ggtitle(paste("Directional plot of Differences"))

Diff<-Plot_cut_map(Difference_plot,map0[which(map0$HA==57),],0)



Real_flow<-Basin_map+ geom_segment(
  aes(x = Easting_1, y = Northing_1, xend = Easting_2, yend = Northing_2,color = Difference),
  data = Sub1[which(Sub1$Direction==12),],
  arrow = arrow(length = unit(0.30, "cm")))+ 
  geom_segment(
    aes(x = Easting_2, y = Northing_2, xend = Easting_1, yend = Northing_1,color = Difference),
    data =  Sub1[which(Sub1$Direction==21),],
    arrow = arrow(length = unit(0.30, "cm")))+
  scale_color_gradientn(colours = rev(rainbow(5)))+ 
  ggtitle(paste("Real Flow"))



Realf<-Plot_cut_map(Real_flow,map0[which(map0$HA==57),],0)

ggarrange(Diff,Realf,ncol=2,nrow=1,
          common.legend = TRUE, 
          legend = "right",labels = c("a","b"))

#################################################################################################################





















Sub1<-Sub[which(Sub$Type=="Same Basin Hyd Connected"),]

ggplot() +geom_point(
  aes(x = POT_KT_12, y = POT_KT_21, col= Type),shape=19,
  data = Sub1)+
  ggtitle("Significant Kendall's Tau")+ geom_abline()+
  geom_vline(xintercept = TH_MKT, linetype="longdash")+ geom_hline(yintercept = TH_MKT, linetype="longdash")+ 
  geom_abline(intercept=-th_diff, linetype="longdash", size=0.5)+   
  geom_abline(intercept=th_diff, linetype="longdash", size=0.5) +
  theme_classic()+xlim(c(-0.35,1))+ylim(c(-0.35,1))+ 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+ xlab("Tail Kendall's Tau 12")+ ylab("Tail Kendall's Tau 21")



