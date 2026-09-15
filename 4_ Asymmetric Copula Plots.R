# Plot finali Simulazioni copula asimmmetrica

######## Clear Workspace ################# 

rm(list = ls())

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
library("evd")
library(ggplot2)
library(matrixStats)
library(plotly)
library(fExtremes)
library(grDevices)
library(plot3D)
library(plot3Drgl)
library(rmutil)
library(akima)
library(latex2exp)

# PLOT FOR PAPER # 

####################################################################################################

load("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Sim Copula/MultipleSimulations.RData")

setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Sim Copula")


# All simulations plot

v1<-ggplot(data=M1,aes(x=beta2,y=beta1,col=Max_KT,size=Diff_KT))+ geom_point()+ 
  theme_classic() +scale_color_gradientn(colours = rainbow(5),
                                         limits=c(0,1))+ 
  xlim(c(0,1))+ylim(c(0,1))+ 
  facet_wrap(~ Theta)

#+  ggtitle(paste0("Variation of KT differences and maximum value of KT for each 1/alpha"))


v1+guides(col=guide_colorbar(title="Max KT"), size=guide_legend(title="Diff KT"))+
  xlab(TeX('$\\beta_2'))+ylab(TeX('$\\beta_1'))

ggsave(paste0("TH ",q,"-Asy_copula-allcomb.jpeg"), units="in",dpi=400, height=5.3,width =8)

#################################################################################################

# Upstream - Downstream plot


M1$Diff_beta<-abs(M1$beta1-M1$beta2)
M1$Diff_KT<-abs(M1$KT_x-M1$KT_y)
M1$Direction<-ifelse(M1$beta2>M1$beta1, 12, 21)


M1$Max_KT<-rowMaxs(as.matrix(M1[,c("KT_x","KT_y")]))

v2<-ggplot()+ geom_point(aes(x=KT_x,y=KT_y,col=Diff_beta),data=M1[which(M1$Direction==12),], size=2 )+
  geom_point(aes(x=KT_y,y=KT_x,col=Diff_beta),data=M1[which(M1$Direction==21),], size=2 )+
  scale_color_gradientn(colours = rainbow(5), limits=c(0,1))+theme_classic()


v2+guides(col=guide_colorbar(title=TeX('| $\\beta_1$ - $\\beta_2$ |')))+
  xlab(TeX("Asymmetric tail Kendall's $\\tau_{XY}$"))+  ylab(TeX("Asymmetric tail Kendall's $\\tau_{YX}$"))

ggsave(paste0("TH 0.98 -dirplot_theta.jpeg"), units="in",dpi=400, height=5.3,width =8)        
#################################################################################################




