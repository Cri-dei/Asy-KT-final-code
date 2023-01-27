# Simulations with Asy Copula


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


path<-c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed")

setwd(path)

source("./Code/Functions/Asymmetric_KT_basic.R")


#Function Asy logistic

generate_asym_logistic <- function(theta, beta1, beta2, n)
  rmvevd(n, dep = theta, asy = list(1-beta1, 1-beta2, c(beta1,beta2)), model = c("alog"), d = 2, mar = c(1,1,1))

# Set coefficients

#theta<- 0.2
beta_1<- seq(0.1,0.98,0.1)
beta_2<- seq(0.1,0.98,0.1)
comb1<-data.frame(t(combn(beta_1,2)))
comb2<-cbind(comb1$X2,comb1$X1)
comb3<-cbind(beta_1,beta_2)
colnames(comb2)<-c("X1","X2")
colnames(comb3)<-c("X1","X2")

comb_all<-rbind(comb1,comb2,comb3)


#thetavalue<-c(0.01,0.03,0.05,0.07,0.09,0.3,0.5,0.8,0.98)

thetavalue<-c(0.005,0.2,0.4,0.6,0.8,0.98)


#qvalues<-c(0.95,0.98)

#qvalues<-0.95
  
  
Simul_asy<-list()
###### Create matrix and simulation #############################


setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/Simulation AsyCopula/Multiple Simulation/TH0.98")

#pdf(file ="Plot_combination_beta2_01.pdf")


#for ( qq in 1:length(qvalues))
#{

  M0<-data.frame(matrix(, nrow = 4000, ncol = 13))
  
  colnames(M0)<-c("Threshold","Theta","beta1","beta2", "KT_x", "KT_y","Diff_median","Diff_Q05",
                  "Diff_Q95", "Diff_Q01", "Diff_Q99", 
                  "pvalue KT_x","pvalue KT_y")

q<-0.98
xx<-1

Sample_ex<-list()
Asy_KT_list<-list()
Asy_pvalue_list<-list()

for(ii in 1:length(thetavalue))
{
  theta<-thetavalue[ii]
  
  
for(i in 1:nrow(comb_all))
{
  
beta2<- comb_all[i,1] 
beta1<- comb_all[i,2]

M0$Threshold[xx]<- q
M0$Theta[xx]<-theta
M0$beta1[xx]<-beta1
M0$beta2[xx]<-beta2 


KT_matrix<-data.frame(matrix(NA,500,2))
pvalue_matrix<-data.frame(matrix(NA,500,2))
List_ex<-list()
Difference<-c()
Diff_list<-list()

for (kk in 1:1000)
{ 
  x<-generate_asym_logistic (theta,beta1,beta2,1000)
  List_ex[[kk]]<-x
  Asy_ex <- Asymmetric_KT_basic(x,q,theta,beta1,beta2)
  KT_matrix[kk,1]<-Asy_ex$`KT test 12`$estimate
  KT_matrix[kk,2]<-Asy_ex$`KT test 21`$estimate

  pvalue_matrix[kk,1]<-Asy_ex$`KT test 12`$p.value
  pvalue_matrix[kk,2]<-Asy_ex$`KT test 21`$p.value

  
  Difference[kk]<-Asy_ex$`KT test 12`$estimate-Asy_ex$`KT test 21`$estimate
  
}


names(KT_matrix)<-c("KT12","KT21")
names(pvalue_matrix)<-c("pvalKT12","pvalKT21")

Asy_KT_list[[xx]]<-  KT_matrix
Asy_pvalue_list[[xx]]<-  pvalue_matrix

Sample_ex[[xx]]<-List_ex

Diff_list[[xx]]<-Difference

#if(class(Asy_KT)!="character"){

M0$KT_x[xx]<- round(median(KT_matrix$KT12),4) 
M0$KT_y[xx]<- round(median(KT_matrix$KT21),4) 

M0$Diff_median[xx]<-round(median(Difference),4)

M0$Diff_Q05[xx]<-round(quantile(Difference, 0.05),4)
M0$Diff_Q95[xx]<-round(quantile(Difference, 0.95),4)

M0$Diff_Q01[xx]<-round(quantile(Difference, 0.01),4)
M0$Diff_Q99[xx]<-round(quantile(Difference, 0.99),4)



M0$`pvalue KT_x`[xx]<-  round(median(pvalue_matrix$pvalKT12),4)
M0$`pvalue KT_y`[xx]<- round( median(pvalue_matrix$pvalKT21),4)

#if(class(Asy_KT$`KT test Sym`)!="character"){
  
#M0$Sym_KT[xx]<- round( Asy_KT$'KT test Sym'$estimate,3)   
#M0$pvalue_Sym_KT[xx]<- round(Asy_KT$'KT test Sym'$p.value,4) 
#}
#}

xx<-xx+1
}
}

dev.off()

####################################################################

napos<-which(is.na(M0$Threshold))  
#M1<-M0[complete.cases(M0),]

M1<-M0[-napos,]

M1$Diff_beta<-abs(M1$beta1-M1$beta2)
M1$Diff_KT<-abs(M1$KT_x-M1$KT_y)

M1$Max_KT<-rowMaxs(as.matrix(M1[,c("KT_x","KT_y")]))
M1$Inside<-ifelse(M1$Diff_Q05<0 & M1$Diff_Q95>0, 1,0)

#Simul_asy[[qq]]<-M1

#################################################################################

 # PLOT FOR PAPER # 

setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Sim Copula")


v1<-ggplot(data=M1,aes(x=beta2,y=beta1,col=Max_KT,size=Diff_KT))+ geom_point()+ 
  theme_classic() +scale_color_gradientn(colours = rainbow(5),
                                         limits=c(0,1))+ 
  xlim(c(0,1))+ylim(c(0,1))+
  ggtitle(paste0("Variation of KT differences and maximum value of KT for each 1/alpha"))+ 
  facet_wrap(~ Theta)


v1+guides(col=guide_colorbar(title="Max KT"), size=guide_legend(title="Diff KT"))+xlab("TeX('$\\alpha^\\beta$')")+
  xlab(TeX('$\\beta_2'))+ylab(TeX('$\\beta_1'))

ggsave(paste0("TH ",q,"-Asy_copula-allcomb.jpeg"), units="in",dpi=400, height=5.3,width =8)

########### FINAL VEERSION 30/12/22 #############

v1<-ggplot(data=M1,aes(x=beta1,y=beta2,col=Max_KT,size=Diff_KT))+ geom_point()+ 
  theme_classic() +scale_color_gradientn(colours = rainbow(5),
                                         limits=c(0,1))+ 
  xlim(c(0,1))+ylim(c(0,1))+
  facet_wrap(~ Theta)


v1+guides(col=guide_colorbar(title="Max KT"), size=guide_legend(title="Diff KT"))+xlab("TeX('$\\alpha^\\beta$')")+
  xlab(TeX('$\\beta_1'))+ylab(TeX('$\\beta_2'))

ggsave(paste0("TH ",q,"-Asy_copula-allcomb.jpeg"), units="in",dpi=400, height=5.3,width =8)

#########################################################################################################



#setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/Simulation AsyCopula")
setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Sim Copula")

ggplot(data=M1,aes(x=beta2,y=beta1,col=Max_KT,size=Diff_KT))+ geom_point()+ 
  theme_classic() +scale_color_gradientn(colours = rainbow(5),
                                         limits=c(0,1))+ 
  xlim(c(0,1))+ylim(c(0,1))+
  ggtitle(paste0("Pth=",q,": Variation of differences KT, Maximum value of KT for each Theta"))+ 
  facet_wrap(~ Theta)

ggsave(paste0("TH ",q,"-Asy_copula-allcomb.jpeg"), units="in",dpi=400, height=5.3,width =8)

#ggsave(paste0("TH ",q,"-Asy_copula.jpeg"), units="in",dpi=400, height=5.3,width =8)



ggplot(data=M1)+geom_point(aes(x=Diff_beta,y=Diff_KT, col=Theta))+
  scale_color_gradientn(colours = rainbow(5),limits=c(0,1))+
  theme_classic()

ggsave(paste0("TH ",q,"-Asy_copula-Diff betaDiffKT.jpeg"), units="in",dpi=400, height=5.3,width =8)

################################################################################
save(List_ex,M1, file="MultipleSimulations.RData")
save.image(file="Works_MultSim.RData")
save(M0,Asy_KT_list,KT_matrix,List_ex,M1,comb_all,Asy_KT_list, file="Workspace_sim.RData")

plot(List_ex[[181]][1])


################################################################################
plot(M1$Diff_beta,M1$Diff_KT)


#Simul_asy<-list()  

names(Simul_asy)<-c("Q 0.95","Q 0.98")


##################################### Plot ########################################################

save(Simul_asy, file="Matrix_AsyCopula.RData")

M1<-Simul_asy[[1]]

ggplot(data=M1,aes(x=beta1,y=beta2,col=Max_KT,size=Diff_KT))+ geom_point()+ 
  theme_classic() +scale_color_gradientn(colours = rainbow(5),
                                         limits=c(0,1))+ 
  xlim(c(0,1))+ylim(c(0,1))+
  ggtitle(paste0("Pth=",M1[1,1],": Variation of differences KT, Maximum value of KT for each Theta"))+ 
  facet_wrap(~ Theta)


ggsave(paste0("TH ",names(Simul_asy[ii]),"-MultipleSim Asy_copula.jpeg"), units="in",dpi=400, height=5.3,width =8)

#ggsave(paste0("TH ",names(Simul_asy[ii]),"-Asy_copula.jpeg"), units="in",dpi=400, height=5.3,width =8)



############################ UNIFORMED VALUES ###################################################
# Copula

b1<-0
b2<- 0.9

theta<-0.9
alp<-1/theta



#### Copula Asymmetric Logistic function

copula_asy<-function(x,v) {
  beta1= b1
  beta2= b2
  alp=alp
  
  return(exp(-((-beta1*log(x))^alp + (-beta2*log(v))^alp)^(1/alp) + (1-beta1)*log(x)+(1- beta2)*log(v))
  )
}

# Fixing beta1,beta2, alpha
n<-1000

u<-runif(n,min=0.0001,max=0.89)
t<-runif(n,min=0.0001,max=0.89)
v<-c()
#z<-runif(500,min=0,max=1)

#### Partial derivate of copula in x, and I found the values of v ########


for(i in 1:n){
  
cumulative1<-function(v) {
  beta1= b1
  beta2= b2
  alp=alp
  x=u[i]
  t1=t[i]
  return(exp(-((-beta1*log(x))^alp + (-beta2*log(v))^alp)^(1/alp) + (1-beta1)*log(x)+(1- beta2)*log(v)) *
                            ((beta1*((-beta1*log(x))^(alp-1))*(((-beta2*log(v))^alp) + (-beta1*log(x))^alp)^(1/(alp-1))/x)+
                               (1-beta2)/x)-t1
                              )}

sol <- try(uniroot(cumulative1, lower= 0.0000001, upper=0.99999)$root )
v[i] <- if (inherits(sol, "try-error")) NA else sol

}

############################## PLOT ##############################################
# Then here I calculate the value of copula for that both values u,v ############

el<-which(is.na(v))
u1<-u[-el]
v1<-v[-el]

copula<-copula_asy(u1,v1)

######################## Plot Points and surface ################################

s = interp(u1,v1,copula)
surface3d(s$x,s$y,s$z,color="red")

plot3d(u1,v1,copula,new=T, col="blue", pch=19,size=8 )
surface3d(s$x,s$y,s$z,color="red")

plot(u,v)


# Scatter3D
scatter3D(x=u1,y=v1,z=copula,new=T)
surface3d(s$x,s$y,s$z,color="red")

#################################################################################


