
######## Clear Workspace ################# 

rm(list = ls())

###########################################

# Causality links KT Asy Tests #
# After the talk with Sebastian #

library(gridExtra)
library(grid)
library(gtools)
library(dplyr)

require (ggplot2)
require (plyr)
library(reshape2)

path<- c("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Causality")



Processing_mat<- function(X,Y){ 
  
                            source("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/Functions/Tail_KT_withsym.R")
  
                            XY<-lapply(1:ncol(X),function(x){data.frame(cbind(X[,x],Y[,x]))})

                            KT1<- lapply(1:length(XY),function(x){Tail_KT_withsym(XY[[x]],q)})

                            Kendal_matrix<-sapply(1:length(KT1),function(x){ c(KT1[[x]]$`KT test 12`$estimate,KT1[[x]]$`KT test 12`$p.value,
                                                KT1[[x]]$`KT test 21`$estimate,KT1[[x]]$`KT test 21`$p.value,
                                                ifelse(is.vector(KT1[[x]]$`KT test Sym`)==FALSE, KT1[[x]]$`KT test Sym`$estimate,NA), 
                                                ifelse(is.vector(KT1[[x]]$`KT test Sym`)==FALSE, KT1[[x]]$`KT test Sym`$p.value,NA))})

                            Kendal_matrix<-t(Kendal_matrix)
                            colnames(Kendal_matrix)<-c("KT12","pvalue12","KT21","pvalue21","KTSym","pvalueSym")
                            return( Kendal_matrix) }





setwd(path)

M0<-as.data.frame(matrix(, nrow = 6, ncol = 8))

colnames(M0)<-c("Case","Threshold", "KT_x",  "pvalue KT_x",
                "KT_y","pvalue KT_y", "Sym_KT", "pvalue_Sym_KT")

Sensitivity_analysis<-list()


set.seed(1257)

q<-0.98

#pdf(paste0("Simulations_6cases_PTH",q,".pdf"), height = 11, width=8.5)

Coef<-c(0.2,0.4,0.6,0.8)

Pos_comb0<-data.frame(combinations(4, 2, Coef, repeats = TRUE))
colnames(Pos_comb0)<- c("Coef","CoefC")

Pos_comb<-Pos_comb0 %>% slice(rep(1:n(), each = 6))

Sens_matrix<-data.frame(matrix(,nrow=nrow(Pos_comb), ncol=6))

colnames(Sens_matrix)<-c("KT_x",  "pvalue KT_x",
                         "KT_y","pvalue KT_y", "Sym_KT", "pvalue_Sym_KT")

M_Overall<-cbind(Pos_comb,Sens_matrix)

Simulations<-list(list(),list(),list(),list(),list(),list())

names(Simulations)<-c("case1","case2","case3","case4","case5","case6")

############## Before running ###############################
### Choose parameters ####

par_tst<-0.3
par_C<-0.3


print(paste("Running analysis for for t-student coeff:", par_tst, " and C coefficient:",par_C))

####################### RUN ##################################

for (ii in 1:nrow(Pos_comb0))
{
  
Cx<-M_Overall$Coef[ii]
#CofC<-M_Overall$CoefC[ii]


#First case: Independence, no Cofounder
case<- 1
n <- 4000
X <-  replicate(1000, par_tst*rt(n = n, df = 3))
Y <-  replicate(1000, par_tst*rt(n = n, df = 3) )


name1<-paste0("Independent - C=0, q= ",q) 

KT1_matrix<- Processing_mat(X,Y)
Sum1<- apply(KT1_matrix,2,quantile, probs=c(0.5), na.rm=TRUE)


#Second case: Causal link X->Y, no Cofounder

case<- 2
#n <- 50000
X2 <-  X
Y2<-  Y + Cx*X 

name2<-paste0("Causal link X->Y - C=0, q= ",q) 


KT2_matrix<- Processing_mat(X2,Y2)
Sum2<- apply(KT2_matrix,2,quantile, probs=c(0.5), na.rm=TRUE)

#Third case: Causal link Y->X, no Cofounder

case<- 3
#n <- 50000

X3<- X + Cx*Y
Y3<- Y
  

name3<-paste0("Causal link X<-Y - C=0, q= ",q) 

KT3_matrix<- Processing_mat(X3,Y3)
Sum3<- apply(KT3_matrix,2,quantile, probs=c(0.5), na.rm=TRUE) 

#Fourth case: Independence link, yes Cofounder

case<- 4
#n <- 50000
C <-  replicate(1000, rt(n = n, df = 3))
X4 <-  X + par_C*C
Y4 <-  Y + par_C*C

name4<-paste0("Independent with Cofounder, q= ",q) 

KT4_matrix<- Processing_mat(X4,Y4)
Sum4<- apply(KT4_matrix,2,quantile, probs=c(0.5), na.rm=TRUE)   

#Fifth case: Causal link X->Y, yes Cofounder

case<- 5
#n <- 50000
#C1 <-  replicate(1000, rt(n = n, df = 3))
X5 <-  X + par_C*C
Y5 <-  Y + par_C*C +Cx*X

KT5_matrix<- Processing_mat(X5,Y5)
Sum5<- apply(KT5_matrix,2,quantile, probs=c(0.5), na.rm=TRUE)   

name5<-paste0("Causal link X->Y with Cofounder, q= ",q) 


#Sixth case: Causal link X<-Y, yes Cofounder

case6<- 6
#n <- 50000
#C2<- replicate(1000, rt(n = n, df = 3))
X6 <-  X + par_C*C +Cx*Y
Y6 <-  Y + par_C*C 


KT6_matrix<- Processing_mat(X6,Y6)
Sum6<- apply(KT6_matrix,2,quantile, probs=c(0.5), na.rm=TRUE)   
name6<-paste0("Causal link Y->X with Cofounder, q= ",q) 

##### Binding the six cases #######

Sum_all<-t(cbind(Sum1,Sum2,Sum3,Sum4,Sum5,Sum6))

Sensitivity_analysis[[ii]]<-cbind(Pos_comb0[ii,],rownames(Sum_all),Sum_all)

names(Sensitivity_analysis[ii])<-ii


Simulations[[1]][[ii]]<-data.frame(KT1_matrix)
Simulations[[2]][[ii]]<-data.frame(KT2_matrix)
Simulations[[3]][[ii]]<-data.frame(KT3_matrix)
Simulations[[4]][[ii]]<-data.frame(KT4_matrix)
Simulations[[5]][[ii]]<-data.frame(KT5_matrix)
Simulations[[6]][[ii]]<-data.frame(KT6_matrix)

}


case<-rep(1,10000)

case1<-cbind(rbind(Simulations[[1]][[1]],Simulations[[1]][[2]],Simulations[[1]][[3]],Simulations[[1]][[4]],
             Simulations[[1]][[5]],Simulations[[1]][[6]],Simulations[[1]][[7]],Simulations[[1]][[8]],Simulations[[1]][[9]],Simulations[[1]][[10]]),case)

case<-rep(2,10000)
case2<-cbind(rbind(Simulations[[2]][[1]],Simulations[[2]][[2]],Simulations[[2]][[3]],Simulations[[2]][[4]],
             Simulations[[2]][[5]],Simulations[[2]][[6]],Simulations[[2]][[7]],Simulations[[2]][[8]],Simulations[[2]][[9]],Simulations[[2]][[10]]),case)

case<-rep(3,10000)
case3<-cbind(rbind(Simulations[[3]][[1]],Simulations[[3]][[2]],Simulations[[3]][[3]],Simulations[[3]][[4]],
             Simulations[[3]][[5]],Simulations[[3]][[6]],Simulations[[3]][[7]],Simulations[[3]][[8]],Simulations[[3]][[9]],Simulations[[3]][[10]]),case)

case<-rep(4,10000)
case4<-cbind(rbind(Simulations[[4]][[1]],Simulations[[4]][[2]],Simulations[[4]][[3]],Simulations[[4]][[4]],
             Simulations[[4]][[5]],Simulations[[4]][[6]],Simulations[[4]][[7]],Simulations[[4]][[8]],Simulations[[4]][[9]],Simulations[[4]][[10]]),case)

case<-rep(5,10000)
case5<-cbind(rbind(Simulations[[5]][[1]],Simulations[[5]][[2]],Simulations[[5]][[3]],Simulations[[5]][[4]],
             Simulations[[5]][[5]],Simulations[[5]][[6]],Simulations[[5]][[7]],Simulations[[5]][[8]],Simulations[[5]][[9]],Simulations[[5]][[10]]),case)

case<-rep(6,10000)
case6<-cbind(rbind(Simulations[[6]][[1]],Simulations[[6]][[2]],Simulations[[6]][[3]],Simulations[[6]][[4]],
             Simulations[[6]][[5]],Simulations[[6]][[6]],Simulations[[6]][[7]],Simulations[[6]][[8]],Simulations[[6]][[9]],Simulations[[6]][[10]]),case)


Final_mat<-rbind(case1,case2,case3,case4,case5,case6)

dfmelt <- melt(Final_mat, measure.vars=1:6)  

case<-seq(1,6,1)


sum_KT12<-tapply(Final_mat$KT12,Final_mat$case,median)
sum_KT21<-tapply(Final_mat$KT21 ,Final_mat$case,median)
sum_KTSym<-tapply(Final_mat$KTSym,Final_mat$case,median)

sumKT<-rbind(sum_KT12,sum_KT21,sum_KTSym)

sum_pv12<-tapply(Final_mat$pvalue12,Final_mat$case,median)
sum_pv21<-tapply(Final_mat$pvalue21,Final_mat$case,median)
sum_pvSym<-tapply(Final_mat$pvalueSym,Final_mat$case,median)

pvalue<-rbind(sum_pv12,sum_pv21,sum_pvSym)

pvalue_check<-pvalue<=0.05


sum_meltpv<-melt(pvalue_check, measure.vars=1:6)  
sum_meltKT<-melt(sumKT, measure.vars=1:6)  

sum_melt$IndDep<-sum_meltpv$value
sum_melt$group<-ifelse(sum_meltpv$value==TRUE,17,19)
sum_melt$variable<-rep(c("KT12","KT21","KTSym"),6)
sum_melt$value<-sum_meltKT$value
sum_melt$case<- sum_melt$Var2


#Summary<-rbind(sum_KT12,sum_pv12,sum_KT21,sum_pv21,sum_KTSym,sum_pvSym,case)

#sum_melt0<-melt(Summary, measure.vars=1:6)  



############## PLOT FOR PAPER ###############################

setwd("C:/Users/Cristina/Documents/PROJECTS 2021/QQ_POT_mixed/Code/POT_from_DailyData/0. FINAL CODE/Causality")

# UPDATE BOXPLOT #
# Boxplot for each couple of combinations and the 6 cases
#######################################################
########### KENDALL'S TAU PLOT #############################
#######################################################


# Version 2
X11()

  
box22<-  dfmelt %>% 
  filter(variable %in% c("KT12","KT21","KTSym")) %>%
  ggplot(aes(x=variable,y=value, fill=variable)) +
  facet_grid(. ~ case, switch = "x")+
  geom_boxplot( ) + 
  labs(fill = "Kendall's Tau") + 
  #geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme_bw(base_size = 16)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Cases")+ylab("")+ 
  theme(legend.position="bottom") 

box22
ggsave(paste0("0_CausalBoxplot_tst",par_tst,"Ccoef",par_C,".jpeg"), units="in",dpi=400, height=8,width =13)



box22+ geom_point(data=sum_melt,aes(x=variable,y=value,shape=IndDep),col="blue", size=4, show.legend=T) +
  scale_shape_manual(name = "State",labels=c("Independent","Dependent"),values=c(19,17))+ 
  theme(legend.position="bottom") 


ggsave(paste0("0_CausalBoxplot_tst",par_tst,"Ccoef",par_C,"L.jpeg"), units="in",dpi=400, height=8,width =13)

#######################################################
########### P VALUE PLOT #############################
#######################################################

# Pvalues boxplot

X11()
dfmelt %>% 
  filter(variable %in% c("pvalue12","pvalue21","pvalueSym")) %>%
  ggplot(aes(x=variable,y=value, fill=variable)) +
  facet_grid(. ~ case, switch = "x")+
  geom_boxplot() + 
  labs(fill = "Kendall's Tau") + 
  #geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme_bw(base_size = 16)+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+xlab("Cases")+ylab("")+
  theme(legend.position="bottom")+ scale_fill_brewer(palette="Dark2")


ggsave(paste0("0_pval_CausalBoxplot_tst",par_tst,"Ccoef",par_C,".jpeg"), units="in",dpi=400, height=8,width =18)

##############################################################

print(paste("Results for t-student coeff:", par_tst, " and C coefficient:",par_C))


### TRY OLD


ggplot()+ geom_point(data=sum_melt,aes(x=variable,y=value,shape=IndDep),col="blue", size=4) +
  scale_shape_manual(name = "State",labels=c("FALSE","TRUE"),values=c(17,19))+ 
  theme(legend.position="bottom") 


+
  facet_grid(. ~ case, switch = "x")

facet_grid(. ~ case, switch = "x") 
box22

#,shape=sum_melt$shape
#  scale_shape_manual(name = "State", values=c("Dependent"=17,"Independent"=19))


X11()
box22+stat_summary(fun=mean, geom="point", shape=sum_melt$shape, size=5,col="black") +
  scale_shape_manual(name = "State", values=c("Dependent"=17,"Independent"=19))

guides(shape = guide_legend(override.aes = list(shape= c(17,19)) ) )



+
  +
  
  
  
  scale_color_manual(values = c(KT12 = "red", KT21 = "green", KTSym = "blue"))+
  theme(legend.position="bottom")

scale_colour_manual(values=c("blue", "red")) +
  scale_shape_manual(values=c(16, 5))

