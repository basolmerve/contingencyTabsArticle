####Functions for Pearson's Chi-Square, Freeman-Tukey and Cressie-Read Statistics###################
#####Pearson's Chi-Square Statistic##################
pearson.chi<-function(observed,expected){
  pearson.chi<-sum(((observed-expected)^2)/expected)
  return(pearson.chi)
}
########Freeman-Tukey Statistic#######
ft<-function(observed,expected){
  ft<-sum((sqrt(observed)+sqrt(observed+1)-sqrt(4*expected+1))^2)
  return(ft)
}
########Cressie-Read Statistic#########
cr<-function(observed,expected){
  cr<-(9/5)*sum(observed*(((observed/expected)^(2/3))-1))
  return(cr)
}
num.row=5###number of rows
num.col=5####number of columns
df=(num.row-1)*(num.col-1)###degrees of freedom for critical table value
alpha=0.05##type I error rate
chitab<-qchisq(alpha,df,lower.tail = F)#####Critical table value
library(rTableICC)
library(vcd)
#####Row totals for Different Sampling Designs#####
######## Row Totals for Balanced Designs########
row_sc1.1<-c(20,20,20,20,20)####Total sample size 100
row_sc1.2<-c(40,40,40,40,40)####Total sample size 200
row_sc1.3<-c(100,100,100,100,100)####Total sample size 500
######## Row Totals for Almost Balanced Designs########
row_sc2.1<-c(15,15,20,25,25)###Total sample size 100
row_sc2.2<-c(30,30,40,50,50)###Total sample size 200
row_sc2.3<-c(75,75,100,125,125)###Total sample size 500
######## Row Totals for Imbalanced Designs########
row_sc3.1<-c(5,5,30,30,30)###Total sample size 100
row_sc3.2<-c(10,10,60,60,60)###Total sample size 200
row_sc3.3<-c(25,25,150,150,150)###Total sample size 500
#########Simulations for Power#############
#######A finite 5x5 matrix of cell probabilities for different effect sizes (w) and sampling designs (sd)####
#######Cell probabilities for w=0.1, sd=balanced#############
cell.prob_sc1.1=array(0,dim=c(num.row,num.col)) 
cell.prob_sc1.1[1,1]=0.03
cell.prob_sc1.1[1,2]=0.05
cell.prob_sc1.1[1,3]=0.04
cell.prob_sc1.1[1,4]=0.04
cell.prob_sc1.1[1,5]=20/100-0.16
cell.prob_sc1.1[2,1]=0.04
cell.prob_sc1.1[2,2]=0.04
cell.prob_sc1.1[2,3]=0.04
cell.prob_sc1.1[2,4]=0.04
cell.prob_sc1.1[2,5]=20/100-0.16
cell.prob_sc1.1[3,1]=0.04
cell.prob_sc1.1[3,2]=0.04
cell.prob_sc1.1[3,3]=0.03
cell.prob_sc1.1[3,4]=0.05
cell.prob_sc1.1[3,5]=20/100-0.16
cell.prob_sc1.1[4,1]=0.04
cell.prob_sc1.1[4,2]=0.04
cell.prob_sc1.1[4,3]=0.04
cell.prob_sc1.1[4,4]=0.04
cell.prob_sc1.1[4,5]=20/100-0.16
cell.prob_sc1.1[5,1]=0.03
cell.prob_sc1.1[5,2]=0.05
cell.prob_sc1.1[5,3]=0.04
cell.prob_sc1.1[5,4]=0.04
cell.prob_sc1.1[5,5]=20/100-0.16
##########Cell probabilities for w=0.1, sd=almost balanced#############
cell.prob_sc1.2=array(0,dim=c(num.row,num.col)) 
cell.prob_sc1.2[1,1]=0.03
cell.prob_sc1.2[1,2]=0.03
cell.prob_sc1.2[1,3]=0.03
cell.prob_sc1.2[1,4]=0.03
cell.prob_sc1.2[1,5]=15/100-0.12
cell.prob_sc1.2[2,1]=0.03
cell.prob_sc1.2[2,2]=0.03
cell.prob_sc1.2[2,3]=0.03
cell.prob_sc1.2[2,4]=0.03
cell.prob_sc1.2[2,5]=15/100-0.12
cell.prob_sc1.2[3,1]=0.04
cell.prob_sc1.2[3,2]=0.04
cell.prob_sc1.2[3,3]=0.04
cell.prob_sc1.2[3,4]=0.03
cell.prob_sc1.2[3,5]=20/100-0.15
cell.prob_sc1.2[4,1]=0.05
cell.prob_sc1.2[4,2]=0.05
cell.prob_sc1.2[4,3]=0.05
cell.prob_sc1.2[4,4]=0.06
cell.prob_sc1.2[4,5]=25/100-0.21
cell.prob_sc1.2[5,1]=0.04
cell.prob_sc1.2[5,2]=0.06
cell.prob_sc1.2[5,3]=0.05
cell.prob_sc1.2[5,4]=0.05
cell.prob_sc1.2[5,5]=25/100-0.20
##########Cell probabilities for w=0.1, , sd=imbalanced#############
cell.prob_sc1.3=array(0,dim=c(num.row,num.col)) 
cell.prob_sc1.3[1,1]=0.01
cell.prob_sc1.3[1,2]=0.01
cell.prob_sc1.3[1,3]=0.01
cell.prob_sc1.3[1,4]=0.01
cell.prob_sc1.3[1,5]=5/100-0.04
cell.prob_sc1.3[2,1]=0.01
cell.prob_sc1.3[2,2]=0.01
cell.prob_sc1.3[2,3]=0.01
cell.prob_sc1.3[2,4]=0.01
cell.prob_sc1.3[2,5]=5/100-0.04
cell.prob_sc1.3[3,1]=0.04
cell.prob_sc1.3[3,2]=0.08
cell.prob_sc1.3[3,3]=0.06
cell.prob_sc1.3[3,4]=0.06
cell.prob_sc1.3[3,5]=30/100-0.24
cell.prob_sc1.3[4,1]=0.06
cell.prob_sc1.3[4,2]=0.06
cell.prob_sc1.3[4,3]=0.06
cell.prob_sc1.3[4,4]=0.07
cell.prob_sc1.3[4,5]=30/100-0.25
cell.prob_sc1.3[5,1]=0.06
cell.prob_sc1.3[5,2]=0.06
cell.prob_sc1.3[5,3]=0.06
cell.prob_sc1.3[5,4]=0.06
cell.prob_sc1.3[5,5]=30/100-0.24
#################Cell probabilities for w=0.3, sd=balanced#############
cell.prob_sc2.1=array(0,dim=c(num.row,num.col)) 
cell.prob_sc2.1[1,1]=0.01
cell.prob_sc2.1[1,2]=0.07
cell.prob_sc2.1[1,3]=0.04
cell.prob_sc2.1[1,4]=0.04
cell.prob_sc2.1[1,5]=20/100-0.16
cell.prob_sc2.1[2,1]=0.04
cell.prob_sc2.1[2,2]=0.04
cell.prob_sc2.1[2,3]=0.04
cell.prob_sc2.1[2,4]=0.04
cell.prob_sc2.1[2,5]=20/100-0.16
cell.prob_sc2.1[3,1]=0.04
cell.prob_sc2.1[3,2]=0.04
cell.prob_sc2.1[3,3]=0.04
cell.prob_sc2.1[3,4]=0.01
cell.prob_sc2.1[3,5]=20/100-0.13
cell.prob_sc2.1[4,1]=0.04
cell.prob_sc2.1[4,2]=0.04
cell.prob_sc2.1[4,3]=0.04
cell.prob_sc2.1[4,4]=0.04
cell.prob_sc2.1[4,5]=20/100-0.16
cell.prob_sc2.1[5,1]=0.01
cell.prob_sc2.1[5,2]=0.07
cell.prob_sc2.1[5,3]=0.04
cell.prob_sc2.1[5,4]=0.04
cell.prob_sc2.1[5,5]=20/100-0.16
################Cell probabilities for w=0.3, sd=almost balanced#############
cell.prob_sc2.2=array(0,dim=c(num.row,num.col)) 
cell.prob_sc2.2[1,1]=0.01
cell.prob_sc2.2[1,2]=0.05
cell.prob_sc2.2[1,3]=0.03
cell.prob_sc2.2[1,4]=0.03
cell.prob_sc2.2[1,5]=15/100-0.12
cell.prob_sc2.2[2,1]=0.03
cell.prob_sc2.2[2,2]=0.03
cell.prob_sc2.2[2,3]=0.03
cell.prob_sc2.2[2,4]=0.02
cell.prob_sc2.2[2,5]=15/100-0.11
cell.prob_sc2.2[3,1]=0.04
cell.prob_sc2.2[3,2]=0.04
cell.prob_sc2.2[3,3]=0.02
cell.prob_sc2.2[3,4]=0.06
cell.prob_sc2.2[3,5]=20/100-0.16
cell.prob_sc2.2[4,1]=0.02
cell.prob_sc2.2[4,2]=0.08
cell.prob_sc2.2[4,3]=0.05
cell.prob_sc2.2[4,4]=0.05
cell.prob_sc2.2[4,5]=25/100-0.20
cell.prob_sc2.2[5,1]=0.05
cell.prob_sc2.2[5,2]=0.05
cell.prob_sc2.2[5,3]=0.05
cell.prob_sc2.2[5,4]=0.02
cell.prob_sc2.2[5,5]=25/100-0.17
##########Cell probabilities for w=0.3, sd=imbalanced#############
cell.prob_sc2.3=array(0,dim=c(num.row,num.col)) 
cell.prob_sc2.3[1,1]=0.01
cell.prob_sc2.3[1,2]=0.01
cell.prob_sc2.3[1,3]=0.01
cell.prob_sc2.3[1,4]=0.01
cell.prob_sc2.3[1,5]=5/100-0.04
cell.prob_sc2.3[2,1]=0.01
cell.prob_sc2.3[2,2]=0.01
cell.prob_sc2.3[2,3]=0.01
cell.prob_sc2.3[2,4]=0.01
cell.prob_sc2.3[2,5]=5/100-0.04
cell.prob_sc2.3[3,1]=0.02
cell.prob_sc2.3[3,2]=0.10
cell.prob_sc2.3[3,3]=0.06
cell.prob_sc2.3[3,4]=0.06
cell.prob_sc2.3[3,5]=30/100-0.24
cell.prob_sc2.3[4,1]=0.06
cell.prob_sc2.3[4,2]=0.06
cell.prob_sc2.3[4,3]=0.06
cell.prob_sc2.3[4,4]=0.02
cell.prob_sc2.3[4,5]=30/100-0.20
cell.prob_sc2.3[5,1]=0.02
cell.prob_sc2.3[5,2]=0.10
cell.prob_sc2.3[5,3]=0.06
cell.prob_sc2.3[5,4]=0.06
cell.prob_sc2.3[5,5]=30/100-0.24
#################Cell probabilities for w=0.5, sd=balanced#######
cell.prob_sc3.1=array(0,dim=c(num.row,num.col)) 
cell.prob_sc3.1[1,1]=0.00001
cell.prob_sc3.1[1,2]=0.10
cell.prob_sc3.1[1,3]=0.02
cell.prob_sc3.1[1,4]=0.04
cell.prob_sc3.1[1,5]=20/100-0.16001
cell.prob_sc3.1[2,1]=0.04
cell.prob_sc3.1[2,2]=0.04
cell.prob_sc3.1[2,3]=0.04
cell.prob_sc3.1[2,4]=0.04
cell.prob_sc3.1[2,5]=20/100-0.16
cell.prob_sc3.1[3,1]=0.04
cell.prob_sc3.1[3,2]=0.04
cell.prob_sc3.1[3,3]=0.00001
cell.prob_sc3.1[3,4]=0.02
cell.prob_sc3.1[3,5]=20/100-0.10001
cell.prob_sc3.1[4,1]=0.04
cell.prob_sc3.1[4,2]=0.04
cell.prob_sc3.1[4,3]=0.04
cell.prob_sc3.1[4,4]=0.04
cell.prob_sc3.1[4,5]=20/100-0.16
cell.prob_sc3.1[5,1]=0.00001
cell.prob_sc3.1[5,2]=0.10
cell.prob_sc3.1[5,3]=0.02
cell.prob_sc3.1[5,4]=0.04
cell.prob_sc3.1[5,5]=20/100-0.16001
#################Cell probabilities for w=0.5, sd=almost balanced#############
cell.prob_sc3.2=array(0,dim=c(num.row,num.col)) 
cell.prob_sc3.2[1,1]=0.01
cell.prob_sc3.2[1,2]=0.07
cell.prob_sc3.2[1,3]=0.03
cell.prob_sc3.2[1,4]=0.02
cell.prob_sc3.2[1,5]=15/100-0.13
cell.prob_sc3.2[2,1]=0.03
cell.prob_sc3.2[2,2]=0.03
cell.prob_sc3.2[2,3]=0.03
cell.prob_sc3.2[2,4]=0.02
cell.prob_sc3.2[2,5]=15/100-0.11
cell.prob_sc3.2[3,1]=0.06
cell.prob_sc3.2[3,2]=0.02
cell.prob_sc3.2[3,3]=0.02
cell.prob_sc3.2[3,4]=0.06
cell.prob_sc3.2[3,5]=20/100-0.16
cell.prob_sc3.2[4,1]=0.02
cell.prob_sc3.2[4,2]=0.08
cell.prob_sc3.2[4,3]=0.05
cell.prob_sc3.2[4,4]=0.09
cell.prob_sc3.2[4,5]=25/100-0.24
cell.prob_sc3.2[5,1]=0.08
cell.prob_sc3.2[5,2]=0.05
cell.prob_sc3.2[5,3]=0.02
cell.prob_sc3.2[5,4]=0.02
cell.prob_sc3.2[5,5]=25/100-0.17
##########Cell probabilities for w=0.5, sd=imbalanced#########
cell.prob_sc3.3=array(0,dim=c(num.row,num.col)) 
cell.prob_sc3.3[1,1]=0.01
cell.prob_sc3.3[1,2]=0.01
cell.prob_sc3.3[1,3]=0.01
cell.prob_sc3.3[1,4]=0.01
cell.prob_sc3.3[1,5]=5/100-0.04
cell.prob_sc3.3[2,1]=0.01
cell.prob_sc3.3[2,2]=0.01
cell.prob_sc3.3[2,3]=0.01
cell.prob_sc3.3[2,4]=0.01
cell.prob_sc3.3[2,5]=5/100-0.04
cell.prob_sc3.3[3,1]=0.02
cell.prob_sc3.3[3,2]=0.10
cell.prob_sc3.3[3,3]=0.06
cell.prob_sc3.3[3,4]=0.09
cell.prob_sc3.3[3,5]=30/100-0.27
cell.prob_sc3.3[4,1]=0.10
cell.prob_sc3.3[4,2]=0.02
cell.prob_sc3.3[4,3]=0.06
cell.prob_sc3.3[4,4]=0.02
cell.prob_sc3.3[4,5]=30/100-0.20
cell.prob_sc3.3[5,1]=0.02
cell.prob_sc3.3[5,2]=0.10
cell.prob_sc3.3[5,3]=0.05
cell.prob_sc3.3[5,4]=0.09
cell.prob_sc3.3[5,5]=30/100-0.26
############List of Simulation Scenarious for Power(here we combine cell probabilites with sample sizes to create all scenarios)################
all_scenarios_power<-list(list(list(list(cell.prob_sc1.1,row_sc1.1,row_sc1.2,row_sc1.3)),
                               list(list(cell.prob_sc1.2,row_sc2.1,row_sc2.2,row_sc2.3)),
                               list(list(cell.prob_sc1.3,row_sc3.1,row_sc3.2,row_sc3.3))),
                          list(list(list(cell.prob_sc2.1,row_sc1.1,row_sc1.2,row_sc1.3)),
                               list(list(cell.prob_sc2.2,row_sc2.1,row_sc2.2,row_sc2.3)),
                               list(list(cell.prob_sc2.3,row_sc3.1,row_sc3.2,row_sc3.3))),
                          list(list(list(cell.prob_sc3.1,row_sc1.1,row_sc1.2,row_sc1.3)),
                               list(list(cell.prob_sc3.2,row_sc2.1,row_sc2.2,row_sc2.3)),
                               list(list(cell.prob_sc3.3,row_sc3.1,row_sc3.2,row_sc3.3))))
names(all_scenarios_power)<-c("w=0.1","w=0.3","w=0.5")
names(all_scenarios_power$`w=0.1`)=c("Balanced","Almost Balanced","Imbalanced")
names(all_scenarios_power$`w=0.1`$Balanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.1`$Balanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","20 20 20 20 20","40 40 40 40 40","100 100 100 100 100")
names(all_scenarios_power$`w=0.1`$`Almost Balanced`)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.1`$`Almost Balanced`$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","15 15 20 25 25 ","30 30 40 50 50 ","75 75 100 125 125 ")
names(all_scenarios_power$`w=0.1`$Imbalanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.1`$Imbalanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities", "5 5 30 30 30", "10 10 60 60 60 ","25 25 150 150 150")
names(all_scenarios_power$`w=0.3`)=c("Balanced","Almost Balanced","Imbalanced")
names(all_scenarios_power$`w=0.3`$Balanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.3`$Balanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","20 20 20 20 20","40 40 40 40 40","100 100 100 100 100")
names(all_scenarios_power$`w=0.3`$`Almost Balanced`)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.3`$`Almost Balanced`$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","15 15 20 25 25 ","30 30 40 50 50 ","75 75 100 125 125 ")
names(all_scenarios_power$`w=0.3`$Imbalanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.3`$Imbalanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities", "5 5 30 30 30", "10 10 60 60 60 ","25 25 150 150 150")
names(all_scenarios_power$`w=0.5`)=c("Balanced","Almost Balanced","Imbalanced")
names(all_scenarios_power$`w=0.5`$Balanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.5`$Balanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","20 20 20 20 20","40 40 40 40 40","100 100 100 100 100")
names(all_scenarios_power$`w=0.5`$`Almost Balanced`)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.5`$`Almost Balanced`$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities","15 15 20 25 25 ","30 30 40 50 50 ","75 75 100 125 125 ")
names(all_scenarios_power$`w=0.5`$Imbalanced)=c("Cell Probabilities and Row Margins")
names(all_scenarios_power$`w=0.5`$Imbalanced$`Cell Probabilities and Row Margins`)=
  c("Cell Probabilities", "5 5 30 30 30", "10 10 60 60 60 ","25 25 150 150 150")
#####Simulation Function###########
sim.function<-function(probabilities,rowtotals){
  y1<-rTable.RxC(p=probabilities, sampling="Product",
                 row.margins = rowtotals)
  tab.power<-y1$rTable
  sr.power<-rowSums(tab.power)
  sc.power<-colSums(tab.power)
  expec.power<-outer(sr.power,sc.power,"*")/sum(tab.power)
  chi.stat.power<-pearson.chi(tab.power,expec.power)
  li.stat.power<-assocstats(tab.power)$chisq_tests[1,1]
  ft.stat.power<-ft(tab.power,expec.power)
  cr.stat.power<-cr(tab.power,expec.power)
  exact.power<-fisher.test(tab.power,simulate.p.value=TRUE,B=1000)$p
  results<-matrix(c(chi.stat.power,li.stat.power,
                    ft.stat.power,cr.stat.power,exact.power),ncol=5,byrow=F)
  
  return(results)
}
#########The objects for simulation study for power#####
M=10000##number of replicates########
scenario_power1<-matrix(nrow=M,ncol=5)
scenario_power2<-matrix(nrow=M,ncol=5)
scenario_power3<-matrix(nrow=M,ncol=5)
scenario_power4<-matrix(nrow=M,ncol=5)
scenario_power5<-matrix(nrow=M,ncol=5)
scenario_power6<-matrix(nrow=M,ncol=5)
scenario_power7<-matrix(nrow=M,ncol=5)
scenario_power8<-matrix(nrow=M,ncol=5)
scenario_power9<-matrix(nrow=M,ncol=5)
scenario_power10<-matrix(nrow=M,ncol=5)
scenario_power11<-matrix(nrow=M,ncol=5)
scenario_power12<-matrix(nrow=M,ncol=5)
scenario_power13<-matrix(nrow=M,ncol=5)
scenario_power14<-matrix(nrow=M,ncol=5)
scenario_power15<-matrix(nrow=M,ncol=5)
scenario_power16<-matrix(nrow=M,ncol=5)
scenario_power17<-matrix(nrow=M,ncol=5)
scenario_power18<-matrix(nrow=M,ncol=5)
scenario_power19<-matrix(nrow=M,ncol=5)
scenario_power20<-matrix(nrow=M,ncol=5)
scenario_power21<-matrix(nrow=M,ncol=5)
scenario_power22<-matrix(nrow=M,ncol=5)
scenario_power23<-matrix(nrow=M,ncol=5)
scenario_power24<-matrix(nrow=M,ncol=5)
scenario_power25<-matrix(nrow=M,ncol=5)
scenario_power26<-matrix(nrow=M,ncol=5)
scenario_power27<-matrix(nrow=M,ncol=5)
###The loop for all simulation scenarios####
set.seed(3)
for(i in 1:M){
  scenario_power1[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[1]][[1]][[2]])
  scenario_power2[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[1]][[1]][[3]])
  
  scenario_power3[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[1]][[1]][[4]])
  scenario_power4[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[2]][[1]][[2]])
  scenario_power5[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[2]][[1]][[3]])
  
  scenario_power6[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[2]][[1]][[4]])
  
  scenario_power7[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[3]][[1]][[2]])
  scenario_power8[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[3]][[1]][[3]])
  
  scenario_power9[i,]<-
    sim.function(probabilities = all_scenarios_power[[1]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[1]][[3]][[1]][[4]])
  
  scenario_power10[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[1]][[1]][[2]])
  scenario_power11[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[1]][[1]][[3]])
  
  scenario_power12[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[1]][[1]][[4]])
  
  scenario_power13[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[2]][[1]][[2]])
  scenario_power14[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[2]][[1]][[3]])
  
  scenario_power15[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[2]][[1]][[4]])
  
  
  scenario_power16[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[3]][[1]][[2]])
  scenario_power17[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[3]][[1]][[3]])
  
  scenario_power18[i,]<-
    sim.function(probabilities = all_scenarios_power[[2]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[2]][[3]][[1]][[4]])
  
  scenario_power19[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[1]][[1]][[2]])
  scenario_power20[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[1]][[1]][[3]])
  
  scenario_power21[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[1]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[1]][[1]][[4]])
  
  scenario_power22[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[2]][[1]][[2]])
  scenario_power23[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[2]][[1]][[3]])
  
  scenario_power24[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[2]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[2]][[1]][[4]])
  
  scenario_power25[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[3]][[1]][[2]])
  scenario_power26[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[3]][[1]][[2]])
  
  scenario_power27[i,]<-
    sim.function(probabilities = all_scenarios_power[[3]][[3]][[1]][[1]],
                 rowtotals =  all_scenarios_power[[3]][[3]][[1]][[4]])
  
}
#######filenames generated for representing the results######
filenames<-list(scenario_power1,scenario_power2,scenario_power3,scenario_power4,scenario_power5,
                scenario_power6,scenario_power7,scenario_power8,scenario_power9,scenario_power10,
                scenario_power11,scenario_power12,scenario_power13,scenario_power14,scenario_power15,
                scenario_power16,scenario_power17,scenario_power18,scenario_power19,scenario_power20,
                scenario_power21,scenario_power22,scenario_power23,scenario_power24,scenario_power25,
                scenario_power26,scenario_power27)
############Power calculations depending on simulation results#######
power_stats<-c()
power<-c()
results<-function(scenario){
  for(j in 1:4){
    power_stats[j]<-length(scenario[,j][scenario[,j]>=chitab])/M####Test statistics are considered
    exact<-length(scenario[1:M,5][scenario[1:M,5]<alpha])/M#####Fisher-Freeman-Halton is considered due to it generates only prob.
    power<-c(power_stats,exact)
    
  }
  print(power)
  
}
all_results_power<-matrix(nrow=27,ncol=5)
for (i in 1:27)
{
  all_results_power[i,]<-results(filenames[[i]])
}
###########The Results of simulation scenarios on the matrix#########
results_power_5x5<-matrix(all_results_power,ncol=5,nrow=27)
colnames(results_power_5x5)<-c("Chi-Square","Log-Likelihood",
                               "Freeman-Tukey","Cressie-Read","Fisher-Freeman-Halton")
rownames(results_power_5x5)<-c("w=0.1,balanced,n=100","w=0.1,balanced, n=200",
                               "w=0.1,balanced,n=500","w=0.1,almost balanced,n=100",
                               "w=0.1,almost balanced, n=200","w=0.1,almost balanced,n=500",
                               "w=0.1,imbalanced,n=100",
                               "w=0.1,imbalanced, n=200","w=0.1,imbalanced,n=500",
                               "w=0.3,balanced,n=100","w=0.3,balanced, n=200",
                               "w=0.3,balanced,n=500","w=0.3,almost balanced,n=100",
                               "w=0.3,almost balanced, n=200","w=0.3,almost balanced,n=500",
                               "w=0.3,imbalanced,n=100",
                               "w=0.3,imbalanced, n=200","w=0.3,imbalanced,n=500",
                               "w=0.5,balanced,n=100","w=0.5,balanced, n=200",
                               "w=0.5,balanced,n=500","w=0.5,almost balanced,n=100",
                               "w=0.5,almost balanced, n=200","w=0.5,almost balanced,n=500",
                               "w=0.5,imbalanced,n=100",
                               "w=0.5,imbalanced, n=200","w=0.5,imbalanced,n=500")


###############Cell probabilities for Type I error Rate##############
#A finite 5x2 matrix of cell probabilities for different sapling designs####
#######sd=balanced#############
cell.prob_sc0.1=array(0,dim=c(num.row,num.col)) 
cell.prob_sc0.1[1,1]=0.04
cell.prob_sc0.1[1,2]=0.04
cell.prob_sc0.1[1,3]=0.04
cell.prob_sc0.1[1,4]=0.04
cell.prob_sc0.1[1,5]=20/100-0.16
cell.prob_sc0.1[2,1]=0.04
cell.prob_sc0.1[2,2]=0.04
cell.prob_sc0.1[2,3]=0.04
cell.prob_sc0.1[2,4]=0.04
cell.prob_sc0.1[2,5]=20/100-0.16
cell.prob_sc0.1[3,1]=0.04
cell.prob_sc0.1[3,2]=0.04
cell.prob_sc0.1[3,3]=0.04
cell.prob_sc0.1[3,4]=0.04
cell.prob_sc0.1[3,5]=20/100-0.16
cell.prob_sc0.1[4,1]=0.04
cell.prob_sc0.1[4,2]=0.04
cell.prob_sc0.1[4,3]=0.04
cell.prob_sc0.1[4,4]=0.04
cell.prob_sc0.1[4,5]=20/100-0.16
cell.prob_sc0.1[5,1]=0.04
cell.prob_sc0.1[5,2]=0.04
cell.prob_sc0.1[5,3]=0.04
cell.prob_sc0.1[5,4]=0.04
cell.prob_sc0.1[5,5]=20/100-0.16
#################### sd=almost balanced#############
cell.prob_sc0.2=array(0,dim=c(num.row,num.col)) 
cell.prob_sc0.2[1,1]=0.03
cell.prob_sc0.2[1,2]=0.03
cell.prob_sc0.2[1,3]=0.03
cell.prob_sc0.2[1,4]=0.03
cell.prob_sc0.2[1,5]=15/100-0.12
cell.prob_sc0.2[2,1]=0.03
cell.prob_sc0.2[2,2]=0.03
cell.prob_sc0.2[2,3]=0.03
cell.prob_sc0.2[2,4]=0.03
cell.prob_sc0.2[2,5]=15/100-0.12
cell.prob_sc0.2[3,1]=0.04
cell.prob_sc0.2[3,2]=0.04
cell.prob_sc0.2[3,3]=0.04
cell.prob_sc0.2[3,4]=0.04
cell.prob_sc0.2[3,5]=20/100-0.16
cell.prob_sc0.2[4,1]=0.05
cell.prob_sc0.2[4,2]=0.05
cell.prob_sc0.2[4,3]=0.05
cell.prob_sc0.2[4,4]=0.05
cell.prob_sc0.2[4,5]=25/100-0.20
cell.prob_sc0.2[5,1]=0.05
cell.prob_sc0.2[5,2]=0.05
cell.prob_sc0.2[5,3]=0.05
cell.prob_sc0.2[5,4]=0.05
cell.prob_sc0.2[5,5]=25/100-0.20
###########################
#######sd=imbalanced#############
cell.prob_sc0.3=array(0,dim=c(num.row,num.col)) 
cell.prob_sc0.3[1,1]=0.01
cell.prob_sc0.3[1,2]=0.01
cell.prob_sc0.3[1,3]=0.01
cell.prob_sc0.3[1,4]=0.01
cell.prob_sc0.3[1,5]=5/100-0.04
cell.prob_sc0.3[2,1]=0.01
cell.prob_sc0.3[2,2]=0.01
cell.prob_sc0.3[2,3]=0.01
cell.prob_sc0.3[2,4]=0.01
cell.prob_sc0.3[2,5]=5/100-0.04
cell.prob_sc0.3[3,1]=0.06
cell.prob_sc0.3[3,2]=0.06
cell.prob_sc0.3[3,3]=0.06
cell.prob_sc0.3[3,4]=0.06
cell.prob_sc0.3[3,5]=30/100-0.24
cell.prob_sc0.3[4,1]=0.06
cell.prob_sc0.3[4,2]=0.06
cell.prob_sc0.3[4,3]=0.06
cell.prob_sc0.3[4,4]=0.06
cell.prob_sc0.3[4,5]=30/100-0.24
cell.prob_sc0.3[5,1]=0.06
cell.prob_sc0.3[5,2]=0.06
cell.prob_sc0.3[5,3]=0.06
cell.prob_sc0.3[5,4]=0.06
cell.prob_sc0.3[5,5]=30/100-0.24
##############List of Simulation Scenarious for Type I Error Rate (here we combine cell probabilites with sample sizes to create all scenarios)######
all_scenarios_alpha<-list(list(cell.prob_sc0.1,row_sc1.1,row_sc1.2,row_sc1.3),
                          list(cell.prob_sc0.2,row_sc2.1,row_sc2.2,row_sc2.3),
                          list(cell.prob_sc0.3,row_sc3.1,row_sc3.2,row_sc3.3))


names(all_scenarios_alpha)=c("Balanced","Almost Balanced","Imbalanced")
names(all_scenarios_alpha$Balanced)=  c("Cell Probabilities","20 20 20 20 20",
                                        "40 40 40 40 40","100 100 100 100 100")




names(all_scenarios_alpha$`Almost Balanced`)= c("Cell Probabilities","15 15 20 25 25 ",
                                                "30 30 40 50 50 ","75 75 100 125 125 ")


names(all_scenarios_alpha$Imbalanced)=  c("Cell Probabilities", "5 5 30 30 30",
                                          "10 10 60 60 60 ","25 25 150 150 150")
##########The objects for simulation study for type I Error Rate#####
scenario_alpha1<-matrix(nrow=M,ncol=5)
scenario_alpha2<-matrix(nrow=M,ncol=5)
scenario_alpha3<-matrix(nrow=M,ncol=5)
scenario_alpha4<-matrix(nrow=M,ncol=5)
scenario_alpha5<-matrix(nrow=M,ncol=5)
scenario_alpha6<-matrix(nrow=M,ncol=5)
scenario_alpha7<-matrix(nrow=M,ncol=5)
scenario_alpha8<-matrix(nrow=M,ncol=5)
scenario_alpha9<-matrix(nrow=M,ncol=5)
###The loop for all simulation scenarios####
set.seed(3)
for(i in 1:M){
  scenario_alpha1[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[1]][[1]],
                 rowtotals =  all_scenarios_alpha[[1]][[2]])
  scenario_alpha2[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[1]][[1]],
                 rowtotals =  all_scenarios_alpha[[1]][[3]])
  
  scenario_alpha3[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[1]][[1]],
                 rowtotals =  all_scenarios_alpha[[1]][[4]])
  scenario_alpha4[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[2]][[1]],
                 rowtotals =  all_scenarios_alpha[[2]][[2]])
  scenario_alpha5[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[2]][[1]],
                 rowtotals =  all_scenarios_alpha[[2]][[3]])
  
  scenario_alpha6[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[2]][[1]],
                 rowtotals =  all_scenarios_alpha[[2]][[4]])
  
  scenario_alpha7[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[3]][[1]],
                 rowtotals =  all_scenarios_alpha[[3]][[2]])
  scenario_alpha8[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[3]][[1]],
                 rowtotals =  all_scenarios_alpha[[3]][[3]])
  
  scenario_alpha9[i,]<-
    sim.function(probabilities = all_scenarios_alpha[[3]][[1]],
                 rowtotals =  all_scenarios_alpha[[3]][[4]])
}
####Filenames created for the results########
filenames_alpha<-list(scenario_alpha1,scenario_alpha2,scenario_alpha3,scenario_alpha4,scenario_alpha5,
                      scenario_alpha6,scenario_alpha7,scenario_alpha8,scenario_alpha9)
####Type I Error rate calculations depending on simulation results########
alpha_stats<-c()
alpha_estimated<-c()

results_alpha<-function(scenario){
  for(j in 1:4){
    alpha_stats[j]<-length(scenario[,j][scenario[,j]>=chitab])/M
    exact<-length(scenario[1:M,5][scenario[1:M,5]<alpha])/M
    alpha_estimated<-c(alpha_stats,exact)
    
  }
  print(alpha_estimated)
  
}
all_results_alpha<-matrix(nrow=9,ncol=5)
for (i in 1:9)
{
  all_results_alpha[i,]<-results_alpha(filenames_alpha[[i]])
}
###########The Results of simulation scenarios on the matrix#########
results_alpha_5x5<-matrix(all_results_alpha,ncol=5,nrow=9)

colnames(results_alpha_5x5)<-c("Chi-Square","Log-Likelihood",
                               "Freeman-Tukey","Cressie-Read","Fisher-Freeman-Halton")
rownames(results_alpha_5x5)<-c("Balanced,n=100","Balanced, n=200",
                               "Balanced,n=500","Almost balanced,n=100",
                               "Almost balanced, n=200","Almost balanced,n=500",
                               "Imbalanced,n=100",
                               "Imbalanced, n=200","Imbalanced,n=500")
##################RESULTS of Power and Type I Error Rate for 5x5 Table###################
results_power_5x5###power result in 5x5
results_alpha_5x5#####type I error rates results in 5x5
