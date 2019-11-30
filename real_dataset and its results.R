########Functions for Pearson's chi-square, Freeman-Tukey and Cressie-Read test statistics##
pearson.chi<-function(observed,expected){
  pearson.chi<-sum(((observed-expected)^2)/expected)
  return(pearson.chi)
}
library(vcd)


ft<-function(observed,expected){
  ft<-sum((sqrt(observed)+sqrt(observed+1)-sqrt(4*expected+1))^2)
  return(ft)
}
cr<-function(observed,expected){
  cr<-(9/5)*sum(observed*(((observed/expected)^(2/3))-1))
  return(cr)
}
######First real contingency table about causes of suicide vs. education level####
ex.1<-matrix(c(9,4,53,4,53,27,53,155,10,174,60,74,197,37,269,22,81,170,23,205,8,34,95,11,139),
          nrow=5,byrow = T) 
rownames(ex.1)<-c("Never received formal education","Primary School","Secondary School",
               "High School","Graduate")
colnames(ex.1)<-c("Marital conflict","Financial difficulty","Disease","Emotional","Others")
n1=sum(ex.1)
df1<-16
sr1<-rowSums(ex.1)######row sums of cont. table
sc1<-colSums(ex.1)####column sums of contigency table 
expec1<-outer(sr1,sc1,"*")/n1######expected values of cont. table
#######Results of tests for contingency table#########
chi1<-pearson.chi(ex.1,expec1)###Pearson's chi-square test statistic
chi1.p<-pchisq(chi1,df=df1,lower.tail = F)#Pearson's chi-square p-value
ft1<-ft(ex.1,expec1)###Freeman-Tukey test statistic
ft1.p<-pchisq(ft1,df=df1,lower.tail = F)###Freeman-Tukey p-value
lik1<-assocstats(ex.1)$chisq_tests[1,1]###Log-Likelihood  test statistic
lik1.p<-pchisq(lik1,df=df1,lower.tail = F)##Log-Likelihood p-value
cr1<-cr(ex.1,expec1)###Cressie-Read test statistic
cr1.p<-pchisq(cr1,df=df1,lower.tail = F)###Cressie-Read p-value
fisher1<-fisher.test(ex.1,simulate.p.value=TRUE,B=1000)$p####Fisher-Freeman-Halton
######Second real contingency table about causes of suicide vs. gender####
ex.2<-matrix(c(34,10,190,24,211,95,236,487,62,650),
             nrow=2,byrow = T) ###First real data set
n2=sum(ex.2)
df2<-5
sr2<-rowSums(ex.2)######row sums of cont. table
sc2<-colSums(ex.2)####column sums of contigency table 
expec2<-outer(sr2,sc2,"*")/n2######expected values of cont. table
#######Results of tests for contingency table#########
chi2<-pearson.chi(ex.2,expec2)###Pearson's chi-square test statistic
chi2.p<-pchisq(chi2,df=df2,lower.tail = F)#Pearson's chi-square p-value
ft2<-ft(ex.2,expec2)###Freeman-Tukey test statistic
ft2.p<-pchisq(ft2,df=df2,lower.tail = F)###Freeman-Tukey p-value
lik2<-assocstats(ex.2)$chisq_tests[1,1]###Log-Likelihood  test statistic
lik2.p<-pchisq(lik2,df=df2,lower.tail = F)##Log-Likelihood p-value
cr2<-cr(ex.2,expec2)###Cressie-Read test statistic
cr2.p<-pchisq(cr2,df=df2,lower.tail = F)###Cressie-Read p-value
fisher2<-fisher.test(ex.2,simulate.p.value=TRUE,B=1000)$p####Fisher-Freeman-Halton

###Results of Real Data Sets#####
results_real_data<-matrix(c(chi1,chi1.p,lik1,lik1.p,ft1,ft1.p,cr1,cr1.p,fisher1,
                            chi2,chi2.p,lik2,lik2.p,ft2,ft2.p,cr2,cr2.p,fisher2),
                          ncol=9,nrow=2,byrow = T)
rownames(results_real_data)<-c("Causes/Education Level","Causes/Gender")
colnames(results_real_data)<-c("Pearson's test stat.","Pearson's p-value","Log-likelihood test stat",
                               "Log-likelihood p-value","Freeman-Tukey test stat.","Freeman-Tukey p-value",
                               "Cressie-Read test stat","Cressi-Read p-value","Fisher-Freeman-Halton")
results_real_data


