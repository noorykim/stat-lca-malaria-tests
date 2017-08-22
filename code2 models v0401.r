#####################################
##### LCA Models: No covariates #####
#####################################

library(poLCA)
?poLCA

## no covariates

lcmn1 <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ 1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -1781.213 
# AIC(1): 3568.426
# BIC(1): 3583.96
# G^2(1): 112.4479 (Likelihood ratio/deviance statistic) 

lcmn2 <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ 1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# maximum log-likelihood: -1701.028 
# AIC(2): 3416.055
# BIC(2): 3452.3
# G^2(2): 5.453354 (Likelihood ratio/deviance statistic)

orderclass <- order(lcmn2$P, decreasing=T)
ordervars <- lcmn2$probs.start
L <- length(ordervars)
for(j in 1:L){
  ordervars[[j]] <- ordervars[[j]][orderclass,]  
}
lcmn2 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1) ~1, 
                probs.start=ordervars,
                na.rm=F, maxiter=5000, nrep=10, graphs=T, nclass=2)



################################################################
##### LCA Models: Treat covariates like manifest variables #####
################################################################

## recode variables to be used as covariates

ds.cov$firstpreg <- ds.cov$firstkid+1
ds.cov$teen <- ds.cov$under20+1


## Treat firstpreg like a test

lcmk1 <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1,firstpreg=firstkid+1) ~1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -2395.607 
# AIC(1): 4799.214
# BIC(1): 4819.925
# G^2(1): 160.0465 (Likelihood ratio/deviance statistic) 

lcmk2 <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1,firstpreg=firstkid+1) ~1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# maximum log-likelihood: -2275.488 
# AIC(2): 4568.976
# BIC(2): 4615.576
# G^2(2): 12.47005 (Likelihood ratio/deviance statistic) 

lcmk3 <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1,firstpreg=firstkid+1) ~1, 
               na.rm=F, maxiter=50000, nrep=10, nclass=3, graphs=T)
# maximum log-likelihood: -2273.466 
# AIC(3): 4574.931
# BIC(3): 4647.42
# G^2(3): 8.522567 (Likelihood ratio/deviance statistic) 


## Treat teen like a test

lcmt1 <- poLCA(data=ds.cov,
               formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                             teen=under20+1) ~1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -2498.913 
# AIC(1): 5005.827
# BIC(1): 5026.538
# G^2(1): 158.6406 (Likelihood ratio/deviance statistic)

lcmt2 <- poLCA(data=ds.cov,
               formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,teen=under20+1) ~1, 
               na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# maximum log-likelihood: -2384.001 
# AIC(2): 4786.002
# BIC(2): 4832.602
# G^2(2): 10.16794 (Likelihood ratio/deviance statistic) 

lcmt3 <- poLCA(data=ds.cov,
               formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,teen=under20+1) ~1, 
               na.rm=F, maxiter=50000, nrep=10, nclass=3, graphs=T) 
# maximum log-likelihood: -2381.996 
# AIC(3): 4791.992
# BIC(3): 4864.481
# G^2(3): 7.631034 (Likelihood ratio/deviance statistic)


## Treat firstpreg, teen as tests

lcmkt1 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstkid=firstkid+1, under20=under20+1) ~1, 
                na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -3113.307 
# AIC(1): 6236.614
# BIC(1): 6262.503
# G^2(1): 322.6845 (Likelihood ratio/deviance statistic) 

lcmkt2 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstkid=firstkid+1, under20=under20+1) ~1, 
                na.rm=F, maxiter=5000, nrep=10, graphs=T, nclass=2)
# maximum log-likelihood: -2865.703 
# AIC(2): 5753.406
# BIC(2): 5810.361
# G^2(2): 90.90625 (Likelihood ratio/deviance statistic) 

lcmkt3 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstkid=firstkid+1, teen=under20+1) ~1, 
                na.rm=F, maxiter=5000, nrep=10, graphs=T, nclass=3)
# maximum log-likelihood: -2821.836 
# AIC(3): 5677.672
# BIC(3): 5765.694
# G^2(3): 33.33732 (Likelihood ratio/deviance statistic) 

orderclass <- order(lcmkt3$P, decreasing=T)
ordervars <- lcmkt3$probs.start
L <- length(ordervars)
for(j in 1:L){
  ordervars[[j]] <- ordervars[[j]][orderclass,]  
}
lcmkt3 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstkid=firstkid+1,teen=under20+1) ~1, 
                probs.start=ordervars,
                na.rm=F, maxiter=5000, nrep=10, graphs=T, nclass=3)

lcmkt4 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstpreg=firstkid+1,teen=under20+1) ~1, 
                na.rm=F, maxiter=50000, nrep=10, graphs=T, nclass=4)
# maximum log-likelihood: -2812.041 
# AIC(4): 5670.083
# BIC(4): 5789.172
# G^2(4): 16.51902 (Likelihood ratio/deviance statistic) 

order(c(.09,.22,.06,.62))

# orderclass <- order(lcmkt4$P, decreasing=T)
orderclass <- c(3,2,4,1)
ordervars <- lcmkt4$probs.start
L <- length(ordervars)
for(j in 1:L){
  ordervars[[j]] <- ordervars[[j]][orderclass,]  
}
lcmkt4 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstpreg=firstkid+1,teen=under20+1) ~1, 
                probs.start=ordervars,
                na.rm=F, maxiter=50000, nrep=10, graphs=T, nclass=4)

lcmkt5 <- poLCA(data=ds.cov,
                formula=cbind(micro=micro+1,pcr=pcr+1,rdt=rdt+1,
                              firstkid=firstkid+1,under20=under20+1) ~1, 
                na.rm=F, maxiter=100000, nrep=10, graphs=T, nclass=5)
# maximum log-likelihood: -2810.19 
# AIC(5): 5678.379
# BIC(5): 5828.535
# G^2(5): 14.68044 (Likelihood ratio/deviance statistic) 
# ALERT: iterations finished, MAXIMUM LIKELIHOOD NOT FOUND 

##
names(lcmkt1)
lcmkt1$llik

maxloglike <- c(lcmkt1$llik, lcmkt2$llik, lcmkt3$llik, lcmkt4$llik, lcmkt5$llik)
maxloglike - min(maxloglike)
2*diff(maxloglike)

classes <- c(1:5) 
aic <- c(lcmkt1$aic, lcmkt2$aic, lcmkt3$aic, lcmkt4$aic, lcmkt5$aic)
bic <- c(lcmkt1$bic, lcmkt2$bic, lcmkt3$bic, lcmkt4$bic, lcmkt5$bic)
G2 <- c(lcmkt1$Gsq, lcmkt2$Gsq, lcmkt3$Gsq, lcmkt4$Gsq, lcmkt5$Gsq)

## find min and max and round to nearest 100
library(reshape)
abmin <- min(aic,bic)
abmax <- max(aic,bic)
ymin <- round_any(abmin, 100, floor)
ymax <- round_any(abmax, 100, ceiling)

## plot
par(mar=c(5, 4, 4, 4) + 0.1)
plot(aic ~ classes, type="b", col="red", ylab="AIC (red), BIC (blue)", ylim=c(ymin,ymax))
#      ,main="Goodness of fit measures for the LCA model with firstkid,under20") 
lines(bic ~ classes,type="b",col="blue")
text(4.5, 5850, "BIC")
text(4.5, 5710, "AIC")
text(4.5, 5600, "G^2")
par(new=T)
plot(G2 ~ classes,type="b",axes=F, ylab="", ylim=c(0,350),col="green")
axis(4, seq(0,350,50))
mtext("G2 (green)", side=4, line=3)



########################################################
##### LCA Models: Treat covariates like covariates #####
########################################################
## recode variables to be used as covariates

ds.cov$firstpreg <- ds.cov$firstkid+1
ds.cov$teen <- ds.cov$under20+1

## Treat firstpreg as a covariate

lcm1p <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ firstpreg, 
               na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -1778.4 
# AIC(1): 3562.799
# BIC(1): 3578.328
# G^2(1): 112.1512 (Likelihood ratio/deviance statistic) 

lcm2p <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ firstpreg, 
               na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# ALERT: negative degrees of freedom; respecify model 


## Treat teen as a covariate

lcm1t <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ teen, 
               na.rm=F, maxiter=5000, nrep=10, nclass=1)
# maximum log-likelihood: -1780.583 
# AIC(1): 3567.166
# BIC(1): 3582.697
# G^2(1): 112.4944 (Likelihood ratio/deviance statistic) 

lcm2t <- poLCA(data=ds.cov,
               formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ teen, 
               na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)


## Treat firstpreg, teen as a covariate

lcm1pt <- poLCA(data=ds.cov,
                formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ teen*firstpreg, 
                na.rm=F, maxiter=5000, nrep=10, nclass=1)
# ALERT: covariates not allowed when nclass=1; will be ignored.

## with interaction
lcm2pti <- poLCA(data=ds.cov,
                 formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ teen*firstpreg, 
                 na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# ALERT: negative degrees of freedom; respecify model

## without interaction
lcm2pt <- poLCA(data=ds.cov,
                formula=cbind(pcr=pcr+1,rdt=rdt+1,micro=micro+1) ~ teen+firstpreg, 
                na.rm=F, maxiter=5000, nrep=10, nclass=2, graphs=T)
# ALERT: negative degrees of freedom; respecify model
