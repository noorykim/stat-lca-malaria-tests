## Mac
setwd("/Users/ ... /2013A-spring/bios841/projects/project3/data")
## Linux
# setwd("/home/ ... /2013A-spring/bios841/projects/project3/data")

##############
##### d6a ####
##############
d6a.orig <- read.csv("export_form_06a.csv", header=T)
str(d6a.orig)
vars <- c("Participant","MedAge")
d6a.wds <- d6a.orig[,vars]
names(d6a.wds)[names(d6a.wds)=="Participant"] <- "id"
names(d6a.wds)[names(d6a.wds)=="MedAge"] <- "age"
d6a.wds <- d6a.wds[!duplicated(d6a.wds),]
head(d6a.wds)
dim(d6a.wds)

d2.orig <- read.csv("export_form_02.csv", header=T)
str(d2.orig)
vars <- c("Participant","ScrMotherChild")
d2.wds <- d2.orig[,vars]
names(d2.wds)[names(d2.wds)=="Participant"] <- "id"
names(d2.wds)[names(d2.wds)=="ScrMotherChild"] <- "gravidity"
d2.wds <- d2.wds[!duplicated(d2.wds),]
head(d2.wds)
dim(d2.wds)

workingds <- read.csv("workingds.csv", header=T)
str(workingds)
head(workingds)
dim(workingds)

pcr.rdt <-table(workingds$pcr,workingds$rdt)

install.packages("psych")
library(psych)
wkappa(pcr.rdt)


?merge
wds <- merge(workingds, d6a.wds, by="id")
head(wds)
dim(wds)
wds2 <- merge(wds, d2.wds, by="id")
head(wds2)
dim(wds2)

## Age distribution
boxplot(wds2$age)
hist( wds2$age, breaks=seq(0,100,1), main="Histogram of Age",
      xlab="Maternal Age" )
table(wds2$age)
quantile(wds2$age, na.rm=T) # 7 20 24 29 89
d6a.orig[d6a.orig$MedAge<10,]

table(wds2$under20)/length(wds2$under20)



## Gravidity
boxplot(wds2$gravidity)
hist( wds2$gravidity, breaks=seq(0,70,1), 
      main="Histogram of Gravidity", xlab="Prior pregnancies" )
table(wds2$gravidity)
table(wds2$gravidity)/length(wds2$gravidity)

quantile(wds2$gravidity, na.rm=T) # 0 1 2 3 63


