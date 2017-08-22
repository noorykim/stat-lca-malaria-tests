## Mac
setwd("/Users/ ... /2013A-spring/bios841/projects/project3/data")
## Linux
setwd("/home/ ... /2013A-spring/bios841/projects/project3/data")

##############
##### d6a ####
##############
d6a.orig <- read.csv("export_form_06a.csv", header=T)
# head(d6a.orig)
d6a <- read.csv("export_form_06a.csv", header=T)
dim(d6a) ## 1560

## rename variables for convenience
names(d6a)[names(d6a)=="Participant"] <- "id"
names(d6a)[names(d6a)=="NumberVisit"] <- "visit"
names(d6a)[names(d6a)=="MedMalariaRDT"] <- "rdt"
names(d6a)[names(d6a)=="record_id"] <- "record"
# names(d6a)[names(d6a)=="MedAge"] <- "age"

## subset variables of interest
vars <- c("id", "visit", "rdt")
d6a <- d6a[,vars]
head(d6a)
dim(d6a) # 1560

## clean up data entry variations
table(d6a$visit) 
d6a[d6a$visit=="mEl1",]$visit <- "mEli"  
d6a[d6a$visit=="meli",]$visit <- "mEli"    
table(d6a$visit) ## 1559 1 

d6a$trimester <- ifelse(d6a$visit=="mEli",2,3)

## dichotomize rapid test results
## assume "99" is code for missing data 
table(d6a$rdt) ## 1189 347 11 1 10
d6a$rdt <- ifelse(d6a$rdt==0, 0, ifelse(d6a$rdt==99,NA,1))
table(d6a$rdt,useNA="always") ## 1189 359 12

## persons who apparently were eligible but did not participate
dim( d6a[d6a$id=="    .",] )  ## 93 rows are missing id numbers
table(d6a[d6a$id=="    .",]$rdt) # 0:72  1:12  NA:9
## remove observations without ID
d6a <- d6a[!(d6a$id=="    ."),]
dim(d6a) ## 1467
head(d6a)

## remove duplicate rows (having same test results)
dim( d6a[duplicated(d6a),] ) # 87
d6a <- d6a[!duplicated(d6a),]
dim(d6a) ## 1380 unique rows

## find duplicate id's with conflicting test results
idcount <- table(d6a$id) 
tossout.d6a <- names(idcount[idcount>1])
tossout.d6a  ## "3242" "3283" "4027" "8266" "8387"
## set conflicting results to NA
d6a[(d6a$id %in% tossout.d6a), ]$rdt <- NA
dim(d6a) ## 1370

## remove duplicate rows
dim( d6a[duplicated(d6a),] ) # 5
d6a <- d6a[!duplicated(d6a),]
dim(d6a) ## 1375 unique rows

table(d6a$rdt,useNA="always") # 1051 316 8
table(d6a$rdt,d6a$visit,useNA="ifany") 

dim(d6a) # 1375
head(d6a)

##############
##### d6b ####
##############

## import data set (already modified to make date formats uniform)
d6b <- read.csv("export_form_06b.csv", header=T)
dim(d6b) ## 1889

## rename variables for convenience
names(d6b)[names(d6b)=="Participant"] <- "id"
names(d6b)[names(d6b)=="NumberVisit"] <- "visit"
names(d6b)[names(d6b)=="MedMalariaRDT"] <- "rdt"
names(d6b)[names(d6b)=="record_id"] <- "record"

## subset variables of interest
vars <- c("id", "visit", "rdt")
d6b <- d6b[,vars]
head(d6b)

## sort
d6b <- d6b[order(d6b$id),]
head(d6b)

## remove odd entry
d6b <- d6b[!(d6b$id=="22~9.2"),]
dim(d6b) ## 1888

## clean up data entry variations
  table(d6b$visit)

  d6b[d6b$visit=="M32C",]$visit <- "m32c"  ## id 3158
  d6b[d6b$visit=="m32C",]$visit <- "m32c"
  d6b[d6b$visit=="m36C",]$visit <- "m36c"
  ## 6 misread as a 'b'
  d6b[d6b$visit=="M3bc",]$visit <- "m36c"  ## id 8055
  table(d6b$visit)

  ## visits not seen elsewhere but with rdt data NA
  d6b[d6b$visit=="c21b",] # id 4216
  d6b[d6b$visit=="c36c",] # id 3466
  d6a[d6a$id==4216,]; d6b[d6b$id==4216,]
  d6a[d6a$id==3466,]; d6b[d6b$id==3466,]
  ## remove
  d6b <- d6b[!(d6b$visit=="c21b"),]
  d6b <- d6b[!(d6b$visit=="c36c"),]
  dim(d6b) # 1886
  table(d6b$visit)

  ## visits NA but with rdt data
  d6b[d6b$visit=="",] # id 8163 3158
  d6a[d6a$id==8163,] # rdt==0
  d6a[d6a$id==3158,] # rdt==1
  ## change "" to NA
  d6b[d6b$visit=="",]$visit <- NA
  table(d6b$visit,useNA="ifany")

d6b$trimester <- ifelse(d6b$visit=="mEli",2,3)
table(d6b$trimester)
table(d6b$visit,d6b$rdt,useNA="ifany")

## dichotomize rapid test results
## assume "99" is code for missing data 
table(d6b$rdt) ## 1254 150 7 5 6 455
d6b$rdt <- ifelse(d6b$rdt==0, 0, ifelse(d6a$rdt==99,NA,1))
table(d6b$rdt,useNA="ifany") ## 1254 619 16
table(d6b$visit,d6b$rdt,useNA="ifany")
table(d6b$trimester,d6b$rdt,useNA="ifany")

head(d6b)
dim(d6b) # 1886

## remove duplicate rows
dim( d6b[duplicated(d6b),] ) # 42
d6b <- d6b[!duplicated(d6b),]
dim(d6b) ## 1844

## find duplicate id's still in (none found)
## flush out weird entries with just NA's
  table(d6b$trimester,useNA="ifany")

  d6b.na <- d6b[is.na(d6b$trimester),]

  d6b.2t <- d6b[d6b$trimester==2,]
  d6b.2t[is.na(d6b.2t$trimester),] 
  d6b.2t <- d6b.2t[!(is.na(d6b.2t$trimester)),] 
  d6b.2t <- d6b.2t[!duplicated(d6b.2t),] 

  d6b.3t <- d6b[d6b$trimester==3,]
  d6b.3t[is.na(d6b.3t$trimester),] 
  d6b.3t <- d6b.3t[!(is.na(d6b.3t$trimester)),] 
  d6b.3t <- d6b.3t[!duplicated(d6b.3t),] 
  
  d6b <- rbind(d6b.2t,d6b.3t,d6b.na)

head(d6b)
dim(d6b) # 1844

##########################
##### merge d6a, d6b #####
##########################

head(d6a)
head(d6b)
rdt <- rbind(d6a,d6b)
head(rdt)
dim(rdt) # 3219

## remove dupes
rdt <- rdt[order(rdt$id),]
dim( rdt[duplicated(rdt),] ) # 2
rdt <- rdt[!duplicated(rdt),]
dim(rdt) # 3217
head(rdt)

## remove observation with no visit information
## (there are other observations with the same id's which do have visit info)
rdt[is.na(rdt$trimester),]
# rdt[rdt$id==3158,]
# rdt[rdt$id==8163,]
rdt <- rdt[!is.na(rdt$trimester),]
dim(rdt) # 3215
head(rdt)

## collapse: remove information for visit, while keeping trimester
vars <- c("id","trimester","rdt")
rdt <- rdt[,vars]
head(rdt)
## remove duplicates resulting from collapse
rdt <- rdt[!duplicated(rdt),]
dim(rdt) # 2905
rdt[1:10,]

## freq table of response patterns
head(rdt)
library(plyr)
rdt.pat <- ddply(rdt, rdt ~ trimester, summarize, n=length(rdt))
rdt.pat
rdt.pat.cc <- rdt.pat[complete.cases(rdt.pat),]

## change long to wide
library(reshape)
rdt.w <- reshape(rdt,timevar="trimester",idvar=c("id"), direction="wide")
head(rdt.w)

## rename columns
names(rdt.w)[names(rdt.w)=="rdt.2"] <- "rdt2t"
names(rdt.w)[names(rdt.w)=="rdt.3"] <- "rdt3t"
head(rdt.w)
dim(rdt.w) # 1411 / 3204

## collapse: at least one positive result vs no positive result
rdt.w$rdt <- pmax(rdt.w$rdt2t, rdt.w$rdt3t, na.rm=T)
head(rdt.w)

table(rdt.w$rdt, useNA="ifany")  # 962 447 2
rdt.w[is.na(rdt.w$rdt),]

rdt.w2 <- rdt.w

names(rdt.w2)[names(rdt.w2)=="rdt2t"] <- 2
names(rdt.w2)[names(rdt.w2)=="rdt3t"] <- 3
names(rdt.w2)[names(rdt.w2)=="rdt"] <- "all"
head(rdt.w2)
## wide to long
rdt.l <- reshape(rdt.w2, 
        varying = c(2, 3, "all"), 
        timevar = "trimester",
        times = c(2, 3, "all"), 
        v.names = "rdt",
        direction = "long")
head(rdt.l)

#############
##### d7 ####
#############
## import data set 
d7 <- read.csv("export_form_07.csv", header=T)
dim(d7) ## 2295
head(d7)[,1:5]

## rename column for convenience
names(d7)[names(d7)=="Participant"] <- "id"
names(d7)[names(d7)=="LabMalarCount1"] <- "Lab1"
names(d7)[names(d7)=="LabMalarCount3"] <- "Lab3"
names(d7)[names(d7)=="record_id"] <- "record"
names(d7)[names(d7)=="NumberVisit"] <- "visit"

## subset variable of interest
vars <- c("id","visit", "Lab1","Lab3","record")
d7 <- d7[,vars]
head(d7)

## Dichotomize lab results
## codes for missing data : 999, others?
table(d7$Lab1)
table(d7$Lab1,d7$Lab3,useNA="ifany")
d7$Lab1 <- ifelse(d7$Lab1==0,0, ifelse(d7$Lab1==999,NA,1))
d7$Lab3 <- ifelse(d7$Lab3==0,0, ifelse(d7$Lab3==999,NA, ifelse(d7$Lab3==777,-1,1)))

table(d7$Lab1,d7$Lab3)
# lab10 <- d7[(d7$Lab1==1 & d7$Lab3==0),]
# lab10[!is.na(lab10$id),]

## Combine lab results into a single result
d7$lab <- pmax(d7$Lab1,d7$Lab3,na.rm=T)
table(d7$lab)
# d7[d7$lab==-1,] # id 8199,  4177
d7 <- d7[d7$id!="4177",]
d7 <- d7[d7$id!="8199",]
table(d7$lab,useNA="ifany")
head(d7)

## clean up data entry variations
table(d7$visit)
d7[d7$visit=="m36C",]$visit <- "m36c"
d7[d7$visit=="M36c",]$visit <- "m36c"
d7[d7$visit=="M36C",]$visit <- "m36c"
d7[d7$visit=="m360",]$visit <- "m36c"
d7[d7$visit=="MEII",]$visit <- "mEli"
d7[d7$visit=="meli",]$visit <- "mEli"
d7[d7$visit=="mElI",]$visit <- "mEli"
d7[d7$visit=="MELI",]$visit <- "mEli"
d7[d7$visit=="mELi",]$visit <- "mEli"
table(d7$visit)

head(d7)
d7$trimester <- ifelse(d7$visit=="mEli",2,3)
table(d7$trimester)

vars <- c("id","trimester","lab")
d7 <- d7[,vars]
head(d7)

## freq table of response patterns
head(d7)
lab.pat <- ddply(d7, lab ~ trimester, summarize, n=length(lab))
lab.pat
lab.pat.cc <- lab.pat[complete.cases(lab.pat),]
lab.pat.cc

## long to wide
library(reshape)
lab.w <- reshape(d7,timevar="trimester",idvar=c("id"),direction="wide")
head(lab.w)

## rename columns
names(lab.w)[names(lab.w)=="lab.2"] <- "lab2t"
names(lab.w)[names(lab.w)=="lab.3"] <- "lab3t"
head(lab.w)
dim(lab.w) # 1444

## collapse: at least one positive result vs no positive result
lab.w$lab <- pmax(lab.w$lab2t, lab.w$lab3t, na.rm=T)

table(lab.w$lab, useNA="ifany")  # 1193 186 65
lab.w[is.na(lab.w$lab),]

head(lab.w)

lab.w2 <- lab.w

names(lab.w2)[names(lab.w2)=="lab2t"] <- 2
names(lab.w2)[names(lab.w2)=="lab3t"] <- 3
names(lab.w2)[names(lab.w2)=="lab"] <- "all"
head(lab.w2)
## wide to long
lab.l <- reshape(lab.w2, 
                 varying = c(2, 3, "all"), 
                 timevar = "trimester",
                 times = c(2, 3, "all"), 
                 v.names = "lab",
                 direction = "long")
head(lab.l)

####################################
##### merge (d6a, d6b) with d7 #####
####################################

## merge wide
rdt.w$id <- as.character(rdt.w$id)
lab.w$id <- as.character(lab.w$id)
d67.w <- merge(rdt.w, lab.w, by=c("id"), all=T)
head(d67.w)
d67.w[1:20,]


## merge long
rdt.l$id <- as.character(rdt.l$id)
lab.l$id <- as.character(lab.l$id)
d67 <- merge(rdt.l, lab.l, by=c("id","trimester"), all=T)
dim(d67) # 4773
head(d67)

## select only those with values collapsed over trimesters 2 & 3
d67t23 <- d67[d67$trimester=="all",]
head(d67t23)
d67t23[1:20,]
dim(d67t23) # 1591

##
d67t23.cc <- d67t23[complete.cases(d67t23),]
dim(d67t23.cc) # 1232 
head(d67t23.cc)

# tutu <- table(d67t23.cc$lab, d67t23.cc$rdt)
# 
# library(psych)
# wkappa(tutu)


##############
##### PCR ####
##############

fanta <- read.csv("FANTA_Pfldh_results_March2013.csv", header=T)
dim(fanta) ## 1585
head(fanta)

names(fanta)[names(fanta)=="Patient.ID"] <- "id"
names(fanta)[names(fanta)=="Spec.Type"] <- "visit"
names(fanta)[names(fanta)=="Pfldh.Positive"] <- "pcr"
names(fanta)[names(fanta)=="Pfldh_Qty"] <- "pcr_qty"

vars <- c("id","visit","pcr","pcr_qty")
fanta <- fanta[,vars]
fanta[1:20,]

fanta$pcr_qty <- ifelse(fanta$pcr==0,0,fanta$pcr_qty)
range(fanta$pcr_qty) # 0.00 1.06

table(fanta$pcr,useNA="ifany")
##
fanta$pcr_qty_10m3 <- ifelse(fanta$pcr_qty>=0.001,1,0) 
table(fanta$pcr_qty_10m3)
##
fanta$pcr_qty_10m4 <- ifelse(fanta$pcr_qty>=0.0001,1,0) 
table(fanta$pcr_qty_10m4)

head(fanta)

table(fanta$visit) ## 674 487 424

## collapse: visit -> trimester
fanta$trimester <- ifelse(fanta$visit=="mEli",2,3)
table(fanta$trimester) ## 674 911
head(fanta)

## remove visit
vars <- c("id","trimester","pcr","pcr_qty")
fanta <- fanta[,vars]
head(fanta)

## long to wide
library(reshape)
fanta.w <- reshape(fanta,timevar="trimester",idvar=c("id"),direction="wide")
head(fanta.w)

names(fanta.w)[names(fanta.w)=="pcr.2"] <- "pcr2t" 
names(fanta.w)[names(fanta.w)=="pcr.3"] <- "pcr3t" 
names(fanta.w)[names(fanta.w)=="pcr_qty.2"] <- "pcr2tq"
names(fanta.w)[names(fanta.w)=="pcr_qty.3"] <- "pcr3tq" 

## 
fanta.w$pcr <- pmax(fanta.w$pcr2t, fanta.w$pcr3t,na.rm=T)
fanta.w$pcrq <- pmax(fanta.w$pcr2tq, fanta.w$pcr3tq,na.rm=T)

head(fanta.w)

###########################################
##### merge (d6a, d6b, d7) with fanta #####
###########################################

ds.w <- merge(d67.w, fanta.w, by="id", all=T)
names(ds.w)[names(ds.w)=="lab"] <- "micro" 
names(ds.w)[names(ds.w)=="lab2t"] <- "micro2t"
names(ds.w)[names(ds.w)=="lab3t"] <- "micro3t"
dim(ds.w) # 1592
head(ds.w)

ds.nocov <- ds.w
dim(ds.nocov) # 1592
head(ds.nocov)
ds.nocov[1:20,]

###########################################
##### merge (d6a, d6b, d7, fanta) with d2 #####
###########################################

d2 <- read.csv("export_form_02.csv", header=T)
names(d2)[names(d2)=="Participant"] <- "id" 
names(d2)[names(d2)=="ScrMotherAge"] <- "age"
names(d2)[names(d2)=="ScrMotherChild"] <- "gravidity"
vars <- c("id","age","gravidity")
d2 <- d2[,vars]
dim(d2) # 1776
head(d2)

# ud6 <- union(d6a$id,d6b$id)
# length(ud6) # 1411
# length( Reduce(intersect, list(ud6,d7$id)) ) # 1264
# length( Reduce(intersect, list(d2$id,ud6,d7$id)) ) # 1185

ds.cov <- merge(d2, ds.w, by="id")
dim(ds.cov) # 1764
head(ds.cov)

dim( ds.cov[duplicated(ds.cov),] ) # 448
ds.cov <- ds.cov[!duplicated(ds.cov),]
dim(ds.cov) # 1316
head(ds.cov)

## categorize age and gravidity
# range.int <- function(vector){
#   min <- vector[which.min(vector)]
#   max <- vector[which.max(vector)]
#   out <- c(min,max)
#   return(out)
# }
# range.int(ds.cov$age) # 9 88 
# range.int(ds.cov$gravidity) # 0 63
ds.cov$under35 <- ifelse(ds.cov$age<35,1,0)
ds.cov$under20 <- ifelse(ds.cov$age<20,1,0)
ds.cov$firstkid <- ifelse(ds.cov$gravidity>0,1,0)
dim(ds.cov) # 1316
head(ds.cov)

varsout <- c("gravidity","age")
ds.cov <- ds.cov[,-which(names(ds.cov) %in% varsout)]
dim(ds.cov) # 1316
head(ds.cov)

## examine duplicate id's
dupes <- as.character(ds.cov[duplicated(ds.cov$id),]$id)
ds.cov[(ds.cov$id %in% dupes), ]

## remove id's with contradictory info on under35, under20, firstkid
# tossout <- c("3211","3292","3458","3488","5028")
# ds.cov <- ds.cov[!(ds.cov$id %in% tossout), ]

## remove duplicate rows
ds.cov <- ds.cov[!duplicated(ds.cov),]

dim(ds.cov) # 1310
head(ds.cov)


table(ds.cov$firstkid, useNA="ifany") # missing 2, id 4045, 8340
ds.cov[is.na(ds.cov$firstkid),]

table(ds.cov$under20, useNA="ifany")  # missing 1, id 8194
table(ds.cov$under35, useNA="ifany")
ds.cov[is.na(ds.cov$under20),] 

head(ds.cov)

## check for missing values
unique(ds.cov$id)
table(ds.cov$rdt2t,useNA="ifany") # miss 34
table(ds.cov$rdt3t,useNA="ifany") # miss 271

write.table(ds.cov, "workingds.csv", sep=",")


###########################################
##### export data table summaries to CSV #####
###########################################

library(plyr)
pat23 <- ddply(ds.cov, ~ micro + pcr + rdt, summarize, n=length(id) )
pat23.cc <- pat23[complete.cases(pat23),]
pat23.cc
write.csv(pat23.cc, file = "pat23_cc.csv")

pat2 <- ddply(ds.cov, ~ micro2t + pcr2t + rdt2t, summarize, n=length(id) )
pat2.cc <- pat2[complete.cases(pat2),]
pat2.cc
write.csv(pat2.cc, file = "pat2_cc.csv")

pat3 <- ddply(ds.cov, ~ micro3t + pcr3t + rdt3t, summarize, n=length(id) )
pat3.cc <- pat3[complete.cases(pat3),]
pat3.cc
write.csv(pat3.cc, file = "pat3_cc.csv")



