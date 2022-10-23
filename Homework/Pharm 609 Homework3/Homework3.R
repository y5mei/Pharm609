####################################################################
# Pharm 609 Homework 3
# Yaowen Mei (20470193)
# 
####################################################################
# Read a csv file
library(stats4)
library(MASS)
library(pander)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(matrixStats)
library(varhandle)
library(nlme)

# improt the data
data <- read.csv("CIPROFLOXACIN.csv")
head(data)
data$GEND <- factor(data$GEND,levels=c(1,2),
                     labels=c("male","female"))

data <- groupedData(DV~TIME|ID,data = data)
data.sub = gsummary(data)
data.sub1 = gsummary(data, inv=T)
data.sub1
plot(data)
sort(table(data$ID))
table(data$GEND)
sapply(data.sub[,c("WT","AGE")],summary)

table(data.sub$GEND)

#demographic characteristics
par(mfrow=c(1,3))
par(mar=c(4,4,2,2))
hist(data.sub$WT, main='Weight Histogram', xlab='Weight(kg)')
hist(data.sub$AGE,main='Age Histogram', xlab='Age(years)')
boxplot(DV~GEND,xlab="Sex", ylab="Concentration (mg/L)", names=c("Males","Females"),data=data.sub)
mtext("n.male=6, n.male=14",side=3)

# Print the corralation table:
dat <- subset(data.sub, select = -c(ID, GEND,RATE,AMT,DV, TIME))
cor_table = rcorr(as.matrix(dat),type= 'pearson')
plot(dat)
cor_table$r
cor_table$P

# plot for the bootstrap
bootdata <- read.csv("boot.csv")
head(bootdata)
par(mfrow=c(1,7))
par(mar=c(4,4,2,2))

hist(bootdata$tvV)
hist(bootdata$tvCl)
hist(bootdata$dVdWT)
hist(bootdata$dCldWT)
hist(bootdata$dVdAGE)
hist(bootdata$dCldAGE)
hist(bootdata$CEps)


