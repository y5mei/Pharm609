####################################################################
# Pharm 609 Homework 3
# Yaowen Mei (20470193)
# Question 1
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


data <- read.csv("CIPROFLOXACIN.csv")
head(data)


data.new <- groupedData(DV~TIME|ID,data = data)
subdata = gsummary(data.new)
plot(data.new)

hist(subdata$WT)
hist(subdata$AGE)


dat <- subset(subdata, select = -c(ID, GEND,RATE,AMT,DV))
cor_table = rcorr(as.matrix(dat),type= 'pearson')
cor_table


dat <- subset(data, select = -c(ID, GEND,RATE,AMT,DV))
cor_table = rcorr(as.matrix(dat),type= 'pearson')
cor_table
corrplot(cor(dat),type = 'lower',tl.col="black")




par(mfrow=c(1,2))
par(mar=c(4,4,2,2))
Concentration <- data$DV
hist(Concentration,main='Concentration Histogram')
qqnorm(Concentration)
quantile(Concentration,probs=c(0.25,0.75))
qqline(Concentration)





