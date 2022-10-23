####################################################################
# Pharm 609 Homework 2
# Simple and Multiple Linear Regression
# Yaowen Mei (20470193)
####################################################################

####################################################################
# Question 1
# Perform an exploratory analysis of Carboplatin Clearance as the response
# variable and potential predictors Sex, Age, Weight, BSA and GFR.
# a. Describe and summarize the distribution of the variables involved
# b. Provide a preliminarily assessment of the linear relationships 
# 	 between covariates and response
# c. Assess whether the response variable may require a logarithmic transformation.
# d. Assess whether any categorical variables may have an effect in the response.
####################################################################
# Read a csv file
install.packages('car')
install.packages('pander')
install.packages('Hmisc')
install.packages("corrplot")
install.packages('varhandle')
install.packages('matrixStats')
library(car)
library(stats4)
library(MASS)
library(varhandle)
library(Hmisc)
library(corrplot)
library(matrixStats)
data <- read.csv("Dataset/Carboplatin.csv")
####################################################################
# Question 1
# a. Describe and summarize the distribution of the variables involved
####################################################################
#Change male, female into 0,1
dat$Sex<-factor(data$Sex,labels = c('0','1'))
#Convert factor into numeric
dat$Sex<- unfactor(dat$Sex)
#Keep only the covariates and response variable
dat=subset(dat, select = -c(Patient,Dose,AUC.Carbo))
#colMeans(dat)
head(dat)
names(dat)
## calculate the mean of each variable
dat.mean=data.frame(colMeans(dat))
colnames(dat.mean) <- c("Mean")
dat.mean
# calculate the quantiles, median, and range
dat.quantiles = colQuantiles(as.matrix(dat),na.rm=TRUE)
colnames(dat.quantiles) <- c("Min","1st Qu.","Median","3rd Qu.","Max")
dat.quantiles
## calculate the variance
dat.var=as.data.frame(colVars(as.matrix(dat)))
colnames(dat.var) <- c("Variance")
rownames(dat.var) <- names(dat)
dat.var
## output the summarize the distribution of the variables in this study
sdistri <- cbind(dat.mean,dat.quantiles,dat.var)
round(sdistri,3)
####################################################################
# Question 1
# b. Provide a preliminarily assessment of the linear relationships 
# 	 between covariates and response
####################################################################
par(mfrow=c(3,2))
par(mar=c(4,4,2,2))
attach(dat)
# Sex
boxplot(CL.Carbo~Sex,xlab="Sex",ylab="CL (L/min)", names=c("Females","Males"),data=dat)
# Age
plot(Age,CL.Carbo,ylim=range(CL.Carbo), xlab="Age",ylab="CL (L/min)")
abline(lm(CL.Carbo~Age,data=dat))
# Weight
plot(Weight,CL.Carbo,ylim=range(CL.Carbo), xlab="Weight (kg)",ylab="CL (L/min)")
abline(lm(CL.Carbo~Weight,data=dat))
# BSA
plot(BSA,CL.Carbo,ylim=range(CL.Carbo), xlab="BSA (m^2)",ylab="CL (L/min)")
abline(lm(CL.Carbo~BSA,data=dat))
# GFR
plot(GFR,CL.Carbo,ylim=range(CL.Carbo), xlab="GFR (mL/min)",ylab="CL (L/min)")
abline(lm(CL.Carbo~GFR,data=dat))
# example
plot(rnorm(100), rnorm(100),
     xlab = expression(hat(mu)[0]), ylab = expression(alpha^beta),
     main = expression(paste("Plot of ", alpha^beta, " versus ", hat(mu)[0])))
####################################################################
# Question 1
#c. Assess whether the response variable may require a logarithmic transformation.
####################################################################
par(mfrow=c(1,2))
hist(CL.Carbo)
qqnorm(CL.Carbo)
qqline(CL.Carbo)
####################################################################
# Question 1
# d. Assess whether any categorical variables may have an effect in the response.
####################################################################
t.test(CL.Carbo~Sex,var.equal=T,data)

####################################################################
# Question 2
# (a)	Perform a pre-modeling collinearity assessment via: 
# (i) correlation table and 
# (ii) individual simple linear fits for each covariate 
#      vs. Carboplatin Clearance.
####################################################################
# Correlation table

(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2
mcor <- cor(dat)
corrplot(mcor, type="lower", order="hclust", tl.col="black", tl.srt=45)
cor_table = rcorr(as.matrix(subset(dat, select = -c(Sex)),type='pearson'))

cor_table <- round(as.matrix(cor_table[[1]]),2)
cor_table
lower.tri(cor_table)
cor_table[lower.tri(cor_table)] <- NA
cor_talbe
pander(cor_talbe, caption = 'foo')
# Sex (individual simple linear fits)
fit.sex <- lm(CL.Carbo ~ Sex, data=dat)
a<- cbind(summary(fit.sex)$coef[,c(1,2,4)],R2adj=summary(fit.sex)$r.squared)
# Age (individual simple linear fits)
fit.age <- lm(CL.Carbo ~ Age, data=dat)
b<- cbind(summary(fit.age)$coef[,c(1,2,4)],R2adj=summary(fit.age)$r.squared)
# Weight (individual simple linear fits)
fit.wt <- lm(CL.Carbo ~ Weight, data=dat)
summary(fit.wt)
# BSA (individual simple linear fits)
fit.bsa <- lm(CL.Carbo ~ BSA, data=dat)
summary(fit.bsa)
# GFR (individual simple linear fits)
fit.gfr <- lm(CL.Carbo ~ GFR, data=dat)
summary(fit.gfr)

mysummaryfunction <- function(covariate){
  a<- summary(lm(CL.Carbo ~ covariate, data=dat))
  b<- a$coef[,c(1,2,4)]
  c<- a$r.squared
  d<- cbind(b,R2adj=c)
  # get the variable name as a string
  #vname <- deparse(substitute(covariate))[1]
  rownames(d)<- c("Intercept","Slope")
  return(d) 
}
mylist <- as.list(subset(dat, select = -c(CL.Carbo)))
lapply(mylist,"mysummaryfunction")
####################################################################
# Question 2
# (b)	Identify the set of variables in (a) that may contribute to 
#     collinearity (if any).
####################################################################
# the P-val indicates that 1) Weight&Age, 2) BSA&Weight, 3) BSA&Age,  
# 4) GFR&Weight, 5) GFR&Age, 6) GFR&BSA this 6 pairs of variables 
# that may contribute to the collinearity
#
#
#
####################################################################
# Question 2
# (c)	Give the interpretation of the estimated slope when using 
# 		the models with sex and age variables in (a).
####################################################################
# For each 1 unit increase in Age(month), there is statistically significant
# 0.43701 increase in the mean Carboplatin Clearance (p-val = 7.51E-6)
#
# As the slope for Sex is -5.9 with a very large P value (p-val = 0.682) 
# There is no statistically significant that gender is a dominant factor 
# in the mean CL.Carbo, accroding to the simple regression case with k=1.
#

####################################################################
# Question 2
# (d)	Produce scatter plots for each variable in (a) adding the 
# 		fitted line, the value of the coefficient of determination R2 
# 		and report any evidence of a relationship other than linear 
# 		(e.g. curvilinear, no slope). Provide an interpretation of R2.
# 		Hint: use the mtext() function to add text to the plots.
####################################################################

par(mfrow=c(3,2))
par(mar=c(4,4,2,2))
attach(dat)
plot(Sex,CL.Carbo,ylim=range(CL.Carbo), xlab="Sex",ylab="CL (L/min)")
abline(lm(CL.Carbo~Sex,data=dat))
mtext(expression(R^2==1111))
plot(Age,CL.Carbo,ylim=range(CL.Carbo), xlab="Age",ylab="CL (L/min)")
abline(lm(CL.Carbo~Age,data=dat))
mtext(expression(R^2==1111))
plot(Weight,CL.Carbo,ylim=range(CL.Carbo), xlab="Weight (kg)",ylab="CL (L/min)")
abline(lm(CL.Carbo~Weight,data=dat))
mtext(expression(R^2==1111))
plot(BSA,CL.Carbo,ylim=range(CL.Carbo), xlab="BSA (m^2)",ylab="CL (L/min)")
abline(lm(CL.Carbo~BSA,data=dat))
mtext(expression(R^2==1111))
plot(GFR,CL.Carbo,ylim=range(CL.Carbo), xlab="GFR (mL/min)",ylab="CL (L/min)")
abline(lm(CL.Carbo~GFR,data=dat))
mtext(expression(R^2==1111))
par(mfrow=c(4,1))
plot(fit.age,1)
plot(fit.wt,1)
plot(fit.bsa,1)
plot(fit.gfr,1)

plot_matrix <- matrix(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,7,8,9),12,2)
plot_matrix
# One figure in row 1 and two figures in row 2
a = matrix(c(1,),4,2, byrow = TRUE)
a
attach(mtcars)
layout(a)
plot(GFR,CL.Carbo,ylim=range(CL.Carbo), xlab="GFR (mL/min)",ylab="CL (L/min)")
abline(lm(CL.Carbo~GFR,data=dat))
mtext(expression(R^2==1111))
plot(fit.age,1)
plot(fit.wt,1)
plot(fit.bsa,1)
plot(fit.gfr,1)
####################################################################
# Question 2
# (e)	Fit a linear model for Carboplatin Clearance vs. Age, Weight 
# and BSA. Does the conclusion regarding the significance of these 
# variables agree with the corresponding individual simple linear fits
# performed in (a)? Support your answer.
####################################################################
# Age and weight fit
fit.ageandweight <- lm(CL.Carbo ~ Age+Weight, data=dat)
summary(fit.ageandweight)
# No, Age seems has no effect on CL in this case, this is wired.
####################################################################
# Question 3
# Forward selection
####################################################################
# Find the covariable with the largest Pr val in simple linear regrasiion
# apply lm function to a list of variables
b = apply(dat, 2, function(dat) {
  a=summary(lm(CL.Carbo~dat))
  b=a$coef
  return (b[1,])})
rownames(b)
c= b["Estimate",]
order(c,decreasing = TRUE)
# Sort the Pr val metrix by Pr val
b[with(b, order(-"Pr(>|t|)")), ]

dd <- data.frame(b = c(5,6,7,8),
                 x = c(40, 60, 80, 20), y = c(8, 3, 9, 9),
                 z = c(1, 1, 1, 2))
dd
row.names(dd)
colnames(dd)
dd[with(dd, order(b)), ]

summary(fit.sex)$coef[2,]
ests.w.cl <- rbind(
  summary(fit.sex)$coef[3,],
  summary(fit.age)$coef[3,],
  summary(fit.bsa)$coef[3,],
  summary(fit.gfr)$coef[3,])

a = names(dat)[1:2]
b = (1:6)
assign(a,b)
myfunc = function (x){
  a=paste("fit.",x,sep = "")
  }
myfunc(a)
fit.sex <- lm(CL.Carbo ~ Sex, data=dat)

lm(CL.Carbo ~ Age, data=dat)
lm(CL.Carbo ~ Sex, data=dat)

pander(table(mtcars$am), caption = 'foo')
cor_table

# Age (individual simple linear fits)
fit.age <- lm(CL.Carbo ~ Age, data=dat)
# Weight (individual simple linear fits)
fit.wt <- lm(CL.Carbo ~ Weight, data=dat)
# BSA (individual simple linear fits)
fit.bsa <- lm(CL.Carbo ~ BSA, data=dat)
# GFR (individual simple linear fits)
fit.gfr <- lm(CL.Carbo ~ GFR, data=dat)

# define my summary function that produce a list of summary for each induvidual simple linear fits
newsummaryfunction <- function(covariate){
  a<- summary(lm(CL.Carbo ~ covariate, data=dat))
  b<- a$coef[,c(1,2,4)]
  c<- a$r.squared
  d<- cbind(b,R2adj=c)
  # get the variable name as a string
  #vname <- deparse(substitute(covariate))[1]
  rownames(d)<- c("Intercept","Slope")
  return(d) 
}
mylist <- as.list(subset(dat, select = -c(CL.Carbo)))
table_lm = lapply(mylist,"mysummaryfunction")
pander(table_lm,caption = 'The Tabel of simple linear regressions CL.Carbon vs. potential covariates')
```
