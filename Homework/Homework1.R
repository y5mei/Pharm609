####################################################################
# Pharm 609 Homework 1
# Yaowen Mei (20470193)
# Question 1
####################################################################
# Read a csv file
data <- read.csv("Dataset/5FuCL.csv")
####################################################################
# Question 1.
# a) Make a histogram and Normal Q-Q plot of the CL variable
# b) Make a histogram and Normal Q-Q plot of the logCL variable
# c) Comment on the effect of transforming the data
####################################################################
# Part a) histogram and Normal Q-Q plot of CL
Clearance <- data$CL
hist(Clearance,main='Clearance Histogram')
qqnorm(Clearance)
qqline(Clearance)
# Part b) histogram and Normal Q-Q plot of logCL
logCL = log(Clearance)
hist(logCL, main = 'Log of Clearance Histogram')
qqnorm(logCL)
qqline(logCL)
# Part c) Comment on the effect of transforming the data:
# after the transforming, we can notice that logCL is abey normal
# distrubtion
# the shape of Q-Q plot does not seem to be linearized after 
# the transforming

####################################################################
# Question 2.
# a) Calculate the corresponding mean, SD, CV and Median for the 
#    logNormally distributed variable CL.
# b) Calculate the approximate CV of CL.Compare its value with the exact CV
####################################################################
# Part a) find the mean, SD,CV, and median for CL
# we found ln(X)~N(-0.252, 0.379^2), 
# X is lognormal ~ LN(0.835,0.328^2)
# Name     Value
# 1   Mean 0.8350874
# 2     SD 0.3931485
# 3     CV 0.3283133
# 4 Median 0.7771818
u <- mean(logCL)
sigma <- sd(logCL)
Mean = exp(u+sigma^2/2)
CV = sqrt(exp(sigma^2)-1)
SD = Mean*CV
Median = exp(u)
data.frame(Name=c('Mean','SD','CV','Median'),
           Value=c(Mean,CV,SD,Median))
# Part b) find the difference between approximate cl and exact cl
# the difference between approximate CL and its exact value is 
# about 3.6%; therefore, it is a good approximation in this case
approximate.CL = sigma
difference <-(CV-approximate.CL)
difference.per <- (CV-approximate.CL)/CV
cat("The difference is",difference,"%",", and this
    value is",difference.per,"% of the exact CL value")


####################################################################
# Question 3.
# a) Change the label of sex variable from numberical to strings ('male/female').
# b) Write a fomulae for 90% CI for the mean logCI, 
#    and calcualte it for both female and male.
# c) back transform 90% CI for logCI to 90% CI for CI.
####################################################################
# Part a) change the lable of sex variable
data$Sex <- factor(data$Sex, labels = c('male','female'))
# Part b) calcualte 90% for logCI for female and male
# foumlae: (Y-z*SE(Y)~Y+z*SE(Y))
cl.male <- log(data$CL[data$Sex=='male'])
cl.female<- log(data$CL[data$Sex=='female'])

alpha = 0.1
z = qnorm(1-alpha/2)
sd.male = sd(cl.male)
sd.female = sd(cl.female)
se.male = sd.male/sqrt(length(cl.male))
se.female = sd.female/sqrt(length(cl.female))

logCL.male <- c(mean(cl.male)-z*se.male,mean(cl.male)+z*se.male)
logCL.female <- c(mean(cl.female)-z*se.female,mean(cl.female)+z*se.female)
cat('The 90% CL for male is from',logCL.male[1], 'to',logCL.male[2])
cat('The 90% CL for female is from',logCL.female[1], 'to',logCL.female[2])
# Part c) back transform
CL.male <- exp(logCL.male)
CL.female <- exp(logCL.female)
cat('The 90% CL for male is from',CL.male[1], 'to',CL.male[2])
cat('The 90% CL for female is from',CL.female[1], 'to',CL.female[2])

####################################################################
# Question 4.
# a) Change the label of sex variable from numberical to strings ('male/female').
# b) Construct a box plot of the temperature for each gender.
# c) Briefly describe the main features of the box plots and compare between genders.
####################################################################
# Part a) change label
bodytemp <- read.table("Dataset/BodyTemps.txt",header = TRUE)
head(bodytemp)
bodytemp$sex <- factor(bodytemp$sex,labels = c('male','female'))

# Part b) construct a box plot
boxplot(bodytemp$temp~bodytemp$sex,range=0,ylab="Temperature (Fahrenheit)")

# Part c) Describe the main features
# It is clearly shown in the box plot that the median value of the body temperature 
# for both male and female are very close (less than 0.5 F); however, male has a lower 
# temperature while female has a higher temperature. The male temperature changes in a
# smaller range than female as the the whisker line for male is longer than the one for female.
# In a addition to these, the interquartile range for female is smaller than male, which indicates
# that the female body temperature data are more consistent.

####################################################################
# Question 5.
# a. State the null and alternative hypotheses in words as well as mathematically.
# b. State the test statistic under H0 and its sampling distribution, assuming that:
#   (i) these data are approximately normally distributed (i.e. Student???s t) and that
#   (ii) both the female and male populations have the same variance.
# c. Explain briefly what the concept of ???sampling distribution??? of the T statistic means.
####################################################################
# Part a) 
# H0: the mean body temperature for male is as same as that for female.
# H1: the mean body temperature for male is different with that for female.
# H0: u(male) = u(female) => u(male)-u(female) = 0
# H1: u(male) /= u(female) => u(male)-u(female) /= 0

# Part b)
# The test statistic under H0 has a T-distribution, for every test statistic, there is a
# standard error (SE) associate with it.
# The test statistic for the difference in mean in body temperature:
# T = (T.male-T.female)/sqrt(sp^2*(1/num.male+1/num.female))
# The pooled variance in this case:
# sp^2 = ((n.male-1)*s.male^2 + (n.female-1)*s.female^2 )/(n.female+n.male-2)

# Part c)
# "sampling distribution" in general is defined as the probability distribution of a given
# statistic based on a random sample. By estimating its sampling variation, one can use 
# sampling distribution to evaluate how close is it between the value of statistic and
# the unknown population paramter.  



####################################################################
# Question 6
# Perform a hypothesis test to compare the mean temperatures between genders.
# a). Ingeneral, how is the p-value interpreted?
# b). What do you conclude from its value, in the context of the data? Use a 0.05
# significance level.
# c). Does the conclusion drawn from the p-value agree with the 95% CI provided in the
# output? Why?
####################################################################
# t-test
t.test(temp~sex,var.equal=T,data=bodytemp)
# The result is :
# Two Sample t-test
# data:  temp by sex
# t = -2.2854, df = 128, p-value = 0.02393
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.53963938 -0.03882216
# sample estimates:
#   mean in group male mean in group female 
#             98.10462             98.39385 

# Part a)
# In general, p-value is the probability for T to have a value at 
# least as extreme as T0 by chance alone. In the case that p-value is 
# very small, one can conclude that the data obtained is less compatible
# with the null. As it is said: "If the p-value is low, the null must go".

# Part b)
# Use a 0.05 significance level, one can conclude that there is a statistically 
# significant difference in the mean body temperature between different 
# genders (p-value = 0.02393).

# Part c)
# The p-value and the CI will always lead us to the same conclusion. In this case,
# the conclusion drawn from the p-value is the same as the one get from 
# 95% CI. The reason for this is that the 95% CI of -0.54~-0.039 does not include 
# zero-our hypothesized mean; meanwhile, p-value is less than alpha. If p-value is 
# greater than 0.05, then 95%CI must include the hypothesized mean.

####################################################################
# Question 7
# Perform a hypothesis test to compare the mean temperatures for genders with 
# the postulated value of 98.6 degrees Fahrenheit.
# a). State the hypotheses in words and also by using ufem and umale.
# b). Perform the test using t.test() Rfunction.
# c). calculate for males and females, the value of
#   i. the observed test statistics 
#   ii. p-values and
#   iii. 95% CI???s.
# d). Briefly explain what a 95%CI means in the context of the data.
# e). Compare the results found for each gender
####################################################################
# Part a) for male:
# H0:the mean body temperature for male is equal to 98.6 F.
# H0: umale = 98.6F
# H1:the mean body temperature for male is not equal to 98.6 F.
# H1: umale /= 98.6F

# Part a) for female:
# H0:the mean body temperature for female is equal to 98.6 F.
# H0: ufemale = 98.6F
# H1:the mean body temperature for female is not equal to 98.6 F.
# H1: ufemale /= 98.6F


# Part b) t-test for male:
# 
# One Sample t-test
# 
# data:  bodytemp.male$temp
# t = -5.7158, df = 64, p-value = 3.084e-07
# alternative hypothesis: true mean is not equal to 98.6
# 95 percent confidence interval:
#   97.93147 98.27776
# sample estimates:
#   mean of x 
# 98.10462 
bodytemp.male <- bodytemp[bodytemp$sex=='male',]
t.test(bodytemp.male$temp,mu=98.6)

# Part b) t-test for female:
# 
# One Sample t-test
# data:  bodytemp.female$temp
# t = -2.2355, df = 64, p-value = 0.02888
# alternative hypothesis: true mean is not equal to 98.6
# 95 percent confidence interval:
#   98.20962 98.57807
# sample estimates:
#   mean of x 
# 98.39385 
bodytemp.female <- bodytemp[bodytemp$sex=='female',]
t.test(bodytemp.female$temp,mu=98.6)

# Part c) calculate for male
mean.male <- mean(bodytemp.male$temp)
sds.male <- sd(bodytemp.male$temp)
n.male <- length(bodytemp.male$temp)
t.statistic.male <- (mean.male-98.6)/(sds.male/sqrt(n.male))
p.value.male <- 2*pt(t.statistic.male,df=(n.male-1))
CIlow.male <- mean.male - qt(.975,df=(n.male-1))*(sds.male/sqrt(n.male))
CIhig.male <- mean.male + qt(.975,df=(n.male-1))*(sds.male/sqrt(n.male))
cat('The t.sta for male is',t.statistic.male,'and the p-value for male is ',p.value.male,'. The 95% CI for male is from',CIlow.male, 'to',CIhig.male)

# Part c) calculate for female
mean.female <- mean(bodytemp.female$temp)
sds.female <- sd(bodytemp.female$temp)
n.female <- length(bodytemp.female$temp)
t.statistic.female <- (mean.female-98.6)/(sds.female/sqrt(n.female))
p.value.female <- 2*pt(t.statistic.female,df=(n.female-1))
CIlow.female <- mean.female - qt(.975,df=(n.female-1))*(sds.female/sqrt(n.female))
CIhig.female <- mean.female + qt(.975,df=(n.female-1))*(sds.female/sqrt(n.female))
cat('The t.sta for female is',t.statistic.female,'and the p-value for female is ',p.value.female,'. The 95% CI for female is from',CIlow.female, 'to',CIhig.female)

# Part d)
# 95% CI means that we have 95% chance to say that the CI contains 
# the true mean body temperature.

# As the two t-tests shown in b), neither the mean body temperature of male nor female 
# is 98.6 F. Therefore, I think the population mean of body temperature may not be 98.6 F.
