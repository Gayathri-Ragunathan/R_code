# Performance Data
# Clearing the workspace
rm(list=ls())

### Library Packages
library(foreign)
library(stargazer)
library(haven)
library(ggplot2)
library(Hmisc)
library(chron)
library(lattice)
library(dummies)
library(lfe)
library(sandwich)
library(lmtest)
library(miceadds)
library(multiwayvcov)

# Importing the performance data
perf<- read_dta("C:/Users/Vasanth Ragunathan/Desktop/WFH/Performance.dta")
View(perf)

# Exploring the dataset
perf_d<-data.frame(perf)
stargazer(perf_d,type="text")

# Creating html file in the working directory
des.html<-stargazer(perf_d,type="html",out="perf_variables.html")

# Exploring the data graphically
perf_d<-within(perf_d,{D_gender<-ifelse(men==1, "men", "women")})
perf_d<-within(perf_d,{treatmentperiod<-ave(experiment_treatment,year_week,FUN=max)})
View(perf_d)
describe(perf_d)
summary(perf_d)
# Plotting histograms
histogram(~ perform1 | D_gender, data=subset(perf_d, treatmentperiod==1), nint =25, col="lightblue")
histogram(~ logdaysworked | D_gender, data=perf_d, nint =25, col="lightblue")
histogram(~ grosswage | D_gender, data=perf_d, nint =25, col="lightblue")
histogram(~ children | D_gender, data=perf_d, nint =25, col="lightblue")# children = 0-> no children, 1-> has children
histogram(~ married | D_gender, data=perf_d, nint =25, col="lightblue")# married = 0-> single, 1-> married
histogram(~ age | D_gender, data=perf_d, nint =25, col="lightblue")
histogram(~ perform1 | D_gender, data=perf_d, nint =25, col="lightblue")


# Model 1
# Running simple regression
# Overall performance of before and after the experiment
reg0<-lm(perform1 ~ expgroup, data=subset(perf_d, treatmentperiod==0))
reg1<-lm(perform1 ~ expgroup, data=subset(perf_d, treatmentperiod==1))
stargazer(reg0, reg1, type="text", align=TRUE)
des.html<-stargazer(reg0, reg1 ,type="html",out="overallperf_before_and_after_exp.html")

# Subsetting the men data
perf_men<-subset(perf_d,men==1)
# Subsetting the women data
perf_women<-subset(perf_d,men==0)

# Performance of only men before and after experiment
reg2<-lm(perform1 ~ expgroup, data=subset(perf_men, treatmentperiod==0))
reg3<-lm(perform1 ~ expgroup, data=subset(perf_men, treatmentperiod==1))
# Performance of only women before and after experiment
reg4<-lm(perform1 ~ expgroup, data=subset(perf_women, treatmentperiod==0))
reg5<-lm(perform1 ~ expgroup, data=subset(perf_women, treatmentperiod==1))
stargazer(reg2,reg3, reg4, reg5, type="text", align=TRUE)
des.html<-stargazer(reg2, reg3,reg4,reg5, type="html",out="men&women_ba.html")

# Model2
# Performance of treated men and treated women (interaction effects)
reg6<-lm(perform1 ~ expgroup+treatmentperiod+men+expgroup*treatmentperiod*men+expgroup*treatmentperiod,data=subset(perf_d))
stargazer(reg6,type="text", align=TRUE)
des.html<-stargazer(reg6,type="html",out="mainregression.html")

# construct factor variables (diff-in-diff)
# control for logdaysworked
fv<-felm(perform1 ~ expgroup | logdaysworked, data=subset(perf_d, treatmentperiod==0))
fv1<-felm(perform1 ~ expgroup | logdaysworked, data=subset(perf_d,treatmentperiod==1))
stargazer(fv, fv1,  type="text", align=TRUE)


# Subsetting the data to focus only on treated men
perf_men<-subset(perf_d,men==1)
treatedmen<-subset(perf_men,expgroup==1)
View(treatedmen)

# Model3
# running ols
# To check the factors that has an influence on men's performance
eff1<-lm(perform1 ~ children, data=subset(treatedmen, treatmentperiod==1))
eff2<-lm(perform1 ~ children+married, data=subset(treatedmen, treatmentperiod==1))
eff3<-lm(perform1 ~ children+married+logdaysworked, data=subset(treatedmen, treatmentperiod==1))
eff4<-lm(perform1 ~ children+married+logdaysworked+grosswage, data=subset(treatedmen, treatmentperiod==1))
eff5<-lm(perform1 ~ children+married+logdaysworked+grosswage+age, data=subset(treatedmen, treatmentperiod==1))
eff6<-lm(perform1 ~ children+married+logdaysworked+grosswage+age+high_educ, data=subset(treatedmen, treatmentperiod==1))
stargazer(eff1,eff2,eff3,eff4,eff5, type="text", align = TRUE)
# To export stargazer table
des.html<-stargazer(eff1, eff2,eff3,eff4,eff5,eff6, type="html",out="factors_men2.html")

# Subsetting the women data
perf_women<-subset(perf_d,men==0)
treatedwomen<-subset(perf_women,expgroup==1)
eff6<-lm(perform1 ~ children, data=subset(treatedwomen, treatmentperiod==1))
eff7<-lm(perform1 ~ children+married, data=subset(treatedwomen, treatmentperiod==1))
eff8<-lm(perform1 ~ children+married+logdaysworked, data=subset(treatedwomen, treatmentperiod==1))
eff9<-lm(perform1 ~ children+married+logdaysworked+grosswage, data=subset(treatedwomen, treatmentperiod==1))
eff10<-lm(perform1 ~ children+married+logdaysworked+grosswage+age, data=subset(treatedwomen, treatmentperiod==1))
eff11<-lm(perform1 ~ children+married+logdaysworked+age+high_educ, data=subset(treatedwomen, treatmentperiod==1))
stargazer(eff6,eff7,eff8,eff9,eff10,eff11, type="text", align = TRUE)

# To export stargazer table
des.html<-stargazer(eff6, eff7,eff8,eff9,eff10, type="html",out="factors_women2.html")





