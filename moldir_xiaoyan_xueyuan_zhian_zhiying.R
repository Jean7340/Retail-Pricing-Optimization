#If not installed yet
#install.packages("lfe")
#install.packages("data.table")

#Load packages
library("lfe")
library("data.table")


#Set working space
rm(list = ls());
setwd("/Users/huxiaoyan/Desktop/Pricing analytics")

#Load data
cardata=fread("cars.csv",stringsAsFactors = F)


#----------------------------------------------------------
#----------------------------------------------------------
#SECTION 1: Run regressions with control variables (X) and fixed effects using "felm" function
# OUR SOLUTION
#Manipulate the set of variables to include and find the specification you find most causal.

reg3_3= felm(log(qu) ~ log(eurpr) + li + cy + hp + we + sp + ac
             + ergdpc + tax + avgurprrival + nco | factor(ye):factor(co)
             +factor(brand):factor(ma), 
             data = cardata)

summary(reg3_3)


# Final model for quetsion 5 "Adding control variables"
reg3_5= felm( log(qu) ~ log(eurpr) + ac:hp + cy:li + ergdpc + tax + avgurprrival 
  + nco | factor(ye):factor(co) + factor(brand):factor(ma) + factor(cla),
  data = cardata)
summary(reg3_5)

#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#SECTION 2: Run an IV regression using "felm" function
# OUR SOLUTION 

# merged two dataset into one dataset
fulldata= read.csv("Merged_Cars_and_Iron_Ore_Data.csv")
head(fulldata)

# FINAL model adding IVs, for question 6 and 7
reg6=felm(formula = log(qu) ~ac:hp +cy:li + ergdpc+avgurprrival+ nco
          | factor(year):factor(co) +
            factor(brand):factor(ma)+ factor(cla)| 
            (log(eurpr) ~ tax + we*unit_value_98), data = fulldata)
summary(reg6)



