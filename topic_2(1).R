
#If packages not installed yet:
#install.packages("mlogit")
#install.packages("gmnl")

#Load packages
library("data.table")
library("mlogit")
library("gmnl")


#Set workspace
rm(list = ls())
#setwd("YOUR FOLDER NAME")
setwd("L:/Takeaki/Google drive/Past classes/Rochester Simon/2025 Spring A pricing analytics/Topic 2")


#Read data
data=fread("kiwi_bubbles.csv",stringsAsFactors = F)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

#Define variables
id=data$id
purchaseKB=1*(data$choice=="KB") #Define an indicator of purchase
priceKB=data$price.KB

#Run regression between P and Q
reg=lm(purchaseKB~priceKB)
summary(reg)

#Plot results
coef=reg$coefficients
pricespace=seq(0,1.7,0.1)

fitted=(coef[1]+coef[2]*pricespace)
plot(priceKB,purchaseKB, main="Price vs Purchase",
     xlab="Price", ylab="Purchase", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlim=c(0.8,1.7))
lines(pricespace,fitted,col="blue",lwd=2)




##
#Not used
# #Individual-level regression
# #Create a matrix where we store coefficients.
# NC=max(id)
# coef.est <- data.frame(segment = 1:NC, intercept.KB = NA,
#                 price.coefKB = NA, price.coefKR = NA, price.coefMB = NA) 
# 
# for (seg in 1:NC) {
#   # Pick subset of data of each individual
#   data.sub <- subset(data, id == seg)
#   
#   #We can run regression only when we have more than three observations
#   if (nrow(data.sub)>3){
#   purchasesubKB=1*(data.sub$choice=="KB") #Define an indicator of purchase
#   pricesubKB=data.sub$price.KB
#   pricesubKR=data.sub$price.KR
#   pricesubMB=data.sub$price.MB
#   
#   #Run regression using data of that individual
#   reg=lm(purchasesubKB~pricesubKB)
#   #Store coefficients in coef.est matrix.
#   coef.est[seg, 2:3] <- t(reg$coefficient)
#   }}
# 
# #Histogram of price coefficient
# hist(coef.est[abs(coef.est[,3])<4&coef.est[,3]!=0,3],breaks=20,
#      main="Distribution of beta_1", xlab="beta_1", ylab="density",col="chocolate")
#   
#End of unused part
##

#Show that regressions don't fit well.
#Run log-log regression. log(0) not well defined, so add 1 to all "purchaseKB" observations.
reg2=lm(log(purchaseKB+1)~log(priceKB))
summary(reg2)

#Plot results
coef2=reg2$coefficients
pricespace=seq(0,1.7,0.1)
fitted2=(exp(coef2[1]+coef2[2]*log(pricespace))-1)


plot(priceKB,purchaseKB, main="Price vs Purchase",
     xlab="Price", ylab="Purchase", cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlim=c(0,1.7))
lines(pricespace,fitted,col="blue",lwd=2)
lines(pricespace,fitted2,col="orange",lwd=2)
legend(1.1, 0.8, legend=c("Linear", "Log price"),
       col=c( "blue","orange"), lty=1, cex=1.1)
summary(reg2)



##------------------------------------------------------------------
##------------------------------------------------------------------
#Choice models:

#Binary logit model - illustration

#Write down choice probability as a function
demand=function(price,beta0,beta1){
  prob=exp(beta0+beta1*price)/(1+exp(beta0+beta1*price))
  return(prob)
}

#Demonstrate how predicted choice probabilities change as we change beta1
pricespace=seq(0,5,0.01)
plot(pricespace,demand(pricespace,5,-3),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

lines(pricespace,demand(pricespace,5,-1),col="blue",lwd=2)
lines(pricespace,demand(pricespace,5,-2),col="blue",lwd=2)
lines(pricespace,demand(pricespace,5,-1.5),col="blue",lwd=2)
lines(pricespace,demand(pricespace,5,-8),col="blue",lwd=2)

#How predicted choice probabilities change as we change beta0
plot(pricespace,demand(pricespace,2,-2),type='l',xlab='Prices',
     ylab='Probability of purchase',col="blue",lwd=2, ylim=c(0,1)
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
lines(pricespace,demand(pricespace,3,-2),col="blue",lwd=2)
lines(pricespace,demand(pricespace,5,-2),col="blue",lwd=2)
lines(pricespace,demand(pricespace,7,-2),col="blue",lwd=2)
lines(pricespace,demand(pricespace,0.3,-2),col="blue",lwd=2)
lines(pricespace,demand(pricespace,9,-2),col="blue",lwd=2)


#What is the profit maximizing price?
#Define unit cost
uc=1

#Define profit
profit=1000*(demand(pricespace,5,-3)*pricespace-demand(pricespace,5,-3)*uc)

#Plot it
plot(pricespace,profit,type='l',xlab='Prices',
     ylab='Profit',ylim=c(0,400),col="blue",lwd=2,
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

#Find the profit maximizing price
pricespace[profit==max(profit)]
#P=1.67 maximizes the profit.



#-------------------------------------------------------------------------
#Estimation of a logit model  using "mlogit" and "gnml" functions.

#Read data
data=fread("kiwi_bubbles_binary.csv",head=T)

#Convert it to mlogit.data form. Column 4 is "price.0" and Column 5 is "price.KB".
#Syntax is detailed in lecture slides.
mlogitdata=mlogit.data(data,id="id",varying=4:5,choice="choice",shape="wide")

#Run maximum likelihood estimation.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)


#Simulate choice probability
#Brute force by directly typing in the choice probability formula inside "plot" - not recommended
coef=mle$coefficients
pricespace=seq(0,2,0.01)
plot(pricespace,exp(coef[1]+coef[2]*pricespace)/(1+exp(coef[1]+coef[2]*pricespace))
     ,type='l',xlab='Prices',ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


#Better approach - first write down the choice probability as a function
demand=function(price,beta0,beta1){
  prob=exp(beta0+beta1*price)/(1+exp(beta0+beta1*price))
  return(prob)
}

#Plot choice probability using the "demand" function we just defined
pricespace=seq(0,2,0.01)
plot(pricespace,demand(pricespace,coef[1],coef[2])
     ,type='l',xlab='Prices',ylab='Probability of purchase',col="blue",lwd=2
     ,cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)



##-------------------------------------------------------------------------------
# MLE coin_flipping_example
# load data
data=read.csv("coin_flip.csv",head=T)
outcome=data$outcome

# graphical
like = function(q) prod(q^(outcome=="H") * (1-q)^(outcome=="T"))
q.vec = seq(0, 1, 0.01)
like.vec = apply(matrix(q.vec), 1, like)
plot(q.vec, like.vec, lwd=2, type = 'l', col = 'blue', xlab = expression(theta), ylab = "likelihood",
     cex=2,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)










#NOT USED

# 
# ##
# #Estimation using MLE
# data=read.csv("coin_flip.csv",head=T)
# outcome=data$outcome
# #outcome = [T,H,H,H,T]
# 
# #This is the log-likelihood function we use in MLE.
# loglike = function(q) -1*sum((outcome=="H")*log(q) + (outcome=="T")*log(1-q))
# 
# #Note that likelihood function has to take an arbitrary q and 
# #return the likelihood associated with it. 
# 
# #Hence this is wrong. "object 'q' not found".
# loglike = -1*sum((outcome=="H")*log(q) + (outcome=="T")*log(1-q))
# 
# #Maximize likelihood
# mle = optim(par = 0.3, fn= loglike,
#             lower = 0.0001, upper = 0.9999, method = "L-BFGS-B")
# mle
# 
# 
# 
# 
# ##
# #Estimate a logit model manually
# #Read data
# data=read.csv("kiwi_bubbles_binary.csv",head=T)
# data=data.frame(data)
# 
# #Define variables
# id=data$id
# choice=data$choice #This is our outcome variable now
# priceKB=data$price.KB
# 
# 
# #Define "q" as the ratio of exponentials. 
# #"para" is the set of paramers to be estimated.
# qfun=function(para) exp(para[1]+para[2]*priceKB)/(1+exp(para[1]+para[2]*priceKB))
# 
# #Then the log-likelihood is the same as coin-tossing, except that it is 
# #now a function of "para" through qfun(para) defined above.
# loglike=function(para) -1*sum((choice=="KB")*log(qfun(para)) 
#                               + (choice==0)*log(1-qfun(para)))
# 
# 
# 
# #Maximize likelihood
# mle = optim(par = c(0,0), fn= loglike)
# mle
# 
# 


#Code likelihood function
#like = function(q) prod(q^(outcome=="H") * (1-q)^(outcome=="T"))


#q^(outcome=="H") = [1,q,q,q,1]. (1-q)^(outcome=="T")=[1-q,1,1,1,1-q].
#Hence by taking products, we have (1-q)*q*q*q*(1-q)

#Note that likelihood function has to take an arbitrary q and 
#return the likelihood associated with it. 

#Hence this is wrong. "object 'q' not found".
#like = prod(q^(outcome=="H") * (1-q)^(outcome=="T"))