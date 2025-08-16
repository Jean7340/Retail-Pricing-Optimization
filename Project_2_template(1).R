#################################################
## Section 1: estimation and sanity-check of a single-segment logit model
#################################################


#Install packages (if not done before)
# install.packages('data.table')
# install.packages('mlogit')
# install.packages("gmnl")
# install.packages('RColorBrewer')
# install.packages("plotly")


#Load packages
library('data.table')
library('mlogit')
library("gmnl")
library('RColorBrewer')
library("plotly")
#################################################
## Load data
#################################################

rcol <- brewer.pal(9, 'Blues')

rm(list = ls());
setwd("YOUR FOLDER")

#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


#################################################
## Multinomial logit with single segment common parameters
#################################################

#Q3-1
#First, convert the original data into "mlogit" format.
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE using "gmnl" function.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

#Parameters are available in "mle$coefficients".
coef=mle$coefficients


#Q3-2
#Compute elasticity
#For our convenience, we define a function "demand".
demand=function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR,probMB))
}
#This "demand" function takes the three prices (each for KB, KR and MB) and the estimated parameters as inputs,
#and compute the probability of KB selected, KR selected and MB selected as three columns.

#We also compute the average price in the data
meanPKB=mean(data$price.KB)
meanPKR=mean(data$price.KR)
meanPMB=mean(data$price.MB)


#Now let's compute the elasticity. For instance, 
#own-elasticity for KB is given by the following formula.
eKB=-coef[4]*meanPKB*(1-demand(meanPKB,meanPKR,meanPMB,coef)[1])

#Likewise, produce and report other elasticities.




#Q3-3
#Next, find the optimal price by solving profit maximization problem

#To find the optimal price, we need to check profit values across many candidate price points
#and find the highest one.
#First, define the profit function.
profit=function(priceKB,priceKR,priceMB,para){
    profitKB=1000*demand(priceKB,priceKR,priceMB,para)[,1]*(priceKB-0.5)
    profitKR=1000*demand(priceKB,priceKR,priceMB,para)[,2]*(priceKR-0.5)
    profitMB=1000*demand(priceKB,priceKR,priceMB,para)[,3]*(priceMB-0.5)
    return(cbind(profitKB,profitKR,profitMB))
}
#The function "profit" takes the prices and estimated parameters as inputs, and return 
#profits for KB, KR and MB as the outcome.

#Example: profits of KB, KR and MB at the price of "KB=$1, KR=$1 and MB=$1.43" is given by:
profit0=profit(1,1,1.43,coef)



#Note that just like the lecture example, we need to evaluate profits at every single COMBINATION of 
#KB price and KR price, because we sell both KB and KR (Slide "topic3.R", line 129).
#"expand.grid" function is useful. See below:
aux=seq(1,1.6,0.02)
pricespace=expand.grid(aux,aux)
pricespace[1:100,]
#"pricespace" now contains every single combination of two prices, in two-cent increment, starting from "KB=$1, KR=$1", 
#all the way through "KB=$1.6, KR=$1.6". 

#To find the optimal price, the simplest approach is to evaluate the "profit" function above,
#using the first column of "pricespace" as the price for KB and the second column as the price for KR.



#################################################
## Section 2: clustered logit
#################################################

#Q4-1
rm(list = ls());
#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


#Load demographic data
demo=fread("demo_P2.csv",stringsAsFactors = F)

#Number of segments - SET YOUR OWN NUMBER
NC = 0

#Clustering
#I recommend to always set seed before running Kmean clustering.
set.seed(1)
#Run "kmeans" function to cluster consumers.
demo_cluster = kmeans(demo[, 2:18], centers = NC, nstart = 1500)


# Now combine cluster identity into the raw data
cluster_id = data.frame(id = demo$id)
cluster_id$cluster = demo_cluster$cluster
data = merge(data, cluster_id, by = "id", all.x = T)

# For those who don't fit in any cluster, group them into one additional cluster
data$cluster[is.na(data$cluster)] = NC+1

#The new data now has a column named "cluster" which indicate the cluster each consumer
#belongs to.

# Segment share
N = max(data$id)
segshare = c( table(demo_cluster$cluster),N - sum(table(demo_cluster$cluster))) / N


#Next, estimate preference parameters segment by segment.
#First, create a matrix in which we store estimated parameters and standard errors
coef.est = data.frame(segment = 1:(NC+1), intercept.KB = NA, intercept.KR = NA, intercept.MB = NA, price.coef = NA) 
se.est <- data.frame(segment = 1:(NC+1), intercept.KR = NA, intercept.KB = NA, intercept.MB = NA, price.coef = NA)


#Run a "for" loop. In each iteration,
for (seg in 1:(NC+1)) {
    #we take subset of the data of consumers who belong to segment "seg".
    data.sub = subset(data, cluster == seg)
    #and convert their data to mlogit,
    mlogitdata=mlogit.data(data.sub,id="id",varying=4:7,choice="choice",shape="wide")
    
    #and then run MLE.
    mle= gmnl(choice ~  price, data = mlogitdata)
    summary(mle)
    
    #We save the output to the matrix.
    #Save coefficients
    coef.est[seg, 2:5] = mle$coefficients
    #Save standard errors
    se.est[seg, 2:5] = summary(mle)$CoefTable[,2]
}

#"coef.est" now contains all parameters from all segments.
#"se.est" contains the corresponding standard errors.


#Q4-2
#Follow the definition on the slide (deck 4, slide 36-37).
#The following three functions may be useful:

#Function 1 "demand"
#This function is identical to the one at line 57 above. 
#It takes (besides prices) estimated parameters of a given segment (4 x 1 vector)
#as inputs, and returns three choice probabilities for that segment (each for 
#KB, KR and MB) at that price.
demand=function(priceKB,priceKR,priceMB,para){
    probKB=exp(para[1]+para[4]*priceKB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probKR=exp(para[2]+para[4]*priceKR)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    probMB=exp(para[3]+para[4]*priceMB)/(1+exp(para[1]+para[4]*priceKB)+exp(para[2]+para[4]*priceKR)+exp(para[3]+para[4]*priceMB))
    return(cbind(probKB,probKR,probMB))
}


#Function 2 "demand.seg"
#This function takes the three prices and the matrix of
#estimated parameters (the "coef.est" matrix we created above: # of segments x 4 matrix)
#and returns the choice probability of all the segments stacked together.
#The first row corresponds to the three choice probabilities of segment 1,
#the second row corresponds to those of segment 2, and so on.
#These outputs correspond to "Pr{k}(y)" for all segment k and product y.
demand.seg=function(priceKB,priceKR,priceMB,para){
    
    #Define a matrix to store the result
    sharemat=matrix(0L,nrow = NC+1, ncol = 3)
    
    #Iterate the "demand" across segments and store the result in each row.
    for(seg in 1:(NC+1)){
        sharemat[seg,]=demand(priceKB,priceKR,priceMB,para[seg,2:5])
    }
    return(sharemat)
}

#Function 3 "demand.agg"
#It takes three prices, the matrix of
#estimated parameters (the "coef.est" matrix we created above)
#and the segment share. It then returns the aggregate choice 
#probability at the market level (slide deck 4, page 33, or "Pr(y)" for all product y). 
demand.agg=function(priceKB,priceKR,priceMB,para,segshare){
    demand=colSums((segshare)*demand.seg(priceKB,priceKR,priceMB,para))
    return(demand)
}

#Note "demand.agg" can also be used when you find optimal prices for 
#KB and KR (Q4-4).

#Q4-3:
#The question asks you to examine the estimated coefficients - any noticeable patterns of consumer segmentation?
#Who are the price sensitive segments? Who are not? What products do these segments prefer?
#Recall consumers sort into the products they like the most. So if price insensitive customers
#systematically prefer one product over others, the elasticity involving that product becomes ... (you will find it!)

#Q4-4:
#Redo the exercise in Q3-3, but with demand with segments. 
#Recall you have already computed the aggregate demand for KB, KR and MB when
#segmentation exists ("demand.agg" function). Define the profit associated with
#it and maximize it with respect to KB and KR prices. You can definitely reuse part
#of the code for Q 3-3.

#For the case without KB, THINK THROUGHLY what you should code before you 
#start coding. With a smart twist, you actually won't have to code anything new
#to solve that version.

#Q5
#The question requests multiple iterations of what you did in Q4-4. Copy-paste your
#Q4-4 and start from there. In each iteration, make sure what price you are choosing,
#and what price is fixed as given.