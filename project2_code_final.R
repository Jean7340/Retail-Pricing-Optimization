#Install packages (if not done before)

options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages("data.table")
install.packages("mlogit")
install.packages("gmnl")
install.packages("RColorBrewer")
install.packages("plotly")

#load packages 
library('data.table')
library('mlogit')
library("gmnl")
library('RColorBrewer')
library("plotly")

#load data 

rcol <- brewer.pal(9, 'Blues')

rm(list = ls());
setwd("/Users/liuzhiying/Desktop/MKT440_Pricing Analytics/Project 2")
# Install the package if not already installed
if (!require("data.table")) install.packages("data.table")

# Load data.table package
library(data.table)

# Now you can use fread()
data <- fread("kiwi_bubbles_P2.csv", stringsAsFactors = FALSE)

data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]


#Q3-1
#First, convert the original data into "mlogit" format.
mlogitdata=mlogit.data(data,id="id",varying=4:7,choice="choice",shape="wide")

#Run MLE using "gmnl" function.
mle= gmnl(choice ~  price, data = mlogitdata)
summary(mle)

coef=mle$coefficients
print(coef)


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

print(meanPKB)
print(meanPKR)
print(meanPMB)

#Now let's compute the elasticity. For instance, 
#own-elasticity for KB is given by the following formula.
eKB=-coef[4]*meanPKB*(1-demand(meanPKB,meanPKR,meanPMB,coef)[1])
print(eKB)

# own-elasticity for MB
eMB=-coef[4]*meanPMB*(1-demand(meanPKB,meanPKR,meanPMB,coef)[3])
print(eMB)

# own-elasticity for KR
eKR=-coef[4]*meanPKR*(1-demand(meanPKB,meanPKR,meanPMB,coef)[2])
print(eKR)

#Cross-elasticity is as follow:

# Cross-price elasticity of KB with respect to KR
eKB_KR = -coef[4] * meanPKR * (-demand(meanPKB, meanPKR, meanPMB, coef)[1])
print(eKB_KR)

# Cross-price elasticity of KB with respect to MB
eKB_MB = -coef[4] * meanPMB * (-demand(meanPKB, meanPKR, meanPMB, coef)[1])
print(eKB_MB)

# Cross-price elasticity of KR with respect to KB
eKR_KB = -coef[4] * meanPKB * (-demand(meanPKB, meanPKR, meanPMB, coef)[2])
print(eKR_KB)

# Cross-price elasticity of KR with respect to MB
eKR_MB = -coef[4] * meanPMB * (-demand(meanPKB, meanPKR, meanPMB, coef)[2])
print(eKR_MB)

# Cross-price elasticity of MB with respect to KB
eMB_KB = -coef[4] * meanPKB * (-demand(meanPKB, meanPKR, meanPMB, coef)[3])
print(eMB_KB)

# Cross-price elasticity of MB with respect to KR
eMB_KR = -coef[4] * meanPKR * (-demand(meanPKB, meanPKR, meanPMB, coef)[3])
print(eMB_KR)


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
# Define price range for KB and KR ($1 to $1.6 in 2-cent increments)
aux = seq(1, 1.6, 0.02)
pricespace = expand.grid(priceKB = aux, priceKR = aux)
head(pricespace)

# Compute profits for all price combinations
profits = mapply(function(kb, kr) {
  profit(kb, kr, 1.43, coef)  # Fix MB's price at 1.43
}, pricespace$priceKB, pricespace$priceKR, SIMPLIFY = FALSE)

# Convert the list to a data frame
profits_matrix = do.call(rbind, profits)
profit_df = cbind(pricespace, profits_matrix)
colnames(profit_df) = c("priceKB", "priceKR", "profitKB", "profitKR", "profitMB")

# Find the optimal price for KB and KR (maximizing combined profit of KB and KR)
optimal_price = profit_df[which.max(profit_df$profitKB + profit_df$profitKR), ]

# Print the optimal price and corresponding profits
print("Optimal Prices for KB and KR (keeping MB at $1.43):")
print(optimal_price)

## Section 2: clustered logit


#Q4-1
rm(list = ls());
#Read data
data=fread("kiwi_bubbles_P2.csv",stringsAsFactors = F)

#Data cleaning - drop periods with price=99 (stockout).
data=data[!(data$price.KB==99),]
data=data[!(data$price.KR==99),]
data=data[!(data$price.MB==99),]

demo=fread("demo_P2.csv",stringsAsFactors = F)
#Number of segments - SET YOUR OWN NUMBER
NC = 8

#Clustering
#I recommend to always set seed before running Kmean clustering.
set.seed(1)
#Run "kmeans" function to cluster consumers.
demo_cluster = kmeans(demo[, 2:18], centers = NC, nstart = 1500)

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

print(head(data$cluster))
print(coef.est)
print(se.est)

cluster_counts = table(data$cluster)

# Print the number of consumers in each cluster
print(cluster_counts)

#Q4-2
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

coef.est=as.matrix(coef.est)
coef.est 
#Function 3 "demand.agg"
#It takes three prices, the matrix of
#estimated parameters (the "coef.est" matrix we created above)
#and the segment share. It then returns the aggregate choice 
#probability at the market level (slide deck 4, page 33, or "Pr(y)" for all product y). 
demand.agg=function(priceKB,priceKR,priceMB,para,segshare){
  demand=colSums((segshare)*demand.seg(priceKB,priceKR,priceMB,para))
  return(demand)
}

meanPKB=mean(data$price.KB)
meanPKR=mean(data$price.KR)
meanPMB=mean(data$price.MB)

print(meanPKB)
print(demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare))
print(coef.est[,5])  
print(segshare)

demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1]

#own price elasticity for KB
elasticity_KB= -(meanPKB/ demand.agg(meanPKB,meanPKR,meanPMB,coef.est,segshare)[1])*sum(segshare*coef.est[,5]*demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1]*(1-demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1]))
elasticity_KB
#own price elasticity for KR 
elasticity_KR= -(meanPKR/demand.agg(meanPKB,meanPKR,meanPMB,coef.est,segshare)[2])*sum(segshare*coef.est[,5]*demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2]*(1-demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2]))
elasticity_KR
#own price elasticity for MB
elasticity_MB= -(meanPMB/demand.agg(meanPKB,meanPKR,meanPMB,coef.est,segshare)[3])*sum(segshare*coef.est[,5]*demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3]*(1-demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3]))
elasticity_MB

#cross price elasticity for KB to KR
elasticity_KB_KR = -(meanPKR / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[1]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2])
elasticity_KB_KR

# Cross-price elasticity of KB with respect to MB
elasticity_KB_MB = -(meanPMB / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[1]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3])
elasticity_KB_MB

# Cross-price elasticity of KR with respect to KB
elasticity_KR_KB = -(meanPKB / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[2]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1])
elasticity_KR_KB

# Cross-price elasticity of KR with respect to MB
elasticity_KR_MB = -(meanPMB / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[2]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3])
elasticity_KR_MB

# Cross-price elasticity of MB with respect to KB
elasticity_MB_KB = -(meanPKB / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[3]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,1])
elasticity_MB_KB

# Cross-price elasticity of MB with respect to KR
elasticity_MB_KR = -(meanPKR / demand.agg(meanPKB, meanPKR, meanPMB, coef.est, segshare)[3]) * 
  sum(segshare * coef.est[,5] * demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,3] * 
        demand.seg(meanPKB, meanPKR, meanPMB, coef.est)[,2])
elasticity_MB_KR



##Question4.4(1)--------
meanPKB <- mean(data$price.KB)
meanPKR <- mean(data$price.KR)
meanPMB <- mean(data$price.MB)

para <- as.matrix(coef.est)
coef <- as.matrix(coef.est)
profit_by_segment <- function(priceKB, priceKR, priceMB, para, segshare) {
  demand_matrix <- demand.seg(priceKB, priceKR, priceMB, para) 
  profitKB <- sum(segshare * demand_matrix[, 1] * (priceKB - 0.5) * 1000)  
  profitKR <- sum(segshare * demand_matrix[, 2] * (priceKR - 0.5) * 1000)
  profitMB <- sum(segshare * demand_matrix[, 3] * (priceMB - 0.5) * 1000)
  return(c(profitKB, profitKR, profitMB))
}

aux <- seq(1, 1.6, 0.02)  
pricespace <- expand.grid(aux, aux)

profits <- apply(pricespace, 1, function(p) {
  profit_by_segment(p[1], p[2], 1.43, coef, segshare)  
})

profits <- t(profits)
profits_df <- data.frame(pricespace, profits)
colnames(profits_df) <- c("priceKB", "priceKR", "profitKB", "profitKR", "profitMB")

profits_df$profit_total <- profits_df$profitKB + profits_df$profitKR

optimal_index <- which.max(profits_df$profit_total)
optimal_KB <- profits_df[optimal_index, "priceKB"]
optimal_KR <- profits_df[optimal_index, "priceKR"]

cat("Optimal price for Kiwi Bubbles (KB):", optimal_KB, "\n")
cat("Optimal price for Kiwi Regular (KR):", optimal_KR, "\n")

print("Pricing Optimization Results with Segmentation:")
print(head(profits_df[order(-profits_df$profit_total), ], 10))


##Question4.4(2)--------
profit_no_KB <- function(priceKR, priceMB, para, segshare) {
  demand_matrix <- demand.seg(99, priceKR, priceMB, para)  
  profitKR <- sum(segshare * demand_matrix[, 2] * (priceKR - 0.5) * 1000)  
  profitMB <- sum(segshare * demand_matrix[, 3] * (priceMB - 0.5) * 1000)
  return(c(profitKR, profitMB))
}

price_range <- seq(1, 1.6, 0.02)  
profits_df_no_kb <- data.frame(priceKR = price_range, profitKR = numeric(length(price_range)), profitMB = numeric(length(price_range)))

coef <- as.matrix(coef.est)
for (i in 1:length(price_range)) {
  profits_df_no_kb[i, 2:3] <- profit_no_KB(profits_df_no_kb[i, "priceKR"], 1.43, coef, segshare) 
}

optimal_index_no_KB <- which.max(profits_df_no_kb$profitKR)
optimal_KR_no_KB <- profits_df_no_kb[optimal_index_no_KB, "priceKR"]

cat("Optimal price for Kiwi Regular (KR) without Kiwi Bubbles:", optimal_KR_no_KB, "\n")
cat("Profit of Kiwi Regular (KR) without KB:", profits_df_no_kb[optimal_index_no_KB, "profitKR"], "\n")
cat("Profit of Mango Bubbles (MB) without KB:", profits_df_no_kb[optimal_index_no_KB, "profitMB"], "\n")

print("Pricing Optimization Results without Kiwi Bubbles:")
print(head(profits_df_no_kb[order(-profits_df_no_kb$profitKR), ], 10))

#section 5
#question1: mango bubble optimal price 
mango_profit <- function(priceMB, priceKB, priceKR, para, segshare) {
  # Calculate demand for each segment
  demand_matrix <- demand.seg(priceKB, priceKR, priceMB, para)
  
  # Aggregate demand for MB across segments, weighted by segment shares
  demand_MB <- sum(segshare * demand_matrix[, 3])
  
  # Calculate profit for MB
  profit_MB <- 1000 * demand_MB * (priceMB - 0.5)
  
  return(profit_MB)
}

# Define a grid of possible prices for MB
aux_MB <- seq(1, 1.6, 0.01)

# Calculate Mango's profit for each price in the grid
profit_MB <- sapply(aux_MB, function(p) {
  mango_profit(p, optimal_KB, optimal_KR, para, segshare)
})

# Find the optimal price for MB
optimal_price_MB <- aux_MB[which.max(profit_MB)]
cat("Mango's optimal price for MB:", optimal_price_MB, "\n")

kiwi_profit <- function(priceKB, priceKR, priceMB, para, segshare) {
  # Calculate demand for each segment
  demand_matrix <- demand.seg(priceKB, priceKR, priceMB, para)
  
  # Aggregate demand for KB and KR across segments, weighted by segment shares
  demand_KB <- sum(segshare * demand_matrix[, 1])
  demand_KR <- sum(segshare * demand_matrix[, 2])
  
  # Calculate profit for KB and KR
  profit_KB <- 1000 * demand_KB * (priceKB - 0.5)
  profit_KR <- 1000 * demand_KR * (priceKR - 0.5)
  
  # Total profit for Kiwi
  profit_total <- profit_KB + profit_KR
  
  return(profit_total)
}

# Fix Mango's price for MB
priceMB <- 1.00

# Define a grid of possible prices for KB and KR
aux_KB <- seq(1, 1.6, 0.01)
aux_KR <- seq(1, 1.6, 0.01)

# Create a grid of all combinations of KB and KR prices
pricespace <- expand.grid(aux_KB, aux_KR)

# Calculate Kiwi's total profit for each combination of KB and KR prices
profits <- apply(pricespace, 1, function(p) {
  kiwi_profit(p[1], p[2], priceMB, para, segshare)
})

# Combine results into a data frame
profits_df <- data.frame(pricespace, profits)
colnames(profits_df) <- c("priceKB", "priceKR", "profit_total")

# Find the optimal prices for KB and KR
optimal_index <- which.max(profits_df$profit_total)
optimal_KB <- profits_df[optimal_index, "priceKB"]
optimal_KR <- profits_df[optimal_index, "priceKR"]

# Print the results
cat("Optimal price for Kiwi Bubbles (KB):", optimal_KB, "\n")
cat("Optimal price for Kiwi Regular (KR):", optimal_KR, "\n")
cat("Maximum profit for Kiwi:", profits_df[optimal_index, "profit_total"], "\n")

# Question 3: New “equilibrium price”
# Iterative strategic pricing adjustments for equilibrium
tolerance <- 0.01
max_iterations <- 30

# Set initial prices (from Q5-2)
price_KB <- 1.04
price_KR <- 1.08
price_MB <- 1.00  # From Q5-1 result

# Initialize price history tracking
price_history <- data.frame()

for (i in 1:max_iterations) {
  
  # Step 1: Mango optimizes MB price given Kiwi's prices
  aux_MB <- seq(1.00, 1.60, 0.01)
  profit_MB_values <- sapply(aux_MB, function(p) profit(price_KB, price_KR, p, coef.est)[,3])
  new_price_MB <- aux_MB[which.max(profit_MB_values)]
  
  # Step 2: Kiwi optimizes KB and KR prices given Mango’s new price
  aux_KB <- seq(1.00, 1.10, 0.01)  # Slightly lower cap to allow for competition effects
  aux_KR <- seq(1.00, 1.12, 0.01)  # Matches previous best-known values
  pricespace <- expand.grid(aux_KB, aux_KR)
  
  profits_Kiwi <- apply(pricespace, 1, function(p) sum(profit(p[1], p[2], new_price_MB, coef.est)[,1:2]))
  optimal_index <- which.max(profits_Kiwi)
  new_price_KB <- pricespace[optimal_index, 1]
  new_price_KR <- pricespace[optimal_index, 2]
  
  # Store price history
  price_history <- rbind(price_history, data.frame(Iteration = i, Price_KB = new_price_KB, Price_KR = new_price_KR, Price_MB = new_price_MB))
  
  # Check for convergence
  if (abs(new_price_KB - price_KB) < tolerance &&
      abs(new_price_KR - price_KR) < tolerance &&
      abs(new_price_MB - price_MB) < tolerance) {
    break  # Stop iteration if prices are stable
  }
  
  # Update prices
  price_KB <- new_price_KB
  price_KR <- new_price_KR
  price_MB <- new_price_MB
}

# Print final equilibrium prices
cat("Final Equilibrium Prices:\n")
cat("Kiwi Bubbles (KB):", price_KB, "\n")
cat("Kiwi Regular (KR):", price_KR, "\n")
cat("Mango Bubbles (MB):", price_MB, "\n")

# Print price history for debugging
print(price_history)
