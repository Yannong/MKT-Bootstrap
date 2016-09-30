##############################Question 1: Demand Model###################### 
# a) Load data
setwd("~/Documents/Advanced MA")
soup.dt <- read.csv("Homework 1 Data.csv")
soup.dt <- soup.dt[,-1]

# b) Calculate the price per unit
soup.dt$pricePerUnit <- soup.dt$dollars/soup.dt$units

# c) Predict log
demand.price <- lm(log(soup.dt$units) ~ soup.dt$pricePerUnit,soup.dt)
demand.totalTime <- lm(log(soup.dt$units) ~ soup.dt$weekNum,soup.dt)
demand.seasonality <- lm(log(soup.dt$units) ~ soup.dt$weekOfYear,soup.dt)
demand1 <- lm(log(units) ~ weekNum&pricePerUnit&weekOfYear,soup.dt)
demand.poly <- lm(log(units) ~ poly(pricePerUnit),soup.dt)

# d) univariate linear model
fit.lm <- lm(log(units) ~ pricePerUnit,data = soup.dt)

###########################Question 2: Profit Maximization####################

# a) Create a new data frame
dt <- data.frame(matrix(seq(0,2,0.01),nrow=201,ncol=1)) 
names(dt)[1]='pricePerUnit'

# b) estimate demand for potential prices
dt$estDemand <- predict(fit.lm,dt)

# c) Calculate the expected profit
trueValue <- exp(dt$estDemand)
dt$profit <- trueValue*(dt$pricePerUnit-0.6)

# d) Find the price that maximizes profits
maxPrice <- dt$pricePerUnit[dt$profit == max(dt$profit)] 
print(maxPrice)

# e) Calculate the optimal price
optPrice <- function (fitmodel) {
  dt <- data.frame(matrix(seq(0,2,0.01),nrow=201,ncol=1)) 
  names(dt)[1]='pricePerUnit'
  dt$estDemand <- predict(fitmodel,dt)
  trueValue <- exp(dt$estDemand)
  dt$profit <- trueValue*(dt$pricePerUnit-0.6)
  maxPrice <- dt$pricePerUnit[dt$profit == max(dt$profit)] 
  maxPrice
}
optPrice(fit.lm)

############################Question 3: Bootstrapping####################
# a) Compute the bootstrap distribution
nBootstraps <- 1000
nsamples <- 360
bsPrice <- rep(NA,nBootstraps)

for (i in 1:nBootstraps){
  bsSample <- sample(nsamples,replace=TRUE)
  bsData <- soup.dt[bsSample,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData)
  bsPrice[i] <- optPrice(bsDemand)
}

# b) Calculate the standard error 
sd(bsPrice)  # 0.034444689

# c) Compute the standard error by increasing the number of bootstrap samples to 2000
nBootstraps2 <- 2000
nsamples <- 360
bsPrice2 <- rep(NA,nBootstraps2)

for (i in 1:nBootstraps2){
  bsSample2 <- sample(nsamples,replace=TRUE)
  bsData2 <- soup.dt[bsSample2,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData2)
  bsPrice2[i] <- optPrice(bsDemand)
}
sd(bsPrice2)  # 0.03516339 

# d) Plot a histogram 
hist(bsPrice)   
hist(bsPrice2)

#######################Question 4: Sample Size Calculation##################
nBootstraps0 <- 1000
nsamples <- 360
bsPrice0 <- rep(NA,nBootstraps0)

for (i in 1:nBootstraps0){
  bsSample0 <- sample(nsamples,1000,replace=TRUE)
  bsData0 <- soup.dt[bsSample0,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData0)
  bsPrice0[i] <- optPrice(bsDemand)
}
sd(bsPrice0)  ##0.01461799

nBootstraps3 <- 1000
nsamples <- 360
bsPrice3 <- rep(NA,nBootstraps3)

for (i in 1:nBootstraps3){
  bsSample3 <- sample(nsamples,2000,replace=TRUE)
  bsData3 <- soup.dt[bsSample3,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData3)
  bsPrice3[i] <- optPrice(bsDemand)
}
sd(bsPrice3) ###0.0117286
#######
nBootstraps4 <- 1000
nsamples <- 360
bsPrice4 <- rep(NA,nBootstraps4)

for (i in 1:nBootstraps4){
  bsSample <- sample(nsamples,3000,replace=TRUE)
  bsData4 <- soup.dt[bsSample,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData4)
  bsPrice4[i] <- optPrice(bsDemand)
}
sd(bsPrice4)
######when sample size=4800,se <0.01
nBootstraps5 <- 1000
nsamples <- 360
bsPrice5 <- rep(NA,nBootstraps5)

for (i in 1:nBootstraps5){
  bsSample5 <- sample(nsamples,4800,replace=TRUE)
  bsData5 <- soup.dt[bsSample5,]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData5)
  bsPrice5[i] <- optPrice(bsDemand)
}
sd(bsPrice5)  ##0.00954978

 #########################Question 5: Potential Profit Loss################
# Create profit function
profit <- function(price,fit,cost){
  coeff1 <- fit$coefficient[1]
  coeff2 <- fit$coefficient[2]
  quantity <- exp(coeff1 + coeff2*price)
  profit <- (price-cost)*quantity
}

##profit when true optimal price=0.83
profit.truePrice <- rep(profit(0.83,fit.lm,0.6),1000)
profit.truePrice

##Profit when price=p'
profit.bs <- c()
bs.optPrice <- rep (NA,1000)
for (i in 1:1000){
  bsData <- soup.dt[sample(1:360,replace=TRUE),]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData)
  bs.optPrice[i] <- optPrice(bsDemand)
  profit.bs[i] <- profit(bs.optPrice[i],fit.lm,0.6)
}

sd(bs.optPrice)

####Profit Loss
ProfitLoss <- profit.truePrice-profit.bs
mean(ProfitLoss) # 4.057148

####sample size=1000
profit.bs <- c()
bs.optPrice <- rep (NA,1000)
for (i in 1:1000){
  bsData <- soup.dt[sample(1:360,1000,replace=TRUE),]
  bsDemand <- lm(log(units)~pricePerUnit,data=bsData)
  bs.optPrice[i] <- optPrice(bsDemand)
  profit.bs[i] <- profit(bs.optPrice[i],fit.lm,0.6)
}

ProfitLoss <- profit.truePrice-profit.bs
mean(ProfitLoss)  ##1.414129, the expected profit loss decrease







