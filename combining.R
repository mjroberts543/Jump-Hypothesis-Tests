library(keras)
library(mlbench)
library(dplyr)
library(neuralnet)
library(ggplot2)
library(lattice)
library(InformationValue)
library(janitor)
library(readr)
library(magrittr)
library(VIM)
library(dplyr)
library(repr)
library(corrplot)
library(RColorBrewer)
library(ModelMetrics)
library(rpart)
library(rattle)
library(rpart.plot)
library(randomForest)
library(caret)
library(rsample)
library(xgboost)
library(tidyverse)
library(quantmod)
library(broom)

## We collect our data from Bloomberg Terminal
## Collect closing prices of individual stock in S&P 500 for the past five years
## Categorize them based on stock sectors (Energy, materials, industrials, utilities
# Healthcare, Financials, Consumer Discretionary, COnsumer Staples, Information
# Technology, Communication Services, and real estate) because stock market sector 
# is a group of stocks that have a lot in common with each other. We picked six
# stocks from 6 different factors. 
# Ventas, Inc. (healthcare real estate capital provider)
# Boeing Co. (the world's largest aerospace company and leading manufacturer of commercial jetliners and defense)
# Ecolab Inc. (ECL) (develops and offers services, technology and systems that specialize in treatment, purification, cleaning and hygiene of water in wide variety of applications.)
# News Corporation (NWS) (American mass media and publishing company delivering authoritative and engaging content from some of the world's most trusted brands.)
# The Kroger Co. (KR)(American retail company that operates supermarkets and multi-department stores throughout the United States.)
# Exxon Mobil Corp (one of the world's largest publicly traded international oil and gas companies.)

#install.packages("quantmod")
#install.packages("broom")


source("C:\\Users\\15129\\Desktop\\bigorsmall.R")
snp=read.csv("C:\\Users\\15129\\Desktop\\6Stocks.csv")

## Six Stocks Daily Stock Prices

start = as.Date("2017-08-08") 
end = as.Date("2022-09-06")

getSymbols(c("VTR", "BA", "ECL","NWS","KR","XOM"), src = "yahoo", from = start, to = end)

stocks = as.xts(data.frame(A = VTR[, "VTR.Close"], 
                           B = BA[, "BA.Close"], C = ECL[, "ECL.Close"], 
                           D = NWS[,"NWS.Close"], E=KR[,"KR.Close"], F=XOM[,"XOM.Close"]))
names(stocks) = c("VTR", "BA", "ECL","NWS","KR","XOM")
index(stocks) = as.Date(index(stocks))


stocks_series = tidy(stocks) %>% 
ggplot(aes(x=index,y=value, color=series)) +
  labs(title = "", 
       subtitle = "",
       caption = " Source: Yahoo Finance") +
  xlab("Date") + ylab("Closing Price") +
  scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Pink", "Green"))+
  geom_line()

stocks_series

## 

getSymbols(c("VTR", "BA", "ECL","NWS","KR","XOM"), src = "yahoo", from = start, to = end)

stocks2 = as.xts(data.frame(A = VTR[, "VTR.Close"], 
                           B = BA[, "BA.Close"], C = ECL[, "ECL.Close"], 
                           D = NWS[,"NWS.Close"], E=KR[,"KR.Close"], F=XOM[,"XOM.Close"]))
names(stocks2) = c("VTR", "BA", "ECL","NWS","KR","XOM")
index(stocks2) = as.Date(index(stocks2))

stocks_series2 = tidy(stocks2) %>% 
  
  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "",
       subtitle = "",
       caption = " Source: Yahoo Finance") + xlab("Date") + ylab("Closing Price") + scale_color_manual(values = c("Red", "Black", "DarkBlue","Orange", "Green", "Pink"))
stocks_series2

# S&P 500 Daily Stock Prices
getSymbols(c("^GSPC"), src = "yahoo", from = start, to = end)

stocks3 = as.xts(data.frame(A=GSPC[,"GSPC.Close"]))
names(stocks3) = c("S&P 500")
index(stocks3) = as.Date(index(stocks3))

options(repr.plot.width = 14, repr.plot.height = 8)
stocks_series3 = tidy(stocks3) %>% 

  ggplot(aes(x=index,y=value, color=series)) + 
  geom_line() +
  facet_grid(series~.,scales = "free") + 
  labs(title = "",
       subtitle = "",
       caption = " Source: Yahoo Finance") + xlab("Date") + ylab("Closing Price") + scale_color_manual(values = c("Purple"))
stocks_series3


par(mfrow=c(3,2))
## Daily Percent Change
## VTR
VTR_Close= VTR[, "VTR.Close"]
# Gets percent change in close price from preceding year
pct_change_VTR <- -diff(VTR_Close$VTR.Close)/VTR_Close$VTR.Close[-1] * 100 
pct_VTR = data.frame(pct_change_VTR) # data frame
mn=floor(min(pct_VTR$VTR.Close))
mx=ceiling(max(pct_VTR$VTR.Close))

hist(pct_VTR$VTR.Close,
     main="Daily Change Percentage in VTR Close Price",
     xlab="Daily Change Percentage for VTR",breaks=seq(mn,mx))

mean1 = mean(pct_VTR$VTR.Close); mean1
median1= median(pct_VTR$VTR.Close)
max1= max(pct_VTR$VTR.Close)
min1= min(pct_VTR$VTR.Close)

## "BA"

BA_Close= BA[, "BA.Close"]
# Gets percent change in close price from preceding year
pct_change_BA <- -diff(BA_Close$BA.Close)/BA_Close$BA.Close[-1] * 100 
pct_BA = data.frame(pct_change_BA) # data frame
mn=floor(min(pct_BA$BA.Close))
mx=ceiling(max(pct_BA$BA.Close))

hist(pct_BA$BA.Close,
     main="Daily Change Percentage in BA Close Price",
     xlab="Daily Change Percentage for BA",breaks=seq(mn,mx))

mean2 = mean(pct_BA$BA.Close)
median2= median(pct_BA$BA.Close)
max2= max(pct_BA$BA.Close)
min2 = min(pct_BA$BA.Close)

## "ECL"

ECL_Close= ECL[, "ECL.Close"]
# Gets percent change in close price from preceding year
pct_change_ECL <- -diff(ECL_Close$ECL.Close)/ECL_Close$ECL.Close[-1] * 100 
pct_ECL = data.frame(pct_change_ECL) # data frame
mn=floor(min(pct_ECL$ECL.Close))
mx=ceiling(max(pct_ECL$ECL.Close))

hist(pct_ECL$ECL.Close,
     main="Daily Change Percentage in ECL Close Price",
     xlab="Daily Change Percentage for ECL",breaks=seq(mn,mx))

mean3 = mean(pct_ECL$ECL.Close)
median3 = median(pct_ECL$ECL.Close)
max3= max(pct_ECL$ECL.Close)
min3= min(pct_ECL$ECL.Close)

## "NWS"

NWS_Close= NWS[, "NWS.Close"]
# Gets percent change in close price from preceding year
pct_change_NWS <- -diff(NWS_Close$NWS.Close)/NWS_Close$NWS.Close[-1] * 100 
pct_NWS = data.frame(pct_change_NWS) # data frame
mn=floor(min(pct_NWS$NWS.Close))
mx=ceiling(max(pct_NWS$NWS.Close))

hist(pct_NWS$NWS.Close,
     main="Daily Change Percentage in NWS Close Price",
     xlab="Daily Change Percentage for NWS",breaks=seq(mn,mx))

mean4= mean(pct_NWS$NWS.Close)
median4= median(pct_NWS$NWS.Close)
max4= max(pct_NWS$NWS.Close)
min4= min(pct_NWS$NWS.Close)

## "KR"

KR_Close= KR[, "KR.Close"]
# Gets percent change in close price from preceding year
pct_change_KR <- -diff(KR_Close$KR.Close)/KR_Close$KR.Close[-1] * 100 
pct_KR = data.frame(pct_change_KR) # data frame
mn=floor(min(pct_KR$KR.Close))
mx=ceiling(max(pct_KR$KR.Close))

hist(pct_KR$KR.Close,
     main="Daily Change Percentage in KR Close Price",
     xlab="Daily Change Percentage for KR",breaks=seq(mn,mx))

mean5= mean(pct_KR$KR.Close)
median5= median(pct_KR$KR.Close)
max5= max(pct_KR$KR.Close)
min5= min(pct_KR$KR.Close)

## "XOM"

XOM_Close= XOM[, "XOM.Close"]
# Gets percent change in close price from preceding year
pct_change_XOM <- -diff(XOM_Close$XOM.Close)/XOM_Close$XOM.Close[-1] * 100 
pct_XOM = data.frame(pct_change_XOM) # data frame
mn=floor(min(pct_XOM$XOM.Close))
mx=ceiling(max(pct_XOM$XOM.Close))

hist(pct_XOM$XOM.Close,
     main="Daily Change Percentage in XOM Close Price",
     xlab="Daily Change Percentage for XOM",breaks=seq(mn,mx))

mean6= mean(pct_XOM$XOM.Close)
median6= median(pct_XOM$XOM.Close)
max6= max(pct_XOM$XOM.Close)
min6= min(pct_XOM$XOM.Close)

## "GSPC"

GSPC_Close= GSPC[, "GSPC.Close"]
# Gets percent change in close price from preceding year
pct_change_GSPC <- -diff(GSPC_Close$GSPC.Close)/GSPC_Close$GSPC.Close[-1] * 100 
pct_GSPC = data.frame(pct_change_GSPC) # data frame
mn=floor(min(pct_GSPC$GSPC.Close))
mx=ceiling(max(pct_GSPC$GSPC.Close))

hist(pct_GSPC$GSPC.Close,
     main="Daily Change Percentage in S&P 500 Close Price",
     xlab="Daily Change Percentage for S&P 500",breaks=seq(mn,mx))

mean7 = mean(pct_GSPC$GSPC.Close)
median7 = median(pct_GSPC$GSPC.Close)
max7 = max(pct_GSPC$GSPC.Close)
min7 = min(pct_GSPC$GSPC.Close)


## Table 

#"VTR", "BA", "ECL","NWS","KR","XOM", "^GSPC"

tab <- matrix(c(mean1, mean2, mean3, mean4, mean5, mean6, mean7, median1, median2, median3, median4, median5,
                median6, median7, min1,min2,min3,min4,min5, min6,min7, max1, max2, max3, max4,max5,max6,
                max7), ncol=7, byrow=TRUE)
colnames(tab) <- c('VTR','BA','ECL','NWS','KR','XOM','S&P 500')
rownames(tab) <- c('Mean','Median','Maximum','Minimum')
tab <- as.table(tab); format(tab,digit=2)


## Correlation plot
#install.packages("corrplot")
#install.packages("RColorBrewer")

M <-cor(stocks)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=6, name="RdYlBu"))

#install.packages(c("keras","mlbench","dplyr","magrittr","neuralnet","pkgbuild","pkgload","rlang","remotes","sessioninfo","usethis"))
#devtools::install_github("bips-hb/neuralnet")



#last date is 9/6/2022
#first date is 8/8/2017
T=dim(snp)[2]-2
T

all=matrix(nrow=7,ncol=T)
for (i in seq(7)){
	all[i,]=as.double(gsub(",","",t(snp[i,seq(3,T+2)])))
}
T



TrainStart=1
TrainStop=800
groupsize=10
percent=100

nu1=trainon(all[1,seq(TrainStart,TrainStop)])
nu2=trainon(all[2,seq(TrainStart,TrainStop)])
nu3=trainon(all[3,seq(TrainStart,TrainStop)])
nu4=trainon(all[4,seq(TrainStart,TrainStop)])
nu5=trainon(all[5,seq(TrainStart,TrainStop)])
nu6=trainon(all[6,seq(TrainStart,TrainStop)])
nu7=trainon(all[7,seq(TrainStart,TrainStop)])
nu=list(nu1,nu2,nu3,nu4,nu5,nu6,nu7)


TestStart=600
TestStop=1250
#T=1279

borses=matrix(nrow=7,ncol=TestStop-TestStart-groupsize)
for (ind in seq(1,7)){
	borses[ind,]=bigorsmall(all[ind,seq(TestStart,TestStop)],nu[[ind]])
	#followplot(TestStart,TestStop,all,ind,borses,groupsize)
}

pdchanges=matrix(nrow=7,ncol=TestStop-TestStart)
for (ind in seq(1,7)){
	pdchanges[ind,]=percent*diff(all[ind,seq(TestStart,TestStop)])/all[ind,seq(TestStart,TestStop-1)]
}


eachmeanbors=apply(borses,1,mean)
mbors=apply(borses[seq(1,6),],2,mean)
totalbors=as.numeric(mbors>=4/6)
mean(as.numeric(mbors>=4/6))
sdf=createstaggerdf(mbors,groupsize)





#######Make and evaluate random forest and log regression models on each set of data below:

####BORS to TOTALBORS
target=totalbors[seq(2*groupsize,length(totalbors)-1)]
sdft=cbind(sdf,target)
train=sdft[seq(dim(sdft)[1]%/%2),]
balancedtrain=balancetrain(train)
test=sdft[seq(dim(sdft)[1]%/%2+1,dim(sdft)[1]-1),]

logitModel <- glm( target ~., data = balancedtrain, family = binomial)
predicted <- predict(logitModel, test, type="response")
confmat(predicted,test$target,cutoff=quantile(predicted,.9))

## Random Forest
set.seed(100)
#Model Bagged Forest
bag.F1 <- randomForest(target ~ ., data = train, mtry = 10, importance = TRUE, ntree = 1000, compete = FALSE)
#Output for the bagged forest Model and Error
bag.F1
preds <- predict(bag.F1,test)
confmat(preds,test$target,cutoff=.5)

## Neural net

n <- neuralnet(target ~ .,data = balancedtrain,threshold=.01, hidden = c(6,5,2),linear.output = F,lifesign="full", rep=10)
preds=predict(n,test)
confmat(preds,test$target,cutoff=.5)



####MPDC to TOTALBORS 
## No balancetrain
mpdchanges=apply(pdchanges,2,mean)
sdfmpd=createstaggerdf(mpdchanges,groupsize)
target=totalbors[seq(groupsize,length(totalbors)-1)]
sdft=cbind(sdfmpd,target)
train=sdft[seq(dim(sdft)[1]%/%2),]
test=sdft[seq(dim(sdft)[1]%/%2+1,dim(sdft)[1]-1),]

logitModel <- glm( target ~., data = balancedtrain, family = binomial)
summary(logitModel)
predicted <- predict(logitModel, test, type="response")
predicted
confmat(predicted,test$target,cutoff=quantile(predicted,.9))

## Random Forest

set.seed(100)
bag.F2 <- randomForest(target ~ ., data = train, mtry = 10, importance = TRUE, ntree = 1000, compete = FALSE)
bag.F2
preds <- predict(bag.F2,test)
confmat(preds,test$target,cutoff=.5)




####BORS to S&P500
target=borses[7,seq(2*groupsize,length(borses[7,])-1)]
sdft=cbind(sdf,target)
train=sdft[seq(dim(sdft)[1]%/%2),]
balancedtrain=balancetrain(train)
test=sdft[seq(dim(sdft)[1]%/%2+1,dim(sdft)[1]-1),]


logitModel <- glm( target ~., data = balancedtrain, family = binomial)
summary(logitModel)
predicted <- predict(logitModel, test, type="response")
predicted
confmat(predicted,test$target,cutoff=quantile(predicted,.9))

## Random Forest

set.seed(100)
bag.F3 <- randomForest(target ~ ., data = train, mtry = 10, importance = TRUE, ntree = 1000, compete = FALSE)
bag.F3
preds <- predict(bag.F3,test)
confmat(preds,test$target,cutoff=.5)



####MPDC to S&P500
mpdchanges=apply(pdchanges,2,mean)
sdfmpd=createstaggerdf(mpdchanges,groupsize)
target=borses[7,seq(groupsize,length(borses[7,])-1)]
sdft=cbind(sdfmpd,target)
train=sdft[seq(dim(sdft)[1]%/%2),]
test=sdft[seq(dim(sdft)[1]%/%2+1,dim(sdft)[1]-1),]


logitModel <- glm( target ~., data = train, family = binomial)
summary(logitModel)
predicted <- predict(logitModel, test, type="response")
predicted
confmat(predicted,test$target,cutoff=quantile(predicted,.9))

## Random Forest

set.seed(100)
bag.F4 <- randomForest(target ~ ., data = train, mtry = 10, importance = TRUE, ntree = 1000, compete = FALSE)
bag.F4
preds <- predict(bag.F4,test)
confmat(preds,test$target,cutoff=.5)





