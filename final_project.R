#analysis of BRICS nations 
#BRAZIL
#EDA
brazil<-read.csv("Brazil.f.csv")
View(brazil)
nrow(brazil)
ncol(brazil)
str(brazil)
summary(brazil)
sd(brazil$BOP)
sd(brazil$CPI)
sd(brazil$GDP)
sd(brazil$CBPR)
sd(brazil$ER)
#all the standard deviations are in the range of 0-3, hence a good sd.
var(brazil$BOP)
var(brazil$CPI)
var(brazil$GDP)
var(brazil$CBPR)
var(brazil$ER)
#no negative values, all the values more than 1,satisfactorly spread out
brazil <- subset(brazil, select = -quarterly)
brazil
cor(brazil)
data.frame(brazil$BOP)
library(moments)
skewness(brazil)
# two things are negatively skewed not good.
hist(brazil$BOP)
hist(brazil$CPI)
hist(brazil$GDP)
hist(brazil$CBPR)
hist(brazil$ER)
plot(brazil$qarterly,brazil$BOP,type="l",xlab = "quarterly",ylab = "BOP")
#splitting and training the data 
train<-sample(1:nrow(brazil),0.7*nrow(brazil))
trains<-brazil[train,]
test<-brazil[-train,]
dim(trains)
dim(test)
#multiple linear regression model 
reg<-lm(ER~BOP+CPI+GDP+CBPR,data = trains)
summary(reg)
cor(reg$model)
coefficients(reg)
reg2<-lm(ER~BOP+CPI+GDP+CBPR,data = test)
summary(reg2)
sncor(reg2$model)
coefficients(reg2)
brazl

####################################
####RUSSIA#####
####################################
russia<-read.csv("Russia.f.csv")
View(russia)
nrow(russia)
ncol(russia)
str(russia)
head(russia)
tail(russia)
summary(russia)
#sd
sd(russia$BOP)
sd(russia$CPI)
sd(russia$GDP)
sd(russia$CBPR)
sd(russia$ER)
#variance
var(russia$BOP)
var(russia$CPI)
var(russia$GDP)
var(russia$CBPR)
var(russia$ER)
#correlation
russia <- subset(russia, select = -quarterly)
snrussia
cor(russia)
library(moments)
skewness(russia)
#histogram
hist(russia$BOP)
hist(russia$CPI)
hist(russia$GDP)
hist(russia$CBPR)
hist(russia$ER)
#splitting and training the data 
train<-sample(1:nrow(russia),0.7*nrow(russia))
trains<-russia[train,]
test<-russia[-train,]
dim(trains)
dim(test)
#multiple linear regression model 
reg<-lm(ER~BOP+CPI+GDP+CBPR,data = trains)
summary(reg)
cor(reg$model)
coefficients(reg)

reg2<-lm(ER~BOP+CPI+GDP+CBPR,data = test)
summary(reg2)
cor(reg2$model)
coefficients(reg2)
#time series
russia_ts<- ts(russia[,1],start=c(2011,1),end=c(2021,1),frequency = 44)
adf.test(russia_ts)
y=diff(russia_ts,differences = 2)
adf.test(y)
acf(y)
pacf(y)
components.ts = decompose(y)
plot(components.ts)
fit<-snaive(y)
print(summary(fit))
checkresiduals(fit)
fit_arima<-auto.arima(y,stepwise = FALSE,approximation = FALSE,trace = TRUE)
#auto.arima is function under fpp2 package
print(summary(fit_arima))
checkresiduals(fit_arima)
fcst<-forecast(fit_arima)
autoplot(fcst,include=120)
print(summary(fcst))





############

########################
####INDIA
########################
india<-read.csv("india.f.csv")
View(india)
nrow(india)
ncol(india)
str(india)
head(india)
tail(india)
summary(india)
#sd
sd(india$BOP)
sd(india$CPI)
sd(india$GDP)
sd(india$CBRP)
sd(india$ER)
#variance
var(india$BOP)
var(india$CPI)
var(india$GDP)
var(india$CBRP)
var(india$ER)
#correlation
india <- subset(india, select = -quarterly)
india
cor(india)
library(moments)
skewness(india)
#histogram
hist(india$BOP)
hist(india$CPI)
hist(india$GDP)
hist(india$CBRP)
hist(india$ER)
#splitting and training the data 
train<-sample(1:nrow(india),0.7*nrow(india))
trains<-india[train,]
test<-india[-train,]
dim(trains)
dim(test)
#multiple linear regression model 
reg<-lm(ER~BOP+CPI+GDP+CBRP,data = trains)
summary(reg)
cor(reg$model)
coefficients(reg)
reg2<-lm(ER~BOP+CPI+GDP+CBRP,data = test)
summary(reg2)
cor(reg2$model)
coefficients(reg2)
func=data.frame(ER=2)
func
predict(reg2,func)

###BEST MODEL, R SQUARED,P-VALUE IS OF INDIA.(BOTH THE MODELS.)

############
#time series
###########
india_ts<- ts(india[,1],start=c(2011,1),end=c(2021,1),frequency = 8)
ind=diff(india_ts)
adf.test(ind)
acf(ind)
pacf(ind)
components.ts = decompose(ind)
plot(components.ts)


#########################
#preliminary analysis
########################

#all the variables appear to have a stationary trend therefore,seasonality needs to be investigated.
#a benchmark method to forecast
######
fit<-snaive(ind)
print(summary(fit))
checkresiduals(fit)
################
##ARIMA
############
fit_arima<-auto.arima(ind,stepwise = FALSE,approximation = FALSE,trace = TRUE)
#auto.arima is function under fpp2 package
print(summary(fit_arima))
checkresiduals(fit_arima)
adf.test(fit_arima)


################
#forecasting with ARIMA model 
##############

fcst<-forecast(fit_arima)
autoplot(fcst,include=120)
print(summary(fcst))









##################
####CHINA
##################

china<-read.csv("china.f.csv")
View(china)
nrow(china)
ncol(china)
str(china)
head(china)
tail(china)
summary(china)
#sd
sd(china$BOP)
sd(china$CPI)
sd(china$GDP)
sd(china$CBPR)
sd(china$ER)
#variance
var(china$BOP)
var(china$CPI)
var(china$GDP)
var(china$CBPR)
var(china$ER)
#correlation
china <- subset(china, select = -quarterly)
china
cor(china)
library(moments)
skewness(china)
#histogram
hist(china$BOP)
hist(china$CPI)
hist(india$GDP)
hist(china$CBPR)
hist(china$ER)
#splitting and training the data 
train<-sample(1:nrow(china),0.7*nrow(china))
trains<-china[train,]
test<-china[-train,]
dim(trains)
dim(test)
#multiple linear regression model 
reg<-lm(ER~BOP+CPI+GDP+CBPR,data = trains)
summary(reg)
cor(reg$model)
coefficients(reg)
#GOOD R AND R SQUARED VALUES
reg2<-lm(ER~BOP+CPI+GDP+CBPR,data = test)
summary(reg2)
#GOOD P VALUE AND R AND R SQUARED 
cor(reg2$model)
coefficients(reg2)
#time series 
china_ts<- ts(china[,1],start=c(2011,1),end=c(2021,1),frequency = 8)
adf.test(china_ts)
y=diff(china_ts,differences = 2)
adf.test(y)
acf(y)
pacf(y)
components.ts = decompose(y)
plot(components.ts)
fit<-snaive(y)
print(summary(fit))
checkresiduals(fit)
fit_arima<-auto.arima(y,stepwise = FALSE,approximation = FALSE,trace = TRUE)
#auto.arima is function under fpp2 package
print(summary(fit_arima))
checkresiduals(fit_arima)
fcst<-forecast(fit_arima)
autoplot(fcst,include=120)
print(summary(fcst))



################
## SOUTH AFRICA 
################
sa<-read.csv("SA.f.csv")
View(sa)
nrow(sa)
ncol(sa)
str(sa)
head(sa)
tail(sa)
summary(sa)
#sd
sd(sa$BOP)
sd(sa$CPI)
sd(sa$GDP)
sd(sa$CBPR)
sd(sa$ER)
#variance
var(sa$BOP)
var(sa$CPI)
var(sa$GDP)
var(sa$CBPR)
var(sa$ER)
#correlation
sa <- subset(sa, select = -quarterly)
sa
cor(sa)
library(moments)
skewness(sa)
#histogram
hist(sa$BOP)
hist(sa$CPI)
hist(sa$GDP)
hist(sa$CBPR)
hist(sa$ER)
#splitting and training the data 
train<-sample(1:nrow(sa),0.7*nrow(sa))
trains<-sa[train,]
test<-sa[-train,]
dim(trains)
dim(test)
#multiple linear regression model 
reg<-lm(ER~BOP+CPI+GDP+CBPR,data = trains)
summary(reg)
cor(reg$model)
coefficients(reg)
reg2<-lm(ER~BOP+CPI+GDP+CBPR,data = test)
summary(reg2)
cor(reg2$model)
coefficients(reg2)
as.numeric(coefficients(reg)[ 7.8106 ]+coefficients(reg)[ 0.6607 ]*24)
func=data.frame(ER=2)
func
predict(reg,func)




