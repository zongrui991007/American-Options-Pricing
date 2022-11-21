source("AmericanOptionPricer.R")
library(quantmod)
library(derivmkts)
library(tidyquant)
#?binomopt

trace(binomopt, edit=TRUE)

# get historical stock prices, and compute the historical volatility

# Pricing date: 1-Oct-2021
# 3M Lookback period: 1-Aug-21 to 1-Oct-21
startDate <- as.Date(2022-04-04)
endDate <- as.Date(2022-04-15)

getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)






options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
getSymbols("AAPL", from = '2022-01-05',
           to = "2022-04-05",warnings = FALSE,
           auto.assign = TRUE)


getSymbols("AAPL", from = '2022-04-04', to = "2022-04-15")




# visualise the data downloaded

names(AAPL)
head(Cl(AAPL))
tail(Cl(AAPL))   # S0
#print the closing prices

plot(Cl(AAPL))

# estimate the historical volatility 

prices <- Cl(AAPL)
head(prices)
tail(prices)
ndays <- length(prices)

#compute daily log-returns

logret <- periodReturn(prices, period="daily", type="log")

plot(logret, main="AAPL daily log returns")

# check the normality of the log-returns

qqnorm(logret, main="Q-Q plot of the log-returns")

# compute annualized volatility

yearvol = sqrt(252.0)*sd(logret)  # stdev(log-ret) STDEV

print(yearvol)   #sigma = 15.75% +- 1.39% (3m)
histvol <- yearvol
histvolerr <- yearvol/sqrt(2*ndays)
histvolerr

# Risk-free interest rate ^IRX = 13-week T-bill rate
getSymbols("^IRX", src="yahoo", from="2022-01-05", to="2022-04-05")

names(IRX)

rfrate <- na.omit(Cl(IRX))
nrf <- length(rfrate)

plot(rfrate, main="13-week T-bill rate")

tail(rfrate)
rf <-  0.538/100
rf
#######################################################
### Price the AMZN options
#######################################################
S0 <-  178.44
Texp <- 9/252
rf <- 0.538/100
histvol <- 0.295


kStrikes <- numeric()
callPrice <- numeric()
putPrice <- numeric()

callDelta <- numeric()
callGamma <- numeric()
putDelta <- numeric()
putGamma <- numeric()


for (i in 1:9) {
  k <- 162.5 + 2.5*(i-1)
  S0up <- 178.44 + 0.1
  S0down <- 178.44 - 0.1
#  copt <- binomial_option("call", histvol, 4/252, rf, k, S0, 40, TRUE)
  copt <- binomial_option("call", histvol, Texp, rf, k, S0, 100, TRUE)
  coptup <- binomial_option("call", histvol, Texp, rf, k, S0up, 100, TRUE)
  coptdown <- binomial_option("call", histvol, Texp, rf, k, S0down, 100, TRUE)
  
  popt <- binomial_option("put", histvol, Texp, rf, k, S0, 100, TRUE)
  poptup <- binomial_option("put", histvol, Texp, rf, k, S0up, 100, TRUE)
  poptdown <- binomial_option("put", histvol, Texp, rf, k, S0down, 100, TRUE)
  
  kStrikes <- c(kStrikes,k)
  callPrice <- c(callPrice,copt$price)
  callDelta <- c(callDelta,(coptup$price - coptdown$price)/0.2)
  callGamma <- c(callGamma,(coptup$price + coptdown$price - 2*copt$price)/0.01)

  putPrice <- c(putPrice,popt$price)
  putDelta <- c(putDelta,(poptup$price - poptdown$price)/0.2)
  putGamma <- c(putGamma,(poptup$price + poptdown$price - 2*popt$price)/0.01)
  
}

callData <- data.frame(kStrikes,callPrice, callDelta,callGamma)
putData <- data.frame(kStrikes,putPrice, putDelta,putGamma)

putData
callData

call.ATM <- S0*histvol*sqrt(Texp/(2*3.1416))
call.ATM

# plot the call option price, delta and gamma vs K
plot(kStrikes, callPrice,
     xlab="Strike", ylab="Price(c)", type='p', main="Price(c)")

plot(kStrikes, callDelta,xlab="Strike", ylab="Delta(c)", type='p', 
     main="Delta(c)")
plot(kStrikes[2:9], callGamma[2:9],xlab="Strike", ylab="Gamma(c)", type='p', 
     main="Gamma(c)")

# plot the put option price, delta and gamma vs K
plot(kStrikes, putPrice,
     xlab="Strike", ylab="Price(p)", type='p')

plot(kStrikes, putDelta,xlab="Strike", ylab="Delta(p)", type='p',
     main="Delta(put)")
plot(kStrikes[2:9], putGamma[2:9],xlab="Strike", ylab="Gamma(p)", type='p',
     main="Gamma(put)")

#--- example from Problem 5.1 -----------
S0 = 90.0
Texp = 9/12
sigma = 0.28
rf = 0.03

#binomopt(s, k, v, r, tt, d, nstep = 10, american = TRUE,
#         putopt=FALSE, specifyupdn=FALSE, crr=FALSE, jarrowrudd=FALSE,
#         up=1.5, dn=0.5, returntrees=FALSE, returnparams=FALSE,
#         returngreeks=FALSE)
c.amer <- binomopt(S0, 93.0, sigma, rf, Texp, rf, 3, TRUE, FALSE)
p.amer <- binomopt(S0, 93.0, sigma, rf, Texp, rf, 3, TRUE, TRUE)

c.amer <- binomopt(S0, 93.0, sigma, rf, Texp, rf, 3, TRUE, FALSE, returngreeks=TRUE)


c.amer[2]
bscall(S0, 93.0, sigma, rf, Texp, rf)

p.amer
bsput(S0, 93.0, sigma, rf, Texp, rf)

# convergence study 
nSteps <- numeric()
putPriceVsN <- numeric()
callPriceVsN <- numeric()
putDeltaVsN <- numeric()
putGammaVsN <- numeric()


for (i in 1:500) {

  n <- i + 5
  coptn <- binomopt(S0, 93.0, sigma, rf, Texp, rf, n, TRUE, FALSE, returngreeks=TRUE)
  poptn <- binomopt(S0, 93.0, sigma, rf, Texp, rf, n, american=TRUE, putopt=TRUE, returngreeks=TRUE)
  
  
    nSteps <- c(nSteps,n)
  callPriceVsN <- c(callPriceVsN,coptn[1])
  putPriceVsN <- c(putPriceVsN,poptn[1])
  putDeltaVsN <- c(putDeltaVsN,poptn[2])
  putGammaVsN <- c(putGammaVsN,poptn[3])
}

plot(nSteps,callPriceVsN,xlim=c(0,500), #ylim=c(2.2, 2.3),
     xlab="No. Steps", ylab="Price(c)", type='l', main="Call option price vs n")
abline(h=7.286, col="red")

plot(nSteps,putPriceVsN,xlim=c(0,500), #ylim=c(2.2, 2.3),
     xlab="No. Steps", ylab="Price(p)", type='l', main="Put option price vs n")
abline(h=10.23, col="red")


plot(nSteps,putDeltaVsN,xlim=c(0,500), #ylim=c(2.2, 2.3),
     xlab="No. Steps", ylab="Delta(p)", type='l', main="Put Delta vs n")

plot(nSteps,putGammaVsN,xlim=c(100,500), ylim=c(0.0182,0.01835),
     xlab="No. Steps", ylab="Gamma(p)", type='l', main="Put Gamma vs n")


plot(nSteps,putPriceVsN,xlim=c(0,100), #ylim=c(2.2, 2.3),
     xlab="No. Steps", ylab="Price(p)", type='l', main="Put option price vs n")
abline(h=putPriceVsN[100], col="red")

#---- put greeks vs strike

nStrikes <- numeric()
putPriceVsK <- numeric()
callPriceVsK <- numeric()
putDeltaVsK <- numeric()
putGammaVsK <- numeric()


for (i in 1:200) {
  
 K <- 50 + 0.5*i
#  coptn <- binomopt(S0, K, sigma, rf, Texp, rf, n, TRUE, FALSE, returngreeks=TRUE)
  poptn <- binomopt(S0, K, sigma, rf, Texp, rf, 100, american=TRUE, putopt=TRUE, returngreeks=TRUE)
  
  
  nStrikes <- c(nStrikes,K)
#  callPriceVsK <- c(callPriceVsK,coptn[1])
  putPriceVsK <- c(putPriceVsK,poptn[1])
  putDeltaVsK <- c(putDeltaVsK,poptn[2])
  putGammaVsK <- c(putGammaVsK,poptn[3])
}


plot(nStrikes, putPriceVsK,xlim=c(50,150), #ylim=c(2.2, 2.3),
     xlab="Strike", ylab="Price(p)", type='l', main="Call option price vs K")
abline(h=10.23, col="red")

plot(nStrikes,putDeltaVsK,xlim=c(50,150), #ylim=c(2.2, 2.3),
     xlab="Strike", ylab="Delta(p)", type='l', main="Put Delta vs n")

plot(nStrikes,putGammaVsK,xlim=c(50,150), #ylim=c(2.2, 2.3),
     xlab="Strike", ylab="Gamma(p)", type='l', main="Put Gamma vs n")

putDeltaVsK


##########################################
# hedging study
##########################################
#American put K=1780.0, 4 days to expiry
k <- 1780.0

putPrice <- numeric()
putDelta <- numeric()
dPrice <- numeric()
hedgedPrice <- numeric()

#day 0: 2-Dec-2019
S0 <- 1781.6
S0up <- S0+ 0.1
Texp <- 4/252

popt <- binomial_option("put", histvol, Texp, rf, k, S0, 100, TRUE)
poptup <- binomial_option("put", histvol, Texp, rf, k, S0up, 100, TRUE)

price0 <- popt$price
delta0 <- (poptup$price - popt$price)/0.1
putPrice <- c(putPrice,price0)
putDelta <- c(putDelta,delta0)

#day 1: 3-Dec-2019
S1 <- 1769.96
S1up <- S1 + 0.1
Texp <- 3/252

popt <- binomial_option("put", histvol, Texp, rf, k, S1, 100, TRUE)
poptup <- binomial_option("put", histvol, Texp, rf, k, S1up, 100, TRUE)

price1 <- popt$price
delta1 <- (poptup$price - popt$price)/0.1
putPrice <- c(putPrice,price1)
putDelta <- c(putDelta,delta1)

dp1 <- price1 - price0
hedgeddp1 <- dp1 - delta0*(S1 - S0)
dPrice <- c(dPrice,dp1)
hedgedPrice <- c(hedgedPrice,hedgeddp1)

#day 2: 4-Dec-2019
S2 <- 1760.69
S2up <- S2 + 0.1
Texp <- 2/252

popt <- binomial_option("put", histvol, Texp, rf, k, S2, 100, TRUE)
poptup <- binomial_option("put", histvol, Texp, rf, k, S2up, 100, TRUE)

price2 <- popt$price
delta2 <- (poptup$price - popt$price)/0.1
putPrice <- c(putPrice,price2)
putDelta <- c(putDelta,delta2)

dp2 <- price2 - price1
hedgeddp2 <- dp2 - delta1*(S2 - S1)
dPrice <- c(dPrice,dp2)
hedgedPrice <- c(hedgedPrice,hedgeddp2)

#day 3: 5-Dec-2019
S3 <- 1740.48
S3up <- S3 + 0.1
Texp <- 1/252

popt <- binomial_option("put", histvol, Texp, rf, k, S3, 100, TRUE)
poptup <- binomial_option("put", histvol, Texp, rf, k, S3up, 100, TRUE)

price3 <- popt$price
delta3 <- (poptup$price - popt$price)/0.1
putPrice <- c(putPrice,price3)
putDelta <- c(putDelta,delta3)

dp3 <- price3 - price2
hedgeddp3 <- dp3 - delta2*(S3 - S2)
dPrice <- c(dPrice,dp3)
hedgedPrice <- c(hedgedPrice,hedgeddp3)

#day 4: 6-Dec-2019
S4 <- 1751.60

price4 <- max(k - S4,0)

putPrice <- c(putPrice,price4)

dp4 <- price4 - price3
hedgeddp4 <- dp4 - delta3*(S4 - S3)
dPrice <- c(dPrice,dp4)
hedgedPrice <- c(hedgedPrice,hedgeddp4)

putPrice
# 13.28089 17.82806 22.38536 39.58822 28.40000
putDelta
# -0.4562717 -0.6145443 -0.7557467 -0.9893727
dPrice # daily PnL of unhedged option
#  4.547176   4.557296  17.202862 -11.188219
hedgedPrice # daily PnL of hedged option
# -0.7638266 -1.1395306  1.9292216 -0.1863942


####################################################
# option chains in Yahoo Finance

#a = getOptionChain("TSLA",src="yahoo","2020-07-17")
amzn1 = getOptionChain("AMZN",src="yahoo")
amzn2 = getOptionChain("AMZN",src="yahoo","2020-11-27")
ppl0 = getOptionChain("PYPL",src="yahoo","2020-11-20")

ppl1 = getOptionChain("PYPL",src="yahoo","2020-12-18")

dfam1=data.frame(amzn1[1])
dfam2=data.frame(amzn1[2])
df0 = data.frame(ppl0[1])
df1 = data.frame(ppl1[1])

head(df0)
head(df1)

dim.data.frame(dfPYPL0)
dim.data.frame(df3)

k1c=df1$calls.strike
#pc1 = df1$calls.lastPrice
midcall1 = 0.5*df1$calls.ask + 0.5*df1$calls.bid
impvolc1 = df1$calls.impliedVolatility

points(k1c,impvolc1, col="blue")

k1p=df2$puts.strike
#pp1 = df1$calls.lastPrice
midput1 = 0.5*df2$puts.ask + 0.5*df2$puts.bid
impvolp1 = df2$puts.impliedVolatility

points(k1p,impvolp1, col="red")


# plot Call - Put vs K

length(midcall1)
length(midput1)

head(k1c)
head(k1p)

cpDiff <- midcall1 - midput1

(fitCallPut <- lm(cpDiff ~ k1call))
plot(k1call, cpDiff)

d3=data.frame(calls)
d3[,2]=a$calls$Last
d3[,3]=a$calls$Strike
head(d3)

oanda.currencies
getSymbols("USD/BRL", src="oanda", from="2020-11-01", to="2020-11-10")
plot(USDBRL)

head(USDBRL)

callBStest <- BSoptionprice("call",355.5,360,1/12,0.17,0.001)
putBStest <- BSoptionprice("put",355.5,360,1/12,0.17,0.001)
# put = 9.46377, call = 4.99377

#1, x, x^2, x^3

