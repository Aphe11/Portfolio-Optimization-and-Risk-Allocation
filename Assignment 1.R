library(PerformanceAnalytics)
library(quadprog)
library(quantmod)
library(PortfolioAnalytics)
library(fPortfolio)
library(fAssets)
library(xts)
library(fBasics)
library(evir)
library(zoo)
library(tseries)
library(ggplot2)
library(TTR)
library(sn)
library(rugarch)
library(dplyr)
library(ismev)
library(fBasics)
library(fExtremes)
library(MASS)
library(copula)
library(fGarch)  
library(ggplot2)

#---------------------------------------------------------------1. DOWNLOAD DATA
startDate <- as.Date("2020-01-01")
endDate <- as.Date("2025-03-10")
portfolio <- c("SOL.JO", "ABG.JO")

# Download data
quantmod::getSymbols(portfolio, src = 'yahoo', from = startDate, to = endDate, auto.assign = TRUE)

# Extracting the Closing Prices
SASOL <- `SOL.JO`[, "SOL.JO.Close"]
ABSA <- `ABG.JO`[, "ABG.JO.Close"]

par(mfrow = c(1,1))

plot(SASOL, type="l", col="blue")
plot(ABSA, type="l", col="blue")

# Creating combined data
SASOL_ABSA_Portfolio <- bind_cols(SASOL, ABSA)
date_range <- seq(from = as.Date("2020-01-01"), by = "day", length.out = nrow(SASOL_ABSA_Portfolio))
SASOL_ABSA_Portfolio$date <- date_range
SASOL_ABSA_Portfolio <- xts(SASOL_ABSA_Portfolio[, -ncol(SASOL_ABSA_Portfolio)], order.by = SASOL_ABSA_Portfolio$date)
SASOL_ABSA_Portfolio<- as.timeSeries(SASOL_ABSA_Portfolio)
View(SASOL_ABSA_Portfoli)
#---------------------------------------------------2. UNIVARIATE STYLIZED FACTS
     
# Estimate Returns
SASOL_returns<-na.omit(timeSeries::returns(SASOL, percentage=TRUE, c("continuous", "discrete", "compound", "simple")[1], trim=TRUE))
SASOL_returns<-as.timeSeries(SASOL_returns)
timestampsSASOL <- index(SASOL)
timestampsSASOL<- as.Date(timestampsSASOL)
timestampsSASOL <- seq(from = startDate, by = "days", length.out = length(SASOL_returns))
SASOL_returnsB=xts(SASOL_returns,order.by = timestampsSASOL)
SASOL_returnsB<-na.omit(SASOL_returnsB)


ABSA_returns<-na.omit(timeSeries::returns(ABSA, percentage=TRUE, c("continuous", "discrete", "compound", "simple")[1], trim=TRUE))
ABSA_returns<-as.timeSeries(ABSA_returns)
timestampsABSA <- index(ABSA)
timestampsABSA<- as.Date(timestampsABSA)
timestampsABSA <- seq(from = startDate, by = "days", length.out = length(ABSA_returns))
ABSA_returnsB=xts(ABSA_returns,order.by = timestampsABSA)
ABSA_returnsB<-na.omit(ABSA_returnsB)

#  Plot Returns
par(mar = c(1, 1, 1, 1) + 1) #Adjust plot margins
plot(SASOL_returns,title=FALSE, main= "SASOL Returns", type="l", col="blue")
plot(ABSA_returns, title=FALSE, main= "ABSA Returns", type="l", col="blue")


# Stylized Fact 1 (Time series data of returns is not iid)
par(mfrow = c(2,2))
seriesPlot(SASOL_returns, title= FALSE , main = "Daily Returns of SASOL", col = "blue")
boxplot(SASOL_returns, title= FALSE , main = "Boxplot of SASOL Returns", col = "blue" , cex = 0.5, pch = 19)
acf(SASOL_returnsB, main = "ACF of SASOL Returns", lag.max = 20 , ylab = " ", xlab = " ", col ="blue", ci.col = "red")
pacf(SASOL_returnsB, main = "PACF of SASOL Returns", lag.max = 20 , ylab = " ", xlab = " ", col= "blue", ci.col = "red")

summary(SASOL_returns)
sd(SASOL_returns)
skewness(SASOL_returns)
kurtosis(SASOL_returns)

seriesPlot(ABSA_returns, title= FALSE , main = "Daily Returns of ABSA", col = "blue")
boxplot(ABSA_returns, title= FALSE , main = "Boxplot of ABSA Returns", col = "blue" , cex = 0.5, pch = 19)
acf(ABSA_returnsB, main = "ACF of ABSA Returns", lag.max = 20 , ylab = " ", xlab = " ", col ="blue", ci.col = "red")
pacf(ABSA_returnsB, main = "PACF of ABSA Returns", lag.max = 20 , ylab = " ", xlab = " ", col= "blue", ci.col = "red")

summary(ABSA_returns)
sd(ABSA_returns)
skewness(ABSA_returns)
kurtosis(ABSA_returns)


# Stylized Fact 2 (The absolute or squared returns are highly autocorrelated)
SASOL_absolute_returns <- abs(SASOL_returns)
SASOL_returns100 <- tail( sort( abs( series(SASOL_returns))), 100)[ 1 ]
idx<-which(series(SASOL_absolute_returns) > SASOL_returns100, arr.ind = TRUE)
SASOL_absolute_returns100 <- timeSeries( rep(0, length(SASOL_returns) ), charvec = time (SASOL_returns))
SASOL_absolute_returns100[idx, 1] <- SASOL_absolute_returns [idx]
SASOL_absolute_returnsX <- xts(SASOL_absolute_returns, order.by = timestampsSASOL)
SASOL_absolute_returnsX<-na.omit(SASOL_absolute_returnsX)
acf(SASOL_absolute_returnsX, main = "ACF of SASOL Absolute Returns" , lag.max = 20, ylab = " ", xlab= " ", col = "blue" , ci.col ="red")
pacf(SASOL_absolute_returnsX, main = "PACF of SASOL Absolute Returns", lag.max = 20 , ylab = " ", xlab = " ", col= "blue", ci.col = "red")
qqnormPlot (SASOL_returnsB, main = "QQ-Plot of SASOL Returns", title = FALSE, col = "blue", cex= 0.5, pch = 19)
plot(SASOL_absolute_returns100 , type = "h" , main = "SASOL Volatility Clustering", ylab = " ", xlab = " ", col = "blue")

ABSA_absolute_returns <- abs(ABSA_returns)
ABSA_returns100 <- tail( sort( abs( series(ABSA_returns))), 100)[ 1 ]
idx_ABSA<-which(series(ABSA_absolute_returns) > ABSA_returns100, arr.ind = TRUE)
ABSA_absolute_returns100 <- timeSeries( rep(0, length(ABSA_returns) ), charvec = time (ABSA_returns))
ABSA_absolute_returns100[idx_ABSA, 1] <- ABSA_absolute_returns [idx_ABSA]
ABSA_absolute_returnsX <- xts(ABSA_absolute_returns, order.by = timestampsABSA)
ABSA_absolute_returnsX<-na.omit(ABSA_absolute_returnsX)
acf(ABSA_absolute_returnsX, main = "ACF of ABSA Absolute Returns" , lag.max = 20, ylab = " ", xlab= " ", col = "blue" , ci.col ="red")
pacf(ABSA_absolute_returnsX, main = "PACF of ABSA Absolute Returns", lag.max = 20 , ylab = " ", xlab = " ", col= "blue", ci.col = "red")
qqnormPlot (ABSA_returnsB, main = "QQ-Plot of ABSA Returns", title = FALSE, col = "blue", cex= 0.5, pch = 19)
plot(ABSA_absolute_returns100 , type = "h" , main = " ABSA Volatility Clustering", ylab = " ", xlab = " ", col = "blue")

#-------------------------------------------------4. MULTIVARIATE STYLIZED FACTS 

# (CROSS-CORRELATION)
par(mfrow = c(3,1))
par(mar = c(2, 2, 2, 2) + 2)
layout( matrix(1:2, nrow = 2 , ncol = 2 , byrow = TRUE))
plot(SASOL_ABSA_Portfolio, type = "l", col = c("blue", "red"), lwd = 2, main = "SASOL and ABSA Portfolio Closing Prices", xlab = "Date",  ylab = "Returns")  
colnames(SASOL_returns)<-c("SASOL")
colnames(ABSA_returns)<-c("ABSA")
SASOLC<-as.numeric(coredata(SASOL_returns))
ABSAC<-as.numeric(coredata(ABSA_returns))
ccf(SASOLC, ABSAC, ylab = " ", xlab = " ", lag.max = 20, main = "Returns SASOL vs ABSA",type = "correlation")

# (ROLLING CORRELATION)
rollc = function(x) {
  num_cols <- ncol(x) 
  if (num_cols == 2) {
    rcor <- cor(x[, 1], x[, 2])
    return(rcor)
  } else {
    rcor <- cor(x)[lower.tri(diag(num_cols), diag = FALSE)]
    return(rcor)
  }
}

# Calculating Rolling Correlation
SASOL_ABSA = rollapply(as.matrix(cbind(SASOL_returns,ABSA_returns)), width = 250, rollc, align = "right", by.column =FALSE)

# Ploting Rolling Correlation
par(mar = c(1, 1, 1, 1) + 3)
par(mfrow = c(1,1))
plot(SASOL_ABSA,  type="l", col="blue")



# --------------5 & 6. FITTING DISTRIBUTIONS//VALUE at RISK & EXPECTED SHORTFALL



# Goodness of Fit
x=SASOL_returns
fitted_densitySASOL <- dsn(x, xi = 2.64, omega = 4.49, alpha = -0.985)
ymax <- max(hist(x, plot = FALSE)$density, 
                         density(x)$y, 
                        fitted_densitySASOL) * 1.1
hist(x, freq = FALSE, main = "Fitted Skewed Student t Distribution",
          ylim = c(0, ymax), xlab = "Returns", col = "lightgray")
lines(density(x), col = "blue", lwd = 2)
curve(dsn(x, xi = 2.64, omega = 4.49, alpha = -0.985), 
            add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Empirical Density", "Skewed Student t Fit"), col = c("blue", "red"),lty = 1, lwd = 2,bty = "n")


library(ghyp)
ef = density(SASOL_returns)
ghdfitSASOL = fit.ghypuv(SASOL_returns, symmetric = FALSE, control= list(maxit = 1000)) #Generalized Hyperbolic (GH) Distribution
hypfitSASOL = fit.hypuv(SASOL_returns, symmetric= FALSE, control = list(maxit = 1000)) #Hypergeometric (HYP) Distribution
nigfitSASOL = fit.NIGuv (SASOL_returns, symmetric = FALSE, control = list(maxit = 1000)) #Normal Inverse Gaussian 
#Densities
ghddensSASOL = dghyp(ef$x, ghdfitSASOL)
hypdensSASOL = dghyp(ef$x, hypfitSASOL)
nigdensSASOL = dghyp(ef$x, nigfitSASOL)
nordensSASOL = dnorm(ef$x, mean = mean(SASOL_returns) , sd = sd(SASOL_returns))
col.def=c("black", "red", "blue", "green", "yellow")
par(mar = c(1, 1, 1, 1))
plot(ef, xlab = " " , ylab = expression(f(x)), ylim = c(0, 0.25))
lines( ef$x, ghddensSASOL, col = "red")
lines( ef$x, hypdensSASOL, col= "blue")
lines( ef$x, nigdensSASOL, col = "green")
lines( ef$x, nordensSASOL, col = "yellow")
legend("topleft", legend = c("empirical", "GHD", "HYP", "NIG", "NORM"),
       col= col.def, lty = 1, cex = 0.5)

#Comparing the distributions
par(mfrow = c(1,1))
par(mar = c(0.5, 0.5, 0.5, 0.5) + 5)
qqghyp(ghdfitSASOL, line = TRUE, ghyp.col = "red", plot.legend= FALSE, gaussian =FALSE, main = "", cex = 0.8)
qqghyp(hypfitSASOL, add = TRUE, ghyp.pch = 2 , ghyp.col = "blue", gaussian = FALSE,  cex = 0.8)
qqghyp(nigfitSASOL, add = TRUE, ghyp.pch = 2 , ghyp.col = "blue", gaussian = FALSE, cex = 0.8)
legend( "topleft", legend = c("GHD", "HYP", "NIG"), col = col.def[-c (1, 5)] , pch= 1:3, cex = 0.5)

AIC_SASOL = stepAIC.ghyp(SASOL_returns, dist= c("ghyp", "hyp", "NIG"), symmetric= FALSE,control=list( maxit = 1000))
LRghdnig = lik.ratio.test(ghdfitSASOL, nigfitSASOL)
LRghdhyp = lik.ratio.test(ghdfitSASOL, hypfitSASOL)

AIC_SASOL

#Value at Risk of SASOL
p = seq(0.001, 0.05, 0.001)

ghd.VaR_SASOL = abs(qghyp(p, ghdfitSASOL))
hyp.VaR_SASOL = abs(qghyp(p, hypfitSASOL))
nig.VaR_SASOL = abs(qghyp(p, nigfitSASOL))
nor.VaR_SASOL = abs(qnorm(p, mean = mean( SASOL_returns), sd = sd(SASOL_returns)))
emp.VaR_SASOL = abs(quantile(x = SASOL_returns, probs = p))

par(mar = c(0.5, 0.5, 0.5, 0.5) + 4)
plot( emp.VaR_SASOL, type = "l", xlab = " ", ylab = "VaR", axes = FALSE, ylim=range(c(hyp.VaR_SASOL,nig.VaR_SASOL, ghd.VaR_SASOL, nor.VaR_SASOL, emp.VaR_SASOL)))
box ( )
axis(1, at = seq(along = p), labels = names(emp.VaR_SASOL), tick = FALSE)
axis(2, at = pretty(range(emp.VaR_SASOL, ghd.VaR_SASOL, hyp.VaR_SASOL, nig.VaR_SASOL, nor.VaR_SASOL)))
lines(seq(along = p), ghd.VaR_SASOL, col = "red")
lines(seq(along = p), hyp.VaR_SASOL, col = "blue")
lines(seq(along = p) , nig.VaR_SASOL, col = "green")
lines(seq(along = p) , nor.VaR_SASOL, col = "orange")
legend("topright", legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),col = col.def, lty = 1, cex = 0.5)

#Expected Shortfall for SASOL
ghd.ES_SASOL = abs(ESghyp(p, ghdfitSASOL))
hyp.ES_SASOL = abs(ESghyp(p, hypfitSASOL))
nig.ES_SASOL = abs(ESghyp(p, nigfitSASOL))
nor.ES_SASOL = abs(mean(SASOL_returns)-sd(SASOL_returns)*dnorm(qnorm(1-p))/p)
obs.p_SASOL = ceiling(p*length(SASOL_returns))
emp.ES_SASOL = sapply(obs.p_SASOL, function(x)abs(mean( sort(c(SASOL_returns))[1:x])))

plot(emp.ES_SASOL, type="l", xlab="Percentile", ylab="ES", axes=FALSE, ylim=range(c(hyp.ES_SASOL, nig.ES_SASOL, ghd.ES_SASOL, nor.ES_SASOL, emp.ES_SASOL)))
box()
axis(1, at=seq(1, length(p), by=4), labels=paste0(round(p[seq(1, length(p), by=4)]*100, 0), "%"), tick=FALSE)
axis(2, at=pretty(range(emp.ES_SASOL, ghd.ES_SASOL, hyp.ES_SASOL, nig.ES_SASOL, nor.ES_SASOL)))
lines(1:length(p), ghd.ES_SASOL, col="red")
lines(1:length(p), hyp.ES_SASOL, col="blue")
lines(1:length(p), nig.ES_SASOL, col="green")
lines(1:length(p), nor.ES_SASOL, col="orange")
legend("topright", legend=c("Empirical", "GHD", "HYP", "NIG", "Normal"), col=col.def, lty=1, cex=0.6)




# Plotting ABSA Fittings

# Goodness of Fit
x <- ABSA_returns
fitted_density_ABSA <- dsn(x, xi = 2.64, omega = 4.49, alpha = -0.985)
ymax <- max(hist(x, plot = FALSE)$density, density(x)$y, fitted_density_ABSA) * 1.1  
hist(x, freq = FALSE, main = "ABSA Fitted Skew Normal Distribution", ylim = c(0, ymax), xlab = "Returns", col = "lightgray")
lines(density(x), col = "blue", lwd = 2)
curve(dsn(x, xi = 2.64, omega = 4.49, alpha = -0.985), add = TRUE, col = "red", lwd = 2)
legend("topleft", legend = c("Empirical Density", "Skew Normal Fit"), col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")


ef = density(ABSA_returns)
ghdfitABSA = fit.ghypuv(ABSA_returns, symmetric = FALSE, control= list(maxit = 1000))
hypfitABSA = fit.hypuv(ABSA_returns, symmetric= FALSE, control = list(maxit = 1000))
nigfitABSA = fit.NIGuv (ABSA_returns, symmetric = FALSE, control = list(maxit = 1000))
#Densities
ghddensABSA = dghyp(ef$x, ghdfitABSA)
hypdensABSA = dghyp(ef$x, hypfitABSA)
nigdensABSA = dghyp(ef$x, nigfitABSA)
nordensABSA = dnorm(ef$x, mean = mean(ABSA_returns) , sd = sd(ABSA_returns))
col.def=c("black", "red", "blue", "green", "orange")
par(mar = c(1.5, 1.5, 1.5, 1.5)+1)
plot(ef, xlab = " " , ylab = expression(f(x)), ylim = c(0, 0.25))
lines( ef$x, ghddensABSA, col = "red")
lines( ef$x, hypdensABSA, col= "blue")
lines( ef$x, nigdensABSA, col = "green")
lines( ef$x, nordensABSA, col = "orange")
legend("topleft", legend = c("empirical", "GHD", "HYP", "NIG", "NORM"), col= col.def, lty = 1, cex = 0.5)

# Comparing the distributions
par(mar = c(1.5, 1.5, 1.5, 1.5)+2.5)
qqghyp(ghdfitABSA, line = TRUE, ghyp.col = "red", plot.legend= FALSE, gaussian =FALSE, main = "", cex = 0.8)
qqghyp(hypfitABSA, add = TRUE, ghyp.pch = 2 , ghyp.col = "blue", gaussian = FALSE,  cex = 0.8)
qqghyp(nigfitABSA, add = TRUE, ghyp.pch = 2 , ghyp.col = "blue", gaussian = FALSE, cex = 0.8)
legend( "topleft", legend = c("GHD", "HYP", "NIG"), col = col.def[-c (1, 5)] , pch= 1:3, cex = 0.5)

AIC_ABSA = stepAIC.ghyp(ABSA_returns, dist= c("ghyp", "hyp", "NIG"), symmetric= FALSE,control=list( maxit = 1000))
LRghdnig = lik.ratio.test(ghdfitABSA, nigfitABSA)
LRghdhyp = lik.ratio.test(ghdfitABSA, hypfitABSA)

AIC_ABSA

# Value at Risk for ABSA
p = seq(0.001, 0.05, 0.001)

ghd.VaR_ABSA = abs(qghyp(p, ghdfitABSA))
hyp.VaR_ABSA = abs(qghyp(p, hypfitABSA))
nig.VaR_ABSA = abs(qghyp(p, nigfitABSA))
nor.VaR_ABSA = abs(qnorm(p, mean = mean( ABSA_returns), sd = sd(ABSA_returns)))
emp.VaR_ABSA = abs(quantile(x = ABSA_returns, probs = p))

plot( emp.VaR_ABSA, type = "l", xlab = " ", ylab = "VaR", axes = FALSE, ylim=range(c(hyp.VaR_ABSA,nig.VaR_ABSA, ghd.VaR_ABSA, nor.VaR_ABSA, emp.VaR_ABSA)))
box ( )
axis(1, at = seq(along = p), labels = names(emp.VaR_ABSA), tick = FALSE)
axis(2, at = pretty(range(emp.VaR_ABSA, ghd.VaR_ABSA, hyp.VaR_ABSA, nig.VaR_ABSA, nor.VaR_ABSA)))
lines(seq(along = p), ghd.VaR_ABSA, col = "red")
lines(seq(along = p), hyp.VaR_ABSA, col = "blue")
lines(seq(along = p) , nig.VaR_ABSA, col = "green")
lines(seq(along = p) , nor.VaR_ABSA, col = "orange")
legend("top", legend = c("Empirical", "GHD", "HYP", "NIG", "Normal"),col = col.def, lty = 1, cex = 0.5)

# Expected Shortfall for ABSA
ghd.ES_ABSA = abs(ESghyp(p, ghdfitABSA))
hyp.ES_ABSA = abs(ESghyp(p,ghdfitABSA))
nig.ES_ABSA = abs(ESghyp(p, nigfitABSA))
nor.ES_ABSA = abs(mean(ABSA_returns)-sd(ABSA_returns)*dnorm(qnorm(1-p))/p)
obs.p_ABSA = ceiling(p*length(ABSA_returns))
emp.ES_ABSA = sapply(obs.p_ABSA, function(x)abs(mean( sort(c(ABSA_returns))[1:x])))

plot(emp.ES_SASOL, type="l", xlab="Percentile", ylab="ES", axes=FALSE, ylim=range(c(hyp.ES_ABSA, nig.ES_ABSA, ghd.ES_ABSA, nor.ES_ABSA, emp.ES_ABSA)))
box()
axis(1, at=seq(1, length(p), by=4), labels=paste0(round(p[seq(1, length(p), by=4)]*100, 0), "%"), tick=FALSE)
axis(2, at=pretty(range(emp.ES_ABSA, ghd.ES_ABSA, hyp.ES_ABSA, nig.ES_ABSA, nor.ES_ABSA)))
lines(1:length(p), ghd.ES_ABSA, col="red")
lines(1:length(p), hyp.ES_ABSA, col="blue")
lines(1:length(p), nig.ES_ABSA, col="green")
lines(1:length(p), nor.ES_ABSA, col="orange")
legend("topright", legend=c("Empirical", "GHD", "HYP", "NIG", "Normal"), col=col.def, lty=1, cex=0.6)





#---------------------------------------------------7. FITTING THE GEV & THE GPD



# For SASOL

SASOL_losses= -1*SASOL_returns # Estimate Losses

par(mar=c(2,2,2,1)+2)
mrlPlot(SASOL_losses, umin = -7 , umax = 7)


sasolGEV=gev(SASOL_losses, 30)
plot(sasolGEV$data, type = "h", col = "blue", xlab = " ", ylab = "Block Maxima", main = "Maximum per block of 30 for SASOL")
sasolGEV2 = gev.fit(sasolGEV$data) # Fitting the maximum losses with the function gev.fit


loc_SASOL <- sasolGEV2$mle[1]   # Location parameter
scale_SASOL <- sasolGEV2$mle[2] # Scale parameter
shape_SASOL <- sasolGEV2$mle[3] # Shape parameter

confidence_level <- 0.95

VaR_SASOL <- qgev(1 - confidence_level, loc_SASOL, scale_SASOL, shape_SASOL) # Calculate the Value at Risk (VaR)
ES_SASOL <- (scale_SASOL / (1 + shape_SASOL)) * (1 - (1 - confidence_level) ^ (-shape_SASOL)) + loc_SASOL

cat("Value at Risk (VaR) at", confidence_level * 100, "% confidence level:", VaR_SASOL, "\n")
cat("Expected Shortfall (ES) at", confidence_level * 100, "% confidence level:", ES_SASOL, "\n")


par("mar") 
par(mar=c(5,5,5,5))
gev.diag(sasolGEV2) # diagnostic check

# GPD
par(mar=c(1,1,1,1))
mrlPlot(SASOL_losses, umin = -7 , umax = 7) #MRL-plot: it gives the mean residual life plot.
SASOL_fit = gpdFit(as.numeric(SASOL_losses), u = 5) 

graphics.off() 
par("mar") 
par(mar=c(1,1,1,1)+2)

# Diagonstic plot
par(mar=c(1,1,1,1)+2)
par(mfrow = c(2, 2))
plot(SASOL_fit, which = 1)
plot(SASOL_fit, which = 2)
plot(SASOL_fit, which = 3)
plot(SASOL_fit, which = 4)

gpdRiskMeasures(SASOL_fit, prob = c(0.95, 0.99, 0.995)) #Risk Measure


# For ABSA

# Estimate Losses

ABSA_losses<- -1*ABSA_returns # Estimate Losses

par(mar=c(2,2,2,2)+2)
mrlPlot(ABSA_losses, umin = -7 , umax = 7)


absaGEV=gev(ABSA_losses, 30)
plot(absaGEV$data, type = "h", col = "blue", xlab = " ", ylab = "Block Maxima", main = "Maximum per block of 30 for ABSA")
absaGEV2 = gev.fit(absaGEV$data) # Fitting the maximum losses with the function gev.fit


loc_ABSA <- absaGEV2$mle[1]   # Location parameter
scale_ABSA <- absaGEV2$mle[2] # Scale parameter
shape_ABSA <- absaGEV2$mle[3] # Shape parameter

confidence_level <- 0.95

VaR_ABSA <- qgev(1 - confidence_level, loc_ABSA, scale_ABSA, shape_ABSA) # Calculate the Value at Risk (VaR)
ES_ABSA <- (scale_ABSA / (1 + shape_ABSA)) * (1 - (1 - confidence_level) ^ (-shape_ABSA)) + loc_ABSA

cat("Value at Risk (VaR) at", confidence_level * 100, "% confidence level:", VaR_ABSA, "\n")
cat("Expected Shortfall (ES) at", confidence_level * 100, "% confidence level:", ES_ABSA, "\n")


par("mar") 
par(mar=c(5,5,5,5))
gev.diag(absaGEV2) # diagnostic check

# GPD 
par(mar=c(1,1,1,1))
mrlPlot(ABSA_losses, umin = -7 , umax = 7) #MRL-plot: it gives the mean residual life plot.
ABSA_fit = gpdFit(as.numeric(ABSA_losses), u = 5) 

par("mar") 
par(mar=c(1,1,3,1))

# Diagonstic plot
par(mfrow = c(2, 2))
plot(ABSA_fit, which = 1)
plot(ABSA_fit, which = 2)
plot(ABSA_fit, which = 3)
plot(ABSA_fit, which = 4)

gpdRiskMeasures(ABSA_fit, prob = c(0.95, 0.99, 0.995)) #Risk Measure




#--------------------------8 FITTING A GARCH-type MODEL FOR SASOL & ABSA RETURNS 




# For SASOL
SASOL_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder =c(1,1)), mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "sstd")
SASOL_fit = ugarchfit(spec = SASOL_spec, data=SASOL_returns, solver.control = list(trace = 0))
par(mar=c(1.2, 2, 3, 1))
plot(SASOL_fit,which="all")
show(SASOL_fit)

# For ABSA
ABSA_spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder =c(1,1)), mean.model = list(armaOrder = c(1,1), include.mean = TRUE), distribution.model = "sstd")
ABSA_fit = ugarchfit(spec = ABSA_spec, data=ABSA_returns, solver.control = list(trace = 0))
par(mar=c(1.2, 2, 3, 1))
plot(ABSA_fit,which="all")
show(ABSA_fit)


#----------------------------------------------------------------------9. COPULA


u1 = pobs(SASOL_returns)  
u2 = pobs(ABSA_returns)  
u = cbind(u1, u2)  

# Elliptical

# Normal Copula
norm_cop = normalCopula(dim = 2)
norm_fit = fitCopula(norm_cop, u, method = "ml")

# Student-t Copula
t_cop = tCopula(dim = 2)
t_fit = fitCopula(t_cop, u, method = "ml")


#Archimedean

# Clayton Copula
clayton_cop = claytonCopula(dim = 2)
clayton_fit = fitCopula(clayton_cop, u, method = "ml")

# Gumbel Copula
gumbel_cop = gumbelCopula(dim = 2)
gumbel_fit = fitCopula(gumbel_cop, u, method = "ml")


# Print Results
summary(norm_fit)
summary(t_fit)
summary(clayton_fit)
summary(gumbel_fit)

# Parameters

cat("Normal Copula Parameter (rho):", coef(norm_fit), "\n")
cat("Student-t Copula Parameters (rho, df):", coef(t_fit), "\n")
cat("Clayton Copula Parameter (theta):", coef(clayton_fit), "\n")
cat("Gumbel Copula Parameter (theta):", coef(gumbel_fit), "\n")

# Kendall's tau

tau_norm = tau(normalCopula(coef(norm_fit), dim=2))
tau_t = tau(tCopula(coef(t_fit)[1], dim=2))
tau_clayton = tau(claytonCopula(coef(clayton_fit), dim=2))
tau_gumbel = tau(gumbelCopula(coef(gumbel_fit), dim=2))

cat("Kendall's tau - Normal:", tau_norm, "\n")
cat("Kendall's tau - Student-t:", tau_t, "\n")
cat("Kendall's tau - Clayton:", tau_clayton, "\n")
cat("Kendall's tau - Gumbel:", tau_gumbel, "\n")


# Tail dependence coefficients

lambda_t = tailIndex(tCopula(coef(t_fit)[1], df = coef(t_fit)[2], dim=2))
lambda_clayton = lambda(claytonCopula(coef(clayton_fit), dim=2))
lambda_gumbel = lambda(gumbelCopula(coef(gumbel_fit), dim=2))

cat("Normal Copula: No tail dependence (both = 0)\n")
cat("Student-t Copula Lambda lower =", lambda_t["lower"], ", upper =", lambda_t["upper"], "\n")
cat("Clayton Copula Lambda lower =", lambda_clayton["lower"], ", upper =", lambda_clayton["upper"], "\n")
cat("Gumbel Copula Lambda lower =", lambda_gumbel["lower"], ", upper =", lambda_gumbel["upper"], "\n")
