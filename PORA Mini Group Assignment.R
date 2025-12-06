#Installing the required packages/libraries
#-----------------------------------------------------------------------------------------------
library(FRAPO) 
library(fPortfolio)
library(quantmod)
library(timeSeries)
library(fBasics)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(RiskPortfolios)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(quadprog)
library(tidyverse)
library(zoo)
library(copula)
library(xts)
library(dplyr)
library(kableExtra)
library(foreach)

# Data Summary and Descriptive Statistics
#-----------------------------------------------------------------------------------------------
#Downloading stock data 
tickers <- c("SOL.JO", "NPN.JO", "MTN.JO", "SHP.JO", "ANG.JO", 
             "FSR.JO", "AGL.JO", "SBK.JO", "ABG.JO", "GFI.JO")
getSymbols(tickers, src = "yahoo", from = "2015-01-01",to = "2025-04-24")
#----We will be utilising the daily timeframe, hence data frequency is daily----#

# Combining close prices into one xts object
prices <- merge.xts(SOL.JO$SOL.JO.Close, NPN.JO$NPN.JO.Close, MTN.JO$MTN.JO.Close,
                    SHP.JO$SHP.JO.Close, ANG.JO$ANG.JO.Close, FSR.JO$FSR.JO.Close,
                    AGL.JO$AGL.JO.Close, SBK.JO$SBK.JO.Close, ABG.JO$ABG.JO.Close, GFI.JO$GFI.JO.Close)
#------------------------------------------------------------------------------------------------
#Checking for missing 
assetsNames <- c("Sasol","Naspers","MTN","Shoprite", "AngloGold", "FirstRand", "AngloAmerican","StandardBank","Absa", "GoldFields")
prices <- na.omit(prices)
colnames(prices) <- assetsNames
head(prices)

#-------------------------------------------------------------------------------------------------
# Calculate daily returns
returns <- as.timeSeries(na.omit(returns(prices, method = "discrete")))
head(returns)

#----Calculatng and plotting cumulative returns------------
chart.CumReturns(returns, 
                 main = "Cumulative Returns of Stocks",
                 wealth.index = TRUE,  # The wealth index shows growth of $1 investment
                 legend.loc = "topleft",
                 colorset = c("#1F77B4", "#FF7F0E","#2CA02C","#D62728","#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF"),
                 lwd = 2)
#Returns are good but they have to be discussed in risk adjusted terms, hence the calculation of a couple of risk metrics in the following sections
#---------------------------------------------------------------------------------------------------

#Summarising stock returns
summary(returns)
#Box plot of returns
boxplot(coredata(returns), 
        main = "Boxplot of Stock Returns",
        xlab = "Stocks",
        ylab = "Returns",
        col = "lightblue",
        las = 2) # Rotate x-axis labels


#Function to count outliers for each column
count_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  lower <- qnt[1] - 1.5 * iqr
  upper <- qnt[2] + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)}

#Adding IQR,Range and the number of outliers to the summary of returns to get a sense of dispersion
summaries <- apply(coredata(returns), 2, summary)
additional_stats <- rbind(IQR = apply(coredata(returns), 2, IQR, na.rm = TRUE),Range = apply(coredata(returns), 2, function(x) diff(range(x, na.rm = TRUE))),Outliers = apply(coredata(returns), 2, count_outliers))
full_stats <- rbind(summaries, additional_stats)
result_table <- as.data.frame(t(full_stats))
colnames(result_table) <- c("Min", "Q1", "Median", "Mean", "Q3", "Max", "IQR", "Range", "Outliers") #how to discuss and commpare the IQR, Range of the different stocks is left 
print(result_table, digits = 4)
#--------------------------------------------------------------------------------------------------------

-----#Some Performance metrics on an individual stock basis#------
Annualized_Return <- Return.annualized(returns, scale = 252) #<--if this code does not run, I have tried everything and cannot find the error, just run the next code
Annualized_Volatility <-  StdDev.annualized(returns, scale = 252)
Sharpe_Ratio <- SharpeRatio.annualized(returns, scale = 252) 
Max_Drawdown <-  maxDrawdown(returns)
var_95 <- VaR(returns, p = 0.95, method = "historical",  # Uses empirical distribution
              portfolio_method = "single")  # Calculates for each stock separately
es_95 <- ES(returns,
            p = 0.95,
            method = "historical",  # Uses empirical distribution
            portfolio_method = "single")

performance_stats <- rbind(Annualized_Volatility, Sharpe_Ratio, Max_Drawdown, var_95, es_95)
performance_stats

#--------------------------------------------------------------------------------------------------------

#--------Modified Sharpe Ratio------
ModSharpe <- as.numeric(Return.annualized(returns, scale = 252) / abs(es_95))
# Create and format the output table
results <- data.frame(
  Stock = colnames(returns),
  Metric = "Modified Sharpe",  # This will be the row label
  Value = round(ModSharpe, 4)  # Rounded to 3 decimal places
)
#----Visualising the modified Sharpe ratio--------
results
#From the performance stats, rank your assets from the best to the worst in terms of the matrices derived.
#Important is to evaluate sharpe ratio in with respect to ES, a Tail risk measure

#In this context, our benchmark portfolio is the equally weighted portfolio. 
#From the benchmark we can calculate the average cumulative returns over the period of study

#Preliminary Data Analysis
#--------------------------------------------------------------------------------------------------------------
#----Extract each of the stocks from xts object, returns
Sasol_rets <- returns$Sasol
Naspers_rets <- returns$Naspers
Mtn_rets <- returns$MTN
Shoprite_rets <- returns$Shoprite
Anglogold_rets <- returns$AngloGold
FirstRand_rets <- returns$FirstRand
AngloAmer_rets <- returns$AngloAmerican
StandardBnk_rets <- returns$StandardBank
Absa_rets <- returns$Absa
Goldfields_rets <- returns$GoldFields

#First part is to explore the univariate stylised facts for each stock in our portfolio to better understand it

#1. -------Time series data of returns, in particular daily return series, are in general not (iid)---------------
Box.test(Sasol_rets, type= 'Ljung-Box', lag = 20)
Box.test(Naspers_rets, type= 'Ljung-Box', lag = 20)
Box.test(Mtn_rets, type= 'Ljung-Box', lag = 20)
Box.test(Shoprite_rets, type= 'Ljung-Box', lag = 20)
Box.test(Anglogold_rets, type= 'Ljung-Box', lag = 20)
Box.test(FirstRand_rets, type= 'Ljung-Box', lag = 20)
Box.test(AngloAmer_rets, type= 'Ljung-Box', lag = 20)
Box.test(StandardBnk_rets, type= 'Ljung-Box', lag = 20)
Box.test(Absa_rets, type= 'Ljung-Box', lag = 20)
Box.test(Goldfields_rets, type= 'Ljung-Box', lag = 20)

#it can be observed that for some stock in the portfolio some do not have significant p values in the sample 
#----formualate null and alternative hypothesis and comment on the result based on the p-value
#The implication is that portfolio optimisation techniques that assume stock returns being iid will not be adequate for all market regimes

#2.------The volatility of return processes is not constant wrt time------------
library(TTR) 
#TEST 1 --> ROLLING VOLATILITY
rollingvolatility30_Sasol <- runSD(Sasol_rets, n=30)
rollingvolatility30_Naspers <- runSD(Naspers_rets, n=30)
rollingvolatility30_MTN <- runSD(Mtn_rets, n=30)
rollingvolatility30_Shoprite <- runSD(Shoprite_rets, n=30)
rollingvolatility30_Anglogold <- runSD(Anglogold_rets, n=30)
rollingvolatility30_FirstRand <- runSD(FirstRand_rets, n=30)
rollingvolatility30_AngloAmerican <- runSD(AngloAmer_rets, n=30)
rollingvolatility30_StandardBank <- runSD(StandardBnk_rets, n=30)
rollingvolatility30_Absa <- runSD(Absa_rets, n=30)
rollingvolatility30_GoldFields <- runSD(Goldfields_rets, n=30)
par(mfrow = c(2,5))
plot(rollingvolatility30_Sasol, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_Naspers, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_MTN, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_Shoprite, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_Anglogold, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_FirstRand, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_AngloAmerican, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_StandardBank, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_Absa, type="l", col="black", main = "30-Period Rolling Volatility of Returns")
plot(rollingvolatility30_GoldFields, type="l", col="black", main = "30-Period Rolling Volatility of Returns")

#TEST 2 --> ENGEL`S ARCH EFFECTS TEST
#Engel`s Arch Effects test
library(FinTS)
ArchTest(Sasol_rets, lags = 30)
ArchTest(Naspers_rets, lags = 30)
ArchTest(Mtn_rets, lags = 30)
ArchTest(Shoprite_rets, lags = 30)
ArchTest(Anglogold_rets, lags = 30)
ArchTest(FirstRand_rets, lags = 30)
ArchTest(AngloAmer_rets, lags = 30)
ArchTest(StandardBnk_rets, lags = 30)
ArchTest(Absa_rets, lags = 30)
ArchTest(Goldfields_rets, lags = 30)
#formulate null and alternative hypothesis and comment

#Implications -> the ideal optimisation techniques should encompass changing volatility regimes, since volatility isn`t constant`
#Portfolio optimisation techniqeus that fail to capture changing volatility regimes may not be adequate

#3.----The Absolute/Squared returns are highly correlated-------
Box.test((Sasol_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Naspers_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Mtn_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Shoprite_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Anglogold_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((FirstRand_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((AngloAmer_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((StandardBnk_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Absa_rets)^2, type = 'Ljung-Box', lag = 20)
Box.test((Goldfields_rets)^2, type = 'Ljung-Box', lag = 20)


#4. -----The Empirical distribution of returns is skewed to the left,negative returns are more likely to be observed than positive ones----
skewness(Sasol_rets)
skewness(Naspers_rets)
skewness((Mtn_rets))
skewness(Shoprite_rets)
skewness(Anglogold_rets)
skewness(FirstRand_rets)
skewness(AngloAmer_rets)
skewness(StandardBnk_rets)
skewness(Absa_rets)
skewness(Goldfields_rets)

# the returns of some stocks have a positive skew. they should be noted in this sample

#5.---------The distribution of financial market returns is leptokurtic. The occurrence of extreme events is more likely than suggested by the normal distribution------ 
par(mfrow = c(2,5))
qqnormPlot(Sasol_rets)
qqnormPlot(Naspers_rets)
qqnormPlot(Mtn_rets)
qqnormPlot(Shoprite_rets)
qqnormPlot(Anglogold_rets)
qqnormPlot(FirstRand_rets)
qqnormPlot(AngloAmer_rets)
qqnormPlot(StandardBnk_rets)
qqnormPlot(Absa_rets)
qqnormPlot(Goldfields_rets)

#Risk models that are based on the normal distribution will fall short in predicting the frequency of extreme events (losses)
#This will mean that optimisations that assume normal distribution will not be adequate as they will not capture adequately the occurance of extreme events
#this also applies to the skew, if it not 0 the optimisation assuming normality of returns will be inadequate

#6.----Extreme returns are observed closely in time, this imples volatility clustering-----
#to visualise, create a new set of squared returns, define a threshold and plot values above that threshold

SaS_abs <- abs(Sasol_rets)
Nasp_abs <- abs(Naspers_rets)
Mtn_abs <- abs(Mtn_rets)
Shopr_abs <- abs(Shoprite_rets)
Anggld_abs <- abs(Anglogold_rets)
FstRnd_abs <- abs(FirstRand_rets)
AngloAm_abs <- abs(AngloAmer_rets)
StBnk_abs <- abs(StandardBnk_rets)
ABSA_abs <- abs(Absa_rets)
GFields_abs <- abs(Goldfields_rets)

#Extracting the values above a given threshold
Sasol_Vol_clust <- ifelse(SaS_abs > 0.07, SaS_abs, 0)
Nasp_vol_clust <- ifelse(Nasp_abs > 0.07, Nasp_abs,0)
MTN_vol_clust <- ifelse(Mtn_abs > 0.07,Mtn_abs,0)
Shopr_vol_clust <- ifelse(Shopr_abs >0.07,Shopr_abs, 0)
Anggld_vol_clust <- ifelse(Anggld_abs > 0.07, Anggld_abs, 0)
Frst_vol_clust <- ifelse(FstRnd_abs > 0.07,FstRnd_abs, 0)
AnglAm_vol_clust <- ifelse(AngloAm_abs > 0.07,AngloAm_abs, 0)
Stndbnk_vol_clust <- ifelse(StBnk_abs > 0.07,StBnk_abs,0)
ABSA_vol_clust <- ifelse(ABSA_abs >0.07, ABSA_abs,0)
GFields_vol_clust <- ifelse(GFields_abs > 0.07 ,GFields_abs,0)

#Plotting the extracted values
par(mfrow = c(2,5))
plot(Sasol_Vol_clust, type="l", col="black", main = "Volatility Clustering Sasol Returns")
plot(Nasp_vol_clust, type="l", col="black", main = "Volatility Clustering Naspers Returns")
plot(MTN_vol_clust, type="l", col="black", main = "Volatility Clustering MTN Returns")
plot(Shopr_vol_clust, type="l", col="black", main = "Volatility Clustering Shoprite Returns")
plot(Anggld_vol_clust, type="l", col="black", main = "Volatility Clustering AngloGold Returns")
plot(Frst_vol_clust, type="l", col="black", main = "Volatility Clustering FirstRand Returns")
plot(AnglAm_vol_clust, type="l", col="black", main = "Volatility Clustering AngloAmerican Returns")
plot(Stndbnk_vol_clust, type="l", col="black", main = "Volatility Clustering StandardBank Returns")
plot(ABSA_vol_clust, type="l", col="black", main = "Volatility Clustering ABSA Returns")
plot(GFields_vol_clust, type="l", col="black", main = "Volatility Clustering GoldFields Returns")


#------Multivariate data analysis (from a portfolio perspective)------------

#1.Extreme observations in one return series are often accompanied by extremes in the other return series
#---In this exercise we want to measure the extent. We will fit bivariate t-copulas seperately for each pair of stocks
#---The second alternative is to fit the RVine copula.  
library(copula)
library(xts)
library(dplyr)
library(kableExtra)
library(foreach)

stock_names <- colnames(returns)

#Applying bivariate t-copula to the stock returns--> note that this portion of code can take time. give it time to run
tail_results <- foreach(i = 1:(ncol(returns)-1), .combine = rbind) %:%
  foreach(j = (i+1):ncol(returns), .combine = rbind) %do% {
    pair_data <- pobs(returns[,c(i,j)])
    fit <- fitCopula(tCopula(dim=2), pair_data, method="mpl")
    rho <- coef(fit)[1]
    df_pair <- coef(fit)[2]  
    
    data.frame(
      Stock1 = stock_names[i],
      Stock2 = stock_names[j],
      Rho = round(rho, 3),
      DF = round(df_pair, 1),
      LowerTail = round(2 * pt(-sqrt((df_pair+1)*(1-rho)/(1+rho)), df_pair+1), 4),
      UpperTail = round(2 * pt(-sqrt((df_pair+1)*(1-rho)/(1+rho)), df_pair+1), 4)
    )
  }
tail_results

library(VineCopula)
# Convert returns to uniform margins
u <- pobs(returns)
#Fit R-vine copula
rvine <- RVineStructureSelect(u, familyset = 2, progress = TRUE) #fit the t-copula, it has both lower and upper tail dependence
# Print summary
summary(rvine)


#-----------------------------------------------------------------------------------------------------------

#2. Contemporaneous correlations are not constant over time.
#----For our dataset, we want to see the extent of this statement----
library(corrplot)

# Calculate correlations in first and second half of sample
half_point <- floor(nrow(returns)/2)
cor_first <- cor(returns[1:half_point,], use = "pairwise.complete.obs")
cor_second <- cor(returns[(half_point+1):nrow(returns),], use = "pairwise.complete.obs")

# Plot comparison
par(mfrow = c(1,2))
corrplot(cor_first, method = "number", title = "")
corrplot(cor_second, method = "number", title = "")
cor_first
cor_second #for actual values of the cporrelations
#compare the correlation heatmaps from the output from corrplot

#---------------------------------------------------------------------------------------------------------
#3. Analysing and comparing cross correlations of returns, absolute returns and squared returns

library(xts)
library(PerformanceAnalytics)
library(ggplot2)
library(ggcorrplot)

# Function to analyze cross-correlations
analyze_cross_correlations <- function(returns) {
  # 1. Contemporaneous correlations of returns
  cor_returns <- cor(returns, use = "pairwise.complete.obs")
  
  # 2. Cross-correlations of absolute returns
  abs_returns <- abs(returns)
  cor_abs <- cor(abs_returns, use = "pairwise.complete.obs")
  
  # 3. Cross-correlations of squared returns
  sq_returns <- returns^2
  cor_sq <- cor(sq_returns, use = "pairwise.complete.obs")
  
  # Visualization
  p1 <- ggcorrplot(cor_returns, title = "Contemporaneous Return Correlations") +
    theme(plot.title = element_text(hjust = 0.5))
  
  p2 <- ggcorrplot(cor_abs, title = "Absolute Return Correlations") +
    theme(plot.title = element_text(hjust = 0.5))
  
  p3 <- ggcorrplot(cor_sq, title = "Squared Return Correlations") +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Statistical comparison
  stats <- data.frame(
    Metric = c("Returns", "Absolute Returns", "Squared Returns"),
    Mean_Correlation = c(mean(abs(cor_returns[lower.tri(cor_returns)])),
                         mean(abs(cor_abs[lower.tri(cor_abs)])),
                         mean(abs(cor_sq[lower.tri(cor_sq)]))),
    SD_Correlation = c(sd(cor_returns[lower.tri(cor_returns)]),
                       sd(cor_abs[lower.tri(cor_abs)]),
                       sd(cor_sq[lower.tri(cor_sq)]))
  )
  # comment on the stats
  return(list(
    return_cor = cor_returns,
    abs_cor = cor_abs,
    sq_cor = cor_sq,
    plots = list(p1, p2, p3),
    statistics = stats
  ))
}

# Run the analysis
results <- analyze_cross_correlations(returns)

# Display results
gridExtra::grid.arrange(grobs = results$plots[1:3], ncol = 2)
print(results$statistics)

#comment on the interesting results
#-----------------------------------------------------------------------------------------------------------------
#----Principal Component Analysis----
#---the end goal here is to find out which stocks have got similar drivers from our portfolio (apply K-means)-----
library(xts)
library(tidyverse)
library(factoextra)

# Perform PCA on the correlation matrix
pca_result <- prcomp(returns, scale = TRUE)

# Summary of PCA results
summary(pca_result)

#Variance Explained using Scree plot
fviz_eig(pca_result, addlabels = TRUE)

# Contributions to the first two principal components
fviz_contrib(pca_result, choice = "var", axes = 1:2)

#--------Identifying stocks with similar drivers------
# Extract the PCA loadings (rotation matrix)
loadings <- pca_result$rotation[,1:2] # First two components

# Calculate Euclidean distance between stocks based on loadings
dist_matrix <- dist(loadings, method = "euclidean")

# Cluster stocks based on their loading similarities
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram----output for stocks with similar drivers
plot(hclust_result, main = "Cluster Dendrogram of Stocks", 
     xlab = "Stocks", sub = "")
rect.hclust(hclust_result, k = 5) # Adjust k as needed

#From the output, note thay Sasol and Naspers are both global companies (Naspers has exposure to hongkong, Sasol to the NYSE)
#Sasol has a dual listing on the JSE and NYSE and has global presence
#Naspers has major shareholding in Tencent, listed in honk Kong. They are similar in those aspects
#Shoprite and MTN`s exposure are also also similar, though not identical. It could explain why they are clustered together in the dendogram
#Goldfields and Angogold are both exposed to the gold markets
#AngloAmerican is striclty mining
#The other three stocks are banks
#Hence from this we can summarise the exposure we have from our ten stocks.


#----Empirical Analysis and Discussion-----------------------------------------------------------------------------------
#---Setting up portfolio optimisations--------
install.packages("CVXR")
library(CVXR)

#1. We would like to analyse the reason behind the portfolio weights in light of the methodology first-------------------

# Create portfolio specification & Setting up the global minimum variance portfolio
pspec <- portfolioSpec()
gmv <- pspec
setType(gmv) <- "Return"
gmvpf <- minvariancePortfolio(data = returns, spec = gmv, constraints = "LongOnly")
print(gmvpf)

# Extract weights
weights <- as.data.frame(getWeights(gmvpf))
weights
weights_gmv <- getWeights(gmvpf)
weights$Asset <- rownames(weights)
colnames(weights) <- c("Weight", "Asset")
#---Plot weights 
ggplot(weights, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight * 100)), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Global Minimum Variance Portfolio Weights",
    x = "Asset",
    y = "Weight (%)"
  )
#Backtest on the entire historical dataset for global minimum variance portfolio
portfolio_returns <- Return.portfolio(
  R = returns,
  weights = weights_gmv,
  rebalance_on = "months"
)
#In Sample Backtest
backtest_results <- portfolio_returns
table.AnnualizedReturns(backtest_results)
maxdd <- maxDrawdown(portfolio_returns)
cvar <- ES(portfolio_returns, p = 0.95, method = "historical")
var <- VaR(portfolio_returns, p = 0.95, method = "historical")
maxdd
cvar
var
charts.PerformanceSummary(backtest_results, main = "GMV perfomance in sample performance")
#-----------------------------------------------------------------------------------------------------------------------
#setting up Mean-CVaR optimisation
portf <- portfolio.spec(assets = colnames(returns))
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type="risk", name="CVaR", arguments=list(p=0.95), method = "historical", enabled=TRUE)
#method has been set to historical because we would want to use the empiracal density of the data as opposed to using Gaussian , where we assume that the data follows a normal distribution
portf <- add.objective(portf, type="return", name="mean") #in this line you can also specify the target return required.

#Mean-CVaR optimisation
mean_cvar <- optimize.portfolio(returns, portf, optimize_method="ROI", maxSR = TRUE) #maximising the sharpe ratio
print(mean_cvar)
# Extract weights
weights1 <- extractWeights(mean_cvar)
weights_df <- data.frame(
  Asset = names(weights1),
  Weight = as.numeric(weights1) * 100  # Convert to percentages
)

# Print weights

ggplot(weights_df, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight )), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Mean CVaR Portfolio Weights",
    x = "Asset",
    y = "Weight (%)"
  )
#In Sample Backtest
# Calculate portfolio returns
portfolio_returns1 <- Return.portfolio(
  R = returns,
  weights = weights1,
  rebalance_on = "months"
)
backtest_results1 <- portfolio_returns1
table.AnnualizedReturns(backtest_results1)
maxdd1 <- maxDrawdown(portfolio_returns1)
cvar1 <- ES(portfolio_returns1, p = 0.95, method = "historical")
var1 <- VaR(portfolio_returns1, p = 0.95, method = "historical")
maxdd1
cvar1
var1
charts.PerformanceSummary(backtest_results1, main = "Mean-CVaR in sample performance")

#------------------------------------------------------------------------------------------------------------------------
#Setting up Minimum CVaR optimisation (Contrasing it from the Mean_CVaR optimisation)
cvar <- pspec
setType(cvar) <- "CVaR"
setAlpha(cvar) <- 0.1
setSolver(cvar) <- "solveRglpk.CVAR"
cvarpf <- minvariancePortfolio(data = returns, spec = cvar, constraints = "LongOnly")
cvarpf

#-------------------------------------------------------------------------------------------------------------------------
#Equal risk contribution weights
#Calculate portfolio weights
V <- cov(returns)
raw_weights <- Weights(PERC(V))# Equal risk concentration portfolio
#Normalise weights
ERCW <- (raw_weights / sum(raw_weights))

weights_try <-data.frame(
  Asset = names(ERCW),
  Weight = as.numeric(ERCW)   # Convert to percentages
)

ggplot(weights_try, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight*100 )), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Equal Risk Contribution Portfolio Weights",
    x = "Asset",
    y = "Weight (%)"
  )
portfolio_returns2 <- Return.portfolio(
  R = returns,
  weights = ERCW,
  rebalance_on = "months"
)

backtest_results2 <- portfolio_returns2
table.AnnualizedReturns(backtest_results2)
maxdd2 <- maxDrawdown(portfolio_returns2)
cvar2 <- ES(portfolio_returns2, p = 0.95, method = "historical")
var2 <- VaR(portfolio_returns2, p = 0.95, method = "historical")
maxdd2
cvar2
var2
charts.PerformanceSummary(backtest_results2, main ="ERC in sample performance")

#-------------------------------------------------------------------------------------------------------
#Setting up optimisation for PCA. This is the ML technique we have chosen
#We will also get portfolio weights, and do an in sample backtest 

# Perform PCA on correlation matrix 
pca <- prcomp(returns, scale = TRUE)

# Extract eigenvalues (squared standard deviations)

# Summary of PCA
summary_pca <- summary(pca)
print(summary_pca)


#We will take components explaining 80-90% of variance
cum_var <- summary_pca$importance[3, ]  # Cumulative variance
num_components <- which(cum_var >= 0.85)[1]  # First component reaching 85%
cat("Number of components to use:", num_components, "\n")

# Get the loadings (rotation matrix)
loadings <- pca$rotation[, 1:num_components]
loadings


# Here we use equal contribution from each selected component...
weights_pca <- rowSums(abs(loadings)) / sum(abs(loadings))
# Normalize to sum to 1
weights_pca <- weights_pca / sum(weights_pca)  

# Print weights
print(weights_pca)

weights_pca_df<- data.frame(
  Asset = names(weights_pca),
  Weight = as.numeric(weights_pca) * 100  # Convert to percentages
)

ggplot(weights_pca_df, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight )), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Machine learning Weights (Principal Component Analysis)",
    x = "Asset",
    y = "Weight (%)"
  )

#In sample backtest for Machine Learning (PCA)

portfolio_returns3 <- Return.portfolio(
  R = returns,
  weights = weights_pca,
  rebalance_on = "months"
)

backtest_results_ML <- portfolio_returns3
table.AnnualizedReturns(backtest_results_ML)#@15.18%, R1000 would be R4109.32 today
maxdd3 <- maxDrawdown(portfolio_returns3)
cvar3 <- ES(portfolio_returns3, p = 0.95, method = "historical")
var3 <- VaR(portfolio_returns3, p = 0.95, method = "historical")
maxdd3
cvar3
var3
charts.PerformanceSummary(backtest_results_ML, main = "Machine Learning in Sample Performace (PCA)")


#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------

#2.------------------------Out of Sample Backtest--------------------------
# Instead of splitting, lets download the train and test sets seperately
tickers <- c("SOL.JO", "NPN.JO", "MTN.JO", "SHP.JO", "ANG.JO", 
             "FSR.JO", "AGL.JO", "SBK.JO", "ABG.JO", "GFI.JO")

# 1. Download and merge TRAINING set (2015-01-01 to 2021-04-24)
Train_set <- lapply(tickers, function(x) {
  getSymbols(x, src = "yahoo", from = "2015-01-01", to = "2021-04-24", auto.assign = FALSE)[,4]})
Train_Close <- do.call(merge, Train_set)
colnames(Train_Close) <- tickers

#-----------------------------------------------------------------------------------------------------
# 2. Download and merge TEST set (2021-01-01 to 2025-04-24)
Test_set <- lapply(tickers, function(x) {
  getSymbols(x, src = "yahoo", from = "2021-01-01", to = "2025-04-24", auto.assign = FALSE)[,4]})

# Merge all stocks into one xts object for testing
Test_Close <- do.call(merge, Test_set)
colnames(Test_Close) <- tickers

head(Test_Close)
head(Train_Close)

#Calculate returns
returns_train <- as.timeSeries(na.omit(returns(Train_Close, method = "discrete")))
returns_test <- as.timeSeries(na.omit(returns(Test_Close, method = "discrete")))
#-------------------------------------------------------------------------------------------------------------
#Optimising GMV on train returns and doing out of sample analysis on test set
# Create portfolio specification & Setting up the global minimum variance portfolio
pspec <- portfolioSpec()
gmv <- pspec
setType(gmv) <- "Return"
gmvpf <- minvariancePortfolio(data = returns_train, spec = gmv, constraints = "LongOnly")
print(gmvpf)

# Extract weights
weights <- as.data.frame(getWeights(gmvpf))
weights_gmv_train <- getWeights(gmvpf)
weights$Asset <- rownames(weights)
colnames(weights) <- c("Weight", "Asset")
#---Plot weights 
ggplot(weights, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight * 100)), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Global Minimum Variance Portfolio Weights-Train Set",
    x = "Asset",
    y = "Weight (%)"
  )
#Backtest on the test set for global minimum variance portfolio
portfolio_returns <- Return.portfolio(
  R = returns_test,
  weights = weights_gmv_train,
  rebalance_on = "months"
)
backtest_results <- portfolio_returns
table.AnnualizedReturns(backtest_results)
maxdd <- maxDrawdown(portfolio_returns)
cvar <- ES(portfolio_returns, p = 0.95, method = "historical")
var <- VaR(portfolio_returns, p = 0.95, method = "historical")
maxdd
cvar
var
charts.PerformanceSummary(backtest_results, main = "GMV out of sample performance")
#----------------------------------------------------------------------------------------------------------------------
#Optimising Mean-CVaR on train returns and doing out of sample analysis on test returns
#setting up Mean-CVaR optimisation
portf <- portfolio.spec(assets = colnames(returns_train))
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type="risk", name="CVaR", arguments=list(p=0.95), method = "historical", enabled=TRUE)
#method has been set to historical because we would want to use the empiracal density of the data as opposed to using Gaussian , where we assume that the data follows a normal distribution
portf <- add.objective(portf, type="return", name="mean") #in this line you can also specify the target return required.

#Mean-CVaR optimisation
mean_cvar <- optimize.portfolio(returns_train, portf, optimize_method="ROI", maxSR = TRUE) #maximising the sharpe ratio
print(mean_cvar)
# Extract weights
weights1 <- extractWeights(mean_cvar)
weights_df <- data.frame(
  Asset = names(weights1),
  Weight = as.numeric(weights1) * 100  # Convert to percentages
)

# Print weights
print(weights_df)
ggplot(weights_df, aes(x = reorder(Asset, -Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", Weight)), vjust = -0.5, size = 3) +
  labs(
    title = "Mean-CVaR Portfolio Weights Train Set(Maximising Sharpe ratio)",
    x = "Asset",
    y = "Weight (%)"
  )
#A backtest on the test set dataset for Mean-CVaR
# Calculate portfolio returns
portfolio_returns1 <- Return.portfolio(
  R = returns_test,
  weights = weights1,
  rebalance_on = "months"
)
backtest_results1 <- portfolio_returns1
table.AnnualizedReturns(backtest_results1)
maxdd1 <- maxDrawdown(portfolio_returns1)
cvar1 <- ES(portfolio_returns1, p = 0.95, method = "historical")
var1 <- VaR(portfolio_returns1, p = 0.95, method = "historical")
maxdd1
cvar1
var1
charts.PerformanceSummary(backtest_results1, main  = "Mean CVaR out of sample performance")
#------------------------------------------------------------------------------------------------------------
#ERC for out of sample testing
#Calculate portfolio weights
V <- cov(returns_train)
raw_weights <- Weights(PERC(V))# Equal risk concentration portfolio
#Normalise weights
ERCW <- (raw_weights / sum(raw_weights))

weights_try <-data.frame(
  Asset = names(ERCW),
  Weight = as.numeric(ERCW)   # Convert to percentages
)


ggplot(weights_try, aes(x = reorder(Asset, Weight), y = Weight, fill = Asset)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Weight*100 )), 
            hjust = ifelse(weights$Weight >= 0, -0.1, 1.1), 
            size = 3.5) +
  coord_flip() +  # Horizontal bars
  labs(
    title = "Equal risk contribution weights- Train Set",
    x = "Asset",
    y = "Weight (%)"
  )
portfolio_returns2 <- Return.portfolio(
  R = returns_test,
  weights = ERCW,
  rebalance_on = "months"
)

backtest_results2 <- portfolio_returns2
table.AnnualizedReturns(backtest_results2)
maxdd2 <- maxDrawdown(portfolio_returns2)
cvar2 <- ES(portfolio_returns2, p = 0.95, method = "historical")
var2 <- VaR(portfolio_returns2, p = 0.95, method = "historical")
maxdd2
cvar2
var2
charts.PerformanceSummary(backtest_results2, main ="Equal risk contribution out of sample performance")

#-------------------------------------------------------------------------------------------------
#Machine learning out of sample performance section utilising PCA
# Perform PCA on correlation matrix 
pca <- prcomp(returns_train, scale = TRUE)

# Summary of PCA
summary_pca <- summary(pca)
print(summary_pca)


#We will take components explaining 80-90% of variance
cum_var <- summary_pca$importance[3, ]  # Cumulative variance
num_components <- which(cum_var >= 0.85)[1]  # First component reaching 85%
cat("Number of components to use:", num_components, "\n")

# Get the loadings (rotation matrix)
loadings <- pca$rotation[, 1:num_components]
loadings


#We use equal contribution from each selected component....(Take note of this)
weights_pca <- rowSums(abs(loadings)) / sum(abs(loadings))
# Normalize to sum to 1
weights_pca <- weights_pca / sum(weights_pca)  

# Print weights
print(weights_pca)

weights_pca_df<- data.frame(
  Asset = names(weights_pca),
  Weight = as.numeric(weights_pca) * 100  # Convert to percentages
)
#Plot the PCA weights
ggplot(weights_pca_df, aes(x = reorder(Asset, -Weight), y = Weight, fill = Asset)) +
  geom_col() +  # Equivalent to geom_bar(stat = "identity")
  geom_text(aes(label = sprintf("%.1f%%", Weight)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Machine Learning Weights (Principal Component Analysis)-Train Set",
    subtitle = "",
    x = NULL,
    y = "Weight (%)"
  ) 


#Out of sample backtest for Machine Learning (PCA)

portfolio_returns3 <- Return.portfolio(
  R = returns_test,
  weights = weights_pca,
  rebalance_on = "months"
)

backtest_results_ML <- portfolio_returns3
table.AnnualizedReturns(backtest_results_ML)
maxdd3 <- maxDrawdown(portfolio_returns3)
cvar3 <- ES(portfolio_returns3, p = 0.95, method = "historical")
var3 <- VaR(portfolio_returns3, p = 0.95, method = "historical")
maxdd3
cvar3
var3
charts.PerformanceSummary(backtest_results_ML, main = "Machine Learning out of Sample Performace (PCA)")

