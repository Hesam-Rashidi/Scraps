##################################################################################################
### This script forecasts the values of a time series variable by emploing Bayesian Statistics ###
##################################################################################################

library(lubridate)
library(bsts)
library(dplyr)
library(ggplot2)
library(reshape2)


### Load the data - Actual data was swapped for a randomly generated one due to confidentiality 
raw_df <- data.frame(replicate(23,sample(0:1,156,rep=TRUE)))
colnames(raw_df)[1] <- "date"
raw_df[,1] <- as.yearmon(seq(ym("2007-01"), ym("2019-12"), by = 'month'))
colnames <- colnames(raw_df)

h<-24                                #select testing period size
train_df <- raw_df[1:(155-24),]     #149 for 6 -- 143 for 12 -- 137 for 18 -- 131 for 24
test_df <- raw_df[132:(131+h),]
end <- raw_df$date[(155-24)]
start <- raw_df$date[(155-24+1)]


### set Y and X   c(11,12,13,14,-3,-4,-6,-9,-10,-15)
Y <- train_df$X2
y <- exp(raw_df$X2[1:(131+h)])
X <- train_df[,c(-1,-2)] #test against full model
X_obs <- test_df[,c(-1,-2)]


### bsts model (00: univariate, 01: multivariate)
ss <- AddLocalLinearTrend(list(), Y)
ss <- AddSeasonal(ss, Y, nseasons = 12)
bsts.model.01 <- bsts(Y~., state.specification = ss, data=X, niter = 10000,
                      ping=0, expected.r2=0.8, expected.model.size=5, seed=100)


### select model
bsts.model <- bsts.model.01


### Get a suggested number of burn-ins
burn <- SuggestBurn(0.1, bsts.model)


### Helper function to get the positive mean of a vector
PositiveMean <- function(b) {
        b <- b[abs(b) > 0]
        if (length(b) > 0) 
                return(mean(b))
        return(0)
}


### Get the average coefficients when variables were selected (non-zero slopes)
#boxplot(bsts.model$coefficients[-(1:burn),],col=rainbow(ncol(bsts.model$coefficients[-(1:burn),])), outline=F)
coeff <- data.frame(melt(apply(bsts.model$coefficients[-(1:burn),], 2, PositiveMean)))
coeff$Variable <- as.character(row.names(coeff))
ggplot(data=coeff, aes(x=Variable, y=value)) + 
        geom_bar(stat="identity", position="identity") + 
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) + theme_classic() + 
        xlab("") + ylab("") + ggtitle("Average coefficients")


### p75 and p25 coefs
P97.5 <- function(b) {
        b <- b[abs(b) > 0]
        if (length(b) > 0) 
                return(quantile(b, 0.975))
        return(0)
}
p97.5 <- data.frame(melt(apply(bsts.model$coefficients[-(1:burn),], 2, P97.5)))
P2.5 <- function(b) {
        b <- b[abs(b) > 0]
        if (length(b) > 0) 
                return(quantile(b, 0.025))
        return(0)
}
p2.5 <- data.frame(melt(apply(bsts.model$coefficients[-(1:burn),], 2, P2.5)))
quantiles <- cbind.data.frame(p2.5, p97.5)


### Inclusion probabilities -- i.e., how often were the variables selected 
inclusionprobs <- melt(colMeans(bsts.model$coefficients[-(1:burn),] != 0))
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
        geom_bar(stat="identity", position="identity") + 
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
        xlab("") + ylab("") + ggtitle("Inclusion probabilities")


### Coefficient table
coef_table_here <- cbind("mean"=coeff$value,"2.5% quantile"=quantiles[,1], "97.5% quantile"= quantiles[,2],
      "inclusion probability"=inclusionprobs$value)
rownames(coef_table_here) <- row.names(inclusionprobs)


### Get the components
components.withreg <- cbind.data.frame(
        colMeans(bsts.model$state.contributions[-(1:burn),"trend",]),
        colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]),
        colMeans(bsts.model$state.contributions[-(1:burn),"regression",]),
        train_df$date)  
names(components.withreg) <- c("Trend", "Seasonality", "Regression", "Date")
residuals_MBSTS <- cbind.data.frame(train_df$X2 -
        (components.withreg$Trend + components.withreg$Regression +components.withreg$Seasonality)
        , train_df$date)
names(residuals_MBSTS) <- c("Residuals", "Date")
component_table <- cbind(components.withreg,residuals_MBSTS)
components.withreg <- melt(components.withreg, id.vars="Date")
names(components.withreg) <- c("Date", "Component", "Value")
ggplot(data=components.withreg, aes(x=Date, y=Value)) + geom_line() + 
        theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
        facet_grid(Component ~ ., scales="free") + guides(colour=FALSE) + 
        theme(axis.text.x=element_text(angle = -90, hjust = 0))


### predict (select accordingly)
p <- predict.bsts(bsts.model, horizon = h, newdata=X_obs, burn = burn, quantiles = c(.025, .975))
plot(p)


### Actual versus predicted
a<-c(exp(as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])+Y)),  
        exp(as.numeric(p$mean)))

d2 <- data.frame(
        # fitted values and predictions
        a,
        # actual data and dates
        as.numeric(y),
        raw_df$date[1:(131+h)])
names(d2) <- c("Fitted", "Actual", "Date")


### MAPE, RMSE
MAPEout <- filter(d2, Date>end) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
RMSEout <- filter(d2, Date>end) %>% summarise(RMSE=sqrt(mean((Actual-Fitted)^2)))
MAPEin <- filter(d2, Date<start) %>% summarise(MAPE=mean(abs(Actual-Fitted)/Actual))
RMSEin <- filter(d2, Date<start) %>% summarise(RMSE=sqrt(mean((Actual-Fitted)^2)))
criteria <- cbind(RMSEin,MAPEin,RMSEout,MAPEout)
names(criteria) <- c("RMSEin","MAPEin","RMSEout","MAPEout")
criteria


### 95% forecast credible interval
posterior.interval <- cbind.data.frame(
        exp(as.numeric(p$interval[1,])),
        exp(as.numeric(p$interval[2,])), 
        subset(d2, Date>end)$Date)
names(posterior.interval) <- c("LL", "UL", "Date")


### Join intervals to the forecast
d3 <- left_join(d2, posterior.interval, by="Date")


### Plot actual versus predicted with credible intervals for the holdout period
ggplot(data=d3, aes(x=Date)) +
        geom_line(aes(y=Actual, colour = "Actual"), size=1.2) +
        geom_line(aes(y=Fitted, colour = "MBSTS"), size=1.0) +
        theme_bw() + theme(legend.title = element_blank()) +
        ylab("Average Earning (USD per hr)") + xlab("Date") +
        geom_vline(xintercept=as.numeric(start), linetype=2) + 
        geom_ribbon(aes(ymin=LL, ymax=UL), fill="grey", alpha=0.5) +
        ggtitle(paste0("MBSTS -- Holdout MAPE = ", round(100*MAPEout,3), "%", ", RMSE = ", round(RMSEout, 3))) +
        theme(axis.text.x=element_text(angle = -90, hjust = 0)) 

qqdist(residuals(bsts.model))
AcfDist(residuals(bsts.model))
barplot(residuals_MBSTS$Residuals)


