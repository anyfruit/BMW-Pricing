# Code for alternative WLS:

bmw <- read.csv('~/BMWpricing_trainingvalidation.csv', as.is = T)
# Approach 1: Variance
# Fitting WLS regression with transformed variables: 
attach(DataSetTraining)
# Best predicator chosen to be log(age) based on regression coefficients;
# to be confirmed with LASSO
# Getting standard deviation:
sdprice.by.log.age <- by(price, log(age), sd)
# Getting mean:"
mprice.by.log.age <- by(price, log(age), mean)
# Plotting SD Price vs. Mean log(age):
plot(mprice.by.log.age, sdprice.by.log.age, xlab="Log(age)", ylab="SD of Price",
     main="SD Price versus log(age) mean; LOESS fit",col="red", lty=0, cex=1, pch=19)
# LOESS:
smooth.sd <- loess.smooth(mprice.by.log.age, sdprice.by.log.age)
# Plot LOESS regressor:
lines(smooth.sd, lty=2)
# Let's use the LOESS fit to create weights for WLS regression:
# Create LOESS object:
smooth.pricesd.orig <- loess(sdprice.by.log.age ~  mprice.by.log.age, control=loess.control(surface="direct"))
# Predict SD
# LOESS interpolant:
sdprice.fits <- predict(smooth.pricesd.orig,data.frame(mprice.by.log.age = log(age)))
my.weights1 <- 1/(sdprice.fits)^2




# Now fit the regression:
m.weightedls1 <- lm(formula_mls_trans, data = DataSetTraining, weights = my.weights1)



summary(m.weightedls1)

# Our own attempt with residuals:
# Fitting WLS regression with transformed variables: 
m.trans1 <- lm(formula_mls_trans, data = DataSetTraining, weights = 1/abs(resid(m.trans)))
summary(m.trans1)
# Investigating Bimodal Std. Res Plot:
group1 <- filter(group_by(DataSetTraining), StanResMLS>=0)
group2 <- filter(group_by(DataSetTraining), StanResMLS<0)
# Fitting WLS model for group1:
m.group1 <- lm(formula_mls_trans, data = group1)
m.group1 <- lm(formula_mls_trans, data = group1, weights = 1/abs(resid(m.group1)))
StanResG1 <- rstandard(m.group1)
plot(group1$price, StanResG1)
# I'm having trouble splitting the two groups. It doesn't look like just group 1 in the plot



# Approach 2: Residuals
# Fitting WLS regression with transformed variables: 
# Residual:
residualweight = residuals(m.trans)
# Absolute value of residual:
absresidualweight = abs(residuals(m.trans))
# Creating weight data frame: 
attach(DataSetTraining)
dataweight = data.frame(log(age), residualweight, absresidualweight)
# Creating Plot for Residuals:
plot1 <- ggplot(dataweight, aes(x=log(age), y=residualweight)) + geom_point(size = 0.1) +  
  ggtitle("Residuals") 
# Creating Plot for Abs. Residuals: 
plot2 <- ggplot(dataweight, aes(x=log(age), y=absresidualweight)) + geom_point(size = 0.1) +  
  ggtitle("Absolute Residuals") 
# Plotting: 
grid.arrange(plot1, plot2, ncol=2)
# Calculate fitted values from a regression of absolute residuals vs log(age)
DataSetTraining$age <- log(DataSetTraining$age)
my.weights2 <- 1/fitted(lm(abs(residuals(m.trans)) ~ age))^2





# Fit a WLS model using weights = 1/(fitted values)2.
m.weightedls2 <- lm(formula_mls_trans, data = DataSetTraining, weights=my.weights2)
# Predictions for second weighted model
age.new<-seq(600,2200,len=length(age))
# Creating data for predicted values:
predWeightedls2 <- predict(m.weightedls2,newdata=data.frame(age = log.age.new))
# Getting predicted values for transformed model (prev.)
predtrans <- fitted(m.trans)
# Plotting: 
print(ggplot(DataSetTraining, aes(x=age, y=price)) + geom_point(size = 0.1) +  
        geom_line(mapping = aes(x = log.age.new, y = predtrans, color = "blue")) +
        geom_line(mapping = aes(x = log.age.new, y = predWeightedls2, color = "green")) +
        scale_color_manual(name = element_blank(), labels = c("Tranformed Model","Weighted Least Squares #2"), 
                           values = c("blue","green")) +
        ggtitle("LS Predictions") )
# Analyzing WLS #2:
summary(m.weightedls2)