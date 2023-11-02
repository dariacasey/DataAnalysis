install.packages("tidyverse")
install.packages("datarium")
library(datarium)
library(tidyverse)
data("marketing", package = "datarium")
summary(marketing)

#   Question 1


#Finding the most accurate model 

# adjust R^2 = .6099, p val very low
SLR_model_youtube <- lm(sales~youtube, data=marketing)
summary(SLR_model_youtube)

# adjust R^2 = .04733, p val .001148
SLR_model_newspaper <- lm(sales~newspaper, data=marketing)
summary(SLR_model_newspaper)

# adjust R^2 = .3287 , p val .very low
SLR_model_facebook <- lm(sales~facebook, data=marketing)
summary(SLR_model_facebook)


# Formulating youtube model
b_0 = 8.439112
b_1 = 0.047537
# E(sales) = 8.439112 + youtube * 0.047537


# Scatter plot of sales vs youtube 
plot(marketing$youtube, marketing$sales)
abline(SLR_model_youtube,col="orange")
abline(h=mean(marketing$sales))


# Model Evaluation 

# Regression Standard Error 
n <- 200
SSE<-sum(residual_errors^2)
MSE <- SSE/(n-2)
S<-sqrt(SSE/(n-2))
S

# Coefficient of Determination 
y_bar <- mean(marketing$sales)
fitted_values<-fitted.values(SLR_model_youtube)
SSR<-sum((fitted_values-y_bar)^2)
SST<-sum((marketing$sales-y_bar)^2)
R_squrd<-SSR/SST
R_squrd


# Residual Plot Against YouTube 
residuals_model <- residuals(SLR_model_youtube)
plot(marketing$youtube, residuals_model, xlab="youtube", 
     ylab="Model 1 Residuals", col="black", main = "Residual Plot")

# Histogram of Residuals 
hist(residuals_model, breaks=6, main = "Residual Histogram")

# QQ Plot
qqnorm(residuals_model, main="Normal QQ plot", xlab="youtube ads", 
       ylab="Residual Quantiles")
qqline(residuals_model)
par(mfrow=c(1,3))

#   Question 2 


#Fitting the model with all 3 predictors 
MLR_model <- lm(sales~youtube+facebook+newspaper, data = marketing)

# p val of newspaper is high at .86 and its coefficient is almost 0 (-0.001037)
# has 0 stars as well 
# has lower adjusted r squared than model with dropped predictor 
summary(MLR_model)

MLR_model2 <- lm(sales~youtube+facebook, data = marketing)

# Model Evaluation 
summary(MLR_model2)
# s = 2.018; observed sales is roughly $2,018 away from the predicted values 
# adj r squared = 0.8962 

# Residual Analysis
residuals_model<-residuals(MLR_model2)
fitted_model<-predict(MLR_model2)

plot(fitted_model, residuals_model, xlab="Fitted response value", 
     ylab="Model 1 Residuals", col="black", main= "Residual vs Fitted values")
abline(h=0)

hist(residuals_model, breaks=6)
# Adds box to histogram
box()
qqnorm(residuals_model, main="Normal QQ plot", xlab="Theoretical Quantiles", 
       ylab="Residual Quantiles")
qqline(residuals_model)


#   Question 3
newdat <- data.frame(youtube = 50.9, facebook = 42.1, newspaper = 87.6)
CI <- predict(MLR_model2, newdat, se.fit = TRUE, interval = "confidence", 
              level = 0.95)
CI
PI <- predict(MLR_model2, newdat, se.fit = TRUE, interval = "prediction", 
              level = 0.95)
PI