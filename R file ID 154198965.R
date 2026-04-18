
library(forecast)

y <- as.numeric(as.character(Case_study$V7[-1]))  # skip the first row
print(y)
length(y)      
train <- y[1:100]
test  <- y[101:200]

print(train)
print( test)

### Step 1: Preliminary analysis of orders

# 1)
ts_train <- ts(train)

plot(ts_train,  main="Time Series Plot of Yt (Training Data)", ylab="Yt", xlab="Time", col="blue",lwd=2)
 

# 2 a)

# Plot the autocorrelation function

Acf(ts_train, main="ACF of Yt (train)", lag.max =40)    

# 1st difference

d1 <- diff(train)

Acf(d1, main="ACF of First Difference", lag.max =40)


# 2nd difference

d2 <- diff(train, differences = 2)

Acf(d2, main="ACF of Second Difference", lag.max =40 )
  


# 2 b)
library(tseries)

adf_level <- adf.test(ts_train)
adf_level

adf_d1 <- adf.test(d1)
adf_d1

adf_d2 <- adf.test(d2)
adf_d2


# 3)

d2 <- diff(train, differences = 2)

par(mfrow=c(1,2))   # ACF and PACF side-by-side
Acf(d2, main="ACF of Δ²Yt (training)")
Pacf(d2, main="PACF of Δ²Yt (training)")
par(mfrow=c(1,1))


### Step 2: Estimation and selection of ARIMA Models

# 1)

library(forecast)

m11 <- Arima(train, order=c(1,2,1), include.constant=FALSE)
m12 <- Arima(train, order=c(1,2,2), include.constant=FALSE)
m13 <- Arima(train, order=c(1,2,3), include.constant=FALSE)
m14 <- Arima(train, order=c(1,2,4), include.constant=FALSE)

m21 <- Arima(train, order=c(2,2,1), include.constant=FALSE)
m22 <- Arima(train, order=c(2,2,2), include.constant=FALSE)
m23 <- Arima(train, order=c(2,2,3), include.constant=FALSE)
m24 <- Arima(train, order=c(2,2,4), include.constant=FALSE)

m31 <- Arima(train, order=c(3,2,1), include.constant=FALSE)
m32 <- Arima(train, order=c(3,2,2), include.constant=FALSE)
m33 <- Arima(train, order=c(3,2,3), include.constant=FALSE)
m34 <- Arima(train, order=c(3,2,4), include.constant=FALSE)

m41 <- Arima(train, order=c(4,2,1), include.constant=FALSE)
m42 <- Arima(train, order=c(4,2,2), include.constant=FALSE)
m43 <- Arima(train, order=c(4,2,3), include.constant=FALSE)
m44 <- Arima(train, order=c(4,2,4), include.constant=FALSE)

models <- list(m11,m12,m13,m14,m21,m22,m23,m24,m31,m32,m33,m34,m41,m42,m43,m44)
names(models) <- c("1,2,1","1,2,2","1,2,3","1,2,4",
                   "2,2,1","2,2,2","2,2,3","2,2,4",
                   "3,2,1","3,2,2","3,2,3","3,2,4",
                   "4,2,1","4,2,2","4,2,3","4,2,4")

results <- data.frame(
  Model = names(models),
  AIC = sapply(models, AIC),
  BIC = sapply(models, BIC)
)

results[order(results$AIC), ]
results[order(results$BIC), ]


# 2 )


summary(Arima(train, order=c(2,2,1)))
summary(Arima(train, order=c(3,2,1)))
summary(Arima(train, order=c(2,2,2)))



### Step 3: Diagnostic tests with in-sample data

# 1) 

# fitting 3 models

mod1 <- arima(train, order=c(2,2,1))
res1 <- residuals(mod1)
mod1

mod2 <- arima(train, order=c(3,2,1))
res2 <- residuals(mod2)
mod2

mod3 <- arima(train, order=c(2,2,2))
res3 <- residuals(mod3)
mod3

# Jung-Box test for the first 10 lags
 

Box.test(res1, lag=10, type="Ljung-Box", fitdf=3)  # ARIMA(2,2,1)

Box.test(res2, lag=10, type="Ljung-Box", fitdf=4)  # ARIMA(3,2,1)

Box.test(res3, lag=10, type="Ljung-Box", fitdf=4)  # ARIMA(2,2,2)


# ACF and PACF of model

par(mfrow=c(1,2))
Acf(res1, main="ACF of Residuals ARIMA(2,2,1)")
Pacf(res1, main="PACF of Residuals ARIMA(2,2,1)")
par(mfrow=c(1,1))

#  ACF and PACF of mode2

par(mfrow=c(1,2))
Acf(res2, main="ACF of Residuals ARIMA(3,2,1)")
Pacf(res2, main="PACF of Residuals ARIMA(3,2,1)")
par(mfrow=c(1,1))

#  ACF and PACF of mode3

par(mfrow=c(1,2))
Acf(res3, main="ACF of Residuals ARIMA(2,2,2)")
Pacf(res3, main="PACF of Residuals ARIMA(2,2,2)")
par(mfrow=c(1,1))


# residual plots 

par(mfrow=c(3,1))

plot(res1, type="p", main="ARIMA(2,2,1) Residuals")
abline(h=0)

plot(res2, type="p", main="ARIMA(3,2,1) Residuals")
abline(h=0)

plot(res3, type="p", main="ARIMA(2,2,2) Residuals")
abline(h=0)

par(mfrow=c(1,1))


# 2)

# Histograms


hist(res1, main="Histogram of Residuals ARIMA(2,2,1)", xlab="Residuals")
hist(res2, main="Histogram of Residuals ARIMA(3,2,1)", xlab="Residuals")
hist(res3, main="Histogram of Residuals ARIMA(2,2,2)", xlab="Residuals")


# QQ plot


qqnorm(res1, main="QQ Plot ARIMA(2,2,1)")
qqline(res1)

qqnorm(res2, main="QQ Plot ARIMA(3,2,1)")
qqline(res2)

qqnorm(res3, main="QQ Plot ARIMA(2,2,2)")
qqline(res3)



# Shapiro–Wilk test

shapiro.test(res1)
shapiro.test(res2)
shapiro.test(res3)

# 3- on the other file

# 4)

plot(train, type="l", col="blue",
     main="Original Series and ARIMA(2,2,1) Fitted Values",
     ylab="Y", xlab="Time", lwd=2)

lines(fitted(mod1), col="red", lwd=2)

legend("topleft",
       legend=c("Original series","Fitted values"),
       col=c("blue","red"),
       lty=1)

### Step 4: Forecast with out-of-sample data

# 1)
# forecast 

library(forecast)

fit <- Arima(train, order=c(2,2,1))

fc10  <- forecast(fit, h=10, level=95)  # when h=10
fc25  <- forecast(fit, h=25, level=95)  # when h=25
fc100 <- forecast(fit, h=100, level=95) # when h= 100

plot(fc10, main="10-step Forecast with 95% CI")   # when h=10
lines(test, col="red")

plot(fc25, main="25-step Forecast with 95% CI")   # when h=25
lines(test, col="red")

plot(fc100, main="100-step Forecast with 95% CI") # when h= 100
lines(test, col="red")


# 2) MSE

mse10  <- mean((test[1:10]  - fc10$mean)^2)
mse25  <- mean((test[1:25]  - fc25$mean)^2)
mse100 <- mean((test[1:100] - fc100$mean)^2)  # same as mean((test - fc100$mean)^2)

mse10
mse25
mse100

