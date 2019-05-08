load("~/UStemperature/results/X_new.RData")
load("~/UStemperature/results/Y_new.RData")
load("~/UStemperature/results/Z_new.RData")

set.seed(123)
index = sample(nrow(Y_new),nrow(Y_new)*0.8)
day = day1
train = day[index,]                     
test = day[-index,]
train_X = X_new[index,]
test_X = X_new[-index,]
train_y = as.matrix(Y_new)[index]
test_y = as.matrix(Y_new)[-index]
train_Z = Z_new[index]
test_Z = Z_new[-index]




##---------------------------------------##
##        1. Using exp model             ## 
##---------------------------------------##

library(fields)
par_est = spatialProcess(train_X,train_y,Z=train_Z,
                         cov.args = list(Covariance = "Exponential"))
sigma = as.numeric(par_est$sigma.MLE)     # nugget variance (sigma^2)
rho = as.numeric(par_est$rho.MLE)         # process variance (rho)
theta = as.numeric(par_est$theta.MLE)     # range parameter (theta)
plot(par_est)  
# plot 1 data vs. predicted values
# plot 2 residuals vs. predicted

#Prediction (kriging) by using expmodel
library(gstat)
library(sp)
trainData = SpatialPointsDataFrame(train_X,data = cbind.data.frame(train_X,train_y),coords.nrs = numeric(0),match.ID = T)
pre_loc = SpatialPointsDataFrame(test_X,data = test_X,coords.nrs = numeric(0),match.ID = T)
g = gstat(formula = trainData$train_y~1, 
          data = trainData, model = vgm(rho,"Exp",theta,sigma^2))
pre = predict(g, pre_loc)
test_y_hat2 = pre$var1.pred
MSE2 = mean((test_y_hat2-test_y)^2)    #9.460397


##------------------------------------------##
## 2. Using Matern model in R base function ## 
##------------------------------------------##

#Prediction (kriging) by using Matern model
## parameter estimation
#(fields use great circle distances, approxiamte the earth by a ball)
library(fields)
par_est = spatialProcess(train_X,train_y,Z=train_Z,
                         mKrig.args = list(m = 3),
                         Distance = "rdist.earth",
                         cov.args = list(Covariance = "Matern",smoothness = 1))
# where m=3 means quadratic form
print(par_est)
sigma.square = as.numeric(par_est$sigma.MLE)^2     # nugget variance (sigma^2) 
rho = as.numeric(par_est$rho.MLE)                  # process variance (rho) 
theta = as.numeric(par_est$theta.MLE)              # range parameter (theta) (miles)
save(par_est,file="~/UStemperature/results/par_est.RData")

par_est$d
# Coefficients of the polynomial fixed part and if present the covariates (Z).

par_est$c
# Coefficients of the nonparametric part.
# has length nrow(train_X)

## The maximum distance among all pairs 
out=rdist.earth(cbind(train_X$lon,train_X$lat),miles = T)
range(out)[2]   # 3167.165(miles)

plot(par_est)  
# plot 1 data vs. predicted values
# plot 2 residuals vs. predicted

par_est$ind.drift
# TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE
# ind.drift can distinguish between polynomial part 
# and the extra covariates coefficients associated with Z.

par_est$eff.df
# [1] 609.1969

par_est$lnProfileLike
# [1] -4691.364

par_est$GCV
# [1] 6.795485
# Estimated value of the GCV function.


## RMSE
pre = predict(par_est, xnew = test_X, Z = test_Z)
RMSE = mean((pre-test_y)^2)/median(test_Z)  # 0.03354896

#--------------------------------------------------------#
par_est2 = spatialProcess(train_X,train_y,Z=train_Z,
                          mKrig.args = list(m = 2),
                          Distance = "rdist.earth",
                          cov.args = list(Covariance = "Matern",smoothness = 1))
save(par_est2,file="~/UStemperature/results/par_est2.RData")
par_est2$eff.df
# [1] 571.8559

par_est2$lnProfileLike
# [1] -4702.465


par_est2$GCV
# [1] 6.840378


par_est2$d
# 105.312 -0.1548362 -2.11127 -0.003804903

pre2 = predict(par_est2, xnew = test_X, Z = test_Z)
RMSE2 = mean((pre2-test_y)^2)/median(test_Z) # 0.0332631







