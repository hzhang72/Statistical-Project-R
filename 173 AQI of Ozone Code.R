set.seed(173)

library(tidyverse)

oz_og <- read.csv("daily_44201_2020.csv")

# Remove duplicated x, y coordinates.
oz_rm <- oz_og[-which(duplicated(oz_og$Longitude)), ]

# Make sure no duplicates x, y coordinates
length(unique(oz_rm$Longitude))
length(unique(oz_rm$Latitude))

# Select x,y coordinates, target variable, and other variables.
oz_sl <- oz_rm[sample(1:nrow(oz_rm), 500),]
length(unique(oz_sl$Site.Num))

oz <- oz_sl %>% 
  select(Longitude, Latitude, AQI, Arithmetic.Mean, X1st.Max.Value) 

attach(oz)

#################################################
#################################################
#################################################

par(mfrow = c(1, 3))

hist(AQI)
boxplot(AQI)
plot(ecdf(AQI))

hist(X1st.Max.Value)
boxplot(X1st.Max.Value)
plot(ecdf(X1st.Max.Value))

hist(Arithmetic.Mean)
boxplot(Arithmetic.Mean)
plot(ecdf(Arithmetic.Mean))

plot(AQI, Arithmetic.Mean, main = "Scatter Plot")
plot(AQI, X1st.Max.Value, main = "Scatter Plot")

par(mfrow = c(1, 1))

#################################################
#################################################
#################################################

#Predict data points on the grid: 

#Create the grid:
x.range <- as.integer(range(oz[,1])) 
y.range <- as.integer(range(oz[,2])) 
grd <- expand.grid(x=seq(from=x.range[1], to=x.range[2], by=0.1), 
                   y=seq(from=y.range[1], to=y.range[2], by=0.1)) 
length(grd)

#################################################
#################################################

library(gstat)
library(sp)

summary(AQI)

# location plot
plot(Longitude, Latitude, xlim = c(-125, -65), ylim = c(23, 50), xlab  ="Longitude",
     ylab = "Latitude", main="Ozone Site Locations in United State")
map("world", "us", add = TRUE)
AQI_colors <- c("lightgreen", "green", "yellow") 
AQI_levels <- cut(AQI, c(min(AQI),30, 50, max(AQI)))
points(Longitude, Latitude, cex = AQI/mean(AQI), pch = 19,
       col = AQI_colors[as.numeric(AQI_levels)])
legend("bottomleft", inset=.01, title="AQI Level", pch=19, col=AQI_colors, cex=0.8,
       c("0-30","31-50","51-100"))

# h-scatterplots
qq <- hscat(AQI ~ Longitude + Latitude, locations = ~ Longitude + Latitude, 
            oz, c(seq(0, 4, 1)))
plot(qq, main = "h-scatterplots")

#################################################
#################################################
#################################################

coordinates(oz) <- ~Longitude+Latitude

coordinates(grd) <- ~ x+y

g <- gstat(id = "AQI", formula = AQI~1, data = oz)
q <- variogram(g)
plot(q, main = "Variogram")

# Remove the trend
g0 <- gstat(id = "AQI", formula = AQI~Longitude+Latitude, data = oz)
q0 <- variogram(g0)
plot(q0, main = "Variogram of detrended data")

v.fit0 <- fit.variogram(q0, vgm(20,"Exp",2,0), fit.method=7)
plot(q0, v.fit0)

v <- variogram(g, alpha = c(0,45,90,135)) 
plot(v, main = "Variogram on the 4 main directions")

# Variogram modeling: 
## the initial values for the partial sill, range, and nugget
v.fit1 <- fit.variogram(q, vgm(20,"Exp",2,0), fit.method=1) 
v.fit2 <- fit.variogram(q, vgm(20,"Exp",2,0), fit.method=2) 
v.fit6 <- fit.variogram(q, vgm(20,"Exp",2,0), fit.method=6) 
v.fit7 <- fit.variogram(q, vgm(20,"Exp",2,0), fit.method=7)

plot(q, v.fit1, main = "Type of weights: n pairs")
plot(q, v.fit2, main = "Type of weights: Cressie’s weights")
plot(q, v.fit6, main = "Type of weights: OLS")
plot(q, v.fit7, main = "Type of weights: default")

#################################################
#################################################
#################################################

idw.out <- idw(AQI~1, oz, grd)

#Collapse the predicted values into a matrix: 
qqq <- matrix(idw.out$var1.pred, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)))
image(seq(from=x.range[1], to=x.range[2], by=0.1), 
      seq(from=y.range[1], to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude",  
      main = "Inverse distance predicted values")

#Add the data points:
points(oz) 

#Add contours:
contour(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
        add=TRUE, col="black", labcex=1)

#################################################
#################################################
#################################################

#Perform ordinary kriging predictions:
pr_ok <- krige(id = "AQI", formula = AQI~1, oz, newdata = grd, model = v.fit7)

#Collapse the vector of the predicted values into a matrix: 
qqq <- matrix(pr_ok$AQI.pred, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Ordinary kriging Predicted values") 

#Add the data points:
points(oz) 

#Add contours:
contour(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
        add=TRUE, col="black", labcex=1)


#Collapse the vector of the variances into a matrix: 
qqq <- matrix(pr_ok$AQI.var, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Ordinary kriging variances") 

#################################################
#################################################
#################################################
#################################################

# Perform co-kriging predictions:
#Begin with the target variable AQI:
g1 <- gstat(id = "AQI", formula = AQI~1, data = oz)

#Append mean:
g1 <- gstat(g1,id="Mean Value", formula = Arithmetic.Mean~1, data = oz)

#Append max:
g1 <- gstat(g1,id="Max Value", formula = X1st.Max.Value~1, data = oz)

plot(variogram(g1))

#Fit a model variogram to all the variograms:
vm <- variogram(g1) 
vm.fit <- fit.lmc(vm, g1, model=v.fit7)

#Plot the fitted variograms to all the sample variograms:
plot(vm,vm.fit)

#Perform co-kriging predictions:
ck <- predict(vm.fit, grd)

#Collapse the vector of the predicted values into a matrix: 
qqq <- matrix(ck$AQI.pred, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1))) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1], to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Co-kriging kriging predicted values") 

#Add the data points:
points(oz) 

#Add contours:
contour(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1], to=y.range[2], by=0.1), qqq, 
        add=TRUE, col="black", labcex=1)

#Collapse the vector of the variances into a matrix: 
qqq <- matrix(ck$AQI.var, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Co-kriging kriging variances") 

#################################################
#################################################
#################################################
#################################################

# Perform cross-validation:
#Ordinary kriging:
cv_pr1 <- krige.cv(formula = AQI~1, oz, model = v.fit1, nfold=nrow(oz)) 

#Compute the prediction sum of squares:
cv_PRESS1 <- sum(cv_pr1$residual^2) / nrow(oz)
cv_PRESS1 # 69.24327

cv_pr2 <- krige.cv(formula = AQI~1, oz, model = v.fit2, nfold=nrow(oz)) 

#Compute the prediction sum of squares:
cv_PRESS2 <- sum(cv_pr2$residual^2) / nrow(oz)
cv_PRESS2 # 62.42498

cv_pr6 <- krige.cv(formula = AQI~1, oz, model = v.fit6, nfold=nrow(oz)) 

#Compute the prediction sum of squares:
cv_PRESS6 <- sum(cv_pr6$residual^2) / nrow(oz)
cv_PRESS6 # 63.46811

cv_pr7 <- krige.cv(formula = AQI~1, oz, model = v.fit7, nfold=nrow(oz)) 

#Compute the prediction sum of squares:
cv_PRESS7 <- sum(cv_pr7$residual^2) / nrow(oz)
cv_PRESS7 # 59.83212

####################

#Universal kriging:
cv_pr0 <- krige.cv(formula = AQI~Longitude+Latitude, oz, model = v.fit0, nfold=nrow(oz)) 

#Compute the prediction sum of squares:
cv_PRESS0 <- sum(cv_pr0$residual^2) / nrow(oz)
cv_PRESS0 # 59.90906

####################

#Co-kriging:
#Perform cross-validation:
cv_ck <- gstat.cv(vm.fit)

#Compute the prediction sum of squares:
cv_PRESS <- sum(cv_ck$residual^2) / nrow(oz)
cv_PRESS # 0.09700724



# spatial analysis using geoR

library(geoR)

b <-as.geodata(oz)
summary(b)
plot(b)

# Compute and plot the semivariogram
# Using the classical estimator
variogram_classical <- variog(b, dir=pi/2, max.dist=50)
plot(variogram_classical, main="Using the classical estimator", ylim = c(0, 120))

# Using the robust estimator
variogram_robust <- variog(b, dir=pi/2, max.dist=50, estimator.type="modulus")
plot(variogram_robust, main="Using the robust estimator", ylim = c(0, 120))

# Compute the semivariogram cloud for both estimators and construct the box plot

# Using the classical estimator
variogram_classical_cloud <- variog(b, dir=pi/2, max.dist=50, option="cloud")

# Using the robust estimator
variogram_robust_cloud <- variog(b, dir=pi/2, max.dist=50, option="cloud", estimator.type="modulus")

# box plot
classical_cloud <- variog(b, dir=pi/2, max.dist=50, bin.cloud=T) 
robust_cloud <- variog(b, dir=pi/2, max.dist=50, bin.cloud=T, estimator.type="modulus")
plot(classical_cloud, bin.cloud=T) 
plot(robust_cloud, bin.cloud=T)

#################################################
#################################################

# Fit a variogram

var4 <- variog4(b) 
plot(var4, main = "Variogram on the 4 main directions")

var2 <- variog(b, direction=pi/2, max.dist=50, estimator.type="modulus")
plot(var2)

initial.values <- expand.grid(seq(50, 100, by=1),
                              seq(0, 10, by=0.1)) 

fit1 <- variofit(var2, cov.model="exp", ini.cov.pars=initial.values,
                 fix.nugget=FALSE, nugget=0, wei="equal")

fit2 <- variofit(var2, cov.model="exp", ini.cov.pars=initial.values,
                 fix.nugget=FALSE, nugget=0, wei="npairs")

fit3 <- variofit(var2, cov.model="exp", ini.cov.pars=initial.values,
                 fix.nugget=FALSE, nugget=0, wei="cressie")

plot(var2, main = "Exponential Models") 
lines(fit1, type="l", lty=2) 
lines(fit2, type="l", lty=3) 
lines(fit3, type="l") 
legend("bottomright", legend=c("Equal weights", "Number of pairs", "Weights by Cressie"), lty=c(2,3,1))

#################################################
#################################################
#################################################

# Perform ordinary kriging:
#Ordinary kriging using the krige.conv function:
qq <- krige.conv(b, locations=grd, krige=krige.control(obj.model=fit3))

#Collapse the vector of the predicted values into a matrix: 
qqq <- matrix(qq$predict, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Ordinary kriging predicted values") 

#Add the data points:
points(oz) 

#Add contours:
contour(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
        add=TRUE, col="black", labcex=1)


#Collapse the vector of the variances into a matrix: 
qqq <- matrix(qq$krige.var, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Ordinary kriging variances") 

#################################################
#################################################

#Perform universal kriging (geoR using the function ksline):
krig_trend <- ksline(b, cov.model="exp", cov.pars=c(40, 20), nugget=20, m0="kt", trend=1, locations=grd)

#Collapse the vector of the predicted values into a matrix: 
qqq <- matrix(krig_trend$predict, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Universal kriging predicted values") 

#Add the data points:
points(oz) 

#Add contours:
contour(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
        add=TRUE, col="black", labcex=1)


#Collapse the vector of the variances into a matrix: 
qqq <- matrix(krig_trend$krige.var, length(seq(from=x.range[1], to=x.range[2], by=0.1)), 
              length(seq(from=y.range[1], to=y.range[2], by=0.1)) ) 

#Use the image.orig to create a raster map: 
image(seq(from=x.range[1], to=x.range[2], by=0.1), seq(from=y.range[1],to=y.range[2], by=0.1), qqq, 
      xlab="Longitude", ylab="Latitude", main="Universal kriging variances") 

#################################################
#################################################
#################################################

#Perform cross validation: 
x_val1 <- xvalid(b, model=fit1)
# plot(x_val1)

#Compute the prediction sum of squares:
dif1 <- oz$AQI - x_val1$predicted
PRESS1 <- sum(dif1^2) / nrow(oz)
PRESS1 # 61.84378

#Perform cross validation: 
x_val2 <- xvalid(b, model=fit2)

#Compute the prediction sum of squares:
dif2 <- oz$AQI - x_val2$predicted
PRESS2 <- sum(dif2^2) / nrow(oz)
PRESS2 # 63.52127

#Perform cross validation: 
x_val3 <- xvalid(b, model=fit3)

#Compute the prediction sum of squares:
dif3 <- oz$AQI - x_val3$predicted
PRESS3 <- sum(dif3^2) / nrow(oz)
PRESS3 # 64.32154

#Or re-estimating the variogram after we omit each data point:
x_val0 <- xvalid(b, model=fit1, reest=TRUE, variog.obj=var2) 

#Compute the prediction sum of squares:
dif0 <- oz$AQI - x_val0$predicted
PRESS0 <- sum(dif0^2) / nrow(oz)
PRESS0 # 62.53157