#======================================
#Setup
#======================================
projrsimple::projr_init()
airquality
cars
#======================================
#Find rows with missing values
#======================================
rowsNA <-which((is.na(airquality)))
rowsNA

#======================================
#Calculations(mean,sd,max,min)
#======================================
#?mean
tempmean <- mean(airquality$Temp,na.rm=TRUE)
tempsd <- sd(airquality$Temp,na.rm=TRUE)
tempmin <- min(airquality$Temp,na.rm=TRUE)
tempmax <- max(airquality$Temp,na.rm=TRUE)

ozonemean <- mean(airquality$Ozone,na.rm=TRUE)
ozonesd <- sd(airquality$Ozone,na.rm=TRUE)
ozonemin <- min(airquality$Ozone,na.rm=TRUE)
ozonemax <- max(airquality$Ozone,na.rm=TRUE)

#======================================
#Simple linear model using calculations
#======================================
yvalues <- cars$dist
yvalues
xvalues <- cars$speed
xvalues
ymatrix <- as.matrix(yvalues)
ymatrix
xmatrix <- cbind(1,as.matrix(xvalues))##the 1s allows us to include the intercept in the model thats why without the value of b1 is different
betas <- solve(t(xmatrix)%*%xmatrix)%*%t(xmatrix)%*%ymatrix
betas

f <- function(xvalues,yvalues){
 
  y <- as.matrix(yvalues)
  x <- cbind(1,as.matrix(xvalues))
  beta <- solve(t(x)%*%x)%*%t(x)%*%y
  n <- nrow(y)
  k <- ncol(x)
  residual <- (y-x%*%beta)#how far the precited values are from actual values
  fraction <- 1/(n-k)
  rss <-sum(residual^2)
  s <- sqrt(rss*fraction)
  varres <- s^2*solve(t(x)%*%x)
  se <- sqrt(diag(varres))
  return(list("coefficents"=beta,"std.error"=se))
}
modelcalc <- f(xvalues,yvalues)
modelcalc

summary(modelfit)[['coefficents']][,'Std error']
#======================================
#Simple linear model using lm()
#======================================
modelfit <- lm(dist~speed,data=cars)
summary(modelfit)
lmcoe <- coef(modelfit)
lmcoe
lmse <- summary(modelfit)$coefficients[, 2]
lmse
calcoe <-modelcalc$coefficents
calcoe 
calstd <- modelcalc$std.error
calstd
