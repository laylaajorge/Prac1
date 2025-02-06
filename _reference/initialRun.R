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
beta <- solve(t(xmatrix)%*%xmatrix)%*%t(xmatrix)%*%ymatrix
beta
#======================================
#Simple linear model using lm()
#======================================
modelfit <- lm(dist~speed,data=cars)
summary(modelfit)
