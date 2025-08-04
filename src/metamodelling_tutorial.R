# WSC 2023 Tutorial Models and Plots
#
# First, read data (NOTE: data MUST be in same folder as this code)
library(readxl)
WSC2023QueueData <- read_excel("WSC2023QueueData.xlsx")

# Identify x, y data, and compute means and variances across replications
x = WSC2023QueueData$Mean_Service; y = WSC2023QueueData$Avg_Wait
reps = 3; nDOEpts = 6; groupID = rep(1:nDOEpts, each = reps)
grp_x = tapply(x, groupID, FUN=mean)
grp_meany = tapply(y, groupID, FUN=mean); grp_vary = tapply(y, groupID, FUN=var)

# Next, plot data
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait")


# Linear regression
LinRegMod = lm(Avg_Wait ~ Mean_Service, data = WSC2023QueueData)
xpreds = matrix((70:95)/100,26,1)
ypreds = predict(LinRegMod,data.frame(Mean_Service = xpreds))
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "Linear Regression Metamodel")
lines(xpreds,ypreds,col = "blue", lwd=2)

# Linear regression on log
LogLinRegMod = lm(log(Avg_Wait) ~ Mean_Service, data = WSC2023QueueData)
xpreds = matrix((70:95)/100,26,1)
ypreds = predict(LogLinRegMod,data.frame(Mean_Service = xpreds))
par(mar = c(5,5,3,1)); plot(x,log(y),xlab="Mean Service", ylab="ln Avg Wait", main = "Linear Regression Metamodel for ln(Wait)")
lines(xpreds,ypreds,col = "blue", lwd=2)
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "Linear Regression Metamodel for ln(Wait)")
lines(xpreds,exp(ypreds),col = "blue", lwd=2)

# For nonlinear models, scale Avg_Wait and Mean_Service
scalpm1 = function(x){(x-(min(x)+max(x))/2)/(.5*(max(x)-min(x)))}
scalpm1ext = function(x,xmin,xmax){(x-(xmin+xmax)/2)/(.5*(xmax-xmin))} # for extrapolation
WSC2023QueueData$sAvg_Wait = scalpm1(WSC2023QueueData$Avg_Wait)
WSC2023QueueData$sMean_Service = scalpm1(WSC2023QueueData$Mean_Service)

# Next, quadratic regression, scaling Mean_Service to +/-1
QuadRegMod = lm(Avg_Wait ~ Mean_Service + I(Mean_Service^2), data = WSC2023QueueData)
sQuadRegMod = lm(Avg_Wait ~ sMean_Service + I(sMean_Service^2), data = WSC2023QueueData)
xpreds = matrix((70:95)/100,26,1)
ypreds = predict(sQuadRegMod,data.frame(sMean_Service = scalpm1(xpreds)))
par(mar = c(5,5,3,1)); plot(scalpm1(x),y,xlab="Mean Service", ylab="Avg Wait", main = "Quadratic Regression Metamodel")
lines(scalpm1(xpreds),ypreds,col = "blue", lwd=2)
summary(QuadRegMod)
summary(sQuadRegMod)

# Next, cubic regression
CubRegMod = lm(Avg_Wait ~ sMean_Service + I(sMean_Service^2) + I(sMean_Service^3), data = WSC2023QueueData)
xpreds = matrix((70:95)/100,26,1)
ypreds = predict(CubRegMod,data.frame(sMean_Service = scalpm1(xpreds)))
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "Cubic Regression Metamodel")
lines(xpreds,ypreds,col = "blue", lwd=2)

# Quadratic model on log Wait
LogWQuadRegMod = lm(log(Avg_Wait) ~ sMean_Service + I(sMean_Service^2), data = WSC2023QueueData)
xpreds = matrix((70:95)/100,26,1)
ypreds = exp(predict(LogWQuadRegMod,data.frame(sMean_Service = scalpm1(xpreds))))
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "Quadratic Regression Metamodel for ln(Wait)")
lines(xpreds,ypreds,col = "blue", lwd=2)
lypreds = predict(LogWQuadRegMod,data.frame(sMean_Service = scalpm1(xpreds)))
plot(x,log(y),xlab="Mean Service", ylab="ln(Avg Wait)", main = "Quadratic Regression Metamodel for ln(Wait)")
lines(xpreds,lypreds,col = "blue", lwd=2)

# Gaussian Process fit with variable nugget based on replication variance
library("mlegp")

# No scaling of x or y needed with GP models

GPmodel = mlegp(grp_x,grp_meany,constantMean=1,nugget=grp_vary/reps,nugget.known=1)
xpreds = matrix((1:95)/100,95,1); ypreds = predict(GPmodel,xpreds) 
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "GP (mlegp) Local Nugget Metamodel"); 
lines(xpreds,ypreds,col = "blue", lwd=2)

# Gaussian Process fit based on single-values estimated nugget
anyReps(x)
GPmodel2 = mlegp(grp_x,grp_meany,constantMean=1,nugget=estimateNugget(x,y))
xpreds = matrix((1:95)/100,95,1); ypreds = predict(GPmodel2,xpreds)
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "GP (mlegp) Global Nugget Metamodel")
lines(xpreds,ypreds,col = "blue", lwd=2)

# Neural network fit
# Scaling of both x and y needed. y scaled by 30, greater than its maximum (makes unscaling easy)
library(nnet)
NNmodel <- nnet(Avg_Wait/30 ~ sMean_Service, data = WSC2023QueueData, size=2) 
xpreds = matrix((70:95)/100,26,1); ypreds = predict(NNmodel,data.frame(sMean_Service = scalpm1(xpreds)))*30
par(mar = c(5,5,3,1)); plot(x,y,xlab="Mean Service", ylab="Avg Wait", main = "Neural Network (nnet) Metamodel")
lines(xpreds,ypreds,col = "blue", lwd=2)

# Quadratic model extrapolation
xpreds = matrix((50:120)/100,71,1)
ypreds = predict(QuadRegMod,data.frame(Mean_Service = xpreds))
par(mar = c(5,5,3,1)); 
plot(xpreds,ypreds,xlab="Mean Service", ylab="Avg Wait", main = "Quadratic Regression Metamodel",
     type="l",col = "blue", lwd=2)

# Gaussian Process extrapolation
xpreds = matrix((50:120)/100,71,1); ypreds = predict(GPmodel,xpreds)
par(mar = c(5,5,3,1))
plot(xpreds,ypreds,xlab="Mean Service", ylab="Avg Wait", main = "Gaussian Process Metamodel",
     col = "blue", type = "l", lwd=2)

# Neural network extrapolation
xpreds = matrix((50:120)/100,71,1); ypreds = predict(NNmodel,data.frame(sMean_Service = scalpm1ext(xpreds,.70,.95)))*30
par(mar = c(5,5,3,1))
plot(xpreds,ypreds,xlab="Mean Service", ylab="Avg Wait", main = "Neural Network Metamodel",col = "blue", type = "l", lwd=2)
