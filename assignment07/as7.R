# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 15/10/20

#Required Packages
require(MASS)
require(rcompanion)
require(caret)
require(leaps)
require(HSAUR3)
require(car)

#Parameters
n <- 100
df1 <- data.frame(x1=numeric(),x2=numeric(),y=numeric())
df2 <- data.frame(x1=numeric(),x2=numeric(),y=numeric())
df3 <- data.frame(xa=numeric(),ya=numeric(),yb=numeric())
df4 <- data.frame(x1=numeric(),x2=numeric(),y=numeric())
df5 <- data.frame(x1=numeric(),x2=numeric(),x3=numeric(),x4=numeric(),y=numeric())

for (i in 1:n){
  #Independent Variables
  x1 <- runif(1)
  x2 <- runif(1)
  x3 <- rchisq(1,2)
  x4 <- rnorm(1)
  xa <- rexp(1)

  #Creating relations in DataFrames
  df1 <- rbind(df1,data.frame(x1=x1, x2=x2, y = 4*x1+5*x2))
  df2 <- rbind(df2,data.frame(x1=x1, x2=x2, y = x1^2+rexp(1)))
  df3 <- rbind(df3,data.frame(xa=xa, y1=sqrt(xa)+1, y2= 2*x2 + rnorm(1,20,1)))
  df4 <- rbind(df4,data.frame(x1=x1, x2=x2, y = exp(2) * x1^2 * x2^3))
  df5 <- rbind(df5,data.frame(x1=x1, x2=x2, x3=x3, x4=x4, y = 4*x1^3*20*x2+x3/5+log(x4^2)))
}

#Work with DataFrame 1

sink('linear_regression_eq1.txt',append = TRUE)
model <- lm(y ~ x1 + x2,data = df1)
summary(model)
sink(file = NULL)


png('li.png',width = 2000, height = 1800,res = 275)
par(mfrow=c(2,2))    # set the plotting area into a 1*2 array
plot(model)
dev.off()
#Work with DataFrame 2
model2 <- lm(y ~ x1, data = df2)

png('plots_df2.png',width = 2000, height = 1800,res = 275)
par(mfrow=c(2,2))    # set the plotting area into a 1*2 array
plot(model2)
dev.off()

#Box-Cox Transformation
png('likelihood.png',width = 2000, height = 1800,res = 275)
bc <- boxcox(model2)
dev.off()
lambda <- bc$x[which.max(bc$y)]
# re-run with transformation
if(lambda!=0){
  mnew <- lm(((y^lambda-1))/lambda ~ x1,data = df2)
} else{
  mnew <- lm(log(y) ~ x1,data = df2)
}

png('boxcox.png',width = 2000, height = 1800,res = 275)
par(mfrow=c(2,2))    # set the plotting area into a 2*2 array
plot(mnew)
dev.off()

png('tukeyimg.png',width = 2000, height = 1800,res = 275)
sink('tukey.txt',append = TRUE)
T_tuk <- transformTukey(df2$x1, plotit=FALSE)
sink(file = NULL)
plotNormalHistogram(T_tuk)
dev.off()

#Work with DataFrame 3
res_man <- manova(cbind(y1,y2) ~ xa, data = df3)
summary(res_man)

sink('manova_eq3.txt',append = TRUE)
summary.aov(res_man)


a3 <- powerTransform(cbind(y1, y2) ~ xa, df3)


#Work with DataFrame 4
logModel <- lm(y ~ log(x1)+log(x2), data = df4)
sink('log.txt',append = TRUE)
coef(logModel)
sink(file = NULL)

#Work with DataFrame 5
sink('mlr.txt',append = TRUE)
mlr <- lm(y ~.,data = df5)
summary(mlr)
sink(file = NULL)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)
# Train the model
step.model <- train(y ~., data = df5,
                    method = "leapBackward",
                    tuneGrid = data.frame(nvmax = 1:5),
                    trControl = train.control
                    )
step.model$results
step.model$bestTune

sink('stepwise.txt',append = TRUE)
summary(step.model$finalModel)
coef(step.model$finalModel, 2)
sink(file = NULL)