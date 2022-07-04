# INTRODUCTION: Factors in COVID-19 case counts in the United States.
# In STEP 1, SLR assumptions are checked, and the data is manipulated to meet SLR assumptions.
# In STEP 2, MLR assumptions are checked, and the data is manipulated to meet MLR assumptions.
# In STEP 3, the model is checked using cross-validation
# In STEP 4, the hypotheses are tested.

# PRELIMINARY
# load dataset
coviddata <- read.csv("covid.csv")

# load libraries
library(ALSM)
library(boot)
library(caret)
library(fmsb)
library(leaps)
library(lmridge)
library(MASS)
library(onewaytests)

# summarize and plot data
summary(coviddata)
cor(coviddata[4:13])
cor(coviddata[-33,4:13]) # North Carolina excluded
plot(coviddata[c(4,7:12)])
plot(coviddata[c(5,7:12)])
plot(coviddata[6:12])
# compare possible response variables
plot(coviddata[4:6])

# Cases will be the response variable

# STEP 1: Diagnose and modify data so that the SLR assumptions are met for each predictor
# define a preliminary linear model
covid.mod <- lm(Cases ~ Masks + Mobility + Vaccinations + Tests + GDP + PopDensity, coviddata)
plot(Cases ~ Masks + Mobility + Vaccinations + Tests + GDP + PopDensity, data = coviddata, ask = FALSE)
summary(covid.mod)

# transform some predictor variables based on graphs
coviddata$logMasks <- log(100 - coviddata$Masks)
coviddata$logTests <- log(coviddata$Tests)
coviddata$logPopDensity <- log(coviddata$PopDensity)

covid.mod2 <- lm(Cases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, coviddata)
plot(Cases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, data = coviddata, ask = FALSE)
summary(covid.mod2)

# use Box-Cox to transform response variable
bcmle <- boxcox((covid.mod2), lambda=seq(-5,5,0.1))
lambda <- bcmle$x[which.max(bcmle$y)]
bcsse <- boxcox.sse(coviddata$logMasks, coviddata$Cases)
lambda2 <- bcsse$lambda[which.min(bcsse$SSE)]
coviddata$transCases <- coviddata$Cases^-0.15 # choose the average lambda

covid.mod3 <- lm(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, coviddata)
plot(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, data = coviddata, ask = FALSE)
summary(covid.mod3)

# define functions to test assumptions
# non-linearity
LoF.tests <- function(x){
  data <- x$model
  p <- length(x$coefficients)
  for(i in seq(2,p)){
    response <- as.numeric(unlist(data[1]))
    explanatory <- signif(as.numeric(unlist(data[i])), digits = 3)
    LMF <- lm(response ~ explanatory)
    LMR <- lm(response ~ factor(explanatory))
    title <- paste(colnames(data[1]), " ~ ", colnames(data[i]), sep = "")
    cat(paste("\n", title, "\n"))
    print(anova(LMF,LMR))
    plot(response ~ explanatory, main = title)
    abline(LMF)
  }
}

# heteroskedasticity
BF.tests <- function(x){
  data <- x$model
  p <- length(x$coefficients)
  for(i in seq(2,p)){
    LM <- lm(as.numeric(unlist(data[1])) ~ as.numeric(unlist(data[i])))
    data$group <- cut(as.numeric(unlist(data[5])), 5)
    data$residual <- LM$residuals
    print(paste(colnames(data[1]), " ~ ", colnames(data[i]), sep = ""))
    bf.test(residual ~ group, data)
  }
}

# non-normality
shapiro.tests <- function(x){
  data <- x$model
  p <- length(x$coefficients)
  for(i in seq(2,p)){
    LM <- lm(as.numeric(unlist(data[1])) ~ as.numeric(unlist(data[i])))
    title <- paste(colnames(data[1]), " ~ ", colnames(data[i]), sep = "")
    print(title)
    print(shapiro.test(residuals(LM)))
    hist(residuals(LM), main = title)
    qqnorm(residuals(LM), main = title); qqline(residuals(LM))
  }
}

# check assumptions for model 3
# non-linearity
LoF.tests(covid.mod3)
residualPlots(covid.mod3)
# heteroskedasticity
BF.tests(covid.mod3)
# non-normality
plot(covid.mod3, ask = FALSE)
shapiro.tests(covid.mod3)

# remove assumption-violating outliers
coviddata2 <- subset(coviddata, X != 8 & X != 39)
covid.mod4 <- lm(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, coviddata2)
plot(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, data = coviddata2, ask = FALSE)
summary(covid.mod4)

# check assumptions for model 4
# non-linearity
LoF.tests(covid.mod4)
residualPlots(covid.mod4)
# heteroskedasticity
BF.tests(covid.mod4)
# non-normality
plot(covid.mod4, ask = FALSE)
shapiro.tests(covid.mod4)

# remove assumption-violating outliers
coviddata3 <- subset(coviddata2, X != 21 & X != 45)
covid.mod5 <- lm(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, coviddata3)
plot(transCases ~ logMasks + Mobility + Vaccinations + logTests + GDP + logPopDensity, data = coviddata3, ask = FALSE)
summary(covid.mod5)

# check assumptions for model 5
# non-linearity
LoF.tests(covid.mod5)
residualPlots(covid.mod5)
# heteroskedasticity
BF.tests(covid.mod5)
# non-normality
plot(covid.mod5, ask = FALSE)
shapiro.tests(covid.mod5)

# STEP 2: Diagnose and transform data so that the MLR assumptions are met
# find the best subset (minimum 5 variables because of project requirement)
plotmodel.s(coviddata3[,c("logMasks", "Mobility", "Vaccinations", "logTests", "GDP", "logPopDensity")], coviddata3$transCases)
BestSub(coviddata3[,c("logMasks", "Mobility", "Vaccinations", "logTests", "GDP", "logPopDensity")], coviddata3$transCases, num = 3)

covid.mod6 <- lm(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity, coviddata3)
plot(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity, data = coviddata3, ask = FALSE)
summary(covid.mod6)

# plot add-on values
avPlots(covid.mod6)

# identify x outliers
n <- nrow(coviddata3)
p <- length(covid.mod6$coefficients)
lm.influence(covid.mod6)$hat
thresh.hat <- 2*p/n
lm.influence(covid.mod6)$hat[which(lm.influence(covid.mod6)$hat > thresh.hat)]

# identify y outliers
rstudent(covid.mod6)
thresh.rstudent <- qt(1 - 0.05/(2*n), n - 1 - p)
rstudent(covid.mod6)[which(abs(rstudent(covid.mod6)) > thresh.rstudent)]

# identify influential cases
influencePlot(covid.mod6)

# DFFITS
dffits(covid.mod6)
dffits(covid.mod6)[which(abs(dffits(covid.mod6)) > 1)]

# Cook's distance
thresh.cooks <- qf(0.2, p, n - p)
cooks.distance(covid.mod6)
cooks.distance(covid.mod6)[which(cooks.distance(covid.mod6) > thresh.cooks)]
plot(covid.mod6, pch = 18, col = "red", which = c(4))

# DFBETAS
dfbetas(covid.mod6)
which(abs(dfbetas(covid.mod6)) > 1, arr.ind = TRUE)
dfbetasPlots(covid.mod6)

# no influential points found

# test for multicollinearity in model 6
plot(coviddata3[c(8:9,14:17)])
cor(coviddata3[c(8:9,14:17)])
car::vif(covid.mod6)

# address multicollinearity by adding interaction terms
coviddata3$Masks.Mobility <- coviddata3$logMasks*coviddata3$Mobility
coviddata3$Masks.PopDensity <- coviddata3$logMasks*coviddata3$logPopDensity
covid.mod7 <- lm(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity + Masks.Mobility + Masks.PopDensity, coviddata3)
summary(covid.mod7)

# test for multicollinearity in model 7
plot(coviddata3[c(8:9,14:16,18:19,17)])
cor(coviddata3[c(8:9,14:16,18:19,17)])
car::vif(covid.mod7)

# address multicollinearity by performing ridge regression
ridgemods <- lmridge(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity + Masks.Mobility + Masks.PopDensity, data = coviddata3, K = seq(0,0.5,0.02))
plot(ridgemods)
lmridge::vif(ridgemods)
# best lambda is about 0.1
ridgemod <- lmridge(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity + Masks.Mobility + Masks.PopDensity, data = coviddata3, K = 0.1)
summary(ridgemod)

# STEP 3: perform cross-validation
set.seed(100)

# randomly cut the data into ten folds
cvData <- coviddata3[c(17,14,8:9,15:16,18:19)]
cvData <- cvData[sample(nrow(cvData)),]
folds <- cut(seq(1,nrow(cvData)),breaks=10,labels=FALSE)

# calculate RMSE
RMSE <- c(rep(0,10))
for(i in 1:10){
  # segment data by fold
  testIndexes <- which(folds == i,arr.ind = TRUE)
  testData <- cvData[testIndexes, ]
  trainData <- cvData[-testIndexes, ]
  # calculate yhats
  mod <- lmridge(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity + Masks.Mobility + Masks.PopDensity, data = cvData, K = 0.1)
  B <- coef(mod)
  N <- nrow(testData)
  Yhat <- c(rep(0,N))
  for(j in 1:N){
    Yhat[j] <- unname(B[1])
    for(k in 2:7){
      Yhat[j] <- Yhat[j] + unname(B[k])*testData[j,k]
    }
  }
  # record RMSE for ith fold
  RMSE[i] <- sqrt(sum((testData[1] - Yhat)^2)/N)
}

mean(RMSE)
mean(coviddata3$transCases)

# STEP 4: perform hypothesis tests
# define a bootstrap model
boot.huber <- function(data, indices, maxit = 200){
  data <- data[indices,]
  mod <- lmridge(transCases ~ logMasks + Mobility + Vaccinations + logTests + logPopDensity + Masks.Mobility + Masks.PopDensity, data, K = 0.1)
  coefficients(mod)
}

covid.boot <- boot(data = coviddata3, statistic = boot.huber, R = 200, maxit = 200)
covid.boot

# plot bootstrap results and display confidence intervals
for(i in 1:length(covid.boot$t0)){
  # exclude impact of Vaccinations, since it is zero
  if(covid.boot$t0[i] != 0){
    plot(covid.boot, index = i)
    title(main = names(covid.boot$t0[i]))
    cat("\n", names(covid.boot$t0[i]), "\n")
    print(boot.ci(covid.boot, index = i, type = "perc"))
  }
}

# find confidence bounds and p-values
# 95% upper bound for Masks
quantile(covid.boot$t[,2], probs = 0.95, type = 6)
mean(covid.boot$t[,2] >= 0)
# 95% upper bound for Mobility
quantile(covid.boot$t[,3], probs = 0.95, type = 6)
mean(covid.boot$t[,3] >= 0)
# 95% upper bound for Masks*Mobility
quantile(covid.boot$t[,7], probs = 0.95, type = 6)
mean(covid.boot$t[,7] >= 0)
# 95% upper bound for Masks*PopDensity
quantile(covid.boot$t[,8], probs = 0.95, type = 6)
mean(covid.boot$t[,8] >= 0)
