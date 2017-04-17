rm(list = ls())

#Import data
usopen <- read.csv("USOpen-men-2013.csv")
dim(usopen)

str(usopen)
head(usopen)


#Plot data, explore data, and brief description
summary(usopen)
nrow(usopen)
colSums(is.na(usopen))
attach(usopen)

#Histograms
par(mfrow = c(3, 5))
hist(Round, col = "blue", xlab = "Round")
hist(FNL1, col = "blue", xlab = "FNL1")
hist(FSP.1, col = "blue", xlab = "FSP.1")
hist(FSW.1, col = "blue", xlab = "FSW.1")
hist(SSP.1, col = "blue", xlab = "SSP.1")
hist(SSW.1, col = "blue", xlab = "SSW.1")
hist(ACE.1, col = "blue", xlab = "ACE.1")
hist(DBF.1, col = "blue", xlab = "DBF.1")
hist(BPC.1, col = "blue", xlab = "BPC.1")
hist(BPW.1, col = "blue", xlab = "BPW.1")
hist(NPA.1, col = "blue", xlab = "NPA.1")
hist(NPW.1, col = "blue", xlab = "NPW.1")
hist(TPW.1, col = "blue", xlab = "TPW.1")
hist(FSP.2, col = "blue", xlab = "FSP.2")
hist(FSW.2, col = "blue", xlab = "FSW.2")
par(mfrow = c(3, 5))
hist(SSP.2, col = "blue", xlab = "SSP.2")
hist(SSW.2, col = "blue", xlab = "SSW.2")
hist(ACE.2, col = "blue", xlab = "ACE.2")
hist(DBF.2, col = "blue", xlab = "DBF.2")
hist(BPC.2, col = "blue", xlab = "BPC.2")
hist(BPW.2, col = "blue", xlab = "BPW.2")
hist(NPA.2, col = "blue", xlab = "NPA.2")
hist(NPW.2, col = "blue", xlab = "NPW.2")
hist(TPW.2, col = "blue", xlab = "TPW.2")

#Density Plot
par(mfrow = c(3, 5))
plot(density(Round), col = "blue", xlab = "Round")
plot(density(FNL1), col = "blue", xlab = "FNL1")
plot(density(FSP.1), col = "blue", xlab = "FSP.1")
plot(density(FSW.1), col = "blue", xlab = "FSW.1")
plot(density(SSP.1), col = "blue", xlab = "SSP.1")
plot(density(SSW.1), col = "blue", xlab = "SSW.1")
plot(density(ACE.1), col = "blue", xlab = "ACE.1")
plot(density(DBF.1), col = "blue", xlab = "DBF.1")
plot(density(BPC.1), col = "blue", xlab = "BPC.1")
plot(density(BPW.1), col = "blue", xlab = "BPW.1")
plot(density(NPA.1, na.rm = T), col = "blue", xlab = "NPA.1")
plot(density(NPW.1, na.rm = T), col = "blue", xlab = "NPW.1")
plot(density(TPW.1), col = "blue", xlab = "TPW.1")
plot(density(FSP.2), col = "blue", xlab = "FSP.2")
plot(density(FSW.2), col = "blue", xlab = "FSW.2")
par(mfrow = c(3, 5))
plot(density(SSP.2), col = "blue", xlab = "SSP.2")
plot(density(SSW.2), col = "blue", xlab = "SSW.2")
plot(density(ACE.2), col = "blue", xlab = "ACE.2")
plot(density(DBF.2), col = "blue", xlab = "DBF.2")
plot(density(BPC.2), col = "blue", xlab = "BPC.2")
plot(density(BPW.2), col = "blue", xlab = "BPW.2")
plot(density(NPA.2, na.rm = T), col = "blue", xlab = "NPA.2")
plot(density(NPW.2, na.rm = T), col = "blue", xlab = "NPW.2")


#Replace na with 0
usopen$NPA.1[is.na(usopen$NPA.1)] <- 0
usopen$NPW.1[is.na(usopen$NPW.1)] <- 0
usopen$NPW.2[is.na(usopen$NPW.2)] <- 0
usopen$NPA.2[is.na(usopen$NPA.2)] <- 0

cor(usopen[, c(
  "FSP.1",
  "FSW.1",
  "SSP.1",
  "SSW.1",
  "ACE.1",
  "DBF.1",
  "BPC.1",
  "BPW.1",
  "NPA.1",
  "NPW.1",
  "TPW.1",
  "FSP.2",
  "FSW.2",
  "SSP.2",
  "SSW.2",
  "ACE.2",
  "DBF.2",
  "BPC.2",
  "BPW.2",
  "NPA.2",
  "NPW.2",
  "TPW.2"
)])
str(usopen)
#Split into training and testing data
## traning data : 70% of the sample size


# setting the seed to make the partition reproductible
set.seed(999)
index <-
  sample(seq_len(nrow(usopen)), size = floor(0.70 * nrow(usopen)))

usopen_train <- usopen[index, ]
usopen_test <- usopen[-index,]

#


# Perform multiple linear regression using lm() function
# Obtain coefficient estimates
# Obtain R^2, explain what it means.
model_usopen <-
  lm(
    FNL1 ~ FSP.1 + FSW.1 + SSP.1 + SSW.1 + ACE.1 + DBF.1 + BPC.1 + BPW.1 + NPA.1 +
      NPW.1 + TPW.1 + FSP.2 + FSW.2 + SSP.2 + SSW.2 + ACE.2 + DBF.2 + BPC.2 +
      BPW.2 + NPA.2 + NPW.2 + TPW.2,
    data = usopen_train
  )
summary(model_usopen)


model_usopen_p <-
  lm(FNL1 ~  BPC.1 + BPW.1 + TPW.1 + BPW.2 + TPW.2,
     data = usopen_train)
summary(model_usopen_p)

anova(model_usopen, model_usopen_p)


model_usopen_p_2 <-
  lm(FNL1 ~ BPC.1 +  BPW.1 + TPW.1 +  TPW.2,
     data = usopen_train)
summary(model_usopen_p_2)

anova(model_usopen_p, model_usopen_p_2)

#standardize
usopen_train_standard = as.data.frame(apply(usopen_train[, c("FNL1", "BPC.1", "BPW.1", "TPW.1", "TPW.2")], 2, function(x) {
  (x - mean(x)) / sd(x)
}))
# redo regression
model_usopen_std <-
  lm(FNL1 ~ BPC.1 +  BPW.1 + TPW.1 +  TPW.2,
     data = usopen_train_standard)
summary(model_usopen_std)

barplot(model_usopen_std$coefficients)

library(car)
vif(model_usopen_std)

install.packages("ridge")
library(ridge);
library(MASS)
mod<-lm.ridge(FNL1 ~ BPC.1 +  BPW.1 + TPW.1 +  TPW.2,data=usopen_train_standard,lambda=seq(0,1,0.05)); 
summary(mod)
select(mod)
vif(mod)
AIC(mod)

model.matrix(model_usopen_std)

AIC(model_usopen_std)
AIC(model_usopen_std_2)


###
rreg <- lm.ridge(FNL1 ~ BPC.1 +  BPW.1 + TPW.1 +  TPW.2,data=usopen_train_standard,lambda=999999999999)
select(rreg)


y.pred <- as.matrix(cbind(const=1,usopen_test[,c("BPC.1","BPW.1","TPW.1","TPW.2")])) %*% coef(rreg)
str(usopen_test)


mean((y.pred-usopen_test[,c("FNL1")])**2)

y <- usopen_train_standard[,"FNL1"]
X0 <- model.matrix(model_usopen_std)[,-1]
lambda <- seq(1, 2, 0.05)
library(genridge)
aridge <- ridge(y, X0, lambda=lambda)
traceplot(aridge)
coef(aridge)
vridge <- vif(aridge)
vridge
###


plot(usopen$covariate, usopen$response, pch = 20)
abline(lr)
lr$coefficients
sum(lr$residuals)
sum(lr$fitted.values)
points(
  mean(usopen$covariate),
  mean(usopen$response),
  pch = 20,
  col = "blue",
  cex = 3
)
abline(lr, col = "blue", lwd = 3)



#4. Perform the following hypothesis testing and interval estimation using lm() and other related R functions.
#4.1. Perform t tests, obtain t statistics and p values, interpret the results, make a conclusion
#(i.e. reject or not reject) and explain why.  Note: please explain what the null hypothesis is.

#4.2. Perform ANOVA test (F test), obtain F statistic and p value,
#interpret the results, make conclusion (i.e. reject or not reject) and explain why.
#Note: please explain what the null hypothesis is.
anova(lr)
#4.3. Compute confidence interval for coefficients, fitted values (mean response),
#interpret the meanings of these quantities.
confint(lr, level = 0.95)
predict.lm(lr, interval = "confidence")

#4.4. Plot data points, the regression line, the confidence interval for fitted values.

par(mfrow = c(2, 4))
#plot(usopen$covariate, usopen$response, pch = 20)
#abline(lr)

# predicts + interval
newx <-
  seq(min(usopen$covariate),
      max(usopen$covariate),
      length.out = length(usopen$covariate))
preds <-
  predict(
    lr,
    newdata = data.frame(x = newx),
    interval = c("confidence"),
    level = 0.95,
    type = "response"
  )

#preds <- predict(lr, newdata = data.frame(x = newx),                 interval = 'confidence')

# plot
plot(usopen$response ~ usopen$covariate, data = usopen)
# add fill
polygon(c(rev(newx), newx),
        c(rev(preds[, 3]), preds[, 2]),
        col = 'grey80',
        border = NA)
# model
abline(lr)
# intervals
lines(newx, preds[, 3], lty = 'dashed', col = 'red')
lines(newx, preds[, 2], lty = 'dashed', col = 'red')



#5. Repeat the same questions (1-5) but with much more concise descriptions of results for the date set
#<bus.csv> using different covariates.
#Description: Cross-sectional analysis of 24 British bus companies (1951).

#5.1 Use response variable = Expenses per car mile (pence), covariate = Car miles per year (1000s).
bus <-
  read.table(
    "C:/Users/Abhay/Google Drive/MSIS/BANA 7038/Homework 2/bus.csv",
    header = TRUE,
    sep = ","
  )
head(bus)
nrow(bus)
summary(bus)
colSums(is.na(bus))
names(bus) <-
  c("response",
    "covariate1",
    "covariate2",
    "covariate3",
    "covariate4")

mean(bus$covariate1)
mean(bus$response)
sd(bus$covariate1)
sd(bus$response)
par(mfrow = c(2, 4))
hist(bus$covariate1)
hist(bus$response)
plot(bus$covariate1, bus$response, pch = 20)
cor(bus$covariate1, bus$response)

boxplot(bus$covariate1)
boxplot(bus$response)

lrbus <- lm(bus$response ~ bus$covariate1, data = bus)
summary(lrbus)
plot(bus$covariate1, bus$response, pch = 20)
abline(lrbus)
lrbus$coefficients
sum(lrbus$residuals)
sum(lrbus$fitted.values)

anova(lrbus)

confint(lrbus, level = 0.95)
predict.lm(lrbus, interval = "confidence")

par(mfrow = c(1, 1))

#abline(lr)

# predicts + interval
newx <-
  seq(min(bus$covariate1),
      max(bus$covariate1),
      length.out = length(bus$covariate1))
preds <-
  predict(
    lrbus,
    newdata = data.frame(x = newx),
    interval = c("confidence"),
    level = 0.95,
    type = "response"
  )

# plot
plot(bus$response ~ bus$covariate1, data = bus)
abline(lrbus)
# intervals
lines(newx, preds[, 3], lty = 'dashed', col = 'red')
lines(newx, preds[, 2], lty = 'dashed', col = 'red')

#5.2. Use response variable = Expenses per car mile (pence), covariate = Percent of Double Deckers in fleet.

#5.3. Use response variable = Expenses per car mile (pence), covariate = Percent of fleet on fuel oil.

#5.4. Use response variable = Expenses per car mile (pence), covariate = Receipts per car mile (pence).

#5.5. What is your observations on all these analysis?




#
set.seed(1234)
x <- rnorm(20)
df <- data.frame(x = x,
                 y = x + rnorm(20))

plot(y ~ x, data = df)

# model
mod <- lm(y ~ x, data = df)

# predicts + interval
newx <- seq(min(df$x), max(df$x), length.out = 100)
preds <- predict(mod, newdata = data.frame(x = newx),
                 interval = 'confidence')

# plot
plot(y ~ x, data = df, type = 'n')
# add fill
polygon(c(rev(newx), newx),
        c(rev(preds[, 3]), preds[, 2]),
        col = 'grey80',
        border = NA)
# model
abline(mod)
# intervals
lines(newx, preds[, 3], lty = 'dashed', col = 'red')
lines(newx, preds[, 2], lty = 'dashed', col = 'red')
#
