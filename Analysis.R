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
detach(usopen)

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

par(mfrow = c(2, 2))
plot(model_usopen_std)

#regression again
model_usopen_std_2 <-
  lm(FNL1 ~ BPC.1 +  BPW.1 + TPW.1 ,
     data = usopen_train_standard)
summary(model_usopen_std_2)
par(mfrow = c(2, 2))
plot(model_usopen_std_2)
library(car)
vif(model_usopen_std_2)


model_usopen_std_2p <-
  lm(FNL1 ~ BPC.1 +   TPW.1 ,
     data = usopen_train_standard)
summary(model_usopen_std_2p)
par(mfrow = c(2, 2))
plot(model_usopen_std_2p)
library(car)
vif(model_usopen_std_2p)
anova(model_usopen_std_2, model_usopen_std_2p)

#

#test
t <- predict(model_usopen_std_2p, usopen_test)
mean((t - usopen_test[, c("FNL1")]) ** 2)


t <- predict(model_usopen_std, usopen_test)
mean((t - usopen_test[, c("FNL1")]) ** 2)

t <- predict(model_usopen_std_2, usopen_test)
mean((t - usopen_test[, c("FNL1")]) ** 2)

#