# install.packages("Amelia")
# install.packages("randomForest")

library(randomForest)
library(MASS)
library(Amelia)

formula <- Survived ~ Pclass + Sex + AgeD + FamilySizeD + Fare + Embarked
features <- c("Pclass", "Sex", "AgeD", "FamilySizeD", "Fare", "Embarked")

trainData  <- read.csv(file="data/train.csv", head=TRUE, sep=",")
testData   <- read.csv(file="data/test.csv" , head=TRUE, sep=",")
testData$Survived <- -1

########### Data Cleansing Begin ##############
combi <- rbind(trainData, testData)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$FamilySizeD[combi$FamilySize == 1] <- 'singleton'
combi$FamilySizeD[combi$FamilySize > 4] <- 'large'
combi$FamilySizeD[1 <= combi$FamilySize & combi$FamilySize < 5] <- 'small'
combi$FamilySizeD <- as.factor(combi$FamilySizeD)

combi$Age[is.na(combi$Age)] <- -1
combi$AgeD[combi$Age < 0] <- "adult"
combi$AgeD[combi$Age > 0 & combi$Age < 19] <- "child"
combi$AgeD[combi$Age > 18 & combi$Age < 61] <- "adult"
combi$AgeD[combi$Age > 60 ] <- "senior"
combi$AgeD <- as.factor(combi$AgeD)

combi$Embarked <- as.factor(combi$Embarked)

combi$Fare[is.na(combi$Fare)] <- 0

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- as.factor(sub(' ', '', combi$Title))

trainData <- head(combi, nrow(trainData))
testData  <- tail(combi, nrow(testData))
########### Data Cleansing End   ##############


# missmap(trainData, main = "Missing values vs observed")
trainIndex <- sample(1:nrow(trainData), size=round(0.9*nrow(trainData)), replace=FALSE)
train <- trainData[trainIndex, ]
valid <- trainData[-trainIndex, ]
test  <- testData

# Omit NA records
# train <- na.omit(train)
# valid <- na.omit(valid)
# test  <- na.omit(test)

# Replace NA records with avg

# Factor like Enum in C, transformed explicitly by data$col <- factor(data$col)
train$Pclass <- factor(train$Pclass) 
valid$Pclass <- factor(valid$Pclass) 
test$Pclass  <- factor(test$Pclass)

str(train) # 'data.frame':  418 obs. of  11 variables... 
head(train)
summary(train)

# summary(valid)
# summary(test)

# train[1,] # first col
# train[1]  # first line
# train$Name[1:10]

sapply(train, sd) # standard deriviation

lr.glm <- glm(
    formula=formula,
    data=train,
    family=binomial("logit"))

lr.stepAIC = stepAIC(lr.glm, direction="backward") ###变量筛选方法-逐步回归对方程修正 向后回归法

model <- lr.glm
confint(model)
summary(model)

valid.results <- predict(
    model,
    newdata=valid)
valid.results <- ifelse(valid.results > 0.5, 1, 0)
errorRate <- mean(valid.results != valid$Survived)
print(paste("Accuracy", 1 - errorRate))

test.results <- predict(
    model, 
    newdata=test)
test.results <- ifelse(test.results > 0.5, 1, 0)
outcsv <- data.frame(PassengerId=test$PassengerId, Survived=test.results)
write.csv(outcsv, file="data/submit_lr.csv", row.names=FALSE, quote=FALSE)