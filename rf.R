# install.packages("Amelia")
# install.packages("randomForest")

library(randomForest)
library(MASS)
library(Amelia)
set.seed(415)

formula <- Survived ~ Pclass + Sex + AgeD + FamilySizeD + Fare + Embarked
features <- c("Pclass", "Sex", "AgeD", "FamilySizeD", "Fare", "Embarked")

trainData  <- read.csv(file="data/train.csv", head=TRUE, sep=",")
testData   <- read.csv(file="data/test.csv" , head=TRUE, sep=",")
summary(testData)
testData$Survived <- numeric()
testData$Survived <- 0

########### Data Cleansing Begin ##############
combi <- rbind(trainData, testData)

combi$Survived <- as.factor(combi$combi$Survived)

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

combi$Pclass <- factor(combi$Pclass) 

trainData <- head(combi, nrow(trainData))
testData  <- tail(combi, nrow(testData))
########### Data Cleansing End   ##############

# missmap(trainData, main = "Missing values vs observed")
train <- trainData
test  <- testData

# summary(train)
summary(test)
# sapply(train, sd) # standard deriviation

rf <- randomForest(formula=formula, data=train, importance=TRUE, ntrees=2000)

varImpPlot(rf)

summary(rf)

submission <- data.frame(PassengerId = test$PassengerId)
prediction <- predict(rf, test)
submission$Survived <- ifelse(prediction > 0.5, 1, 0)
write.csv(submission, file="data/submit_rf.csv", row.names=FALSE, quote=FALSE)
