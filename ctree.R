# install.packages("Amelia")
# install.packages("partykit")

library(MASS)
library(Amelia)
library(partykit)

formula <- Survived ~ Pclass + Sex + AgeD + SibSp + Parch + FamilySizeD + Fare + Embarked + Title + CabinD
features <- c("Pclass", "Sex", "AgeD", "FamilySizeD", "Fare", "Embarked")

trainData  <- read.csv(file="data/train.csv", head=TRUE, sep=",")
testData   <- read.csv(file="data/test.csv" , head=TRUE, sep=",")
testData$Survived <- 0

########### Data Cleansing Begin ##############
combi <- rbind(trainData, testData)

combi$Survived <- as.factor(combi$Survived)

combi$Pclass <- factor(combi$Pclass) 

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

# 20% -> 16%
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Capt', 'Col', 'Major', 'Dr', 'Rev')] <- 'Officer'
combi$Title[combi$Title %in% c('Jonkheer', 'Don', 'Sir', 'the Countess', 'Dona', 'Lady')] <- 'Royalty'
combi$Title[combi$Title %in% c('Mme')] <- 'Mrs'
combi$Title[combi$Title %in% c('Mlle')] <- 'Miss'
combi$Title[combi$Title %in% c('Master')] <- 'Master'
combi$Title <- as.factor(combi$Title)

# 1%
combi$Cabin <- as.character(combi$Cabin)
combi$CabinD <- substr(combi$Cabin, 0, 1)
combi$CabinD[combi$CabinD==''] <- 'F'
combi$CabinD <- as.factor(combi$CabinD)

trainData <- head(combi, nrow(trainData))
testData  <- tail(combi, nrow(testData))
########### Data Cleansing End   ##############

# missmap(trainData, main = "Missing values vs observed")
train <- trainData
test  <- testData

# decress mincriterion to get more splits and improve score slightly
ct <- ctree(formula=formula, data=train, control=ctree_control(mtry=1000L, mincriterion=0.05, maxdepth=Inf))
# ?ctree
?ctree_control
plot(ct)
# plot(ct,type='simple')
# summary(combi)
# summary(train)
# summary(test)
# summary(ct)
summary(ct)
print(ct)
# str(ct)


submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(ct, test)

write.csv(submission, file="data/submit_ctree.csv", row.names=FALSE, quote=FALSE)
