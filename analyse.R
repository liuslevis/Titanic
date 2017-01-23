library(MASS)
library(Amelia) # install.packages("Amelia") for plotting

keepTrain <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
keepTest              <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")

trainData <- read.csv(file="data/train.csv", head=TRUE, sep=",")[keepTrain]
trainIndex <- sample(1:nrow(trainData), size=round(0.7*nrow(trainData)), replace=FALSE)

# missmap(trainData, main = "Missing values vs observed")

train <- na.omit(trainData[trainIndex, ]) # na.omit or replace with avg
valid <- na.omit(trainData[-trainIndex, ])
test  <- na.omit(read.csv(file="data/test.csv" , head=TRUE, sep=",")[keepTest])


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
    formula=Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
    data=train,
    family=binomial("logit"))

lr.stepAIC = stepAIC(lr.glm, direction="backward") ###变量筛选方法-逐步回归对方程修正 向后回归法

summary(lr.glm)
summary(lr.stepAIC)

confint(lr.stepAIC)

fitted.results <- predict(lr.glm, newdata=subset(valid, select=c(2,3,4,5,6,7,8), type='response'))
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
errorRate <- mean(fitted.results != valid$Survived)
print(paste("Accuracy", 1 - errorRate))