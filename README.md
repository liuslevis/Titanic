# Titanic

Machine Learning from Disaster

## Task

1. Predict survival of given passenger in test set.

2. Explain what you've learn.

## Data Format

```
VARIABLE DESCRIPTIONS:
survival        Survival
                (0 = No; 1 = Yes)
pclass          Passenger Class
                (1 = 1st; 2 = 2nd; 3 = 3rd)
name            Name
sex             Sex
age             Age
sibsp           Number of Siblings/Spouses Aboard
parch           Number of Parents/Children Aboard
ticket          Ticket Number
fare            Passenger Fare
cabin           Cabin
embarked        Port of Embarkation
                (C = Cherbourg; Q = Queenstown; S = Southampton)

SPECIAL NOTES:
Pclass is a proxy for socio-economic status (SES)
 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower

Age is in Years; Fractional if Age less than One (1)
 If the Age is Estimated, it is in the form xx.5

With respect to the family relation variables (i.e. sibsp and parch)
some relations were ignored.  The following are the definitions used
for sibsp and parch.

Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
Parent:   Mother or Father of Passenger Aboard Titanic
Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

Other family relatives excluded from this study include cousins,
nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
only with a nanny, therefore parch=0 for them.  As well, some
travelled with very close friends or neighbors in a village, however,
the definitions do not support such relations.
```

## Data Cleansing

```R
# install.packages("Amelia")
# install.packages("randomForest")

library(randomForest)
library(MASS)
library(Amelia)

formula <- Survived ~ Pclass + Sex + AgeD + FamilySizeD + Fare + Embarked + Title + CabinD
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

########### Data Cleansing End   ##############

summary(combi)

###############################################################
  PassengerId   Survived Pclass      Name               Sex
 Min.   :   1   0:967    1:323   Length:1309        female:466
 1st Qu.: 328   1:342    2:277   Class :character   male  :843
 Median : 655            3:709   Mode  :character
 Mean   : 655
 3rd Qu.: 982
 Max.   :1309

      Age            SibSp            Parch            Ticket
 Min.   :-1.00   Min.   :0.0000   Min.   :0.000   CA. 2343:  11
 1st Qu.: 7.00   1st Qu.:0.0000   1st Qu.:0.000   1601    :   8
 Median :24.00   Median :0.0000   Median :0.000   CA 2144 :   8
 Mean   :23.68   Mean   :0.4989   Mean   :0.385   3101295 :   7
 3rd Qu.:35.00   3rd Qu.:1.0000   3rd Qu.:0.000   347077  :   7
 Max.   :80.00   Max.   :8.0000   Max.   :9.000   347082  :   7
                                                  (Other) :1261
      Fare            Cabin           Embarked   FamilySize     FamilySizeD
 Min.   :  0.000   Length:1309         :  2    Min.   : 1.000   large:  82
 1st Qu.:  7.896   Class :character   C:270    1st Qu.: 1.000   small:1227
 Median : 14.454   Mode  :character   Q:123    Median : 1.000
 Mean   : 33.270                      S:914    Mean   : 1.884
 3rd Qu.: 31.275                               3rd Qu.: 2.000
 Max.   :512.329                               Max.   :11.000

     AgeD          Title         CabinD
 adult :1083   Master : 61   F      :1035
 child : 193   Miss   :262   C      :  94
 senior:  33   Mr     :757   B      :  65
               Mrs    :198   D      :  46
               Ms     :  2   E      :  41
               Officer: 23   A      :  22
               Royalty:  6   (Other):   6
###############################################################
```

## Logistic Regression

### Train Model & Predict

```
trainData <- head(combi, nrow(trainData))
testData  <- tail(combi, nrow(testData))

train <- trainData
test  <- testData

rf <- randomForest(formula=formula, data=train, importance=TRUE, ntree=200, do.trace=0, nodesize=2**3)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, test)

write.csv(submission, file="data/submit_rf.csv", row.names=FALSE, quote=FALSE)
```

### Intepretet Model

Print basic info of RF model.

```R
print(rf)

Call:
 randomForest(formula = formula, data = train, importance = TRUE,      ntree = 200, do.trace = 0, nodesize = 2^3)
               Type of random forest: classification
                     Number of trees: 200
No. of variables tried at each split: 2

        OOB estimate of  error rate: 17.17%
Confusion matrix:
    0   1 class.error
0 492  57   0.1038251
1  96 246   0.2807018
```

Now print variable importances, `MeanDecreaseAccuracy` and `MeanDecreaseGini`.

```
print(rf$importance)

                      0           1 MeanDecreaseAccuracy MeanDecreaseGini
Pclass      0.034765316 0.064290071          0.046156168        25.780427
Sex         0.087423429 0.092601983          0.089328456        52.247596
AgeD        0.003163959 0.008927173          0.005402472         6.213197
FamilySizeD 0.031941258 0.003872676          0.021190435        14.273401
Fare        0.036098034 0.047790488          0.040652877        38.712060
Embarked    0.001089857 0.017851786          0.007587945         7.527506
Title       0.111351535 0.129023039          0.118008365        73.595587
CabinD      0.027994844 0.011000870          0.021461199        21.816103


```

Plot variable importances graph.

```R
varImpPlot(rf)
```

![img](./Rplots.pdf)

`Mean decrease in node impurity`: feature importance is calculated by looking at the splits of each tree. The importance of the splitting variable is proportional to the improvement to the gini index given by that split and it is accumulated (for each variable) over all the trees in the forest.

`Mean decrease in accuracy`: This method, proposed in the original paper, passes the OOB samples down the tree and records prediction accuracy. A variable is then selected and its values in the OOB samples are randomly permuted. OOB samples are passed down the tree and accuracy is computed again. A decrease in accuracy obtained by this permutation is averaged over all trees for each variable and it provides the importance of that variable (the higher the decreas the higher the importance).

## CTree

## Random Forest