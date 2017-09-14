setwd("~/Titanic Task")

# Read in train and test data
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# adding new column to dataSets for merging
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

# Creating a Survived column in test dataSet
titanic.test$Survived <- NA

# Merging train and test dataSets
titanic.full <- rbind(titanic.train, titanic.test)

# Clean missing values
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S' 

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median

# Categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)


# Split dataSet back into train and test
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

# Categorical casting of Survived
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Predict Survived based on the given columns
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Formula to predict Survived
survived.formula <- as.formula(survived.equation)

install.packages("randomForest")
library(randomForest)

# Predictive model
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Predict survived from test dataSet
Survived <- predict(titanic.model, newdata = titanic.test)

# dataFrames for passengerId and Survived 
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

# Output results as a .csv file
write.csv(output.df, file = "RESULTS.csv", row.names = FALSE)
