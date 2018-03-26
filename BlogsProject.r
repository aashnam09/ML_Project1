library(caret)
library(e1071)

solution <- function()
{
train.set<-read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_train.csv',header=FALSE, sep=",")
train.set

test.feb1<-read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_test-2012.02.11.00_00.csv',header=FALSE, sep=",")

test.feb2<-read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_test-2012.02.13.00_00.csv',header=FALSE, sep=",")

test.mar1<-read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_test-2012.03.11.00_00.csv',header=FALSE, sep=",")

test.mar2<-read.csv('C:/Users/aashn/OneDrive/Desktop/Assign1/BlogFeedback/blogData_test-2012.03.13.00_00.csv',header=FALSE, sep=",")

test.feb1
test.feb2
test.mar1
test.mar2

#================== EXPERIMENT 1=========================
BlogVar <- paste("V", 51:60, sep="")

exp1.train <- train.set[BlogVar]
exp1.train

exp1.test.feb1 <- test.feb1[BlogVar]
exp1.test.feb1

exp1.test.feb2 <- test.feb2[BlogVar]
exp1.test.feb2

exp1.test.mar1 <- test.mar1[BlogVar]
exp1.test.mar1 

exp1.test.mar2 <- test.mar2[BlogVar]
exp1.test.mar2 

# Linear Regression
LinearFit.exp1 <- lm(train.set["V281"]$V281 ~ ., data=exp1.train)

PredTrain.LMexp1 <- predict(LinearFit.exp1, exp1.train, se.fit = TRUE)

MeanSqr.LMexp1 <- mean ((PredTrain.LMexp1$fit-train.set["V281"])^2)

print(MeanSqr.LMexp1)

PredFeb1.exp1 <- predict(LinearFit.exp1, exp1.test.feb1, se.fit = TRUE)
PredFeb1.exp1

PredFeb2.exp1 <- predict(LinearFit.exp1, exp1.test.feb2, se.fit = TRUE)
PredFeb2.exp1

PredMar1.exp1 <- predict(LinearFit.exp1, exp1.test.mar1, se.fit = TRUE)
PredMar1.exp1

PredMar2.exp1 <- predict(LinearFit.exp1, exp1.test.mar2, se.fit = TRUE)
PredMar2.exp1

MeanSqr.PredFeb1.exp1 <- mean((PredFeb1.exp1$fit-test.feb1$V281)^2)
MeanSqr.PredFeb2.exp1 <- mean((PredFeb2.exp1$fit-test.feb2$V281)^2)
MeanSqr.PredMar1.exp1 <- mean((PredMar1.exp1$fit-test.mar1$V281)^2)
MeanSqr.PredMar2.exp1 <- mean((PredMar2.exp1$fit-test.mar2$V281)^2)

print("Test MSE - Experiment 1")
print(MeanSqr.PredFeb1.exp1)
print(MeanSqr.PredFeb2.exp1)
print(MeanSqr.PredMar1.exp1)
print(MeanSqr.PredMar2.exp1)

# Logistic Regression

y = train.set["V281"]

y.class = y
y.mean = mean(y$V281)
y.class$V281[y$V281 <= y.mean] = 0
y.class$V281[y$V281 > y.mean] = 1

exp1.glm <- glm(y.class$V281 ~ ., data=exp1.train)
exp1.glm

PredExp1.glm.train <- predict(exp1.glm, exp1.train, se.fit = TRUE)

PredExp1.glm.train$fit[PredExp1.glm.train$fit <= .5] = 0
PredExp1.glm.train$fit[PredExp1.glm.train$fit > .5] = 1

confusionMatrix(PredExp1.glm.train$fit, unlist(y.class))			

test.feb1$V281[test.feb1$V281 <= y.mean] = 0
test.feb1$V281[test.feb1$V281 > y.mean] = 1

PredExp1.Feb1glm <- predict(exp1.glm, exp1.test.feb1, se.fit = TRUE)
PredExp1.Feb1glm

PredExp1.Feb1glm$fit[PredExp1.Feb1glm$fit <= .5] = 0
PredExp1.Feb1glm$fit[PredExp1.Feb1glm$fit > .5] = 1

confusionMatrix(PredExp1.Feb1glm$fit, test.feb1$V281)

test.feb2$V281[test.feb2$V281 <= y.mean] = 0
test.feb2$V281[test.feb2$V281 > y.mean] = 1

PredExp1.Feb2glm <- predict(exp1.glm, exp1.test.feb2, se.fit = TRUE)
PredExp1.Feb2glm

PredExp1.Feb2glm$fit[PredExp1.Feb2glm$fit <= .5] = 0
PredExp1.Feb2glm$fit[PredExp1.Feb2glm$fit > .5] = 1

confusionMatrix(PredExp1.Feb2glm$fit, test.feb2$V281)

test.mar1$V281[test.mar1$V281 <= y.mean] = 0
test.mar1$V281[test.mar1$V281 > y.mean] = 1

PredExp1.Mar1glm <- predict(exp1.glm, exp1.test.mar1, se.fit = TRUE)
PredExp1.Mar1glm

PredExp1.Mar1glm$fit[PredExp1.Mar1glm$fit <= .5] = 0
PredExp1.Mar1glm$fit[PredExp1.Mar1glm$fit > .5] = 1

confusionMatrix(PredExp1.Mar1glm$fit, test.mar1$V281)

test.mar2$V281[test.mar2$V281 <= y.mean] = 0
test.mar2$V281[test.mar2$V281 > y.mean] = 1

PredExp1.Mar2glm <- predict(exp1.glm, exp1.test.mar2, se.fit = TRUE)
PredExp1.Mar2glm

PredExp1.Mar2glm$fit[PredExp1.Mar2glm$fit <= .5] = 0
PredExp1.Mar2glm$fit[PredExp1.Mar2glm$fit > .5] = 1

confusionMatrix(PredExp1.Mar2glm$fit, test.mar2$V281)

#===========================================EXPERIMENT 2=================================================
BlogVar <- paste("V", 63:262, sep="")

exp2.train <- train.set[BlogVar]
exp2.train

exp2.test.feb1 <- test.feb1[BlogVar]
exp2.test.feb1

exp2.test.feb2 <- test.feb2[BlogVar]
exp2.test.feb2

exp2.test.mar1 <- test.mar1[BlogVar]
exp2.test.mar1

exp2.test.mar2 <- test.mar2[BlogVar]
exp2.test.mar2

LinearFit.exp2 <- lm(train.set["V281"]$V281 ~ ., data=exp2.train)

PredTrain.LMexp2 <- predict(LinearFit.exp2, exp2.train, se.fit = TRUE)

MeanSqr.LMexp2 <- mean ((PredTrain.LMexp2$fit-train.set["V281"])^2)
print(MeanSqr.LMexp2)

PredFeb1.exp2 <- predict(LinearFit.exp2, exp2.test.feb1, se.fit = TRUE)
PredFeb1.exp2

PredFeb2.exp2 <- predict(LinearFit.exp2, exp2.test.feb2, se.fit = TRUE)
PredFeb2.exp2

PredMar1.exp2 <- predict(LinearFit.exp2, exp2.test.mar1, se.fit = TRUE)
PredMar1.exp2

PredMar2.exp2 <- predict(LinearFit.exp2, exp2.test.mar2, se.fit = TRUE)
PredMar2.exp2

MeanSqr.PredFeb1.exp2 <- mean ((PredFeb1.exp2$fit-test.feb1$V281)^2)
MeanSqr.PredFeb2.exp2 <- mean ((PredFeb2.exp2$fit-test.feb2$V281)^2)
MeanSqr.PredMar1.exp2 <- mean ((PredMar1.exp2$fit-test.mar1$V281)^2)
MeanSqr.PredMar2.exp2 <- mean ((PredMar2.exp2$fit-test.mar2$V281)^2)

print("Test MSE - Experiment 2")
print(MeanSqr.PredFeb1.exp2)
print(MeanSqr.PredFeb2.exp2)
print(MeanSqr.PredMar1.exp2)
print(MeanSqr.PredMar2.exp2)

# Logistic Regression
exp2.glm <- glm(y.class$V281 ~ ., data=exp2.train)
exp2.glm

PredExp2.glm.train <- predict(exp2.glm, exp2.train, se.fit = TRUE)

PredExp2.glm.train$fit[PredExp2.glm.train$fit <= .5] = 0
PredExp2.glm.train$fit[PredExp2.glm.train$fit > .5] = 1

confusionMatrix(PredExp2.glm.train$fit, unlist(y.class))

PredExp2.Feb1glm <- predict(exp2.glm, exp2.test.feb1, se.fit = TRUE)
PredExp2.Feb1glm 

PredExp2.Feb1glm$fit[PredExp2.Feb1glm$fit <= .5] = 0
PredExp2.Feb1glm$fit[PredExp2.Feb1glm$fit > .5] = 1

confusionMatrix(PredExp2.Feb1glm$fit, test.feb1$V281)

PredExp2.Feb2glm <- predict(exp2.glm, exp2.test.feb2, se.fit = TRUE)
PredExp2.Feb2glm

PredExp2.Feb2glm$fit[PredExp2.Feb2glm$fit <= .5] = 0
PredExp2.Feb2glm$fit[PredExp2.Feb2glm$fit > .5] = 1

confusionMatrix(PredExp2.Feb2glm$fit, test.feb2$V281)

PredExp2.Mar1glm <- predict(exp2.glm, exp2.test.mar1, se.fit = TRUE)
PredExp2.Mar1glm

PredExp2.Mar1glm$fit[PredExp2.Mar1glm$fit <= .5] = 0
PredExp2.Mar1glm$fit[PredExp2.Mar1glm$fit > .5] = 1

confusionMatrix(PredExp2.Mar1glm$fit, test.mar1$V281)

PredExp2.Mar2glm <- predict(exp2.glm, exp2.test.mar2, se.fit = TRUE)
PredExp2.Mar2glm

PredExp2.Mar2glm$fit[PredExp2.Mar2glm$fit <= .5] = 0
PredExp2.Mar2glm$fit[PredExp2.Mar2glm$fit > .5] = 1

confusionMatrix(PredExp2.Mar2glm$fit, test.mar2$V281)

}


