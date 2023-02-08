install.packages("caTools")
install.packages("rpart")
install.packages("rpart.plot")

library(caTools)
library(rpart)
library(rpart.plot)

Telco = read.csv("Telco-Customer-Churn.csv")
Trial <- Telco[,-1]
str(Trial)

set.seed(123)
split = sample.split(Trial$Churn  , SplitRatio = 0.70)
Train = subset(Trial, split==TRUE)
Test = subset(Trial, split==FALSE)

#CART tree
TrialTree = rpart(Churn ~ . , method="class", data=Train, minbucket=350)
prp(TrialTree)
summary(TrialTree)


TrialPredict = predict(TrialTree, newdata=Test, type="class") # automatically assumes threshold = 0.5 and directly predicts 0 or 1
head(TrialPredict)
tbl = table(Test$Churn , TrialPredict)
tbl
sum(diag(tbl)/sum(tbl))

TrialPredict2 = predict(TrialTree, newdata=Test) # automatically assumes threshold = 0.5 and directly predicts 0 or 1
head(TrialPredict2)
tbl2 = table(Test$Churn, TrialPredict2[,2] > 0.75) # The second column of StevensPredict returns P(y=1), which we compare with the threshold 0.75
sum(diag(tbl2)/sum(tbl2))

# Random Forest
install.packages("randomForest")
library(randomForest)

# The following commands convert the data type of the 0's and 1's from integer to categorical
# If this step is skipped, the randomForest command will use regression trees by default, not classification trees
Train$Churn = as.factor(Train$Churn)
Test$Churn = as.factor(Test$Churn)
str(Train)

#TrialForest = randomForest(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines
#                           + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport + StreamingTV
#                           + StreamingMovies + Contract + PaperlessBilling + PaymentMethod  + MonthlyCharges , data = Train
#                           , ntree=350, nodesize=15)
TrialForest = randomForest(Churn ~ ., data = Train, ntree=350, nodesize=15, na.action = na.omit) #why is this errored?
Variable <- importance(TrialForest)
varImpPlot(TrialForest, sort = TRUE)
sorted_var <- Variable[order(MeanDecreaseGini),]
sorted_var
Variable$MeanDecreaseGini


plot(TrialForest,type="l",main="Error as RF grows")

PredictForest = predict(TrialForest, newdata=Test) # automatically assumes threshold = 0.5 and directly predicts 0 or 1
head(PredictForest)
tbl3 = table(Test$Churn, PredictForest)
tbl3
sum(diag(tbl3)/sum(tbl3))


PredictForest2 = predict(TrialForest, newdata = Test, type = "prob") # returns P(y=0) and P(y=1) for every observation in test data
head(PredictForest2)
tbl4 = table(Test$Churn, PredictForest2[,2] > 0.75) # The second column of PredictForest returns P(y=1), which we compare with the threshold 0.75
sum(diag(tbl4)/sum(tbl4))
