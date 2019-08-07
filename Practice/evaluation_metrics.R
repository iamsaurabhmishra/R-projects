# 1. Import dataset
trainData <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/breastcancer_training.csv')
testData <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/breastcancer_test.csv')

# 2. Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=trainData)

# 3. Predict on testData
pred <- predict(logitmod, newdata = testData, type = "response")

# 4. If p > .5, then Class is 1 else 0
y_pred <- ifelse(pred > 0.5, 1, 0)
y_act <- testData$Class

# 5. Accuracy
mean(y_pred == y_act)  # 94%
#The computed the accuracy from the above model turned out to be 94%, 
#which looks pretty good. But, it doesn’t reveal much 
#information about how well the model actually did in predicting the 1’s and 0’s independently.
#Nor does it say how well it would have performed with a different prediction probability cutoff.
library(caret)
tab<-table(y_pred,testData$Class)
caret::confusionMatrix(tab)
# 122/133 benign cases are predicted as benign,70/71 malignant are predicted correctly as well
#1 malignant case was predicted as benign(false negative/type 2 error)
#11 benign cases were predicted as malignant (false positive/type 1 error)

#Sensitivity is the percentage of actual 1’s that were correctly predicted. 
#It shows what percentage of 1’s were covered by the model.
#The total number of 1’s is 71 out of which 70 was correctly predicted. 
#So, sensitivity is 70/71 = 98.59%
#Sensitivity matters more when classifying the 1’s 
#correctly is more important than classifying the 0’s. 
#Specificity is the proportion of actual 0’s that were correctly predicted.
#So in this case, it is 122 / (122+11) = 91.73%.
#Specificity matters more when classifying the 0’s correctly is more important than classifying the 1’s.
#Maximizing specificity is more relevant in cases like spam detection, 
#where you strictly don’t want genuine messages (0’s) to end up in spam (1’s).
#Detection rate is the proportion of the whole sample where the events were detected correctly. 
#So, it is 70 / 204 = 34.31%.

library(InformationValue)
actual <- as.numeric(as.character(y_act))
pred <- as.numeric(as.character(y_pred))
precision(actual,pred)
InformationValue::plotROC(y_act, pred)
InformationValue::AUROC(y_act, pred)

















