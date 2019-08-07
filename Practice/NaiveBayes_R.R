training <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/iris_train.csv')
test <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/iris_test.csv')

library(klaR)  #naive bayes library
nb_mod<- NaiveBayes(Species~.,data=training)
pred<-predict(nb_mod,test)
#confusion matrix
tab<-table(pred$class,test$Species)
caret::confusionMatrix(tab)
#Plot density of each feature using nb_mod
opar=par(mfrow=c(2,2),mar=c(4,0,0,0))
plot(nb_mod,main="")
par(opar)

#Plotting the Confusion Matrix
library(ggplot2)
test$pred<-pred$class
ggplot(test, aes(Species, pred, color = Species)) +
  geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", 
       subtitle="Predicted vs. Observed from Iris dataset", y="Predicted", x="Truth")
