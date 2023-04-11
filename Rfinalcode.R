if (!require("car")){install.packages("car")};library(car)
if (!require("DAAG")){install.packages("DAAG",dependencies = TRUE)};library(DAAG) 
if (!require("olsrr")){install.packages("olsrr",dependencies = TRUE)};library(olsrr) 
library(readxl)
library(dummies)
library(VIF)
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
#gc() #free up memrory and report the memory usage
av31_91<-data.frame(datastas_coursework)
model1<-lm(sales ~ price+ad1+ad2+prom+as.factor(region)+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11, data=av31_91)
summary(model1)
vif(model1)
durbinWatsonTest(model1)
hist(resid(model1))
library(olsrr)
library(DAAG)
library(car)
step_fwd<-ols_step_forward_p(model1)
set.seed(42)
n=nrow(av31_91) #The number of rows in the table FULL_DATASET.
trainingsetsize<- round(n*.8) #The size of the training set.
ramdom_selection<- sample(x = 1:nrow(av31_91), size = trainingsetsize) #The number of the rows that have been randomly alocated to the training set.
yourowntrainingset<- av31_91[ramdom_selection,] #New table holding the training set.
yourowntestset<- av31_91[-ramdom_selection,] #New table holding the testset.

#check the content of the traininset and the test set by typing into the console:
yourowntrainingset
yourowntestset

######Out cross validation#####
cvresult <- cv.lm(data=yourowntrainingset,form.lm =formula(sales ~ price+ad1+ad2+prom+as.factor(region)+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11),m = 64 ,plotit = FALSE) #This function provides the cross-validation income predictions for each of the rows in the training set. You can find the cross-validation income prediction in the colums named "cvpred" of the table 'cvresult'. Note that this cross-validation prediction is the estimated function f(-1)(x_1) that appears in the second step of the Leave-one-out slide of Week 7. 
head(cvresult) #First five observations of the table 'cvresult'

# Average square prediction error using the data in 'cvresult'. Following the leave-one-out cross-validation procedure presented in the slides of Week 7. This is simply done by:

CVMSPE<- sum( (yourowntrainingset[,"sales"]-cvresult[,"cvpred"])^2  )  / nrow(yourowntrainingset)# cross validation mean squared predition error.
CVMSPE
sqrt(CVMSPE)#Square root of the CVMSPE. This value allow for a better interpretation of the CVMSPE, as it is in the same scale as the predicted variable 'sales'. We can interpret this 'sqrt(CVMSPE) value as the expected error the model may commit when prediction the sales of the company. For example, if  sqrt(CVMSPE) was equal to 85.6, then you could say that for new bakeries, you are expecting to make a prediction of their income that would be off by 85.6. Important note: C.V. can lead to underestimations of the prediction error. For this reason, is better to estimate the prediction error in the test set (point 9).


#####The Testset#####
#Computing the average square prediction error for the observations in the test set.
#Hints: You will need an already fitted model. You can use the function 'predict.lm'.

final_mod<- lm(sales ~ price+ad1+ad2+prom+as.factor(region)+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11,data = yourowntrainingset) #The final chosen linear model for to the trainingset data. (Considering variable selection and any other procedures.)
predicted_values<- predict.lm(object = final_mod,newdata = yourowntestset)#The predicted income values for the bakeries in the training set, using fitted model 'final_mod'.

testsetMSPE=sum( (yourowntestset[,"sales"]-predicted_values)^2 ) / nrow(yourowntestset)# The test set, mean squared prediction error. However, for ease of interpretation you can take the square root of this value. See below:
sqrt(testsetMSPE)#Often the MSPE obtained from the test set is larger than the Cross-Validation prediction error.

ad1_sales<-sum(av31_91$ad1)*model1$coefficients[3] #question 1 answer
ad2_banners<-sum(av31_91$ad2)*model1$coefficients[4]
ad3_promotions<-sum(av31_91$prom)*model1$coefficients[5]

ef_ad1<-(ad1_sales)/2000000 #question 2 answer
ef_ad2<-(ad2_banners)/500000

cvresult <- cv.lm(data=yourowntrainingset,form.lm =formula(sales ~ price+ad1+ad2+prom+as.factor(region)+m1+m2+m3+m4+m5+m6+m7+m8+m9+m10+m11),m =10 ,plotit = TRUE) 

plot(model1)
