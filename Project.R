library(ggplot2)
library(lattice)
library(data.table)
library(data.table)
library(MASS)

setwd("C:\\Users\\anast\\OneDrive\\Documents\\R")
Retaildata3<-read.table("Retaildata.csv",header = TRUE,sep = ",")
colSums(is.na(Retaildata3))

summary(Retaildata3)
str(Retaildata)
head(Retaildata3)
Retaildata4<-Retaildata3[ ,-1]
View(Retaildata3)

set.seed(1001)
bp3<-boxplot(Retaildata4$Months.Since.Last.Buy~Retaildata4$Sale.Made)
bp5<-boxplot(Retaildata4$Spend.Numeric~Retaildata4$Sale.Made)
bp1
str()

Mean<-mean(Retaildata4$Spend.Numeric)
Sd<-sd(Retaildata4$Spend.Numeric)
minlim<-Mean-2*Sd
maxlim<-Mean+2*Sd
for(i in 1:nrow(Retaildata4))
{
  if(Retaildata4[i,"Spend.Numeric"]>maxlim)
  {
    Retaildata4[i,"Spend.Numeric"]=maxlim
  }
  if(Retaildata4[i,"Spend.Numeric"]<minlim)
  {
    Retaildata4[i,"Spend.Numeric"]=minlim
  }
}
summary(Retaildata4)
bp5
#THE AMOUNT SOEND BY THE CUSTOMER SAME ..BUT THERE IS VARIATION IN SALE MADE
#TESTING AND TRAINING DATASET
library(caTools)
set.seed(1000)

table(Retaildata4$Sale.Made)
sale_data1<-Retaildata4[(Retaildata4$Sale.Made=='1'), ]
sale_data2<-Retaildata4[(Retaildata4$Sale.Made=='0'), ] 
train_data1<-round(nrow(sale_data1)*.80,0)
train_data2<-round(nrow(sale_data2)*.80,0)
index_traindata1<-sample(1:nrow(sale_data1),train_data1,replace = FALSE)
index_traindata2<-sample(1:nrow(sale_data2),train_data2,replace = FALSE)
train_saledata1<-sale_data1[index_traindata1, ]
train_saledata2<-sale_data2[index_traindata2, ]
train_saledata<-rbind(train_saledata1,train_saledata2)
View(train_saledata)
test_saledata1<-sale_data1[-index_traindata1, ]
test_saledata2<-sale_data2[-index_traindata2, ]
test_saledata<-rbind(test_saledata1,test_saledata2)
table(train_saledata$Sale.Made)
table(test_saledata$Sale.Made)
names(train_saledata)
names(test_saledata)
#Dicision tree
library(rpart)
table(Retaildata4)
mytree<-rpart(Sale.Made~Months.Since.Last.Buy+Spend.Category+Spend.Numeric+Mens.Merchandise+Womens.Merchandise+Area+New.Customer+Purchase.Channel,data = train_saledata)
predict_saledata<-predict(mytree,test_saledata)
combined_saledata<-cbind(test_saledata$Sale.Made,predict_saledata)
nrow(test_saledata$Sale.Made)
names(predict_saledata)
names(test_saledata$Sale.Made)
colnames(combined_saledata)<-c("actual","predictive")

#confusion matrix
table(predict_saledata,test_saledata$Sale.Made)
df_combinedtree<-data.frame(combined_saledata)

###model performance
(186+62+4+14)/(186+35+12+4+62+23+13+14)
#76%accuracy
(186+62)/(186+35+62+23)
#81%model sensitivity
#76%accurate high model sensitivity
#ROCR CURVE
library(ROCR)
ROCRpredict<-prediction(predict_saledata,test_saledata$Sale.Made)
ROCPpref<-performance(ROCRpredict,"tpr","fpr")
plot(ROCPpref)
#LOGISTIC REGRESSION



mylogitree<-glm(Sale.Made~Months.Since.Last.Buy+Spend.Category+Spend.Numeric+Mens.Merchandise+Womens.Merchandise+Area+New.Customer+Purchase.Channel,data = train_saledata)
predict1<-predict(mylogitree,test_saledata,type="response")
combined_saledata1<-cbind(test_saledata$Sale.Made,predict1)

names(predict1)
names(test_saledata$Sale.Made)
colnames(combined_saledata1)<-c("actual","predictive")
df_combined_saledata1<-data.frame(combined_saledata1)
table(df_combined_saledata1$acual,df_combined_saledata1$predictive)
preds<-prediction(as.numeric(predict1),as.numeric(test_saledata$Sale.Made))
 
##confusion matrix
table(predict1,test_saledata$Sale.Made)

