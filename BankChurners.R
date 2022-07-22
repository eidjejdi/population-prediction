setwd("D:/NaverCloud/Lecture/KISA/data")
setwd("D:/Naver MyBox/Lecture/KISA/data")
dsn = read.csv("BankChurners.csv", header = T, fileEncoding = "utf-8")
library("dplyr")
dsn = select(dsn, -c('CLIENTNUM', 'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1', 
                     'Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2'))
dsn$Attrition_Flag = ifelse(dsn$Attrition_Flag == 'Attrited Customer', 1, 0)
table(dsn$Attrition_Flag)
cor(dsn[sapply(dsn, is.numeric)])
dsn1 = dsn[sapply(dsn, is.numeric)]
dsn2 = dsn[sapply(dsn, is.character)]

dsn1_names = names(dsn1)
dsn2_names = names(dsn2)

for (i in 2:15){
  boxplot(dsn1[,i]~dsn1$Attrition_Flag, main=dsn1_names[i])
}
  
res = cor(dsn1)
round(res,2)

library(gmodels)
CrossTable(x = dsn2$Gender, y = dsn$Attrition_Flag, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = dsn2$Education_Level, y = dsn$Attrition_Flag, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = dsn2$Marital_Status, y = dsn$Attrition_Flag, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = dsn2$Income_Category, y = dsn$Attrition_Flag, prop.t=FALSE, expected=TRUE, chisq =TRUE)
CrossTable(x = dsn2$Card_Category, y = dsn$Attrition_Flag, prop.t=FALSE, expected=TRUE, chisq =TRUE)

idx_train = sample(1:nrow(dsn), size = nrow(dsn)*0.7)
df_train = dsn[idx_train, ]
df_test = dsn[-idx_train, ]

library(randomForest)
rf <- randomForest(as.factor(df_train$Attrition_Flag) ~ .,data=df_train, ntree=100, proximity=TRUE)
x <- subset(df_test, select=-Attrition_Flag)
pred<-predict(rf,x)
table(pred,df_test$Attrition_Flag)

library(caret)
usample <- upSample(dsn[,-1], as.factor(dsn[,1]))
table(usample$Class)

idx_train = sample(1:nrow(usample), size = nrow(usample)*0.7)
df_train = usample[idx_train, ]

rf <- randomForest(as.factor(df_train$Class) ~ .,data=df_train, ntree=100, proximity=TRUE)
pred<-predict(rf,x)
table(pred,df_test$Attrition_Flag)
