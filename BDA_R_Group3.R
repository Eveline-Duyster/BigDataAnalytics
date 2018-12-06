#### Reading in packages

library(sqldf)
library(dummies)
library(caret)
library(mlogit)
library(mnlogit)
library(bigmemory)
library(e1071)
library(randomForest)
memory.limit(60000)
library(pROC)

#### Reading in data ####

ESS1 <- read.table("wave1.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors=F, quote= "")
ESS2 <- read.table("wave2.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors=F, quote= "")
ESS3 <- read.table("wave3.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors=F, quote= "")
#ESS <- rbind(ESS1, ESS2, ESS3) # DIT NOG NIET DOEN
names(ESS1)[12]<-"alpha2"
names(ESS2)[12]<-"alpha2"
names(ESS3)[12]<-"alpha2"

WorldBank <- read.delim("unemployment2.txt", header = TRUE, sep = ",", dec=".", quote="", fileEncoding="UTF-16LE")

WorldBank <- read.csv("unemployment.csv", header = TRUE, sep = "", dec=".", quote="\"", fileEncoding="latin1", row.names=NULL)

WorldBank2<-data.frame(cbind(as.character(WorldBank$X), as.character(WorldBank$Country.Name), WorldBank$X2012, WorldBank$X2014, WorldBank$X2016))
names(WorldBank2)<-c("Country Name", "alpha3", "2012", "2014", "2016")

#### Merging data ####

cntrycode<-rbind(c("AL", "ALB"), c("AT", "AUT"), c("BE", "BEL"), c("BG", "BGR"), c("CH", "CHE"), c("CY", "CYP"), 
                 c("CZ", "CZE"), c("DE", "DEU"), c("DK", "DNK"), c("EE", "EST"), c("ES", "ESP"), c("FI", "FIN"),
                 c("FR", "FRA"), c("GB", "GBR"), c("HU", "HUN"), c("IE", "IRL"), c("IL", "ISR"), c("IS", "ISL"), 
                 c("IT", "ITA"), c("LT", "LTU"), c("NL", "NLD"), c("NO", "NOK"), c("PL", "POL"), c("PT", "PRT"), 
                 c("RU", "RUB"), c("SE", "SWE"), c("SI", "SVN"), c("SK", "SVK"), c("UA", "UKR"), c("XK", "UNK"))
cntrycode<-data.frame(matrix(cntrycode, ncol=2))
names(cntrycode) <- c("alpha2", "alpha3")
WorldBank3<-merge(WorldBank2, cntrycode)
Reg1<-WorldBank3[,c(1,3,6)]
Reg2<-WorldBank3[,c(1,4,6)]
Reg3<-WorldBank3[,c(1,5,6)]
Reg1<-cbind(Reg1, rep(1,length(Reg1[,1])))
Reg2<-cbind(Reg2, rep(2,length(Reg2[,1])))
Reg3<-cbind(Reg3, rep(3,length(Reg3[,1])))

names(Reg1)[2]<-"unempl_reg"
names(Reg2)[2]<-"unempl_reg"
names(Reg3)[2]<-"unempl_reg"
names(Reg1)[4]<-"Wave"
names(Reg2)[4]<-"Wave"
names(Reg3)[4]<-"Wave"
#Reg<-rbind(Reg1, Reg2, Reg3)

merged1<-merge(ESS1, Reg1, all.x=TRUE)
merged2<-merge(ESS2, Reg2, all.x=TRUE)
merged3<-merge(ESS3, Reg3, all.x=TRUE)
merged<-rbind(merged1, merged2, merged3)

df1 <- dummy(merged$alpha2, sep = "_")

Data<-cbind(merged, df1)

write.csv(DataFull, file='BDAdataFull.csv')

#### Descriptive Stats ####

sumstat <- summary(Data)
describe(Data)
hist(Data$happy2, main = "Histogram of the Happiness Assessment", ylab="Frequency", xlab="Grade", col='cyan')

#### treating missing values ####

DataFull<-Data[complete.cases(Data),]
DataIncomplete<-Data[-complete.cases(Data),]

t.test(DataFull2$self_empl, DataIncomplete$self_empl[complete.cases(DataIncomplete$self_empl)])

# age education partner child significant -> mean imputation



#### Grouping Happiness ####

#quantile(DataFull$happy2, c(.2, .4, .6, .8, 1))
Happiness<-rep(0, length(DataFull$happy2))
for (i in 1:length(DataFull$happy2)){
  if (DataFull$happy2[i]<(mean(DataFull$happy2)-2*sd(DataFull$happy2))){
    Happiness[i]<-1
  }
  if (DataFull$happy2[i]<(mean(DataFull$happy2)-sd(DataFull$happy2)) &
      DataFull$happy2[i]>(mean(DataFull$happy2)-2*sd(DataFull$happy2))){
    Happiness[i]<-2
  }
  if (DataFull$happy2[i]>(mean(DataFull$happy2)-sd(DataFull$happy2)) &
      DataFull$happy2[i]<(mean(DataFull$happy2))){
    Happiness[i]<-3
  }
  if (DataFull$happy2[i]>(mean(DataFull$happy2)) &
      DataFull$happy2[i]<(mean(DataFull$happy2)+sd(DataFull$happy2))){
    Happiness[i]<-4
  }
  if (DataFull$happy2[i]>(mean(DataFull$happy2)+sd(DataFull$happy2))){
    Happiness[i]<-5
  }
}

DataFull$happy2<-Happiness

DataFull<-DataFull[,-1]
DataFull<-DataFull[,-12]
DataFull[,12]<-as.numeric(paste(DataFull[,12]))

#### Training and Test set dividing ####

intrain <- createDataPartition(y = DataFull$happy2, p= 0.7, list = FALSE)
training <- DataFull[intrain,]
testing <- DataFull[-intrain,]

attrtrain<-training[,2:42]
attrtrain2<-attrtrain[,(colSums(attrtrain)!=0)]
training<-cbind(training$happy2, attrtrain2)
names(training)[1]<-'happy2'
attrtest<-testing[,2:42]
attrtest<-attrtest[,(colSums(attrtrain)!=0)]
testing<-cbind(testing$happy2, attrtest)
names(testing)[1]<-'happy2'

#### Multinomial Logit Model ####


datalog<-mlogit.data(training, shape="wide", choice="happy2") # create long format from wide format
f <-mFormula(happy2 ~ 1 | unempl_self+unempl_part+income+male+age+education+healt+part_pres
             +child+self_empl+unempl_reg+unempl_reg*unempl_self+unempl_part*unempl_self
             +unempl_part*unempl_reg+unempl_self*male+unempl_self*healt
             +alpha2_AL
             +alpha2_AT+alpha2_BE+alpha2_BG
             +alpha2_CH+alpha2_CY+alpha2_CZ+alpha2_DE+alpha2_DK+alpha2_EE+alpha2_ES+alpha2_FI
             +alpha2_FR+alpha2_GB+alpha2_HU+alpha2_IE+alpha2_IL+alpha2_IS+alpha2_IT+alpha2_LT
             +alpha2_LT+alpha2_NL+alpha2_NO
             +alpha2_PL+alpha2_PT+alpha2_RU
             +alpha2_SE+alpha2_SI
             +alpha2_SK+alpha2_UA+alpha2_XK) # define formula for MNL model
log<-mnlogit(formula=f, data=datalog, ftol = 1e-6, gtol = 1e-6, linDepTol=1e-50, print.level=1,
             na.rm=TRUE) # fit MNL

# make predictions

datalogtest<-mlogit.data(testing, shape="wide", choice="happy2") # create long format from wide format
prediction<-predict(log, newdata=datalogtest, na.rm=TRUE)
pred<-rep(0,length(prediction[,1]))
for (i in 1:length(prediction[,1])){
  pred[i]<-names(which.max(prediction[i,]))
}
pred<-as.numeric(pred)

conf_MNL<-confusionMatrix(as.factor(pred), as.factor(testing[,1])) # create confusion matrix

roc_MNL<-multiclass.roc(as.numeric(pred), as.numeric(testing[,1])) # define ROC cyrves and AuC

# hypothesis testing

f2 <-mFormula(happy2 ~ 1 | unempl_self+unempl_part+income+male+age+education+healt+part_pres
             +child+self_empl+unempl_reg+unempl_self*unempl_reg
             +unempl_part*unempl_self
             +unempl_part*unempl_reg
             +unempl_self*male
             #+unempl_self*healt
             +alpha2_AL
             +alpha2_AT+alpha2_BE+alpha2_BG
             +alpha2_CH+alpha2_CY+alpha2_CZ+alpha2_DE+alpha2_DK+alpha2_EE+alpha2_ES+alpha2_FI
             +alpha2_FR+alpha2_GB+alpha2_HU+alpha2_IE+alpha2_IL+alpha2_IS+alpha2_IT+alpha2_LT
             +alpha2_LT+alpha2_NL+alpha2_NO
             +alpha2_PL+alpha2_PT+alpha2_RU
             +alpha2_SE+alpha2_SI
             +alpha2_SK+alpha2_UA+alpha2_XK) # define the MNL formula for the restricted model
log2<-mnlogit(formula=f2, data=datalog, ftol = 1e-6, gtol = 1e-6, linDepTol=1e-50, print.level=1,
             na.rm=TRUE) # fit the MNL
hyp<-lrtest(log, log2)


#### Support Vector Machine ####

interaction<-matrix(c(t(training$unempl_self*training$unempl_part), t(training$unempl_self*training$unempl_reg),
               t(training$unempl_part*training$unempl_reg), t(training$unempl_self*training$male),
               t(training$unempl_self*training$healt)),ncol=5)
names(interaction)<-c('selfpart', 'selfreg', 'partreg', 'selfmale', 'partmale')
trainingsvm<-cbind(training[,1],scale(training[,2:length(training[1,])]), scale(interaction))
colnames(trainingsvm)[1]<-'happy2'
trainingsvm<-trainingsvm[, colSums(is.na(trainingsvm)) != nrow(trainingsvm)]
trainingsvm[,1]<-as.integer(trainingsvm[,1])

svm_Linear <- svm(happy2 ~., data = trainingsvm, kernel='linear',
                  cost=1, type='C-classification') # fit different svm kernels, choose best one
svm_Radial <- svm(happy2 ~., data = trainingsvm, kernel='radial',
                  cost=1, type='C-classification')
svm_Poly <- svm(happy2 ~., data = trainingsvm, kernel='polynomial',
                  cost=1, type='C-classification')
svm_sig <- svm(happy2 ~., data = trainingsvm, kernel='sigmoid',
                cost=1, type='C-classification')

interactiontest<-matrix(c(t(testing$unempl_self*testing$unempl_part), t(testing$unempl_self*testing$unempl_reg),
                      t(testing$unempl_part*testing$unempl_reg), t(testing$unempl_self*testing$male),
                      t(testing$unempl_self*testing$healt)),ncol=5)
names(interactiontest)<-c('selfpart', 'selfreg', 'partreg', 'selfmale', 'partmale')

testingsvm<-cbind(testing[,1],scale(testing[,2:length(training[1,])]), scale(interactiontest))
testingsvm<-testingsvm[, colSums(is.na(testingsvm)) != nrow(testingsvm)]
test_pred_SVM_rad <- predict(svm_Radial, newdata = testingsvm)
conf_SVM_rad<-confusionMatrix(test_pred_SVM_rad, as.factor(testingsvm[,1])) # confusion matrix for SVM
roc_SVM<-multiclass.roc(as.numeric(test_pred_SVM_rad), as.numeric(testingsvm[,1])) # roc curve and AuC


#### Random Forest ####

trainingRF<-cbind(training, interaction)
names(trainingRF)[40:44]<-c('selfpart', 'selfreg', 'partreg', 'selfmale', 'partmale')
trainingRF[,1]<-as.factor(trainingRF[,1])
RF<-randomForest(happy2~., data=trainingRF, na.action=na.fail)
testingRF<-cbind(testing, interactiontest)
names(testingRF)[40:44]<-c('selfpart', 'selfreg', 'partreg', 'selfmale', 'partmale')
testingRF[,1]<-as.factor(testingRF[,1])
test_pred_RF<-predict(RF, newdata=testingRF)
conf_RF<-confusionMatrix(test_pred_RF, as.factor(testingRF[,1]))

# create variable importance plot
varImpPlot(RF, sort=TRUE)

# create roc curves and AuC
roc_RF<-multiclass.roc(as.numeric(test_pred_RF), as.numeric(testingRF[,1]))

# create partial dependence plot
partialPlot(RF, pred.data=trainingRF, x.var='age', which.class=1, col='blue', main='Partial Dependence Plot of Age for Class 1')
partialPlot(RF, pred.data=trainingRF, x.var='age', which.class=5, col='blue', main='Partial Dependence Plot of Age for Class 5')
