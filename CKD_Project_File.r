
## Read in Data


data=read.csv("casestudydata.csv")
names(data)
data=data[,-1]  ## remove ID

## Explore and relabel Data


y=data$CKD
out_sample=which(is.na(data$CKD)==1)
data_out=data[out_sample,]   ## the ones without a disease status
data_in=data[-out_sample,]   ## the ones with a disease status
summary(data_in)


##impute data

install.packages("mice")
library(mice)
imputed <- mice(data_in,m=5,meth='pmm')  #m=5 generates 5 different imputations
imputed_in <- complete(imputed,3) #choose the 3rd imputation. can choose whichever out of 5

## Remove 2 obs with missing value in CareSource - can't impute for categorical variable

which(imputed_in$CareSource==" ")
imputed_in=imputed_in[-c(925, 5934),]
dim(imputed_in)
summary(imputed_in)

imputed_in$CareSource=factor(imputed_in$CareSource)

training =read.csv("train.csv")

validation =read.csv("validation.csv")

model=glm(CKD~Age+Female+PVD+Hypertension+Diabetes+CVD+Anemia +Racegrp + CHF,family="binomial",data=train)

validation= subset(validation, select = c("Age", "Female", "PVD", "Hypertension",  "Diabetes", "CVD", "Anemia", "Racegrp",  "CHF", "CKD"))

phat=predict(model, validation[, 1:9], type="response")


classify=ifelse(phat>.22,1,0)  #change prediction var & threshold to see
summary(classify)  # notice that not many are "yes"  - is this desirable?

c_accuracy(validation$CKD,classify)  # to run this you must run my code below first.
cc=c_accuracy(validation$CKD,classify)
round(cc,5)

## Caclculate Costs & profit
acc=c_accuracy(validation$CKD,classify)
c1=1300   # earn $1300 for a true positive
c2=100  #  penalize me $100 for a false positive
cost = acc[9]*c2
revenue = acc[7]*c1
profit=revenue-cost

cost
revenue
profit   

## YOu must change the threshold of 50% above , to lower your costs.
##    How to do this?  One way is to search over 101 thresholds (each % from 0 to 100)

##  you may realize at some point, that plotting an ROC curve with roc()  gives you all possibilities
install.packages('pROC')
library(pROC)
pROC(validation$CKD,phat)
model_ROC=roc(validation$CKD~phat,percent=TRUE,plot=TRUE)

## AUC is actually the probability of ranking a randomly chosen diseased person
##    higher than a randomly chosen non-disease person   (using the predictions from your model)

## cutoff value vs cost/profit (copy from Neel's code and modify cost part)
i=1
k=0.01 ## cutt off value
profit ={}
cost={}
tpr={}
fpr={}
accuracy={}
m={}

## run a while loop for cuttoff values 0.01 to 1

while( k <= 1){  
  classify=ifelse(phat> k,1,0) 
  acc=c_accuracy(validation$CKD,classify)
  c1=1300   
  c2=100  
  
  profit[i]= acc[7]*c1-acc[9]*c2 #care about net-profit only
  
  tpr[i] = acc[4]
  fpr[i]=acc[5]
  accuracy[i]= acc[3]
  m[i]=k
  
  
  
  k=k+0.01 ## increment the cuttoff value by 0.01
  
  i=i+1 ## i represents the index of the cutoff value. 
  
  
  
}

plot(m, accuracy, xlab="cutoff value", main = "Cut off value vs Accuracy")
lines(tpr)



## Function Below, RUN THIS FIRST
## make sure actuals and classifications are 0 (no) or 1 (yes) only 
##  Built by Matthew J. Schneider

c_accuracy=function(actuals,classifications){
  df=data.frame(actuals,classifications);
  
  
  tp=nrow(df[df$classifications==1 & df$actuals==1,]);        
  fp=nrow(df[df$classifications==1 & df$actuals==0,]);
  fn=nrow(df[df$classifications==0 & df$actuals==1,]);
  tn=nrow(df[df$classifications==0 & df$actuals==0,]); 
  
  
  recall=tp/(tp+fn)
  precision=tp/(tp+fp)
  accuracy=(tp+tn)/(tp+fn+fp+tn)
  tpr=recall
  fpr=fp/(fp+tn)
  fmeasure=2*precision*recall/(precision+recall)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,tp,tn,fp,fn)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","tp","tn","fp","fn")
  
  #print(scores)
  return(scores);
}
