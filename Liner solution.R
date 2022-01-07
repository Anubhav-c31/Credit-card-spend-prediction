#_________linear regression on Credit Card Data ________________________

# Objective - We are looking to find the KPI [Key performance Indicators] of the total spend of credit card(Primary Card + Secondary card)

# Clear environment
rm(list = ls())
# Get File
setwd("C:\\Users\\anubh\\Desktop\\BA360\\R\\Day 14\\Regression Case Studies - Linear & Logistic\\Linear Regression Case")
library(readxl)
credit_data<-read_excel("Linear Regression Case.xlsx", sheet = 1)
View(credit_data)

# Understanding Data
dim(credit_data)
names(credit_data)
str(credit_data)

# Deciding Dependent variable
credit_data$total_spend=credit_data$cardspent+credit_data$card2spent

#________________________________________________________________-
# Step 1 - Exploratory Data Analysis
# _____________________________________________________________

# user written function for creating descriptive statistics
mystats_num <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  p1<-quantile(a,0.01)
  p5<-quantile(a,0.05)
  p10<-quantile(a,0.10)
  q1<-quantile(a,0.25)
  q2<-quantile(a,0.5)
  q3<-quantile(a,0.75)
  p90<-quantile(a,0.90)
  p95<-quantile(a,0.95)
  p99<-quantile(a,0.99)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, nmiss=nmiss, outlier_flag=outlier_flag, mean=m, 
           stdev=s,min = min, p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,
           q3=q3,p90=p90,p95=p95,p99=p99,max=max, UC=UC, LC=LC ))
}

mystats_cat <- function(y) {
  
  Var_Type=class(y)
  n<-length(y)
  nmiss<-sum(is.na(y))
  return(c(Var_Type=Var_Type, n=n,nmiss=nmiss))
  
}


# ------------------------------------------------------------
names(credit_data) # this function will give me the names of all the variables
library(dplyr)

# drop redundant columns
cred_data1 = subset(credit_data,select = -c(cardspent,card2spent,custid,birthmonth))

# Separation of Categorical and Numerical variables.
cat_vars=subset(cred_data1,select = c(region,townsize,gender,agecat,edcat,jobcat,union,
                                empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                homeown,hometype,addresscat,carown,cartype,
                                carcatvalue,carbought,carbuy,commutecat,commutecar,
                                commutemotorcycle,commutecarpool,commutebus,commuterail,
                                commutepublic,commutebike,commutewalk,commutenonmotor,
                                telecommute,reason,polview,polparty,polcontrib,vote,card,
                                cardtype,cardbenefit,cardfee,cardtenurecat,card2,
                                card2type,card2benefit,card2fee,card2tenurecat,
                                active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                voice,pager,internet,callid,callwait,forward,confer,ebill,
                                owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                news,response_01,response_02,response_03))

num_vars=subset(cred_data1,select = -c(region,townsize,gender,agecat,edcat,jobcat,union,
                                      empcat,retire,inccat,default,jobsat,marital,spousedcat,
                                      homeown,hometype,addresscat,carown,cartype,
                                      carcatvalue,carbought,carbuy,commutecat,commutecar,
                                      commutemotorcycle,commutecarpool,commutebus,commuterail,
                                      commutepublic,commutebike,commutewalk,commutenonmotor,
                                      telecommute,reason,polview,polparty,polcontrib,vote,card,
                                      cardtype,cardbenefit,cardfee,cardtenurecat,card2,
                                      card2type,card2benefit,card2fee,card2tenurecat,
                                      active,bfast,churn,tollfree,equip,callcard,wireless,multline,
                                      voice,pager,internet,callid,callwait,forward,confer,ebill,
                                      owntv,ownvcr,owndvd,owncd,ownpda,ownpc,ownipod,owngame,ownfax,
                                      news,response_01,response_02,response_03))


num_data <- apply(num_vars, 2, mystats_num)

cat_data <- apply(cat_vars,2,mystats_cat)


diag_stats_num <- t(data.frame(num_data))
write.csv(diag_stats_num,"diag_stat_num.csv")
diag_stats_cat <- t(data.frame(cat_data))
write.csv(diag_stats_cat,"diag_stat_cat.csv")

## Missing value + outlier (Data Preparation)

## Missing value treatment
num_vars<- num_vars[!is.na(num_vars$total_spend),] # dropping obs where DV=missing
num_vars<-num_vars[,which(colMeans(!is.na(num_vars))>0.5)] # dropping columns with more then 50% missing values
str(num_vars_m)
require(Hmisc)
View(num_vars_na)
#Imputing missings with mean for IV's
num_vars_na <- data.frame(apply(num_vars,2, function(x) impute(x, mean))) 
cat_vars_na <- data.frame(apply(cat_vars,2, function(x) impute(x, mode))) 


# Outlier 

outlier = function(x){
  m <- mean(x)
  s <- sd(x)
  UC <- m+3*s 
  LC <- m-3*s
  x[x>UC]=UC
  x[x<LC]=LC
  return(x)
}
num_vars_na <- apply(num_vars_na,2,outlier)
my_data<-cbind(cat_vars_na,num_vars_na)
str(my_data)


# Checking distribution of Dependent variable

hist(my_data$total_spend)
hist(log(my_data$total_spend))
my_data$ln_spend<-log(my_data$total_spend)


#Splitting data into [Training] and [Testing] Dataset

set.seed(999)

train_ind <- sample(1:nrow(my_data), size = floor(0.70 * nrow(my_data)))

my_data[train_ind,]

length(train_ind)

training<-my_data[train_ind,]
testing <-my_data[-train_ind,]

#Factor Analysis

num_my_data=select_if(training,is.numeric)
str(num_my_data)
cat_my_data=select_if(training,is.factor)
cat_my_data$ln_spend=num_my_data$ln_spend
str(cat_my_data)

# Correlation
c <- cor(num_my_data)
require(corrplot)
corrplot(cor(num_my_data,use="pairwise.complete.obs"), method="circle", tl.cex = 0.2)
write.csv(c, file = "corrm.csv")


# Independent categorical variable
# Anova Testing (P_value<0.05)
c_fit=aov(ln_spend~.,data=cat_my_data)
summary(c_fit)
#region+gender+agecat+edcat+jobcat+empcat+retire+inccat+addresscat+commuterail+reason+card+card2+churn+ebill+ownvcr+owndvd+owncd

# Independent numerical variable
n_fit=lm(ln_spend~.,data=num_my_data)
options(scipen=999)
summary(n_fit)

step= stepAIC(n_fit,direction = "both")

#ln_spend ~ income + lninc + pets_dogs + address + carditems + card2items + tollten + wiremon + hourstv 


## Final Modelling

final_fit=lm(ln_spend ~ income + lninc + pets_dogs + address + carditems + card2items + tollten + wiremon + hourstv  +
               region+gender+agecat+edcat+jobcat+empcat+retire+inccat+addresscat+commuterail+reason+
               card+card2+churn+ebill+ownvcr+owndvd+owncd
             ,
             data=training

)
step= stepAIC(final_fit,direction = "both")

fit = lm (ln_spend ~ lninc + address + carditems + card2items + wiremon + 
            gender + edcat + jobcat + commuterail + reason + card + card2 + 
            churn,
          data=training
)

summary(fit)

# Check for multicollinearity
library(car)
vif(fit)

# MODEL VALIDATION STEPS :

####################### SCORING USING PREDICT FUNCTION

t1<-cbind(training, pred_sales = exp(predict(fit,training)))
names(t1)

t1<- transform(t1, APE = abs(pred_sales - total_spend)/total_spend)
mean(t1$APE)
View(t1)

t2<-cbind(testing, pred_sales=exp(predict(fit,testing)))
t2<- transform(t2, APE = abs(pred_sales - total_spend)/total_spend)

mean(t2$APE)
View(t2)

################################## Decile Analysis Reports - t1(training)

# find the decile locations 
decLocations <- quantile(t1$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t1$decile <- findInterval(t1$pred_sales,c(-Inf,decLocations, Inf))
View(t1)
require(sqldf)
t1_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(total_spend) as avg_Actual_sales
               from t1
               group by decile
               order by decile desc")

View(t1_DA)
write.csv(t1_DA,"train_decile_analysis.csv")


##################################Decile Analysis Reports - t2(testing)

# find the decile locations 
decLocations <- quantile(t2$pred_sales, probs = seq(0.1,0.9,by=0.1))

# use findInterval with -Inf and Inf as upper and lower bounds
t2$decile <- findInterval(t2$pred_sales,c(-Inf,decLocations, Inf))

require(sqldf)
t2_DA <- sqldf("select decile, count(decile) as count, avg(pred_sales) as avg_pre_sales,   
               avg(total_spend) as avg_Actual_sales
               from t2
               group by decile
               order by decile desc")

View(t2_DA)
write.csv(t2_DA,"test_decile_analysis.csv")

#################################################################################################

# ANSWER:
# Following Variables are major drivers of total credit card spend.
# lninc , address , carditems , card2items , wiremon , 
# gender , edcat , jobcat , commuterail , reason , card , card2 , 
# churn





