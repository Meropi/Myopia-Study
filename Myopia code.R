## We read the data and use str to understand their structure:

library(ggplot2)
myopiadata<-read.table ("myopia.csv", header=TRUE, sep=";")
str(myopiadata)

#Initial Transformations

myopiadata$HEREDITY<-FALSE
myopiadata[myopiadata$MOMMY==1 & myopiadata$DADMY==1,19]<-"BOTH"
myopiadata[myopiadata$MOMMY==1 & myopiadata$DADMY==0,19]<-"ONE"
myopiadata[myopiadata$MOMMY==0 & myopiadata$DADMY==1,19]<-"ONE"
myopiadata[myopiadata$MOMMY==0 & myopiadata$DADMY==0,19]<-"NONE"

myopiadata$HEREDITY<-as.factor(myopiadata$HEREDITY)

#conversion to factors
myopiadata$DADMY<-as.factor(myopiadata$DADMY)
myopiadata$MOMMY<-as.factor(myopiadata$MOMMY)
myopiadata$MYOPIC<-as.factor(myopiadata$MYOPIC)
myopiadata$GENDER<-as.factor(myopiadata$GENDER)

#conversion to numeric 
myopiadata$STUDYYEAR<-as.numeric(myopiadata$STUDYYEAR)
myopiadata$AGE<-as.numeric(myopiadata$AGE)
myopiadata$SPORTHR<-as.numeric(myopiadata$SPORTHR)
myopiadata$READHR<-as.numeric(myopiadata$READHR)
myopiadata$COMPHR<-as.numeric(myopiadata$COMPHR)
myopiadata$STUDYHR<-as.numeric(myopiadata$STUDYHR)
myopiadata$TVHR<-as.numeric(myopiadata$TVHR)
myopiadata$DIOPTERHR<-as.numeric(myopiadata$DIOPTERHR)

myopic<-myopiadata[,c("MYOPIC","GENDER", "HEREDITY", "AGE", "STUDYYEAR","SPHEQ","AL", "ACD","LT", "VCD", "SPORTHR", "READHR","COMPHR", "STUDYHR", "TVHR")]

index <- sapply(myopic, class) == "numeric"
myopicnum<-myopic[,index]
summary(myopic)
n <- nrow(myopicnum)
par(mfrow=c(2,3)); 
#Histograms
for(i in 3:12){
  hist(myopicnum[,i], main=names(myopicnum[i]), probability=TRUE)
  lines(density(myopicnum[,i]), col=2)
  ind <- seq(min(myopicnum[,i]), max(myopicnum[,i]), length.out=100)
  ynorm<-dnorm(ind, mean=mean(myopicnum[,i]), sd(myopicnum[,i]))
  lines(ind, ynorm, col=3, lty=3, lwd=3)
}
#Relative frequencies
plot(table(myopicnum[,1])/n, type='h', xlim=range(myopicnum[,1])+c(-1,1), main=names(myopicnum)[1], ylab='Relative frequency')
plot(table(myopicnum[,2])/n, type='h', xlim=range(myopicnum[,2])+c(-1,1), main=names(myopicnum)[2], ylab='Relative frequency')

#Barplots
par(mfrow=c(1,3));

myopicfac3<-c("MYOPIC")
myopicfac3<-as.data.frame(myopic[,myopicfac3])
names(myopicfac3)<-"MYOPIC"
barplot(sapply(myopicfac3,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=0.7)
legend('center', fil=2:3, legend=c('Not Myopic','Myopic'), ncol=1, bty="n", cex=1)

myopicfac1<-c("HEREDITY")
myopicfac1<-as.data.frame(myopic[,myopicfac1])
names(myopicfac1)<-"HEREDITY"
barplot(sapply(myopicfac1,table)/n, horiz=T, las=1, col=2:4, ylim=c(0,8), cex.names=0.7)
legend('center', fil=2:5, legend=c('Both','None','One'), ncol=1,bty='n', cex=1)

myopicfac2<-c("GENDER")
myopicfac2<-as.data.frame(myopic[,myopicfac2])
names(myopicfac2)<-"GENDER"
barplot(sapply(myopicfac2,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8), cex.names=0.7)
legend('center', fil=2:3, legend=c('Male','Female'), ncol=1,bty='n', cex=1)

#Scatterplots
par(mfrow = c(1,1))
require(corrplot)
corrplot(cor(myopicnum[,3:7]), method="number")

#Pearson's correlation coefficent
pairs(myopicnum[,3:7])

#Boxplots
par(mfrow=c(2,3));
boxplot(myopic$SPORTHR ~ myopic$MYOPIC,
        data = myopic,
        main = "SPORTHR vs Myopic",
        xlab = "Myopic",
        ylab = "SPORTHR",
        col = c("lightseagreen") )
boxplot(myopic$READHR ~ myopic$MYOPIC,
        data = myopic,
        main = "READHR vs Myopic",
        xlab = "Myopic",
        ylab = "READHR",
        col = c("lightsalmon") )
boxplot(myopic$COMPHR ~ myopic$MYOPIC,
        data = myopic,
        main = "COMPHR vs Myopic",
        xlab = "Myopic",
        ylab = "COMPHR",
        col = c("mediumspringgreen") )
boxplot(myopic$STUDYHR ~ myopic$MYOPIC,
        data = myopic,
        main = "STUDYHR vs Myopic",
        xlab = "Myopic",
        ylab = "STUDYHR",
        col = c("mediumorchid") )
boxplot(myopic$TVHR ~ myopic$MYOPIC,
        data = myopic,
        main = "TVHR vs Myopic",
        xlab = "Myopic",
        ylab = "TVHR",
        col = c("wheat1") )
boxplot(myopic$SPHEQ ~ myopic$MYOPIC,
        data = myopic,
        main = "SPHEQ vs Myopic",
        xlab = "Myopic",
        ylab = "SPHEQ",
        col = c("yellow") )
par(mfrow=c(2,3));
boxplot(myopic$AL ~ myopic$MYOPIC,
        data = myopic,
        main = "AL vs Myopic",
        xlab = "Myopic",
        ylab = "AL",
        col = c("khaki") )
boxplot(myopic$ACD ~ myopic$MYOPIC,
        data = myopic,
        main = "ACD vs Myopic",
        xlab = "Myopic",
        ylab = "ACD",
        col = c("lightcoral") )
boxplot(myopic$LT ~ myopic$MYOPIC,
        data = myopic,
        main = "LT vs Myopic",
        xlab = "Myopic",
        ylab = "LT",
        col = c("snow") )
boxplot(myopic$VCD ~ myopic$MYOPIC,
        data = myopic,
        main = "VCD vs Myopic",
        xlab = "Myopic",
        ylab = "VCD",
        col = c("snow1") )

#GGPLOTS
par(mfrow=c(2,3));

label_parsed<-c(`0`="Not Myopic", `1`="Myopic")
ggplot(data=myopic, aes(x=myopic$GENDER))+
scale_x_discrete(labels = c('Male','Female'))+
geom_bar(aes(fill=factor(myopic$MYOPIC,labels=label_parsed)), position = "identity", alpha=0.5)+
labs(fill="Myopia", x="Gender", y="Frequency")

ggplot(data=myopic, aes(x=myopic$HEREDITY))+
scale_x_discrete(labels = c('BOTH','NONE', 'ONE'))+
geom_bar(aes(fill=factor(myopic$MYOPIC,labels=label_parsed)), position = "identity", alpha=0.5)+
labs(fill="Myopia", x="Heredity", y="Frequency")

#Initial model with all the variabes (after the initial transformations)
fullmod1 <- glm(MYOPIC~GENDER+HEREDITY+AGE+STUDYYEAR+SPHEQ+AL+ACD+LT+VCD+SPORTHR+READHR+COMPHR+STUDYHR+TVHR,family=binomial, data=myopic)


#Cheking multi-collinearity
require(car)
vif(fullmod1)

#New full-model after omitting AL variable which cause multicollinearity
fullmod <- glm(MYOPIC~GENDER+HEREDITY+AGE+STUDYYEAR+SPHEQ+ACD+LT+VCD+SPORTHR+READHR+COMPHR+STUDYHR+TVHR,family=binomial, data=myopic)

#No multicollinearity anymore
vif(fullmod)

summary(fullmod)

#The null model
nullmod <- glm(MYOPIC~1,family=binomial, data=myopic)
summary(nullmod)

# #In the null model the intercept is -1.8915. This means that the log(p/(1-p)) = -1.8915
# #Let's take the frequency table of MYOPIA. p=81/618=0.131068. This means that the odds are 0.131068/(1-0.131068)=0.150838. The log(0.150838) is -1.891549 which is the intercept value.
# exp(-4.10)
# table(myopic$SPHEQ, myopic$MYOPIC)
# prop.table(table(myopic$MYOPIC))

#STEPWISE PROCEDURE
summary(step(fullmod, direction='both'))

##IN SAMPLE PREDICTIVE ABILITY TO THE MODEL DERIVED FROM STEPWISE
stepmodel<-glm(MYOPIC ~ GENDER + HEREDITY + SPHEQ + ACD + SPORTHR + READHR + STUDYHR, family = binomial, data = myopic)

summary(stepmodel)

#In-sample predictive ability
preds<-predict.glm(stepmodel, newdata = myopic[,-1], type = "response")
contrasts(myopic$MYOPIC)
glm_predict <- rep("0",nrow(myopic))

glm_predict[preds>.5] <- "1"
table(predicted = glm_predict, actual = myopic$MYOPIC)
mean(glm_predict == myopic$MYOPIC)

##IMPLEMENTING LASSO
# Lasso with glmnet
library(glmnet)
LASSO <- model.matrix(MYOPIC~., myopic)[,-1]
lambdas <- 10 ^ seq(8,-4,length=250)
hmda_models_lasso <- glmnet(LASSO, myopic$MYOPIC,alpha=1, lambda=lambdas, family="binomial")

# Plot
plot(hmda_models_lasso, xvar = "lambda", label = TRUE)

# Cross Validation
lasso.cv <- cv.glmnet(LASSO,myopic$MYOPIC, alpha=1, lambda=lambdas, family="binomial")

coef(lasso.cv, s = "lambda.min")
coef(lasso.cv, s = "lambda.1se")

# Cross Validation Plot
plot(lasso.cv)

log(lasso.cv$lambda.min)
log(lasso.cv$lambda.1se)

##IN SAMPLE PREDICTIVE ABILITY TO THE MODEL DERIVED FROM LASSO
preds <- predict(hmda_models_lasso, LASSO, type = "class", s = lasso.cv$lambda.min)

# Performance
table(predicted = preds, actual = myopic$MYOPIC)
mean(preds  == myopic2$MYOPIC)

preds2 <- predict(hmda_models_lasso, LASSO, type = "class", s = lasso.cv$lambda.1se)
table(predicted = preds2, actual = myopic2$MYOPIC)
mean(preds2== myopic2$MYOPIC)

#Taking the summary of the Final Model
finalmodel<-glm(MYOPIC~ HEREDITY+SPHEQ+SPORTHR, family = binomial, data = myopic)
summary(finalmodel)

#Final Model - changing the reference level of heredity

myopic2 <- within(myopic, HEREDITY <- relevel(HEREDITY, ref = "NONE"))

finalmodel<-glm(MYOPIC~ HEREDITY+SPHEQ+SPORTHR, family = binomial, data = myopic2)
summary(finalmodel)











