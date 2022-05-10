##Ridge, Lasso, Elastnet Example
install.packages('MASS')
install.packages('glmnet')
library(MASS)
library(glmnet)

#Generate data-Example1
set.seed(1877)
n=1000
p=5000
real_p=15
x=matrix(rnorm(n*p),nrow=n,ncol=p)
y=apply(x[,1:real_p],1,sum)+rnorm(n)
y
#Split data into train and test
train_rows=sample(1:n,0.66*n)
x.train=x[train_rows,]
x.test=x[-train_rows,]
y.train=y[train_rows]
y.test=y[-train_rows]

#Fit models
#(For plots on left):
fit.lasso=glmnet(x.train,y.train,family="gaussian",alpha=1)
fit.ridge=glmnet(x.train,y.train,family='gaussian',alpha=0)
fit.elnet=glmnet(x.train,y.train,family='gaussian',alpha=0.5)
s
#10-fold CV for each alpha=0,0.1,,,,,
for(i in 0:10){
  assign(paste('fit',i,sep=""),cv.glmnet(x.train,y.train,type.measure="mse",alpha=i/10,family='gaussian'))
}

#Plot solution paths:
par(mfrow=c(3,2))
plot(fit.lasso,xvar='lambda')
plot(fit.ridge,xvar='lambda')
plot(fit.elnet,xvar='lambda')
yhat0=predict(fit0,s=fit0$lambda.1se,newx=x.test)
yhat1=predict(fit1,s=fit1$lambda.1se,newx=x.test)
yhat2=predict(fit2,s=fit2$lambda.1se,newx=x.test)
yhat3=predict(fit3,s=fit3$lambda.1se,newx=x.test)
yhat4=predict(fit4,s=fit4$lambda.1se,newx=x.test)
yhat5=predict(fit5,s=fit5$lambda.1se,newx=x.test)
yhat6=predict(fit6,s=fit6$lambda.1se,newx=x.test)
yhat7=predict(fit7,s=fit7$lambda.1se,newx=x.test)
yhat8=predict(fit8,s=fit8$lambda.1se,newx=x.test)
yhat9=predict(fit9,s=fit9$lambda.1se,newx=x.test)
yhat10=predict(fit10,s=fit10$lambda.1se,newx=x.test)

(mse0=mean((y.test-yhat0)^2))
mse1=mean((y.test-yhat1)^2)
mse2=mean((y.test-yhat2)^2)
mse3=mean((y.test-yhat3)^2)
mse4=mean((y.test-yhat4)^2)
(mse5=mean((y.test-yhat5)^2))
mse6=mean((y.test-yhat6)^2)
mse7=mean((y.test-yhat7)^2)
mse8=mean((y.test-yhat8)^2)
mse9=mean((y.test-yhat9)^2)
(mse10=mean((y.test-yhat10)^2))

#Lasso is the winner!

##----------example2-----------------

#generate data
set.seed(2022)
n=1000
p=5000
real_p=1500
x=matrix(rnorm(n*p),nrow=n,ncol=p)
y=apply(x[,1:real_p],1,sum)+rnorm(n)

#split data into train and test sets
train_rows=sample(1:n,0.66*n) #1부터 n사이의 값에서 0.66n개 만큼 추출
x.train=x[train_rows,]
x.test=x[-train_rows,]
y.train=y[train_rows]
y.test=y[-train_rows]


#Fit models
#(For plots on left):
fit.lasso=glmnet(x.train,y.train,family="gaussian",alpha=1)
fit.ridge=glmnet(x.train,y.train,family='gaussian',alpha=0)
fit.elnet=glmnet(x.train,y.train,family='gaussian',alpha=0.5)

#10-fold CV for each alpha=0,0.1,,,,,
for(i in 0:10){
  assign(paste('fit',i,sep=""),cv.glmnet(x.train,y.train,type.measure="mse",alpha=i/10,family='gaussian'))
}

#Plot solution paths:
par(mfrow=c(3,2))
plot(fit.lasso,xvar='lambda')
plot(fit.ridge,xvar='lambda')
plot(fit.elnet,xvar='lambda')
yhat0=predict(fit0,s=fit0$lambda.1se,newx=x.test)
yhat1=predict(fit1,s=fit1$lambda.1se,newx=x.test)
yhat2=predict(fit2,s=fit2$lambda.1se,newx=x.test)
yhat3=predict(fit3,s=fit3$lambda.1se,newx=x.test)
yhat4=predict(fit4,s=fit4$lambda.1se,newx=x.test)
yhat5=predict(fit5,s=fit5$lambda.1se,newx=x.test)
yhat6=predict(fit6,s=fit6$lambda.1se,newx=x.test)
yhat7=predict(fit7,s=fit7$lambda.1se,newx=x.test)
yhat8=predict(fit8,s=fit8$lambda.1se,newx=x.test)
yhat9=predict(fit9,s=fit9$lambda.1se,newx=x.test)
yhat10=predict(fit10,s=fit10$lambda.1se,newx=x.test)

(mse0=mean((y.test-yhat0)^2))
mse1=mean((y.test-yhat1)^2)
mse2=mean((y.test-yhat2)^2)
mse3=mean((y.test-yhat3)^2)
mse4=mean((y.test-yhat4)^2)
(mse5=mean((y.test-yhat5)^2))
mse6=mean((y.test-yhat6)^2)
mse7=mean((y.test-yhat7)^2)
mse8=mean((y.test-yhat8)^2)
mse9=mean((y.test-yhat9)^2)
(mse10=mean((y.test-yhat10)^2))

#Ridge is the winner!

##--------example3----------------------##

#Generate data
set.seed(1998)
n=100
p=50
covmatrix=outer(1:p,1:p,function(x,y){0.7^abs(x-y)}) 
covmatrix
x=mvrnorm(n,rep(0,p),covmatrix) #다변수 정규분포를 가지는 데이터셋 (샘플크기 : n / 평균 : rep(0,p) / 분산-공분산 행렬)
dim(x)
y=10*apply(x[,1:2],1,sum)+apply(x[,5:14],1,sum)+rnorm(n)

#split data into train and test sets
train_rows=sample(1:n,0.66*n)
x.train=x[train_rows,]
x.test=x[-train_rows,]
y.train=y[train_rows]
y.test=y[-train_rows]

#Fit models
fit.lasso=glmnet(x.train,y.train,family='gaussian',alpha=1)
fit.ridge=glmnet(x.train,y.train,family='gaussian',alpha=0)
fit.elnet=glmnet(x.train,y.train,family='gaussian',alpha=0.5)

#10-fold CV for each alpha
for(i in 0:10){
  assign(paste('fit',i,sep=""),cv.glmnet(x.train,y.train,type.measure="mse",alpha=i/10,family='gaussian'))
}

#Plot solution paths:
par(mfrow=c(3,2))
plot(fit.lasso,xvar='lambda')
plot(fit.ridge,xvar='lambda')
plot(fit.elnet,xvar='lambda')
yhat0=predict(fit0,s=fit0$lambda.1se,newx=x.test)
yhat1=predict(fit1,s=fit1$lambda.1se,newx=x.test)
yhat2=predict(fit2,s=fit2$lambda.1se,newx=x.test)
yhat3=predict(fit3,s=fit3$lambda.1se,newx=x.test)
yhat4=predict(fit4,s=fit4$lambda.1se,newx=x.test)
yhat5=predict(fit5,s=fit5$lambda.1se,newx=x.test)
yhat6=predict(fit6,s=fit6$lambda.1se,newx=x.test)
yhat7=predict(fit7,s=fit7$lambda.1se,newx=x.test)
yhat8=predict(fit8,s=fit8$lambda.1se,newx=x.test)
yhat9=predict(fit9,s=fit9$lambda.1se,newx=x.test)
yhat10=predict(fit10,s=fit10$lambda.1se,newx=x.test)

(mse0=mean((y.test-yhat0)^2))
mse1=mean((y.test-yhat1)^2)
mse2=mean((y.test-yhat2)^2)
mse3=mean((y.test-yhat3)^2)
mse4=mean((y.test-yhat4)^2)
(mse5=mean((y.test-yhat5)^2))
mse6=mean((y.test-yhat6)^2)
mse7=mean((y.test-yhat7)^2)
mse8=mean((y.test-yhat8)^2)
mse9=mean((y.test-yhat9)^2)
(mse10=mean((y.test-yhat10)^2))

#elnet is the winner!

##---------another example-----------##
install.packages('tidyverse')
library(tidyverse)
install.packages('caret')
library(caret)
install.packages('glmnet')
library(glmnet)
#Glmnet은 패널티 최대 우도를 통해서 일반화 선형 모델을 적합하는 패키지

data('Boston',package = 'MASS')
help(Boston)
#set a seed so you can reproduce the result
set.seed(1212)

#split the data into training and test data
sample_size=floor(0.75*nrow(Boston))
training_index=sample(seq_len(nrow(Boston)),size=sample_size)
training_index
train=Boston[training_index,]
test=Boston[-training_index,]

#Predictor
x=model.matrix(medv~.,train)[,-1]
#Response
y=train$medv

cor(x)
ols=lm(y~x)
summary(ols)

#Performing Ridge regression
#lambda는 영향을 많이 끼치므로 최적값을 찾아야할것이다.
model.ridge0=glmnet(x,y,alpha=0)
coef(model.ridge0)
plot(model.ridge0,xvar='lambda')
plot(model.ridge0,main='Ridge')

cv.r=cv.glmnet(x,y,alpha=0)
cv.r$lambda.min
model.ridge=glmnet(x,y,alpha=0,lambda=cv.r$lambda.min)
coef(model.ridge)
#우리는 여기서 ridge는 중요하지 않은 변수들은 0과 가깝게 만들어줌을 알 수있었다.
x.test.ridge=model.matrix(medv~.,test)[,-1]
predictions.ridge=model.ridge %>% predict(x.test.ridge) %>% as.vector()
data.frame(
  RMSE.r=RMSE(predictions.ridge,test$medv),
  Rsquare.r=R2(predictions.ridge,test$medv)
)
#Performing lasso regression
model.lasso0=glmnet(x,y,alpha=1)
plot(model.lasso0,xvar='lambda')
plot(model.lasso0,main='LASSO')
cv.l=cv.glmnet(x,y,alpha=1)
cv.l$lambda.min
model.lasso=glmnet(x,y,alpha=1,lambda=cv.l$lambda.min)
coef(model.lasso)
x.test.lasso=model.matrix(medv~.,test)[,-1]
predictions.lasso=model.lasso %>% predict(x.test.lasso) %>% as.vector()
data.frame(
  RMSE.l=RMSE(predictions.lasso,test$medv),
  Rsquare1=R2(predictions.lasso,test$medv)
)
predictions.lasso
#---Performing Elastic Net regression---#
#elasticnet에서 alpha값과 lambda최적값을 찾으려면 caret패키지를 사용해야한다. 
model.net=train(
  medv~.,data=train,method='glmnet',
  trControl=trainControl("cv",number=10),
  tuneLength=10 #cross-validation으로 최적의 parameter를 찾는다.
)
model.net$bestTune
coef(model.net$finalModel,model.net$bestTune$lambda)
x.test.net=model.matrix(medv~.,test)[,-1]
predictions.net=model.net %>% predict(x.test.net) %>% as.vector()

data.frame(
RMSE.net=RMSE(predictions.net,test$medv)
Rsquare.net=R2(as.numeric(predictions.net),as.numeric(test$mdev))
)