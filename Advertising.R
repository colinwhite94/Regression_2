install.packages("ggplot2")
install.packages(c("ggplot2","lmtest","normtest"),dep = TRUE)
library(MASS)
library(ggplot2) 
library(lmtest)
library(normtest)

## Read in the Data
spd = read.csv("speed_dist.txt",sep=" ")

# attach(ad)
head(spd)
summary(spd)

## Exploratory Analyses
## scatterplots
plot(spd$Speed,spd$Distance,pch=19,xlab="Speed",ylab="Dist")
with(spd,plot(Speed,Distance,pch=19,xlab="Speed",ylab="Distance"))
scatter.smooth(spd$Speed,spd$Distance)

## ggplot 
ggplot(data = spd) + geom_point(aes(x = P, y = R),col ="red")

ggplot(data = spd) + geom_point(aes(x = P, y = R),col ="red") + 
  geom_smooth(aes(P,R)) + theme_classic()

cor(spd$P,spd$R)


## Fit a Linear Model to Data
spd_lm = lm(sqrt(Distance) ~ Speed, data=spd)
summary(spd_lm)
#plot(spd_lm)


## Check Linear & Equal Variance Assumption 
plot(spd_lm$fitted.values,spd_lm$residuals,
     pch=20,ylim=c(-30,30))

abline(a=0,b=0,lwd=2,col = "red",lty = 3)

plot(spd$Speed,spd_lm$residuals,
     pch=20,ylim=c(-30,30))
abline(a=0,b=0,lwd=2,col = "red",lty = 3)
abline(h = 0,lwd=2,col = "red",lty = 2)

bptest(spd_lm)         ## Breusch-Pagan test

## Check Normality Assumption
std_res = MASS::stdres(spd_lm) ## This is accounting for more than just sigma

hist(std_res)


hist(std_res,freq = FALSE)
curve(dnorm,from = -4,to = 4,add = TRUE,
      col = "cornflowerblue",lwd = 2,lty = 4)

hist(std_res,freq = FALSE,breaks = 15)
curve(dnorm,from = -4,to = 4,add = TRUE,
      col = "green")

ggplot() + geom_density(aes(x=std_res))
ggplot() + geom_histogram(aes(x=std_res))

### q-q plot
qqnorm(std_res,pch=20)
abline(a=0,b=1,col = "red",lty = 2)

### not normal (fake data)
set.seed(2)
exp.vars = scale(rt(37,df = 4))
qqnorm(exp.vars,pch=20,main = "Not Normal Q-Q Plot")
abline(a=0,b=1)

### actually normal (fake data)
exp.vars = scale(rnorm(37))
qqnorm(exp.vars,pch=20,main = "Normal Q-Q Plot")
abline(a=0,b=1)


ks.test(std_res,"pnorm") # Kolmogorov-Smirnov test
normtest::jb.norm.test(std_res)  #Jarque-Bera test

## Check to see if there are outliers
cd = cooks.distance(spd_lm)
plot(cd,type="h",lwd = 2)
abline(h = 4/nrow(spd),col = "red")

outliers = which(cd > 4/nrow(spd))
spd[outliers,]

## Plot sqrt-transformed data
plot(sqrt(spd$Distance),(spd$Speed),pch=19,xlab="Sqrt(Distance)",ylab="(Speed)")
ggplot(spd,aes(x=sqrt(Distance),y=(Speed))) + geom_point()

## Fit a sqrt-transformed SLR Model
trans_lm = lm(sqrt(Distance)~(Speed),data=spd)
summary(trans_lm) ## be careful in interpreting these coefficients since we transformed the data

summary(trans_lm)$r.squared ## R^2 of original model

std_res = stdres(trans_lm)
ggplot() + geom_density(aes(x=std_res))
ggplot() + geom_histogram(aes(x=std_res))

ks.test(std_res,"pnorm")
normtest::jb.norm.test(std_res)
bptest(trans_lm)

## Plot Fitted Regression line on transformed scale
plot((spd$Speed),sqrt(spd$Distance),pch=19,ylab="Sqrt(Distance)",xlab="(Speed)")
abline(a=trans_lm$coef[1],b=trans_lm$coef[2],lwd=3,col="red")

## Plot the transformed regression model on original scale of the data
plot(spd$Speed,spd$Distance,pch=19,xlab="Speed",ylab="Distance")
#pred_dist = seq(min(spd$Speed),max(spd$Speed),by=1) ## Sequence (seq) of Pages of Advertising that I'm interested in predicting revenue    
preds_trans = trans_lm$coef[1]+trans_lm$coef[2]*spd$Speed ## Prediction of log(Rev)
preds_orig = (preds_trans)^2 ## Predictions of Revenue
lines(spd$Speed,preds_orig,lwd=3,col="blue") ## Draw "line" on original scale of data

preds = data.frame(Speed=spd$Speed,Distance=preds_orig)
ggplot(data=spd,aes(x=Speed,y=Distance))+geom_point()+
  geom_line(data=preds,aes(x=Speed,y=Distance),color="red",inherit.aes=FALSE)

## Check assumptions of transformed model
hist(stdres(trans_lm),freq = FALSE) #Normality OK
curve(dnorm,from = -4,to = 4,add = TRUE)

plot(trans_lm$fitted.values,trans_lm$residuals,pch=19) ## Equal Variacne OK
abline(a=0,b=0)

## Assess the fit (via R^2)
summary(trans_lm)$r.squared ## R^2 is bigger than untransformed model
og_lm <- lm(Distance ~ Speed, data = spd)
summary(og_lm)$r.squared

## Assess predictive ability of the model (via cross validation) 
set.seed(1)
n_test = 4
n_cv = 1000
bias = numeric(n_cv)
rpmse = numeric(n_cv)

for(i in 1:n_cv){
  
  test_obs = sample(1:nrow(spd),n_test)
  test_spd = spd[test_obs,]
  train_spd = spd[-test_obs,]
  train_lm = lm(sqrt(Distance)~(Speed),data=train_spd)
  test_preds = (predict.lm(train_lm,newdata=test_spd))^2
  bias[i] = mean(test_preds-test_spd$Distance)
  rpmse[i] = sqrt(mean((test_preds-test_spd$Distance)^2))
  
}

mean(bias)
mean(rpmse)
range(spd$Distance)
sd(spd$Distance)

## Original scale
set.seed(1)
n_test = 4
n_cv = 1000
bias = rep(0,n_cv)
rpmse = rep(0,n_cv)

for(cv in 1:n_cv){
  test_obs = sample(1:nrow(spd), n_test)
  test_spd = spd[test_obs,]
  train_spd = spd[-test_obs,]
  train_lm = lm(R ~ P, data=train_spd)
  test_preds = predict.lm(train_lm,newdata=test_spd)
  bias[cv] = mean(test_preds-test_spd$R)
  rpmse[cv] = sqrt(mean((test_preds-test_spd$R)^2))
}


mean(bias_log)
mean(rpmse_log)

mean(bias)
mean(rpmse)
range(spd$R)
sd(spd$R)







