data(worldcup,package='faraway')
ls()
worldcup
mean(worldcup$Time)

#HISTOGRAM
small.size.dataset=c(91,49,76,112,97,42,70, 100, 8, 112, 95, 90, 78, 62, 56, 94, 65, 58, 109, 70, 109, 91, 71, 76, 68, 62, 134, 57, 83, 66)
hist(small.size.dataset,xlab='My Data Points',main='Histogram of My Data')
hist(small.size.dataset,xlab='My Data Points',main='Histogram of My Data',freq=F)
hist(small.size.dataset,xlab='My Data Points',main='Histogram of My Data',freq=F,col='green')
lines(density(small.size.dataset),col='red',lwd=5)
hist(small.size.dataset,xlab='My Data Points',main='Histogram of My Data',freq=F,col='green',breaks=10)
lines(density(small.size.dataset),col='red',lwd=5)

#SCATTERPLOT
set.seed(2016)
Test_1_scores=round(rnorm(50, 78, 10))
Test_1_scores
Test_2_scores=round(rnorm(50, 78, 10))
Test_2_scores
plot(Test_2_scores~Test_1_scores)
plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores')
plot(Test_2_scores~Test_1_scores, main='Test scores for two exams (50 students)', xlab='Test_1_scores', ylab='Test 2 scores',col='blue')




#BASIC STATISTICS
co2
help(co2)
class(co2)
plot(co2,main='Atmospheric CO2 Concentration')
#BYHAND
co2.values = as.numeric(co2) 
co2.times = as.numeric( time(co2) ) 
SSxx  = sum(   (co2.times - mean(co2.times)  ) * (co2.times - mean(co2.times) ) ) 
SSxy  = sum(   (co2.values - mean(co2.values)  ) * (co2.times - mean(co2.times) ) ) 
slope = SSxy / SSxx
slope   
intercept = mean(co2.values) - (slope*mean(co2.times)) 
intercept 
#SIMPLE
co2.linear.model = lm(co2 ~ time(co2) ) 
summary(co2.linear.model)
plot(co2, main='Atmospheric CO2 Concentration with Fitted Line') 
abline(co2.linear.model )  
#BYHAND
co2.fitted.values = slope*co2.times + intercept 
co2.residuals = co2.values - co2.fitted.values
#SIMPLE
( co2.residuals = resid( co2.linear.model ) ) 
par(mfrow=c(1,3)) 
( c02.residuals = resid( co2.linear.model ) ) 
hist(co2.residuals, main= "Histogram of CO2 Residuals") 
qqnorm(c02.residuals, main= "Normal Probability Plot") 
qqline(c02.residuals) 
plot(c02.residuals ~ time(co2), main="Residuals on Time") 
#ZOOMED
plot(c02.residuals ~ time(co2), xlim=c(1960, 1963), main="Zoomed in Residuals on Time") 





#DATASLEEP
sleep
plot(sleep$extra~sleep$group,main='Extra Sleep in Gossett Data by Group')
attach(sleep)
extra.1=extra[group==1] 
extra.2=extra[group==2] 
t.test(extra.1, extra.2, paired=TRUE, alternative="two.sided") 
plot(extra.2~extra.1, xlab='extra sleep with drug 1',  ylab='extra sleep with drug 2' ,  main='Extra Sleep Drug 2 against Extra Sleep Drug 1') 
sleep.linear.model = lm(extra.2 ~ extra.1 ) 
summary(sleep.linear.model)
abline(sleep.linear.model) 

sleep.linear.model = lm(extra.1 ~ extra.2 ) 
summary(sleep.linear.model)
abline(sleep.linear.model)
plot(extra.1~extra.2, xlab='extra sleep with drug 2',  ylab='extra sleep with drug 1' ,  main='Extra Sleep Drug 1 against Extra Sleep Drug 2')
( extra.residuals = resid( sleep.linear.model ) )
hist(extra.residuals, main= "Histogram of Residuals") 


x=c(1,2,3,4)
y=c(5,7,12,13)
(m=lm(y~x))

library(faraway)
punting
m=lm(punting$Distance~ punting$Hang) 
qqnorm(resid(m)) 
qqline(resid(m))





#
k=ts(rnorm(100))
print(k)
(acf(k,type='covariance'))

#MOVING AVERAGE
# Generate noise
noise=rnorm(10000)

# Introduce a variable
ma_2=NULL

# Loop for generating MA(2) process

for(i in 3:10000){
    ma_2[i]=noise[i]+0.7*noise[i-1]+0.2*noise[i-2]
}

# Shift data to left by 2 units
moving_average_process=ma_2[3:10000]

# Put time series structure on a vanilla data
moving_average_process=ts(moving_average_process)

# Partition output graphics as a multi frame of 2 rows and 1 column
par(mfrow=c(2,1))

# plot the process and plot its ACF
plot(moving_average_process, main='A moving average process of order 2', ylab=' ', col='blue')
acf(moving_average_process, main='Correlogram of a moving average process of order 2')






# Simulating a non-stationary time series

# Set seed so thet we generate the same dataset
set.seed(2017)
# time variable 
t=seq(0,1,1/100)
# generate a time series
some.time.series=2+3*t+ rnorm(length(t))
k=ts(some.time.series)
(acf(k,type='covariance'))
(acf(k))


# Simulating MA(4) process.
# X_t= Z_t+0.2 Z_(t-1)+0.3 Z_(t-2)+ 0.4 Z_(t-3)

set.seed(2^10)
z=NULL
z=rnorm(1000)
data=NULL
for(i in 4:1000){
  data[i-3]=z[i]+0.2*z[i-1]+0.3*z[i-2]+0.4*z[i-3]
  }
data=ts(data)
(acf(data))







-----------------------------------------------------------------------------------

#SIMULATION OF AR(2) PROCESS
#set seed a common number, so we can reproduce the same datasets
set.seed(2017)



#model parameters (we will estimate them)
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
phi

#number of data points
n=10000



#simulate ar process
ar.process=arima.sim(n,model=list(ar=c(1/3,1/2)), sd=4)
ar.process[1:5]




#find and name 2nd and 3rd sample autocorrelation
r=NULL
r[1:2]=acf(ar.process, plot=F)$acf[2:3]
r

#matrix R
R=matrix(1,2,2) # matrix of dimension 2 by 2, with entries all 1's.
R




#edit R
R[1,2]=r[1] # only diagonal entries are edited
R[2,1]=r[1] # only diagonal entries are edited
R




#b-column vector on the right
b=matrix(r,nrow=2,ncol=1)# b- column vector with no entries
b

#solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=matrix(c(solve(R,b)[1,1], solve(R,b)[2,1]),2,1)
phi.hat




#variance estimation
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat




#plot time series, along with acf, pacf
par(mfrow=c(3,1))
plot(ar.process, main='Simulated AR(2)')
acf(ar.process, main='ACF')
pacf(ar.process, main='PACF')
-----------------------------------------------------------------------------
#SIMULATION OF AR(3) PROCESS
set.seed(2017)
sigma=4
phi=NULL
phi[1:3]=c(1/3,1/2,7/100)
n=100000

#Simulate AR(3) process
ar3.process=arima.sim(n,model=list(ar=c(1/3,1/2, 7/100)), sd=4)

r=NULL
r[1:3]=acf(ar3.process, plot=F)$acf[2:4]
r
R=matrix(1,3,3) 
R[1,2]=r[1] 
R[1,3]=r[2]
R[2,1]=r[1]
R[2,3]=r[1]
R[3,1]=r[2]
R[3,2]=r[1]
R


# b-column vector on the right
b=matrix(,3,1)# b- column vector with no entries
b[1,1]=r[1]
b[2,1]=r[2]
b[3,1]=r[3]
b

# solve Rx=b and find phi's
phi.hat=solve(R,b)
phi.hat


# sigme estimation
c0=acf(ar3.process, type='covariance', plot=F)$acf[1]
var.hat=c0*(1-sum(phi.hat*r))
var.hat


par(mfrow=c(3,1))
plot(ar3.process, main='Simulated AR(3)')
acf(ar3.process, main='ACF')
pacf(ar3.process, main='PACF')
------------------------------------------------------------------------------------------
#Modeling recruitment time series from 'astsa' package as an AR process
library(astsa)
my.data=rec
head(my.data)
?
# Plot rec 
plot(rec, main='Recruitment time series', col='blue', lwd=3)


# subtract mean to get a time series with mean zero
ar.process=my.data-mean(my.data)
?
# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process, main='Recruitment', col='red', lwd=3)
pacf(ar.process, main='Recruitment', col='green', lwd=3)


# order
p=2
?
# sample autocorreleation function r
r=NULL
r[1:p]=acf(ar.process, plot=F)$acf[2:(p+1)]
cat('r=',r,'\n')


# matrix R
R=matrix(1,p,p) # matrix of dimension 2 by 2, with entries all 1's.
?
# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R






# b-column vector on the right
b=NULL
b=matrix(r,p,1)# b- column vector with no entries
b




# solve(R,b) solves Rx=b, and gives x=R^(-1)b vector
phi.hat=NULL
phi.hat=solve(R,b)[,1]
phi.hat




#variance estimation using Yule-Walker Estimator
c0=acf(ar.process, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat

# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
phi0.hat

cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')
------------------------------------------------------------------------------------------------------

#Johnson & Johnson quarterly earnings per share



# Time plot for Johnson&Johnson
plot(JohnsonJohnson, main='Johnson&Johnosn earnings per share', col='blue', lwd=3)






# log-return of Johnson&Johnson
jj.log.return=diff(log(JohnsonJohnson))
jj.log.return.mean.zero=jj.log.return-mean(jj.log.return)






# Plots for log-returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero, main='Log-return (mean zero) of Johnson&Johnosn earnings per share')
acf(jj.log.return.mean.zero, main='ACF')
pacf(jj.log.return.mean.zero, main='PACF')






# Order
p=4




# sample autocorreleation function r
r=NULL
r[1:p]=acf(jj.log.return.mean.zero, plot=F)$acf[2:(p+1)]
r



# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.
?
# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R



# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b

phi.hat=solve(R,b)[,1]
phi.hat


# Variance estimation using Yule-Walker Estimator
c0=acf(jj.log.return.mean.zero, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
phi0.hat

cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')
---------------------------------------------------------------------------------------------------
#LAKE HURON

data=LakeHuron
plot(data)
new=diff(data)
plot(new)
pacf(new)

# Order
p=2




# sample autocorreleation function r
r=NULL
r[1:p]=acf(new, plot=F)$acf[2:(p+1)]
r



# matrix R
R=matrix(1,p,p) # matrix of dimension 4 by 4, with entries all 1's.
?
# define non-diagonal entires of R
for(i in 1:p){
    for(j in 1:p){
        if(i!=j)
            R[i,j]=r[abs(i-j)]
        }
    }
R

# b-column vector on the right
b=matrix(r,p,1)# b- column vector with no entries
b

phi.hat=solve(R,b)[,1]
phi.hat


# Variance estimation using Yule-Walker Estimator
c0=acf(new, type='covariance', plot=F)$acf[1]
c0
var.hat=c0*(1-sum(phi.hat*r))
var.hat

# Constant term in the model
phi0.hat=mean(new)*(1-sum(phi.hat))
phi0.hat

cat("Constant:", phi0.hat," Coeffcinets:", phi.hat, " and Variance:", var.hat, '\n')
----------------------------------------------------------------------------------------------------

#ARIMA

#ARIMA(2,1,1) Simulation


# parameters
phi=c(.7, .2)
beta=0.5
sigma=3
m=10000


set.seed(5)
(Simulated.Arima=arima.sim(n=m,list(order = c(2,1,1), ar = phi, ma=beta)))



plot(Simulated.Arima, ylab=' ',main='Simulated time series from ARIMA(2,1,1) process', col='blue', lwd=2)



acf(Simulated.Arima)


Diff.Simulated.Arima=diff(Simulated.Arima)

plot(Diff.Simulated.Arima)



acf(Diff.Simulated.Arima)


pacf(Diff.Simulated.Arima)



library(astsa)
sarima(Simulated.Arima,2,1,1,0,0,0) # astsa not installed



library(forecast)
auto.arima(Simulated.Arima) # forecast not installed


fit1<-arima(Diff.Simulated.Arima, order=c(4,0,0))
fit1

fit2<-arima(Diff.Simulated.Arima, order=c(2,0,1))
fit2


fit3<-arima(Simulated.Arima, order=c(2,1,1))
fit3
--------------------------------------------------------------------------------------------------
#SARIMA
x=NULL
z=NULL
n=10000

z=rnorm(n)
x[1:13]=1

for(i in 14:n){
  x[i]<-z[i]+0.7*z[i-1]+0.6*z[i-12]+0.42*z[i-13]
}

par(mfrow=c(2,1))
plot.ts(x[12:120], main='The first 10 months of simulation SARIMA(0,0,1,0,0)_12', ylab='') 

acf(x, main='SARIMA(0,0,1,0,0,1)_12 Simulation')
-------------------------------------------------------------------------------------------------
#SARIMA APPLICATION

library(astsa)

d=1
DD=1

per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
library(astsa)

sarima(log(jj), 0,1,1,1,1,0,4)
--------------------------------------------------------------------------------------------------------------------------------------





