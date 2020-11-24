data1=read.table(file.choose(),header=T,sep=",")
data1
temp1 <- as.matrix(data1) 
temp1

df <- as.data.frame(matrix(temp1),nrow = 6, ncol = 12)
df.ts <- ts(as.vector(t(as.matrix(df))),start=c(2013,1), end=c(2018,12), frequency=12)
df.ts

#plot for differenced series
plot(diff(df.ts))


# acf and pacf of the differenced data

acf(diff(df.ts), main='ACF of log return data')
pacf(diff(df.ts), main='PACF of log return data')

model1<-arima(k, order=c(0.9,1,0.5))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals, lag = log(length(model1$residuals)))

model2<-arima(k, order=c(0.35,1,0.5))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals, lag = log(length(model2$residuals)))

model3<-arima(k, order=c(0.42,1,0.5))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals, lag = log(length(model3$residuals)))

model4<-arima(k, order=c(0.2,1,0.5))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals, lag = log(length(model4$residuals)))


df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, model1.test$p.value), 
               c(model2$aic, SSE2, model2.test$p.value), c(model3$aic, SSE3, model3.test$p.value),
               c(model4$aic, SSE4, model4.test$p.value))
colnames(df)<-c('Arima(0.9,1,0.5)','Arima(0.35,1,0.5)', 'Arima(0.42,1,0.5)', 'Arima(0.2,1,0.5)')

format(df, scientific=FALSE)





