y<-c(4.03,15.18,17.55,7.08,0.98,12.28,13.37,14.37,5.41,1.04,4.66)
x<-c(2011:2021)

#y1<-c(19481,20267,23344,27441,29385,29674,33319,37774,43201,45538,46013,48159)
#x<-c(2011:2021)
#y<-diff(y1)

x2<-x^2
y2<-c(8.86,9.23,11.78,5.00,9.10,8.61,7.99,17.34,12.00,3.87,5.29)

Ind<-c(rep(1,11),rep(2,11))
Xd<-c(x,x)
X2d<-c(x2,x2)
Yd<-c(y,y2)
data<-data.frame(Industry=Ind,Year=Xd,Year2=X2d,Growth=Yd,stringsAsFactors=FALSE)
unique(as.character(data$Industry))
plot(Growth~Year, pch =as.numeric(as.character(Industry)),data = data,ylab = "Growth rate(%)",main = '增長率趨勢分析')
lines(x,f2$fitted,col="red",lwd=2)
lines(x,f3$fitted,col="blue",lwd=2)
lines(x,y,lty=2)
lines(x,y2,lty=4)
legend(2015.5,2,cex=0.5,box.lty=0,c('網絡娛樂行業','網絡文學'),col = c('blue','red'),pch = 2:1,lwd = 1)
for(i in 2017:2021){
  segments(i,f3$fitted[i-2010],i,f2$fitted[i-2010],lty=5,col="#4F4F2F",lwd=2)
  points(i,f3$fitted[i-2010],pch=24,col="blue",bg="blue")
  points(i,f2$fitted[i-2010],pch=21,col="red",bg="red")
}


f3<-lm(y2~x2+x)
summary(f3)

f1<-lm(y~x)
summary(f1)


f2<-lm(y~x2+x)
summary(f2)

plot(x,y,xlab = "Year",ylab = "Growth rate(%)",main = '增長率趨勢分析')
abline(f1)
lines(x,f2$fitted,col="red")
lines(x,f3$fitted,col="blue")
legend("bottomright",cex=0.5,c('網絡娛樂行業','網絡文學'),col = c('blue','red'),pch = 2:1,lwd = 1)

yts<-ts(y,start=2011,end=2021,frequency=1)
acf(yts)
pacf(yts)
ar2<- arima(yts, order = c(2,0,0))
summary(ar2)
library(astsa)
Box.test(ar2$residuals, type="Ljung-Box", lag=5,fitdf=2+0)
MSE<-function(fitdata,actualdata){
  return(mean((fitdata-actualdata)^2))
}
MSE(fitted(ar2),y)
library(forecast)
pred.rate<-function(data,model){
  predata<-forecast(model,h=length(data))[[4]]
  return(predata)
}
pred=pred.rate(yts,ar2)

MSE(fitted(f2),y)
MSE(fitted(f1),y)

m3<-function(data){
  n<-length(data)
  m<-0
  for (i in 1:n){
    if (i >= 3){
      if (i>11){
        data[i]<-mean(data[(i-3):(i-1)])
      }
      m[i]<-mean(data[(i-2):i])
    }else{
      m[1]<-data[1]
      m[2]<-mean(data[1:2])
    }
  }
  return(m)
}
m=m3(c(y,1:10))

plot(yts,type = 'l', col = 'black',xlim=c(2011,2032),main = 'Fitted Plot')
lines(fitted(ar2),col = 'red')
abline(f1,col="green")
lines(x,f2$fitted,col="orange")
lines(c(x,2022:2031),m,col="purple")
lines(ts(pred,start=2022,end=2032,frequency=1),col = 'blue')

yp<-c(y,as.numeric(pred))
xp<-c(2011:2032)
xp2<-xp^2
fp2<-lm(yp~xp2+xp)
summary(f2)
ysm<-1/xp+8.652273-1/2023
ysm[13]<-fp2$fitted[13]
plot(xp,yp,type = 'l', col = 'black',xlim=c(2011,2032),xlab = "Year",ylab = "Growth rate(%)",main = '增長率趨勢分析')
lines(xp[1:13],fp2$fitted[1:13],col = 'red')
lines(spline(xp[13:22],ysm[13:22]),col="red")
abline(h=8.6, lty = 2)
lines(fitted(ar2),col = 'purple')
abline(f1,col="green")
lines(x,f2$fitted,col="orange")
lines(ts(pred,start=2022,end=2032,frequency=1),col = 'blue')
legend("bottomright",cex=0.5,c('Truth','Prediction','Fitted value','Simple LR','Simple QR','Trend'),col = c('black','blue','purple','green','orange','red'),lwd = 1)
axis(2, at=8.6,col.axis="black", las=0,cex.axis=0.7, tck=-.01)
arrows(2021, 15, 2020, 9, col="red")
text(2021, 15.5, labels="Trend prediction")
