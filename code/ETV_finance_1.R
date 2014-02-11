	##############################################################
	##									                                      	##
	##	     VaR and C-VaR computation with ETV on EURUSD	      ##
	##										                                      ##
	##			by Charles-Abner Dadi 				                      ##
	##										                                      ##
	##  <charles-abner.dadi@graduates.centraliens.net>		      ##							
	##										                                      ##
	##############################################################


set.seed(1234)
library(xts)
library(tseries)
library(quantmod)
library(fExtremes)
library(ismev)
library(evir)
library(forecast)
output.path	<<-as.character("/ETV/")
input.path<<-as.character("/ETV/")

Data<-read.csv2(paste(input.path,"EURUSD.csv",sep=""),header=TRUE,sep=";",stringsAsFactors=FALSE,skip=0)
View(Data)
Data<-as.data.frame(Data[-1,1:5])	# keep currency data (open, low, high,close)
INDEX <- strptime(Data[,1],'%d/%m/%Y')

for(i in 2:ncol(Data))Data[,i]=as.numeric(Data[,i])

data <- na.omit(xts(Data[,2:5], order.by=as.POSIXct(INDEX, format='%d/%m/%y')))#convert data to multiple times series
data<-data[-1,]
names(data)=c("Open","High","Low","Close")

View(data)
#str(data)
#plot(data)

Return<-na.omit(Delt(data[,'Close'],k=1)) #convert spot to return
Squarred_Return<-xts(Return^2,order.by=index(Return))
plot.ts(Return, main="Daily Return EURUSD")

#we can compare distirbution of return and gaussian distribution (in particular R/L tails)
hist((Return-mean(Return))/sd(Return),col="blue",breaks=100,xlim=c(-8,8))
par(new=TRUE)
plot(dnorm(seq(-10,10,0.001)),lwd=1,col="red")

#We observe a yearly dependance in the squared root of return corresponding to volatilty 
#we consider the left tail and the right tail (because we can be long/short)
z <- acf(Return^2,lag=252)
# Check class of the object
class(z)
# View attributes of the "acf" object
attributes(z)
# Use "acf" attribute to view the first 13 elements (1 = lag at 0)
z$acf[1:13]
# Get rid of the first element (i.e. lag 0)
z$acf[-1]
# Plot the autocorrelation function without lag 0
plot(z$acf[-1], 
     type="h", 
     main="Autocorrelation Function on Volatility", 
     xlab="Lag",     
     ylab="ACF", 
     ylim=c(-0.2,0.2), # this sets the y scale to -0.2 to 0.2
     las=1,
     xaxt="n")
abline(h=0)
# Add labels to the x-axis
x <- c(1:252)
axis(1, at=x)



#Method of Block Maxima:
#1/ We divide sample in n blocks, what is the efficient n? We choose 6 months 
n=126
N=length(Return)

s<-seq(1,(N-n),n)
LeftTail<-NULL;RightTail<-NULL
for(t in s)RightTail<-c(RightTail,max(Return[t:(t+n)]))
for(t in s)LeftTail<-c(LeftTail,min(Return[t:(t+n)]))

plot(LeftTail,type="h",col="red",ylim=c(min(LeftTail),max(RightTail)),lwd=2,main="",ylab="",xlab="")
par(new=TRUE)
plot(RightTail,type="h",col="blue",ylim=c(min(LeftTail),max(RightTail)),lwd=2,
		main="6 Months minima and maxima of the daily returns of the EURUSD.(BM)",ylab="Return",xlab="Time")
abline(h=0,lwd=3,lty = "dotted")

#2/ Fit GEV by maximization of log-likelihood

#2.1: Estimating GEV
LeftTail=-LeftTail


RightTailFit<-gev(RightTail)
LeftTailFit<-gev(-LeftTail)
tailplot(RightTailFit, optlog = NA, extend = 1.5, labels = TRUE)
plot(LeftTailFit)


#2.2: Validation of results
RightTailValidation=gev.fit(RightTail)
class(RightTailValidation) <- "gev.fit"
plot(RightTailValidation)

dev.new()

LeftTailFit=gev.fit(LeftTail)
class(LeftTailValidation) <- "gev.fit"
plot(LeftTailValidation)
dev.new()



#3/ Compute VaR



###################
##Methode POT#####
##################

x = as.timeSeries(Return)

#right
#we are looking the optimum threshold
th<-findthresh(x, 100) #we would get 100 values above threshold
	#Alternatively
qqnorm(x)#we select the threshold graphically to get 3sigma
fitRight1 = gpd(x,th)
View(fitRight1 $par.ests)

##We compare result with another fitting function from package Ismev
fitRight2=gpdFit(x,th)
tailPlot(fitRight2)

#left
th<-findthresh(-x, 100) #we would get 100 values above threshold
	#Alternatively
qqnorm(-x)#we select the threshold graphically to get 3sigma
fitLeft1 = gpd(-x,th)
View(fitLeft1 $par.ests)

##We compare result with another fitting function from package Ismev
fitLeft2=gpdFit(-x,th)
tailPlot(fitLeft2)

#We can read VaR on graphic


plot(-fitLeft1$data ,type="h",col="red",ylim=c(min(LeftTail),max(RightTail)),lwd=2,main="",ylab="",xlab="")
par(new=TRUE)
plot(fitRight1$data ,type="h",col="blue",ylim=c(min(LeftTail),max(RightTail)),lwd=2,
		main="6 Months minima and maxima of the daily returns of the EURUSD.(POT)",ylab="Return",xlab="Time")
abline(h=0,lwd=3,lty = "dotted")



