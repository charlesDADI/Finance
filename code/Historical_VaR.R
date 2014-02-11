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
output.path	<<-as.character("C:/Users/charlesabner/Dropbox/Ivan-Charles-Dan/ETV/")
input.path<<-as.character("C:/Users/charlesabner/Dropbox/Ivan-Charles-Dan/ETV/")

Data<-read.csv2(paste(input.path,"EURUSD.csv",sep=""),header=TRUE,sep=";",stringsAsFactors=FALSE,skip=0)
Data<-as.data.frame(Data[-1,1:5])	# keep currency data (open, low, high,close)
INDEX <- strptime(Data[,1],'%d/%m/%Y')

for(i in 2:ncol(Data))Data[,i]=as.numeric(Data[,i])

data <- na.omit(xts(Data[,2:5], order.by=as.POSIXct(INDEX, format='%d/%m/%y')))#convert data to multiple times series
data<-data[-1,]
names(data)=c("Open","High","Low","Close")

#View(data)
#str(data)
#plot(data)

Return<-na.omit(Delt(data[,'Close'],k=1)) #convert spot to return
Squarred_Return<-xts(Return^2,order.by=index(Return))
plot.ts(Return, main="Rolling  Historical VaR 6-months",ylim=c(-0.03,0.03))


N<-length(Return)
q_001<-NULL;q_005<-NULL;q_01<-NULL;
ind<-seq(1,(N-126),126)
for(t in ind)q_001<-c(q_001,quantile(Return[t:(t+126)],0.001))
for(t in ind)q_005<-c(q_005,quantile(Return[t:(t+126)],0.005))
for(t in ind)q_01<-c(q_01,quantile(Return[t:(t+126)],0.01))

par(new=TRUE)
plot.ts(q_001,col="red",ylim=c(-0.03,0.03),lwd=2)
par(new=TRUE)
plot.ts(q_005,col="blue",ylim=c(-0.03,0.03),lwd=2)
par(new=TRUE)
plot.ts(q_01,col="green",ylim=c(-0.03,0.03),lwd=2)


