setwd("C:\\Users\\seena\\Downloads\\FYP-master\\Data")
library(dplyr)
library(ggplot2)
library(plotly) 
library(extRemes) 
library(moments)

#Function used for analyzing the distribution of data having outliers

analysis<-function(openDataCoin)
{ 
  print(mean(openDataCoin))
  print(median(openDataCoin)) 
  print(sqrt(var(openDataCoin))) 
  print(max(openDataCoin)-min(openDataCoin)) 
  print(hist(openDataCoin))
  print(curve(dnorm(x,mean(openDataCoin),sd(openDataCoin)),col="red" ,add=TRUE)) 
}

#------------------------------------------------------------------------------------------------------------------

#Checking outliers of open column for btc


total.file<- read.csv("C:\\Users\\seena\\Downloads\\FYP-master\\Data\\CryptocoinsHistoricalPrices.csv") 
length(total.file$High) 
head(total.file) 
total.file$Date<-as.character(total.file$Date) 
total.file$Date <- as.Date(total.file$Date,format='%Y-%m-%d') 
total.file$Date 
new.total.file<- total.file %>% mutate(Year= format(as.Date(total.file$Date, format="%Y-%m-%d"),"%Y"))%>% mutate(Month=format(as.Date(total.file$Date, format="%Y-%m-%d"),"%m")) 
head(new.total.file) 


BTC.2014 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2014) 
BTC.Open.2014<-BTC.2014$Open 
boxplot(BTC.Open.2014, horizontal = TRUE, las=1,notch = TRUE, col="slategray3", ylim=c(100,1000), boxwex=0.5, whisklty=1, main="Opening of btc for the year 2014" ,xlab="btc Open ")

BTC.2015 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2015)
BTC.Open.2015<-BTC.2015$Open
boxplot(BTC.Open.2015, horizontal = TRUE, las=1, notch = TRUE, col="slategray3", ylim=c(100,500), boxwex=0.5, whisklty=1, main="Opening of btc for the year 2015" ,xlab="btc Open ") 
analysis(BTC.Open.2015) 

BTC.2016 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2016)
BTC.Open.2016<-BTC.2016$Open 
boxplot(BTC.Open.2016, horizontal = TRUE, las=1, notch = TRUE, col="slategray3", ylim=c(100,1000), boxwex=0.5, whisklty=1, main="Opening of btc for the year 2016" ,xlab="btc Open ") 

BTC.2017 <- new.total.file%>%filter(coin=="BTC")%>%filter(Year==2017) 
BTC.Open.2017<-BTC.2017$Open 
boxplot(BTC.Open.2017, horizontal = TRUE, las=1, notch = TRUE, col="slategray3", ylim=c(800,12000), boxwex=0.5, whisklty=1, main="Opening of btc for the year 2017" ,xlab="btc Open ")
analysis(BTC.Open.2017)

#------------------------------------------------------------------------------------------------------------------

#Checking outliers of open column for bch

BCH.2017 <- new.total.file%>%filter(coin=="BCH")%>%filter(Year==2017)
BCH.Open.2017<-BCH.2017$Open 
boxplot(BCH.Open.2017, horizontal = TRUE, las=1, notch = TRUE, col="slategray3", ylim=c(100,5000), boxwex=0.5, whisklty=1, main="Opening of bch for the year 2017" ,xlab="bch Open ") 
analysis(BCH.Open.2017) 

BCH.2018 <- new.total.file%>%filter(coin=="BCH")%>%filter(Year==2018)
BCH.Open.2018<-BCH.2018$Open 
boxplot(BCH.Open.2018, horizontal = TRUE, las=1, notch = TRUE, col="slategray3", ylim=c(100,5000), boxwex=0.5, whisklty=1, main="Opening of bch for the year 2018" ,xlab="bch Open ")

#----------------------------------------------------------------------------------------------------------------



# To Find 5 Most Dominat Cryptocurrencies
file.list <- list.files(path="C:/Users/seena/Downloads/FYP-master/Data2", pattern = ".csv", full.names = TRUE)
CoinName = c()
highestMarketCap = c()
for (i in 1:length(file.list))
  { 
  file.df <- read.csv(file.list[i],header = TRUE)
  maxVal <- gsub(",","",max(as.character(file.df$Market.Cap))) 
  maxVal <- as.numeric(maxVal) 
  highestMarketCap <- c(highestMarketCap,maxVal) 
  CoinName <- c(CoinName, as.character(file.df$coin[[1]]))
}
  
newdf <- data.frame(CoinName,highestMarketCap) 
newdf <- arrange(newdf,CoinName,highestMarketCap) 
newdf <- arrange(newdf,desc(highestMarketCap)) 
topfive <- newdf[seq(1:5),] 
ggplot(topfive,aes(x=topfive$CoinName,y=topfive$highestMarketCap,fill=topfive$CoinName))+geom_bar(stat = "identity")+xlab("Coins")+ylab("Market Cap")+ggtitle("Top 5 Cryptocurrencies")

# ----------------------------------------------------------------------------------------------------------

# Highest Price Obtained by Top Cryptocurrencies 

setwd("C:\\Users\\seena\\Downloads\\FYP-master\\Data2\\")
btcData <- read.csv('BTC.csv') 
btcData$Date<-as.character(btcData$Date) 
btcData$Date <- as.Date(btcData$Date,format='%Y-%m-%d') 

ethData <- read.csv('ETH.csv') 
ethData$Date<-as.character(ethData$Date) 
ethData$Date <- as.Date(ethData$Date,format='%Y-%m-%d') 

bchData <- read.csv('BCH.csv') 
bchData$Date<-as.character(bchData$Date)
bchData$Date <- as.Date(bchData$Date,format='%Y-%m-%d') 

DOGEData <- read.csv('DOGE.csv') 
DOGEData$Date<-as.character(DOGEData$Date) 
DOGEData$Date <- as.Date(DOGEData$Date,format='%Y-%m-%d') 

KinData <- read.csv('KIN.csv') 
KinData$Date<-as.character(KinData$Date) 
KinData$Date <- as.Date(KinData$Date,format='%Y-%m-%d') 

df1<- btcData %>%bind_rows(ethData,bchData,DOGEData,KinData) 
plot_ly(x=df1$Date,y=df1$High,type='scatter',mode='lines',color=df1$coin)%>% layout(xaxis =list(title='Year'), yaxis = list(title='Price in USD'))

#-------------------------------------------------------------------------------------------------------------

# To plot the Open, Close, Low and High Prices of the Top 5 Cryptocurrencies

ethData <- read.csv('ETH.csv') 
DogeData <- read.csv('Doge.csv') 

ethData$Date<-as.character(ethData$Date) 
ethData$Date <- as.Date(ethData$Date,format='%Y-%m-%d') 
DogeData$Date<-as.character(DogeData$Date) 
DogeData$Date <- as.Date(DogeData$Date,format='%Y-%m-%d')

openData <- function(coinData)
  { 
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Close))+ geom_line(color="blue")+ ggtitle("data")) 
  } 
closeData <- function(coinData)
  { 
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Close))+ geom_line(color="yellow")) 
  } 
highData <- function(coinData)
  { 
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$High))+ geom_line(color="red")) 
  } 
lowData <- function(coinData)
  {
  print(ggplot(data=coinData,aes(x=coinData$Date,y=coinData$Low))+ geom_line(color="green")) 
} 

openData(ethData) 
closeData(ethData) 
lowData(ethData) 
highData(ethData) 

openData(DogeData) 
closeData(DogeData) 
lowData(DogeData) 
highData(DogeData)

#---------------------------------------------------------------------------------------------------------------

#Code to iterate through files to obtain coins whose prices have gone greater than 1000$ 
highCoins = c() 
for (i in 1:length(file.list))
  { 
  file.df <- read.csv(file.list[i],header = TRUE) 
  max.value <-max(file.df$High,na.rm=TRUE) 
  if(max.value >= 1000) 
    { 
    highCoins <- c(highCoins,as.character(file.df$coin[1])) 
    } 
  } 
length(highCoins) 
highCoins




#--------------------------------------------------------------------------------------------------------------
#Plot for analysing the distribution of the delta of all coins in a yearly format

total.file<- read.csv("C:\\Users\\seena\\Downloads\\FYP-master\\Data\\CryptocoinsHistoricalPrices.csv") 
length(total.file$High) 
head(total.file) 
total.file$Date<-as.character(total.file$Date) 
total.file$Date <- as.Date(total.file$Date,format='%Y-%m-%d') 
total.file$Date 
new.total.file<- total.file %>% mutate(Year= format(as.Date(total.file$Date, format="%Y-%m-%d"),"%Y"))%>% mutate(Month=format(as.Date(total.file$Date, format="%Y-%m-%d"),"%m")) 
head(new.total.file) 
summary(new.total.file) 
year<-c("2014","2015","2016","2017","2018") 
typeof(year) 
typeof(new.total.file$Year) 
for(y in c("2014","2015","2016","2017","2018"))
{ 
  delta.yearly<-new.total.file%>%group_by(Year)%>%filter(Year==y) 
  delta.yearly 
  print(ggplot(delta.yearly[delta.yearly$Delta>1&delta.yearly$Delta<10,],aes(x=Date,y=Delta, color=coin)) +geom_point()+ scale_x_date(date_breaks = "1 month", date_labels = "%B")+ theme(legend.position="none")) 
}

#--------------------------------------------------------------------------------------------------------------

#Code to plot the distribution of the count of all coins delta whose value exceeds one 

ggplot(total.file[total.file$Delta > 1 & total.file$Delta < 10, ], aes(x=Delta, color=coin)) +geom_histogram() + theme(legend.position="none")

#-------------------------------------------------------------------------------------------------------------

#Code to plot the distribution of the count of all coins delta whose value is less than one 

ggplot(total.file[total.file$Delta <1, ], aes(x=Delta, color=coin)) + geom_histogram() + theme(legend.position="none")

#------------------------------------------------------------------------------------------------
#Code to plot the distribution of the count of all coins delta whose value is less than 0

ggplot(total.file[total.file$Delta <0, ], aes(x=Delta, color=coin)) +geom_histogram() + theme(legend.position="none")

#-------------------------------------------------------------------------------------------------------------




