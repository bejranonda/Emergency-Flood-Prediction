#
#Test program to carry out various time series analyses
#
#load libraries needed
library(timeSeries)
library(Kendall)
library(Rwave)
library(TSA)
library(wmtsa)
library(plotrix)
library(car)
library(DAAG)
library(forecast)
source("clear.R")
#clear()


#--Rayong-- Change station name
cat("\nStart AutoRegressive Model")

########################define file for input / read ################################
## obs
cat("\nReading Obs")
#data <- read.table("../../data/raw/seawl24.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("../../data/raw/wloct.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data <- read.table("wlsep.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data.f.index = read.table("../../data/raw/seaPrd.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

ts.col = 1
par.col = 3
index.col = 2



#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="../../output/predictions/AR_BKK10Nov"


L=file.exists(dir_name)
if(!L) dir.create(dir_name, recursive=TRUE)
dir0=getwd()
dir1=dir_name

##########################################The stuff above needs to be processed only once.################################

n.ahead = 5 # number of ahead prediction
dash.val = 2.5 # dash line at chart
cal.ratio = 0.5
#prdn = nrow(data)
prdn = 25
caln = round(prdn*cal.ratio)
calv = prdn - caln
ln1 = rep(2.5,prdn+n.ahead) # dash line at chart

#P1 Obs
data.prd = data[1:prdn,par.col]

#P2 Index
#data.index = data[,index.col]
data.index = data[1:prdn,index.col] 

#P3 future Index
#data.index.f = data.f.index[,2]
data.index.f = data[,index.col] 


#P1 Obs calibration
data.prd.cal = data.prd[1:caln]
#P2 Index calibration
data.index.cal = data.index[1:caln]

#P1 Obs verification
data.prd.vrf = data.prd[(caln+1):prdn]
#P2 Index verification
data.index.vrf = data.index[(caln+1):prdn]

#####################
## time sieries
#####################

#P1 Obs
ts.data.prd = ts(data.prd,frequency=365,start=c(2011,283))
ts.data.index = ts(data.index,frequency=365,start=c(2011,283))
ts.data.index.f = ts(data.index.f,frequency=365,start=c(2011,283))


data.prd.use = data.prd
data.index.use = ts(scale(data.index),frequency=365,start=c(2011,283))
data.index.f.use = ts(scale(data.index.f[(prdn+1):(prdn+n.ahead)]),frequency=365,start=c(2011,283+prdn))
#data.index.f.all = ts(data.index.f,frequency=365,start=c(2011,283)) # Sea indundation
data.index.f.all = data.index.f # Sea indundation

data.index.use1 = ts(data.index,frequency=365,start=c(2011,283))
data.index.f.use1 = ts(data.index.f[(prdn+1):(prdn+n.ahead)],frequency=365,start=c(2011,283+prdn))

data.index.use2 = ts(data.index-3,frequency=365,start=c(2011,283))
data.index.f.use2 = ts(data.index.f[(prdn+1):(prdn+n.ahead)]-3,frequency=365,start=c(2011,283+prdn))

data.index.use3 = ts(data.index*100,frequency=365,start=c(2011,283))
data.index.f.use3 = ts(data.index.f[(prdn+1):(prdn+n.ahead)]*100,frequency=365,start=c(2011,283+prdn))

data.index.use4 = ts(data.index*100-300,frequency=365,start=c(2011,283))
data.index.f.use4 = ts(data.index.f[(prdn+1):(prdn+n.ahead)]*100-300,frequency=365,start=c(2011,283+prdn))





##############################
###### AR prediction #########
##############################

cat("\n## AR\n")
mth = "ols"
ord = c(1:2)
case = length(ord)

#Prediction charts
setwd(dir1)
pdf(paste("ARp-charts.pdf"))
setwd(dir0)

par(ps = 10,mfrow = c(2,1))

for(j in 1:case){
						fit = ar(data.prd.use,order.max=ord[j],method="ols")
						fit.err = sqrt(mean((na.omit(fit$resid))^2))
						cat("\nAR",j," --",fit.err)
						fitp = forecast(fit,h = n.ahead)
						#Plot charts
						plot(fitp)
						title(sub = paste("AR",ord[j]," WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
		 	}



##############################
###### ARIMA prediction #########
##############################

cat("\n\n## ARIMA\n")

###################### To select all indicator ###############
##############################################################


#Prediction charts
leg.txt <- c("prediction", "observation")
par(ps = 10,mfrow = c(2,1))
					     	fit = auto.arima(data.prd.use,stepwise=TRUE)
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA1 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA1 --",fit.err)

					     	fit = auto.arima(data.prd.use,stepwise=FALSE)
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA2 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA2 --",fit.err)

						fit = auto.arima(data.prd.use,start.p=0,start.q=0,stepwise=TRUE) # no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA3 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA3 --",fit.err)

						fit = auto.arima(data.prd.use,start.p=1,start.q=1,stepwise=FALSE)# no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA4 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA4 --",fit.err)

						fit = auto.arima(data.prd.use,start.p=2,start.q=2,stepwise=FALSE)# no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA5 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA5 --",fit.err)

						fit = auto.arima(data.prd.use,d=0,start.p=1,start.q=1,stepwise=FALSE)# no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA6 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA6 --",fit.err)

						fit = auto.arima(data.prd.use,d=1,start.p=1,start.q=1,stepwise=FALSE)# no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA7 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA7 --",fit.err)

						fit = auto.arima(data.prd.use,d=2,start.p=1,start.q=1,stepwise=FALSE)# no d defined
						fit.err = sqrt(mean((na.omit(fit$residuals))^2))
						fitp = forecast(fit,n.ahead)
						plot(fitp)
						title(sub = paste("ARIMA8 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMA8 --",fit.err)


#########################################
###### ARIMAexternal Prediction    ######
#########################################

cat("\n\nARIMAexternal prediction\n")
						#fit = auto.arima(data.prd.use,xreg=data.index.use,d=1,start.p=1,start.q=0,stepwise=TRUE)
						fit = auto.arima(data.prd.use,xreg=data.index.use,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use)
						plot(fitp, ylim = c(1.6,4))
						title(sub = paste("ARIMAex1 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMAex1 --",fit.err)

						fit = auto.arima(data.prd.use,xreg=data.index.use,d=0,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use)
						plot(fitp, ylim = c(1.6,4))
						title(sub = paste("ARIMAex2 WL at Navy Headquarter - WooWooWuu.de",round(fit.err,digits = 3)))
						lines(ln1,col = "Red",lty = "dashed")
						cat("\nARIMAex2 --",fit.err)


						fit = auto.arima(data.prd.use,xreg=data.index.use,d=1,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "Day in October", xlim = c(1,45), ylim = c(1.6,4),lwd = 2)
						title(sub = paste("ARIMAex4 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						lines(ln1,col = "Red",lty = "dashed")
						lines(ln1+.3,col = "Red",lty = "dashed")
						lines(ln1+.5,col = "Red",lty = "dashed")
						#lines(data.index.f.all,col="Green",lty = 2)
						text(7,2.55,"Wall 2.5 m. - Genearal wall")
						text(7,2.85,"Wall 2.8 m. - Top-up wall")
						text(7,3.05,"Wall 3.0 m. - Extra top-up wall")
						text(26,2.2,"predicted river height")
						cat("\nARIMAex --",fit.err)
						
						#####
						# Main calculation to show
						par(ps = 18)
						#fit = auto.arima(data.prd.use,xreg=data.index.use,d=2,stepwise=FALSE) choice 1
						fit = auto.arima(data.prd.use,xreg=data.index.use,d=1,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "October and November", xlim = c(1,30), ylim = c(1.5,3.5),lwd = 2, xaxt = "n")
						day = c(21:31,1:17)
						day.i = c(1:28)
						axis(1, at=day.i,labels=day, col.axis="black", las=1)
						abline(v=(seq(1,45,1)), col="lightgray", lty="dotted")
						abline(h=(seq(1.5,4,0.1)), col="lightgray", lty="dotted")
						title(sub = paste("ARIMAex4 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						lines(ln1,col = "Red",lty = "dashed")
						lines(ln1+.3,col = "Red",lty = "dashed")
						lines(ln1+.5,col = "Red",lty = "dashed")
						#lines(data.index.f.all,col="Green",lty = 2)
						text(7,2.59,"Wall 2.5 m.MSL - General wall")
						text(7.5,2.89,"Wall 2.8 m.MSL - Top-up wall")
						text(7,3.09,"Wall 3.0 m.MSL - Extra top-up wall")
						text(20,2.4,"predicted river height",col = "blue")
						cat("\nARIMAex --",fit.err)

						fit = auto.arima(data.prd.use,xreg=data.index.use1,d=2,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use1)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "Day in October", xlim = c(1,45), ylim = c(1.6,4),lwd = 2)
						title(sub = paste("ARIMAexTest1 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						cat("\nARIMAexTest1 --",fit.err)

						fit = auto.arima(data.prd.use,xreg=data.index.use2,d=2,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use2)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "Day in October", xlim = c(1,45), ylim = c(1.6,4),lwd = 2)
						title(sub = paste("ARIMAexTest2 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						cat("\nARIMAexTest2 --",fit.err)

						fit = auto.arima(data.prd.use,xreg=data.index.use3,d=2,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use3)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "Day in October", xlim = c(1,45), ylim = c(1.6,4),lwd = 2)
						title(sub = paste("ARIMAexTest3 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						cat("\nARIMAexTest3 --",fit.err)

						fit = auto.arima(data.prd.use,xreg=data.index.use4,d=2,stepwise=FALSE)
						fitp = forecast(fit,n.ahead,xreg = data.index.f.use4)
						plot(fitp, ylab = "WL Chaophaya (m.MSL)",xlab = "Day in October", xlim = c(1,45), ylim = c(1.6,4),lwd = 2)
						title(sub = paste("ARIMAexTest1 WL at Navy Headquarter - WooWooWuu.de - RMSQ error=",round(fit.err,digits = 2)," m."))
						cat("\nARIMAexTest4 --",fit.err)

dev.off()

#######################################################
######## ENDE #########################################
#######################################################

cat("\nEND")
setwd(dir0)

