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

p2 = "GCM"
p3 = "Ocean Index"

prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)


########################define file for input / read ################################
## obs
cat("\nReading Obs")
#data <- read.table("Daten_Rayong1971-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("1.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
#data <- read.table("monthly-obs_filled1971-2006(2100).csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
data <- read.table("monthly-obs_filled1971-2006(2100)+SST.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#data <- read.table("test-monthly-obs_filled1971-2006(2100)+SST.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")




## GCM
cat("\nReading GCM")
#gcm <- read.table("ECHO-G monthly 1971-1999-2100_A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#gcm1 <- read.table("ECHO-G monthly 1971-1999-2100_A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#gcm2 <- read.table("ECHO-G monthly 1971-1999-2100_A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
#gcm3 <- read.table("ECHO-G monthly 1971-1999-2100_A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
gcm <- read.table("ECHO-G monthly 1971-1999-2070_A1B+GCMs+HiRes.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
gcm1 <- read.table("ECHO-G monthly 1971-1999-2070_A1B+GCMs+HiRes.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
gcm2 <- read.table("ECHO-G monthly 1971-1999-2070_A1B+GCMs+HiRes.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")
gcm3 <- read.table("ECHO-G monthly 1971-1999-2070_A1B+GCMs+HiRes.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")



## ocean index
cat("\nReading ocean index")
oindex <- read.table("ocean index.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")

## order ARIMA
cat("\nReading ARIMA order")
order.m <- read.table("ARMA pdq optimal 1971-2000.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-")


#attach(data)

#################### define input column by header/title/working folder####################
#specify station (from here on need to rerun for each station again)

#station=X48459_MAX # main station to check end of file
#station=ObsMnT
#station_name="ObsRn"
#station_name="X48459_MAX"
#station_name="ObsMnT"

#########################--Rayong-- Change directory path###############################
#output goes to subdirectory with name of station

dir_name="test"


L=file.exists(dir_name)  
if(!L) dir.create(dir_name)
dir0=dirname(file.path("D:","Temp","arima","dummy"))
dir1=dirname(file.path("D:","Temp","arima",paste(dir_name),"dummy"))

##########################################The stuff above needs to be processed only once.################################



#################### observed data ####################
months=c("Jan","Feb","Mar","Apr","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#begin (-999) and end (999) of series
#istart=which(station==NA)+1 
#if(length(istart)==0){istart=1}

#ifinal=which(station==999)-1
#nmax=ifinal-istart +1 
#yearstart=year[istart]
#monthstart=which(months==month[istart])
#ts=station[istart:ifinal]
#############################this is a first approach to fix missing data, needs reconsideration
#tsub=substituteNA(ts, type = "mean")
#str(tsub)
#tseries=ts(as.vector(tsub),start=c(yearstart,monthstart),frequency=12)
#t=time(tseries)

############# max t is here #########################
#tfinal = max(t)

############# year of observation #########################
### Calibration
start0.c=1971  #year start in table file
startyear.c=1971 #year to analysis begins
endyear.c=1999
endmonth.c=0
### Verification
start0.v = 1971  #year start in table file
startyear.v = 2000 #year to analysis begins
endyear.v = 2004 # Actually limit only 2000 (nprd = 180 =15*12) but 1 year more for lagging data (shifting part)
endmonth.v = 0

# To run scenarios
gcm.n = 1 # GCMs scenarios to run
gcm.title = c("c20m")
#gcm.n = 3 # GCMs scenarios to run
#gcm.title = c("A1B","A2","B1")

ifinal.c=min(which(data$year==(endyear.c+1)))-1 + endmonth.c
datafinal.c = length(data$year)

#ifinal.v = min(which(data$year==(endyear.v+1)))-1 + endmonth.v
ifinal.v = min(which(data$year==(endyear.v+1+1)))-1 + endmonth.v # add 1 more year for lag
datafinal.v = length(data$year)

######################################################################################
########################define file for comparison ###################################
######################################################################################
#Column of parameters specific


######################################################################################
#******************** remove unwanted column here ##################################
######################################################################################
#### column to remove
drmv1 = c(1,2) # Data1 Obs
drmv2 = c(1,2) # Data1 GCM
drmv3 = c(1,2) # Data3 Ocean Index
####

subty.c = (startyear.c-start0.c)*12
#P1 Obs
data01 = data[-c(0:subty.c,(ifinal.c+1):datafinal.c),-drmv1]

#P2 GCM
data02 = gcm[-c(0:subty.c,(ifinal.c+1):datafinal.c),-drmv2]
gcm.nprd = dim(data02)[2]

#P3 Ocen Index
data03 = oindex[-c(0:subty.c,(ifinal.c+1):datafinal.c),-drmv3]
ocean.nprd = dim(data03)[2]



nparmat2 = length(colnames(data02))
nparmat3 = length(colnames(data03))



######******************** data for validation here ##################################
subty.v = (startyear.v-start0.v)*12
#P1
vdata01=data[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv1]
#P2
vdata02=gcm[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv2]
vdata021=gcm1[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv2]
vdata022=gcm2[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv2]
vdata023=gcm3[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv2]
#P3
vdata03=oindex[-c(0:subty.v,(ifinal.v+1):datafinal.v),-drmv3]

######################################################################################


#####################
## time sieries
#####################

#P1 Obs
data1=  ts(data01, frequency=12,start=c(1971,1))
vdat1=  ts(vdata01, frequency=12,start=c(startyear.v,1))

#P2 Obs

data2= ts(data02, frequency=12,start=c(1971,1))
vdat21= ts(vdata021, frequency=12,start=c(startyear.v,1))
vdat22= ts(vdata022, frequency=12,start=c(startyear.v,1))
vdat23= ts(vdata023, frequency=12,start=c(startyear.v,1))

#P3
data3= ts(data03, frequency=12,start=c(1971,1))
vdat3= ts(vdata03, frequency=12,start=c(startyear.v,1))


#nprd = 180 # number of month for forward prediction (nPredictied)
nprd = ifinal.v-ifinal.c-12 # number of month for forward prediction (nPredictied) # remove 1 year for lag

#obsn = 32+13
obsn = ncol(data01)



##############################
###### AR prediction #########
##############################

cat("\n## AR\n")
mth = "ols"
ord = c(12,24)
case = length(ord)
count = 0

ar.result = array(NA, c(dim(data1)[1]+nprd,obsn*case)) ## result file
dat.col = data[(subty.c+1):dim(ar.result)[1],drmv1] ## result file
ar.result = cbind(dat.col,ar.result) ## result file

rsar = array(NA, c(obsn,case,44))
#pdar = array(NA, c(obsn,case,44))

#Prediction charts
setwd(dir1)
pdf(paste("ARp-charts.pdf"))
setwd(dir0)


leg.txt <- c("prediction", "observation")
par(ps = 10,mfrow = c(case,1))

for(i in 1:obsn){
			prd.i = prd.type[i]
			for(j in 1:case){
						nona = which(is.na(data1[,i])== FALSE)
						startdat = nona[1]
						dat = ts(data1[nona,],frequency = 12, start= tsp(data1)[1]+(startdat-1)/12)
						fit = ar(dat[,i],order.max=ord[j],method=mth)
						rsar[i,j,1] = colnames(data1)[i]
						rsar[i,j,2] = mth
						rsar[i,j,3] = ord[j]
						rsar[i,j,4] = fit$order
						rsar[i,j,5] = fit$var.pred
						nonars = which(is.na(fit$resid) == FALSE)
						rsar[i,j,6] = mean(fit$resid[nonars])
						rsar[i,j,7] = sqrt(mean((fit$resid[nonars])^2))
						rsar[i,j,8] = 1-sum((fit$resid[nonars])^2)/sum((data1[nona[nonars],i]-mean(data1[nona[nonars],i]))^2)
						rsar[i,j,9] = fit$n.used
						rsar[i,j,10] = length(nonars)
						fitp = forecast(fit,nprd)
						rsar[i,j,11] = nprd
						resid = fitp$mean-vdat1[,i]
						rsar[i,j,12] = mean(resid)
						rsar[i,j,13] = sqrt(mean((resid)^2))
						rsar[i,j,14] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)
						for(k in 1:10)  {
									rsar[i,j,15+(k-1)*3] = mean(resid[1:(k*6)])
									rsar[i,j,16+(k-1)*3] = sqrt(mean((resid[1:(k*6)])^2))
									rsar[i,j,17+(k-1)*3] = 1-sum((resid[1:(k*6)])^2)/sum((vdat1[1:(k*6),i]-mean(vdat1[1:(k*6),i]))^2)
								    }
						#Plot charts
										if(prd.i==1) plot(fitp,1,ylim=c(29,37))
										if(prd.i==2) plot(fitp,1,ylim=c(10,40))
										if(prd.i==3) plot(fitp,1,ylim=c(15,35))
										if(prd.i>3) plot(fitp,1)
						title(sub = paste(rsar[i,j,1],"/ NS(verification)=",format(as.numeric(rsar[i,j,14]),digits=3,scientific=FALSE)))
						lines(vdat1[,i],col = "Green",lty = "dashed")
						legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")
										count = count+1 # count the loop
										print(paste("AR#",i,j,rsar[i,j,1]," NS=",format(as.numeric(rsar[i,j,14])))) # count the loop print out the running progress

						# record prediction
						pr.ln = length(c(fit$resid+dat[,i],fitp$mean))
						ar.result[(dim(data1)[1]+nprd-pr.ln+1):(dim(data1)[1]+nprd),case*(i-1)+j+2] = c(fit$resid+dat[,i],fitp$mean) ## result file
						colnames(ar.result)[case*(i-1)+j+2] = paste(colnames(data1)[i],fit$order,sep="-") ## result file
				
						}
		 	}

dev.off()


# Write prediction results
setwd(dir1) ## result file
write.csv(ar.result, file = "AR prediction-all.csv", row.names = FALSE) ## result file
setwd(dir0) ## result file


#Calibration
cat("\n- AR calibration")
setwd(dir1)
sink(paste("ARc.csv"))
setwd(dir0)

cat("Obs,method,max_order,order,PredictionVariance,meanResid,rmsResid,Nash–Sutcliffe,n,nCalibrated,nPredicted,pMean-all,pRMS-all,pNS-all\n")

for(i in 1:3){
		for(j in 1:case){
						for(l in 1:14)cat(paste(rsar[i,j,l],","))
						cat("\n")
				}
		}
sink()

#Prediction
cat("\n- AR prediction")
setwd(dir1)
sink(paste("ARp.csv"))
setwd(dir0)

cat("Obs,order,period,n,Mean,RMS,NS\n")

for(i in 1:3){
		for(j in 1:case){
					cat(rsar[i,j,1],",",rsar[i,j,4],",calibrated,",rsar[i,j,10],",",rsar[i,j,6],",",rsar[i,j,7],",",rsar[i,j,8],"\n")
					cat(rsar[i,j,1],",",rsar[i,j,4],",predicted-all,",rsar[i,j,11],",",rsar[i,j,12],",",rsar[i,j,13],",",rsar[i,j,14],"\n")
					for(k in 1:10) cat(rsar[i,j,1],",",rsar[i,j,4],",predicted,",k*6,",",rsar[i,j,15+(k-1)*3],",",rsar[i,j,16+(k-1)*3],",",rsar[i,j,17+(k-1)*3],"\n")
					}
		}
sink()

#dev.off()


##############################
###### ARIMA prediction #########
##############################

cat("\n\n## ARIMA\n")

###################### To select all indicator ###############
##############################################################


prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)
ord = array(NA,c(3,length(unique(prd.type)))) # set of optimal order in ARIMA (Tmax, Tmin , Rain, SST)
#ord = array(c(c(1,1,2),c(0,1,1),c(2,0,2),c(1,1,2)),c(3,length(unique(prd.type)))) # set of optimal order in ARIMA (Tmax, Tmin , Rain, SST)
case = 4 #stepwiseTRUE/stepwiseFALSE, define d,not define d,
count = 0

arma.result = array(NA, c(dim(data1)[1]+nprd,obsn*case)) ## result file
dat.col = data[(subty.c+1):dim(arma.result)[1],drmv1] ## result file
arma.result = cbind(dat.col,arma.result) ## result file

rsarm = array(NA, c(obsn,case,54)) # output each simulations
optimal.arma = array(NA, c(obsn,4))
colnames(optimal.arma) = c("sta","p","d","q")

#Prediction charts
setwd(dir1)
pdf(paste("ARIMAp-charts.pdf"))
setwd(dir0)
leg.txt <- c("prediction", "observation")
par(ps = 10,mfrow = c(2,1))

for(i in 1:obsn){

			prd.i = prd.type[i]
			sta.sel = which(order.m[,1]==colnames(dat)[i])
			ord[1,prd.i] = order.m$p[sta.sel] # p
			ord[2,prd.i] = order.m$d[sta.sel] # d
			ord[3,prd.i] = order.m$q[sta.sel] # q

			for(j in 1:case){
						#nona = which(is.na(data2[,i])== FALSE)
										if(j==1) datmat = data1
										if(j==2) datmat = data1
										nona = which(is.na(datmat[,i])== FALSE )
										startdat = nona[1]
										dat = ts(datmat[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12)
						#if(j==1) fit = auto.arima(data1[,i],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=TRUE)
						#if(j==2) fit = auto.arima(data1[,i],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=FALSE)
										if(j==1) fit = auto.arima(dat[,i],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=TRUE)
										if(j==2) fit = auto.arima(dat[,i],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=FALSE)
										if(j==3) fit = auto.arima(dat[,i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=TRUE) # no d defined
										if(j==4) fit = auto.arima(dat[,i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=FALSE)# no d defined
										#if(j==5) fit = auto.arima(dat[,i],stepwise=FALSE)# no d defined
						#rsarm[i,j,1] = colnames(data1)[i]
										rsarm[i,j,1] = colnames(dat)[i]
						rsarm[i,j,2] = j-1
						rsarm[i,j,3] = (k-1)*2
						rsarm[i,j,4] = fit$arma[1] #p
						rsarm[i,j,5] = fit$arma[6] #d
						rsarm[i,j,6] = fit$arma[2] #q
						rsarm[i,j,7] = fit$arma[5] #order
						rsarm[i,j,8] = fit$coef[1]
						rsarm[i,j,9] = fit$coef[2]
						rsarm[i,j,10] = fit$coef[3]
						rsarm[i,j,11] = fit$coef[5]
						nonars = which(is.na(fit$residuals) == FALSE)
						rsarm[i,j,12] = mean(fit$residuals[nonars])
						rsarm[i,j,13] = sqrt(mean((fit$residuals[nonars])^2))
						#rsarm[i,j,14] = 1-sum((fit$residuals[nonars])^2)/sum((data1[nonars,i]-mean(data1[nonars,i]))^2)
										rsarm[i,j,14] =1-sum((fit$residuals[nonars])^2)/sum((dat[nonars,i]-mean(dat[nonars,i]))^2)
						rsarm[i,j,15] = fit$sigma2
						rsarm[i,j,16] = fit$aic
						rsarm[i,j,17] = fit$aicc
						rsarm[i,j,18] = fit$bic
						rsarm[i,j,19] = length(nona)
						rsarm[i,j,20] = length(nonars)

						fitp = forecast(fit,nprd)
						rsarm[i,j,21] = nprd
						resid = fitp$mean-vdat1[,i]
						rsarm[i,j,22] = mean(resid)
						rsarm[i,j,23] = sqrt(mean((resid)^2))
						rsarm[i,j,24] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)
						for(k in 1:10)  {
									rsarm[i,j,25+(k-1)*3] = mean(resid[1:(k*6)])
									rsarm[i,j,26+(k-1)*3] = sqrt(mean((resid[1:(k*6)])^2))
									rsarm[i,j,27+(k-1)*3] = 1-sum((resid[1:(k*6)])^2)/sum((vdat1[1:(k*6),i]-mean(vdat1[1:(k*6),i]))^2)
								    }
						#Plot charts
										#if(prd.i==1) plot(fitp,1,ylim=c(29,37))
										#if(prd.i==2) plot(fitp,1,ylim=c(15,35))
										#if(prd.i==3) plot(fitp,1,ylim=c(15,35))
										#if(prd.i>3) plot(fitp,1)
										plot(fitp,1)
						if(j==1) title(sub = paste(rsarm[i,j,1]," by stepwise fitting","/ NS(verification)=",format(as.numeric(rsarm[i,j,24]),digits=3,scientific=FALSE)))
						if(j==2) title(sub = paste(rsarm[i,j,1]," by non-stepwise fitting","/ NS(verification)=",format(as.numeric(rsarm[i,j,24]),digits=3,scientific=FALSE)))
						if(j==3) title(sub = paste(rsarm[i,j,1]," by no d,stepwise fitting","/ NS(verification)=",format(as.numeric(rsarm[i,j,24]),digits=3,scientific=FALSE)))
						if(j==4) title(sub = paste(rsarm[i,j,1]," by no d,non-stepwise fitting","/ NS(verification)=",format(as.numeric(rsarm[i,j,24]),digits=3,scientific=FALSE)))

						lines(vdat1[,i],col = "Green",lty = "dashed")
						legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")
										count = count+1 # count the loop
										print(paste("ARIMA#",i,j,rsarm[i,j,1],":",rsarm[i,j,4],rsarm[i,j,5],rsarm[i,j,6]," NS=",format(as.numeric(rsarm[i,j,24])))) # count the loop print out the running progress

						# record prediction
						pr.ln = length(c(fit$resid+dat[,i],fitp$mean))
						arma.result[(dim(data1)[1]+nprd-pr.ln+1):(dim(data1)[1]+nprd),case*(i-1)+j+2] = c(fit$resid+dat[,i],fitp$mean) ## result file
						colnames(arma.result)[case*(i-1)+j+2] = paste(colnames(data1)[i],j,sep="-") ## result file
						}
			optimal.arma[i,1] = colnames(dat)[i]
			sel.case = which.max(rsarm[i,,24])
			optimal.arma[i,2] = rsarm[i,sel.case,4]
			optimal.arma[i,3] = rsarm[i,sel.case,5]
			optimal.arma[i,4] = rsarm[i,sel.case,6]

		 	}
dev.off()

# Write prediction results
setwd(dir1) ## result file
write.csv(arma.result, file = "ARIMA prediction-all.csv", row.names = FALSE) ## result file
setwd(dir0) ## result file


setwd(dir1)
write.csv(optimal.arma, file= "optimal arma pdq.csv", row.names = FALSE)
setwd(dir0)


cat("\nARIMA results recording")
#Calibration
setwd(dir1)
sink(paste("ARIMAc.csv"))
setwd(dir0)

cat("Obs,max_p,start_q,p,d,q,order,ar1,ar2,ma1,reg,meanResid,rmsResid,Nash–Sutcliffe,sigma2,AIC,AICc,BIC,n,nCalibrated,nPredicted,pMean-all,pRMS-all,pNS-all\n")
for(i in 1:obsn){
		for(j in 1:case){
					for(k in 1:24) cat(paste(rsarm[i,j,k],","))
					cat("\n")
					}
		 }
sink()

#Prediction
setwd(dir1)
sink(paste("ARIMAp.csv"))
setwd(dir0) 

cat("Obs,p,d,q,order,period,n,Mean,RMS,NS\n")

for(i in 1:obsn){
		for(j in 1:case){
					cat(rsarm[i,j,1],",",rsarm[i,j,4],",",rsarm[i,j,5],",",rsarm[i,j,6],",",rsarm[i,j,7],",calibrated,",rsarm[i,j,20],",",rsarm[i,j,12],",",rsarm[i,j,13],",",rsarm[i,j,14],"\n")
					cat(rsarm[i,j,1],",",rsarm[i,j,4],",",rsarm[i,j,5],",",rsarm[i,j,6],",",rsarm[i,j,7],",predicted-all,",rsarm[i,j,21],",",rsarm[i,j,22],",",rsarm[i,j,23],",",rsarm[i,j,24],"\n")
					for(k in 1:10) cat(rsarm[i,j,1],",",rsarm[i,j,4],",",rsarm[i,j,5],",",rsarm[i,j,6],",",rsarm[i,j,7],",predicted,",k*6,",",rsarm[i,j,25+(k-1)*3],",",rsarm[i,j,26+(k-1)*3],",",rsarm[i,j,27+(k-1)*3],"\n")
					}
		}
sink()







#########################################
###### ARIMAexternal Prediction    ######
#########################################

cat("\n\nARIMAexternal prediction\n")

### Change to A1B
#data <- read.table("Daten_Rayong1971-2100+Index2+CGCM3A1B.csv", stringsAsFactors=FALSE,header=TRUE, sep=",",dec=".",na.strings="-") 
###

count = 0
prd.type = c(rep(c(1,2),4),rep(3,24),rep(4,13)) # fit to ord, to use optimal order which is put in "ord"(Tmax, Tmin , Rain, SST)
ord = array(c(c(1,1,3),c(1,1,2),c(3,0,1),c(1,1,2)),c(3,length(unique(prd.type)))) # set of optimal order in ARIMA (Tmax, Tmin , Rain, SST)



lag.n = 7 # number of Lag of Ocean Indices
case = gcm.n+lag.n # case to run
step.rec.month = 3 # month of step to record prediction performance
step.rec.n = nprd/step.rec.month # month of step to record prediction performance

lag.title = c(0:(lag.n-1))

# Define External regressors
extd = array(NA, c(case,max(dim(data2)[2],dim(data3)[2]))) # set of selected parameters

p2.ext.col = 1:dim(data2)[2] # set parameters (by column) for ARIMAex for Data2#GCM_A1B,A2,B1
for(p in 1:gcm.n){
extd[p,1:length(p2.ext.col)] = p2.ext.col  # set by column for Data2#GCM_xx
}

p3.ext.col = 1:13 # set parameters (by column) for ARIMAex for Data3#Index Lag 0..xx
for(p in (gcm.n+1):(gcm.n+lag.n)){
extd[p,1:length(p3.ext.col)] = p3.ext.col  # set by column for Data3#Index Lag_XX
}

total.run = length(which(!is.na(extd)))

arimaex.gcm.result = array(NA, c(dim(data1)[1]+nprd,obsn*(gcm.nprd+ocean.nprd*lag.n))) ## result file
arimaex.ocean.result = array(NA, c(dim(data1)[1]+nprd,obsn*(ocean.nprd*lag.n))) ## result file
dat.col = data[(subty.c+1):dim(arimaex.gcm.result)[1],drmv1] ## result file
arimaex.gcm.result = cbind(dat.col,arimaex.gcm.result) ## result file
arimaex.ocean.result = cbind(dat.col,arimaex.ocean.result) ## result file

#extd[1,1:5] = c(4,6,13,33,47)  
#extd[1,1:(length(colnames(data2))-3+1)] = c(length(colnames(data2))-3,4:(length(colnames(data2)))) # first digit is number of external data # first position = number of parameters, the afterward numbers = regression paramters in columns of data2
#extd[2,1:(length(colnames(data3))-3+1)] = c(length(colnames(data3))-3,4:(length(colnames(data3)))) # first digit is number of external data # first position = number of parameters, the afterward numbers = regression paramters in columns of data3

#rsarmex1 = array(NA, c(obsn,case,max(extd[,1]),53)) # output each simulations
rsarmex1 = array(NA, c(obsn,case,dim(extd)[2],26+(step.rec.n-1)*3))

#Prediction charts
setwd(dir1)
pdf(paste("ARIMAexP-charts.pdf"))
setwd(dir0)

leg.txt <- c("prediction", "observation")

for(i in 1:obsn){
			prd.i = prd.type[i]

			ord[1,prd.i] = as.integer(optimal.arma[i,2]) # p
			ord[2,prd.i] = as.integer(optimal.arma[i,3]) # d
			ord[3,prd.i] = as.integer(optimal.arma[i,4]) # q

			par(ps = 10,mfrow = c(2,1))



			#######################
			# Case of GCM
			datmat = data1
			nona.1 = which(!is.na(datmat[,i]))
			datmatex = data2 # GCM
			prd.n = length(which(!is.na(extd[1,])))
			for(k in 1:prd.n){
						for(j in 1:gcm.n){
										if(j == 1){
												nona = nona.1[which(!is.na(datmatex[nona.1,extd[j,k]]))]
												startdat = nona[1]

												dat = ts(datmat[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12) # Obs
												datex = ts(datmatex[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12) # Regressor

												fit = auto.arima(dat[,i],xreg=datex[,extd[j,k]],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=TRUE)
												}

										rsarmex1[i,j,k,1] = colnames(dat)[i]
										rsarmex1[i,j,k,2] = paste(colnames(datex)[extd[j,k]],gcm.title[j])
										rsarmex1[i,j,k,3] = fit$arma[1] #p
										rsarmex1[i,j,k,4] = fit$arma[6] #d
										rsarmex1[i,j,k,5] = fit$arma[2] #q
										rsarmex1[i,j,k,6] = fit$arma[5] #order
										rsarmex1[i,j,k,7] = fit$coef[1]
										rsarmex1[i,j,k,8] = fit$coef[2]
										rsarmex1[i,j,k,9] = fit$coef[3]
										rsarmex1[i,j,k,10] = fit$coef[5]
										nonars = which(is.na(fit$residuals) == FALSE)
										rsarmex1[i,j,k,11] = mean(fit$residuals[nonars])
										rsarmex1[i,j,k,12] = sqrt(mean((fit$residuals[nonars])^2))
										rsarmex1[i,j,k,13] = 1-sum((fit$residuals[nonars])^2)/sum((dat[nonars,i]-mean(dat[nonars,i]))^2)
										rsarmex1[i,j,k,14] = fit$sigma2
										rsarmex1[i,j,k,15] = fit$aic
										rsarmex1[i,j,k,16] = fit$aicc
										rsarmex1[i,j,k,17] = fit$bic
										rsarmex1[i,j,k,18] = length(nona)
										rsarmex1[i,j,k,19] = length(nonars)

										# Verification
										#if(j==1) fitp = forecast(fit,nprd,xreg = vdat2[,extd[j,k]])
										#if(j==2) fitp = forecast(fit,nprd,xreg = vdat3[,extd[j,k]])
										if(j==1) fitp = forecast(fit,nprd,xreg = vdat21[1:nprd,extd[j,k]])
										if(j==2) fitp = forecast(fit,nprd,xreg = vdat22[1:nprd,extd[j,k]])
										if(j==3) fitp = forecast(fit,nprd,xreg = vdat23[1:nprd,extd[j,k]])
										if(j==4) fitp = forecast(fit,nprd,xreg = vdat3[1:nprd,extd[j,k]])


										rsarmex1[i,j,k,20] = nprd
										resid = fitp$mean-vdat1[,i]
										rsarmex1[i,j,k,21] = mean(resid)
										rsarmex1[i,j,k,22] = sqrt(mean((resid)^2))
										rsarmex1[i,j,k,23] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)
										for(l in 1:step.rec.n)  {
													rsarmex1[i,j,k,24+(l-1)*3] = mean(resid[1:(l*step.rec.month)])
													rsarmex1[i,j,k,25+(l-1)*3] = sqrt(mean((resid[1:(l*step.rec.month)])^2))
													rsarmex1[i,j,k,26+(l-1)*3] = 1-sum((resid[1:(l*step.rec.month)])^2)/sum((vdat1[1:(l*step.rec.month),i]-mean(vdat1[1:(l*step.rec.month),i]))^2)
												    }

										#Plot charts
										#if(prd.i==1) plot(fitp,1,ylim=c(20,50))
										#if(prd.i==2) plot(fitp,1,ylim=c(10,40))
										#if(prd.i==3) plot(fitp,1,ylim=c(0,100))
										#if(prd.i>3) plot(fitp,1)
										plot(fitp,1)

										#if(j==1) title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))
										#if(j==2) title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))
										title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))

										lines(vdat1[,i],col = "Green",lty = "dashed")
										legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")

										# record prediction
										pr.ln = length(c(fit$resid+dat[,i],fitp$mean))
										col.i = (i-1)*(gcm.n*gcm.nprd)+(j-1)*gcm.nprd+k
										arimaex.gcm.result[(dim(data1)[1]+nprd-pr.ln+1):(dim(data1)[1]+nprd),col.i+2] = c(fit$resid+dat[,i],fitp$mean) ## result file
										colnames(arimaex.gcm.result)[col.i+2] = paste(colnames(data1)[i],rsarmex1[i,j,k,2],sep="-") ## result file
										cat(" step -",col.i)

											count = count+1 # count the loop
											print(paste("ARIMAex1#",i,j,k,rsarmex1[i,j,k,1],"--",rsarmex1[i,j,k,2],":",rsarmex1[i,j,k,3],rsarmex1[i,j,k,4],rsarmex1[i,j,k,5]," NS=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)," ##",format(count/total.run/obsn*100,digits=2,scientific=FALSE),"%")) # count the loop print out the running progress

										}

						}


			#######################
			# Case of Ocean Index
			datmat = data1
			nona.1 = which(!is.na(datmat[,i]))
			datmatex = data3 # SST
			prd.n = length(which(!is.na(extd[4,])))

			j.start = gcm.n+1
			j.end = gcm.n+lag.n

			for(k in 1:prd.n){

						#check predictor = predictant
						if(colnames(datmatex)[extd[j,k]] != colnames(dat)[i]){
						#cat(colnames(datmatex)[extd[j,k]],":",colnames(dat)[i])
						#cat("\nPredicting by Ocean Index")
						for(j in j.start:j.end){
										lag.t = j-gcm.n-1
										if(lag.t>=1){
												rmv.lag = 1:lag.t # remove month-lag (lag n month) from data
												datmatex = rbind(data3,vdat3)[-rmv.lag,][-c(0:subty.c,(ifinal.c+1):datafinal.c),] # create new lag data
												}
												else{datmatex = data3} 

										nona = nona.1[which(!is.na(datmatex[nona.1,extd[j,k]]))]
										startdat = nona[1]

										dat = ts(datmat[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12) # Obs
										datex = ts(datmatex[nona,],frequency = 12, start= tsp(datmat)[1]+(startdat-1)/12) # Regressor

										fit = auto.arima(dat[,i],xreg=datex[,extd[j,k]],d=ord[2,prd.i],start.p=ord[1,prd.i],start.q=ord[3,prd.i],stepwise=TRUE)
										rsarmex1[i,j,k,1] = colnames(dat)[i]
										rsarmex1[i,j,k,2] = paste(colnames(datex)[extd[j,k]]," lag-",lag.title[j-gcm.n],sep="")
										rsarmex1[i,j,k,3] = fit$arma[1] #p
										rsarmex1[i,j,k,4] = fit$arma[6] #d
										rsarmex1[i,j,k,5] = fit$arma[2] #q
										rsarmex1[i,j,k,6] = fit$arma[5] #order
										rsarmex1[i,j,k,7] = fit$coef[1]
										rsarmex1[i,j,k,8] = fit$coef[2]
										rsarmex1[i,j,k,9] = fit$coef[3]
										rsarmex1[i,j,k,10] = fit$coef[5]
										nonars = which(is.na(fit$residuals) == FALSE)
										rsarmex1[i,j,k,11] = mean(fit$residuals[nonars])
										rsarmex1[i,j,k,12] = sqrt(mean((fit$residuals[nonars])^2))
										rsarmex1[i,j,k,13] = 1-sum((fit$residuals[nonars])^2)/sum((dat[nonars,i]-mean(dat[nonars,i]))^2)
										rsarmex1[i,j,k,14] = fit$sigma2
										rsarmex1[i,j,k,15] = fit$aic
										rsarmex1[i,j,k,16] = fit$aicc
										rsarmex1[i,j,k,17] = fit$bic
										rsarmex1[i,j,k,18] = length(nona)
										rsarmex1[i,j,k,19] = length(nonars)

										# Verification
										#vdat.lag = lag(vdat3, k = lag.t)[-rmv.lag,]
										if(lag.t>=1){vdat.lag = vdat3[-rmv.lag,]
													}else{vdat.lag = vdat3}

										fitp = forecast(fit,nprd,xreg = vdat.lag[1:nprd,extd[j,k]])


										rsarmex1[i,j,k,20] = nprd
										resid = fitp$mean-vdat1[,i]
										rsarmex1[i,j,k,21] = mean(resid)
										rsarmex1[i,j,k,22] = sqrt(mean((resid)^2))
										rsarmex1[i,j,k,23] = 1-sum((resid)^2)/sum((vdat1[1:nprd,i]-mean(vdat1[1:nprd,i]))^2)
										for(l in 1:step.rec.n)  {
													rsarmex1[i,j,k,24+(l-1)*3] = mean(resid[1:(l*step.rec.month)])
													rsarmex1[i,j,k,25+(l-1)*3] = sqrt(mean((resid[1:(l*step.rec.month)])^2))
													rsarmex1[i,j,k,26+(l-1)*3] = 1-sum((resid[1:(l*step.rec.month)])^2)/sum((vdat1[1:(l*step.rec.month),i]-mean(vdat1[1:(l*step.rec.month),i]))^2)
												    }

										#Plot charts
										#if(prd.i==1) plot(fitp,1,ylim=c(20,50))
										#if(prd.i==2) plot(fitp,1,ylim=c(10,40))
										#if(prd.i==3) plot(fitp,1,ylim=c(0,100))
										#if(prd.i>3) plot(fitp,1)
										plot(fitp,1)

										#if(j==1) title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))
										#if(j==2) title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))
										title(sub = paste(rsarmex1[i,j,k,1],"regressed to '",rsarmex1[i,j,k,2],"'/ NS(verification)=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)))

										lines(vdat1[,i],col = "Green",lty = "dashed")
										legend(2001,min(fitp$mean[1:24],vdat1[1:24,i])+0.5,legend = leg.txt, col=c("blue","green"), lty=c("solid","dashed"), merge=FALSE, bty = "n")



										# record prediction
										pr.ln = length(c(fit$resid+dat[,i],fitp$mean))
										col.i = (i-1)*(ocean.nprd*lag.n)+(k-1)*lag.n+j-gcm.n
										arimaex.ocean.result[(dim(data1)[1]+nprd-pr.ln+1):(dim(data1)[1]+nprd),col.i+2] = c(fit$resid+dat[,i],fitp$mean) ## result file
										colnames(arimaex.ocean.result)[col.i+2] = paste(colnames(data1)[i],colnames(datex)[extd[j,k]],"lag",lag.title[j-gcm.n],sep="-") ## result file
										cat(" step -",col.i)

											count = count+1 # count the loop
											print(paste("ARIMAex2#",i,j,k,rsarmex1[i,j,k,1],"--",rsarmex1[i,j,k,2],":",rsarmex1[i,j,k,3],rsarmex1[i,j,k,4],rsarmex1[i,j,k,5]," NS=",format(as.numeric(rsarmex1[i,j,k,23]),digits=3,scientific=FALSE)," ##",format(count/total.run/obsn*100,digits=2,scientific=FALSE),"%")) # count the loop print out the running progress


										}

						}#END for(j in j.start:j.end)
						}#END check predictor = predictant if(colnames(datex)[extd[j,k]] != colnames(dat)[i])




		 	}
dev.off()

# Write prediction results
setwd(dir1) ## result file
write.csv(arimaex.gcm.result, file = "ARIMAex prediction-gcm.csv", row.names = FALSE) ## result file
write.csv(arimaex.ocean.result, file = "ARIMAex prediction-ocean.csv", row.names = FALSE) ## result file
setwd(dir0) ## result file


#Calibration
setwd(dir1)
sink(paste("ARIMAexC.csv"))
setwd(dir0)

cat("Obs,Predictor,p,d,q,order,ar1,ar2,ma1,reg,meanResid,rmsResid,Nash–Sutcliffe,sigma2,AIC,AICc,BIC,n,nCalibrated,nPredicted,pMean-all,pRMS-all,pNS-all\n")
for(i in 1:obsn){
		for(j in 1:case){
					for(k in 1:extd[j,1])	{
									for(l in 1:23) cat(paste(rsarmex1[i,j,k,l],","))
									cat("\n")
									}
					}
		 }
sink()

#Prediction
setwd(dir1)
sink(paste("ARIMAexP.csv"))
setwd(dir0)



cat("Obs,predictor,p,d,q,order,period,n(month),Mean,RMS,NS\n")

for(i in 1:obsn){
		for(j in 1:case){
					for(k in 1:extd[j,1])	{
									cat(rsarmex1[i,j,k,1],",",rsarmex1[i,j,k,2],",",rsarmex1[i,j,k,3],",",rsarmex1[i,j,k,4],",",rsarmex1[i,j,k,5],",",rsarmex1[i,j,k,6],",calibrated,",rsarmex1[i,j,k,18],",",rsarmex1[i,j,k,11],",",rsarmex1[i,j,k,11],",",rsarmex1[i,j,k,13],"\n")
									cat(rsarmex1[i,j,k,1],",",rsarmex1[i,j,k,2],",",rsarmex1[i,j,k,3],",",rsarmex1[i,j,k,4],",",rsarmex1[i,j,k,5],",",rsarmex1[i,j,k,6],",predicted-all,",rsarmex1[i,j,k,20],",",rsarmex1[i,j,k,21],",",rsarmex1[i,j,k,22],",",rsarmex1[i,j,k,23],"\n")
									for(l in 1:step.rec.n) cat(rsarmex1[i,j,k,1],",",rsarmex1[i,j,k,2],",",rsarmex1[i,j,k,3],",",rsarmex1[i,j,k,4],",",rsarmex1[i,j,k,5],",",rsarmex1[i,j,k,6],",predicted,",l*step.rec.month,",",rsarmex1[i,j,k,24+(l-1)*3],",",rsarmex1[i,j,k,25+(l-1)*3],",",rsarmex1[i,j,k,26+(l-1)*3],"\n")
									}
				}
		}
sink()


#######################################################
######## ENDE #########################################
#######################################################

setwd(dir0)

