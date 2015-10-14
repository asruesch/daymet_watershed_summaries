args = commandArgs(TRUE)
sStart = args[1]
sEnd = args[2]
options(warn=2)

datfiles="J:/daymet/climSummaries/allPeffPet2"
outputFolder="J:/daymet/climSummaries/modelClimateData"
fileList=list.files(datfiles)
setwd(datfiles)
daysfile="J:/daymet/climSummaries/daysLookup.csv"
days=read.csv(daysfile, header=TRUE)
lags = rbind(c(1, 3), c(4, 6), c(7, 9), c(10, 12), c(13, 24), c(25, 48))
logFile="J:/daymet/climSummaries/lagNA"
naFiles="J:/daymet/climSummaries/naFiles"
naFiles=paste(scan(naFiles), "_daily.RData", sep="")

###################################################################################
calcClimate=function(m, firstM, period){
if (firstM != 10) {# if we are not calculating water year data
	periodMonths=subset(monthlyClimateData, monthlyClimateData$month %in% m)
	newPet=aggregate(periodMonths$petTotal, by=list(periodMonths$year), sum)
	newPmean=aggregate(periodMonths$peffTotal, by=list(periodMonths$year), sum)
	newNdays=aggregate(periodMonths$numDays, by=list(periodMonths$year), sum)
} else {
	waterYearDat=monthlyClimateData
	waterYearDat$waterYear=ifelse(waterYearDat$month %in% c(1:9), waterYearDat$year, waterYearDat$year+1)
	waterYearDat2=subset(waterYearDat, waterYearDat$waterYear != 2012)
	newPet=aggregate(waterYearDat2$peffTotal, by=list(waterYearDat2$waterYear), sum)
	newPmean=aggregate(waterYearDat2$petTotal, by=list(waterYearDat2$waterYear), sum)
	newNdays=aggregate(waterYearDat2$numDays, by=list(waterYearDat2$waterYear), sum)
}
periodLag=subset(monthlyClimateData, monthlyClimateData$month==firstM)
output=data.frame(periodLag[,c(1:3,7:12)], newPet$x/newNdays$x, newPmean$x/newNdays$x)
period=period
colnames(output)[10:11]=c("pet", "pmean")
output=data.frame(output, period)
return(output)
}


###################################################################################
for (f in sStart:sEnd){
	start = proc.time()
	site = strsplit(fileList[f], "_")[[1]][1]
	outFile = paste(outputFolder, "/", site, "_modelData.RData", sep="")
	# naFlag=any(is.na(c(monthly$pmean, monthly$pet)))
	# nanFlag=any(is.nan(c(monthly$pmean, monthly$pet)))
	naFileFlag=fileList[f] %in% naFiles
	# if (naFlag | nanFlag | naFileFlag) {
	if (naFileFlag) {
		end = proc.time() - start
		print(paste(site, "took", end[[3]], "seconds to run"))
		write(site, file=logFile, append=T)	
		next
	} 
	complete = file.exists(outFile)
	if (complete) {
		end = proc.time() - start
		print(paste(site, "took", end[[3]], "seconds to run"))	
		next }
	load(fileList[f])
	if (is.null(monthly)) {
		end = proc.time() - start
		print(paste(site, "took", end[[3]], "seconds to run"))
		write(site, file=logFile, append=T)	
		write(site, file=naFiles, append=T)
		next
	}	
	rows=nrow(lags)*12*32 #12 months times 32 years
	year=array(0, rows)
	month=array(0, rows)
	lagPeff=array(0, rows)
	lagStart=array(0, rows)
	monthly$peffTotal=monthly$pmean*days$numDays #can potentially not have this in data frame, same with below
	monthly$petTotal=monthly$pet*days$numDays #can potentially not have this in data frame, same with below
	monthly$numDays=days$numDays
	x=0
	for (r in 1:nrow(monthly)){ #could change this to 49:nrow, also would need to change rows, above
	#could also change this to just those rows with months 3, 4, 6, 8, 9, 10
		for (l in 1:nrow(lags)){
			x=x+1
			lagStart[x]=lags[l,1]
			year[x]=monthly$year[r]
			month[x]=monthly$month[r]
			if((r-lags[l,2])>0){
				ldata=monthly[(r-lags[l,2]):(r-lags[l,1]), ]
				lagPeff[x]=sum(ldata$peffTotal)/sum(ldata$numDays)
			} else {
				lagPeff[x]=NA
			}
		}
	}
lagDat=data.frame(year, month, lagStart, lagPeff)
for (l in unique(lagDat$lagStart)){
	newDat=subset(lagDat[,4], lagDat$lagStart==l)
	assign(paste("lag", l, sep=""), newDat)
	}
lag1=subset(lagDat, lagDat$lagStart==1)
lagCombo=data.frame(lag1, lag4, lag7, lag10, lag13, lag25)
lagPeffFinal=lagCombo[,c(1:2, 4:9)]
colnames(lagPeffFinal)[3]="lag1"
monthlyClimateData=data.frame(monthly[,c(1:3, 8:10)], lagPeffFinal[,3:8])
spring=calcClimate(c(3,4,5), 3, "spring")
summer=calcClimate(c(6,7,8), 6, "summer")
fall=calcClimate(c(9,10,11), 9, "fall")
wrt=calcClimate(c(6, 7, 8, 9), 6, "wrt")
wy=calcClimate(c(1:12), 10, "wy")
april=calcClimate(c(4), 4, "april")
august=calcClimate(c(8), 8, "august")
out=rbind(spring,summer, fall, wrt, wy, april, august)
save(out,file=paste(outputFolder, "/", monthly$site[1], "_modelData.RData", sep=""))
end = proc.time() - start
print(paste(monthly$site[1], "took", end[[3]], "seconds to run"))
}
