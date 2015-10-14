args = commandArgs(TRUE)

sStart = args[1]
sEnd = args[2]

inDir = "J:/daymet/climSummaries/all"
outDir = "J:/daymet/climSummaries/allPeffPet2"
logFile = "J:/daymet/climSummaries/naFiles"
file.create(logFile)
load("J:/daymet/upstrArea.RData")
huc16Ids = upstrArea$CATCHID[order(upstrArea$UPSTRAREA, decreasing=TRUE)]
sitelist = paste(inDir, "/", huc16Ids, "_daily.RData", sep="")
library(chron)
n <- length(sitelist)
load(sitelist[1])
dates <- allYrs[,1:3]
days <- nrow(dates)
JulianDay <- julian(dates[,2],dates[,3],1970,origin.=c(1,1,1970))
mf <- array(((0.5*(sin(julian(dates[,2],dates[,3],1970,origin.=c(3,21,1970))*2*pi/366))+0.5)*0.8+0.4)*4)
site_lat = 45

calcClim = function (allYrs, site, dates, days, JulianDay, mf, s) {
	precip <- allYrs[,4]/1000
	tmin <- allYrs[,6]/10
	tmax <- allYrs[,5]/10
	tavg <- array((tmin+tmax)/2)
	melt <- array(0,days)
	snow <- array(0,days)
	SWE <- array(0,days)
	Peff <- array(0,days)

	latRadians=(pi/180)*site_lat
	Gsc=0.0820   #Gsc solar constant = 0.0820 MJ m-2 min-1,
	dr=1+0.033*cos(2*pi/365*JulianDay)  #inverse relative distance to Sun
	delta=0.409*sin((2*pi/365*JulianDay)-1.39) #solar decimation
	ws=acos(-tan(latRadians)*tan(delta))   #sunset hour angle
	Ra=24*60/pi*Gsc*dr*(ws*sin(latRadians)*sin(delta)+cos(latRadians)*cos(delta)*sin(ws))*0.408 #(results in mm/day)
	pet=0.0023*(tavg+17.8)*sqrt(tmax-tmin)*Ra

	for (i in 2:days) {
		melt[i] <- if (tavg[i]<0) 0 
			else min(SWE[i-1],tavg[i]*mf[i])
		snow[i] <- if (tmax[i]>3 | tmin[i]>-2) 0 
			else precip[i]
		SWE[i] <- SWE[i-1]-melt[i]+snow[i]
		Peff[i] <- melt[i]+precip[i]-snow[i]
	}

	daily <- cbind(dates,Peff,tmax,tmin,pet)
	monthly <- aggregate(daily[,4:7],by=list(daily[,2], daily[,1]),FUN=mean)
	monthly <- cbind(as.integer(site),monthly)
	colnames(monthly)=c("site","month","year","pmean","tmax","tmin","pet")
	return(monthly)
}

for (s in sStart:sEnd) {
	start = proc.time()
	site = strsplit(basename(sitelist[s]),"_")[[1]][1]
	outFile = paste(outDir, "/", site, "_daily.RData", sep="")
	load(sitelist[s])
	naFlag = any(is.na(allYrs[,4:6]))
	if (naFlag) {
		write(site, file=logFile, append=T)
		next
	}
	if (file.exists(outFile)) {
		end = proc.time() - start
		print(paste(site, "took", end[[3]], "seconds to run"))
		next
	}
	monthly = calcClim(allYrs, site, dates, days, JulianDay, mf)
	save(monthly, file=outFile)
	end = proc.time() - start
	print(paste(site, "took", end[[3]], "seconds to run"))
}

# save(all,file="C:/DNR/Ecoflows/Data/flow/test_monthly_daymet.RData")
