hsweDataFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/climSummaries/appended/swe_gageStations_daily.csv"
prcpDataFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/climSummaries/appended/prcp_gageStations_daily.csv"

sweDta <- read.csv(sweDataFile, stringsAsFactors=FALSE)
prcpDta <- read.csv(prcpDataFile, stringsAsFactors=FALSE)

dta <- merge(sweDta, prcpDta, by=c("code", "CATCHID", "Site", "yr", "mo", "day", "time"))
names(dta)[8:9] <- c("swe", "prcp")
dta$sweDepth <- dta$swe * (1/998.3) * 1000
dta <- dta[order(dta$code, dta$yr, dta$mo, dta$day),]
dta$offsetSwe[2:dim(dta)[1]] <- dta$sweDepth[1:(dim(dta)[1] - 1)]
dta$offsetSwe[1] <- NA
dta$diffSwe <- dta$sweDepth - dta$offsetSwe
# + diffSwe == increase in snow depth, subtract from precip
# - diffSwe == decrease in snow depth, add to snow depth
dta$adjPrcp <- dta$prcp - dta$diffSwe

outDailyFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/climSummaries/appended/adjPrcp_gageStations_daily.csv"
write.csv(dta, outDailyFile, row.names=FALSE, quote=FALSE)

avgMoPrcp <- aggregate(dta$adjPrcp, by=list(dta$code, dta$yr, dta$mo), mean, na.rm=TRUE)
maxMoPrcp <- aggregate(dta$adjPrcp, by=list(dta$code, dta$yr, dta$mo), max, na.rm=TRUE)
sumMoPrcp <- aggregate(dta$adjPrcp, by=list(dta$code, dta$yr, dta$mo), sum, na.rm=TRUE)

outDta <- data.frame(code=avgMoPrcp[,1]
	, yr=avgMoPrcp[,2]
	, mo=avgMoPrcp[,3]
	, maxClim=maxMoPrcp[,4]
	, sumClim=sumMoPrcp[,4]
	, avgClim=avgMoPrcp[,4]
)

outDta <- outDta[order(outDta$code, outDta$yr, outDta$mo),]
outFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/climSummaries/appended/adjPrcp_gageStations_monthly.csv"
write.csv(outDta, outFile, row.names=FALSE, quote=FALSE)

adjPrcp <- outDta$sumClim
prcp <- aggregate(dta$prcp, by=list(dta$code, dta$yr, dta$mo), sum, na.rm=TRUE)
prcp <- prcp[order(prcp[,1], prcp[,2], prcp[,3]),]
times <- as.POSIXct(paste(prcp[,2], "-", prcp[,3], "-01", sep=""), tz="UTC")

# comparePrcp <- data.frame(code=rep(prcp[,1],2)
	# , times=rep(times,2)
	# , prcp=c(prcp[,4], adjPrcp)
	# , type=c(rep("prcp", length(times))
		# , rep("adjPrcp", length(times)))
# )

comparePrcp <- data.frame(code=prcp[,1]
	, times=times
	, prcp=prcp[,4]
	, adjPrcp=adjPrcp
)

# example <- comparePrcp[comparePrcp$code == 1,]
# qplot(times, prcp, data=comparePrcp, col=type, geom="line")

# par(mfrow=c(3,1))
huc16IdFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/inputData/3WI_USGS_flowStations_daymetLcc_withGridCode.dbf"
huc16Ids <- read.dbf(huc16IdFile)
pdfFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/adjustedPrecipFigures_monthly.pdf"
pdf(file = pdfFile, width=7, height=9.5, onefile=TRUE, paper="letter")
codes <- unique(comparePrcp$code)
codeTable <- merge(data.frame(code=codes), huc16Ids, by.x="code", by.y="FINCATCH") 
codeTable <- codeTable[c("code", "Name")]
par(mfrow=c(4,1))
for (code in codes) {
	desc <- as.character(codeTable$Name[codeTable$code == code])
	subDta <- comparePrcp[comparePrcp$code == code,]
	yrBlock1 <- subDta[subDta$times < as.POSIXct("1988-01-01"),]
	yrBlock2 <- subDta[(subDta$times >= as.POSIXct("1988-01-01")) & (subDta$times < as.POSIXct("1996-01-01")),]
	yrBlock3 <- subDta[(subDta$times >= as.POSIXct("1996-01-01")) & (subDta$times < as.POSIXct("2004-01-01")),]
	yrBlock4 <- subDta[subDta$times >= as.POSIXct("2004-01-01"),]
	plot(prcp ~ times
		, data=yrBlock1
		, ylim=range(c(yrBlock1$prcp, yrBlock1$adjPrcp))
		, type="l"
		, main=desc
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ times
		, data=yrBlock1
		, type="l"
		, col="red"
	)
	plot(prcp ~ times
		, data=yrBlock2
		, ylim=range(c(yrBlock2$prcp, yrBlock2$adjPrcp))
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ times
		, data=yrBlock2
		, type="l"
		, col="red"
	)
	plot(prcp ~ times
		, data=yrBlock3
		, ylim=range(c(yrBlock3$prcp, yrBlock3$adjPrcp))
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ times
		, data=yrBlock3
		, type="l"
		, col="red"
	)
	plot(prcp ~ times
		, data=yrBlock4
		, ylim=range(c(yrBlock4$prcp, yrBlock4$adjPrcp))
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ times
		, data=yrBlock4
		, type="l"
		, col="red"
	)
}
dev.off()


# par(mfrow=c(3,1))
huc16IdFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/inputData/3WI_USGS_flowStations_daymetLcc_withGridCode.dbf"
huc16Ids <- read.dbf(huc16IdFile)
pdfFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/adjustedPrecipFigures_daily.pdf"
pdf(file = pdfFile, width=7, height=9.5, onefile=TRUE, paper="letter")
codes <- unique(comparePrcp$code)
codeTable <- merge(data.frame(code=codes), huc16Ids, by.x="code", by.y="FINCATCH") 
codeTable <- codeTable[c("code", "Name")]
par(mfrow=c(4,1))
dta$time <- as.POSIXct(dta$time, tz="UTC")
for (code in codes) {
	desc <- as.character(codeTable$Name[codeTable$code == code])
	subDta <- dta[dta$code == code,]
	yrBlock1 <- subDta[subDta$time < as.POSIXct("1988-01-01"),]
	yrBlock2 <- subDta[(subDta$time >= as.POSIXct("1988-01-01")) & (subDta$time < as.POSIXct("1996-01-01")),]
	yrBlock3 <- subDta[(subDta$time >= as.POSIXct("1996-01-01")) & (subDta$time < as.POSIXct("2004-01-01")),]
	yrBlock4 <- subDta[subDta$time >= as.POSIXct("2004-01-01"),]
	plot(prcp ~ time
		, data=yrBlock1
		, ylim=range(c(yrBlock1$prcp, yrBlock1$adjPrcp), na.rm=TRUE)
		, type="l"
		, main=desc
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ time
		, data=yrBlock1
		, type="l"
		, col="red"
	)
	plot(prcp ~ time
		, data=yrBlock2
		, ylim=range(c(yrBlock2$prcp, yrBlock2$adjPrcp), na.rm=TRUE)
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ time
		, data=yrBlock2
		, type="l"
		, col="red"
	)
	plot(prcp ~ time
		, data=yrBlock3
		, ylim=range(c(yrBlock3$prcp, yrBlock3$adjPrcp), na.rm=TRUE)
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ time
		, data=yrBlock3
		, type="l"
		, col="red"
	)
	plot(prcp ~ time
		, data=yrBlock4
		, ylim=range(c(yrBlock4$prcp, yrBlock4$adjPrcp), na.rm=TRUE)
		, type="l"
		, main=""
		, ylab="Precipiation (mm)"
		, xlab=""
	)
	lines(adjPrcp ~ time
		, data=yrBlock4
		, type="l"
		, col="red"
	)
}
dev.off()

