setwd("C:/Users/ruesca/Documents/ELOHA/daymetSummaries/swe")
library(foreign)
huc16IdFile <- "C:/Users/ruesca/Documents/ELOHA/daymetSummaries/3WI_USGS_flowStations_daymetLcc_withGridCode.dbf"   # I can give you this file if you need it

huc16Ids <- read.dbf(huc16IdFile, as.is=TRUE)[c("FINCATCH", "CATCHID", "Site")]

csvs <- list.files(pattern="swe_.*._daily")
dta <- NULL
for (csv in csvs) {
	code <- strsplit(csv, "_")[[1]][2]
	print(code)
	subDta <- read.csv(csv)
	subDta <- cbind(rep(code, dim(subDta)[1]), subDta)
	dta <- rbind(dta, subDta)
}
names(dta) <- c("code","yr","mo", "day", "time","shedAvg")
dtaMerge <- merge(dta, huc16Ids, by.x="code", by.y="FINCATCH")
dtaMerge <- dtaMerge[c("code","CATCHID","Site","yr","mo","day","time","shedAvg")]
write.csv(dtaMerge, "swe_gageStations_daily.csv", row.names=FALSE, quote=FALSE)


csvs <- list.files(pattern="swe_.*.monthly")
dta <- NULL
for (csv in csvs) {
	code <- strsplit(csv, "_")[[1]][2]
	print(code)
	subDta <- read.csv(csv)
	subDta <- cbind(rep(code, dim(subDta)[1]), subDta)
	dta <- rbind(dta, subDta)
}
names(dta) <- c("code","yr","mo","time","maxClim","sumClim", "avgClim")
dtaMerge <- merge(dta, huc16Ids, by.x="code", by.y="FINCATCH")
dtaMerge <- dtaMerge[c("code","CATCHID","Site","yr","mo","time","maxClim","sumClim", "avgClim")]
write.csv(dtaMerge, "swe_gageStations_monthly.csv", row.names=FALSE, quote=FALSE)

