args <- commandArgs(TRUE)
# args <- c("J:/daymet", "prcp", 1986, 1987)

library(foreign, verbose=FALSE)
library(raster, verbose=FALSE)

dir <- args[1]
var <- args[2]
yrs <- args[3]:args[4]

source(paste(dir, "/code/climSummaryFunctions_byYear.r", sep=""))
ncDir <- paste(dir, "/data", sep="")
outDir <- paste(dir, "/climSummaries/", var, sep="")
if (!file.exists(outDir)) { file.create(outDir) }
topologyTableFile <- paste(dir, "/shedTopology11052012_daymetLcc.dbf", sep="")
topologyTable <- read.dbf(topologyTableFile)[c("CATCHID", "TOCATCHID")]
watersheds <- raster(paste(dir, "/shedTopology11052012_daymetLcc.tif", sep=""))
ids <- raster(paste(dir, "/id.tif", sep=""))
load(paste(dir, "/lookupTable.RData", sep=""))
load(paste(dir, "/upstrArea.RData", sep=""))
huc16Ids_all <- upstrArea$CATCHID[order(upstrArea$UPSTRAREA)]

tileCodes <- c(11925, 11926, 11927, 12104, 12105, 12106, 12107, 12284, 12285, 12286) 
tileExtents <- getTileExtents(tileCodes, ncDir)
shedPx <- getValues(watersheds)
idPx <- getValues(ids)
gridCoords <- coordinates(watersheds)


for (yr in yrs) {
	print(yr)
	yrDir <- paste(outDir, "/", yr, sep="")
	if (!file.exists(yrDir)) { 
		dir.create(yrDir)
		huc16Ids = huc16Ids_all
	} else {
		print("populating file names...")
		outFiles <- paste(yrDir, "/", var, "_", huc16Ids_all, "_", yr, "_daily.RData", sep="")
		print("checking if files have already been created...")
		exists = file.exists(outFiles)
		print("looking for small file sizes that need to be re-run...")
		if (var == "prcp") {
			big = (file.info(outFiles)$size) > 1600 # needs to be changed
		} else {
			big = (file.info(outFiles)$size) > 2000
		}
		print("populating list of file names that need to be run...")
		complete = big & exists
		huc16Ids <- huc16Ids_all[!complete]
	}
	if (length(huc16Ids) > 0) {
		print("loading netCDF data into RAM (keep an eye on RAM use in system monitor)...")
		d <- getNetcdfData(tileCodes, ncDir, yr, var)
		print("running watershed summaries")
		for (huc16Id in huc16Ids) {
			ltm <- proc.time()
			print(paste("HUC:", huc16Id))
			ttm <- proc.time()
			trimmed <- findUpstream(huc16Id, watersheds, shedPx, ids, idPx, lookupTable, gridCoords, topologyTable)
			trimTime <- proc.time() - ttm
			intersections <- getOverlayingTiles(trimmed, tileExtents)
			all <- traceMaxClim(d, trimmed, intersections, tileExtents, var, yr)
			save(all, file = paste(yrDir, "/", var, "_", huc16Id, "_", yr, "_daily.RData", sep=""))
			print(proc.time() - ltm)
		}
	}
}
