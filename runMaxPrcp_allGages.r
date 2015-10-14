library(foreign)
library(raster)

source("D:/aaronRuesch/daymet/code/maxPrcpFunctions.r")
ncDir <- "D:/aaronRuesch/daymet/prcpData"
outDir <- "D:/aaronRuesch/daymet/maxPrcpSummaries"
topologyFile <- "D:/aaronRuesch/daymet/FlowSheds11202012.dbf"
topologyTable <- read.dbf(topologyFile, as.is=TRUE)[c("CATCHID", "FLWTOCATCH")]
watersheds <- raster("D:/aaronRuesch/daymet/catchid.tif")
huc16IdFile <- "D:/aaronRuesch/daymet/3WI_USGS_flowStations_snapped_reproject.dbf"
huc16Ids <- read.dbf(huc16IdFile, as.is=TRUE)["FINCATCH"]
yrs <- 1980:2011
tileCodes <- c(11925, 11926, 11927, 12104, 12105, 12106, 12107, 12284, 12285, 12286) 


huc16block <- as.integer(huc16Ids[1:30, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}

huc16block <- as.integer(huc16Ids[31:61, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[62:92, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[93:123, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[124:154, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[155:185, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[186:216, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[217:247, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[248:278, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}


huc16block <- as.integer(huc16Ids[279:299, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}
 
huc16block <- as.integer(huc16Ids[300:330, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}

huc16block <- as.integer(huc16Ids[331:362, "FINCATCH"])
for (huc16 in huc16block) {
  print(paste("HUC16:", huc16))
  tileExtents <- getTileExtents(tileCodes, ncDir)
  trimmed <- findUpstream(huc16, watersheds, topologyTable)
  intersections <- getOverlayingTiles(trimmed, tileExtents)
  all <- traceMaxPrecip(trimmed, intersections, yrs, ncDir)
  write.csv(all, paste(outDir, "/", huc16, ".csv", sep=""), row.names=FALSE, quote=FALSE)
}