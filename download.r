tiles <- c(11927, 11926, 11925, 12107, 12106, 12105, 12104, 12286, 12285, 12284)

# years = 1980:2011
years = 2012:2014

# str0 <- "http://daymet.ornl.gov/thredds/fileServer/allcf/"
str0 = "http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/tiles/"
str2 <- "/prcp.nc"

for (tile in tiles) {
	for (year in years) {
		str1 <- paste(year, "/", tile, "_", year, sep="")
		URL <- paste(str0, str1, str2, sep="")
		destFile = paste("K:/daymet/data/prcp_", tile, "_", year, ".nc", sep="")
		print(URL)
		print(destFile)
		download.file(URL, destFile, "auto", mode="wb", cacheOK="FALSE")
	}
}

