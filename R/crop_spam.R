
spamCrops <- function() {
	x <- matrix(c("wheat", "whea", "rice", "rice", "maize", "maiz", "barley", "barl", "pearl millet", "pmil", "small millet", "smil", "sorghum", "sorg", "other cereals", "ocer", "potato", "pota", "sweet potato", "swpo", "yams", "yams", "cassava", "cass", "other roots", "orts", "bean", "bean", "chickpea", "chic", "cowpea", "cowp", "pigeonpea", "pige", "lentil", "lent", "other pulses", "opul", "soybean", "soyb", "groundnut", "grou", "coconut", "cnut", "oilpalm", "oilp", "sunflower", "sunf", "rapeseed", "rape", "sesameseed", "sesa", "other oil crops", "ooil", "sugarcane", "sugc", "sugarbeet", "sugb", "cotton", "cott", "other fibre crops", "ofib", "arabica coffee", "acof", "robusta coffee", "rcof", "cocoa", "coco", "tea", "teas", "tobacco", "toba", "banana", "bana", "plantain", "plnt", "tropical fruit", "trof", "temperate fruit", "temf", "vegetables", "vege", "rest of crops", "rest"), ncol=2, byrow=TRUE)
	colnames(x) <- c("crop", "code")
	x
}




crop_spam <- function(crop="", var="area", folder=".") {
	stopifnot(var %in% c("area", "yield"))
	stopifnot(dir.exists(folder))
	crop <- tolower(trimws(crop))
	crops <- spamCrops()
	if (!(crop %in% crops)) { stop("crop not know to SPAM; see spamCrops()") }
	i <- which(crop == crops)[1]
	crop <- toupper(crops[i,2])
	urlbase <- "https://s3.amazonaws.com/mapspam/2010/v1.1/geotiff/"
	if (var == "area") {
		url <- paste0(urlbase, "spam2010v1r1_global_harv_area.geotiff.zip")
	} else {
		url <- paste0(urlbase, "spam2010v1r1_global_yield.geotiff.zip")
	}
	zipf <- file.path(folder, basename(url))
	if (!file.exists(zipf)) {
		download.file(url, zipf, mode="wb")
	}
	ff <- unzip(zipf, list=TRUE)
	fs <- grep(crop, ff$Name, value=TRUE)
	unzip(zipf, files=fs, junkpaths=TRUE, exdir=folder)
	ffs <- file.path(folder, fs)
	x <- terra::rast(ffs)

	nicenms <- c("A", "all", "I", "irrigated", "H", "rainfed-highinput", "L", "rainfed-lowinput", "S", "rainfed-subsistence", "R", "rainfed")
	nicenms <- matrix(nicenms, ncol=2, byrow=TRUE)
	nicenms <- nicenms[order(nicenms[,1]), ]

	n <- sort(names(x))
	n <- substr(n, nchar(n[1]), nchar(n[1]))
	i <- match(n, nicenms[,1])
	names(x) <- nicenms[,2]
	
	terra::ext(x) <- c(-180, 180, -90, 90)
	x
}


