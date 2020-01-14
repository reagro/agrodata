

.data_agrins <- function(name) {
	name <- tolower(name[1])
	n <- nchar(name)
	x <- .get_data(name) 
	if (class(x) == "data.frame") return(x)
	if (class(x) == "SpatVector") return(terra::vect(x))
	x
}


data_ibli <- function(name, path) {
	name <- tolower(name)

	if (name == "marsabit_modis_ndvi") {
		if (missing(path)) {
			stop("provide a path (where the data should be stored)")
		}
		ff <- .get_zip(path, "https://gfc.ucdavis.edu/events/agrin/data/marsabit_modis_ndvi.zip", "16fe5aa1e4080725d0b6cce123f25bf4", "^MOD09A1.*ndvi\\.tif$", 360)
		return(ff)
	} else {
		.get_data(name, "ibli", ext="")
	}
}


data_rice <- function(name) {
	.get_data(name, "rice", ext="")
}


data_crop <- function(name, path) {
	name <- tolower(name)
	if (name %in% c("xxxcrop_ref")) {
		.get_shp(name, "crop")	
	} else if (name == "sentinel") {
		if (missing(path)) {
			stop("provide a path (where the data should be stored)")
		}
		ff <- .get_zip(path, "https://gfc.ucdavis.edu/events/agrin/data/sentinel.zip", "5ded28dab9d73bccd6ea83379f730e66", "^S1A.*_Clip.tif$", 29)
		return(ff)
		
	} else {
		.get_data(name, "crop")
	}
}


