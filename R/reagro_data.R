# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license GPL3


.setExtension <- function(filename, value) {
	value <- trimws(value)
	if (value != "" & substr(value, 1, 1) != ".") {
		value <- paste(".", value, sep="") 
	}
	lfn <- nchar(filename)
	fname <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break 
			}
		}
		if (extstart > 0 & (lfn[f] - extstart) < 8) {
			fname[f] <- paste(substr(filename[f], 1, extstart-1), value, sep="")
		} else { 
			fname[f] <- paste(filename[f], value, sep="")  
		}
	}
	return( unlist(fname) ) 
}   



.extension <- function(filename, value=NULL, maxchar=10) {
	if (!is.null(value)) {
		filename <- .setExtension(filename, value)
		return(filename)
	}   
	lfn <- nchar(filename)
	ext <- list()
	for (f in 1:length(filename)) {
		extstart <- -1
		for (i in lfn[f] : 2) {
			if (substr(filename[f], i, i) == ".") {
				extstart <- i
				break
			}
		}
		if (extstart > 0) {
			ext[f] <- substr(filename[f], extstart, lfn[f])
		} else { 
			ext[f] <- "" 
		}   
	}
	ext <- unlist(ext)
	ext[nchar(ext) > maxchar] <- ''
	return(ext)
}   


.get_data <- function(name, path, ext=".rds") {
	name <- .setExtension(name, ext)
	fn <- system.file(file.path(path, name), package="agrodata")
	if (!(file.exists(fn))) {
		stop(paste(name, "is not a valid data set name"))
	}
	x <- readRDS(fn)
	if (class(x) == "PackedSpatVector") {
		x <- terra::vect(x)
	} 
	x
}

.get_shp <- function(name, path) {
	name <- .setExtension(name, ".shp")
	fn <- system.file(file.path(path, name), package="agrodata")
	if (!(file.exists(fn))) {
		stop(paste(name, "is not a valid data set name"))
	}
	terra::vect(fn)
}

.get_zip <- function(datadir, url, md5, pattern, nfiles, quiet=FALSE, mode="wb",  ...) {
	dir.create(datadir, recursive=TRUE, showWarnings=FALSE)
	f <- file.path(datadir, basename(url))
	if (file.exists(f)) {
		if (tools::md5sum(f) == md5) {
			ff <- list.files(pattern, path=datadir, full=TRUE)
			if (length(ff) == nfiles) {
				return(ff)
			} else {
				unzip(f, junkpaths=TRUE, exdir=datadir)
				ff <- list.files(pattern, path=datadir, full=TRUE)
				return(ff)
			}
		}
	} 
	tmpfile <- tempfile()
	download.file(url, tmpfile, quiet=quiet, mode=mode, ...)
	if (tools::md5sum(tmpfile) == md5) {
		file.copy(tmpfile, f)
		unzip(f, junkpaths=TRUE, exdir=datadir)
		ff <- list.files(pattern, path=datadir, full=TRUE)
		return(ff)
	} else {
		stop("download failed")
	}
}



reagro_data <- function(name) {
	ff <- list.files(system.file("dat", package="agrodata"), full.names=TRUE)
	bf <- basename(ff)
	i <- match(tolower(name), .extension(tolower(bf), ""))
	if (length(i) > 0) {
		f <- ff[i]
		e <- .extension(bf[i])
		if (e == ".rds") {
			r <- readRDS(f)
			if (class(r) == "PackedSpatVector") {
				r <- vect(r)
			}
		} else {  # e == ".tif"
			r <- rast(f)
		}
	} else {
		stop("not a valid data set name")
	}
	return(r)
}	


