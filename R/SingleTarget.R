# Functions to write the SingleTarget group of the SONAR-netCDF4 convention:


##################################################
#' Write the SingleTarget group of the SONAR-netCDF4 convention
#' 
#' @param x A table of SingleTarget data.
#' @param filePath The path to the SONAR-netCDF4 file to write to.
#' 
#' @examples
#' #### Small example: ####
#' # Define dimension variables and variables:
#' single_target_dimVars <- list(
#' 	ping_time = seq(as.POSIXct("1970-02-22 00:00:00", tz = "UTC"), by = 2, length.out = 2), 
#' 	beam = c("Beam1", "Beam2")
#' )
#' single_target_vars <- list(
#' 	single_target_identifier = structure(
#' 		list( # ping_time
#' 			structure(list(1:3, 1), names = single_target_dimVars$beam), # beam
#' 			structure(list(2:8, 1:2), names = single_target_dimVars$beam) # beam
#' 		), 
#' 		names = as.character(single_target_dimVars$ping_time)
#' 	), 
#' 	single_target_detection_algorithm = 
#' 		rep(list(
#' 		rep(
#' 			"testData", 
#' 			length(single_target_dimVars$beam))), 
#' 			length(single_target_dimVars$ping_time)
#' 		), 
#' 	single_target_range = structure(
#' 		list( # ping_time
#' 			structure(
#' 				list(
#' 					c(2, 4, 7), 
#' 					3
#' 				), 
#' 				names = single_target_dimVars$beam
#' 			), # beam
#' 			structure(
#' 				list(
#' 					c(1, 8, 29, 55, 60, 60, 79), 
#' 					c(5, 35)
#' 					), 
#' 				names = single_target_dimVars$beam
#' 			) # beam
#' 		), 
#' 		names = as.character(single_target_dimVars$ping_time)
#' 	)
#' )
#' 
#' # Write the data to SONAR-neCDF4:
#' filePath <- tempfile(fileext = "nc")
#' writeSTD_vars_dimVars(
#' 	vars = single_target_vars, 
#' 	dimVars = single_target_dimVars, 
#' 	filePath = filePath
#' )
#' 
#' # Read back in:
#' dflat <- readSTD(filePath)
#' 
#' # Then try to write again from the flat table (long format):
#' filePath2 <- tempfile(fileext = "nc")
#' writeSTD(
#' 	dflat, 
#' 	filePath = filePath2
#' )
#' 
#' # The original and written data are identical:
#' dflat2 <- readSTD(filePath2)
#' identical(dflat, dflat2)
#' 	
#' #### A bigger example with TS(f): ####
#' # Define the dimension variables with frequency as the third dimension:
#' single_target_dimVars_TSf <- c(
#' 	single_target_dimVars, 
#' 	list(frequency = as.double(seq(36, 40) * 1e3))
#' )
#' nFreq <- length(single_target_dimVars_TSf$frequency)
#' 
#' # Generate sorted radom expoential values as TS(f):
#' single_target_vars_TSf <- c(
#' 	single_target_vars, 
#' 	list(
#' 		uncompensated_TS  = structure(
#' 			list( # ping_time
#' 				structure(
#' 					list(
#' 						lapply(rep(3, nFreq),  stats::rexp), 
#' 						lapply(rep(1, nFreq),  stats::rexp)
#' 					), 
#' 					names = single_target_dimVars$beam
#' 				), # beam
#' 				structure(
#' 					list(
#' 						lapply(rep(7, nFreq),  stats::rexp), 
#' 						lapply(rep(2, nFreq),  stats::rexp)
#' 						), 
#' 					names = single_target_dimVars$beam
#' 				) # beam
#' 			), 
#' 			names = as.character(single_target_dimVars$ping_time)
#' 		)
#' 	)
#' )
#' 
#' 
#' # Write the data:
#' filePath3 <- tempfile(fileext = "nc")
#' writeSTD_vars_dimVars(
#' 	vars = single_target_vars, 
#' 	dimVars = single_target_dimVars, 
#' 	filePath = filePath3
#' )
#' 
#' # Read back in to a flat table:
#' dflat3 <- readSTD(filePath3)
#' 	
#' # Then try to write again:
#' filePath4 <- tempfile(fileext = "nc")
#' writeSTD(
#' 	dflat3, 
#' 	filePath = filePath4
#' )
#' 
#' # The original and written data are identical:
#' dflat4 <- readSTD(filePath4)
#' identical(dflat3, dflat4)
#' 
#' @export
#' 
writeSTD <- function(x, filePath) {
	
	singleTargetSchema <- getSingleTargetSchema()
	validVar <- c(singleTargetSchema$dimension_variables, singleTargetSchema$variables)
	invalidVar <- setdiff(names(x), validVar)
	if(length(invalidVar)) {
		warning("There are variable not defined by the SONAR-netCDF4 convention. Please remove these from the data (", paste(invalidVar, collapse = ", "), ").")
	}
	
	x <- flatTable2nestedListPerVariable(x)
	writeSTD_vars_dimVars(vars = x$vars, dimVars = x$dimVars, filePath = filePath)

}

##################################################
#' Read the SingleTarget group of the SONAR-netCDF4 convention from dimemsions and variables (nested list)
#' 
#' @param vars A nested list of SingleTarget variables following the SingleTarget group of the SONAR-netCDF4 convention.
#' @param dimVars A list of dimension variables.
#' @param filePath The path to the SONAR-netCDF4 file to read.
#' 
#' @export
#' @rdname writeSTD
#' 	
writeSTD_vars_dimVars <- function(vars, dimVars, filePath) {
	
	# Get the dimensions if not given:
	dimensions <- getSingleTargetSchema(vars)$dimensions
	vlen_variables <- getSingleTargetSchema(vars)$vlen_variables
	
	pingInd <- seq_along(unique(dimVars$ping_time))
	lapply(
		pingInd, 
		writeSTD_OnePing, 
		vars = vars, 
		dimVars = dimVars, 
		dimensions = dimensions, 
		vlen_variables = vlen_variables, 
		filePath = filePath, 
		append = TRUE
	)
	
	invisible(filePath)
}

writeSTD_OnePing <- function(pingInd, vars, dimVars, dimensions, vlen_variables, filePath, append = FALSE) {
	
	if(file.exists(filePath)) {
		if(append) {
			nc <- RNetCDF::open.nc(filePath, write = TRUE)
			if(RNetCDF::file.inq.nc (nc)$format != "netcdf4") {
				stop("The file ", filePath, " must be a netcdf4 file.")
			}
			
			# Get the SingleTarget group:
			grp <- RNetCDF::grp.inq.nc(nc, getSingleTargetSchema()$groupName)$self
		}
		else {
			stop("The file ", filePath, " already exists.")
		}
	}
	else {
		# Create a new NetCDF dataset
		nc <- RNetCDF::create.nc(filePath, format = "netcdf4")
		# Create the Sonar/Beam_group/SingleTarget group:
		grp <- RNetCDF::grp.def.nc(nc, "Sonar")
		grp <- RNetCDF::grp.def.nc(grp, "Beam_group1")
		grp <- RNetCDF::grp.def.nc(grp, "SingleTarget")
		
		
		# Create the time variable:
		timeDimVar <- getTimeDimVar(dimVars)
		defineTime(grp, timeLabel = timeDimVar, type = "NC_DOUBLE", unlim = TRUE, time_unit = "nanoseconds since 1970-01-01 00:00:00 +00:00")
		
		# Create the other dimensions:
		nonTimeDimVar <- setdiff(names(dimVars), timeDimVar)
		nonTimeDimVarNCType <- getNCType(dimVars[nonTimeDimVar])
		mapply(RNetCDF::dim.def.nc, nonTimeDimVar, dimlength = lengths(dimVars[nonTimeDimVar]), MoreArgs = list(ncfile = grp))
		mapply(RNetCDF::var.def.nc, nonTimeDimVar, vartype = nonTimeDimVarNCType, dimensions = nonTimeDimVar, MoreArgs = list(ncfile = grp))
		mapply(RNetCDF::att.put.nc, nonTimeDimVar, name = "variableType", type = "NC_CHAR", value = "dimension", MoreArgs = list(ncfile = grp))
		mapply(RNetCDF::var.put.nc, nonTimeDimVar, data = dimVars[nonTimeDimVar], MoreArgs = list(ncfile = grp))
		
		
		# Define variables:
		varNCType <- getNCType(vars, vlen_variables = vlen_variables, nc = grp)
		mapply(RNetCDF::var.def.nc, names(vars), vartype = varNCType, dimensions = dimensions[names(vars)], MoreArgs = list(ncfile = grp))
		
		# Add the variableType:
		variableType <- ifelse(names(vars) %in% vlen_variables, "vlen_variable", "variable")
		mapply(RNetCDF::att.put.nc, names(vars), name = "variableType", type = "NC_CHAR", value = variableType, MoreArgs = list(ncfile = grp))	
	}
	
	
	# Subset dimensions to those present in the data:
	dimensions <- subset(dimensions, names(dimensions) %in% names(vars))
	dimensions <- subset(dimensions, names(dimensions) %in% names(vars))
	
	# Put the time:
	timeDimVar <- getTimeDimVar(dimVars)
	putOneTime(grp, time = dimVars[[timeDimVar]], timeInd = pingInd, timeLabel = timeDimVar)
	
	# Put the data:
	putOnePing(grp, pingInd, vars, dimensions)
	
	#RNetCDF::sync.nc(nc)
	
	
	RNetCDF::close.nc(nc)
}


putOnePing <- function(nc, pingInd, vars, dimensions) {
	
	mapply(putOneVarOnePing, varName = names(vars), MoreArgs = list(nc = nc, pingInd = pingInd, vars = vars, dimensions = dimensions))
	
}
		
putOneVarOnePing <- function(nc, pingInd, varName, vars, dimensions) {
	
	if(length(dimensions[[varName]]) == 2) {
		RNetCDF::var.put.nc(
			nc, 
			varName, 
			vars[[varName]][[pingInd]], 
			start = c(pingInd, NA), 
			count = c(1, length(vars[[varName]][[pingInd]]))
		)
	}
	else if(length(dimensions[[varName]]) == 3) {
		
		beamInd <- seq_along(vars[[varName]][[pingInd]])
		
		mapply(putOneVarOnePingOneBeam, beamInd = beamInd, MoreArgs = list(nc = nc, pingInd = pingInd, varName = varName, vars = vars, dimensions = dimensions))

	}
	else {
		stop("Only variables of one or two dimensions in addition to the ping_time are allowed.")
	}
	
}

putOneVarOnePingOneBeam <- function(nc, pingInd, beamInd, varName, vars, dimensions) {
	
	RNetCDF::var.put.nc(
		nc, 
		varName, 
		vars[[varName]][[pingInd]][[beamInd]], 
		start = c(pingInd, beamInd, NA), 
		count = c(1, 1, length(vars[[varName]][[pingInd]][[beamInd]]))
	)
}


defineTime <- function(nc, timeLabel = "ping_time", type = "NC_DOUBLE", unlim = FALSE, time_unit = "nanoseconds since 1970-01-01 00:00:00 +00:00", dimlength = 1) {
	RNetCDF::dim.def.nc(nc, timeLabel, dimlength = dimlength, unlim = unlim)
	RNetCDF::var.def.nc(nc, timeLabel, vartype = type, dimensions = timeLabel)
	RNetCDF::att.put.nc(nc, timeLabel, name = "long_name", type = "NC_CHAR", value = timeLabel)
	RNetCDF::att.put.nc(nc, timeLabel, name = "units", type = "NC_CHAR", value = time_unit)
	RNetCDF::att.put.nc(nc, timeLabel, name = "variableType", type = "NC_CHAR", value = "timeDimension")
}
putOneTime <- function(nc, time, timeInd, timeLabel = "ping_time", time_unit = "nanoseconds since 1970-01-01 00:00:00 +00:00", dimlength = 1) {
	RNetCDF::var.put.nc(nc, timeLabel, data = RNetCDF::utinvcal.nc(time_unit, time[timeInd]), start = timeInd, count = 1)
}

getNCType <- function(x, vlen_variables, nc) {
	
	# This is ulgy as hell, but works to the the first element of a nested list (with support for 5 levels):
	RType <- sapply(x, function(y) class(y[[1]][[1]][[1]][[1]][[1]])[1])
	# We ignore int64 and uint64:
	NCTypes <- list(
		integer = "NC_INT", 
		double = "NC_DOUBLE", 
		numeric = "NC_DOUBLE", 
		character = "NC_STRING"
	)
	
	NCType <- unlist(NCTypes[match(RType, names(NCTypes))])
	
	if(!missing(vlen_variables)) {
		
		hasVlen <- names(x) %in% vlen_variables
		
		uNCTypes <- unique(NCType[hasVlen])
		mapply(RNetCDF::type.def.nc, paste0("vlen_", uNCTypes), "vlen", basetype = uNCTypes, MoreArgs = list(ncfile = nc))
		
		NCType[hasVlen] <- paste0("vlen_", NCType[hasVlen])
	}
	
	return(NCType)
}

getTimeDimVar <- function(dimVars) {
	# Put the time:
	isTimeDimVar <- sapply(dimVars, function(x) "POSIXct" %in% class(x))
	if(sum(isTimeDimVar) == 1) {
		timeDimVar <- names(dimVars)[isTimeDimVar]
	}
	else if(any(isTimeDimVar)) {
		stop("Exactly 1 (or 0) time variable can be written")
	}
	return(timeDimVar)
}


flatTable2nestedListPerVariable <- function(x) {
	
	# Get the details of the SingleTarget group:
	singleTargetSchema <- getSingleTargetSchema(x)
	variables <- singleTargetSchema$variables
	dimensions <- singleTargetSchema$dimensions
	dimension_variables <- singleTargetSchema$dimension_variables
	vlen_variables <- singleTargetSchema$vlen_variables
	
	#### Get first the dimension variables:
	dimVars <- lapply(x[, ..dimension_variables], unique)
	
	
	
	#### Then get the variables:
	vars  <- splitVariables(x, variables, dimensions, dimension_variables, vlen_variables)
	
	out <- list(
		dimVars = dimVars, 
		vars = vars
	)
	
	return(out)
}

splitVariables <- function(x, variables, dimensions, dimension_variables, vlen_variables) {
	
	presentDimensions <- intersect(names(x), dimension_variables)
	
	keys <- list()
	keys[[1]] <- x[[dimension_variables[1]]]
	keys[[2]] <- split(x[[dimension_variables[2]]], keys[[1]])
	if(length(presentDimensions) == 3) {
		keys[[3]] <- split(x[[dimension_variables[3]]], keys[[1]])
		keys[[3]] <- lapply(seq_along(keys[[3]]), function(ind) split(keys[[3]][[ind]], keys[[2]][[ind]]))
	}
	
	
	
	vars <- lapply(variables, simplifyVariable, x, dimensions, vlen_variables, keys)
	names(vars) <- variables
	
	return(vars)
	
}

simplifyVariable <- function(varName, x, dimensions, vlen_variables, keys) {
	
	#if(varName == "single_target_identifier") browser()

	# Split first and second level:
	temp <- x[[varName]]
	temp <- split(temp, keys[[1]])
	temp <- lapply(
		seq_along(temp), 
		function(ind) 
			split(temp[[ind]], keys[[2]][[ind]]) 
	)
	
	# Move into the third level if required:
	if(any(lengths(dimensions) == 3)) {
		temp <- lapply(
			seq_along(temp), 
			function(ind1) 
				lapply(
					seq_along(temp[[ind1]]), 
					function(ind2) 
						split(temp[[ind1]][[ind2]], keys[[3]][[ind1]][[ind2]])
				)
		)
	}
	
	
	
	
	
	if(! varName %in% vlen_variables) {
		
		temp <- lapply(
			seq_along(temp), 
			function(ind1) 
				lapply(
					seq_along(temp[[ind1]]), 
					function(ind2) 
						unlist( lapply(temp[[ind1]][[ind2]], utils::head, 1), use.names = FALSE)
				)
		)
		
		
		if(length(dimensions[[varName]]) == 2) {
			temp <- lapply(
				seq_along(temp), 
				function(ind1) 
					unlist( lapply(temp[[ind1]], utils::head, 1), use.names = FALSE)
			)
		}
		
	}
	else {
		if(any(lengths(dimensions) == 3)  &&  length(dimensions[[varName]]) == 2) {
			temp <- lapply(
				seq_along(temp), 
				function(ind1) 
					lapply(temp[[ind1]], function(y) y[[1]] )
			)
		}
	}

	return(temp)
}


##################################################
#' Read the SingleTarget group of the SONAR-netCDF4 convention
#' 
#' @param filePath The path to the SONAR-netCDF4 file to read.
#' 
#' @export
#' @rdname writeSTD
#' 	
readSTD <- function(filePath) {
	
	# Read  back in:
	ncIn <- RNetCDF::open.nc(filePath)
	SingleTargetGroup <- RNetCDF::grp.inq.nc(ncIn, getSingleTargetSchema()$groupName)$self
	dat <- RNetCDF::read.nc(SingleTargetGroup)

	# Get the variable types:
	variableType <- mapply(RNetCDF::att.get.nc, seq_along(dat) - 1, MoreArgs = list(ncfile = SingleTargetGroup, attribute = "variableType"))
	RNetCDF::close.nc(ncIn)

	# Identify variables:
	isVariable <- endsWith(variableType, "variable")
	
	# Expand the dimension of the variables to the largest dimension:
	dims <- lapply(dat[isVariable], dim)
	maxDim <- dims[[which.max(lengths(dims))]]
	dat[isVariable] <- lapply(dat[isVariable], function(x) if(length(dim(x)) < length(maxDim)) array(x, dim = maxDim) else x)
	
	
	## R arranges the fastest changing dimension first, contrary to netCDF4, so we transpose the tables:
	dat[isVariable] <- lapply(dat[isVariable], function(x) if(length(dim(x)) >= 2) aperm(x))

	# Collapse the arrays:
	d <- data.table::as.data.table(
		c(
			do.call(data.table::CJ, dat[!isVariable]), 
			lapply(dat[isVariable], c)
		)
	)
	
	
	# Expand to a flat table by repeatig the non-vlen variables and unlist the vlen-variables::
	vlenVariable <- subset(names(d), startsWith(variableType, "vlen"))
	vlens <- lapply(d[, ..vlenVariable], lengths)
	dflat <- data.table::as.data.table(
		lapply(names(d), function(name) if(name %in% vlenVariable) unlist(d[[name]]) else rep(d[[name]], vlens[[1]]))
	)
	names(dflat) <- names(d)

	# Convert time:
	time_unit <- "nanoseconds since 1970-01-01 00:00:00 +00:00"
	dflat[, ping_time := RNetCDF::utcal.nc(time_unit, ping_time, "c")]
	
	return(dflat[])
}
