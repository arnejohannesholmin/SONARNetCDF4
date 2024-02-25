#' Tools to Read and Write netCDF4 Files in the SONAR-netCDF4 Convension
#'
#' Functions to read and write netCDF4 files in the SONAR-netCDF4 convension, which is a standard for SONAR and echosounder data and derived data defined by the International Council for the Exploration of the Sea (ICES).
#'
#' 
#' @docType package
#' @name sonaRnetcdf4
#'
"_PACKAGE"

# Global variables
utils::globalVariables(c(
	 "..dimension_variables", "..vlenVariable", ":=", "ping_time"))

# Packages to import to NAMESPACE (typically packages which are used extensively or packcages with special syntax that requires import, e.g, data.table)
#' @import data.table
NULL

