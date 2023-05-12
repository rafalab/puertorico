#' Municipio names, abbreviation and other useful objects
#'
#' \code{puertorico} currently contains the following "municipio" data sets. All data 
#' are arranged according to the alphabetical order of the municipality names. 
#'
#'@format An rda file with several objects
#' \describe{
#'   \item{municipios.names}{character vector of full municipality names}
#'   \item{municipios.abr}{character vector of municipality abbreviations as set by the U.S. Census Bureau.}
#'   \item{municipios.health.regions}{factor municipality divisions (Ponce, Mayagüez/Aguadilla, Caguas, Arecibo, Bayamón, Metro, Fajardo).}
#'   \item{x}{Components \code{x} for the center of each municipality.}
#'   \item{y}{Components \code{y} for the center of each municipality.} 
#'}
#'@usage municipios
#'
#'@examples
#' head(municipios)
#' municipio.names
#' municipio.abr
#'
"municipios"