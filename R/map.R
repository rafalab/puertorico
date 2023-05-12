#' Puerto Rico Map 
#'
#' Data set containing data necessary to build a map for Puerto Rico, including 
#' its municipalities. It also includes `map_centers` which has municipalities 
#' and the approximate coordinate of the center. 
#' 
#' map data frame contains 28,332 rows and 4 columns
#' \describe{
#'   \item{municipio}{Municipality name}
#'   \item{part}{...}
#'   \item{X}{\code{x} coordinate for polygon.}
#'   \item{Y}{\code{y} coordinate for polygon.}
#'}
#'
#'@usage map
#'
#'@format An object of class data.frame.
#'
#'@aliases map_centers
#'
#'@examples
#'
#'head(map)
#'head(map_centers)
"map"

