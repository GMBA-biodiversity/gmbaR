#' Get mountain range IDs from elevation range
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within a certain mountain elevation range
#'
#' @param elevation_low Numeric, lower limit of an elevation range
#' @param elevation_high Numeric, higher limit of an elevation range
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs associated
#' covering the input elevation range
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' rangeids <- gmba_ids_from_elevation(4000, 5000)
#' }

gmba_ids_from_elevation <- function(elevation_low, elevation_high){
  if(!(elevation_low <= elevation_high)){
    stop("The 'elevation_high' must be higher than the 'elevation_low'.")}
  inv_elevation_low <- gmba_inv()$Elev_Low
  inv_elevation_high <- gmba_inv()$Elev_High
  if(!between(elevation_low, min(inv_elevation_low), max(inv_elevation_low))){
    stop("The lower elevation limit must be within -606 and 4926.")}
  if(!between(elevation_high, min(inv_elevation_high), max(inv_elevation_high))){
    stop("The higher elevation limit must be within 184 and 8718.")}
  elevation_low_match <- which(inv_elevation_low <= elevation_low)
  elevation_high_match <- which(inv_elevation_high >= elevation_high)
  ranges <- gmba_inv()$GMBA_V2_ID[intersect(elevation_low_match, elevation_high_match)]
  return(ranges)
}
