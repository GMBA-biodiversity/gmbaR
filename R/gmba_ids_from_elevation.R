#' Get mountain range IDs from elevation range
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within a certain mountain elevation range
#'
#' @param elevation_low Numeric, lower limit of an elevation range
#' @param elevation_high Numeric, higher limit of an elevation range
#' @param range_selection Character string, the options are:
#' \itemize{
#' \item{\emph{"all"} = all level ranges, cross-scale (returns lists of ids!) (default)}
#' \item{\emph{"basic"} = basic mountain ranges only}
#' \item{\emph{"300"} = a selection for global or IPBES/IPCC regional level analyses}
#' \item{\emph{"100"} = a selection for global analyses}
#' \item{\emph{"level1"}-\emph{"level10"} = ranges pertaining to the respective level
#' of the inventory hierarchy}
#' \item{\emph{"manual"} = range IDs provided with \code{manual}}}
#' @param manual Character string containing the IDs of manually selected mountain ranges.
#' Only needed when \code{range_selection} is set to \emph{manual}. Default is set to \emph{NULL}
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

gmba_ids_from_elevation <- function(elevation_low, elevation_high,
                                    range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ##### range_selection and manual are checked in gmba_ids_from_selection()

  ##### set attributes
  r <- which(attributetable()$GMBA_V2_ID %in% gmba_ids_from_selection(range_selection, manual))
  c <- which(names(attributetable()) == "Elev_Low")
  inv_elevation_low <- attributetable()[r,c]
  c <- which(names(attributetable()) == "Elev_High")
  inv_elevation_high <- attributetable()[r,c]

  ###### check arguments
  # elevation_low
  if(!between(elevation_low, min(inv_elevation_low), max(inv_elevation_low))){
    stop("The lower elevation limit must be within ", floor(min(inv_elevation_low)), " and ", ceiling(max(inv_elevation_low)), ".")}
  # elevation_high
  if(!between(elevation_high, min(inv_elevation_high), max(inv_elevation_high))){
    stop("The higher elevation limit must be within ", floor(min(inv_elevation_high)), " and ", ceiling(max(inv_elevation_high)), ".")}
  # elevation plausibility check
  if(!(elevation_low <= elevation_high)){
    stop("The 'elevation_high' must be equal to or higher than the 'elevation_low'.")}

  ###### run function
  elevation_low_match <- which(inv_elevation_low <= elevation_low)
  elevation_high_match <- which(inv_elevation_high >= elevation_high)
  ranges <- intersect(elevation_low_match, elevation_high_match)
  r <- ranges
  c <- which(names(attributetable()) == "GMBA_V2_ID")
  output <- attributetable()[r,c]

  ###### return output
  return(output)

}
