#' Get mountain range IDs from area
#'
#' Get GMBA Inventory v2.0 mountain range IDs of mountain ranges that have a certain size in area
#'
#' @param area Numeric, threshold value of area in size, given in km^2
#' @param calc Character string, should the area be taken as \emph{"max"}imum or \emph{"min"}imum?
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs having a smaller or bigger
#' size in area as submitted
#'
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' rangeids <- gmba_ids_from_area(5000, "max")
#' rangeids <- gmba_ids_from_area(5000, "min")
#' }

gmba_ids_from_area <- function(area, calc){

  ###### check arguments
  # area
  inv_area <- attributetable()$Area
  if(!between(area, min(inv_area), max(inv_area))){
    stop("The lower elevation limit must be within 2 and 14480076.")}

  # calc
  calc <- match.arg(calc, c("max", "min"))

  ###### run function
  if(calc == "max"){
    output <- gmba_inv()$GMBA_V2_ID[which(inv_area < area)]
  }
  if(calc == "min"){
    output <- gmba_inv()$GMBA_V2_ID[which(inv_area > area)]
  }

  ###### return output
  return(output)

}
