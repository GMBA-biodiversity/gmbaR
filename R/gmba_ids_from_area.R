#' Get mountain range IDs from area
#'
#' Get GMBA Inventory v2.0 mountain range IDs of mountain ranges that have a certain size in area
#'
#' @param area Numeric, threshold value of area in size, given in km^2
#' @param calc Character string, should the area be taken as \emph{"max"}imum or \emph{"min"}imum?
#' @param range_selection Character string, the options are:
#' \itemize{
#' \item{\emph{"all"} = all level ranges, cross-scale (returns lists of ids!) (default)}
#' \item{\emph{"basic"} = basic mountain ranges only}
#' \item{\emph{"300"} = ranges included in the 300 pre-selection}
#' \item{\emph{"100"} = ranges included in the 100 pre-selection}
#' \item{\emph{"level1"}-\emph{"level10"} = ranges pertaining to the respective level
#' of the inventory hierarchy}
#' \item{\emph{"manual"} = range IDs provided with \code{manual}}}
#' @param manual Character string containing the IDs of manually selected mountain ranges.
#' Only needed when \code{range_selection} is set to \emph{manual}. Default is set to \emph{NULL}
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

gmba_ids_from_area <- function(area, calc,
                               range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ###### range_selection and manual are checked in gmba_ids_from_selection()

  ###### set attributes
  r <- which(attributetable()$GMBA_V2_ID %in% gmba_ids_from_selection(range_selection, manual))
  c <- which(names(attributetable()) == "Area")
  inv_area <- attributetable()[r,c]

  ###### check arguments
  # area
  if(!between(area, min(inv_area), max(inv_area))){
    stop("The lower elevation limit must be within ", floor(min(inv_area)), " and ", ceiling(max(inv_area)), ".")}
  # calc
  calc <- match.arg(calc, c("max", "min"))

  ###### run function
  r <- which(names(attributetable()) == "GMBA_V2_ID")
  if(calc == "max"){c <- which(inv_area < area)}
  if(calc == "min"){c <- which(inv_area > area)}
  output <- attributetable()[r,c]

  ###### return output
  return(output)

}
