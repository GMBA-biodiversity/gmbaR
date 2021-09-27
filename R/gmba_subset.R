#' Subset the GMBA Inventory v2.0
#'
#' Get a subset of the GMBA Inventory v2.0, based on mountain range IDs.
#'
#' @param rangeid_vector Vector (character or numeric) of GMBA Inventory v2.0 mountain range IDs
#'
#' @return An \code{sf} object containing the given mountain ranges
#'
#' @export
#'
#' @examples
#' \dontrun{
#' blackforest_europeanalps <- gmba_subset(c(11175, 10001))
#' blackforest_europeanalps <- gmba_subset(c("11175", "10001"))
#' }

gmba_subset <- function(rangeid_vector){
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  subset <- gmba_inv()[which(gmba_inv()$GMBA_V2_ID %in% rangeid_vector),]
  return(subset)
}
