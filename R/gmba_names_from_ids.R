#' Get mountain range names from mountain range IDs
#'
#' Get GMBA Inventory v2.0 mountain range names from known mountain range IDs.
#'
#' @param rangeid_vector Vector (character or numeric) of GMBA Inventory v2.0 mountain range IDs
#'
#' @return Vector of the GMBA Inventory v2.0 mountain range names associated with the input IDs
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  rangenames <- gmba_names_from_ids(c(11175, 10001))
#'  rangenames <- gmba_names_from_ids(c("11175", "10001"))
#' }

gmba_names_from_ids <- function(rangeid_vector){
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  inv_names <- as.character(gmba_inv()$DBaseName)
  inv_ids <- as.character(gmba_inv()$GMBA_V2_ID)
  rangenames <- inv_names[match(rangeid_vector, inv_ids)]
  names(rangenames) <- rangeid_vector
  return(rangenames)
}
