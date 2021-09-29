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

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}

  ##### set attributes
  inv_names <- attributetable()$DBaseName
  inv_ids <- attributetable()$GMBA_V2_ID

  ##### run function
  output <- inv_names[match(rangeid_vector, inv_ids)]
  names(output) <- rangeid_vector

  ##### return output
  return(output)

}
