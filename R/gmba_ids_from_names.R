#' Get mountain range IDs from mountain range names
#'
#' Get GMBA Inventory v2.0 mountain range IDs from known mountain range names.
#'
#' @param rangename_vector Character vector of GMBA Inventory v2.0 mountain range names.
#' Use \code{gmba_search_names()} if you do not know the inventory names needed
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs associated
#' with the input names
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rangeid <- gmba_ids_from_names("Black Forest")
#' rangeids <- gmba_ids_from_names(c("Black Forest", "European Alps"))
#' }

gmba_ids_from_names <- function(rangename_vector){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }

  ##### set attributes
  inv_ids <- attributetable()$GMBA_V2_ID
  inv_names <- attributetable()$DBaseName

  ##### run function
  output <- inv_ids[which(inv_names %in% rangename_vector)]

  ##### return output
  if(length(output) == 0){
    warning("The given mountain range(s) do not exist. Misspelled? Use gmba_search_names() to find the inventory names needed.\n")}
  else {
    return(output)
  }

}
