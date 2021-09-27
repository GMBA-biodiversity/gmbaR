#' Search for mountain range names
#'
#' Search GMBA Inventory v2.0 mountain range names based on parts of names.
#'
#' @param part_of_rangename Character string of part of a mountain range name
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range names
#' associated with the input name part
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  gmba_search_names("forest")
#' }

gmba_search_names <- function(part_of_rangename){ # LANGUAGES TO ADD
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  name_vector_lowercase <- tolower(part_of_rangename)
  gmba_inventory_names <- as.character(gmba_inv()$DBaseName)
  gmba_inventory_names_lowercase <- tolower(gmba_inventory_names)
  rangenames <- gmba_inventory_names[which(grepl(name_vector_lowercase, gmba_inventory_names_lowercase))]
  return(rangenames)
}
