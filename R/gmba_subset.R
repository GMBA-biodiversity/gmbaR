#' Subset the GMBA Inventory v2.0
#'
#' Get a subset of the GMBA Inventory v2.0, based on mountain range IDs.
#'
#' @param rangeid_vector Vector (character or numeric) of GMBA Inventory v2.0 mountain range IDs. Optionally, the selections \emph{"basic}, \emph{"300}" and \emph{"100"} can be used:
#' \itemize{
#' \item{\emph{"basic"} = basic mountain ranges only}
#' \item{\emph{"300"} = ranges included in the 300 pre-selection}
#' \item{\emph{"100"} = ranges included in the 100 pre-selection}}
#'
#' @return An \code{sf} object containing the given mountain ranges
#'
#' @export
#'
#' @examples
#' \dontrun{
#' blackforest_europeanalps <- gmba_subset(c(11175, 10001))
#' blackforest_europeanalps <- gmba_subset(c("11175", "10001"))
#' major_291_mountains_of_the_world <- gmba_subset("300")
#' }

gmba_subset <- function(rangeid_vector){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }

  ##### run function
  if(rangeid_vector == "basic"){
    rangeid_vector <- gmba_ids_from_selection("basic")}
  if(rangeid_vector == "300"){
    rangeid_vector <- gmba_ids_from_selection("300")}
  if(rangeid_vector == "100"){
    rangeid_vector <- gmba_ids_from_selection("100")}

  r <- which(gmba_inv()$GMBA_V2_ID %in% rangeid_vector)
  output <- gmba_inv()[r,]

  ##### return output
  return(output)
}
