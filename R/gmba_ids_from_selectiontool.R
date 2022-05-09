#' Get mountain range IDs from the Selection Tool
#'
#' Get your pre-selected GMBA Inventory v2.0 mountain range IDs from the \href{https://github.com/GMBA-biodiversity/Inventory/blob/main/Gmba_Inventory_v2.0_Selection_Tool_20200330.xlsx}{Selection Tool} Excel file.
#'
#' @param local Character string containing the path to the Selection Tool on the local drive
#' @param overlap Logical, set if to output those selected mountain ranges from the Selection Tool
#' that overlap with other (only those!) or not. Default is set to FALSE
#'
#' @return Vector of the GMBA Inventory v2.0 mountain range names associated with the input IDs
#'
#' @import openxlsx
#' @export
#'
#' @examples
#' \dontrun{
#' selection <- gmba_ids_from_selectiontool("Selection_Tool.xlsx")
#' selection_overlapping <- gmba_ids_from_selectiontool("Selection_Tool.xlsx",
#'                                                      overlap = TRUE)
#' }

gmba_ids_from_selectiontool <- function(local, overlap = FALSE){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}

  ##### set attributes
  inv_ids <- attributetable()$GMBA_V2_ID

  ##### run function
  selectiontool <- read.xlsx(local, sheet = 1)
  if(overlap == FALSE){
    output <- inv_ids[!is.na(selectiontool$Range_Selector)]
  }
  if(overlap == TRUE){
    overlapping <- which(selectiontool$Overlap_Warning == "polygons overlap!")
    if(length(overlapping) == 0){
      stop("There are no overlapping polygons selected in the Selection Tool.")}
    else {
      output <- inv_ids[overlapping]
    }
  }

  ##### return output
  return(output)

}
