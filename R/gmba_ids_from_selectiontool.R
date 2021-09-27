#' Get mountain range IDs from the Selection Tool
#'
#' Get your pre-selected GMBA Inventory v2.0 mountain range IDs from the \href{url}{Selection Tool} Excel file.
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
#' selection <- gmba_ids_from_selectiontool("Selection_Tool_v20210423.xlsx")
#' selection_overlapping <- gmba_ids_from_selectiontool("Selection_Tool_v20210423.xlsx",
#'                                                      overlap = TRUE)
#' }

gmba_ids_from_selectiontool <- function(local, overlap = FALSE){
  gmba_inventory_v_2_0_selectiontool <- read.xlsx(local, sheet = 1)
  names(gmba_inventory_v_2_0_selectiontool)[c(7:16)] <- paste("Level", c(1:10), sep = "")
  gmba_inventory_v_2_0_selectiontool$GMBA_V2_ID <- as.character(gmba_inventory_v_2_0_selectiontool$GMBA_V2_ID)

  gmba_inventory_ids <- gmba_inventory_v_2_0_selectiontool$GMBA_V2_ID
  if(overlap == FALSE){
    selected_ids <- gmba_inventory_ids[!is.na(gmba_inventory_v_2_0_selectiontool$Range_Selector)]
    return(selected_ids)
  }
  if(overlap == TRUE){
    overlapping <- which(gmba_inventory_v_2_0_selectiontool$Overlap_Warning == "polygons overlap!")
    if(length(overlapping) == 0){
      stop("There are no overlapping polygons selected in the Selection Tool.")}
    else {
      selected_ids <- gmba_inventory_ids[overlapping]
      return(selected_ids)
    }
  }
}
