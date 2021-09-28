#' Get a lower mountain range ID from a higher mountain range ID
#'
#' Get a certain lower GMBA Inventory v2.0 mountain range ID from a higher mountain range ID.\cr\cr
#' Within the GMBA Inventory v2.0 hierarchy, "lower" levels are spatially high,
#' such as on the continental level. Consequently, "higher" levels are those of spatial detail,
#' such as individual mountain ranges within a mountain system.
#'
#' @param rangeid String (character or numeric) of a single GMBA Inventory v2.0 mountain range ID
#' @param lowerlevel_numeric A single number (as numeric), indicating what lower level to be given
#' @param method Character string, the options are:
#' \itemize{
#' \item{\emph{"parent"} = a certain lower range of range ID}
#' \item{\emph{"steps"} = number of lower levels from range ID}}
#' See the \href{https://github.com/GMBA-biodiversity/gmbaR/blob/main/README.md}{README} for a visual explanation of the two methods
#'
#' @return Character string of the lower GMBA Inventory v2.0 mountain range ID associated
#' with the input ID
#'
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#' Europe <- gmba_lower_id_from_higher(11175, 2, method = "parent")
#' CentralUplands <- gmba_lower_id_from_higher(11175, 2, method = "steps")
#' }

gmba_lower_id_from_higher <- function(rangeid, lowerlevel_numeric, method){
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  method <- match.arg(method, c("parent", "steps"))
  # attribute objects
  attributetable <- attributetable()
  attributes <- names(attributetable)
  ids <- attributetable$GMBA_V2_ID
  # method = parent
  if(method == "parent"){
    # identify parent range path and id
    r <- which(ids == as.character(rangeid))
    c <- which(attributes == "Path")
    rangepath <- attributetable[r,c]
    parentrangepath <- paste(unlist(strsplit(rangepath, " > "))[c(1:lowerlevel_numeric)], collapse = " > ")
    r <- which(attributetable$Path == parentrangepath)
    c <- which(attributes == "GMBA_V2_ID")
    lowerlevel <- attributetable[r,c]
    return(lowerlevel)
  }
  # method = steps
  if(method == "steps"){
    if(lowerlevel_numeric > 0){
      # identify lower level
      r <- which(ids == as.character(rangeid))
      c <- which(attributes == "Path")
      rangepath <- attributetable[r,c]
      levels <- strsplit(rangepath, split = " > ", fixed = TRUE)
      levels <- levels[[1]]
      lowerlevel <- which(levels == attributetable$DBaseName[r])-lowerlevel_numeric
      if(lowerlevel > 0){
        # dissolve lower level
        lowerlevel <- levels[lowerlevel]
        lowerlevel <- gmba_ids_from_names(lowerlevel)
        return(lowerlevel)
      } else {
        stop("The selected number of steps doesn't give a valid mountain range level.")
      }
    } else {
      stop("The number of steps needs to be 1 or higher.")
    }
  }
}
