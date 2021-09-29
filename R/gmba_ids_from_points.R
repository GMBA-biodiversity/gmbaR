#' Get mountain range IDs from points coordinates
#'
#' Get GMBA Inventory v2.0 mountain range IDs based on xy point coordinates.
#'
#' @param xy_dataframe A dataframe containing point coordinates,
#' with columns "x" and "y" i.e. longitude and latitude
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
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs associated
#' with the input names
#'
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#' rangeid_vector <- gmba_ids_from_points(xy, "300")
#' rangeid_vector <- gmba_ids_from_points(xy, "manual", c("11175", "10001"))
#' rangeid_vector <- gmba_ids_from_points(xy, "manual",
#'                                        gmba_ids_from_names(c("Black Forest", "European Alps")))
#' }

gmba_ids_from_points <- function(xy_dataframe,
                                 range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ##### range_selection and manual are checked in gmba_ids_from_selection()

  ##### run function
  r <- which(gmba_inv()$GMBA_V2_ID %in% gmba_ids_from_selection(range_selection, manual))
  polygons <- gmba_inv()[r,]
  points <- xy_dataframe
  coordinates <- do.call("st_sfc",c(lapply(1:nrow(xy_dataframe),
                                           function(i) {st_point(as.numeric(xy_dataframe[i,]))}), list("crs" = 4326)))
  points$polygons <- apply(st_intersects(polygons, coordinates, sparse = FALSE), 2,
                           function(col) {
                             polygons[which(col),]$GMBA_V2_ID
                           })
  if(range_selection != "all"){
    points$ranges <- NA
    for(r in 1:nrow(points)){
      if(length(unlist(points[r,which(names(points) == "polygons")])) != 0){
        points[r,which(names(points) == "ranges")] <- unlist(points[r,which(names(points) == "polygons")])
      }
    }
    points <- points[,-which(names(points) == "polygons")]
  } else
  {
    names(points)[which(names(points) == "polygons")] <- "ranges"
  }
  output <- points

  ##### return output
  return(output)
}
