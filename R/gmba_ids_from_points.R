#' Get mountain range IDs from points coordinates
#'
#' Get GMBA Inventory v2.0 mountain range IDs based on xy point coordinates.
#'
#' @param xy_dataframe A dataframe containing point coordinates,
#' with columns "x" and "y" i.e. longitude and latitude
#' @param range_selection Character string, the options are:
#' \itemize{
#' \item{\emph{"all"} = all level ranges, cross-scale (attention: may need some processing time!)}
#' \item{\emph{"basic"} = basic mountain ranges only (default)}
#' \item{\emph{"300"} = ranges included in the 300 pre-selection}
#' \item{\emph{"100"} = ranges included in the 100 pre-selection}
#' \item{\emph{"level1"}-\emph{"level10"} = ranges pertaining to the respective level
#' of the inventory hierarchy}
#' \item{\emph{"manual"} = range IDs provided with \code{manual}}}
#' @param manual Character string containing the IDs of manually selected mountain ranges.
#' Only needed when \code{range_selection} is set to \emph{manual}. Default is set to \emph{NULL}
#'
#' @return The input \code{xy_dataframe} with an additional column "GMBA_V2_ID", containing
#' the GMBA Inventory v2.0 mountain range IDs associated with the respective input point coordinate.
#' In case a point coordinate is located in more than one mountain polygon (cross-scale), they
#' are given as a collapsed character string, separated by comma. If a point coordinate does not lie
#' within a mountain range polygon, an \emph{NA} is returned
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
                                 range_selection = "basic", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ##### range_selection and manual are checked in gmba_ids_from_selection()

  ##### set attributes
  inv_ids <- gmba_inv()$GMBA_V2_ID

  ##### run function
  r <- which(inv_ids %in% gmba_ids_from_selection(range_selection, manual))
  selection_ids <- inv_ids[r]
  polygons <- gmba_inv()[r,]
  points <- xy_dataframe
  coordinates <- do.call("st_sfc",c(lapply(1:nrow(xy_dataframe),
                                           function(i){st_point(as.numeric(xy_dataframe[i,]))}), list("crs" = 4326)))
  # transformation to planar, since the sf library assumes a planar projection
  polygons_pl <- st_transform(polygons, 2163)
  coordinates_pl <- st_transform(coordinates, 2163)
  # intersect points with polygons
  points$GMBA_V2_ID <- NA
  c <- which(names(points) == "GMBA_V2_ID")
  for(r in 1:nrow(points)){
    intersections_r <- st_intersects(polygons_pl, coordinates_pl[[r]])
    intersections_r <- as.vector(as.matrix(intersections_r))
    ranges <- paste(sort(selection_ids[which(intersections_r == TRUE)], decreasing = FALSE),
                    collapse = ",", sep = "")
    points[r,c] <- ranges
    if(points[r,c] == ""){points[r,c] <- NA}
  }
  output <- points

  ##### return output
  return(output)
}
