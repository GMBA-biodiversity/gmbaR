#' Get mountain range IDs from points coordinates
#'
#' Get GMBA Inventory v2.0 mountain range IDs based on xy point coordinates.
#'
#' @param xy_dataframe A dataframe containing point coordinates,
#' with columns "x" and "y" i.e. longitude and latitude
#' @param range_selection Character string, the options are:
#' \itemize{
#' \item{\emph{"all"} = all level ranges, cross-scale (returns lists of ids!)}
#' \item{\emph{"basic"} = basic mountain ranges only}
#' \item{\emph{"100"} = ranges included in the 100 pre-selection}
#' \item{\emph{"300"} = ranges included in the 300 pre-selection}
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

gmba_ids_from_points <- function(xy_dataframe, range_selection, manual = NULL){
  range_selection <- match.arg(range_selection, c("all", "basic", "100", "300",
                                                  "level1", "level2", "level3", "level4", "level5",
                                                  "level6", "level7", "level8", "level9", "level10",
                                                  "manual"))
  points <- xy_dataframe
  if(range_selection == "all"){
    polygons <- gmba_inv()
  } else
    if(range_selection == "basic"){
      polygons <- gmba_inv()[which(gmba_inv()$MapUnit == "Basic"),]
    } else
      if(range_selection == "100"){
        polygons <- gmba_inv()[which(gmba_inv()$GMBA_V2_ID %in% gmba_ids_from_preselection("100")),]
      } else
        if(range_selection == "300"){
          polygons <- gmba_inv()[which(gmba_inv()$GMBA_V2_ID %in% gmba_ids_from_preselection("300")),]
        } else
          if(range_selection == "level1"){
            polygons <- gmba_inv()[which(gmba_inv()$Level == 1),]
          } else
            if(range_selection == "level2"){
              polygons <- gmba_inv()[which(gmba_inv()$Level == 2),]
            } else
              if(range_selection == "level3"){
                polygons <- gmba_inv()[which(gmba_inv()$Level == 3),]
              } else
                if(range_selection == "level4"){
                  polygons <- gmba_inv()[which(gmba_inv()$Level == 4),]
                } else
                  if(range_selection == "level5"){
                    polygons <- gmba_inv()[which(gmba_inv()$Level == 5),]
                  } else
                    if(range_selection == "level6"){
                      polygons <- gmba_inv()[which(gmba_inv()$Level == 6),]
                    } else
                      if(range_selection == "level7"){
                        polygons <- gmba_inv()[which(gmba_inv()$Level == 7),]
                      } else
                        if(range_selection == "level8"){
                          polygons <- gmba_inv()[which(gmba_inv()$Level == 8),]
                        } else
                          if(range_selection == "level9"){
                            polygons <- gmba_inv()[which(gmba_inv()$Level == 9),]
                          } else
                            if(range_selection == "level10"){
                              polygons <- gmba_inv()[which(gmba_inv()$Level == 10),]
                            }
  if(range_selection == "manual"){
    if(is.null(manual)){
      stop("No mountain range selection provided.")
    }
    polygons <- gmba_subset(range_selection)
  }
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
  return(points)
}
