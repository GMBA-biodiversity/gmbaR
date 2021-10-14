#' Read the GMBA Inventory v2.0 to R
#'
#' Read the GMBA Inventory v2.0 (standard extend, all layers) to R, from the web or the local drive. This function is to be used
#' without an assignment operator (e.g. "<-").
#'
#' @param source Character string, options are \emph{"web"} (Default) or \emph{"local"}
#' @param extend Character string, options are \emph{"standard"} (Default) or \emph{"broad"}. Check \url{https://www.earthenv.org/mountains}
#' for the details on the options. Ignored when \code{source} is set to \emph{"local"}
#' @param local Character string containing the path to the inventory shapefile on the local drive.
#' Only needed when \code{source} is set to \emph{"local"}. Default is set to \emph{NULL}
#'
#' @return \code{gmba_inv()}, a function containing the GMBA Inventory v2.0
#'
#' @references
#' \url{https://www.earthenv.org/mountains}
#'
#' @import sf utils
#' @export
#'
#' @examples
#' \dontrun{
#' gmba_read("web")
#' gmba_read("web", extend = "broad")
#' gmba_read(source = "local", local = "GMBA_Inventory_v2.0.shp")
#' }

gmba_read <- function(source = "web", extend = "standard", local = NULL) {

  ##### check attributes
  source <- match.arg(source, c("web", "local"))

  ##### run function
  # source = web
  if(source == "web"){
    extend <- match.arg(extend, c("standard", "broad"))
    # set the inventory web link
    if(extend == "standard"){ # default layer: standard extent, all layers
      webfile <- "https://data.earthenv.org/mountains/standard/GMBA_Inventory_v2.0.zip"
    }
    if(extend == "broad"){
      webfile <- "https://data.earthenv.org/mountains/broad/GMBA_Inventory_v2.0_broad.zip"
    }
    # create temp files
    temp <- tempfile()
    temp2 <- tempfile()
    # download the inventory zip and save it in 'temp'
    download.file(webfile, temp)
    # unzip the inventory in 'temp' and save it in 'temp2'
    unzip(zipfile = temp, exdir = temp2)
    # find the filepath of the inventory shapefile (.shp) in temp2
    # the $ at the end of ".shp$" ensures we are not also finding files such as .shp.xml
    shp_file <- list.files(temp2, pattern = ".shp$", full.names=TRUE)
    # read the shapefile
    if(extend == "standard"){
      gmba_inventory_v_2_0 <- st_read(shp_file, quiet=TRUE)
    }
    if(extend == "broad"){
      gmba_inventory_v_2_0_broad <- st_read(shp_file, quiet=TRUE)
    }
  }

  # source = local
  if(source == "local"){
    if(is.null(local)){
      stop("No file path to the inventory shapefile provided.")
    }
    # read the shapefile
    gmba_inventory_v_2_0 <- st_read(local, quiet=TRUE)
  }

  ##### return the inventory
  # standard extend
  if(exists("gmba_inventory_v_2_0")){
    gmba_inventory_v_2_0$GMBA_V2_ID <- as.character(gmba_inventory_v_2_0$GMBA_V2_ID)
    return(assign("gmba_inv",
                  local({
                    function() gmba_inventory_v_2_0
                  }),
                  envir = parent.frame()))
  }
  # broad extend
  if(exists("gmba_inventory_v_2_0_broad")){
    warning("Note you read the GMBA Inventory 2.0 in its broad extend. With this, some of the gmbaR functions might not fully work, as they depend on attributes only available in the standard extend version.",
            immediate. = TRUE)
    gmba_inventory_v_2_0_broad$GMBA_V2_ID <- as.character(gmba_inventory_v_2_0_broad$GMBA_V2_ID)
    return(assign("gmba_inv",
                  local({
                    function() gmba_inventory_v_2_0_broad
                  }),
                  envir = parent.frame()))
  }

}
