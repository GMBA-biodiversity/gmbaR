#' Read the GMBA Inventory v2.0 to R
#'
#' Read the GMBA Inventory v2.0 to R, from the web or the local drive. This function is to be used
#' without an assignment operator (e.g. "<-").
#'
#' @param source Character string, options are \emph{web} (Default) or \emph{local}
#' @param local Character string containing the path to the inventory shapefile on the local drive.
#' Only needed when \code{source} is set to \emph{local}. Default is set to \emph{NULL}
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
#' gmba_read(source = "local", local = "GMBA_Inventory_V2_All_Attributes.shp")
#' }

gmba_read <- function(source = "web", local = NULL) { # WEB TO ADD

  ##### check attributes
  source <- match.arg(source, c("web", "local"))

  ##### run function
  # source = web
  if(source == "web"){
    # set the inventory web link (default layer: standard extent, all layers)
    webfile <- "https://data.earthenv.org/mountains/standard/GMBA_Inventory_v2.0.zip"
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
    gmba_inventory_v_2_0 <- st_read(shp_file, quiet=TRUE)
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
  gmba_inventory_v_2_0$GMBA_V2_ID <- as.character(gmba_inventory_v_2_0$GMBA_V2_ID)
  return(assign("gmba_inv",
                local({
                  function() gmba_inventory_v_2_0
                }),
                envir = parent.frame()))

}
