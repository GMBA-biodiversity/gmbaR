#' gmbaR
#'
#' The GMBA Mountain Inventory v2.0 is an inventory of 8329 mountain ranges across the world,
#' stored in a shapefile with a growing attribute table. Here, we provide a few R functions
#' to explore the inventory and select mountain ranges based on user define criteria for
#' subsequent analyses.
#'
#' @docType package
#' @name gmbaR
#' @keywords internal
#' @import sf grDevices
"_PACKAGE"

## helper functions

# function to retrieve the attribute table from gmba_inv()
attributetable <- function(){

  ##### run function
  output <- gmba_inv()[,c(which(names(gmba_inv()) %in% c("GMBA_V2_ID",
                                                   "GMBA_V1_ID",
                                                   "MapName",
                                                   "WikiDataUR",
                                                   "MapUnit",
                                                   "Level",
                                                   "Feature",
                                                   "Area",
                                                   "Perimeter",
                                                   "Elev_Low",
                                                   "Elev_High",
                                                   "Path",
                                                   "Level_01",
                                                   "Level_02",
                                                   "Level_03",
                                                   "Level_04",
                                                   "Level_05",
                                                   "Level_06",
                                                   "Level_07",
                                                   "Level_08",
                                                   "Level_09",
                                                   "Level_10",
                                                   "Select_300",
                                                   "Countries",
                                                   "WikiDataID",
                                                   "DBaseName",
                                                   "AsciiName",
                                                   "Name_DE",
                                                   "Name_ES",
                                                   "Name_FR",
                                                   "Name_PT",
                                                   "Name_RU",
                                                   "Name_TR",
                                                   "Name_ZH")))]
  output <- st_set_geometry(output, NULL)

  ##### return output
  return(output)

}

# function to get pre-selection of polygon ids
# preselection options:
# "300" = a selection for global or IPBES/IPCC regional level analyses (proposed by M. Snethlage)
# "100" = a selection for global analyses (proposed by J. Geschke);
gmba_ids_from_preselection <- function(preselection){

  ##### check arguments
  preselection <- match.arg(preselection, c("300", "100"))

  ##### run function
  if(preselection == "300"){
    output <- gmba_inv()$GMBA_V2_ID[which(gmba_inv()$Select_300 == "x")]
  }
  if(preselection == "100"){
    output <- gmba_inv()$GMBA_V2_ID[which(gmba_inv()$GMBA_V2_ID %in%
                                                  c(14458, 13127, 12149, 12171, 11328, 14260, 18512, 12170, 10076, 15591,
                                                    18573, 12165, 11744, 12275, 18442, 18171, 11419, 12409, 13907, 12031,
                                                    13075, 11400, 11401, 11464, 11703, 12174, 11997, 12508, 12260, 12507,
                                                    18562, 21027, 12536, 13970, 11225, 11486, 19097, 12419, 11496, 11539,
                                                    12675, 12416, 12457, 15490, 10001, 11608, 11201, 19031, 19241, 19236,
                                                    19237, 12883, 14301, 12509, 12259, 12505, 20365, 11117, 11123, 11563,
                                                    12227, 13955, 14060, 18557, 17999, 11712, 14033, 18391, 11957, 18501,
                                                    14131, 12535, 12506, 21038, 12541, 12686, 12307, 12542, 15783, 12543,
                                                    21019, 12385, 12105, 11134, 12497, 12390, 12551, 12189, 11983, 12532,
                                                    12122, 12123, 12121, 12147, 14220, 14221, 14239, 12142, 14218, 14219))]
  }

  ##### return output
  return(output)

}

# function to create color transparency (e.g. for maps)
# not specifically relevant for the GMBA Inventory v2.0 but useful in general
add_alpha <- function(col, alpha = 1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2,
        function(x)
          rgb(x[1], x[2], x[3], alpha = alpha))
}
