#' Get mountain range IDs from country codes
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within one or more ISO-3 country codes
#'
#' @param iso Character vector of one or more ISO-3 country code(s). If codes are not known, check
#' \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{Wikipedia} or use the R package
#' \href{https://cran.r-project.org/web/packages/countrycode/index.html}{countrycode} to convert
#' country names
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
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs that are located within
#' the input country codes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rangeid <- gmba_ids_from_country("che")
#' rangeids <- gmba_ids_from_country(c("che", "deu"))
#' }

gmba_ids_from_countries <- function(iso,
                                    range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ##### range_selection and manual are checked in gmba_ids_from_selection()

  ###### set attributes
  r <- which(attributetable()$GMBA_V2_ID %in% gmba_ids_from_selection(range_selection, manual))
  c <- which(names(attributetable()) == "Countries")
  inv_countries <- tolower(attributetable()[r,c])

  ###### check arguments
  # iso
  iso <- gsub(" ", "", iso, fixed = TRUE)
  iso <- tolower(unlist(strsplit(iso, ",")))
  if(sum(unique(nchar(iso))) != 3){
    stop("The input must be one or more ISO-3 code(s), all with 3 characters.")}

  ###### run function
  ranges <- NA
  for(i in 1:length(iso)){
    r <- which(grepl(iso[i], inv_countries))
    c <- which(names(attributetable()) == "GMBA_V2_ID")
    rangesloop <- attributetable()[r,c]
    ranges <- c(ranges, rangesloop)
  }
  ranges <- unique(ranges)
  ranges <- ranges[-which(is.na(ranges))]
  if(length(ranges) == 0){
    warning("For the selected countries, no GMBA Inventory v2.0 IDs were found.")
  }
  output <- ranges

  ###### return output
  return(output)

}
