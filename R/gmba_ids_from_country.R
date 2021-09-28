#' Get mountain range IDs from country codes
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within one or more ISO-3 country codes
#'
#' @param iso Character vector of one or more ISO-3 country code(s). If codes are not known, check
#' \href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{Wikipedia} or use the R package
#' \href{https://cran.r-project.org/web/packages/countrycode/index.html}{countrycode} to convert
#' country names
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

gmba_ids_from_country <- function(iso){
  attributes <- names(attributetable())
  countries <- attributetable()$Countries
  countries <- tolower(countries)
  iso <- gsub(" ", "", iso, fixed = TRUE)
  iso <- tolower(unlist(strsplit(iso, ",")))
  if(sum(unique(nchar(iso))) != 3){
    stop("The input must be one or more ISO-3 code(s), all with 3 characters.")
  }
  ranges <- NA
  for(i in 1:length(iso)){
    rangesloop <- attributetable()[which(grepl(iso[i], countries)), which(attributes == "GMBA_V2_ID")]
    ranges <- c(ranges, rangesloop)
  }
  ranges <- unique(ranges)
  ranges <- ranges[-which(is.na(ranges))]
  if(length(ranges) == 0){
    warning("For the selected countries, no GMBA Inventory v2.0 IDs were found.")
  }
  return(ranges)
}
