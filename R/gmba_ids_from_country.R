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

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }

  ###### set attributes
  inv_countries <- tolower(attributetable()$Countries)

  ###### check arguments
  # iso
  iso <- gsub(" ", "", iso, fixed = TRUE)
  iso <- tolower(unlist(strsplit(iso, ",")))
  if(sum(unique(nchar(iso))) != 3){
    stop("The input must be one or more ISO-3 code(s), all with 3 characters.")
  }

  ###### run function
  output <- NA

  for(i in 1:length(iso)){
    r <- which(grepl(iso[i], inv_countries))
    c <- which(names(attributetable()) == "GMBA_V2_ID")
    rangesloop <- attributetable()[r,c]
    output <- c(output, rangesloop)
  }

  output <- unique(output)
  output <- output[-which(is.na(output))]

  if(length(output) == 0){
    warning("For the selected countries, no GMBA Inventory v2.0 IDs were found.")
  }

  ###### return output
  return(output)

}
