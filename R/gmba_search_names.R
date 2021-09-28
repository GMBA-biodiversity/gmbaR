#' Search for mountain range names
#'
#' Search GMBA Inventory v2.0 mountain range names based on parts of names.
#'
#' @param part_of_rangename Character string of part of a mountain range name
#' @param language Character string of a language to select. The options are:
#' \itemize{
#' \item{\emph{"EN"} = English (default)}
#' \item{\emph{"DE"} = German / Deutsch}
#' \item{\emph{"ES"} = Spanish / español}
#' \item{\emph{"FR"} = French / français}
#' \item{\emph{"PT"} = Portuguese / português}
#' \item{\emph{"RU"} = Russian / русский}
#' \item{\emph{"TR"} = Turkish / Türkçe}
#' \item{\emph{"ZH"} = Chinese / 中文}
#' }
#' Note: Mountain names in a language other than English are not complete but
#' only available for those where Wikidata has a translation available.
#' @param db Logical, indicating if to output a named vector. If \emph{TRUE},
#' the translation (language other than English) will be the name of the
#' database name of each mountain range. If \emph{FALSE}, the translation
#' (language other than English) will be given. The argument is only needed when
#' \code{language} is set to other than \emph{EN}. Default is set to \emph{FALSE}
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range names
#' associated with the input name part
#'
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#'  gmba_search_names("alp")
#'  gmba_search_names("alp", "DE")
#'  gmba_search_names("alp", "DE", TRUE)
#' }

gmba_search_names <- function(part_of_rangename, language = "EN", db = FALSE){
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  language <- match.arg(language, c("EN", "DE", "ES", "FR", "PT", "RU", "TR", "ZH"))
  name_vector_lowercase <- tolower(part_of_rangename)
  if(language == "EN"){
    inv_names <- as.character(gmba_inv()$DBaseName)
  }
  if(language == "DE"){
    inv_names <- as.character(gmba_inv()$Name_DE)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "ES"){
    inv_names <- as.character(gmba_inv()$Name_ES)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "FR"){
    inv_names <- as.character(gmba_inv()$Name_FR)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "PT"){
    inv_names <- as.character(gmba_inv()$Name_PT)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "RU"){
    inv_names <- as.character(gmba_inv()$Name_RU)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "TR"){
    inv_names <- as.character(gmba_inv()$Name_TR)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  if(language == "ZH"){
    inv_names <- as.character(gmba_inv()$Name_ZH)
    inv_names <- inv_names[-which(is.na(inv_names))]
  }
  inv_names_lowercase <- tolower(inv_names)
  rangenames <- inv_names[which(grepl(name_vector_lowercase, inv_names_lowercase))]
  if(language != "EN"){
    if(!(db %in% c(FALSE, TRUE))){stop("Argument 'db' must be set to FALSE or TRUE")}
    if(isTRUE(db)){
      if(language == "DE"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_DE %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "ES"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_ES %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "FR"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_FR %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "PT"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_PT %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "RU"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_RU %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "TR"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_TR %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "ZH"){
        rangenames_names <- rangenames
        rangenames <- attributetable()[which(attributetable()$Name_ZH %in% rangenames),
                                     which(names(attributetable()) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
    }
  }
  return(rangenames)
}
