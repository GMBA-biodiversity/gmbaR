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
#' (language other than English) will be given. Only needed when \code{language} is set
#' to other than \emph{EN}. Default therefore is set to \emph{NULL}
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range names
#' associated with the input name part
#'
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#'  gmba_search_names("forest")
#' }

gmba_search_names <- function(part_of_rangename, language = "EN", db = "NULL"){
  # check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }
  # function
  language <- match.arg(language, c("EN", "DE", "ES", "FR", "PT", "RU", "TR", "ZH"))
  name_vector_lowercase <- tolower(part_of_rangename)
  if(language == "EN"){
    gmba_inventory_names <- as.character(gmba_inv()$DBaseName)
  }
  if(language == "DE"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_DE)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "ES"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_ES)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "FR"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_FR)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "PT"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_PT)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "RU"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_RU)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "TR"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_TR)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  if(language == "ZH"){
    gmba_inventory_names <- as.character(gmba_inv()$Name_ZH)
    gmba_inventory_names <- gmba_inventory_names[-which(is.na(gmba_inventory_names))]
  }
  gmba_inventory_names_lowercase <- tolower(gmba_inventory_names)
  rangenames <- gmba_inventory_names[which(grepl(name_vector_lowercase, gmba_inventory_names_lowercase))]
  if(language != "EN"){
    db <- match.arg(db, c(TRUE, FALSE))
    if(db == TRUE){
      attributetable <- gmba_inv()[,c(2,3:35)] # ADJUST COLUMNS
      attributetable <- st_set_geometry(attributetable, NULL)
      if(language == "DE"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_DE %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "ES"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_ES %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "FR"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_FR %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "PT"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_PT %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "RU"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_RU %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "TR"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_TR %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
      if(language == "ZH"){
        rangenames_names <- rangenames
        rangenames <- attributetable[which(attributetable$Name_ZH %in% rangenames),
                                     which(names(attributetable) == "DBaseName")]
        names(rangenames) <- rangenames_names
      }
    }
  }
  return(rangenames)
}
