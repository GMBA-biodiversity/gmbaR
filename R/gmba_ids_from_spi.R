#' Get mountain range IDs from SPI regions
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within one or more (sub-) regions referenced to
#' in the science-policy-interfaces a) IPBES, the Intergovernmental Science-Policy Platform on
#' Biodiversity and Ecosystem Services, or b) the IPCC, Intergovernmental Panel on Climate Change.
#'
#' @param spi Character string, either \emph{"ipbes"} or \emph{"ipcc"}
#' @param regions Character vector, indicating the IPBES or IPCC regions to get mountain range
#' IDs for. See details for the format of regions needed
#' @param calc Calculation of how mountain ranges are associated with the IPBES or IPCC regions.
#' Options are \emph{"maj"} (default) and \emph{"str"}. See details for the methodologies
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
#' @details {
#' \bold{IPBES regions:}
#' The subsequent IPBES regions (and subregions) are implemented. In \code{region}, use the
#' (sub)region name in the same spelling, incl. upper and lower cases:
#' \enumerate{
#' \item{Africa (Central Africa, East Africa and adjacent islands, North Africa, Southern Africa, West Africa)}
#' \item{The Americas (Caribbean, Mesoamerica, North America, South America)}
#' \item{Asia and the Pacific (North-East Asia, Oceania, South Asia, South-East Asia, Western Asia)}
#' \item{Europe and Central Asia (Central and Western Europe, Central Asia, Eastern Europe)}
#' }
#'
#' \bold{IPCC regions:}
#' The subsequent IPCC AR6 regions are implemented. In \code{region}, use the region abbreviations
#' in upper cases:
#' \enumerate{
#' \item{Greenland/Iceland (GIC)}
#' \item{N.W.North-America (NWN)}
#' \item{N.E.North-America (NEN)}
#' \item{W.North-America (WNA)}
#' \item{C.North-America (CNA)}
#' \item{E.North-America (ENA)}
#' \item{N.Central-America (NCA)}
#' \item{S.Central-America (SCA)}
#' \item{Caribbean (CAR)}
#' \item{N.W.South-America (NWS)}
#' \item{N.South-America (NSA)}
#' \item{N.E.South-America (NES)}
#' \item{South-American-Monsoon (SAM)}
#' \item{S.W.South-America (SWS)}
#' \item{S.E.South-America (SES)}
#' \item{S.South-America (SSA)}
#' \item{N.Europe (NEU)}
#' \item{Western&Central-Asia (WCE)}
#' \item{E.Europe (EEU)}
#' \item{Mediterranean (MED)}
#' \item{Sahara (SAH)}
#' \item{Western-Africa (WAF)}
#' \item{Central-Africa (CAF)}
#' \item{N.Eastern-Africa (NEAF)}
#' \item{S.Eastern-Africa (SEAF)}
#' \item{W.Southern-Africa (WSAF)}
#' \item{E.Southern-Africa (ESAF)}
#' \item{Madagascar (MDG)}
#' \item{Russian-Arctic (RAR)}
#' \item{W.Siberia (WSB)}
#' \item{E.Siberia (ESB)}
#' \item{Russian-Far-East (RFE)}
#' \item{W.C.Asia (WCA)}
#' \item{E.C.Asia (ECA)}
#' \item{Tibetan-Planteau (TIB)}
#' \item{E.Asia (EAS)}
#' \item{Arabian-Peninsula (ARP)}
#' \item{S.Asia (SAS)}
#' \item{S.E.Asia (SEA)}
#' \item{N.Australia (NAU)}
#' \item{C.Australia (CAU)}
#' \item{E.Australia (EAU)}
#' \item{S.Australia (SAU)}
#' \item{New-Zealand (NZ)}
#' \item{E.Antarctica (EAN)}
#' \item{W.Antarctica (WAN)}
#' \item{Arctic-Ocean (ARO)}
#' \item{N.Pacific-Ocean (NPO)}
#' \item{Equatorial.Pacific-Ocean (EPO)}
#' \item{S.Pacific-Ocean (SPO)}
#' \item{N.Atlantic-Ocean (NAO)}
#' \item{Equatorial.Atlantic-Ocean (EAO)}
#' \item{S.Atlantic-Ocean (SAO)}
#' \item{Arabian-Sea (ARS)}
#' \item{Bay-of-Bengal (BOB)}
#' \item{Equatorial.Indic-Ocean (EIO)}
#' \item{S.Indic-Ocean (SIO)}
#' \item{Southern-Ocean (SOO)}
#' }
#'
#' \bold{The method behind \code{calc}:}
#' We attributed corresponding IPBES or IPCC regions to each of the GMBA Inventory v2.0
#' polygons by a geometric union between the GMBA_Inventory_v2.0_broad layer on the one hand and
#' the respective reference region shapefiles IPCC AR6 (\href{https://doi.org/10.5194/essd-12-2959-2020}{doi.org/10.5194/essd-12-2959-2020})
#' and IPBES (\href{https://doi.org/10.5281/zenodo.3928281}{doi.org/10.5281/zenodo.3928281}).
#' Area calculations for all \emph{GMBA_V2_ID} and IPBES or IPCC reference region combinations were
#' performed after reprojecting each of the two union layers to Mollweide. For each GMBA Inventory v2.0 polygon and
#' reference region combination, we selected the one with the largest overlap and attributed that
#' reference region to the respective \emph{GMBA_V2_ID} (\code{calc} = \emph{"maj"}). In addition, we created a
#' string with all the reference regions each \emph{GMBA_V2_ID} intersected with (\code{calc} = \emph{"str"}).
#' The default is set to \emph{"maj"}
#' }
#'
#' @return Character vector of the GMBA Inventory v2.0 mountain range IDs associated
#' with the input IPBES or IPCC regions
#'
#' @import openxlsx
#' @export
#'
#' @examples
#' \dontrun{
#' rangeids <- gmba_ids_from_ipcc("ipbes", c("Europe and Central Asia"))
#' }

gmba_ids_from_spi <- function(spi, regions, calc = "maj",
                              range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}
  ##### range_selection and manual are checked in gmba_ids_from_selection()

  ##### set attributes
  inv_ids <- gmba_ids_from_selection(range_selection, manual)

  ##### check arguments
  # spi
  spi <- match.arg(spi, c("ipbes", "ipcc"))
  # regions
  if(spi == "ipbes"){
    regions <- match.arg(regions, c("Africa",
                                    "Central Africa",
                                    "East Africa and adjacent islands",
                                    "North Africa",
                                    "Southern Africa",
                                    "West Africa",
                                    "The Americas",
                                    "Caribbean",
                                    "Mesoamerica",
                                    "North America",
                                    "South America",
                                    "Asia and the Pacific",
                                    "North-East Asia",
                                    "Oceania",
                                    "South Asia",
                                    "South-East Asia",
                                    "Western Asia",
                                    "Europe and Central Asia",
                                    "Central and Western Europe",
                                    "Central Asia",
                                    "Eastern Europe"),
                         several.ok = TRUE)}
  if(spi == "ipcc"){
    regions <- match.arg(regions, c("GIC",
                                    "NWN",
                                    "NEN",
                                    "WNA",
                                    "CNA",
                                    "ENA",
                                    "NCA",
                                    "SCA",
                                    "CAR",
                                    "NWS",
                                    "NSA",
                                    "NES",
                                    "SAM",
                                    "SWS",
                                    "SES",
                                    "SSA",
                                    "NEU",
                                    "WCE",
                                    "EEU",
                                    "MED",
                                    "SAH",
                                    "WAF",
                                    "CAF",
                                    "NEAF",
                                    "SEAF",
                                    "WSAF",
                                    "ESAF",
                                    "MDG",
                                    "RAR",
                                    "WSB",
                                    "ESB",
                                    "RFE",
                                    "WCA",
                                    "ECA",
                                    "TIB",
                                    "EAS",
                                    "ARP",
                                    "SAS",
                                    "SEA",
                                    "NAU",
                                    "CAU",
                                    "EAU",
                                    "SAU",
                                    "NZ",
                                    "ARO",
                                    "NPO",
                                    "EPO",
                                    "SPO",
                                    "NAO",
                                    "EAO",
                                    "SAO",
                                    "ARS",
                                    "BOB",
                                    "EIO",
                                    "SIO",
                                    "SOO"),
                         several.ok = TRUE)}
  if(("EAN" %in% regions) | ("WAN" %in% regions)){
    message("Note: 'EAN' and 'WAN' are not among the IPCC AR6 regions relevant for the GMBA Inventory v2.0.")}
  # calc
  calc <- match.arg(calc, c("maj", "str"))

  ##### run function
  ranges <- NA

  # spi = ipbes
  if(spi == "ipbes"){
    # if IPBES regions are given as input, adjust regions
    # (as the regions are the sum of their subregions)
    if("Africa" %in% regions){
      regions <- c(regions, c("Central Africa",
                              "East Africa and adjacent islands",
                              "North Africa",
                              "Southern Africa",
                              "West Africa"))
      regions <- regions[-which(regions == "Africa")]}
    if("The Americas" %in% regions){
      regions <- c(regions, c("Caribbean",
                              "Mesoamerica",
                              "North America",
                              "South America"))
      regions <- regions[-which(regions == "The Americas")]}
    if("Asia and the Pacific" %in% regions){
      regions <- c(regions, c("North-East Asia",
                              "Oceania",
                              "South Asia",
                              "South-East Asia",
                              "Western Asia"))
      regions <- regions[-which(regions == "Asia and the Pacific")]}
    if("Europe and Central Asia" %in% regions){
      regions <- c(regions, c("Central and Western Europe",
                              "Central Asia",
                              "Eastern Europe"))
      regions <- regions[-which(regions == "Europe and Central Asia")]}
    regions <- unique(regions)
    regions <- tolower(regions)
    c <- which(names(gmba_ipbes_maj_str) == "GMBA_V2_ID")
    # calc = maj
    if(calc == "maj"){
      for(r in 1:length(regions)){
        ipbes_maj <- tolower(gmba_ipbes_maj_str$IPBES_maj)
        r <- which(grepl(regions[r], ipbes_maj))
        rangesloop <- gmba_ipbes_maj_str[r,c]
        ranges <- c(ranges, rangesloop)
      }
    }
    # calc = str
    if(calc == "str"){
      for(r in 1:length(regions)){
        ipbes_str <- gmba_ipbes_maj_str$IPBES_str
        for(i in 1:length(ipbes_str)){
          ipbes_string <- tolower(unlist(strsplit(ipbes_str[i], ", ")))
          for(n in 1:length(ipbes_string)){
            if(grepl(regions[r], ipbes_string[n])){
              rangesloop <- gmba_ipbes_maj_str[i,c]
              ranges <- c(ranges, rangesloop)
            }
          }
        }
      }
    }
    ranges <- unique(ranges)
    ranges <- ranges[-which(is.na(ranges))]
    output <- ranges[which(ranges %in% inv_ids)]
  }

  # IPCC



  ##### return output
  return(output)

}

