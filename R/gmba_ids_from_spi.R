#' Get mountain range IDs from SPI regions
#'
#' Get GMBA Inventory v2.0 mountain range IDs that are within one or more (sub-) regions referenced to
#' in the science-policy-interfaces a) IPBES, the Intergovernmental Science-Policy Platform on
#' Biodiversity and Ecosystem Services, or b) the IPCC, Intergovernmental Panel on Climate Change.
#'
#' @param spi Character string, either \emph{"ipbes"} or \emph{"ipcc}
#' @param regions Character vector, indicating the IPBES or IPCC regions to get mountain range IDs for
#' @param calc Calculation of how mountain ranges are associated with the IPBES or IPCC regions.
#' Options are \emph{"maj"} (default) and \emph{"str"}. See details
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
#' \itemize{
#' \item{IPBES regions are: \n1: Africa \n 2: Central Africa \n 3: East Africa and adjacent islands \n 4: North Africa \n 5: Southern Africa \n 6: West Africa \n7: The Americas \n 8: Caribbean \n 9: Mesoamerica \n 10: North America \n 11: South America \n12: Asia and the Pacific \n 13: North-East Asia \n 14: Oceania \n 15: South Asia \n 16: South-East Asia \n 17: Western Asia \n18: Europe and Central Asia \n 19: Central and Western Europe \n 20: Central Asia \n 21: Eastern Europe}
#' \item{IPCC regions are: \n1: Greenland/Iceland (GIC) \n2: N.W.North-America (NWN) \n3: N.E.North-America (NEN) \n4: W.North-America (WNA) \n5: C.North-America (CNA) \n6: E.North-America (ENA) \n7: N.Central-America (NCA) \n8: S.Central-America (SCA) \n9 or 10: Caribbean (CAR) \n11: N.W.South-America (NWS) \n12: N.South-America (NSA) \n13: N.E.South-America (NES) \n14: South-American-Monsoon (SAM) \n15: S.W.South-America (SWS) \n16: S.E.South-America (SES) \n17: S.South-America (SSA) \n18: N.Europe (NEU) \n19: Western&Central-Asia (WCE) \n20: E.Europe (EEU) \n21 or 22: Mediterranean (MED) \n23: Sahara (SAH) \n24: Western-Africa (WAF) \n25: Central-Africa (CAF) \n26: N.Eastern-Africa (NEAF) \n27: S.Eastern-Africa (SEAF) \n28: W.Southern-Africa (WSAF) \n29: E.Southern-Africa (ESAF) \n30: Madagascar (MDG) \n31: Russian-Arctic (RAR) \n32: W.Siberia (WSB) \n33: E.Siberia (ESB) \n34: Russian-Far-East (RFE) \n35: W.C.Asia (WCA) \n36: E.C.Asia (ECA) \n37: Tibetan-Planteau (TIB) \n38: E.Asia (EAS) \n39: Arabian-Peninsula (ARP) \n40: S.Asia (SAS) \n41 or 42: S.E.Asia (SEA) \n43: N.Australia (NAU) \n44: C.Australia (CAU) \n45: E.Australia (EAU) \n46: S.Australia (SAU) \n47: New-Zealand (NZ) \n48: E.Antarctica (EAN) \n49: W.Antarctica (WAN) \n50: Arctic-Ocean (ARO) \n51: N.Pacific-Ocean (NPO) \n52: Equatorial.Pacific-Ocean (EPO) \n53: S.Pacific-Ocean (SPO) \n54: N.Atlantic-Ocean (NAO) \n55: Equatorial.Atlantic-Ocean (EAO) \n56: S.Atlantic-Ocean (SAO) \n57: Arabian-Sea (ARS) \n58: Bay-of-Bengal (BOB) \n59: Equatorial.Indic-Ocean (EIO) \n60: S.Indic-Ocean (SIO) \n61: Southern-Ocean (SOO)}
#' \item{We attributed corresponding IPBES or IPCC regions to each of the GMBA Inventory v2.0
#' polygons by a geometric union between the GMBA_Inventory_v2.0_broad layer on the one hand and
#' the respective reference region shapefiles IPCC AR6 (\href{https://doi.org/10.5194/essd-12-2959-2020}{doi.org/10.5194/essd-12-2959-2020})
#' and IPBES (\href{https://doi.org/10.5281/zenodo.3928281}{doi.org/10.5281/zenodo.3928281}).
#' Area calculations for all \emph{GMBA_V2_ID} and IPBES or IPCC reference region combinations were
#' performed after reprojecting each of the two union layers to Mollweide. For each GMBA Inventory v2.0 polygon and
#' reference region combination, we selected the one with the largest overlap and attributed that
#' reference region to the respective \emph{GMBA_V2_ID} (\code{calc} = \emph{"maj"}). In addition, we created a
#' string with all the reference regions each \emph{GMBA_V2_ID} intersected with (\code{calc} = \emph{"str"}).
#' The default is set to \emph{"maj"}}}
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
#' rangeids <- gmba_ids_from_ipcc("ipbes", "eca")
#' }

gmba_ids_from_spi <- function(spi, regions, calc = "maj",
                              range_selection = "all", manual = NULL){

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")
  }

  ##### set attributes
  inv_ids <- gmba_ids_from_selection(range_selection, manual)
  ipbes <- read.xlsx("https://raw.github.com/GMBA-biodiversity/gmbaR/main/man/ipbes_ipcc/GMBA_IPBES_maj_str.xlsx", sheet = 1)
  ipcc <- read.xlsx("https://raw.github.com/GMBA-biodiversity/gmbaR/main/man/ipbes_ipcc/GMBA_IPCC_AR6_maj_str.xlsx", sheet = 1)

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
    # add regions to subregions
    if("Africa" %in% regions){
      regions <- c(regions, c("Central Africa",
                              "East Africa and adjacent islands",
                              "North Africa",
                              "Southern Africa",
                              "West Africa"))}
    if("The Americas" %in% regions){
      regions <- c(regions, c("Caribbean",
                              "Mesoamerica",
                              "North America",
                              "South America"))}


    regions <- unique(regions)
    regions <- tolower(regions)




    c <- which(names(ipbes) == "GMBA_V2_ID")
    # calc = maj
    if(calc == "maj"){
      for(r in 1:length(regions)){
        ipbes_maj <- tolower(ipbes$IPBES_maj)
        r <- which(grepl(regions[r], ipbes_maj))
        rangesloop <- ipbes[r,c]
        ranges <- c(ranges, rangesloop)
      }
    }
    # calc = str
    if(calc == "str"){
      for(r in 1:length(regions)){
        ipbes_str <- ipbes$IPBES_str
        for(i in 1:length(ipbes_str)){
          ipbes_string <- tolower(unlist(strsplit(ipbes_str[i], ", ")))
          for(n in 1:length(ipbes_string)){
            if(grepl(regions[r], ipbes_string[n])){
              rangesloop <- ipbes[i,c]
              ranges <- c(ranges, rangesloop)
            }
          }
        }
      }
    }
  }

  # IPCC

  ##### return output
  ranges <- unique(ranges)
  if("NA" %in% ranges){ranges <- ranges[-which(ranges == "NA")]}
  ranges <- ranges[-which(is.na(ranges))]
  return(ranges)

}

