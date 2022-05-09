#' Interactively select mountain ranges from the GMBA Inventory v2.0
#'
#' Interactively filter and select mountain ranges from the GMBA Inventory v2.0.
#' This function can be considered as the \href{https://github.com/GMBA-biodiversity/Inventory/blob/main/Gmba_Inventory_v2.0_Selection_Tool_20200330.xlsx}{Selection Tool} implemented in R.
#'
#' @details {First, you are given four options to filter the inventory. After filtering, you can select
#' all or individual ranges from the filter. A 'stop' as response to each question stops
#' the function from running.\cr\cr
#' The filter options are:
#' \enumerate{
#' \item{By mountain range name (English only, for searching mountain range names in other
#' languages please use \code{gmba_search_names( )})}
#' \item{By country, using ISO-3 codes}
#' \item{By IPBES region or subregion, using the \emph{"str"} method from \code{gmba_ids_from_spi( )}}
#' \item{By IPCC AR6 region, using the \emph{"str"} method from \code{gmba_ids_from_spi( )}}}
#' Then, the selection options are:
#' \enumerate{
#' \item{All of the filtered mountain ranges}
#' \item{By number(s), as displayed after the last filter}}
#' Finally, the output options are:
#' \enumerate{
#' \item{An \code{sf} object containing the selected mountain ranges}
#' \item{A character vector with the GMBA Inventory v2.0 database names of the selected mountain ranges}
#' \item{A character vector with the GMBA Inventory v2.0 map names of the selected mountain ranges}
#' \item{A character vector with the GMBA Inventory v2.0 IDs of the selected mountain ranges}}
#' }
#'
#' @return Depending on the output format selection, see details
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  gmba_select()
#' }

gmba_select <- function() { # IPCC + OVERLAP WARNING TO ADD

  ##### check if the inventory is read
  if(exists("gmba_inv") == FALSE){
    stop("The GMBA Inventory v2.0 is not read to R. Use gmba_read() to create gmba_inv")}

  ##### set attributes
  inv_ids <- attributetable()$GMBA_V2_ID
  inv_names <- tolower(attributetable()$DBaseName)
  inv_countries <- tolower(attributetable()$CountryISO)

  ##### run function
  # raw range selection
  rangeselection <- inv_ids

  # initial selection tool information
  cat(paste("First, you are given four options to filter the GMBA Inventory v2.0. After filtering, you can select all or individual ranges from the filter. A 'stop' as response to each question stops the function from running.\n\n", sep=""))

  ### filter loop
  repeat{
    cat(paste("+++ Filter options +++ \n1: by mountain range name \n2: by country \n3: by IPBES (sub)region \n4: by IPCC region", sep=""))
    filteroption <- suppressWarnings(readline(prompt = paste("Input: ")))
    if(filteroption == "stop"){stop("manual stop")}
    filteroption <- as.numeric(filteroption)
    if(!(filteroption %in% c(1:4))){repeat{
      filteroption <- suppressWarnings(readline(prompt = paste("The input must be one from 1 to 4. Input: ")))
      if(filteroption == "stop"){stop("manual stop")}
      filteroption <- as.numeric(filteroption)
      if(filteroption %in% c(1:4)){break}}} # readline test

    # filter by mountain range name
    if(filteroption == 1){
      # readline
      name_input <- suppressWarnings(readline(prompt = paste("Enter (part of) a mountain range name: ")))
      if(name_input == "stop"){stop("manual stop")}
      # filter
      ranges <- gmba_search_names(name_input)
      rangeselection <- rangeselection[which(rangeselection %in% gmba_ids_from_names(ranges))]
      r <- which(inv_ids %in% rangeselection)
      c <- which(names(attributetable()) == "DBaseName")
      print(attributetable()[r,c])
    }

    # filter by country
    if(filteroption == 2){
      # readline
      country_input <- suppressWarnings(readline(prompt = paste("Enter one or more countries per ISO-3 code, separated by a comma: ")))
      if(country_input == "stop"){stop("manual stop")}
      country_input <- gsub(" ", "", country_input, fixed = TRUE)
      country_input <- tolower(unlist(strsplit(country_input, ",")))
      if(sum(unique(nchar(country_input))) != 3){repeat{
        country_input <- suppressWarnings(readline(prompt = paste("The input must be one or more ISO-3 code(s), all with 3 characters, separated by a comma. Input: ", sep="")))
        if(country_input == "stop"){stop("manual stop")}
        country_input <- gsub(" ", "", country_input, fixed = TRUE)
        country_input <- tolower(unlist(strsplit(country_input, ",")))
        if(sum(unique(nchar(country_input))) == 3){break}}} # readline test
      # filter
      ranges <- gmba_ids_from_countries(country_input)
      if(length(ranges) == 0){
        warning("For the selected countries, no mountain polygons were found.",
                immediate. = TRUE)
      } else {
        rangeselection <- rangeselection[which(rangeselection %in% ranges)]
        r <- which(inv_ids %in% rangeselection)
        c <- which(names(attributetable()) == "DBaseName")
        print(attributetable()[r,c])
      }
    }

    # filter by IPBES (sub)regions
    if(filteroption == 3){
      # readline
      cat(paste("Enter one or more IPBES (sub) regions, separated by comma: \n1: Africa \n 2: Central Africa \n 3: East Africa and adjacent islands \n 4: North Africa \n 5: Southern Africa \n 6: West Africa \n7: The Americas \n 8: Caribbean \n 9: Mesoamerica \n 10: North America \n 11: South America \n12: Asia and the Pacific \n 13: North-East Asia \n 14: Oceania \n 15: South Asia \n 16: South-East Asia \n 17: Western Asia \n18: Europe and Central Asia \n 19: Central and Western Europe \n 20: Central Asia \n 21: Eastern Europe", sep=""))
      ipbes_input <- suppressWarnings(readline(prompt = paste("Input: ")))
      if(ipbes_input == "stop"){stop("manual stop")}
      ipbes_input <- gsub(" ", "", ipbes_input, fixed = TRUE)
      ipbes_input <- as.numeric(unlist(strsplit(ipbes_input, ",")))
      if(FALSE %in% (ipbes_input %in% c(1:21))){repeat{
        ipbes_input <- suppressWarnings(readline(prompt = paste("The input must be within 1 to 21, separated by a comma. Input: ", sep="")))
        if(ipbes_input == "stop"){stop("manual stop")}
        ipbes_input <- as.numeric(unlist(strsplit(ipbes_input, ",")))
        breaktest <- unique(ipbes_input %in% c(1:21))
        if(!(FALSE %in% breaktest)){break}}} # readline test
      ipbes_input <- gmba_match_ipbes(ipbes_input)
      # filter
      ranges <- gmba_ids_from_spi("ipbes", ipbes_input, calc = "str")
      rangeselection <- rangeselection[which(rangeselection %in% ranges)]
      r <- which(inv_ids %in% rangeselection)
      c <- which(names(attributetable()) == "DBaseName")
      print(attributetable()[r,c])
    }

    # filter by IPCC regions
    if(filteroption == 4){
      # readline
      cat(paste("Enter one or more IPCC regions, separated by comma: \n1: Greenland/Iceland (GIC) \n2: N.W.North-America (NWN) \n3: N.E.North-America (NEN) \n4: W.North-America (WNA) \n5: C.North-America (CNA) \n6: E.North-America (ENA) \n7: N.Central-America (NCA) \n8: S.Central-America (SCA) \n9 or 10: Caribbean (CAR) \n11: N.W.South-America (NWS) \n12: N.South-America (NSA) \n13: N.E.South-America (NES) \n14: South-American-Monsoon (SAM) \n15: S.W.South-America (SWS) \n16: S.E.South-America (SES) \n17: S.South-America (SSA) \n18: N.Europe (NEU) \n19: Western&Central-Asia (WCE) \n20: E.Europe (EEU) \n21 or 22: Mediterranean (MED) \n23: Sahara (SAH) \n24: Western-Africa (WAF) \n25: Central-Africa (CAF) \n26: N.Eastern-Africa (NEAF) \n27: S.Eastern-Africa (SEAF) \n28: W.Southern-Africa (WSAF) \n29: E.Southern-Africa (ESAF) \n30: Madagascar (MDG) \n31: Russian-Arctic (RAR) \n32: W.Siberia (WSB) \n33: E.Siberia (ESB) \n34: Russian-Far-East (RFE) \n35: W.C.Asia (WCA) \n36: E.C.Asia (ECA) \n37: Tibetan-Planteau (TIB) \n38: E.Asia (EAS) \n39: Arabian-Peninsula (ARP) \n40: S.Asia (SAS) \n41 or 42: S.E.Asia (SEA) \n43: N.Australia (NAU) \n44: C.Australia (CAU) \n45: E.Australia (EAU) \n46: S.Australia (SAU) \n47: New-Zealand (NZ) \n48: E.Antarctica (EAN) \n49: W.Antarctica (WAN) \n50: Arctic-Ocean (ARO) \n51: N.Pacific-Ocean (NPO) \n52: Equatorial.Pacific-Ocean (EPO) \n53: S.Pacific-Ocean (SPO) \n54: N.Atlantic-Ocean (NAO) \n55: Equatorial.Atlantic-Ocean (EAO) \n56: S.Atlantic-Ocean (SAO) \n57: Arabian-Sea (ARS) \n58: Bay-of-Bengal (BOB) \n59: Equatorial.Indic-Ocean (EIO) \n60: S.Indic-Ocean (SIO) \n61: Southern-Ocean (SOO)", sep=""))
      ipcc_input <- suppressWarnings(readline(prompt = paste("Input: ")))
      if(ipcc_input == "stop"){stop("manual stop")}
      ipcc_input <- gsub(" ", "", ipcc_input, fixed = TRUE)
      ipcc_input <- as.numeric(unlist(strsplit(ipcc_input, ",")))
      if(FALSE %in% (ipcc_input %in% c(1:47))){repeat{
        ipcc_input <- suppressWarnings(readline(prompt = paste("The input must be within 1 to 61, separated by a comma. Input: ", sep="")))
        if(ipcc_input == "stop"){stop("manual stop")}
        ipcc_input <- as.numeric(unlist(strsplit(ipcc_input, ",")))
        breaktest <- unique(ipcc_input %in% c(1:47))
        if(!(FALSE %in% breaktest)){break}}} # readline test
      ipcc_input <- gmba_match_ipcc(ipcc_input)
      # run filter
      ranges <- gmba_ids_from_spi("ipcc", ipcc_input, calc = "str")
      rangeselection <- rangeselection[which(rangeselection %in% ranges)]
      r <- which(inv_ids %in% rangeselection)
      c <- which(names(attributetable()) == "DBaseName")
      print(attributetable()[r,c])
    }

    # return to loop question
    continue <- suppressWarnings(readline(prompt = paste("Continue filtering? Yes or no: ")))
    if(continue == "stop"){stop("manual stop")}
    if(!(continue %in% c("Yes", "no"))){repeat{
      continue <- suppressWarnings(readline(prompt = paste("The input must be one of 'Yes' or 'no'. Input: ")))
      if(continue == "stop"){stop("manual stop")}
      if(continue %in% c("Yes", "no")){break}}} # readline test
    if(continue == "no"){break}
  }

  ### select ranges
  cat(paste("+++ Select options +++ \n1: all \n2: by number(s)", sep=""))
  selectoption <- suppressWarnings(readline(prompt = paste("Input: ")))
  if(selectoption == "stop"){stop("manual stop")}
  selectoption <- as.numeric(selectoption)
  if(!(selectoption %in% c(1:2))){repeat{
    selectoption <- suppressWarnings(readline(prompt = paste("The input must be one from 1 to 4. Input: ")))
    if(selectoption == "stop"){stop("manual stop")}
    selectoption <- as.numeric(selectoption)
    if(selectoption %in% c(1:2)){break}}} # readline test

  # select all polygons from the filter
  if(selectoption == 1){
    # select
    message("Note: All above displayed mountain ranges selected.")
  }

  # select individual polygons from the filter
  if(selectoption == 2){
    # readline
    numbers_input <- suppressWarnings(readline(prompt = paste("Enter the number(s) of the range(s) to select, separated by a comma: ")))
    if(selectoption == "stop"){stop("manual stop")}
    numbers_input <- gsub(" ", "", numbers_input, fixed = TRUE)
    numbers_input <- as.numeric(unlist(strsplit(numbers_input, ",")))
    if(FALSE %in% (numbers_input %in% c(1:length(rangeselection)))){repeat{
      numbers_input <- suppressWarnings(readline(prompt = paste("The input must be within 1 to ", length(ranges),", separated by a comma. Input: ", sep="")))
      if(selectoption == "stop"){stop("manual stop")}
      numbers_input <- as.numeric(unlist(strsplit(numbers_input, ",")))
      breaktest <- unique(numbers_input %in% c(1:length(rangeselection)))
      if(!(FALSE %in% breaktest)){break}}} # readline test
    # select
    rangeselection <- rangeselection[numbers_input]
    cat(paste("Selected mountain ranges:\n", sep=""))
    r <- which(inv_ids %in% rangeselection)
    c <- which(names(attributetable()) == "DBaseName")
    print(attributetable()[r,c])

  }

  ### output format
  # readline
  cat(paste("+++ Output format +++ \n1: mountain range polygons \n2: mountain range database names \n3: mountain range map names \n4: GMBA Inventory v2.0 IDs", sep=""))
  formatoption <- suppressWarnings(readline(prompt = paste("Input: ")))
  if(formatoption == "stop"){stop("manual stop")}
  formatoption <- as.numeric(formatoption)
  if(!(formatoption %in% c(1:4))){repeat{
    formatoption <- suppressWarnings(readline(prompt = paste("The input must be one from 1 to 4. Input: ")))
    if(formatoption == "stop"){stop("manual stop")}
    formatoption <- as.numeric(formatoption)
    if(formatoption %in% c(1:4)){break}}} # readline test

  # mountain range polygons
  if(formatoption == 1){
    r < which(inv_ids %in% rangeselection)
    output <- gmba_inv()[r,]
  }

  # mountain range database names
  if(formatoption == 2){
    r <- which(inv_ids %in% rangeselection)
    c <- which(names(attributetable()) == "DBaseName")
    output <- attributetable()[r,c]
  }

  # mountain range map names
  if(formatoption == 3){
    r <- which(inv_ids %in% rangeselection)
    c <- which(names(attributetable()) == "MapName")
    output <- attributetable()[r,c]
  }

  # GMBA Inventory IDs
  if(formatoption == 4){
    output <- rangeselection
  }

  ##### return output
  return(output)

  }
