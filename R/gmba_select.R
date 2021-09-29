#' Select mountain ranges from the GMBA Inventory v2.0
#'
#' Interactively filter and select mountain ranges from the GMBA Inventory v2.0.
#' This function can be considered as the \href{url}{Selection Tool} implemented in R.
#'
#' @details {First, you are given four options to filter the inventory. After filtering, you can select
#' all or individual ranges from the filter. A 'stop' as response to each question stops
#' the function from running.\cr\cr
#' The filter options are:
#' \enumerate{
#' \item{By mountain range name}
#' \item{By country}
#' \item{By IPBES region or subregion, based on \href{https://doi.org/10.5281/zenodo.3928281}{doi:10.5281/zenodo.3928281}}
#' \item{By IPCC AR6 region, based on \href{https://doi.org/10.5194/essd-12-2959-2020}{doi:10.5194/essd-12-2959-2020}}}
#' Then, the selection options are:
#' \enumerate{
#' \item{All of the filtered mountain ranges}
#' \item{By number(s), as displayed after the last filter}}
#' Finally, the output options are:
#' \enumerate{
#' \item{An \code{sf} object containing the selected mountain ranges}
#' \item{A character vector with the names of the selected mountain ranges}
#' \item{A character vector with the GMBA v2.0 IDs of the selected mountain ranges}}
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
  inv_countries <- tolower(attributetable()$Countries)

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
      name_input <- tolower(name_input)
      r <- which(grepl(name_input, inv_names))
      c <- which(names(attributetable()) == "GMBA_V2_ID")
      ranges <- attributetable()[r,c]
      rangeselection <- rangeselection[which(rangeselection %in% ranges)]
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
      ranges <- NA
      for(iso in 1:length(country_input)){
        r <- which(grepl(country_input[iso], inv_countries))
        c <- which(names(attributetable()) == "GMBA_V2_ID")
        rangesloop <- attributetable()[r,c]
        ranges <- c(ranges, rangesloop)
      }
      ranges <- unique(ranges)
      ranges <- ranges[-which(is.na(ranges))]
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

    # filter by IPBES (sub) regions
    if(filteroption == 3){
      # based on doi.org/10.5281/zenodo.3928281
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
      # filter
      country_input <- NA
      ipbes_countries_1 <- c("ago","bdi","ben","bfa","bwa","caf","civ","cmr","cod","cog","com","cpv","dji","dza","egy","eri","esh","eth","gab","gha","gin","gmb","gnb","gnq","ken","lbr","lby","lso","mar","mdg","mli","moz","mrt","mus","mwi","myt","nam","ner","nga","reu","rwa","sdn","sen","shn","sle","som","ssd","stp","swz","syc","tcd","tgo","tun","tza","uga","zaf","zmb","zwe")
      ipbes_countries_2 <- c("bdi","caf","cmr","cod","cog","gab","gnq","stp","tcd")
      ipbes_countries_3 <- c("com","dji","eri","eth","ken","mdg","mus","myt","reu","rwa","som","ssd","syc","tza","uga")
      ipbes_countries_4 <- c("dza","egy","esh","lby","mar","mrt","sdn","tun")
      ipbes_countries_5 <- c("ago","bwa","lso","moz","mwi","nam","shn","swz","zaf","zmb","zwe")
      ipbes_countries_6 <- c("ben","bfa","civ","cpv","gha","gin","gmb","gnb","lbr","mli","ner","nga","sen","sle","tgo")
      ipbes_countries_7 <- c("abw","aia","arg","atg","bes","bhs","blm","blz","bmu","bol","bra","brb","bvt","can","chl","col","cri","cub","cuw","cym","dma","dom","ecu","flk","glp","grd","grl","gtm","guf","guy","hnd","hti","jam","kna","lca","maf","mex","msr","mtq","nic","pan","per","pri","pry","sgs","slv","spm","sur","sxm","tca","tto","NA","ury","usa","vct","ven","vgb","vir")
      ipbes_countries_8 <- c("abw","aia","atg","bes","bhs","blm","bmu","brb","cub","cuw","cym","dma","dom","glp","grd","hti","jam","kna","lca","maf","msr","mtq","pri","sxm","tca","tto","NA","vct","vgb","vir")
      ipbes_countries_9 <- c("blz","cri","gtm","hnd","mex","nic","pan","slv")
      ipbes_countries_10 <- c("can","grl","spm","usa")
      ipbes_countries_11 <- c("arg","bol","bra","bvt","chl","col","ecu","flk","guf","guy","per","pry","sgs","sur","ury","ven")
      ipbes_countries_12 <- c("afg","are","asm","atf","aus","bgd","bhr","brn","btn","cck","chn","cok","cxr","fji","fsm","gum","hkg","hmd","idn","ind","iot","irn","irq","jor","jpn","khm","kir","kor","kwt","lao","lbn","lka","mac","mdv","mhl","mmr","mng","mnp","mys","ncl","nfk","niu","npl","nru","nzl","omn","pak","pcn","phl","plw","png","prk","pse","pyf","qat","sau","sgp","slb","syr","tha","tkl","tls","ton","tuv","twn","umi","vnm","vut","wlf","wsm","NA","NA","NA","yem","NA")
      ipbes_countries_13 <- c("chn","hkg","jpn","kor","mac","mng","prk","twn")
      ipbes_countries_14 <- c("asm","atf","aus","cok","fji","fsm","gum","hmd","kir","mhl","mnp","ncl","nfk","niu","nru","nzl","pcn","plw","png","pyf","slb","tkl","ton","tuv","umi","vut","wlf","wsm","NA","NA")
      ipbes_countries_15 <- c("afg","bgd","btn","ind","iot","irn","lka","mdv","npl","pak")
      ipbes_countries_16 <- c("brn","cck","cxr","idn","khm","lao","mmr","mys","phl","sgp","tha","tls","vnm","NA","NA")
      ipbes_countries_17 <- c("are","bhr","irq","jor","kwt","lbn","omn","pse","qat","sau","syr","yem")
      ipbes_countries_18 <- c("ala","alb","and","arm","aut","aze","bel","bgr","bih","blr","che","cyp","cze","deu","dnk","esp","est","fin","fra","fro","gbr","geo","ggy","gib","grc","hrv","hun","imn","irl","isl","isr","ita","jey","kaz","kgz","lie","ltu","lux","lva","mco","mda","mkd","mlt","mne","nld","nor","pol","prt","rou","rus","sjm","smr","srb","svk","svn","swe","tjk","tkm","tur","ukr","uzb","vat","NA","NA","NA")
      ipbes_countries_19 <- c("ala","alb","and","aut","bel","bgr","bih","che","cyp","cze","deu","dnk","esp","est","fin","fra","fro","gbr","ggy","gib","grc","hrv","hun","imn","irl","isl","isr","ita","jey","lie","ltu","lux","lva","mco","mkd","mlt","mne","nld","nor","pol","prt","rou","sjm","smr","srb","svk","svn","swe","tur","vat","NA","NA","NA")
      ipbes_countries_20 <- c("kaz","kgz","tjk","tkm","uzb")
      ipbes_countries_21 <- c("arm","aze","blr","geo","mda","rus","ukr")
      for(r in ipbes_input){
        if(r == 1){country_input <- c(country_input, ipbes_countries_1)}
        if(r == 2){country_input <- c(country_input, ipbes_countries_2)}
        if(r == 3){country_input <- c(country_input, ipbes_countries_3)}
        if(r == 4){country_input <- c(country_input, ipbes_countries_4)}
        if(r == 5){country_input <- c(country_input, ipbes_countries_5)}
        if(r == 6){country_input <- c(country_input, ipbes_countries_6)}
        if(r == 7){country_input <- c(country_input, ipbes_countries_7)}
        if(r == 8){country_input <- c(country_input, ipbes_countries_8)}
        if(r == 9){country_input <- c(country_input, ipbes_countries_9)}
        if(r == 10){country_input <- c(country_input, ipbes_countries_10)}
        if(r == 11){country_input <- c(country_input, ipbes_countries_11)}
        if(r == 12){country_input <- c(country_input, ipbes_countries_12)}
        if(r == 13){country_input <- c(country_input, ipbes_countries_13)}
        if(r == 14){country_input <- c(country_input, ipbes_countries_14)}
        if(r == 15){country_input <- c(country_input, ipbes_countries_15)}
        if(r == 16){country_input <- c(country_input, ipbes_countries_16)}
        if(r == 17){country_input <- c(country_input, ipbes_countries_17)}
        if(r == 18){country_input <- c(country_input, ipbes_countries_18)}
        if(r == 19){country_input <- c(country_input, ipbes_countries_19)}
        if(r == 20){country_input <- c(country_input, ipbes_countries_20)}
        if(r == 21){country_input <- c(country_input, ipbes_countries_21)}
      }
      country_input <- unique(country_input)
      if("NA" %in% country_input){country_input <- country_input[-which(country_input == "NA")]}
      country_input <- country_input[-which(is.na(country_input))]
      ranges <- NA
      for(iso in 1:length(country_input)){
        r <- which(grepl(country_input[iso], inv_countries))
        c <- which(names(attributetable()) == "GMBA_V2_ID")
        rangesloop <- attributetable()[r,c]
        ranges <- c(ranges, rangesloop)
      }
      ranges <- unique(ranges)
      ranges <- ranges[-which(is.na(ranges))]
      rangeselection <- rangeselection[which(rangeselection %in% ranges)]
      r <- which(inv_ids %in% rangeselection)
      c <- which(names(attributetable()) == "DBaseName")
      print(attributetable()[r,c])
    }

    # filter by IPCC regions
    if(filteroption == 4){
      # readline

      warning("The IPCC filter option is not implemented yet.",
              immediate. = TRUE)
      #
      # # based on doi.org/10.5194/essd-12-2959-2020
      # cat(paste("Enter one or more IPCC regions, separated by comma: \n1: Greenland/Iceland (GIC) \n2: N.W.North-America (NWN) \n3: N.E.North-America (NEN) \n4: W.North-America (WNA) \n5: C.North-America (CNA) \n6: E.North-America (ENA) \n7: N.Central-America (NCA) \n8: S.Central-America (SCA) \n9 or 10: Caribbean (CAR) \n11: N.W.South-America (NWS) \n12: N.South-America (NSA) \n13: N.E.South-America (NES) \n14: South-American-Monsoon (SAM) \n15: S.W.South-America (SWS) \n16: S.E.South-America (SES) \n17: S.South-America (SSA) \n18: N.Europe (NEU) \n19: Western&Central-Asia (WCE) \n20: E.Europe (EEU) \n21 or 22: Mediterranean (MED) \n23: Sahara (SAH) \n24: Western-Africa (WAF) \n25: Central-Africa (CAF) \n26: N.Eastern-Africa (NEAF) \n27: S.Eastern-Africa (SEAF) \n28: W.Southern-Africa (WSAF) \n29: E.Southern-Africa (ESAF) \n30: Madagascar (MDG) \n31: Russian-Arctic (RAR) \n32: W.Siberia (WSB) \n33: E.Siberia (ESB) \n34: Russian-Far-East (RFE) \n35: W.C.Asia (WCA) \n36: E.C.Asia (ECA) \n37: Tibetan-Planteau (TIB) \n38: E.Asia (EAS) \n39: Arabian-Peninsula (ARP) \n40: S.Asia (SAS) \n41 or 42: S.E.Asia (SEA) \n43: N.Australia (NAU) \n44: C.Australia (CAU) \n45: E.Australia (EAU) \n46: S.Australia (SAU) \n47: New-Zealand (NZ)", sep=""))
      # ipcc_input <- suppressWarnings(readline(prompt = paste("Input: ")))
      # if(ipcc_input == "stop"){stop("manual stop")}
      # ipcc_input <- gsub(" ", "", ipcc_input, fixed = TRUE)
      # ipcc_input <- as.numeric(unlist(strsplit(ipcc_input, ",")))
      # if(FALSE %in% (ipcc_input %in% c(1:47))){repeat{
      #   ipcc_input <- suppressWarnings(readline(prompt = paste("The input must be within 1 to 47, separated by a comma. Input: ", sep="")))
      #   if(ipcc_input == "stop"){stop("manual stop")}
      #   ipcc_input <- as.numeric(unlist(strsplit(ipcc_input, ",")))
      #   breaktest <- unique(ipcc_input %in% c(1:47))
      #   if(!(FALSE %in% breaktest)){break}}} # readline test
      # country_input <- NA
      # ipcc_countries_1 <- c()
      # ipcc_countries_2 <- c()
      # ipcc_countries_3 <- c()
      # ipcc_countries_4 <- c()
      # ipcc_countries_5 <- c()
      # ipcc_countries_6 <- c()
      # ipcc_countries_7 <- c()
      # ipcc_countries_8 <- c()
      # ipcc_countries_9_10 <- c()
      # ipcc_countries_11 <- c()
      # ipcc_countries_12 <- c()
      # ipcc_countries_13 <- c()
      # ipcc_countries_14 <- c()
      # ipcc_countries_15 <- c()
      # ipcc_countries_16 <- c()
      # ipcc_countries_17 <- c()
      # ipcc_countries_18 <- c()
      # ipcc_countries_19 <- c()
      # ipcc_countries_20 <- c()
      # ipcc_countries_21_22 <- c()
      # ipcc_countries_23 <- c()
      # ipcc_countries_24 <- c()
      # ipcc_countries_25 <- c()
      # ipcc_countries_26 <- c()
      # ipcc_countries_27 <- c()
      # ipcc_countries_28 <- c()
      # ipcc_countries_29 <- c()
      # ipcc_countries_30 <- c()
      # ipcc_countries_31 <- c()
      # ipcc_countries_32 <- c()
      # ipcc_countries_33 <- c()
      # ipcc_countries_34 <- c()
      # ipcc_countries_35 <- c()
      # ipcc_countries_36 <- c()
      # ipcc_countries_37 <- c()
      # ipcc_countries_38 <- c()
      # ipcc_countries_39 <- c()
      # ipcc_countries_40 <- c()
      # ipcc_countries_41_42 <- c()
      # ipcc_countries_43 <- c()
      # ipcc_countries_44 <- c()
      # ipcc_countries_45 <- c()
      # ipcc_countries_46 <- c()
      # ipcc_countries_47 <- c()
      # for(r in ipcc_input){
      #   if(r == 1){country_input <- c(country_input, ipcc_countries_1)}
      #   if(r == 2){country_input <- c(country_input, ipcc_countries_2)}
      #   if(r == 3){country_input <- c(country_input, ipcc_countries_3)}
      #   if(r == 4){country_input <- c(country_input, ipcc_countries_4)}
      #   if(r == 5){country_input <- c(country_input, ipcc_countries_5)}
      #   if(r == 6){country_input <- c(country_input, ipcc_countries_6)}
      #   if(r == 7){country_input <- c(country_input, ipcc_countries_7)}
      #   if(r == 8){country_input <- c(country_input, ipcc_countries_8)}
      #   if(r %in% c(9,10)){country_input <- c(country_input, ipcc_countries_9_10)}
      #   if(r == 11){country_input <- c(country_input, ipcc_countries_11)}
      #   if(r == 12){country_input <- c(country_input, ipcc_countries_12)}
      #   if(r == 13){country_input <- c(country_input, ipcc_countries_13)}
      #   if(r == 14){country_input <- c(country_input, ipcc_countries_14)}
      #   if(r == 15){country_input <- c(country_input, ipcc_countries_15)}
      #   if(r == 16){country_input <- c(country_input, ipcc_countries_16)}
      #   if(r == 17){country_input <- c(country_input, ipcc_countries_17)}
      #   if(r == 18){country_input <- c(country_input, ipcc_countries_18)}
      #   if(r == 19){country_input <- c(country_input, ipcc_countries_19)}
      #   if(r == 20){country_input <- c(country_input, ipcc_countries_20)}
      #   if(r %in% c(21,22)){country_input <- c(country_input, ipcc_countries_21_22)}
      #   if(r == 23){country_input <- c(country_input, ipcc_countries_23)}
      #   if(r == 24){country_input <- c(country_input, ipcc_countries_24)}
      #   if(r == 25){country_input <- c(country_input, ipcc_countries_25)}
      #   if(r == 26){country_input <- c(country_input, ipcc_countries_26)}
      #   if(r == 27){country_input <- c(country_input, ipcc_countries_27)}
      #   if(r == 28){country_input <- c(country_input, ipcc_countries_28)}
      #   if(r == 29){country_input <- c(country_input, ipcc_countries_29)}
      #   if(r == 30){country_input <- c(country_input, ipcc_countries_30)}
      #   if(r == 31){country_input <- c(country_input, ipcc_countries_31)}
      #   if(r == 32){country_input <- c(country_input, ipcc_countries_32)}
      #   if(r == 33){country_input <- c(country_input, ipcc_countries_33)}
      #   if(r == 34){country_input <- c(country_input, ipcc_countries_34)}
      #   if(r == 35){country_input <- c(country_input, ipcc_countries_35)}
      #   if(r == 36){country_input <- c(country_input, ipcc_countries_36)}
      #   if(r == 37){country_input <- c(country_input, ipcc_countries_37)}
      #   if(r == 38){country_input <- c(country_input, ipcc_countries_38)}
      #   if(r == 39){country_input <- c(country_input, ipcc_countries_39)}
      #   if(r == 40){country_input <- c(country_input, ipcc_countries_40)}
      #   if(r %in% c(41,42)){country_input <- c(country_input, ipcc_countries_41_42)}
      #   if(r == 43){country_input <- c(country_input, ipcc_countries_43)}
      #   if(r == 44){country_input <- c(country_input, ipcc_countries_44)}
      #   if(r == 45){country_input <- c(country_input, ipcc_countries_45)}
      #   if(r == 46){country_input <- c(country_input, ipcc_countries_46)}
      #   if(r == 47){country_input <- c(country_input, ipcc_countries_47)}
      #   }
      # country_input <- unique(country_input)
      # if("NA" %in% country_input){country_input <- country_input[-which(country_input == "NA")]}
      # country_input <- country_input[-which(is.na(country_input))]
      # ranges <- NA
      # for(iso in 1:length(country_input)){
      #   rangesloop <- attributetable()[which(grepl(country_input[iso], inv_countries)), which(names(attributetable()) == "GMBA_V2_ID")]
      #   ranges <- c(ranges, rangesloop)
      # }
      # ranges <- unique(ranges)
      # ranges <- ranges[-which(is.na(ranges))]
      # rangeselection <- rangeselection[which(rangeselection %in% ranges)]
      # print(attributetable()[which(inv_ids %in% rangeselection), which(names(attributetable()) == "DBaseName")])

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
    cat(paste("Note: All above displayed mountain ranges selected.\n", sep=""))
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
  cat(paste("+++ Output format +++ \n1: mountain range polygons \n2: mountain range map names \n3: GMBA Inventory v2 IDs", sep=""))
  formatoption <- suppressWarnings(readline(prompt = paste("Input: ")))
  if(formatoption == "stop"){stop("manual stop")}
  formatoption <- as.numeric(formatoption)
  if(!(formatoption %in% c(1:3))){repeat{
    formatoption <- suppressWarnings(readline(prompt = paste("The input must be one from 1 to 3. Input: ")))
    if(formatoption == "stop"){stop("manual stop")}
    formatoption <- as.numeric(formatoption)
    if(formatoption %in% c(1:3)){break}}} # readline test

  # mountain range polygons
  if(formatoption == 1){
    r < which(inv_ids %in% rangeselection)
    output <- gmba_inv()[r,]
  }

  # mountain range map names
  if(formatoption == 2){
    r <- which(inv_ids %in% rangeselection)
    c <- which(names(attributetable()) == "MapName")
    output <- attributetable()[r,c]
  }

  # GMBA Inventory v2 IDs
  if(formatoption == 3){
    output <- rangeselection
  }

  ##### return output
  return(output)

  }
