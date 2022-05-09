#' Filter the GMBA KBA/PA master file
#'
#' Filter the results from the GMBA results on PA coverage of mountain KBAs. Currently for internal use only.
#'
#' @param fileobject Name of the master file object in the R studio environment, without quotes
#' @param variable Character string of variable field/column from the master file, all except "ResultValue"
#' @param option Character string of the option to filter
#'
#' @return Dataframe filtered from given master file object
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  waffledata <- gmba_kbapa("Calculation", "rounded")
#' }

gmba_kbapa <- function(fileobject, variable, option){

  ###### rename master file object
  if(exists("fileobject") == FALSE){
    stop("Please provide the name of the master file object, without quotes")
  }
  masterfile <- fileobject

  ###### check arguments
  variable <- match.arg(variable, c("ID", "ID_underscore", "Year", "UnitOfAnalysis", "Landscape",
                                    "Country", "Mountain", "Name", "Metric", "Definition", "Calculation",
                                    "Unit", "FilterString"))
  if(variable == "UnitOfAnalysis"){
    option <- match.arg(option, c("Country", "CountrySystem", "System", "Range"))
  }
  if(variable == "Landscape"){
    option <- match.arg(option, c("Highland", "Lowland"))
  }
  if(variable == "Definition"){
    option <- match.arg(option, c("WCMC", "GMBA"))
  }
  if(variable == "Calculation"){
    option <- match.arg(option, c("Site", "Area", "rounded"))
  }

  ##### run function
  if(variable == "ID"){output <- masterfile[which(masterfile$ID == option),]}
  if(variable == "ID_underscore"){output <- masterfile[which(masterfile$ID_underscore == option),]}
  if(variable == "Year"){output <- masterfile[which(masterfile$Year == option),]}
  if(variable == "UnitOfAnalysis"){output <- masterfile[which(masterfile$UnitOfAnalysis == option),]}
  if(variable == "Landscape"){output <- masterfile[which(masterfile$Landscape == option),]}
  if(variable == "Country"){output <- masterfile[which(masterfile$Country == option),]}
  if(variable == "Mountain"){output <- masterfile[which(masterfile$Mountain == option),]}
  if(variable == "Name"){output <- masterfile[which(masterfile$Name == option),]}
  if(variable == "Metric"){output <- masterfile[which(masterfile$Metric == option),]}
  if(variable == "Definition"){output <- masterfile[which(masterfile$Definition == option),]}
  if(variable == "Calculation"){output <- masterfile[which(masterfile$Calculation == option),]}
  if(variable == "Unit"){output <- masterfile[which(masterfile$Unit == option),]}
  if(variable == "FilterString"){output <- masterfile[which(masterfile$FilterString == option),]}

  ##### return output
  return(output)

}
