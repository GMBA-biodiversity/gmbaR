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
#'  waffledata <- gmba_kbapa(data, "Calculation", "rounded")
#' }

gmba_kbapa <- function(fileobject, variable, option){

  ###### rename master file object
  if(exists(fileobject) == FALSE){
    stop("Please provide the name of the master file object, without quotes")
  }

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
  if(variable == "ID"){output <- fileobject[which(fileobject$ID == option),]}
  if(variable == "ID_underscore"){output <- fileobject[which(fileobject$ID_underscore == option),]}
  if(variable == "Year"){output <- fileobject[which(fileobject$Year == option),]}
  if(variable == "UnitOfAnalysis"){output <- fileobject[which(fileobject$UnitOfAnalysis == option),]}
  if(variable == "Landscape"){output <- fileobject[which(fileobject$Landscape == option),]}
  if(variable == "Country"){output <- fileobject[which(fileobject$Country == option),]}
  if(variable == "Mountain"){output <- fileobject[which(fileobject$Mountain == option),]}
  if(variable == "Name"){output <- fileobject[which(fileobject$Name == option),]}
  if(variable == "Metric"){output <- fileobject[which(fileobject$Metric == option),]}
  if(variable == "Definition"){output <- fileobject[which(fileobject$Definition == option),]}
  if(variable == "Calculation"){output <- fileobject[which(fileobject$Calculation == option),]}
  if(variable == "Unit"){output <- fileobject[which(fileobject$Unit == option),]}
  if(variable == "FilterString"){output <- fileobject[which(fileobject$FilterString == option),]}

  ##### return output
  return(output)

}
