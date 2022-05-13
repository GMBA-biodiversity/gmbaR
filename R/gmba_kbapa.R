#' Filter the GMBA KBA/PA master file
#'
#' Filter the results from the GMBA results on PA coverage of mountain KBAs. Currently for internal use only.
#'
#' @param fileobject Name of the master file object in the R studio environment, without quotes
#' @param option List of vectors of character strings containing the options to filter, in the format \code{list(c("variable", "filterterm"), ...)}.
#' The filter vectors are applied in the order given. The variable \emph{"ResultValue"} can not be filtered.
#' Variables that can be filtered: "ID", "ID_underscore", "Year", "UnitOfAnalysis", "Landscape", "Country", "Mountain", "Name", "Metric", "Definition", "Calculation", "Unit", and "FilterString".
#' For variables \emph{"Name"} and \emph{"Metric"}, pattern matching is used, using \code{grepl( )}.
#'
#' @return Dataframe filtered from given master file object
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  waffledata <- gmba_kbapa(data, "Calculation", "rounded")
#' }

gmba_kbapa <- function(fileobject, options){

  ###### check inputs
  if(exists("fileobject") == FALSE){
    stop("Please provide the fileobject name, without quotes")
  }
  if(class(options) != "list"){
    stop("Please provide a list of filter options")
  }

  ##### run function
  output <- fileobject

  for(f in 1:length(options)){
    if(options[[f]][1] == "ID"){output <- output[which(output$ID == options[[f]][2]),]}
    if(options[[f]][1] == "ID_underscore"){output <- output[which(output$ID_underscore == options[[f]][2]),]}
    if(options[[f]][1] == "Year"){output <- output[which(output$Year == options[[f]][2]),]}
    if(options[[f]][1] == "UnitOfAnalysis"){output <- output[which(output$UnitOfAnalysis == options[[f]][2]),]}
    if(options[[f]][1] == "Landscape"){output <- output[which(output$Landscape == options[[f]][2]),]}
    if(options[[f]][1] == "Country"){output <- output[which(output$Country == options[[f]][2]),]}
    if(options[[f]][1] == "Mountain"){output <- output[which(output$Mountain == options[[f]][2]),]}
    if(options[[f]][1] == "Name"){output <- output[which(grepl(options[[f]][2], output$Name)),]}
    if(options[[f]][1] == "Metric"){output <- output[which(grepl(options[[f]][2], output$Metric)),]}
    if(options[[f]][1] == "Definition"){output <- output[which(output$Definition == options[[f]][2]),]}
    if(options[[f]][1] == "Calculation"){output <- output[which(output$Calculation == options[[f]][2]),]}
    if(options[[f]][1] == "Unit"){output <- output[which(output$Unit == options[[f]][2]),]}
    if(options[[f]][1] == "FilterString"){output <- output[which(output$FilterString == options[[f]][2]),]}
  }

  ##### return output
  return(output)

}
