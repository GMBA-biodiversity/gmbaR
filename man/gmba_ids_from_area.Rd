% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmba_ids_from_area.R
\name{gmba_ids_from_area}
\alias{gmba_ids_from_area}
\title{Get mountain range IDs from area}
\usage{
gmba_ids_from_area(area, calc, range_selection = "all", manual = NULL)
}
\arguments{
\item{area}{Numeric, threshold value of area in size, given in km^2}

\item{calc}{Character string, should the area be taken as \emph{"max"}imum or \emph{"min"}imum?}

\item{range_selection}{Character string, the options are:
\itemize{
\item{\emph{"all"} = all level ranges, cross-scale (returns lists of ids!) (default)}
\item{\emph{"basic"} = basic mountain ranges only}
\item{\emph{"300"} = ranges included in the 300 pre-selection}
\item{\emph{"100"} = ranges included in the 100 pre-selection}
\item{\emph{"level1"}-\emph{"level10"} = ranges pertaining to the respective level
of the inventory hierarchy}
\item{\emph{"manual"} = range IDs provided with \code{manual}}}}

\item{manual}{Character string containing the IDs of manually selected mountain ranges.
Only needed when \code{range_selection} is set to \emph{manual}. Default is set to \emph{NULL}}
}
\value{
Character vector of the GMBA Inventory v2.0 mountain range IDs having a smaller or bigger
size in area as submitted
}
\description{
Get GMBA Inventory v2.0 mountain range IDs of mountain ranges that have a certain size in area
}
\examples{
\dontrun{
rangeids <- gmba_ids_from_area(5000, "max")
rangeids <- gmba_ids_from_area(5000, "min")
}
}
