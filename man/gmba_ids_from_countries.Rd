% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gmba_ids_from_countries.R
\name{gmba_ids_from_countries}
\alias{gmba_ids_from_countries}
\title{Get mountain range IDs from country codes}
\usage{
gmba_ids_from_countries(iso, range_selection = "all", manual = NULL)
}
\arguments{
\item{iso}{Character vector of one or more ISO-3 country code(s). If codes are not known, check
\href{https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3}{Wikipedia} or use the R package
\href{https://cran.r-project.org/web/packages/countrycode/index.html}{countrycode} to convert
country names}

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
Character vector of the GMBA Inventory v2.0 mountain range IDs that are located within
the input country codes
}
\description{
Get GMBA Inventory v2.0 mountain range IDs that are within one or more ISO-3 country codes
}
\examples{
\dontrun{
rangeid <- gmba_ids_from_country("che")
rangeids <- gmba_ids_from_country(c("che", "deu"))
}
}
