% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIPS.COMID.R
\name{COMID.demo}
\alias{COMID.demo}
\title{COMID to ACS Demographics}
\usage{
returns demographics for a given COMID
}
\arguments{
\item{COMID}{as a numeric object}
}
\value{
a list of named numeric vectors of American Indian, Asian, Black, Latino, Other, White, and Total population found within given FIPS
}
\description{
This function uses the FIPS county code that contains the most NHDPlus COMID xy coordinate points to extract census data: a major factor when considering environmental harms.
Demographics can be further assessed through an environmental justice lens to consider any communities disproportionately affected by pollutants at the location of certain COMIDs.
}
\examples{
FIPS.demo(21980363) will return a named vector of demographic populations for 36109: Tompkins county
}
\seealso{
[COMID.FIPS()][FIPS.demo()][COMID.all.demo()]
}
\author{
Kent Codding
}
