% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIPS.COMID.R
\name{FIPS.demo}
\alias{FIPS.demo}
\title{FIPS to ACS Demographics}
\usage{
returns demographics for a given FIPS code
}
\arguments{
\item{overlay}{as an sf object}
}
\value{
a named numeric vector of American Indian, Asian, Black, Latino, Other, White, and Total population found within given FIPS
}
\description{
This function uses the county code to extract census data: a major factor when considering environmental harms.
Demographics can be further assessed through an environmental justice lens to consider any communities disproportionately affected by pollutants at the location of certain COMIDs.
}
\examples{
FIPS.demo(overlay)
}
\seealso{
[COMID.FIPS()][COMID.demo()]
}
\author{
Kent Codding
}
