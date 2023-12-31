% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FIPS.COMID.R
\name{COMID.all.FIPS}
\alias{COMID.all.FIPS}
\title{COMID to All FIPS}
\usage{
determines county code for a given COMID
}
\arguments{
\item{COMID}{as a numeric value}
}
\value{
FIPS county code that the COMID lies in
}
\description{
Obtaining a county code for environmental assessment allows further assessment of other databases that use county codes. For example, these county codes can be used to extract census data: a major factor when considering environmental harms.
The plant AES Cayuga LLC (COMID = 21980363) on Cuyaga Lake borders both Tompkins (36109) and Senaca County (36099). About 2.6 Million different COMIDs exist within the continental U.S. but only 3141 counties exist in the U.S. Therefore, the vast majority of hydrological features will reside in only one county. However, features like AES Cayuga sometimes lie across the borders of two counties. This function returns all FIPS county codes found at each point for further analysis: 36109 and 36099 in this case as 6 coordinates assigned to this COMID within the NHDPlus dataset lie in both Tompkins and Seneca county.
}
\examples{
COMID.FIPS(21980363) will return 36109 36109 36109 36109 36099 36099
}
\seealso{
[COMID.FIPS()][COMID.FIPSgeo()][COMID.all.demo()]
}
\author{
Kent Codding
}
