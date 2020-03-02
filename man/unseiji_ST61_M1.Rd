\name{unseiji_ST61_M1}
\alias{unseiji_ST61_M1}
\docType{data}
\title{
  Stable isotopic data of first molar from ST61 
}
\description{
  This data set gives the stable carbons and nitrogen isotope ratios of dentin serial sections obtained from a human maxillary first molar tooth of an individual ST61 (AD 1655--1732) from the Unseiji temple, Japan, reported in Tsutaya et al. (2016).
}
\usage{unseiji_ST61_M1}
\format{
  \code{unseiji_ST61_C} is a list containing 6 elements.\cr
  \tabular{ll}{
    \code{rt.y} \tab y-value for the root tip (i.e., length [mm] of the dentin).\cr
    \code{ecj} \tab x- and y-coordinates of enamel-cement junction. Dentin horn is placed on the geometric origin, and root tip is placed on the y-axis with a positive value.\cr
    \code{section.y} \tab y-values for the horizontal sections.\cr
    \code{t.start} and \code{t.end} \tab The age at the start and end of dentin formation, respectively.\cr
    \code{section} \tab Data frame with 16 dentin serial sections (rows) and 3 variables (columns) of assigned midpoint age with equal temporal division, carbon, and nitrogen isotope ratios, named \code{age}, \code{d13C}, and \code{d15N}, respectively. Stable isotope ratios of the last 3 sections (i.e., row number 14-16) should be excluded because of the porous structure of inner dentin.
  }
}
\details{
  See detail for Tsutaya et al. (2016).
}
\source{
  Tsutaya T, Miyamoto H, Uno H, Omori T, Gakuhari T, Inahara A, Nagaoka T, Abe M, Yoneda M. 2016. From cradle to grave: multi-isotopic investigations on the life history of a higher-status female in the Edo period Japan. Anthropological Science 124:185--197. DOI: 10.1537/ase.161029.
}
\keyword{datasets}

