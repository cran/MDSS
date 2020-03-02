\name{MDSS-package}
\alias{MDSS-package}
\alias{MDSS}
\docType{package}
\title{
  Modeling Human Dentin Serial Sectioning
}
\description{
  \code{MDSS} is a package to model microstructures of	human tooth dentin and horizontal serial-sectioning of the dentin. The corresponding age range of dentin serial sections, that is used in stable isotope analyses,	can be calculated by using this package.
}
\details{
\tabular{ll}{
  Package: \tab MDSS\cr
  Type: \tab Package\cr
  Version: \tab 1.0-0\cr
  Date: \tab 2020-02-20\cr
  License: \tab GPL (>= 3)\cr
}
  Function \code{\link{mds}} models tooth dentin microstructures and horizontal sectioning patterns. The changing dentin growth rates, oblique dentin growth layers, and dentin outer shape are modeled with pre-determined parameters and user's manual adjustments in \code{mds}. Function \code{\link{cca}} calculates corresponding age ranges for each dentin serial sections set with \code{mds}. This package adopts S3 class. The detail of mathematical models is described in Tsutaya (in prep).
}
\author{
  Takumi Tsutaya <tsutayatakumi@gmail.com>
}
\references{
  Tsutaya T. in prep. Blurred time resolution of tooth dentin serial sections.
}
\keyword{ package }
\seealso{
  \code{\link{mds}}, \code{\link{cca}}
}
\examples{
## See ?mds and ?cca for examples.
}

