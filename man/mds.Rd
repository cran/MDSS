\name{mds}
\alias{mds}
\alias{mds.default}
\title{
  Model dentin serial sections
}
\description{
  Function \code{\link{mds}} models tooth dentin microstructures and horizontal sectioning patterns. The changing dentin growth rates, oblique dentin growth layers, and dentin outer shape are modeled with pre-determined parameters and user's manual adjustments.
}
\usage{
\method{mds}{default}(tooth, rt.y = 1, ecj = NULL, section.y,
  factor.GL = 1, n.GL = 500, thin = FALSE)
}
\arguments{
  \item{tooth}{Character scalar showing the target tooth type. Only \code{c("I", "C", "M1", "M2", "M3")} is supported.}
  \item{rt.y}{A positive y-value of dentin root tip in mm. Dentin horn is set on the geometric origin, and the longitudinal central axis of dentin equals the y-axis.}
  \item{ecj}{\code{c(x, y)} coordinates of enamel-cement junction (ECJ) in mm. If \code{NULL} (default), the standardized coordinate is used.}
  \item{section.y}{Vector of y-values for serial sections. Please sandwich by the start and end points for each section, so that the length of \code{section.y} equals the +1 number of the serial sections.}
  \item{factor.GL}{Numeric vector to adjust slope of dentin growth layers (GLs). A product of \code{factor.GL} and pre-determined slope is used to describe the obliqueness of GLs. If \code{factor.GL} = 1, obliqueness of GLs is the same with the reference. If >1, steeper GLs. If <1, gentler GLs.}
  \item{n.GL}{Scalar indicating number of GLs. Default is 500.}
  \item{thin}{If dentin sample was sliced out into a longitudinal thin plate, put \code{TRUE}. If \code{TRUE}, the two-dimensional area of individual GLs is used to weigh their contribution in a given section. But if cyclindrical dentin sample, such like the whole dentin or longitudinal half/quarter of dentin, was cut into serial sections, put \code{FALSE} (default). If \code{FALSE}, the cylindrical three-dimensional volume of individual GLs, that is calculated by turning it around the longitudinal central axis of the tooth, is used.}
}
\details{
  The \code{mds} is a function to model tooth dentin microstructures and horizontal serial-sectioning patterns for human permanent incisors, canines, and molars. The changing dentin growth rates, oblique dentin growth layers, and curved dentin outer shape are considered in this model. Users can adjust these parameters for individual tooth dentin samples. The details of this model are described in Tsutaya (in prep).
}
\value{
  \code{mds} returns an object of \code{\link{class}} \code{"mds"}.\cr
  The function \code{\link{plot}} is used to plot a figure of the results.\cr
  An object of class \code{"mds"} is a list containing at least the following components:\cr
  \item{parea}{Matrix of weighted proportion (volume contribution) for each GL included in each section. The row and column correspond to GLs and sections, respectively.}
  \item{included}{List containing the growth line numbers that are included in each section. The crown-most section has the smallest number as a list element.}
  \item{ge.x}{Numeric vector of the standardized x-value of cross-section between growth lines and crown dentin outer shape.}
  \item{ge.y}{Numeric vector of the standardized y-value of cross-section between growth lines and crown dentin outer shape.}
  \item{gd.x}{Numeric vector of the standardized x-value of cross-section between growth lines and root dentin outer shape.}
  \item{gd.y}{Numeric vector of the standardized y-value of cross-section between growth lines and root dentin outer shape.}
  \item{ecj.x}{Scalar of the standardized x-value of ECJ.}
  \item{ecj.y}{Scalar of the standardized y-value of ECJ.}
  \item{m.y}{Numeric vector of the standardized y-value of cross-section between growth lines and longitudinal central axis of tooth (i.e., y-axis).}
  \item{n.y}{Numeric vector of the standardized y-value of cross-section between sectioning planes and longitudinal central axis of tooth (i.e., y-axis).}
  \item{EShape}{Function describing the crown dentin outer shape.}
  \item{DShape}{Function describing the root dentin outer shape.}
  \item{Gcurve}{Function describing the tooth dentin growth lines.}
  \item{call}{The matched call.}
}
\references{
  Tsutaya T 2020. Blurred time resolution of tooth dentin serial sections. American Journal of Physical Anthropology 173:748--759. DOI: 10.1002/ajpa.24113.
}
\author{
  Takumi Tsutaya developed this model.
}
\seealso{
  \code{\link{MDSS}}, \code{\link{cca}}, \code{\link{plot.mds}}
}
\examples{
## 18 sections for 18 mm lateral incisor
l18_I2_s18.mds <- mds(
  tooth = "I",
  rt.y = 18,
  section.y = 0:18,
  n.GL = 100)

# Plot tooth dentin anatomy.
plot(l18_I2_s18.mds)

## Example of the first molar of ST61 from the Unseiji temple.
unseiji_ST61_M1.mds <- mds(
  tooth = "M1",
  rt.y = unseiji_ST61_M1$rt.y,
  ecj = unseiji_ST61_M1$ecj,
  section.y = unseiji_ST61_M1$section.y,
  n.GL = 100)

# Plot tooth dentin anatomy.
plot(unseiji_ST61_M1.mds)

## Irregular sectioning for 20 mm canine.
l20_C_irr.mds <- mds(
  tooth = "C",
  rt.y = 20,
  section.y = c(2:14, 15, 17, 19),
  n.GL = 100)

# Plot tooth dentin anatomy.
plot(l20_C_irr.mds)
}
\keyword{math}
