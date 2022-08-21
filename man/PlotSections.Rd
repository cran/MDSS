\name{PlotSections}
\alias{PlotSections}
\title{
  Plot multiple corresponding age rangse for measured data
}
\description{
  \code{PlotSections} plots the corresponding age range for horizontal dentin serial sections with multiple class \code{"cca"} objects.
}
\usage{
PlotSections(age, delta, obj,
  error = 0.1, col = "black", pch = 21, \dots)
}
\arguments{
  \item{age}{List of user-assigned ages for each sections.}
  \item{delta}{List of vector observed delta values for each sections.}
  \item{obj}{List of class \code{"cca"} objects.}
  \item{error}{Vector of analytical error. Used for the height of "beans".}
  \item{col}{Vector of \code{"col"} values for plot.}
  \item{pch}{Vector of \code{"pch"} values for plot.}
  \item{\dots}{Additional arguments passed to \code{\link{plot}}.}
}
\details{
  Function \code{PlotSections} plots corresponding age range with its measured delta values and equally assigned ages for a series of serial sections. Multiple trajectories can be plotted together into a single figure.
}
\value{
  A plot of multiple trajectories of dentin serial sections.
}
\references{
  Tsutaya T 2020. Blurred time resolution of tooth dentin serial sections. American Journal of Physical Anthropology 173:748--759. DOI: 10.1002/ajpa.24113.
}
\author{
  Takumi Tsutaya developed this model.
}
\seealso{
  \code{\link{MDSS}}, \code{\link{mds}}, \code{\link{cca}}
}
\examples{
## Example of the first molar of ST61 from the Unseiji temple.
unseiji_ST61_M1.mds <- mds(
  tooth = "M1",
  rt.y = unseiji_ST61_M1$rt.y,
  ecj = unseiji_ST61_M1$ecj,
  section.y = unseiji_ST61_M1$section.y,
  n.GL = 100)
unseiji_ST61_M1.cca <- cca(
  x = unseiji_ST61_M1.mds,
  t.start = unseiji_ST61_M1$t.start,
  t.end = unseiji_ST61_M1$t.end)
unseiji_ST61_C.mds <- mds(
  tooth = "C",
  rt.y = unseiji_ST61_C$rt.y,
  ecj = unseiji_ST61_C$ecj,
  section.y = unseiji_ST61_C$section.y,
  n.GL = 100)
unseiji_ST61_C.cca <- cca(
  x = unseiji_ST61_C.mds,
  t.start = unseiji_ST61_C$t.start,
  t.end = unseiji_ST61_C$t.end)

# Plot C and M1 modeled age ranges with stable nitrogen isotope ratios.
PlotSections(
  age = list(
    unseiji_ST61_C$section$age,
    unseiji_ST61_M1$section$age),
  delta = list(
    unseiji_ST61_C$section$d15N,
    unseiji_ST61_M1$section$d15N),
  obj = list(
    C = unseiji_ST61_C.cca,
    M1 = unseiji_ST61_M1.cca))

# Plot the same data with different colors.
PlotSections(
  age = list(
    unseiji_ST61_C$section$age,
    unseiji_ST61_M1$section$age),
  delta = list(
    unseiji_ST61_C$section$d15N,
    unseiji_ST61_M1$section$d15N),
  obj = list(
    C = unseiji_ST61_C.cca,
    M1 = unseiji_ST61_M1.cca),
  col = c("red", "blue"),
  pch = c(1, 2))
}
\keyword{math}
