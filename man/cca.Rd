\name{cca}
\alias{cca}
\alias{cca.default}
\title{
  Calculate corresponding age range for dentin serial sections
}
\description{
  \code{cca} calculates corresponding age range for horizontal dentin serial sections under a given class \code{"mds"} object and given ages at the start and end of tooth dentin formation.
}
\usage{
\method{cca}{default}(x, t.start, t.end)
}
\arguments{
  \item{x}{An object of class \code{"mds"}.}
  \item{t.start}{The age at the start of tooth dentin formation.}
  \item{t.end}{The age at the end of tooth dentin formation.}
}
\details{
  Function \code{cca} calculates corresponding age range for each serial section. Each GL has its formation age, and serial sections consist of several GLs that were formed in different ages. Relative volume assigned to each GL in a section is used to weigh the contribution from the GL. Modeled age ranges for the stable isotopic trajectory of dentin serial sections can be obtained by using this function.
}
\value{
  \code{cca} returns an object of \code{\link{class}} \code{"cca"}.\cr
  The function \code{\link{plot}} is used to show figures of the results.\cr
  An object of class \code{"cca"} has following list components in addition to those succeeded from \code{"mds"}:\cr
  \item{t.all}{List including 5 elements.\cr
    \tabular{ll}{
    \code{t.g} \tab Numeric vector of assigned age to each growth lines with the equal temporal division.\cr
    \code{t.n} \tab Numeric vector of assigned age to each sectioning plane with the equal temporal division.\cr
    \code{t.g.mid} \tab Numeric vector of midpoint age of each GL.\cr
    \code{t.n.ref} \tab Numeric vector of calculated midpoint of the modeled age range of each section with the consideration of changing dentin growth rates and oblique growth layers.\cr
    \code{t.n.obs} \tab Numeric vector of assigned midpoint age to each section with the equal temporal division.
    }
  }
  \item{cca}{Matrix of assigned ages to each section. The row consists of the assigned midpoint ages to sections with equal-temporal division (\code{equally}), the modeled midpoint age calculated from the weighted contribution from GLs (\code{weighted}), the duration of modeled age range (\code{duration}), and lower and upper ages of the modeled age range (\code{lower} and \code{upper}). The column corresponds to each section, and the smallest column number represents the crown-most section.}
}
\references{
  Tsutaya T 2020. Blurred time resolution of tooth dentin serial sections. American Journal of Physical Anthropology 173:748--759. DOI: 10.1002/ajpa.24113.
}
\author{
  Takumi Tsutaya developed this model.
}
\seealso{
  \code{\link{MDSS}}, \code{\link{mds}}, \code{\link{plot.cca}}
}
\examples{
## 18 sections for 18 mm lateral incisor
l18_I2_s18.mds <- mds(
  tooth = "I",
  rt.y = 18,
  section.y = 0:18,
  n.GL = 100)
l18_I2_s18.cca <- cca(
  x = l18_I2_s18.mds,
  t.start = 0.5875,
  t.end = 10.5)

# Plot corresponding age ranges.
plot(l18_I2_s18.cca)

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

# Plot modeled age ranges with stable nitrogen isotope ratios.
plot(unseiji_ST61_M1.cca,
  age = unseiji_ST61_M1$section$age,
  delta = unseiji_ST61_M1$section$d15N,
  type = "o")

# Plot without excluded sections with the porous dentin structure.
plot(unseiji_ST61_M1.cca,
  age = unseiji_ST61_M1$section$age[1:13],
  delta = unseiji_ST61_M1$section$d15N[1:13],
  type = "o")

## Irregular sectioning for 20 mm canine.
l20_C_irr.mds <- mds(
  tooth = "C",
  rt.y = 20,
  section.y = c(2:14, 15, 17, 19),
  n.GL = 100)
l20_C_irr.cca <- cca(
  x = l20_C_irr.mds,
  t.start = 0.35,
  t.end = 13.5)

# Plot corresponding age ranges.
plot(l20_C_irr.cca, xlim = c(0, 14))
}
\keyword{math}
