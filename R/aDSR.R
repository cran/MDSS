# =====R code=========================
# NLSed and standardized DSR functions.
# ==============================
# 2018-10-10: Tsutaya T: Created.
# ==============================
aDSR4_I <- function(x){
# Calculate standardized dentin height (0-1)
#  for standaridized age point (0-1)
#  Solve numrically on note.
#
#  args:
#   x: Relative age, c(0, 1)
#
#  returns:
#   x: Relative dentin length, c(0, 1)
  0.000000 + 1.443227 * x + -2.847861 * x^2 + 
  4.644941 * x^3 + -2.240306 * x^4}

aDSR4_C <- function(x){
# Calculate standardized dentin height (0-1)
#  for standaridized age point (0-1)
#  Solve numrically on note.
#
#  args:
#   x: Relative age, c(0, 1)
#
#  returns:
#   x: Relative dentin length, c(0, 1)
  0.000000 + 1.059527 * x + -1.130802 * x^2 + 
  2.730541 * x^3 + -1.659266 * x^4}

aDSR4_M1 <- function(x){
# Calculate standardized dentin height (0-1)
#  for standaridized age point (0-1)
#  Solve numrically on note.
#
#  args:
#   x: Relative age, c(0, 1)
#
#  returns:
#   x: Relative dentin length, c(0, 1)
  0.000000 + 0.3991395 * x + 2.9376856 * x^2 + 
  -3.5859552 * x^3 + 1.2491301 * x^4}

aDSR4_M2 <- function(x){
# Calculate standardized dentin height (0-1)
#  for standaridized age point (0-1)
#  Solve numrically on note.
#
#  args:
#   x: Relative age, c(0, 1)
#
#  returns:
#   x: Relative dentin length, c(0, 1)
  0.000000 + 0.6015182 * x + -1.6590813 * x^2 + 
  4.5219458 * x^3 + -2.4643827 * x^4}

aDSR4_M3 <- function(x){
# Calculate standardized dentin height (0-1)
#  for standaridized age point (0-1)
#  Solve numrically on note.
#
#  args:
#   x: Relative age, c(0, 1)
#
#  returns:
#   x: Relative dentin length, c(0, 1)
  0.000000 + 0.9935197 * x + -1.8409300 * x^2 + 
  3.1922449 * x^3 + -1.3448346 * x^4}

