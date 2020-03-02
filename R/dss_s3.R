# =====R code=========================
# # Weaning age recontruction using dentin serial section.
# ==============================
# 2018-11-07: Tsutaya T: Created.
# ==============================
# OBJECTIVE ----------
# This program performs Apporoximate Bayesian Computation with SMC
#  adopting Partial Rejection Control (PRC)
#  in order to calculates posterior distributuions
#  of the weaning ages and weaning parameters.
# ==============================
# FUNCTION DEFINITIONS ----------
# ==============================
# Define the class "mds" = Model dentin section
# ==============================
mds <- function(tooth, rt.y, ecj, section.y,
  factor.GL, n.GL, thin){
# S3 method.
  UseMethod("mds")
}

mds.default <- function(tooth, rt.y = 1, ecj = NULL, section.y,
  factor.GL = 1, n.GL = 500, thin = FALSE){
# Default method for S3 "mds" class.
#  Model dentin section, and 
#   calculate proportion of GLs in each sections.
#
# arg:
#  all arguments will be passed to CalcPropTable().
#
# return:
#  Class "mds" object.
  if(tooth == "I"){
    tooth.DSR <- "I"
    tooth.GL <- "uI2"
  }else if(tooth == "C"){
    tooth.DSR <- "C"
    tooth.GL <- "uC"
  }else if(tooth == "M1"){
    tooth.DSR <- "M1"
    tooth.GL <- "uM2"
  }else if(tooth == "M2"){
    tooth.DSR <- "M2"
    tooth.GL <- "uM2"
  }else if(tooth == "M3"){
    tooth.DSR <- "M3"
    tooth.GL <- "uM2"
  }else{
    stop(message="Only c(I, C, M1, M2, M3) are supported")
  }

  if(length(ecj) == 0){
    ecj.x <- eval(parse(text = paste(
      "rt.y * ecj.x.", tooth.GL, sep = "")))
    ecj.y <- eval(parse(text = paste(
      "rt.y * ecj.y.", tooth.GL, sep = "")))
  }else{
    ecj.x <- ecj[1]
    ecj.y <- ecj[2]
  }

  mds.result <- CalcPropTable(
    tooth.DSR = tooth.DSR,
    tooth.GL = tooth.GL,
    rt.y = rt.y,
    ecj = c(ecj.x, ecj.y),
    section.y = section.y,
    factor.GL = factor.GL,
    n.GL = n.GL,
    thin = thin)
  mds.result$call <- match.call()
  class(mds.result) <- "mds"

  return(mds.result)
}

print.mds <- function(x, ...){
# Print method for S3 "mds" class.
#
# arg:
#  x: An object which was defined as "mds" class.
#
# returns:
#  Print of match.call().
  cat("Call:\n")
  print(x$call) # Which method did match.

  cat("\n")
}

plot.mds <- function(x, ...){
# Plot method for S3 "mds" class.
#
# arg:
#  x: An object which was defined as "mds" class.
#  \dots: Passed to PlotParea.
#
  PlotParea(
    ge.x = x$ge.x,
    ge.y = x$ge.y,
    gd.x = x$gd.x,
    gd.y = x$gd.y,
    ecj.x = x$ecj.x,
    ecj.y = x$ecj.y,
    m.y = x$m.y,
    n.y = x$n.y,
    EShape = x$EShape,
    DShape = x$DShape, ...)
}

# ==============================
# Define the class "cca" = Calculate Corresponding Age
# ==============================
cca <- function(x, t.start, t.end){
# S3 method.
  UseMethod("cca")
}

cca.default <- function(x, t.start, t.end){
# Default method for S3 "cca" class.
#  Calculates corresponding age for each section.
#
# arg:
#  x: An object which was defined as "mds" class.
#   Arguments will be passed to ReturnGLAge().
#
# return:
#  Class "cca" object.
  t.all <- AssignAge(
    t.start = t.start,
    t.end = t.end,
    rt.y = x$rt.y,
    section.y = x$section.y,
    parea = x$parea)
  cca.result <- ReturnGLAge(
    t.start = t.start,
    t.end = t.end,
    rt.y = x$rt.y,
    section.y = x$section.y,
    parea = x$parea,
    included = x$included)
  result <- x
  result$t.all <- t.all
  result$cca <- cca.result
  result$t.start <- t.start
  result$t.end <- t.end
  result$call <- match.call()
  class(result) <- "cca"

  return(result)
}

plot.cca <- function(x, age = NULL, delta = NULL, error = 0.1, ...){
# Plot method for S3 "cca" class.
#
# arg:
#  x: An object which was defined as "cca" class.
#  age: Vector of originally-assigned age for each DSS.
#   If not assigned, equally-assigned age will be used.
#  delta: Vector of isotope ratio for each DSS.
#   If assigned, age * delta plot will be drawn.
#  error: Analytical error. Used for the height of "beans".
#  \dots: Passed to PlotDeltaBean or PlotAgeBean.
  if(length(age) == 0){
    age <- x$t.all$t.n.obs
  }
  if(length(delta) != 0){
    PlotDeltaBean(
      age = age,
      delta = delta,
      error = error,
      t.g.mid = x$t.all$t.g.mid,
      t.n.ref = x$t.all$t.n.ref,
      parea = x$parea, ...)
  }else{
    PlotAgeBean(
      t.g.mid = x$t.all$t.g.mid,
      t.n = x$t.all$t.n,
      t.n.ref = x$t.all$t.n.ref,
      parea = x$parea,
      section.y = x$section.y,
      rt.y = x$rt.y, ...)
  }
}

print.cca <- function(x, ...){
# Print method for S3 "cca" class.
#
# arg:
#  x: An object which was defined as "cca" class.
#
# returns:
#  Print of calculated ranges and its probability.
  cat("Call:\n")
  print(x$call) # Which method did match.
  cat("\nApparent, Weighted, Lower, Upper ages of the sections:\n")
  print(x$cca)

  cat("\n")
}

