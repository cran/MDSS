# =====R code=========================
# Dentin serial section.
# ==============================
# 2017-10-09: Tsutaya T: Created.
# 2017-10-29: Tsutaya T: Changed for calculation of volume instead of area.
# 2017-10-29: Tsutaya T: Modified for automation.
# 2018-08-31: Tsutaya T: Adopted new growth line function.
# 2018-10-09: Tsutaya T: Set deep pathes for several args (e.g., my).
# 2018-10-12: Tsutaya T: Changed area calculation.
# 2018-10-15: Tsutaya T: Adopted irregular sampling.
# 2019-12-25: Tsutaya T: Devided ward and dss.
# 2020-01-05: Tsutaya T: Revised explanations.
# 2022-08-21: Tsutaya T: Added PlotSections.
# ==============================
# PURPOSE
# INPUTS
# OUTPUTS
# SOURCE and LIBRARY
# DEPENDENCE
# EXECUTED STATEMENT
# DEFINITION
# ----------
# Core functions in area calculation.
# ----------
OmNA <- function(obj){
# Omit NA from a vector.
  obj[!is.na(obj)]
}
CvNA <- function(obj){
# Convert NA to FALSE in a vector.
  obj[is.na(obj)] <- FALSE
  return(obj)
}
CvNT <- function(obj){
# Convert NA to TRUE in a vector.
  obj[is.na(obj)] <- TRUE
  return(obj)
}

# Compute volumes of related growth lines for each sections.
#
# Crown portion is named as "Eanmel",
#  root portion is named as "Dentin" for convenience.
#
#     r side (root)
#         -----
#        | DSS5 \       <- DShape (RShape in the article)
#         -------
# p side | DSS4   \ o side (outer)
# (pulp)  ---------       <- ECJ
#        | DSS3   /       <- EShape (CShape in the article)
#         -------
#     c side (crown)
#
## General
IntCR <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  vol <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = n.y[i],
      upper = n.y[j])$value
  return(vol)
}

IntPR <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  vol <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = m.y[ml],
      upper = n.y[j])$value
  return(vol)
}

## Enamel (Crown portion)
IntCO_E <- function(ml, m.y, n.y, ge.y, EShapeY, GCurveY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = n.y[i],
      upper = ge.y[ml])$value
  vol2 <-
    integrate(
      f = EShapeY,
      lower = ge.y[ml],
      upper = n.y[j])$value
  return(vol1 + vol2)
}

IntPO_E <- function(ml, m.y, n.y, ge.y, EShapeY, GCurveY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = m.y[ml],
      upper = ge.y[ml])$value
  vol2 <-
    integrate(
      f = EShapeY,
      lower = ge.y[ml],
      upper = n.y[j])$value
  return(vol1 + vol2)
}

IntCR_E <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  IntCR(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    GCurveY = GCurveY,
    i = i,
    j =j)
}

IntPR_E <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  IntPR(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    GCurveY = GCurveY,
    i = i,
    j =j)
}

Int00_E <- function(n.y, EShapeY, i, j = i+1){
  vol <- 
    integrate(
      f = EShapeY,
      lower = n.y[i],
      upper = n.y[j])$value
  return(vol)
}

## Dentin (Root portion)
IntCO_D <- function(ml, m.y, n.y, gd.y, GCurveY, DShapeY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = n.y[i],
      upper = gd.y[ml])$value
  vol2 <-
    integrate(
      f = DShapeY,
      lower = gd.y[ml],
      upper = n.y[j])$value
  return(vol1 + vol2)
}

IntPO_D <- function(ml, m.y, n.y, gd.y, GCurveY, DShapeY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = m.y[ml],
      upper = gd.y[ml])$value
  vol2 <-
    integrate(
      f = DShapeY,
      lower = gd.y[ml],
      upper = n.y[j])$value
  return(vol1 + vol2)
}

IntCR_D <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  IntCR(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    GCurveY = GCurveY,
    i = i,
    j = j)
}

IntPR_D <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  IntPR(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    GCurveY = GCurveY,
    i = i,
    j = j)
}

Int00_D <- function(n.y, DShapeY, i, j = i+1){
  vol <- 
    integrate(
      f = DShapeY,
      lower = n.y[i],
      upper = n.y[j])$value
  return(vol)
}

## ECJ (DSS contains ECJ)
Int00_JE <- function(n.y, ecj.y, EShapeY, i, j = i+1){
  vol <- 
    integrate(
      f = EShapeY,
      lower = n.y[i],
      upper = ecj.y)$value
  return(vol)
}

IntCO_JE <- function(ml, m.y, n.y, ge.y, ecj.y, EShapeY, GCurveY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = n.y[i],
      upper = ge.y[ml])$value
  vol2 <-
    integrate(
      f = EShapeY,
      lower = ge.y[ml],
      upper = ecj.y)$value
  return(vol1 + vol2)
}

IntPO_JE <- function(ml, m.y, ge.y, ecj.y, EShapeY, GCurveY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = m.y[ml],
      upper = ge.y[ml])$value
  vol2 <-
    integrate(
      f = EShapeY,
      lower = ge.y[ml],
      upper = ecj.y)$value
  return(vol1 + vol2)
}

IntCR_JE <- function(ml, m.y, n.y, ecj.y, GCurveY, i, j = i+1){
  vol <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = n.y[i],
      upper = ecj.y)$value
  return(vol)
}

IntPR_JE <- function(ml, m.y, ecj.y, GCurveY, i, j = i+1){
  vol <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = m.y[ml],
      upper = ecj.y)$value
  return(vol)
}

Int00_JD <- function(n.y, ecj.y, DShapeY, i, j = i+1){
  vol <- 
    integrate(
      f = DShapeY,
      lower = ecj.y,
      upper = n.y[j])$value
  return(vol)
}

IntCO_JD <- function(ml, m.y, n.y, gd.y, ecj.y, DShapeY, GCurveY, i, j = i+1){
  vol1 <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = ecj.y,
      upper = gd.y[ml])$value
  vol2 <-
    integrate(
      f = DShapeY,
      lower = gd.y[ml],
      upper = n.y[j])$value
  return(vol1 + vol2)
}

IntCR_JD <- function(ml, m.y, n.y, ecj.y, GCurveY, i, j = i+1){
  vol <-
    integrate(
      f = function(y){GCurveY(y = y, m.y = m.y[ml])},
      lower = ecj.y,
      upper = n.y[j])$value
  return(vol)
}

Int00_J <- function(n.y, ecj.y, EShapeY, DShapeY, i, j = i+1){
  return(
    Int00_JE(
      n.y = n.y,
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      i = i) +
    Int00_JD(
      n.y = n.y,
      ecj.y = ecj.y,
      DShapeY = DShapeY,
      i = i))
}

IntCO00_J <- function(ml, m.y, n.y, ge.y, ecj.y, EShapeY, DShapeY, GCurveY, i, j = i+1){
  return(
    IntCO_JE(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      ge.y = ge.y,
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      GCurveY = GCurveY,
      i = i) +
    Int00_JD(
      n.y = n.y,
      ecj.y = ecj.y,
      DShapeY = DShapeY,
      i = i))
}

IntPO00_J <- function(ml, m.y, n.y, ge.y, ecj.y, EShapeY, DShapeY, GCurveY, i, j = i+1){
  return(
    IntPO_JE(
      ml = ml,
      m.y = m.y,
      ge.y = ge.y,
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      GCurveY = GCurveY,
      i = i) + 
    Int00_JD(
      n.y = n.y,
      ecj.y = ecj.y,
      DShapeY = DShapeY,
      i = i))
}

IntCRCO_J <- function(ml, m.y, n.y, gd.y, ecj.y, DShapeY, GCurveY, i, j = i+1){
  return(
    IntCR_JE(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i) + 
    IntCO_JD(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      gd.y = gd.y,
      ecj.y = ecj.y,
      DShapeY = DShapeY,
      GCurveY = GCurveY,
      i = i))
}

IntCRCR_J <- function(ml, m.y, n.y, ecj.y, GCurveY, i, j = i+1){
  return(
    IntCR_JE(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i) + 
    IntCR_JD(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i))
}

IntPRCO_J <- function(ml, m.y, n.y, gd.y, ecj.y, DShapeY, GCurveY, i, j = i+1){
  return(
    IntPR_JE(
      ml = ml,
      m.y = m.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i) +
    IntCO_JD(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      gd.y = gd.y,
      ecj.y = ecj.y,
      DShapeY = DShapeY,
      GCurveY = GCurveY,
      i = i))
}

IntPRCR_J <- function(ml, m.y, n.y, ecj.y, GCurveY, i, j = i+1){
  return(
    IntPR_JE(
      ml = ml,
      m.y = m.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i) +
    IntCR_JD(
      ml = ml,
      m.y = m.y,
      n.y = n.y,
      ecj.y = ecj.y,
      GCurveY = GCurveY,
      i = i))
}

Int00PO_J <- function(ml, m.y, n.y, gd.y, DShapeY, GCurveY, i, j = i+1){
  IntPO_D(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    gd.y = gd.y,
    GCurveY = GCurveY,
    DShapeY = DShapeY,
    i = i)
}

Int00PR_J <- function(ml, m.y, n.y, GCurveY, i, j = i+1){
  IntPR(
    ml = ml,
    m.y = m.y,
    n.y = n.y,
    GCurveY = GCurveY,
    i = i)
}

# ----------
# Automation of area computation
# ----------
CalcArea <- function(ni, m.y, n.y,
  ge.y, gd.y, ecj.y, EShapeY, DShapeY, GCurveY,
  m.num, m.target, m.type2, m.00arg, m.iarg2,
  z = c("E", "D", "J")){
# Core unit of area (one serical section) culculation for growth lines.
#  Calculate from given target GLs and their type (e.g., CO, CR, PO, PR).
#  Input coordinates must be standardized ones (i.e., tooth height = 1.0).
#
#  ni: ni-th section. [1 -> n.section].
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1]. 
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  ge.y: Vector of Y coordinates of intersection between GLs and EShape.
#  gd.y: Vector of Y coordinates of intersection between GLs and DShape.
#  ecj.y: Scalar of Y coordinate of ECJ.
#  EShapeY, DShapeY, GCurveY: Function that back-calculates X coordinate
#   from a given Y coordinate for a point on the tooth shape functions.
#  m.num: Vector of number (1, 2, 3, ..., 100?) of GLs.
#   Starts from 1st GL and ends at last GL. [1 -> 100?]. 
#  m.target: Vector of target GL numbers (e.g., 17, 18, ..., 24)
#   that are included in the target ni-th section.
#  m.type2: Vector of type (e.g., "CO", "CRCR", "00", ...)
#   for all GLs (including GLs that do not relate with ni-th section).
#  m.00arg: Scalar of characters that are passed and parsed in
#   corresponding Int00 functions.
#  m.iarg2: Vector of characters that are passed and parsed in
#   corresponding Int functions. Its order corresponds with m.target.
#  z: Region of interest (i.e., Enamel, Dentin, Junction).
#
# depend:
#  Int00_, IntXX_
#
# return:
#  Vector of area for individual GLs in a given section.
  m.last2 <- length(m.num)
  m.type <- m.type2[m.target]
  m.iarg <- m.iarg2[m.target]
  m.area <- numeric(m.last2) # Starts at 0, end at 99
  m.len <- length(m.target)
  area.l <- function(){}

  if(m.len == 0){ # No growth line in the section.
    eval(parse(text = paste(
      "area.l <- Int00_", z, "(", m.00arg, "i=ni)",
      sep = "")))
    ml <- max(m.num[n.y[ni] <= m.y], na.rm = TRUE)
    m.area[ml+1] <- area.l
    m.diff <- m.area
  }else{ # >=1 growth lines in the section.
    # First area.
    ml <- m.target[1]
    eval(parse(text = paste(
      "area.l <- Int00_", z, "(", m.00arg, "i=ni)",
      sep = "")))
    m.area[ml] <- area.l
    
    # Later areas.
    for(ml in m.target){
      type.l <- m.type2[ml]
      iarg.l <- m.iarg2[ml]      
      eval(parse(text = paste(
        "area.l <- Int", type.l, "_", z, "(ml=ml,", iarg.l, "i=ni)",
        sep = "")))
      m.area[ml+1] <- area.l
    }
    
    # Subtract previous areas from the results.
    m.area.diff <- -diff(m.area)
    m.diff <- c(m.area.diff, 0) # Start at 0, end at 99
    if(m.area[1] == 0){ # Section that does not contain first GL.
      m.diff[min(m.target) - 1] <- 0
    }
    if(m.area[m.last2] != 0){ # Section that contains last GL.
      m.diff[m.last2] <- m.area[m.last2]
    }
  }

  return(m.diff)
}

CalcAreaEnamel <- function(ni, m.y, n.y,
  ge.y, gd.y, ecj.y, EShapeY, DShapeY, GCurveY,
  m.num, gn.x, ge.x, en.x){
# Calculate areas of growth lines
#  in ni-th section of dentin in enamel region.
#  Input coordinates must be standardized ones (i.e., tooth height = 1.0).
#
#  ni: ni-th section. [1 -> n.section].
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1]. 
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  ge.y: Vector of Y coordinates of intersection between GLs and EShape.
#  gd.y: Vector of Y coordinates of intersection between GLs and DShape.
#  ecj.y: Scalar of Y coordinate of ECJ.
#  EShapeY, DShapeY, GCurveY: Function that back-calculates X coordinate
#   from a given Y coordinate for a point on the tooth shape functions.
#  m.num: Vector of number (1, 2, 3, ..., 100?) of GLs.
#   Starts from 1st GL and ends at last GL. [1 -> 100?]. 
#  gn.x: Matrix of X coordinates of intersection between GLs and sections.
#   Starts from 0th section * 1st GL: [ , 1] = all NaN
#   Ends at (n.section + 1) * last GL: [last, last] = 0.
#  ge.x: Vector of X coordinates of intersection between GLs and EShape.
#  en.x: Vector of X coordinates of intersection between EShape and sections.
#
# depend:
#  OmNA, CvNA, Int00_E, IntXX_E, CalcArea
#
# return:
#  Vector of area for individual GLs in a given section.
  # Variables.
  nj <- ni + 1

  # Extract related growth lines and assign type.
  is.gn.j <- gn.x[ , nj] < en.x[nj] # Growth & jth N section
  is.gn.i <- gn.x[ , ni] <= en.x[ni] # Growth & ith N section
  is.ge.ij <- ge.y < n.y[nj] & ge.y > n.y[ni]

  m.co <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.ge.ij)])
  m.cr <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.gn.j)])
  m.po <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.ge.ij)])
  m.pr <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.gn.j)])
  
  m.target <- unique(c(m.co, m.cr, m.po, m.pr))
  m.target <- m.target[order(m.target)]
  m.target <- m.target[m.target != max(m.target)]

  m.type2 <- as.character(m.num)
  m.type2[m.co] <- "CO"
  m.type2[m.cr] <- "CR"
  m.type2[m.po] <- "PO"
  m.type2[m.pr] <- "PR"

  m.00arg <- "n.y = n.y, EShapeY = EShapeY,"

  m.iarg2 <- as.character(m.num)
  m.iarg2[m.co] <- "m.y = m.y, n.y = n.y, ge.y = ge.y,
    EShapeY = EShapeY, GCurveY = GCurveY,"
  m.iarg2[m.cr] <- "m.y = m.y, n.y = n.y, GCurveY = GCurveY,"
  m.iarg2[m.po] <- "m.y = m.y, n.y = n.y, ge.y = ge.y,
    EShapeY = EShapeY, GCurveY = GCurveY,"
  m.iarg2[m.pr] <- "m.y = m.y, n.y = n.y, GCurveY = GCurveY,"

  # Calculate area.
  m.diff <- CalcArea(
    ni = ni,
    m.y = m.y,
    n.y = n.y,
    ge.y = ge.y,
    gd.y = gd.y,
    ecj.y = ecj.y,
    EShapeY = EShapeY,
    DShapeY = DShapeY,
    GCurveY = GCurveY,
    m.num = m.num,
    m.target = m.target,
    m.type2 = m.type2,
    m.00arg = m.00arg,
    m.iarg2 = m.iarg2,
    z = "E")
  
  return(list(m.diff = m.diff, m.target = m.target))
}

CalcAreaDentin <- function(ni, m.y, n.y,
  ge.y, gd.y, ecj.y, EShapeY, DShapeY, GCurveY,
  m.num, gn.x, gd.x, dn.x){
# Calculate areas of growth lines
#  in ni-th section of dentin in dentin region.
#  Input coordinates must be standardized ones (i.e., tooth height = 1.0).
#
#  ni: ni-th section. [1 -> n.section].
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1]. 
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  ge.y: Vector of Y coordinates of intersection between GLs and EShape.
#  gd.y: Vector of Y coordinates of intersection between GLs and DShape.
#  ecj.y: Scalar of Y coordinate of ECJ.
#  EShapeY, DShapeY, GCurveY: Function that back-calculates X coordinate
#   from a given Y coordinate for a point on the tooth shape functions.
#  m.num: Vector of number (1, 2, 3, ..., 100?) of GLs.
#   Starts from 1st GL and ends at last GL. [1 -> 100?]. 
#  gn.x: Matrix of X coordinates of intersection between GLs and sections.
#   Starts from 0th section * 1st GL: [ , 1] = all NaN
#   Ends at (n.section + 1) * last GL: [last, last] = 0.
#  gd.x: Vector of X coordinates of intersection between GLs and DShape.
#  dn.x: Vector of X coordinates of intersection between DShape and sections.
#
# depend:
#  OmNA, CvNA, Int00_D, IntXX_D, CalcArea
#
# return:
#  Vector of area for individual GLs in a given section.
  # Variables.
  nj <- ni + 1

  # Extract related growth lines and assign type.
  is.gn.j <- gn.x[ , nj] < dn.x[nj] # Growth & jth N section
  is.gn.i <- gn.x[ , ni] <= dn.x[ni] # Growth & ith N section
  is.gd.ij <- gd.y < n.y[nj] & gd.y > n.y[ni]

  m.co <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.gd.ij)])
  m.cr <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.gn.j)])
  m.po <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.gd.ij)])
  m.pr <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.gn.j)])
  
  m.target <- unique(c(m.co, m.cr, m.po, m.pr))
  m.target <- m.target[order(m.target)]
  m.target <- m.target[m.target != max(m.target)]

  m.type2 <- as.character(m.num)
  m.type2[m.co] <- "CO"
  m.type2[m.cr] <- "CR"
  m.type2[m.po] <- "PO"
  m.type2[m.pr] <- "PR"
  m.type <- m.type2[m.target]

  m.00arg <- "n.y = n.y, DShapeY = DShapeY,"

  m.iarg2 <- as.character(m.num)
  m.iarg2[m.co] <- "m.y = m.y, n.y = n.y, gd.y = gd.y,
    DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.cr] <- "m.y = m.y, n.y = n.y, GCurveY = GCurveY,"
  m.iarg2[m.po] <- "m.y = m.y, n.y = n.y, gd.y = gd.y,
    DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.pr] <- "m.y = m.y, n.y = n.y, GCurveY = GCurveY,"
  m.iarg <- m.iarg2[m.target]

  # Calculate area.
  m.diff <- CalcArea(
    ni = ni,
    m.y = m.y,
    n.y = n.y,
    ge.y = ge.y,
    gd.y = gd.y,
    ecj.y = ecj.y,
    EShapeY = EShapeY,
    DShapeY = DShapeY,
    GCurveY = GCurveY,
    m.num = m.num,
    m.target = m.target,
    m.type2 = m.type2,
    m.00arg = m.00arg,
    m.iarg2 = m.iarg2,
    z = "D")

  return(list(m.diff = m.diff, m.target = m.target))
}

CalcAreaECJ <- function(ni, m.y, n.y,
  ge.y, gd.y, ecj.x, ecj.y, EShapeY, DShapeY, GCurveY,
  m.num, gn.x, ge.x, gj.x, gd.x, en.x, dn.x){
# Calculate areas of growth lines
#  in ni-th section of dentin in ECJ region.
#  Input coordinates must be standardized ones (i.e., tooth height = 1.0).
#
#  ni: ni-th section. [1 -> n.section].
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1]. 
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  ge.y: Vector of Y coordinates of intersection between GLs and EShape.
#  gd.y: Vector of Y coordinates of intersection between GLs and DShape.
#  ecj.y: Scalar of Y coordinate of ECJ.
#  EShapeY, DShapeY, GCurveY: Function that back-calculates X coordinate
#   from a given Y coordinate for a point on the tooth shape functions.
#  m.num: Vector of number (1, 2, 3, ..., 100?) of GLs.
#   Starts from 1st GL and ends at last GL. [1 -> 100?]. 
#  gn.x: Matrix of X coordinates of intersection between GLs and sections.
#   Starts from 0th section * 1st GL: [ , 1] = all NaN
#   Ends at (n.section + 1) * last GL: [last, last] = 0.
#  ge.x: Vector of X coordinates of intersection between GLs and EShape.
#  gj.x: Vector of X coordinates of intersection between GLs and ECJ line.
#  gd.x: Vector of X coordinates of intersection between GLs and DShape.
#  en.x: Vector of X coordinates of intersection between EShape and sections.
#  dn.x: Vector of X coordinates of intersection between DShape and sections.
#
# depend:
#  OmNA, CvNA, Int00_J, IntXX_J, CalcArea
#
# return:
#  Vector of area for individual GLs in a given section.
  # Variables.
  nj <- ni + 1

  # Extract related growth lines and assign type.
  is.gn.j <- gn.x[ , nj] < dn.x[nj] # Growth & jth N section
  is.gn.i <- gn.x[ , ni] <= en.x[ni] # Growth & ith N section
  is.gj <- ecj.x > gj.x # Growth & ecj line
  is.ge.ij <- ge.y <= ecj.y & ge.y > n.y[ni] # Growth & Enamel outer
  is.gd.ij <- gd.y < n.y[nj] & gd.y > ecj.y # Growth & Dentin outer

  m.co00 <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.ge.ij)])
  m.po00 <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.ge.ij)])
  m.crco <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.gd.ij)])
  m.crcr <- OmNA(m.num[CvNA(is.gn.i) & CvNA(is.gn.j)])
  m.prco <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.gj) & CvNA(is.gd.ij)])
  m.prcr <- OmNA(m.num[!CvNA(is.gn.i) & CvNA(is.gj) & !CvNA(is.gd.ij)])
  m.00po <- OmNA(m.num[!CvNA(is.gn.i) & !CvNA(is.gj) & CvNA(is.gd.ij)])
  m.00pr <- OmNA(m.num[!CvNA(is.gn.i) & !CvNA(is.gj) & !CvNA(is.gd.ij) &
    CvNA(is.gn.j)])

  
  m.target <- unique(c(m.co00, m.po00, m.crco, m.crcr,
    m.prco, m.prcr, m.00po, m.00pr))
  m.target <- m.target[order(m.target)]
  m.target <- m.target[m.target != max(m.target)]

  m.type2 <- as.character(m.num)
  m.type2[m.co00] <- "CO00"
  m.type2[m.po00] <- "PO00"
  m.type2[m.crco] <- "CRCO"
  m.type2[m.crcr] <- "CRCR"
  m.type2[m.prco] <- "PRCO"
  m.type2[m.prcr] <- "PRCR"
  m.type2[m.00po] <- "00PO"
  m.type2[m.00pr] <- "00PR"

  m.00arg <- "n.y = n.y, ecj.y = ecj.y,
    EShapeY = EShapeY, DShapeY = DShapeY,"
 
  m.iarg2 <- as.character(m.num)
  m.iarg2[m.co00] <- "m.y = m.y, n.y = n.y, ge.y = ge.y, ecj.y = ecj.y,
    EShapeY = EShapeY, DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.po00] <- "m.y = m.y, n.y = n.y, ge.y = ge.y, ecj.y = ecj.y,
    EShapeY = EShapeY, DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.crco] <- "m.y = m.y, n.y = n.y, gd.y = gd.y, ecj.y = ecj.y,
    DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.crcr] <- "m.y = m.y, n.y = n.y, ecj.y = ecj.y,
    GCurveY = GCurveY,"
  m.iarg2[m.prco] <- "m.y = m.y, n.y = n.y, gd.y = gd.y, ecj.y = ecj.y,
    DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.prcr] <- "m.y = m.y, n.y = n.y, ecj.y = ecj.y,
    GCurveY = GCurveY,"
  m.iarg2[m.00po] <- "m.y = m.y, n.y = n.y, gd.y = gd.y,
    DShapeY = DShapeY, GCurveY = GCurveY,"
  m.iarg2[m.00pr] <- "m.y = m.y, n.y = n.y,
    GCurveY = GCurveY,"

  # Calculate area.
  m.diff <- CalcArea(
    ni = ni,
    m.y = m.y,
    n.y = n.y,
    ge.y = ge.y,
    gd.y = gd.y,
    ecj.y = ecj.y,
    EShapeY = EShapeY,
    DShapeY = DShapeY,
    GCurveY = GCurveY,
    m.num = m.num,
    m.target = m.target,
    m.type2 = m.type2,
    m.00arg = m.00arg,
    m.iarg2 = m.iarg2,
    z = "J")

  return(list(m.diff = m.diff, m.target = m.target))
}

# ----------
# Functions to obtain intersections among G, E, D, N.
# ----------
csGE <- function(m.y, a.opt, slopeE){
# Calculate intersectiosn between GLs and EShape.
#  Returns corresponding X coordinate for given m.y by quadratic formula.
#
# args:
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1].
#  a.opt: Numeric scalar for GL function.
#   GCurve: y = a.opt * m.y * x + m.y
#  slopeE: Positive numeric scalar for EShape function.
#   EShape: y = slopeE * x^2 + 0
#
# return:
#  Vector of X coordinates of intersections between GLs and EShape.
  a1 <- slopeE
  b1 <- -(a.opt * m.y)
  c1 <- -(m.y)

  result <- (-(b1) + sqrt(b1^2 - 4 * a1 * c1)) / (2 * a1)
  
  return(result)
}

csGD <- function(m.y, a.opt, slopeD){
# Calculate intersectiosn between GLs and DShape.
#  Returns corresponding X coordinate for given m.y by quadratic formula.
#
# args:
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1].
#  a.opt: Numeric scalar for GL function.
#   GCurve: y = a.opt * m.y * x + m.y
#  slopeD: Negative numeric scalar for EShape function.
#   DShape: y = slopeD * x^2 + 1.0
#
# return:
#  Vector of X coordinates of intersections between GLs and DShape.
  a1 <- slopeD
  b1 <- -(a.opt * m.y)
  c1 <- 1.0 - m.y

  result <- (-(b1) - sqrt(b1^2 - 4 * a1 * c1)) / (2 * a1)
  
  return(result)
}

csEN <- function(n.y, slopeE){
# Calculate intersection between EShape and serial sections.
#  Returns X coordinate for given n.y (Y coordinate of sections).
#
# args:
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  slopeE: Positive numeric scalar for EShape function.
#   EShape: y = slopeE * x^2 + 0
#
# return:
#  Vector of X coordinates of intersections between GLs and EShape.
  sqrt(n.y / slopeE)
}

csDN <- function(n.y, slopeD){
# Calculate intersection between DShape and serial sections.
#  Returns X coordinate for given n.y (Y coordinate of sections).
#
# args:
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  slopeD: Positive numeric scalar for EShape function.
#   DShape: y = slopeD * x^2 + 0
#
# return:
#  Vector of X coordinates of intersections between GLs and DShape.
  sqrt((n.y - 1.0) / slopeD)
}

csGN <- function(a.opt, m.y, n.y){
# Calculate intersection between GLs and serial sections.
#  Returns X coordinate for given m.y and n.y.
#
# args:
#  a.opt: Numeric scalar for GL function.
#   GCurve: y = a.opt * m.y * x + m.y
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1].
#  a.opt: Numeric scalar for GL function.
#   GCurve: y = a.opt * m.y * x + m.y
#
# return:
#  Vector of X coordinates of intersections between GLs and sections.
  (n.y - m.y) / a.opt / m.y
}

# ----------
# Functions to calculate, present, and summarize proportion table.
# ----------
CalcPropTable <- function(tooth.DSR, tooth.GL,
  rt.y, ecj, section.y,
  factor.GL = 1, n.GL = 100, thin = FALSE){
# Calculate area for individual GLs in a given section from
#  given tooth anatomy and sampling parameters.
#  Main function that is used in S3 "mds" class.
#
# args:
#  tooth.DSR: Character scalar that indicates tooth type for
#   the reference dentin secretion rates (DSR).
#   c("I", "C", "M1", "M2", "M3")
#  tooth.GL: Character scalar that indicates tooth type for
#   the reference GL slope. c("uI2", "uC", "uM2")
#  rt.y: Y coordinate of apical root in mm. (Dentin horn is (0, 0))
#  ecj: c(X, Y) coordinates of ECJ in mm.
#   If not assigned, relative position of model tooth will be used.
#  section.y: Vector of Y coordinates of serial sections.
#   Must start from ny.i and end at ny.j (i.e., sandwich!).
#   So the length of section.y equals the number +1 of the serial sections.
#  factor.GL: Numeric vector to adjust slope of GLs.
#   1: Same with the reference. >1: Steeper GLs. <1: Gentler GLs.
#  n.GL: Salar indicating number of GLs.
#  thin: If the longitudinal half of dentin was longitudinally sectioned
#   further thin (to prevent 3D distribution of GLs), put TRUE.
#   If TRUE, "area" of individual GLs is calculated.
#   But if the longitudinal half of dentin was directly cut into sections
#   (3D semi- or quater-circle), put FALSE.
#   If FALSE, "volume" of individual GLs is calculated.
#
# depend:
#  CalcAreaEnamel, CalcAreaDentin, CalcAreaECJ
#
# return:
#  parea: Matrix of proportion for individual GLs in each sections.
  n.section <- length(section.y) - 1
  ecj.x <- ecj[1] / rt.y
  ecj.y <- ecj[2] / rt.y
  n.num <- seq(section.y) # Serial number of serial sections. 0 -> max
  m.num <- 1:n.GL # Serial number of tooth growth layer. 1 -> max
  n.y <- section.y / rt.y # Y coordinate of individual sections.
  n.ecj <- sum(n.y < ecj.y)

  # Time -> Y coordinate 
  aDSR <- eval(parse(text = paste(
    "aDSR4_", tooth.DSR,
    sep = ""))) # aDSR: Convert relative time (rt: 0 -> 1) to Y coordinate.
  a.opt <- eval(parse(text = paste(
    "a.opt.", tooth.GL, " * factor.GL",
    sep = "")))
  m.y <- aDSR(m.num / n.GL) # Y coordinate of start point of individual growth lines.

  # Shape parameters
  slopeE <- ecj.y / ecj.x^2
  slopeD <- (ecj.y - 1.0) / ecj.x^2

  if(n.ecj == 0){ # No ECJ in input dentin sections.
    ni.E <- integer(0)
    ni.J <- integer(0)
    ni.D <- 1:n.section
  }else if(n.ecj >= n.section + 1){ # No ECJ in input enamel sections.
    ni.E <- 1:n.section
    ni.J <- integer(0)
    ni.D <- integer(0)
  }else if(n.ecj == 1){ # ECJ in the first section.
    ni.E <- integer(0)
    ni.J <- 1
    ni.D <- 2:n.section
  }else if(n.ecj == n.section){ # ECJ in the last section.
    ni.E <- 1:(n.ecj - 1)
    ni.J <- n.section
    ni.D <- integer(0)
  }else{ # ECJ somewhere.
    ni.E <- 1:(n.ecj - 1)
    ni.J <- n.ecj
    ni.D <- (n.ecj + 1):n.section
  }

  ## Shape functions: x -> y.
  EShape <- function(x){slopeE * x^2} # Function for outer shape in crown.
  DShape <- function(x){slopeD * x^2 + 1.0} # Function for outer shape in root.
  GCurve <- function(x, m.y){a.opt * m.y * x + m.y} # Function of groth curve that returns Y coordinate (my: 0 -> 1).

  ## Shape functions
  if(thin){ # Volume: Thin 2D plane
    EShapeY <- function(y){sqrt(y / slopeE)}
    DShapeY <- function(y){sqrt((y - 1.0) / slopeD)}
    GCurveY <- function(y, m.y){(y - m.y) / (a.opt * m.y)}
  }else{ # Volume: Cylindrical 3D column = pi * radius^2 * height
    EShapeY <- function(y){pi * y / slopeE}
    DShapeY <- function(y){pi * (y - 1.0) / slopeD}
    GCurveY <- function(y, m.y){pi * ((y - m.y) / (a.opt * m.y))^2}
  }

  # Matrix of cross section.
  ## Growth line & Enamel
  ge.x <- try(csGE( # X coordinates of GL * EShape
    m.y = m.y,
    a.opt = a.opt,
    slopeE = slopeE))
  ge.y <- EShape(ge.x) # Y coordinates of GL * EShape.

  ## Growth line & Dentin
  gd.x <- csGD( # X coordinates of GL * DShape.
    m.y = m.y,
    a.opt = a.opt,
    slopeD = slopeD)
  gd.y <- DShape(gd.x) # Y coordinates of GL * DShape.

  ## Serial section and enamel, dentin, GL
  en.x <- csEN( # X coordinates of EShape * sections
    n.y = n.y,
    slopeE = slopeE)
  dn.x <- csDN( # X coordinates of DShape * sections
    n.y = n.y,
    slopeD = slopeD)
  gn.x <- mapply( # X coordinate of GL (m.y: Row) * section (n.y: Column)
    function(a){csGN(a.opt = a.opt, m.y = m.y, n.y = a)},
    a = n.y)
  gn.x[gn.x < 0] <- NaN

  ## ECJ and Enamel/Dentin, growth line
  gj.x <- csGN(
    a.opt = a.opt,
    m.y = m.y,
    n.y = ecj.y)
  gj.x[gj.x <0] <- NaN

  # Proportion table
  area.mtx <- matrix(0, nrow = length(m.num), ncol = n.section)
  target.list <- list()
  for(i in ni.E){
    x <- CalcAreaEnamel(
      ni = i,
      m.y = m.y,
      n.y = n.y,
      ge.y = ge.y,
      gd.y = gd.y, 
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      DShapeY = DShapeY,
      GCurveY = GCurveY,
      m.num = m.num,
      gn.x = gn.x,
      ge.x = ge.x,
      en.x = en.x)
    area.mtx[ , i] <- x$m.diff
    target.list[[i]] <- x$m.target
  }
  if(length(ni.J != 0)){
    x <- CalcAreaECJ(
      ni = ni.J,
      m.y = m.y,
      n.y = n.y,
      ge.y = ge.y,
      gd.y = gd.y, 
      ecj.x = ecj.x,
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      DShapeY = DShapeY,
      GCurveY = GCurveY,
      m.num = m.num,
      gn.x = gn.x,
      ge.x = ge.x,
      gj.x = gj.x,
      gd.x = gd.x,
      en.x = en.x,
      dn.x = dn.x)
    area.mtx[ , ni.J] <- x$m.diff
    target.list[[ni.J]] <- x$m.target
  }
  for(i in ni.D){
    x <- CalcAreaDentin(
      ni = i,
      m.y = m.y,
      n.y = n.y,
      ge.y = ge.y,
      gd.y = gd.y, 
      ecj.y = ecj.y,
      EShapeY = EShapeY,
      DShapeY = DShapeY,
      GCurveY = GCurveY,
      m.num = m.num,
      gn.x = gn.x,
      gd.x = gd.x,
      dn.x = dn.x)
    area.mtx[ , i] <- x$m.diff
    target.list[[i]] <- x$m.target
  }
  parea.mtx <- t(t(area.mtx) / apply(area.mtx, 2, sum))
  target.list[[1]] <- c(target.list[[1]][1] - 1, target.list[[1]])

  result <- list(
    parea = parea.mtx,
    included = target.list,
    ge.x = ge.x,
    ge.y = ge.y,
    gd.x = gd.x,
    gd.y = gd.y,
    ecj.x = ecj.x,
    ecj.y = ecj.y,
    m.y = m.y,
    n.y = n.y,
    EShape = EShape,
    DShape = DShape,
    Gcurve = GCurve,
    section.y = section.y,
    rt.y = rt.y,
    n.GL = n.GL)

  return(result)
}

PlotParea <- function(ge.x, ge.y, gd.x,
  gd.y, ecj.x, ecj.y, m.y, n.y,
  EShape, DShape, ...){
# Plot tooth dentin anatomy. Scale of Y axis is adjusted to [0 -> 1].
#
# args:
#  ge.x, ge.y, gd.x, gd.y: Vector of X/Y coordinates of intersections
#   between GLs and EShape/DShape.
#  ecj.x, ecj.y: Scalar of X/Y coordinate of ECJ.
#  m.y: Vector of Y coordinates of individual GLs.
#   Starts from 1st GL and ends at last GL. [non-0 -> 1]. 
#  n.y: Vector of Y coordinates of individual sections.
#   Starts from 0th section and ends at last section. [0 -> 1].
#  EShape, DShape: Functions describing tooth outer shape.
# \dots: Additional arguments that are passed to plot().
  ge.xx <- ge.x[ge.x <= ecj.x]
  gd.xx <- gd.x[gd.x <= ecj.x]
  ge.yy <- ge.y[ge.x <= ecj.x]
  gd.yy <- gd.y[gd.x <= ecj.x]
  xx <- seq(0, ecj.x, length.out = 100)
  Width <- c(0, 0, ecj.x)
  Height <- c(0, 1, ecj.y)

  plot(Width, Height, type = "n", ...)
  for(i in seq(m.y)){
    lines(
      x = c(0, c(ge.xx, gd.xx)[i]),
      y = c(m.y[i], c(ge.yy, gd.yy)[i]),
      col = topo.colors(length(m.y))[i])
  }
  points(xx, EShape(xx), type = "l")
  points(xx, DShape(xx), type = "l")
  points(ecj.x, ecj.y, pch = 19)
  abline(h = n.y)
}

# ----------
# Functions to clarify contributions from different ages (GLs).
# ----------
AssignAge <- function(t.start, t.end, rt.y, section.y, parea){
# Assign ages to individual GLs and serial sections.
#
# args:
#  t.start, t.end: Times (year) at the start and end of dentin formation.
#   Adjust these values for the intended part, if the target is not the
#   whole tooth (e.g., due to tooth worn or unfused root). 
#  rt.y: Y coordinate of apical root in mm. (Dentin horn is (0, 0))
#  section.y: Vector of Y coordinates of serial sections.
#   Must start from ny.i and end at ny.j (i.e., sandwich!).
#   So the length of section.y equals the number +1 of the serial sections.
#  parea: Matrix of proportion for individual GLs in each sections.
#
# returns:
#  t.g: Age of start point of individual GLs.
#  t.n: Age of start point of individual serial sections.
#  t.g.mid: Age of midpoint [i -> i+1] of individual GLs.
#  t.n.ref: True midpoint age of sections with weight of parea.
#  t.n.obs: Apparent midpoint age of sections.
  t.duration <- t.end - t.start
  t.incl <- t.duration / length(parea[ , 1])
  t.g <- seq(t.start, t.end, by = t.incl)
  n.y <- section.y / rt.y
  t.n <- t.duration * n.y + t.start

  t.g.mid <- t.g[-1] - diff(t.g)/2
  t.n.ref <- apply(t.g.mid * parea, 2, sum)
  t.n.obs <- t.n[-1] - diff(t.n)/2

  return(list(
    t.g = t.g,
    t.n = t.n,
    t.g.mid = t.g.mid,
    t.n.ref = t.n.ref,
    t.n.obs = t.n.obs))
}

CalcIncludedGL <- function(included){
# Returns number of GLs that are included in each sections.
#  args:
#   included: A list indicating the number of GLs that are
#    included in earch serial sections.
#
# returns:
#  Matrix with
#   row: c(min, max) of included GL number.
#   col: Number of serial section.
  i.len <- length(included)
  result <- matrix(0, nrow = 2, ncol = i.len)
  for(i in seq(included)){
    result[1, i] <- min(included[[i]], na.rm = TRUE)
    result[2, i] <- max(included[[i]], na.rm = TRUE)
  }
  
  return(result)
}

ReturnGLAge <- function(t.start, t.end,
  rt.y, section.y, parea, included){
# Returns matrix of apparent, true weighted, lower, upper ages
#  of included GLs in each sections.
#  Main function that is used in S3 "cca" class.
#
# arg:
#  t.start, t.end: Times (year) at the start and end of dentin formation.
#   Assign entire formation time (dentin horn -> apical root),
#    even if the target of section.y is not the whole tooth
#    (e.g., due to tooth worn or unfused root). 
#  rt.y: Y coordinate of apical root in mm. (Dentin horn is (0, 0))
#  section.y: Vector of Y coordinates of serial sections.
#   Must start from ny.i and end at ny.j (i.e., sandwich!).
#   So the length of section.y equals the number +1 of the serial sections.
#  parea: Matrix of proportion for individual GLs in each sections.
#   included: A list indicating the number of GLs that are
#    included in earch serial sections.
#
# return:
#  Matrix with
#   row: c(equally, weighted, duration, min, max) of included GL age.
#   col: Number of serial section.
  t.all <- AssignAge(
    t.start = t.start,
    t.end = t.end,
    rt.y = rt.y,
    section.y = section.y,
    parea = parea)
  included.GL <- CalcIncludedGL(included)
  included.age <- matrix(0, nrow = 2, ncol = length(included))
  duration <- numeric(length(included))
  for(i in seq(included)){
    included.age[ , i] <- t.all$t.g[(included.GL[ , i] + 1)]
    duration[i] <- diff(included.age[ , i])
  }

  result <- rbind(t.all$t.n.obs, t.all$t.n.ref, duration, included.age)
  colnames(result) <- seq(included)
  rownames(result) <- c(
    "equally", "weighted", "duration", "lower", "upper")

  return(result)
}

PlotDeltaBean <- function(age, delta,
  error, t.g.mid, t.n.ref, parea, ...){
# Plot contributed ages for each sections as a form of "beans".
#
# args:
#  age: User-assigned ages for each sections. Used only in figure.
#   If NA, simple mid-point age of each sections will be used.
#  delta: Vector of observed delta values for each sections.
#  error: Analytical error. Used for the height of "beans".
#  t.g.mid: Age of midpoint [i -> i+1] of individual GLs.
#  t.n.ref: True midpoint age of sections with weight of parea.
#  parea: Matrix of proportion for individual GLs in each sections.
# \dots: Additional arguments that are passed to image().
  Age <- age
  delta_value <- delta
  plot(Age, delta_value, ...)
  for(i in seq(parea[1, ])){
    prop2 <- parea[ , i]
    prop <- prop2 / max(prop2, na.rm = TRUE)
    is.prop <- prop != 0
    dprop <- prop * error
    polygon(
      x = c(t.g.mid[is.prop], rev(t.g.mid[is.prop])),
      y = c(dprop[is.prop], rev(dprop[is.prop] * -1)) + delta[i],
      lty = "solid", lwd = 0.5,
      col = rgb(211, 211, 211, 50, maxColorValue = 255))
    lines(
      x = rep(t.n.ref[i], 2),
      y = delta[i] + c(1, -1) * error / 4,
      lwd = 3)
  }
}

PlotAgeBean <- function(t.g.mid, t.n, t.n.ref,
  parea, rt.y, section.y, ...){
# Plot contributed ages for each sections as a form of "beans".
#
# args:
#  t.g.mid: Age of midpoint [i -> i+1] of individual GLs.
#  t.n: Age of start point of individual serial sections.
#  t.n.ref: True midpoint age of sections with weight of parea.
#  parea: Matrix of proportion for individual GLs in each sections.
#  section.y: Vector of Y coordinates of serial sections.
#   Must start from ny.i and end at ny.j (i.e., sandwich!).
#   So the length of section.y equals the number +1 of the serial sections.
#  rt.y: Y coordinate of apical root in mm. (Dentin horn is (0, 0))
# \dots: Additional arguments that are passed to plot().
  Age <- t.n
  Y_axis <- section.y
  plot(Age, Y_axis, type = "n", ylim = c(0, rt.y), ...)
  for(i in seq(parea[1, ])){
    rect(t.n[i], section.y[i], t.n[i+1], section.y[i+1])
    prop2 <- parea[ , i]
    prop <- prop2 / max(prop2, na.rm = TRUE)
    is.prop <- prop != 0
    dprop <- prop * diff(section.y)[i] / 2
    location.y <- section.y[i] + diff(section.y)[i]/2
    polygon(
      x = c(t.g.mid[is.prop], rev(t.g.mid[is.prop])),
      y = c(dprop[is.prop], rev(dprop[is.prop] * -1)) + location.y,
      lty = "solid", lwd = 0.5,
      col = rgb(211, 211, 211, 50, maxColorValue = 255))
    lines(
      x = rep(t.n.ref[i], 2),
      y = location.y + c(1, -1) * min(diff(section.y)) / 8,
      lwd = 3)
  }
}

PlotSections <- function(age, delta,
  obj, error = 0.1, col = "black", pch = 21, ...){
# Plot multiple bean and section delta with age for actual dataset.
#
# args:
#  age: List of user-assigned ages for each sections.
#  delta: List of vector observed delta values for each sections.
#  obj: List of class "cca" objects. 
#  error: Vector of analytical error. Used for the height of "beans".
#  col: Vector of "col" values for plot.
#  pch: Vector of "pch" values for plot.
#  \dots: Additional arguments that are passed to image().
  # Variables.
  n.sec <- length(age)

  if(length(error) == 1){
   error <- rep(error, n.sec)
  }
  if(length(col) == 1){
   col.p <- rep(col, n.sec)
  }else{
    col.p <- col
  }
  col.b <- adjustcolor(col.p, alpha.f = 0.2)
  if(length(pch) == 1){
   pch <- rep(pch, n.sec)
  }

  if(!exists("xlim")){
    xlim <- c(min(unlist(age), na.rm = TRUE),
      max(unlist(age), na.rm = TRUE))
  }

  if(!exists("ylim")){
    ylim <- c(min(unlist(delta), na.rm = TRUE),
      max(unlist(delta), na.rm = TRUE))
    ylim <- ylim + max(error) * c(-1, 1)
  }

  # Defining the plot area.
  Age <- NA
  delta_value <- NA
  plot(Age, delta_value, xlim = xlim, ylim = ylim, ...)

  # Plotting the beans and lines.
  for(i in 1:n.sec){
    t.g.mid <- obj[[i]]$t.all$t.g.mid
    t.n.ref <- obj[[i]]$t.all$t.n.ref
    parea <- obj[[i]]$parea

    # True age distribution.
    for(j in seq(parea[1, ])){
      prop2 <- parea[ , j]
      prop <- prop2 / max(prop2, na.rm = TRUE)
      is.prop <- prop != 0
      dprop <- prop * error[i]
      polygon(
        x = c(t.g.mid[is.prop], rev(t.g.mid[is.prop])),
        y = c(dprop[is.prop], rev(dprop[is.prop] * -1)) +
          delta[[i]][j],
        lty = "solid", lwd = 0.5, col = col.b[i])
      lines(
        x = rep(t.n.ref[j], 2),
        y = delta[[i]][j] + c(1, -1) * error[i] / 4,
        lwd = 3)
      }

    # Section delta.
    points(age[[i]], delta[[i]], type = "o",
      lwd = 1, col = col.p[i], pch = pch[i])
  }
}

# END

