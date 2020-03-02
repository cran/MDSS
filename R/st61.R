# =====R code=========================
# Stable isotope data of dentin serial sections of ST61.
# ==============================
# 2018-19-17: Tsutaya T: Coreated.
# ==============================
# Tsutaya T, Miyamoto H, Uno H, Omori T, Gakuhari T, Inahara A, Nagaoka T, Abe M, Yoneda M. 2016. From cradle to grave: multi-isotopic investigations on the life history of a higher-status female in the Edo period Japan. Anthropological Science 124:185-197. DOI: 10.1537/ase.161029.
# ==============================
unseiji_ST61_C <- list(
  rt.y = 20,
  ecj = c(3, 4.5),
  section.y = c(0:5, 6.33, 7.67, 9.00, 10.33, 11.67, 13:20),
  t.start = 0.375,
  t.end = 12.1,
  section = data.frame(
    age = c(0.7, 1.3, 1.8, 2.4, 3.0, 3.7,
      4.5, 5.3, 6.0, 6.8, 7.6, 8.3,
      8.9, 9.5, 10.0, 10.6, 11.2, 11.8),
    d13C = c(-19.2, -18.7, -18.9, -19.0, -19.1, -19.2,
      -19.3, -19.3, -19.3, -19.2, -19.1, -19.1,
      -19.1, -19.1, -19.1, -19.3, -19.5, -19.6),
    d15N = c(12.9, 12.5, 12.1, 12.1, 12.4, 11.9,
      11.7, 11.4, 11.5, 11.5, 11.4, 11.5,
      11.7, 11.6, 11.7, 11.9, 12.1, 12.6)))

unseiji_ST61_M1 <- list(
  rt.y = 18,
  ecj = c(5, 5.5),
  section.y = c(0:15, 18),
  t.start = 0.0,
  t.end = 10.0,
  section = data.frame(
    age = c(0.3, 0.8, 1.4, 1.9, 2.5, 3.1,
      3.6, 4.2, 4.7, 5.3, 5.8, 6.4,
      6.9, 7.5, 8.1, 9.2),
    d13C = c(-18.9, -19.1, -19.0, -18.7, -18.9, -18.8,
      -19.0, -19.1, -19.1, -19.0, -19.2, -19.3,
      -19.2, -19.3, -19.4, -19.4),
    d15N = c(14.5, 12.6, 12.2, 12.2, 12.4, 12.1,
      12.0, 11.7, 11.4, 11.6, 11.7, 11.7,
      11.6, 12.0, 12.3, 14.1)))

