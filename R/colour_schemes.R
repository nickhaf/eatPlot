# Pattern settings --------------------------------------------------------
sig_pattern <- c(
  "TRUE" = "none",
  "FALSE" = "stripe"
)

adj_pattern_fill <- c(
  "ohneAdj" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
  "mitAdj" = grDevices::rgb(33, 89, 104, maxColorValue = 255)
)

# Fill settings -----------------------------------------------------------
adj_fill <- c(
  "ohneAdj_TRUE" = grDevices::rgb(147, 205, 221, maxColorValue = 255),
  "mitAdj_TRUE" = grDevices::rgb(33, 89, 104, maxColorValue = 255),
  "ohneAdj_FALSE" = "white",
  "mitAdj_FALSE" = "white"
)

# Frame settings ----------------------------------------------------------
sig_frame <- c(
  "FALSE" = "dashed",
  "TRUE" = "solid"
)
# TODO: Original linetype (without manual setting) seems to be better but cannot be reproduced?
