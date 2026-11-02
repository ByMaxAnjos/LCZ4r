# Variables used through dplyr/tidyr non-standard evaluation. Declaring them
# keeps R CMD check free of "no visible binding" notes.
utils::globalVariables(c(
  ".", "lcz", "lcz.col", "lcz_id", "my_id", "my_time", "station", "reference",
  "class", "values", "x", "y", "date", "var_interp", "anomaly", "uhi",
  "urban", "rural", "observed", "predicted", "residual", "zscore"
))
