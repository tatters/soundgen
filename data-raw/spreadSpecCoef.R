# Prepare the matrix of coef for the spectrum spreading function
# (see Wonhos' dissertation from 1999)
max_Barks = 27  # max 27 barks (~27000 Hz)
spreadSpecCoef = matrix(0, nrow = max_Barks, ncol = max_Barks)
for (i in 1:nrow(spreadSpecCoef)) {
  for (j in 1:ncol(spreadSpecCoef)) {
    spreadSpecCoef[i, j] = 15.81 + 7.5 * (i - j + 0.474) -
      17.5 * sqrt((1 + (i - j + 0.474) ^ 2))
  }
}
spreadSpecCoef = 10 ^ (spreadSpecCoef / 10)
# image(spreadSpecCoef)

# devtools::use_data(spreadSpecCoef, overwrite = TRUE, internal = TRUE)
