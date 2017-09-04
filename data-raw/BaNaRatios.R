# empirically observed ratios of harmonics, from BaNa algorithm: Ba, H., Yang, N., Demirkol, I., & Heinzelman, W. (2012, August). BaNa: A hybrid approach for noise resilient pitch detection. In Statistical Signal Processing Workshop (SSP), 2012 IEEE (pp. 369-372). IEEE.
BaNaRatios = data.frame(
  name = c(
    'F1/F0',
    'F2/FO',
    'F2/F1',
    'F3/F0',
    'F3/F2',
    'F4/F0',
    'F4/F1',
    'F4/F2',
    'F4/F3'
  ),
  value_low = c(1.9, 2.8, 1.42, 3.8, 1.29, 4.8, 2.4, 1.59, 1.15),
  value_high = c(2.1, 3.2, 1.59, 4.2, 1.42, 5.2, 2.6, 1.8, 1.29),
  divide_lower_by = c(1, 1, 2, 1, 3, 1, 2, 3, 4)
)
