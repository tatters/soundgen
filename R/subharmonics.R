# Functions for adding subharmonics (vocal fry).

#' Constant subharmonics regime
#'
#' Internal soundgen function.
#'
#' Helper function for adding vocal fry (subharmonics) to a single epoch, with
#' unchanging subharmonics regime (fixed number of subharmonics and sideband
#' width). See master function \code{\link{getVocalFry}}.
#' @param rolloff matrix of original amplitudes of each harmonic in f0 stack
#'   returned by \code{\link{getRolloff}} (columns=time, rows=frequency bins)
#' @param pitch_per_gc vector of the same length as ncol(rolloff)): f0 in Hz,
#'   one value per glottal cycle
#' @param nSubharm the number of subharmonics to generate (a positive integer).
#'   If nSubharm==1, subFreq = f0 / 2. If nSubharm==2, subFreq = f0 / 3 and 2 * f0 / 3.
#'   Etc
#' @param sideband_width_vector (either numeric or vector of the same length as
#'   pitch_per_gc): regulates how quickly the strength of subharmonics fades as
#'   they move away from harmonics in f0 stack. Low values produce narrow
#'   sidebands, high values produce uniformly strong subharmonics
#' @param throwaway01 discard harmonics that are weaker than this number (between
#'   0 and 1) to save computational resources
#' @return Returns a modified rolloff matrix with added subharmonics
#' @keywords internal
getVocalFry_per_epoch = function(rolloff,
                                 pitch_per_gc,
                                 nSubharm,
                                 sideband_width_vector,
                                 throwaway01) {
  if (nSubharm < 1) {
    return(rolloff)
  }

  # add an extra (temporary) empty harmonic at 0 Hz and another above the last
  #   f-harmonic, so as to have nice blocks
  g_seq = seq(0, nrow(rolloff) + 1, by = 1 / (nSubharm + 1))
  # initialize combined f-g harmonic stack
  rolloff_new = matrix(NA, nrow = length(g_seq), ncol = ncol(rolloff))
  rownames(rolloff_new) = g_seq
  # empty. NB: has to be zero and not NA, because we use this amplitude
  # in calculation later, so these rows are removed separately at the end
  rolloff_new[1,] = rolloff_new[nrow(rolloff_new),] = rep(0, ncol(rolloff_new))
  # fill in f harmonic stack
  rolloff_new[match(rownames(rolloff), rownames(rolloff_new)),] = rolloff

  # calculate amplitude adjustment coefficients based on the density of normal
  # distribution, so g harmonics are stronger when they are close to existing f
  # harmonics. Note that these coefficients are the same for each block, so we
  # only have to calculate them once, say for the first block (0 to f0). There
  # is obviously a mirror symmetry, so we don't have to calculate multipl_upr
  # separately, just invert multipl_lwr
  multipl_lwr = list()
  normaliz = dnorm(0, mean = 0, sd = sideband_width_vector) # the same for all g harmonics
  for (s in 1:nSubharm) {
    dist_lwr = pitch_per_gc * s / (nSubharm + 1)
    dist_upr = pitch_per_gc - dist_lwr
    multipl_lwr[[s]] = dnorm(dist_lwr, mean = 0,
                             sd = sideband_width_vector) / normaliz
  }
  multipl_upr = rev(multipl_lwr)

  # insert g harmonics to each block between f harmonics
  # (0 to f0, f0 to 2*f0, etc)
  for (block in 1:(nrow(rolloff) + 1)) {
    # +1 because we have an extra block
    # above the last f harmonic

    # lower border of the block (lower f harmonic)
    row_lwr = 1 + (block - 1) * (nSubharm + 1)
    # upper border of the block (upper f harmonic)
    row_upr = row_lwr + nSubharm + 1
    g_idx = (row_lwr + 1):(row_upr - 1) # rows to be filled with g harmonics
    for (g in 1:length(g_idx)) {
      # add up the contribution of the closest f harmonics immediately below and
      # above the current g harmonic
      harm_g = rolloff_new[row_lwr] * multipl_lwr[[g]] +
        rolloff_new[row_upr] * multipl_upr[[g]]
      rolloff_new[g_idx[g],] = harm_g
    }
  }

  # clean up
  rolloff_new[rolloff_new < throwaway01] = 0
  rolloff_new = rolloff_new [apply(rolloff_new, 1, sum) > 0, , drop = FALSE]
  return(rolloff_new)
}


#' Subharmonics
#'
#' Internal soundgen function.
#'
#' Adds subharmonics to the main (f0) harmonic stack, forming sidebands.
#' @inheritParams getVocalFry_per_epoch
#' @inheritParams soundgen
#' @return Returns a list consisting of a list of rolloff matrices (one matrix
#'   per epoch) and a dataframe of epochs.
#' @keywords internal
#' @examples
#' pitch_per_gc = c(400, 500, 600, 700)
#' rolloff = getRolloff(pitch_per_gc, rolloff = -30)
#' # one epoch, two subharmonics
#' rolloff_subh = soundgen:::getVocalFry(rolloff, pitch_per_gc,
#'   subFreq = 200, subDep = 150, shortestEpoch = 100)
#' # three epochs with 2/3/4 subharmonics
#' rolloff_subh = soundgen:::getVocalFry(rolloff, pitch_per_gc,
#'   subFreq = 200, subDep = 150, shortestEpoch = 0)
getVocalFry = function(rolloff,
                       pitch_per_gc,
                       subFreq = 100,
                       subDep = 100,
                       throwaway = -120,
                       shortestEpoch = 300) {
  # force subFreq to be a multiple of f0 at each point
  nSubharm = round(pitch_per_gc / subFreq, 0) - 1
  nSubharm[nSubharm < 0] = 0
  if (max(nSubharm) < 1) {
    return(list(
      'rolloff' = list(rolloff),
      'epochs' = data.frame('start' = 1, 'end' = length(pitch_per_gc))
    ))
  }

  throwaway01 = 2 ^ (throwaway / 10)
  period_ms = 1000 / pitch_per_gc
  min_epoch_length_points = round(shortestEpoch / period_ms)

  # to ensure each epoch is long enough, we keep the same number of harmonics
  # for at least min_epoch_length_points glottal cycles
  if (length(nSubharm) > 1) {
    nSubharm = clumper(nSubharm, minLength = min_epoch_length_points)
  }

  # divide the task into epochs based on the required number of subharmonics
  nSubharm_change_idx = which(diff(nSubharm) != 0)  # last idx before change
  epoch_start_idx = c(1, nSubharm_change_idx + 1)
  epoch_end_idx = c(nSubharm_change_idx, length(pitch_per_gc))
  nEpochs = length(nSubharm_change_idx) + 1
  # check that nEpochs == length(unique(nSubharm))
  nSubharm_per_epoch = nSubharm[c(nSubharm_change_idx, length(nSubharm))]

  # add vocal fry to rolloff for each epoch separately
  rolloff_new = vector("list", nEpochs)
  for (e in 1:nEpochs) {
    idx = epoch_start_idx[e]:epoch_end_idx[e]
    rolloff_new[[e]] = getVocalFry_per_epoch (
      rolloff = rolloff[, idx, drop = FALSE],
      pitch_per_gc = pitch_per_gc[idx],
      nSubharm = nSubharm_per_epoch[e],
      sideband_width_vector = subDep[idx],
      throwaway01 = throwaway01
    )
    # View(rolloff_new[[1]])
  }

  return(list(
    'rolloff' = rolloff_new,
    'epochs' = data.frame('start' = epoch_start_idx, 'end' = epoch_end_idx)
  ))
}
