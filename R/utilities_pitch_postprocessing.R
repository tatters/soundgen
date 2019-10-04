### UTILITIES FOR POSTPROCESSING OF PITCH CONTOURS ###

#' Pathfinder
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing pitch contour. Starts with a
#' reasonable guess and computes the more-or-less optimal pitch contour (not
#' quite the very optimal - too computationally expensive).
#' @param pitchCands a matrix of multiple pitch candidates per fft frame. Each
#'   column is one fft frame, each row is one candidate (the last row is always
#'   "manual")
#' @param pitchCert a matrix of the same dimensionality as pitchCands specifying
#'   our certainty in pitch candidates
#' @inheritParams analyze
#' @param interpolWin_bin when interpolating pitch candidates, the median is
#'   calculated over \code{plus-minus interpolWin_bin}
#' @param interpolTol when interpolating pitch candidates, the criterion
#'   for needing to interpolate is the absence of pitch candidates with values
#'   within \code{1 plus-minus interpolTol} of the median of pitch center of
#'   gravity over the interpolation window. For ex., if \code{interpolTol}
#'   is .05, we look for values from 0.95 to 1.05 time the median value over
#'   interpolation window.
#' @param interpolCert when interpolating pitch candidates, all generated pitch
#'   candidates are assigned a certainty equal to \code{interpolCert}
#' @param manualCert the certainty in manually added pitch candidates
#' @return Returns a numeric vector of pitch values representing the best found
#'   path through pitch candidates.
#' @keywords internal
pathfinder = function(pitchCands,
                      pitchCert,
                      pitchSource,
                      manual = NULL,
                      certWeight = 0.5,
                      pathfinding = c('none', 'fast', 'slow')[2],
                      annealPars = list(maxit = 5000, temp = 1000),
                      interpolWin_bin = 3,
                      interpolTol = 0.05,
                      interpolCert = 0.3,
                      manualCert = 1,
                      snakeStep = 0.05,
                      snakePlot = FALSE) {
  nr = nrow(pitchCands)
  nc = ncol(pitchCands)

  # Add "inviolable" manual pitch values
  if (length(manual$frame) > 0) {
    freqMan = certMan = rep(NA, nc)
    freqMan[manual$frame] = manual$freq
    certMan[manual$frame] = manualCert
    pitchCands = rbind(pitchCands, freqMan)
    pitchCert = rbind(pitchCert, certMan)
    pitchSource = rbind(pitchSource, rep('manual', nc))
    nr = nr + 1
  }

  # take log to approximate human perception of pitch differences
  pitchCands[!is.na(pitchCands)] = log2(pitchCands[!is.na(pitchCands)])
  if (!is.null(manual)) manual$freq = log2(manual$freq)

  # get the "center of gravity" of pitch candidates in each frame (mean of all
  # pitch candidates weighted by their respective certainties)
  pitchCenterGravity = rep(NA, ncol(pitchCands))
  for (i in 1:ncol(pitchCands)) {
    idxDeadCert = which(pitchSource[, i] == 'manual' & pitchCert[, i] == manualCert)
    if (length(idxDeadCert) == 1) {
      # special case: pitchCenterGravity has to pass through manual candidates
      pitchCenterGravity[i] = pitchCands[idxDeadCert, i]
    } else {
      pitchCenterGravity[i] = weighted.mean(x = pitchCands[, i],
                                            w = pitchCert[, i],
                                            na.rm = TRUE)
    }
  }

  ## INTERPOLATION
  # if a frame has no pitch candidate at all (NA) or no candidate
  # between the most likely candidates for the adjacent frames, add such a
  # candidate with ~low certainty
  if (interpolWin_bin > 0) {
    # order pitch candidates and certainties in each frame pushing NAs down so
    # as to simplify the matrix (the position of manual candidates is not
    # important since they are saved in 'manual')
    if (nrow(pitchCands) > 1) {
      o = apply(as.matrix(1:ncol(pitchCands), nrow = 1), 1, function(x) {
        order(pitchCands[, x])
      })
      pitchCands = apply(matrix(1:ncol(pitchCands)), 1, function(x) {
        pitchCands[o[, x], x]
      })
      pitchCert = apply(matrix(1:ncol(pitchCert)), 1, function(x) {
        pitchCert[o[, x], x]
      })
      pitchSource = apply(matrix(1:ncol(pitchSource)), 1, function(x) {
        pitchSource[o[, x], x]
      })
    }
    intplt = interpolate(pitchCands = pitchCands,
                         pitchCert = pitchCert,
                         pitchSource = pitchSource,
                         pitchCenterGravity = pitchCenterGravity,
                         interpolWin_bin = interpolWin_bin,
                         interpolTol = interpolTol,
                         interpolCert = interpolCert)
    pitchCands = intplt$pitchCands
    pitchCert = intplt$pitchCert
    pitchSource = intplt$pitchSource
    pitchCenterGravity = intplt$pitchCenterGravity
  }

  # remove rows with all NA's
  keep_rows = which(rowSums(!is.na(pitchCands)) > 0)
  if (length(keep_rows) > 0) {
    pitchCands = pitchCands[keep_rows, , drop = FALSE]
    pitchCert = pitchCert[keep_rows, , drop = FALSE]
    pitchSource = pitchSource[keep_rows, , drop = FALSE]
  }

  # special case: only a single pitch candidate for all frames in a syllable
  # (no paths to chose among)
  if (nrow(pitchCands) == 1) {
    return(2 ^ pitchCands)
  }

  ## PATH-FINDING
  # find the best path through frame-by-frame pitch candidates
  if (nc < 2) pathfinding = 'none'
  if (pathfinding == 'fast') {
    bestPath = pathfinding_fast(
      pitchCands = pitchCands,
      pitchCert = pitchCert,
      pitchSource = pitchSource,
      manual = manual,
      pitchCenterGravity = pitchCenterGravity,
      certWeight = certWeight
    )
  } else if (pathfinding == 'slow') {
    bestPath = pathfinding_slow(
      pitchCands = pitchCands,
      pitchCert = pitchCert,
      certWeight = certWeight,
      pitchCenterGravity = pitchCenterGravity,
      manual = manual,
      annealPars = annealPars
    )
  } else {  # if (pathfinding == 'none')
    bestPath = rep(NA, nc)
    for (i in 1:nc) {
      idx = which.min(abs(pitchCands[, i] - pitchCenterGravity[i]))
      if (length(idx) > 0 && !is.na(idx)) bestPath[i] = pitchCands[idx, i]
    }
    if (length(manual$frame) > 0) {
      bestPath[manual$frame] = manual$freq
    }
  }

  ## SNAKE
  # apply the snake algorithm to minimize the elastic forces acting on this
  # pitch contour without deviating too far from high-certainty anchors
  if (snakeStep > 0) {
    bestPath = snake(
      pitch = bestPath,
      pitchCands = pitchCands,
      pitchCert = pitchCert,
      certWeight = certWeight,
      pitchCenterGravity = pitchCenterGravity,
      snakeStep = snakeStep,
      snakePlot = snakePlot
    )
  }
  if (!is.numeric(bestPath) |
      length(bestPath) != nc) browser()
  return(2 ^ bestPath)
}


#' Interpolate
#'
#' Internal soundgen function.
#'
#' Interpolation: if a frame has no pitch candidate at all (NA) or no candidate
#' between the most likely candidates for the adjacent frames, add such a
#' candidate with some (low) certainty.
#' @inheritParams pathfinder
#' @inheritParams analyze
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @param manualUnvoiced a vector with indices of frames that are manually
#'   specified as unvoiced and should not be interpolated
#' @return Returns a modified pitchCands matrix.
#' @keywords internal
interpolate = function(pitchCands,
                       pitchCert,
                       pitchSource,
                       pitchCenterGravity,
                       interpolWin_bin = 3,
                       interpolTol = 0.3,
                       interpolCert = 0.3) {
  # ... add an empty row for new, interpolated pitch candidates
  nc = ncol(pitchCands)
  pitchCands = rbind(rep(NA, nc), pitchCands)
  pitchCert = rbind(rep(NA, nc), pitchCert)
  pitchSource = rbind(rep('intpl', nc), pitchSource)
  for (f in 1:nc) {
    left = max(1, f - interpolWin_bin)
    right = min(ncol(pitchCands), f + interpolWin_bin)
    # median over interpolation window (by default plus-minus 2 points)
    idx_man = which(pitchSource[, left:right] == 'manual' &
                      !is.na(pitchCert[, left:right]))
    if (length(idx_man) > 0) {
      # special case: if any manual cand-s over interpolWin_bin, only consider them
      med = median(pitchCands[, left:right] [idx_man], na.rm = TRUE)
    } else {
      med = median(pitchCenterGravity[left:right], na.rm = TRUE)
    }
    sum_pitchCands = sum(
      pitchCands[, f] > (1 - interpolTol) * med &
        pitchCands[, f] < (1 + interpolTol) * med,
      na.rm = TRUE
    )
    if (sum_pitchCands == 0 & !is.na(med)) {
      # if there are no pitch candidates in the frequency range
      # expected based on pitch candidates in the adjacent frames...
      # use median of adjacent frames for the new pitch cand
      pitchCands[1, f] = med
      # certainty assigned to interpolated frames
      pitchCert[1, f] = interpolCert
      # update pitchCenterGravity for the interpolated frame
      if (length(idx_man) > 0) {
        pitchCenterGravity[f] = med
      } else {
        pitchCenterGravity[f] = weighted.mean(x = pitchCands[, f],
                                              w = pitchCert[, f],
                                              na.rm = TRUE)
      }
    }
  }
  return(list(pitchCands = pitchCands,
              pitchCert = pitchCert,
              pitchSource = pitchSource,
              pitchCenterGravity = pitchCenterGravity))
}


#' Path through pitch candidates: fast
#'
#' Internal soundgen function.
#'
#' Uses a quick-and-simple heuristic to find a reasonable path though pitch
#' candidates. The idea is to start at the median of the center of gravity of
#' pitch candidates over the first/last few frames and then go over the path
#' twice (forward and backward), minimizing the cost of transitions at each step
#' in terms of pitch jumps and distance from high-certainty candidates. The best
#' of these two paths is accepted.
#' @inheritParams pathfinder
#' @inheritParams analyze
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @param manual dataframe giving manual pitch candidates, which the path
#'   MUST go through
#' @keywords internal
pathfinding_fast = function(pitchCands,
                            pitchCert,
                            pitchSource,
                            manual,
                            pitchCenterGravity,
                            certWeight) {
  nc = ncol(pitchCands)
  nr = nrow(pitchCands)

  # find the most plausible starting pitch by taking median over the first few
  # frames, weighted by certainty
  if (1 %in% manual$frame) {
    point_current = manual$freq[manual$frame == 1]
  } else {
    p = median(pitchCenterGravity[1:min(5, nc)], na.rm = TRUE)
    c = pitchCert[, 1] / abs(pitchCands[, 1] - p)
    point_current = pitchCands[which.max(c), 1]
  }
  path = point_current
  costPathForward = 0

  # run forwards
  for (i in 2:nc) {
    cands = pitchCands[, i]
    if (any(!is.na(cands))) {
      cost_cert = abs(cands - pitchCenterGravity[i])
      # get the cost of transition from the current point to each of the pitch
      # candidates in the next frame
      cost_pitchJump = apply(as.matrix(1:length(cands), nrow = 1), 1, function(x) {
        costJumps(point_current, cands[x])
      })
      if (length(cost_pitchJump) == 0) cost_pitchJump = 0
      # get a weighted average of transition costs associated with the certainty
      # of each estimate vs. the magnitude of pitch jumps
      costs = certWeight * cost_cert + (1 - certWeight) * cost_pitchJump
      if (i %in% manual$frame) {
        idx = which(pitchSource[, i] == 'manual')
      } else {
        idx = which.min(costs)
      }
      point_current = pitchCands[idx, i]
      path = c(path, point_current)
      costPathForward = costPathForward + costs[idx]
    } else {
      path = c(path, NA)
    }
  }

  # run backwards
  pitchCands_rev = pitchCands[, rev(1:nc), drop = FALSE]
  pitchCert_rev = pitchCert[, rev(1:nc), drop = FALSE]
  pitchSource_rev = pitchSource[, rev(1:nc), drop = FALSE]
  pitchCenterGravity_rev = rev(pitchCenterGravity)
  manual_rev = manual
  manual_rev$frame = nc - manual$frame + 1

  if (1 %in% manual_rev$frame) {
    point_current = manual_rev$freq[manual_rev$frame == 1]
  } else {
    p = median(pitchCenterGravity_rev[1:min(5, nc)], na.rm = TRUE)
    c = pitchCert_rev[, 1] / abs(pitchCands_rev[, 1] - p)
    point_current = pitchCands_rev[which.max(c), 1]
  }
  path_rev = point_current
  costPathBackward = 0

  for (i in 2:nc) {
    cands = pitchCands_rev[, i]
    if (any(!is.na(cands))) {
      cost_cert = abs(cands - pitchCenterGravity_rev[i])
      cost_pitchJump = apply(as.matrix(1:length(cands), nrow = 1), 1, function(x) {
        costJumps(point_current, cands[x])
      })
      if (length(cost_pitchJump) == 0) cost_pitchJump = 0
      costs = certWeight * cost_cert + (1 - certWeight) * cost_pitchJump
      if (i %in% manual_rev$frame) {
        idx = which(pitchSource_rev[, i] == 'manual')
      } else {
        idx = which.min(costs)
      }
      point_current = pitchCands_rev[idx, i]
      path_rev = c(path_rev, point_current)
      costPathBackward = costPathBackward + costs[idx]
    } else {
      path_rev = c(path_rev, NA)
    }
  }
  #
  #   er = try(costPathForward < costPathBackward, silent = TRUE)
  #   if (class(er) == 'try-error' | is.na(er)) browser()
  if (length(costPathForward) != 1 | length(costPathBackward) != 1) browser()
  if (costPathForward < costPathBackward) {
    bestPath = path
  } else {
    bestPath = rev(path_rev)
  }
  return(bestPath)
}


#' Cost of jumps
#'
#' Internal soundgen function.
#'
#' Internal helper function for calculating the cost of transitions between
#' pitch candidates. Needed for postprocessing of pitch contour - finding the
#' optimal pitch contour.
#' @param cand1,cand2 two candidate pitch values
#' @keywords internal
#' @examples
#' a = seq(-3, 3, by = .01)
#' b = 1 / (1 + 10 * exp(3 - 7 * abs(a)))
#' plot(a, b, type = 'l')
costJumps = function(cand1, cand2) {
  return(1 / (1 + 10 * exp(3 - 7 * abs(cand1 - cand2))))
}


#' Path through pitch candidates: slow
#'
#' Internal soundgen function.
#'
#' Optimizes the path through pitch candidates using simulated annealing with
#' \code{\link[stats]{optim}}. This can be really slow, depending on control
#' parameters.
#' @inheritParams pathfinder
#' @inheritParams analyze
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @param manual a dataframe of manual pitch candidates from pathfinder()
#' @keywords internal
pathfinding_slow = function(pitchCands = pitchCands,
                            pitchCert = pitchCert,
                            certWeight = certWeight,
                            pitchCenterGravity = pitchCenterGravity,
                            manual = NULL,
                            annealPars = list(maxit = 5000, temp = 1000)) {
  # start with the pitch contour most faithful to center of gravity of pitch
  # candidates for each frame
  path_init = apply(matrix(1:ncol(pitchCands)), 1, function(x) {
    which.min(abs(pitchCands[, x] - pitchCenterGravity[x]))
  })

  # use annealing to wiggle the path, attempting to minimize its cost
  o = optim(
    par = path_init,
    fn = costPerPath,
    gr = generatePath,
    pitchCands = pitchCands,
    pitchCert = pitchCert,
    certWeight = certWeight,
    pitchCenterGravity = pitchCenterGravity,
    manual = manual,
    method = 'SANN',
    control = annealPars
  )

  bestPath = apply(matrix(1:ncol(pitchCands)), 1, function(x) {
    pitchCands[o$par[x], x]
  })
  return(bestPath)
}


#' Cost per path
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contours called by
#' \code{\link{pathfinding_slow}}. Calculates the cost of a particular path
#' through pitch candidates based on pitch jumps and distance from
#' high-certainty candidates.
#' @param path evaluated path through pitch candidates (as integers specifying
#'   the rows in pitchCands, not the actual values of pitch)
#' @inheritParams pathfinder
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @param manual a dataframe of manual pitch candidates from pathfinder()
#' @keywords internal
costPerPath = function(path,
                       pitchCands,
                       pitchCert,
                       certWeight,
                       pitchCenterGravity,
                       manual = NULL) {
  # if there is nothing to wiggle, generatePath() returns NA and we want
  # annealing to terminate quickly, so we return very high cost
  if (is.na(path[1])) return(1e10)

  # if the path fails to pass through any of the manual pitch values,
  # also return immediately with very high cost
  if (!is.null(manual)) {
    # the last cand in every column is the manual one, so the path
    # should always go through the last cand in the manual frames
    if (any(path[manual$frame] != nrow(pitchCands))) {
      return(1e10)
    }
  }

  # get the cost of transition from the current point to each of the pitch
  # candidates in the next frame
  cost_pitchJump = apply(as.matrix(1:(length(path) - 1), nrow = 1), 1, function(x) {
    costJumps(pitchCands[path[x], x], pitchCands[path[x + 1], x + 1])
  })
  cost_pitchJump = mean(cost_pitchJump, na.rm = TRUE)

  # get the cost of deviating from pitchCenterGravity
  cost_cert = apply(as.matrix(1:length(path)), 1, function(x) {
    abs(pitchCands[path[x], x] - pitchCenterGravity[x])
  })
  cost_cert = mean(cost_cert, na.rm = TRUE)

  # get a weighted average of transition costs associated with the certainty
  # of each estimate vs. the magnitude of pitch jumps
  costs = certWeight * cost_cert + (1 - certWeight) * cost_pitchJump
  return(costs)
}


#' Generate path
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contours called by
#' \code{\link{pathfinding_slow}}. Generates proposals for new paths through
#' pitch candidates. It gives up and returns NA after 100 attempts, which stops
#' annealing - so the adaptation of pitch contour doesn't happen
#' @param path currently evaluated path
#' @inheritParams pathfinder
#' @param ... nothing really, but otherwise optim() complains
#' @keywords internal
generatePath = function(path, pitchCands, ...) {
  i = 1
  while (i < 100) {
    point_to_wiggle = sample(1:length(path), 1)
    idx = which(!is.na(pitchCands[, point_to_wiggle])) [-path[point_to_wiggle]]
    if (length(idx) > 0 & is.numeric(idx)) {
      path[point_to_wiggle] = sample(idx, size = 1)
      return(path)
    }
    i = i + 1
  }
  return(NA)
}


#' Snake
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Wiggles a snake
#' along the gradient of internal + external forces. NB: if the snake is run,
#' the final contour may deviate from the actually measured pitch candidates!
#' @param pitch numeric vector representing our best guess at pitch contour,
#'   which we are now attempting to improve by minimizing its elastic tension
#' @inheritParams pathfinder
#' @inheritParams analyze
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @return Returns optimized pitch contour (numeric vector of the same length as
#'   \code{pitch}).
#' @keywords internal
snake = function(pitch,
                 pitchCands,
                 pitchCert,
                 certWeight,
                 pitchCenterGravity,
                 snakeStep = 0.05,
                 snakePlot = FALSE) {
  ran = diff(range(pitchCands, na.rm = TRUE)) # range of pitch
  maxIter = floor(ran / snakeStep * 2)  # just heuristic, no theory behind this

  # plot for debugging or esthetic appreciation
  if (snakePlot) {
    # plot all pitch candidates and the initial path
    plot(seq(1, ncol(pitchCands)), pitch,
         type = 'n', xlab = 'FFT frame', ylab = 'log(pitch)',
         ylim = c(
           range(pitchCands, na.rm = TRUE)[1] - .3 * ran,
           range(pitchCands, na.rm = TRUE)[2] + .3 * ran
         )
    )
    for (r in 1:nrow(pitchCands)) {
      points (seq(1, ncol(pitchCands)),
              pitchCands[r, ],
              cex = as.numeric(pitchCert[r, ]) * 2)
    }
    lines (seq(1, ncol(pitchCands)), pitch)
  }

  # optimization algorithm follows
  i = 1
  force_old = 1e10  # Inf causes NaN in force_delta
  while (i < maxIter) {
    force = forcePerPath(pitch = pitch,
                         pitchCands = pitchCands,
                         pitchCert = pitchCert,
                         pitchCenterGravity = pitchCenterGravity,
                         certWeight = certWeight)
    force_new = mean(abs(force))
    force_delta = (force_old - force_new) / force_old
    force_old = force_new
    if (is.na(force_delta) | force_delta < snakeStep) break
    # wiggle the snake along the gradient of the total force acting on it
    # (elastic + attraction of high-certainty pitch candidates)
    pitch = pitch + snakeStep * force
    if (snakePlot) {
      lines(seq(1, length(pitch)), pitch,
            type = 'l', col = 'green', lty = 4)
    }
    i = i + 1
  }

  if (snakePlot) {
    lines(seq(1, length(pitch)), pitch,
          type = 'l', col = 'blue', lwd = 3)
  }
  return (pitch)
}


#' Force per path
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Returns the
#' total force acting on a snake (sum of internal and external gradients, i.e.
#' of the elastic force trying to straighten the snake [internal] and of the
#' force pushing the snake towards the most certain pitch estimates [external])
#' @inheritParams snake
#' @inheritParams pathfinder
#' @inheritParams analyze
#' @param pitchCenterGravity numeric vector giving the mean of all pitch
#'   candidates per fft frame weighted by our certainty in each of these
#'   candidates
#' @return Returns a numeric vector of the same length as \code{pitch} that
#'   gives the total force acting on the snake at each point.
#' @keywords internal
forcePerPath = function (pitch,
                         pitchCands,
                         pitchCert,
                         pitchCenterGravity,
                         certWeight) {
  ran = diff(range(pitchCands, na.rm = TRUE))
  # external_force = -(pitch_path - pitchCenterGravity) / ran
  external_force = pitch # just a quick way to initialize a vector of the right length
  for (i in 1:ncol(pitchCands)) {
    cands = na.omit(pitchCands[, i])
    certs = na.omit(pitchCert[, i])
    deltas = 1 / exp((cands - pitch[i]) ^ 2)
    forces = certs * deltas
    forces = ifelse(cands > pitch[i], forces, -forces)
    external_force[i] = sum(forces)
  }
  # external_force is the "external" force - the attraction of high-certainty
  # pitch candidates

  internal_force = -findGrad(pitch)
  # internal_force is the elastic force trying to make the curve smooth

  total_force = certWeight * external_force + (1 - certWeight) * internal_force
  # weighted average of internal and external forces

  return(total_force)
}


#' Find gradient
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contour. Returns
#' the elastic force acting on a snake. See \code{\link{snake}}.
#' @param path numeric vector
#' @param interpol the number of points to interpolate beyond each end of the path
#' @return Returns a vector of the same length as input path giving its 4th derivative.
#' @keywords internal
findGrad = function(path, interpol = 3) {
  # extrapolate 2 values before the first one and two after the last one based
  # on /interpol/ number of points in case the path is shorter than the
  # specified interpol:
  interpol = ifelse(interpol > length(path), length(path), interpol)
  if (interpol == 1) {
    path = c(rep(path[1], 2), path, rep(path[length(path)], 2))
  } else {
    slopeLeft = suppressWarnings(summary(lm(
      path[1:interpol] ~ seq(1, interpol)
    ))$coef[2, 1])
    minus12 = path[1] - c(1, 2) * slopeLeft
    slopeRight = suppressWarnings(summary(lm(
      path[(length(path) - interpol + 1):length(path)] ~ seq(1, interpol)
    ))$coef[2, 1])
    plus12 = path[length(path)] + c(1, 2) * slopeRight
    path = c (minus12[2], minus12[1], path, plus12[1], plus12[2])
  }

  # take the 4th derivative of the path with extrapolated values
  # (so that we get d4f over the entire length of the original path)
  grad = rep(0, length(path))
  for (i in 3:(length(path) - 2)) {  # approximation
    grad[i] = path[i - 2] - 4 * path[i - 1] + 6 * path[i] - 4 * path[i + 1] +
      path[i + 2]
  }
  grad = grad[3:(length(grad) - 2)]
  return (grad)
}


#' Median smoothing
#'
#' Internal soundgen function.
#'
#' Internal helper function for smoothing pitch contours or other contours. Only
#' outliers are modified, so it's not like smoothing with a kernel. NB: the
#' expected input is pitch, so deviance is calculated on a log-scale.
#' @param df dataframe (each column is processed separately, so multiple
#'   contours can be fed into this function at once to speed things up)
#' @param smoothing_ww width of smoothing window (points)
#' @param smoothingThres tolerated deviance from moving median (semitones)
#' @param inviolable a vector of indices of the rows rows of df that should not
#'   be modified (meant for manual pitch values)
#' @return Returns a dataframe of the same dimensions as df.
#' @keywords internal
#' @examples
#' df = data.frame(a = rnorm(20, mean = 100, sd = 20),
#'                 b = rnorm(20, mean = 100, sd = 10))
#' df1 = soundgen:::medianSmoother(df, smoothing_ww = 5,
#'       smoothingThres = 1,
#'       inviolable = c(rep(TRUE, 10), rep(FALSE, 10)))
#' plot(df[, 2], type='b')
#' lines(df1[, 2], type='b', col='blue', pch=3)
medianSmoother = function(df,
                          smoothing_ww,
                          smoothingThres,
                          inviolable = NULL) {
  temp = df # to calculate median_over_window for original values
  hw = floor(smoothing_ww / 2) # smooth over plus-minus half the smoothing_ww
  for (i in 1:nrow(df)) {
    window = c(max(i - hw, 1), min(i + hw, nrow(df))) # smoothing window
    median_over_window = apply(as.matrix(temp[(window[1]:window[2]), ]), 2, function(x) {
      median(unlist(x), na.rm = TRUE)  # w/o unlist returns NULL for NA vectors (weird...)
      # NB: use either temp or df, for original or smoothed values to be used
      # for calculating median_over_window
    })
    # difference from median pitch etc over window, in semitones
    deviance = 12 * log2(as.numeric(df[i, ]) / median_over_window)
    if (!i %in% inviolable) {
      cond = which(abs(deviance - 1) > smoothingThres)
      df[i, cond] = median_over_window[cond]
    }
  }
  return (df)
}


#' Find voiced segments
#'
#' Internal soundgen function.
#'
#' Internal helper function for postprocessing of pitch contours. Merges voiced
#' segments at least \code{shortestSyl} ms long and separated by less than
#' \code{shortestPause} ms. Called by \code{\link{analyze}}
#' @param pitchCands matrix of possible pitch values per column. One column is
#'   one fft frame, one row is one pitch candidate
#' @inheritParams analyze
#' @inheritParams spectrogram
#' @param samplingRate sampling rate (Hz)
#' @param minVoicedCands a frame is considered to be voiced if at least this
#'   many pitch candidates are not NA. Defaults to 2: since dom is usually
#'   defined, in practice this means that we also want at least one other pitch
#'   candidate (autocor, cep or BaNa)
#' @param pitchMethods methods of pitch tracking in analyze()
#' @return Returns a dataframe specifying where each voiced segment starts and
#'   ends (in fft frames, not ms!)
#' @keywords internal
findVoicedSegments = function(pitchCands,
                              shortestSyl,
                              shortestPause,
                              step,
                              samplingRate,
                              minVoicedCands,
                              pitchMethods,
                              manualV = NULL,
                              manualUnv = NULL) {
  resetMVC = FALSE
  messageMVC = FALSE
  if (is.null(minVoicedCands)) {
    resetMVC = TRUE
  }
  if (is.numeric(minVoicedCands)) {
    if (minVoicedCands < 1 | minVoicedCands > length(pitchMethods)) {
      resetMVC = TRUE
      messageMVC = TRUE
    }
  }
  if (resetMVC) {
    if ('dom' %in% pitchMethods & length(pitchMethods) > 1) {
      # since dom is usually defined, we want at least one more pitch candidate
      # (unless dom is the ONLY method that the user wants for pitch tracking)
      minVoicedCands = 2
    } else {
      minVoicedCands = 1
    }
    if (messageMVC) {
      message(paste0('minVoicedCands must be between 1 and length(pitchMethods);',
                     ' resetting to ', minVoicedCands))
    }
  }
  putativelyVoiced = apply(pitchCands, 2, function(x) sum(!is.na(x)) >= minVoicedCands)
  if (length(manualV) > 0)   putativelyVoiced[manualV]   = TRUE
  if (length(manualUnv) > 0) putativelyVoiced[manualUnv] = FALSE

  # the smallest number of consecutive non-NA pitch values that constitute a
  # voiced segment; but at least 1
  nRequired = max(1, ceiling(shortestSyl / step))
  # the greatest number of NA values that we tolerate before we say a new voiced
  # syllable begins
  toleratedGap = floor(shortestPause / step)

  # find and save separately all voiced segments
  segmentStart = numeric()
  segmentEnd = numeric()
  i = 1
  while (i < (length(putativelyVoiced) - nRequired + 1)) {
    # find beginning
    while (i < (length(putativelyVoiced) - nRequired + 1)) {
      if (sum(putativelyVoiced[i:(i + nRequired - 1)]) == nRequired) {
        segmentStart = c(segmentStart, i)
        break
      }
      i = i + 1
    }
    # find end
    if (length(segmentEnd) < length(segmentStart)) {
      while (i < (length(putativelyVoiced) - toleratedGap + 1)) {
        if (sum(putativelyVoiced[i:(i + toleratedGap)]) == 0 |
            i %in% manualUnv ) {  # interrupt the syllable if we hit a manuaUnv frame
          segmentEnd = c(segmentEnd, i - 1)
          i = i - 1
          break
        }
        i = i + 1
      }
      if (length(segmentEnd) < length(segmentStart)) {
        # if the end is not found, take the last voiced value
        segmentEnd = c(segmentEnd, tail(which(putativelyVoiced), 1))
        break
      }
    }
    i = i + 1
  }
  return(data.frame(segmentStart = segmentStart, segmentEnd = segmentEnd))
}


#' Plot pitch candidates
#'
#' Internal soundgen function.
#'
#' Plots pitch candidates or adds them to a spectrogram.
#' @param pitchCands,pitchCert,pitchSource matrices of pitch candidates, their
#'   certainty, and pitch tracking method used as generated internally by
#'   analyze(); columns = STFT frames, rows = candidates
#' @param pitch best guess at pitch contour; length = ncol(pitchCands)
#' @param candPlot,pitchPlot lists of graphical settings for plotting candidates
#'   and pitch contour, respectively
#' @param addToExistingPlot if TRUE, assumes that a spectrogram is already
#'   plotted; if FALSE, sets up a new plot
#' @param showLegend if TRUE, shows a legend
#' @param ... other graphical parameters used for creating a new plot if
#'   addToExistingPlot = FALSE
#' @examples
#' \dontrun{
#' s = soundgen()
#' a = analyze(s, 16000, windowLength = 25, step = 25,
#'             summary = 'extended', plot = FALSE)
#' spectrogram(s, 16000, windowLength = 25, step = 5)
#' addPitchCands(pitchCands = a$pitchCands,
#'               pitchCert = a$pitchCert,
#'               pitchSource = a$pitchSource,
#'               pitch = a$result$pitch)
#' }
addPitchCands = function(pitchCands,
                         pitchCert,
                         pitchSource,
                         pitch,
                         candPlot = list(),
                         pitchPlot = list(
                           col = rgb(0, 0, 1, .75),
                           lwd = 3
                         ),
                         addToExistingPlot = TRUE,
                         showLegend = TRUE,
                         ...) {
  if (length(pitchCands) < 1) invisible()
  # if plot_spec is FALSE, we first have to set up an empty plot
  if (addToExistingPlot == FALSE) {
    arguments = list(...)  # save ... arguments as a list
    m = max(pitchCands, na.rm = TRUE) / 1000  # for ylim on the empty plot
    if (is.na(m)) m = 5  # samplingRate / 2 / 1000
    arguments$ylim = c(0, m)
    do.call(plot,   # need do.call, otherwise can't pass the modified ylim
            args = c(list(x = as.numeric(colnames(pitchCands)),
                          y = rep(0, ncol(pitchCands)),
                          type = 'n'),
                     arguments))
  }
  # add pitch candidates to the plot
  pitchMethods = unique(na.omit(as.character(pitchSource)))
  if (any(!is.na(pitchCands))) {
    if (is.null(candPlot$levels)) {
      candPlot$levels = pitchMethods # c('autocor', 'spec', 'dom', 'cep')
    }
    if (is.null(candPlot$col)) {
      candPlot$col[candPlot$levels == 'autocor'] = 'green'
      candPlot$col[candPlot$levels == 'spec'] = 'red'
      candPlot$col[candPlot$levels == 'dom'] = 'orange'
      candPlot$col[candPlot$levels == 'cep'] = 'violet' # c('green', 'red', 'orange', 'violet')
      candPlot$col[candPlot$levels == 'manual'] = 'blue'
    }
    if (is.null(candPlot$pch)) {
      candPlot$pch[candPlot$levels == 'autocor'] = 16
      candPlot$pch[candPlot$levels == 'spec'] = 2
      candPlot$pch[candPlot$levels == 'dom'] = 3
      candPlot$pch[candPlot$levels == 'cep'] = 7
      candPlot$pch[candPlot$levels == 'manual'] = 18
      # candPlot$pch = c(16, 2, 3, 7)
    }
    if (is.null(candPlot$cex)) {
      candPlot$cex = 2
    }
    pitchSource_1234 = matrix(match(pitchSource, candPlot$levels),
                              ncol = ncol(pitchSource))
    for (r in 1:nrow(pitchCands)) {
      cex = pitchCert[r, ] * candPlot$cex
      cex[!is.numeric(cex)] = 2  # for manual, certainty could be Inf
      points(
        x = as.numeric(colnames(pitchCands)),
        y = pitchCands[r, ] / 1000,
        col = candPlot$col[pitchSource_1234[r, ]],
        pch = candPlot$pch[pitchSource_1234[r, ]],
        cex = pitchCert[r, ] * candPlot$cex
      )
    }
    # add the final pitch contour to the plot
    if (any(is.numeric(pitch))) {
      if (is.null(pitchPlot$col)) {
        pitchPlot$col = rgb(0, 0, 1, .75)
      }
      if (is.null(pitchPlot$lwd)) {
        pitchPlot$lwd = 3
      }
      do.call('lines', c(list(
        x = as.numeric(colnames(pitchCands)),
        y = pitch / 1000
      ),
      pitchPlot)
      )
    }
    # add a legend
    if (showLegend) {
      candPlot = as.data.frame(candPlot)
      candPlot = candPlot[candPlot$levels %in% c(pitchMethods, 'combined'), ]
      legend("topright",
             legend = c(as.character(candPlot$levels), 'combined'),
             pch = c(candPlot$pch, NA),
             lty = c(rep(NA, length(pitchMethods)),
                     ifelse(!is.null(pitchPlot$lty), pitchPlot$lty, 1)),
             lwd = c(rep(NA, length(pitchMethods)), pitchPlot$lwd),
             col = c(as.character(candPlot$col), pitchPlot$col),
             bg = "white")
    }
  }
}

#' Interpolate pitch contour
#'
#' Internal soundgen function
#'
#' Takes in a pitch contour and fills up the unvoiced gaps (NAs) by linear
#' interpolation in the middle and constant interpolation at the ends. Called by
#' pitchSmoothPraat().
#' @inheritParams pitchSmoothPraat
#' @param idx_unv which(is.na(pitch))
#' @return Returns the same numeric vector with NAs filled in by interpolation.
#' @examples
#' soundgen:::intplPitch(c(NA, 405, 441, 460, NA, NA, NA, 480, 490, NA, NA))
intplPitch = function(pitch, idx_unv = NULL) {
  len = length(pitch)
  idx_v = which(!is.na(pitch))
  if (is.null(idx_unv)) idx_unv = which(is.na(pitch))

  # fill in NAs at the ends by constant interpolation
  if (idx_v[1] > 1) {
    pitch[1:(idx_v[1] - 1)] = pitch[idx_v[1]]
  }
  last_nonNA = idx_v[length(idx_v)]
  if (last_nonNA < len) {
    pitch[(last_nonNA + 1):len] = pitch[last_nonNA]
  }

  # fill in NAs in the middle by linear interpolation
  while(any(is.na(pitch))) {
    first_na = which(is.na(pitch))[1]
    last_na = first_na + which(!is.na(pitch[first_na:len]))[1] - 2
    l = last_na - first_na + 3
    interp = seq(pitch[first_na - 1], pitch[last_na + 1], length.out = l)
    pitch[first_na:last_na] = interp[2:(l - 1)]
  }
  return(pitch)
}


#' Pitch smoothing as in Praat
#'
#' Smoothes an intonation (pitch) contour with a low-pass filter, as in Praat
#' (http://www.fon.hum.uva.nl/praat/). Algorithm: interpolates missing values
#' (unvoiced frames), performs FFT to obtain the spectrum, multiplies by a
#' Gaussian filter, performs an inverse FFT, and fills the missing values back
#' in.
#' @param pitch numeric vector of pitch values (NA = unvoiced)
#' @param bandwidth the bandwidth of low-pass filter, Hz (high = less smoothing,
#'   close to zero = more smoothing)
#' @param samplingRate the number of pitch values per second
#' @param plot if TRUE, plots the original and smoothed pitch contours
#' @export
#' @examples
#' pitch = c(NA, NA, 405, 441, 459, 459, 460, 462, 462, 458, 458, 445, 458, 451,
#' 444, 444, 430, 416, 409, 403, 403, 389, 375, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, 183, 677, 677, 846, 883, 886, 924, 938, 883, 946, 846, 911, 826, 826,
#' 788, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 307,
#' 307, 368, 377, 383, 383, 383, 380, 377, 377, 377, 374, 374, 375, 375, 375,
#' 375, 368, 371, 374, 375, 361, 375, 389, 375, 375, 375, 375, 375, 314, 169,
#' NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 238, 285, 361, 374, 375, 375,
#' 375, 375, 375, 389, 403, 389, 389, 375, 375, 389, 375, 348, 361, 375, 348,
#' 348, 361, 348, 342, 361, 361, 361, 365, 365, 361, 966, 966, 966, 959, 959,
#' 946, 1021, 1021, 1026, 1086, 1131, 1131, 1146, 1130, 1172, 1240, 1172, 1117,
#' 1103, 1026, 1026, 966, 919, 946, 882, 832, NA, NA, NA, NA, NA, NA, NA, NA,
#' NA, NA)
#' pitch_sm = pitchSmoothPraat(pitch, bandwidth = 2,
#'                             samplingRate = 39.57244, plot = TRUE)
pitchSmoothPraat = function(pitch,
                            bandwidth,
                            samplingRate,
                            plot = FALSE) {
  idx_unv = which(is.na(pitch))
  len = length(pitch)
  bin_width = samplingRate / 2 / len
  half_len = len %/% 2
  even = len %% 2 == 0

  # interpolate NAs
  pitch1 = intplPitch(pitch, idx_unv = idx_unv)

  # get spectrum
  sp = stats::fft(pitch1)
  freq = seq(bin_width / 2,
             samplingRate / 2 - bin_width / 2,
             length.out = half_len)
  # plot(freq, abs(sp[1:half_len]), type = 'l')

  # gaussian filter
  filter = exp(-(freq/bandwidth)^2)  # NB: a bit different from dnorm()
  if (even) {
    filter = c(filter, rev(filter))
  } else {
    filter = c(filter, filter[half_len], rev(filter))
  }

  # inverse fft
  pitch2 = abs(fft(sp * filter, inverse = TRUE)) / len
  pitch2[idx_unv] = NA

  if (plot) {
    plot(pitch)
    points(pitch2, type = 'l', col = 'red')
  }
  return(pitch2)
}
