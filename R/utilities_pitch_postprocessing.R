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
  if (length(interpolWin_bin) > 0 && interpolWin_bin > 0) {
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
  if (!is.numeric(bestPath) | length(bestPath) != nc) {
    # browser()
    bestPath = pitchCenterGravity
  }

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
  nr = nrow(pitchCands)
  nc = ncol(pitchCands)
  pitchCands = rbind(rep(NA, nc), pitchCands)
  pitchCert = rbind(rep(NA, nc), pitchCert)
  pitchSource = rbind(rep('intpl', nc), pitchSource)
  for (f in 1:nc) {
    left = max(1, f - interpolWin_bin)
    right = min(ncol(pitchCands), f + interpolWin_bin)
    win = left:right
    med = NA
    noLocalCands = FALSE
    if (length(win) > 1) {
      # median over interpolation window (by default plus-minus 2 points)
      idx_man = which(pitchSource[, win] == 'manual' &
                        !is.na(pitchCert[, win]))
      if (length(idx_man) > 0) {
        # special case: if any manual cand-s over interpolWin_bin, only consider them
        # med = median(pitchCands[, win] [idx_man], na.rm = TRUE)
        idx_man_rc = which(pitchSource[, win] == 'manual' &
                             !is.na(pitchCert[, win]), arr.ind = TRUE)
        curve = rep(NA, length(win))
        curve[idx_man_rc[, 'col']] = pitchCands[, win] [idx_man]
      } else {
        # no manual cand's over interpolWin_bin --> use any cand-s for
        # interpolation
        curve = pitchCenterGravity[win]
        curve[f + 1 - left] = NA
      }
      curve_df = na.omit(data.frame(x = 1:length(curve),
                                    y = curve))
      if (nrow(curve_df) > 2) {
        # >2 points - use loess
        l = try(suppressWarnings(loess(y ~ x, data = curve_df, span = 1)))
        if (class(med) != 'try-error') {
          p = predict(l, newdata = 1:tail(curve_df$x, n = 1))
          med = p[f + 1 - left]
          # print(c(f, 'loess'))
        }
      } else {
        # a single point - just take the middle
        med = mean(curve_df$y)
        # print(c(f, 'mean'))
      }
      noLocalCands = sum(
        pitchCands[, f] > (1 - interpolTol) * med &
          pitchCands[, f] < (1 + interpolTol) * med,
        na.rm = TRUE
      ) == 0
      if (noLocalCands & !is.na(med)) {
        # if there are no pitch candidates in the frequency range
        # expected based on pitch candidates in the adjacent frames...
        # use median of adjacent frames for the new pitch cand
        pitchCands[1, f] = med
        # certainty assigned to interpolated frames
        pitchCert[1, f] = interpolCert
        # update pitchCenterGravity for the interpolated frame
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
    if (length(point_current) < 1) point_current = NA
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
      if (length(cost_pitchJump) == 0 || !any(!is.na(cost_pitchJump))) cost_pitchJump = 0
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
    if (length(point_current) < 1) point_current = NA
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
      if (length(cost_pitchJump) == 0 || !any(!is.na(cost_pitchJump))) cost_pitchJump = 0
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
  #   if (class(er)[1] == 'try-error' | is.na(er)) browser()
  if (length(costPathForward) != 1 | length(costPathBackward) != 1) {
    # browser()
    return(NA)
  }
  if (costPathForward < costPathBackward) {
    bestPath = path
  } else {
    bestPath = rev(path_rev)
  }
  # if (length(bestPath) != nc) browser()
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
    # to be conservative, treat NAs as repetitions of value i
    winVal = as.matrix(temp[(window[1]:window[2]), ])
    for (c in 1:ncol(winVal)) {
      idx_na = which(is.na(winVal[, c]))
      winVal[idx_na, c] = df[i, c]
    }
    median_over_window = apply(winVal, 2, function(x) {
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
  return(df)
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
#' @param manualV index of frames that should definitely be voiced (manual
#'   candidates)
#' @param manualTryToV index of frames that should be treated as voiced as long
#'   as they have any candidates at all (even <minVoicedCands)
#' @param manualUnv index of frames forced to be unvoiced
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
                              manualTryToV = NULL,
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
  putativelyVoiced = apply(pitchCands, 2, function(x)
    sum(!is.na(x)) >= minVoicedCands)
  if (length(manualV) > 0) putativelyVoiced[manualV] = TRUE
  if (length(manualTryToV)) {
    # voice as forced by manualTryToV, as long as those frames have some cand-s
    anyCands = which(apply(pitchCands, 2, function(x) any(!is.na(x))))
    putativelyVoiced[manualTryToV[manualTryToV %in% anyCands]] = TRUE
  }
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
      if (sum(putativelyVoiced[i:(i + nRequired - 1)]) == nRequired |
          i %in% manualV) {  # start a new syllable if we hit a manually voiced frame
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
#' @param extraContour another contour to add to the plot, such as harmHeight, Hz
#' @param addToExistingPlot if TRUE, assumes that a spectrogram is already
#'   plotted; if FALSE, sets up a new plot
#' @param showLegend if TRUE, shows a legend
#' @param y_Hz if TRUE, plot in Hz, otherwise in kHz
#' @param ... other graphical parameters used for creating a new plot if
#'   addToExistingPlot = FALSE
#' @keywords internal
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
                         timestamps = NULL,
                         candPlot = list(),
                         pitchPlot = list(),
                         extraContour = NULL,
                         extraContour_pars = list(),
                         priorMean = NULL,
                         priorSD = NULL,
                         pitchFloor = NULL,
                         pitchCeiling = NULL,
                         addToExistingPlot = TRUE,
                         showLegend = TRUE,
                         y_Hz = FALSE,
                         ...) {
  if (is.null(pitchCands) & is.null(pitch)) invisible()
  if (length(pitchCands) < 1 & length(pitch) < 1) invisible()
  if (is.null(timestamps) & !is.null(pitchCands)) {
    timestamps = as.numeric(colnames(pitchCands))
  }
  if (is.null(pitchPlot$showPrior)) {
    showPrior = TRUE
  } else {
    showPrior = pitchPlot$showPrior
  }
  if (showPrior & is.numeric(priorMean) & is.numeric(priorSD)) {
    prior = getPrior(
      priorMean = priorMean,
      priorSD = priorSD,
      pitchFloor = pitchFloor,
      pitchCeiling = pitchCeiling,
      len = 100,
      plot = FALSE
    )
  } else {
    prior = NULL
  }
  pitchPlot = pitchPlot[names(pitchPlot) != 'showPrior']
  yScaleCoef = ifelse(y_Hz, 1, 1/1000)

  # If addToExistingPlot is FALSE, we first have to set up an empty plot
  if (addToExistingPlot == FALSE) {
    arguments = list(...)  # save ... arguments as a list
    m = max(pitchCands, na.rm = TRUE) * yScaleCoef  # for ylim on the empty plot
    if (is.na(m)) m = 5  # samplingRate / 2 * yScaleCoef
    arguments$ylim = c(0, m)
    do.call(plot,   # need do.call, otherwise can't pass the modified ylim
            args = c(list(x = timestamps,
                          y = rep(0, ncol(pitchCands)),
                          type = 'n'),
                     arguments))
  }

  # Prepare plot pars - combine user-defined with defaults
  plotPars = defaults_analyze_pitchCand  # see presets.R
  candPlot$final = pitchPlot

  for (i in 1:length(candPlot)) {
    temp = candPlot[[i]]
    if (length(temp) > 0) {
      method = names(candPlot)[i]
      idx = which(plotPars$method == method)
      if (length(idx) > 0) {
        for (j in 1:length(temp)) {
          property = names(temp[j])[1]
          if (!property %in% colnames(plotPars)) {
            # set to default, otherwise other rows become NA
            if (property == 'cex') {
              plotPars[, property] = 2
            } else if (property == 'lty') {
              plotPars[, property] = 1  # par(lty) gives "solid" - a character
            } else {
              plotPars[, property] = par(property)
            }
          }
          plotPars[idx, property] = temp[j]
        }
      }
    }
  }

  # Add pitch candidates to the plot
  pitchMethods = unique(na.omit(as.character(pitchSource)))
  if (length(pitchMethods) > 0) {
    for (r in 1:nrow(pitchCands)) {
      method_r = as.character(pitchSource[r, ])
      method_r[is.na(method_r)] = 'def'
      pars_method = as.list(plotPars[match(method_r, plotPars$method), 2:ncol(plotPars)])
      pars_method$cex = pars_method$cex *
        plotPars$cex[plotPars$method == 'final'] *
        pitchCert[r, ]
      # for manual, certainty could be Inf, so we reset to 2
      pars_method$cex[which(!is.na(pars_method$cex) &
                              pars_method$cex == Inf)] = 2
      # pars_method$cex[is.na(pars_method$cex)] = 0
      do.call(points, c(pars_method,
                        list(x = timestamps,
                             y = pitchCands[r, ] * yScaleCoef)))
    }
  } else {
    showLegend = FALSE
  }

  # Add the final pitch contour to the plot
  if (any(is.numeric(pitch))) {
    pars_pitchContour = as.list(plotPars[plotPars$method == 'final',
                                         2:ncol(plotPars)])
    do.call('lines', c(list(
      x = timestamps,
      y = pitch * yScaleCoef
    ), pars_pitchContour))
  }

  # Add an extra contour such as harmHeight
  if (!is.null(extraContour)) {
    if (any(!is.na(extraContour)) & length(timestamps) > 0) {
      if (is.null(extraContour_pars$type)) extraContour_pars$type = 'l'
      if (extraContour_pars$type != 'n') {
        if (is.null(extraContour_pars$lty)) extraContour_pars$lty = 2
        if (is.null(extraContour_pars$lwd)) extraContour_pars$lwd = 2
        if (is.null(extraContour_pars$col)) extraContour_pars$col = 'red'
        do.call(points, c(list(x = timestamps, y = extraContour * yScaleCoef),
                          extraContour_pars))
      }
    }
  }

  # Show prior
  if (!is.null(prior)) {
    ran_x_5 = (tail(timestamps, 1) - timestamps[1]) * .075   # 7.5% of plot width
    points(x = prior$prob * ran_x_5,
           y = prior$freq * yScaleCoef, type = 'l', lty = 2)
    text(x = ran_x_5,
         y = priorMean * yScaleCoef,
         pos = 2, labels = 'Prior', cex = 0.65, offset = 0.25)
  }
  text(x = 0,
       y = pitchFloor * yScaleCoef,
       pos = 4, labels = 'floor', cex = 0.65, offset = 0)
  text(x = 0,
       y = pitchCeiling * yScaleCoef,
       pos = 4, labels = 'ceiling', cex = 0.65, offset = 0)

  # Add a legend
  if (showLegend) {
    pl = plotPars[plotPars$method %in% c(pitchMethods, 'final'), ]
    legend("topright",
           legend = pl$method,
           pch = pl$pch,
           lty = ifelse(pl$method == 'final', pl$lty, NA),
           lwd = pl$lwd,
           col = pl$col,
           bg = "white")
  }
  invisible()
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
#' @keywords internal
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
#' in. The \code{bandwidth} parameter is about half the cutoff frequency (ie
#' some frequencies will still be present up to ~2 * bandwidth)
#'
#' @seealso \code{\link{analyze}}
#'
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
#' pitchSmoothPraat(pitch, bandwidth = 10, samplingRate = 40, plot = TRUE)
#' pitchSmoothPraat(pitch, bandwidth = 2, samplingRate = 40, plot = TRUE)
pitchSmoothPraat = function(pitch,
                            bandwidth,
                            samplingRate,
                            plot = FALSE) {
  # make positive
  m = min(pitch, na.rm = TRUE)
  pitch = pitch - m
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
  # plot(freq, abs(sp[1:half_len]) / max(abs(sp[1:half_len])), type = 'l')

  # gaussian filter
  filter = exp(-(freq/bandwidth)^2)  # NB: a bit different from dnorm()
  # points(freq, filter, type = 'l', col = 'blue')
  if (even) {
    filter = c(filter, rev(filter))
  } else {
    filter = c(filter, filter[half_len], rev(filter))
  }

  # inverse fft
  pitch2 = abs(fft(sp * filter, inverse = TRUE)) / len
  pitch2[idx_unv] = NA
  pitch2 = pitch2 + m  # back to the original scale

  if (plot) {
    plot(pitch + m)
    points(pitch2, type = 'l', col = 'red')
  }
  return(pitch2)
}


#' Find inflections
#'
#' Finds inflections in discrete time series such as pitch contours. When there
#' are no missing values and no thresholds, this can be accomplished with a fast
#' one-liner like \code{which(diff(diff(x) > 0) != 0) + 1}. Missing values are
#' interpolated by repeating the first and last non-missing values at the head
#' and tail, respectively, and by linear interpolation in the middle. Setting a
#' threshold means that small "wiggling" no longer counts. To use an analogy
#' with ocean waves, smoothing (low-pass filtering) removes the ripples and only
#' leaves the slow roll, while thresholding preserves only waves that are
#' sufficiently high, whatever their period.
#' @param x numeric vector with or without NAs
#' @param thres minimum vertical distance between two extrema for them to count
#'   as two independent inflections
#' @param plot if TRUE, produces a simple plot
#' @return Returns a vector of indices giving the location of inflections.
#' @export
#' @examples
#' x = sin(2 * pi * (1:100) / 15) * seq(1, 5, length.out = 100)
#' idx_na = c(1:4, 6, 7, 14, 25, 30:36, 39, 40, 42, 45:50,
#'            57, 59, 62, 66, 71:79, 98)
#' x[idx_na] = NA
#' soundgen:::findInflections(x, plot = TRUE)
#' soundgen:::findInflections(x, thres = 5, plot = TRUE)
#'
#' for (i in 1:10) {
#'   temp = soundgen:::getRandomWalk(len = runif(1, 10, 100), rw_range = 10,
#'                                   rw_smoothing = runif(1, 0, 1))
#'   soundgen:::findInflections(temp, thres = 1, plot = TRUE)
#'   invisible(readline(prompt="Press [enter] to continue"))
#' }
findInflections = function(x, thres = NULL, plot = FALSE) {
  # remove leading/trailing NAs
  orig = x  # for plotting
  r = rle(is.na(x))
  if (r$values[1]) {
    x = zoo::na.trim(x)
    shift = r$lengths[1]
  } else {
    shift = 0
  }

  len = length(x)
  if (len < 3) return(numeric(0))
  if (any(is.na(x))) {
    xInt = approx(x, n = len, na.rm = TRUE)$y  # soundgen:::intplPitch(x)
  } else {
    xInt = x
  }
  extrema = which(diff(diff(xInt) > 0) != 0) + 1

  # threshold
  if (!is.null(thres) && is.finite(thres) && length(extrema) > 0) {
    de = abs(diff(c(xInt[1], xInt[extrema])))
    idx_keep = which(de > thres)
    extrema = extrema[idx_keep]

    # now get rid of "staircase effects" - successive extrema in the same direction
    de = c(xInt[1], xInt[extrema], xInt[len])
    extrema = extrema[which(diff(diff(de) > 0) != 0)]
  }
  extrema = extrema + shift

  if (plot) {
    plot(orig, type = 'b')
    points(c(rep(NA, shift), xInt), type = 'b', col = 'gray70', pch = 4, xlab = '', ylab = '')
    points(extrema, orig[extrema], col = 'blue', pch = 18)
  }
  return(extrema)
}


#' Pitch descriptives
#'
#' Provides common descriptives of time series such as pitch contours, including
#' measures of average / range / variability / slope / inflections etc. Several
#' degrees of smoothing can be applied consecutively.
#' @param x input: numeric vector, a list of time stamps and values in rows, a
#'   dataframe with one row per file and time/pitch values stored as characters
#'   (as exported by \code{\link{pitch_app}}), or path to csv file containing
#'   the output of \code{\link{pitch_app}} or \code{\link{analyze}}
#' @param step distance between values in s (only needed if input is a vector)
#' @param smoothBW a vector of bandwidths (Hz) for consecutive smoothing of
#'   input using \code{\link{pitchSmoothPraat}}
#' @param inflThres minimum difference (in semitones) between consecutive
#'   extrema to consider them inflections; to apply a different threshold at
#'   each smoothing level, provide \code{inflThres} as a vector of the same
#'   length as \code{smoothBW}
#' @param timeUnit specify whether the time stamps (if any) are in ms or s
#' @param plot if TRUE, plots the inflections for manual verification
#' @return Returns a dataframe with columns containing summaries of one or
#'   multiple inputs (one input per row). The descriptives are as follows:
#'   \describe{\item{duration}{total duration, s} \item{durDefined}{duration
#'   after omitting leading and trailing NAs} \item{propDefined}{proportion of
#'   input with non-NA value, eg proportion of voiced frames if the input is
#'   pitch} \item{start, end}{the first and last values} \item{mean, median,
#'   max, min}{average and extreme values} \item{time_max, time_min}{the
#'   location of minimum and maximum relative to durDefined, 0 to 1}
#'   \item{range, range_sem, sd, sd_sem}{range and standard deviation on the
#'   original scale and in semitones} \item{CV}{coefficient of variation =
#'   mean/sd (provided for historical reasons)} \item{meanSlope,
#'   meanSlope_sem}{mean slope in Hz/s or semitones/s (NB: does not depend on
#'   duration or missing values)} \item{meanAbsSlope, meanAbsSlope_sem}{mean
#'   absolute slope (modulus, ie rising and falling sections no longer cancel
#'   out)} \item{maxAbsSlope, maxAbsSlope_sem}{the steepest slope}}
#' @export
#' @examples
#' x = 5 + sin(2 * pi * (1:100) / 15) * seq(1, 5, length.out = 100)
#' idx_na = c(1:4, 6, 7, 14, 25, 30:36, 39, 40, 42, 45:50,
#'            57, 59, 62, 66, 71:79, 98)
#' x[idx_na] = NA
#' plot(x, type = 'b')
#' pd = pitchDescriptives(x, step = .025, smoothBW = c(NA, 25, 2),
#'                        inflThres = c(NA, 2, 2), plot = TRUE)
#' pd
#'
#' \dontrun{
#' # a single file
#' data(sheep, package = 'seewave')
#' a = analyze(sheep)
#' pd1 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         timeUnit = 'ms', inflThres = NULL, plot = TRUE)
#' pd2 = pitchDescriptives(a$detailed[, c('time', 'pitch')],
#'                         timeUnit = 'ms', inflThres = c(0.1, .5), plot = TRUE)
#'
#' # multiple files returned by pitch_app()
#' pd = pitchDescriptives(
#'   '~/Downloads/pitch_manual_1708.csv',
#'   timeUnit = 'ms', smoothBW = c(NA, 2), inflThres = .25)
#'
#' # a single file, exported from Praat
#' pd = pitchDescriptives(
#'   '~/Downloads/F-Hin-Om_jana.wav_F0contour.txt',
#'   timeUnit = 's', smoothBW = c(NA, 2), inflThres = .25, plot = TRUE)
#' }
pitchDescriptives = function(x,
                             step = NULL,
                             smoothBW = c(NA, 25, 2),
                             inflThres = .25,
                             timeUnit = c('s', 'ms')[1],
                             plot = FALSE) {
  if (is.character(x)) {
    orig = x
    if (file.exists(x)) {
      x = try(read.csv(x), silent = TRUE)
      if (class(x) == 'try-error')
        stop('Input not recognized')
    }
  }
  if (is.list(x) && !is.null(x$time) && !is.null(x$pitch)) {
    if (is.numeric(x$time)) {
      data = suppressWarnings(list(list(file = basename(orig),
                                        time = x$time,
                                        pitch = as.numeric(x$pitch))))
    } else if (is.character(x$time)) {
      nr = nrow(x)
      data = vector('list', nr)
      for (i in 1:nr) {
        data[[i]] = try(list(
          file = x$file[i],
          time = suppressWarnings(as.numeric(unlist(strsplit(x$time[i], ',')))),
          pitch = suppressWarnings(as.numeric(unlist(strsplit(x$pitch[i], ','))))
        ), silent = TRUE)
        if (class(data[[i]]) == 'try-error')
          stop('Input not recognized')
      }
    }
  } else if (is.vector(x)) {
    if (length(x) < 2) return(NA)
    if (is.null(step))
      stop('If x is a vector, step must be provided')
    data = list(list(
      pitch = x,
      time = step * (1:length(x) - .5)
    ))
  } else {
    stop('Input not recognized')
  }

  # process one input at a time
  out = NULL
  len_data = length(data)
  time_start = proc.time()
  for (i in 1:len_data) {
    data_i = data[[i]]
    if (timeUnit == 'ms') data_i$time = data_i$time / 1000
    res = do.call(
      .pitchDescriptives,
      list(time = data_i$time,
           pitch = data_i$pitch,
           smoothBW = smoothBW,
           inflThres = inflThres,
           plot = plot)
    )
    if (!is.null(data_i$file)) res$file = data_i$file
    if (is.null(out)) {
      out = res
    } else {
      out = rbind(out, res)
    }
    reportTime(i = i, nIter = len_data, time_start = time_start)
    if (plot && i < len_data)
      invisible(readline(prompt="Press [enter] to continue"))
  }
  return(out)
}


#' Pitch descriptives per file
#'
#' Internal soundgen function.
#'
#' @param time a vector of time stamps in s
#' @param pitch a vector of pitch values in Hz
#' @inheritParams pitchDescriptives
#' @keywords internal
.pitchDescriptives = function(time,
                              pitch,
                              smoothBW = c(25, 2),
                              inflThres = 2,
                              plot = FALSE) {
  len = length(time)
  if (len < 2) return(NA)
  if (length(pitch) != len) stop('time and pitch must have the same length')
  if (!any(!is.na(pitch))) return(NA)
  # plot(time, pitch, type = 'b')

  step = time[2] - time[1]
  samplingRate = 1 / step
  if (length(inflThres) < length(smoothBW))
    inflThres = rep(inflThres[1], length(smoothBW))

  # apply smoothing
  out = data.frame(file = NA)
  for (i in 1:length(smoothBW)) {
    if (is.finite(smoothBW[i])) {
      pitch_sm = pitchSmoothPraat(pitch, bandwidth = smoothBW[i],
                                  samplingRate = samplingRate)
    } else {
      pitch_sm = pitch
    }
    # plot(time, pitch_sm, type = 'b')
    out_i = timeSeriesSummary(pitch_sm, step = step,
                              inflThres = inflThres[i], plot = plot)
    if (is.finite(smoothBW[i])) {
      if (i == 1) {
        colnames(out_i)[4:ncol(out_i)] = paste0(
          colnames(out_i)[4:ncol(out_i)], '_', smoothBW[i]
        )
      } else {
        # only save durDefined and propDefined once
        out_i = out_i[, 4:ncol(out_i)]
        colnames(out_i) = paste0(colnames(out_i), '_', smoothBW[i])
      }
    }
    out = cbind(out, out_i)
  }
  return(out)
}


#' Time series summary
#'
#' Internal soundgen function
#'
#' A helper function called by .pitchDescriptives for each smoothing level.
#' @param x numeric vector
#' @param step time step in s
#' @inheritParams pitchDescriptives
#' @keywords internal
timeSeriesSummary = function(x, step, inflThres = NULL, plot = FALSE) {
  len = length(x)
  out = data.frame(duration = step * (len + 1))
  vars = c('durDefined', 'propDefined',
           'start', 'start_oct', 'end', 'end_oct',
           'mean', 'mean_oct', 'median', 'median_oct',
           'min', 'min_oct', 'time_min',
           'max', 'max_oct', 'time_max',
           'range', 'range_sem', 'sd', 'sd_sem', 'CV',
           'meanSlope', 'meanSlope_sem', 'meanAbsSlope', 'meanAbsSlope_sem',
           'maxAbsSlope', 'maxAbsSlope_sem')
  out[, vars] = NA

  # transform to semitones, drop NAs
  x_noNA = as.numeric(na.omit(x))
  x_sem = soundgen::HzToSemitones(x)
  x_sem_noNA = as.numeric(na.omit(x_sem))
  ran_not_NA = range(which(!is.na(x)))
  if (length(x_noNA) < 1) return(out)

  # basic descriptives
  out$durDefined = (diff(ran_not_NA) + 1) * step  # from first to last non-NA frame
  out$propDefined = sum(!is.na(x)) / len * 100
  out$start = x_noNA[1]
  out$end = tail(x_noNA, n = 1)
  out$mean = mean(x_noNA)
  out$median = median(x_noNA)

  # min, max
  idx_min = which.min(x)
  if (length(idx_min) > 0) {
    out$min = x[idx_min]
    out$time_min = (idx_min - ran_not_NA[1]) / diff(ran_not_NA)
  }
  idx_max = which.max(x)
  if (length(idx_max) > 0) {
    out$max = x[idx_max]
    out$time_max = (idx_max - ran_not_NA[1]) / diff(ran_not_NA)
  }

  # convert mean-related vars from Hz to octaves above C0
  vars_to_oct = c('start', 'end', 'mean', 'median', 'min', 'max')
  for (v in vars_to_oct) {
    out[, paste0(v, '_oct')] = soundgen::HzToSemitones(out[, v], ref = 16.3516) / 12
  }

  # range, sd
  out$range = out$max - out$min
  out$range_sem = diff(range(x_sem_noNA))
  out$sd = sd(x_noNA)
  out$sd_sem = sd(x_sem_noNA)
  out$CV = out$sd / out$mean

  # slope
  pd = as.numeric(na.omit(diff(x))) / step
  pd_abs = abs(pd)
  pd_sem = as.numeric(na.omit(diff(x_sem))) / step
  pd_sem_abs = abs(pd_sem)

  if (length(pd) > 0) {
    out$meanSlope = mean(pd)
    out$meanSlope_sem = mean(pd_sem)
    out$meanAbsSlope = mean(pd_abs)
    out$meanAbsSlope_sem = mean(pd_sem_abs)
    out$maxAbsSlope = max(pd_abs)
    out$maxAbsSlope_sem = max(pd_sem_abs)
    # note: sumvar incorrectly ignores NAs b/c it divides by dur instead of step
  }

  # inflections
  infl = findInflections(x_sem, thres = inflThres, plot = plot)
  n_inflections = length(infl)
  out$inflex = n_inflections / out$durDefined
  return(out)
}
