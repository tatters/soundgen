#' Morph dataframes
#'
#' Internal soundgen function.
#'
#' Morphs two dataframes of anchors, with two columns and at least two rows in
#' each.
#' @param a,b dataframes to morph
#' @param nMorphs length of morphing sequence
#' @param method morphing method. 'smooth' equalizes contour lengths and takes a
#'   weighted mean. 'perAnchor' is a more sophisticated algorithm that attempts
#'   to match individual anchors
#' @param lenSmooth the length of curves generated from anchors prior to
#'   averaging (only applicable if method is 'smooth')
#' @param matchIdx manual override of anchor matching: if you have a better idea
#'   of which anchors should morph into each other, specify
#' @param plot if TRUE, plots the morphing sequence of anchors
#' @param ... other graphical pars passed on to \code{plot}
#' @return A list of length nMorphs containing anchor dataframes for morphing
#' @keywords internal
#' @examples
#' a = data.frame(time=c(0, .2, .9, 1), value=c(100, 110, 180, 110))
#' b = data.frame(time=c(0, .3, .5, .8, 1), value=c(300, 220, 190, 400, 350))
#' plot (a, type = 'b', ylim = c(0, 500))
#' points (b, type = 'b', col = 'blue')
#' m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'smooth', plot = TRUE)
#' m = soundgen:::morphDF(a, b, nMorphs = 15, method = 'perAnchor', plot = TRUE)
#'
#' m = soundgen:::morphDF(a = data.frame(time = c(0, 1), freq = c(700, 700)),
#'                        b = data.frame(time = c(0, 1), freq = c(400, 600)),
#'                        nMorphs = 5, method = 'perAnchor', plot = TRUE)
#' m = soundgen:::morphDF(a = data.frame(time = c(-30, 120, 350), value = c(-120, 10, -120)),
#'                        b = data.frame(time = c(50, 500), value = c(0, -30)),
#'                        nMorphs = 10, method = 'perAnchor', plot = TRUE)
#' m = soundgen:::morphDF(a = data.frame(time = c(-50, 1214), value = c(-50, -70)),
#'                        b = data.frame(time = c(0, 49, 256), value = c(-120, 10, -120)),
#'                        nMorphs = 8, method = 'perAnchor', plot = TRUE)
morphDF = function(a,
                   b,
                   nMorphs = 5,
                   method = c('smooth', 'perAnchor')[2],
                   lenSmooth = 50,
                   matchIdx = NULL,
                   plot = F,
                   ...) {
  # example of expected input a & b: data.frame(time=c(0,1), value=c(-30,15))  NB: min 2 rows!!!
  if (identical(a, b)) {
    return(rep(list(a), nMorphs))
  }

  if (class(a)[1] != 'data.frame') a = as.data.frame(a)
  if (class(b)[1] != 'data.frame') b = as.data.frame(b)

  # in case one df is NA or NULL, we copy the other one
  if ((any(is.na(a)) | ncol(a) < 2) & (!any(is.na(b)) & ncol(b) == 2)) {
    a = b
    a[, 2] = 0
  } else if ((any(is.na(b)) | ncol(b) < 2) & (!any(is.na(a)) & ncol(a) == 2)) {
    b = a
    b[, 2] = 0
  } else if ((any(is.na(a)) | ncol(a) < 2) & (any(is.na(b)) | ncol(b) < 2)) {
    return(rep(list(NA), nMorphs))
  }

  mymax_x = max(max(a[, 1]), max(b[, 1]))
  mymin_x = min(min(a[, 1]), min(b[, 1]))
  mymax_y = max(max(a[, 2]), max(b[, 2]))
  mymin_y = min(min(a[, 2]), min(b[, 2]))
  cols = rainbow(n = nMorphs)
  out = list()

  if (method == 'smooth') {
    ## Option 1: upsample to the same number of anchors, then take a weighted mean
    a_up = getSmoothContour(len = lenSmooth, anchors = a) # plot(a_up)
    b_up = getSmoothContour(len = lenSmooth, anchors = b) # plot(b_up)

    idx = seq(0, 1, length.out = nMorphs)
    timeIdx_a = seq(a[1, 1], a[nrow(a), 1], length.out = lenSmooth)
    timeIdx_b = seq(b[1, 1], b[nrow(b), 1], length.out = lenSmooth)
    out[[1]] = data.frame(time = timeIdx_a, value = a_up)
    if (plot)
      plot(
        out[[1]],
        col = cols[1],
        xlim = c(mymin_x, mymax_x),
        ylim = c(mymin_y, mymax_y),
        ...
      )
    for (d in 2:length(idx)) {
      hybrid = a_up * (1 - idx[d]) + b_up * idx[d]
      hybrid_time = timeIdx_a * (1 - idx[d]) + timeIdx_b * idx[d]
      out[[d]] = data.frame(time = hybrid_time, value = hybrid)
      if (plot)
        points(out[[d]], main = idx[d], col = cols[d])
    }
  } else if (method == 'perAnchor') {
    ## Option 2: alternatively, overimpose both curves without upsampling
    # anchors and find the best match for each anchor
    mycopy_a = a
    mycopy_b = b
    swap = FALSE
    if (nrow(a) < nrow(b)) {
      a = mycopy_b
      b = mycopy_a
      swap = TRUE  # we swap a and b to have the longer dataframe first
    }
    a_norm = a
    b_norm = b
    a_norm[, 1] = a_norm[, 1] - mymin_x
    a_norm[, 1] = a_norm[, 1] / (mymax_x - mymin_x)
    a_norm[, 2] = a_norm[, 2] - mymin_y
    a_norm[, 2] = a_norm[, 2] / (mymax_y - mymin_y)
    b_norm[, 1] = b_norm[, 1] - mymin_x
    b_norm[, 1] = b_norm[, 1] / (mymax_x - mymin_x)
    b_norm[, 2] = b_norm[, 2] - mymin_y
    b_norm[, 2] = b_norm[, 2] / (mymax_y - mymin_y)

    # FIND MATCHING ANCHORS
    a_norm$match = NA
    if (nrow(a_norm) > 2) {
      # we only look for closest anchors if there are more than two,
      # otherwise just morph endpoints into endpoints
      for (i in 2:(nrow(a_norm) - 1)) {
        # again, excluding endpoints
        mymatch = apply(matrix(1:nrow(b_norm)), 1, function(x) {
          dist(as.matrix(rbind(b_norm[x, 1:2], a_norm[i, 1:2])))
        }) # the matching anchor is the closest one
        #    (after normalizing both x and y axes)

        if (nrow(b_norm) > 2) {
          # if both sounds have >2 anchors, always match middle anchors
          # to middle anchors, not endpoints
          mymatch = mymatch[2:(length(mymatch) - 1)]
          a_norm$match[i] = which.min(mymatch) + 1
        } else {
          # if one sound has >2 anchors and the other 2 anchors
          a_norm$match[i] = which.min(mymatch)
        }
      }
    }
    # the first and last anchors of sound A always morph into the first and
    # last anchors of sound B
    a_norm$match[1] = 1
    a_norm$match[nrow(a_norm)] = nrow(b_norm)
    # make sure every b anchor is represented
    un = unique(a_norm$match)
    not_matched = which(!rownames(b) %in% un)
    if (!sum(not_matched) == 0) {
      for (i in not_matched) {
        j = which.min(apply(matrix(1:nrow(a_norm)), 1, function(x) {
          dist(as.matrix(rbind(a_norm[x, 1:2], b_norm[i, 1:2])))
        })) # the closest anchor
        a_norm$match[j] = not_matched
      }
    }
    # arrange in a non-decreasing order
    for (i in 1:nrow(a_norm)) {
      if (a_norm$match[i] < max(a_norm$match[1:i])) {
        a_norm$match[i] = max(a_norm$match[1:i])
      }
    }
    a$match = a_norm$match # row number of the matching anchor in /b/
    # END OF ANCHORS MATCHING

    # morph
    idx = seq(0, 1, length.out = nMorphs)
    out[[1]] = a[, 1:2]
    for (d in 1:length(idx)) {
      hybrid = a
      for (i in 1:nrow(hybrid)) {
        # NB: w/o rounding unique() below fails to find duplicates. For more on
        # weird arithmetic, see
        # https://stackoverflow.com/questions/588004/is-floating-point-math-broken
        hybrid[i, 1] = round(hybrid[i, 1] +
                       idx[d] * (b[hybrid$match[i], 1] - hybrid[i, 1]), 4)
        hybrid[i, 2] = round(hybrid[i, 2] +
                       idx[d] * (b[hybrid$match[i], 2] - hybrid[i, 2]), 4)
      }
      # remove duplicate rows
      hybrid = unique(hybrid[, 1:2]) # hybrid[!duplicated(hybrid[, 1:2]), ]
      if (swap) {
        out[[nMorphs - d + 1]] = hybrid[, 1:2]
      } else {
        out[[d]] = hybrid[, 1:2]
      }
      if (plot) {
        if (d == 1) {
          plot(a[, 1:2], col = cols[1], xlim = c(mymin_x, mymax_x),
               ylim = c(mymin_y, mymax_y), type = 'b', ...)
        } else {
          points(hybrid[, 1:2], main = idx[d], col = cols[d], type = 'b')
        }
      }
    }
  }
  return (out)
}



#' Morph formants
#'
#' Internal soundgen function.
#' @param f1,f2 dataframes specifying one formant of the two target sounds
#'   (different numbers of rows are ok)
#' @param nMorphs length of morphing sequence
#' @keywords internal
morphFormants = function(f1, f2, nMorphs = 5) {
  if (is.list(f1)) f1 = as.data.frame(f1)
  if (is.list(f2)) f2 = as.data.frame(f2)
  # for compatibility with morphDF(), make sure we have at least two anchors
  if (nrow(f1) == 1) {
    f1 = rbind(f1, f1)
    f1$time[2] = 1
  }
  if (nrow(f2) == 1) {
    f2 = rbind(f2, f2)
    f2$time[2] = 1
  }
  h = morphDF(f1[, c(1, 2)], f2[, c(1, 2)], nMorphs = nMorphs)
  h_amp = morphDF(f1[, c(1, 3)], f2[, c(1, 3)], nMorphs = nMorphs)
  h_width = morphDF(f1[, c(1, 4)], f2[, c(1, 4)], nMorphs = nMorphs)
  for (i in 1:length(h)) {
    h[[i]]$amp = h_amp[[i]]$amp
    h[[i]]$width = h_width[[i]]$width
  }
  return (h)
}

#' Morph lists
#'
#' Internal soundgen function.
#' @param l1,l2 lists of formants (various lengths are ok)
#' @param nMorphs length of morphing sequence
#' @return A list of length nMorphs.
#' @keywords internal
#' @examples
#' l1 = list(f1 = data.frame(time = c(0, .5, 1),
#'                           freq = c(700, 900, 1200),
#'                           amp = c(30), width = c(80)),
#'           f2 = data.frame(time = c(0),
#'                           freq = c(900),
#'                           amp = c(30),
#'                           width = c(120)),
#'           f3 = data.frame(time = c(0),
#'                           freq = c(1500),
#'                           amp = c(20),
#'                           width = c(150)))
#' l2 = list(f1 = data.frame(time = c(0),
#'                           freq = c(400),
#'                           amp = c(40),
#'                           width = c(120)),
#'           f2 = data.frame(time = c(0, 1),
#'                           freq = c(1500, 2000),
#'                           amp = c(30),
#'                           width = c(150)))
#' ml = soundgen:::morphList(l1, l2, 4)
morphList = function(l1, l2, nMorphs = 5) {
  # make sure both exactFormants lists contain equal numbers of formants
  while (length(l1) > length(l2)) {
    l2[[length(l2) + 1]] = l1[[length(l2) + 1]]
    names(l2)[length(l2)] = names(l1)[length(l1)]
    l2[[length(l2)]]$amp = 0  # add a silent formant
  }
  while (length(l2) > length(l1)) {
    l1[[length(l1) + 1]] = l2[[length(l1) + 1]]
    names(l1)[length(l1)] = names(l2)[length(l1)]
    l1[[length(l1)]]$amp = 0  # add a silent formant
  }
  nFormants = length(l1)

  # morph one formant at a time
  out = rep(list(l1), nMorphs)
  for (f in 1:nFormants) {
    temp = morphFormants(f1 = l1[[f]], f2 = l2[[f]], nMorphs)
    # a convoluted way of saving the output of morphFormants()
    # in appropriate slots in the output list
    for (i in 1:nMorphs) {
      out[[i]] [[f]] = temp[[i]]
    }
  }
  return (out)
}
