## OPTIMIZE PARAMETERS FOR ACOUSTIC ANALYSIS ##

#' Optimize parameters for acoustic analysis
#'
#' This customized wrapper for \code{\link[stats]{optim}} attempts to optimize the
#' parameters of \code{\link{segmentFolder}} or \code{\link{analyzeFolder}} by
#' comparing the results with a manually annotated "key". This optimization
#' function uses a single measurement per audio file (e.g., median pitch or the
#' number of syllables). For other purposes, you may want to adapt the
#' optimization function so that the key specifies the exact timing of
#' syllables, their median length, frame-by-frame pitch values, or any other
#' characteristic that you want to optimize for. The general idea remains the
#' same, however: we want to tune function parameters to fit our type of audio
#' and research priorities. The default settings of \code{\link{segmentFolder}}
#' and \code{\link{analyzeFolder}} have been optimized for human non-linguistic
#' vocalizations.
#'
#' If your sounds are very different from human non-linguistic vocalizations,
#' you may want to change the default values of other arguments to speed up
#' convergence. Adapt the code to enforce suitable constraints, depending
#' on your data.
#' @param myfolder path to where the .wav files live
#' @param myfun the function being optimized: either 'segmentFolder' or
#'   'analyzeFolder' (in quotes)
#' @param key a vector containing the "correct" measurement that we are aiming
#'   to reproduce
#' @param pars names of arguments to \code{myfun} that should be
#'   optimized
#' @param bounds a list setting the lower and upper boundaries for possible
#'   values of optimized parameters. For ex., if we optimize \code{smooth}
#'   and \code{smoothOverlap}, reasonable bounds might be list(low = c(5,
#'   0), high = c(500, 95))
#' @param fitnessPar the name of output variable that we are comparing with the
#'   key, e.g. 'nBursts' or 'pitch_median'
#' @param fitnessFun the function used to evaluate how well the output of
#'   \code{myfun} fits the key. Defaults to 1 - Pearson's correlation (i.e. 0 is
#'   perfect fit, 1 is awful fit). For pitch, log scale is more meaningful, so a
#'   good fitness criterion is "function(x) 1 - cor(log(x), log(key), use =
#'   'pairwise.complete.obs')"
#' @param nIter repeat the optimization several times to check convergence
#' @param init initial values of optimized parameters (if NULL, the default
#'   values are taken from the definition of \code{myfun})
#' @param initSD each optimization begins with a random seed, and
#'   \code{initSD} specifies the SD of normal distribution used to generate
#'   random deviation of initial values from the defaults
#' @param control a list of control parameters passed on to
#'   \code{\link[stats]{optim}}. The method used is "Nelder-Mead"
#' @param otherPars a list of additional arguments to \code{myfun}
#' @param mygrid a dataframe with one column per parameter to optimize, with
#'   each row specifying the values to try. If not NULL, \code{optimizePars}
#'   simply evaluates each combination of parameter values, without calling
#'   \code{\link[stats]{optim}} (see examples)
#' @param verbose if TRUE, reports the values of parameters evaluated and fitness
#' @return Returns a matrix with one row per iteration with fitness in the first
#'   column and the best values of each of the optimized parameters in the
#'   remaining columns.
#' @export
#' @examples
#' \dontrun{
#' # Download 260 sounds from the supplements in Anikin & Persson (2017)
#' # - see http://cogsci.se/publications.html
#' # Unzip them into a folder, say '~/Downloads/temp'
#' myfolder = '~/Downloads/temp'  # 260 .wav files live here
#'
#' # Optimization of SEGMENTATION
#' # Import manual counts of syllables in 260 sounds from
#' # Anikin & Persson (2017) (our "key")
#' key = segmentManual  # a vector of 260 integers
#'
#' # Run optimization loop several times with random initial values
#' # to check convergence
#' # NB: with 260 sounds and default settings, this might take ~20 min per iteration!
#' res = optimizePars(myfolder = myfolder, myfun = 'segmentFolder', key = key,
#'   pars = c('shortestSyl', 'shortestPause', 'sylThres'),
#'   fitnessPar = 'nBursts',
#'   nIter = 3, control = list(maxit = 50, reltol = .01, trace = 0))
#'
#' # Examine the results
#' print(res)
#' for (c in 2:ncol(res)) {
#'   plot(res[, c], res[, 1], main = colnames(res)[c])
#' }
#' pars = as.list(res[1, 2:ncol(res)])  # top candidate (best pars)
#' s = do.call(segmentFolder, c(myfolder, pars))  # segment with best pars
#' cor(key, as.numeric(s[, fitnessPar]))
#' boxplot(as.numeric(s[, fitnessPar]) ~ as.integer(key), xlab='key')
#' abline(a=0, b=1, col='red')
#'
#' # Try a grid with particular parameter values instead of formal optimization
#' res = optimizePars(myfolder = myfolder, myfun = 'segmentFolder', key = segment_manual,
#'   pars = c('shortestSyl', 'shortestPause'),
#'   fitnessPar = 'nBursts',
#'   mygrid = expand.grid(shortestSyl = c(30, 40),
#'                        shortestPause = c(30, 40, 50)))
#' 1 - res$fit  # correlations with key
#'
#' # Optimization of PITCH TRACKING (takes several hours!)
#' res = optimizePars(myfolder = myfolder,
#'                    myfun = 'analyzeFolder',
#'                    key = log(pitchManual),  # log-scale better for pitch
#'                    pars = c('specThres', 'specSmooth'),
#'                    bounds = list(low = c(0, 0), high = c(1, Inf)),
#'                    fitnessPar = 'pitch_median',
#'                    nIter = 2,
#'                    otherPars = list(plot = FALSE, verbose = FALSE, step = 50,
#'                                     pitchMethods = 'spec'),
#'                    fitnessFun = function(x) {
#'                      1 - cor(log(x), key, use = 'pairwise.complete.obs') *
#'                        (1 - mean(is.na(x) & !is.na(key)))  # penalize failing to detect F0
#'                      })
#' }
optimizePars = function(myfolder,
                        key,
                        myfun,
                        pars,
                        bounds = NULL,
                        fitnessPar,
                        fitnessFun = function(x) 1 - cor(x, key, use = 'pairwise.complete.obs'),
                        nIter = 10,
                        init = NULL,
                        initSD = .2,
                        control = list(maxit = 50, reltol = .01, trace = 0),
                        otherPars = list(plot = FALSE, verbose = FALSE),
                        mygrid = NULL,
                        verbose = TRUE) {
  if (is.null(bounds)) {
    bounds = list(low = rep(-Inf, length(pars)),
                  high = rep(Inf, length(pars)))
  }
  defaults = as.list(args(get(myfun)))

  ## Option 1: grid optimization (just evaluate the fitness for each combination of pars)
  if (!is.null(mygrid)) {
    if (!identical(colnames(mygrid)[1:length(pars)], pars)) {
      stop('mygrid should be either NULL or a dataframe with one column per parameter')
    }
    mygrid$fit = NA
    for (i in 1:nrow(mygrid)) {
      mygrid$fit[i] = evaluatePars(p = as.numeric(mygrid[i, 1:length(pars)]),
                                   pars = pars,
                                   myfun  = myfun,
                                   key = key,
                                   fitnessPar = fitnessPar,
                                   fitnessFun = fitnessFun,
                                   myfolder = myfolder,
                                   otherPars = otherPars,
                                   verbose = verbose)
    }
    return(mygrid)
  }

  ## Option 2: use optim() to find the best values of pars
  if (is.null(init)) {
    pars_defaults = defaults[names(defaults) %in% pars]
  } else {
    pars_defaults = init
  }
  optimal_pars = list()
  time_start = proc.time()

  for (i in 1:nIter) {
    # start with randomly wiggled default pars
    p_init = rnorm_truncated(
      length(pars_defaults),
      mean = as.numeric(unlist(pars_defaults)),
      sd = as.numeric(unlist(pars_defaults)) * initSD,
      low = bounds$low, high = bounds$high
    )
    # run Nelder-Mead optimization (other methods don't work)
    myOptim = optim(
      par = p_init,
      fn = evaluatePars,
      myfun = myfun,
      pars = pars,
      bounds = bounds,
      fitnessPar = fitnessPar,
      fitnessFun = fitnessFun,
      otherPars = otherPars,
      myfolder = myfolder,
      key = key,
      method = 'Nelder-Mead',
      control = control,
      verbose = verbose
    )
    my_r = 1 - myOptim$value # the best achievable correlation with these predictors
    my_pars = myOptim$par # optimal pars
    optimal_pars[[i]] = c(my_r, my_pars)
    reportTime(i = i, nIter = nIter, time_start = time_start)
  }
  res = as.data.frame(sapply(optimal_pars, cbind))
  rownames(res) = c('r', pars)
  res = t(res)
  res = res[order(res[, 1], decreasing = TRUE),]
  return (res)
}


#' Evaluate parameters for optimization
#'
#' Internal soundgen function.
#'
#' Called by \code{\link{optimizePars}}.
#' @param p numeric vector of evaluated values of parameters
#' @inheritParams optimizePars
#' @return Returns 1 - Pearson's correlation between fitness measure and the key
#'   (i.e. 0 is perfect fit, 1 is awful fit).
#' @keywords internal
evaluatePars = function(p,
                        pars,
                        myfun,
                        bounds = NULL,
                        fitnessPar,
                        fitnessFun = function(x) 1 - cor(x, key, use = 'pairwise.complete.obs'),
                        myfolder,
                        key,
                        otherPars = list(plot = FALSE, verbose = FALSE),
                        verbose = TRUE) {
  # if the pars go beyond the bounds, don't even evaluate
  if (sum(p < bounds$low) > 0 |
      sum(p > bounds$high) > 0) {
    return(1)
  }
  params = as.list(p)
  names(params) = pars
  s = try(do.call(myfun, c(params, myfolder = myfolder, otherPars)))
  if (class(s)[1] == 'try-error') {
    stop('Error in myfun')
  } else {
    trial = as.numeric(s[, fitnessPar])
    out = fitnessFun(trial)
    if (verbose) {
      print(paste0('Tried pars ', paste(round(p, 3), collapse = ', '), '; fit = ', round(out, 4)))
    }
    return (out)
  }
}
