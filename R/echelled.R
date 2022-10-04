#' echelled
#'
#' \code{echelled} computes echelle diagram from observed oscillation spectrum
#'
#' This function takes two arguments, a dataframe containing observed
#' frequencies and the periodicity with which the frequencies are to be scaled
#' as
#'          frequency (modulo) periodicity
#'
#'
#'
#' @param df_ospec Dataframe. It must contain at least a column (after
#' the name of sig) that contains the oscillation frequencies
#'
#' @param per Numeric. The value of the periodic pattern with which the
#' frequencies will be sliced.
#'
#' @return Dataframe. The input data frame with an additional column with the
#' frequencies scaled by per:
#'         sig: (double float) oscillation frequency (frequency units)
#'         ech: (double float) sig modulo per (frequency units)
#'
#'
#' @seealso \code{\link{read_adipls}}, \code{\link{read_losc}},
#'     \code{\link{read_graco}}, and \code{\link{read_gyre}}
#'
#'
echelled <- function(df_ospec = NULL,
                     per = NULL,
                     collapse = F) {

  if (!any(names(df_ospec) == "sig")) stop("At least 'sig' data must be present")
  if (!any(names(df_ospec) == "amp")) df_ospec$amp <- 1

  df_ech <- df_ospec %>%
    mutate(ech = sig%%per) %>%  # compute the echelle and add to original df
    dplyr::select(sig, amp, ech) # select only sig, amp and ech

  if (collapse) {
    # First we need to find the total number of points and the index (pixel) at
    # which the spectrum fold with the Dnu value
    np <- length(df_ech$sig)  # Total number of points
    #idx <- length(which(df_ech$sig == df_ech$ech)) # Index of last point of cycle
    #idx <- which(abs(per - df_ech$ech) < 1)[1]
    idx <- which(diff(df_ech$ech) < 0)[1]

    # In order to sum up all the amplitudes for each pixel, we generate a factor
    # per pixel with gl function
    df_ech$ind <- gl(idx, 1, np)

    # Using group_by each factor (pixel) is treated simultaneously to sum up
    # all the amplitudes corresponding to this factor.
    df_ech <- df_ech %>%
      mutate(ind = gl(idx, 1, np)) %>% # Indexation of cycles in the dataframe
      group_by(ind) %>%                # grouping by index
      mutate(samp = sum(amp))          # for each group sum all the amp

    # Since we are only interested in the sum of amplitudes, we just select
    # the first cycle from 1 to idx, which contains the ech and sum(amp),i.e.
    # the collapsed ED.
    df_ech <- df_ech[1:idx, c("sig", "ech","samp")]
  }
  return(df_ech)
}

