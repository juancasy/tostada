#' n_range
#'
#' \code{n_range} determines the common n range for all the L in the
#'     oscillation spectrum as
#'     n = (\code{\link{n_maxmin}}, \code{\link{n_minmax}})
#'
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc.
#'
#'
#' @param model.df DataFrame
#' @param mono logical option (default FALSE) to calculate minimum n value
#' from which the n vector is monotonic (i.e. diff(n)=1).
#' @return It returns a vector with c(n_maxmin, n_minmax)
#'
#'
#'
#' @examples
#' range <- n_nrange(df_oscillations)
#'
#' \dontrun{
#'   n_range(df_olscillations)
#' }
#' @seealso \code{\link{n_maxmin}}, \code{\link{n_minmax}}

n_range <- function(model.df, mono = F) {

  n_high <- n_minmax(model.df)
  ifelse(mono,
         n_low <- n_maxmin(model.df, mono = T),
         n_low <- n_maxmin(model.df)
  )

  nrange <- c(n_low, n_high)
}
