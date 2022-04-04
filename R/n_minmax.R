#' n_minmax
#'
#' \code{n_minmax} determines the smallest radial order (n) among all the
#'     max(n, L)
#'
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc.
#'
#'
#' @param model.df DataFrame.
#' @return It returns a single numeric value
#'
#'
#'
#' @examples
#' n_max <- n_minmax(df_oscillations)
#'
#' \dontrun{
#'   n_minmax(df_olscillations)
#' }


n_minmax <- function(model.df) {
  L <- unique(model.df$l)
  nmax <-sapply(L, function(x)
    max(model.df$n[which(model.df$l == x)])
    )
  min_nmax <- min(nmax)
}
