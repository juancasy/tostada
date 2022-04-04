#' n_maxmin
#'
#' \code{n_maxmin} determines the largest radial order (n) among all the
#'     min(n, L)
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
#' n_min <- n_maxmin(df_oscillations)
#'
#' \dontrun{
#'   n_maxmin(df_olscillations)
#' }


n_maxmin <- function(model.df){
  L <- unique(model.df$l)
  nmin <- sapply(L, function(x)
    min(model.df$n[which(model.df$l == x)]))
  max_nmin <- max(nmin)
}

