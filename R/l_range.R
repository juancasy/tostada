#' l_range
#'
#' \code{l_range} determines the common L range for all the
#'     oscillation spectrum as
#'     l_range = (L_min, L_max)
#'
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc.
#'
#'
#' @param model.df DataFrame.
#' @return It returns a vector with c(l_min, l_max)
#'
#'
#'
#' @examples
#' range <- l_range(df_oscillations)
#'
#' \dontrun{
#'   l_range(df_oscillations)
#' }
#' @seealso \code{\link{n_range}}

l_range <- function(model.df) {
  l_high <- max(model.df$l)
  l_low <- min(model.df$l)
  lrange <- c(l_low, l_high)
}
