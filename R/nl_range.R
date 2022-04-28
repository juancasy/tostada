#' nl_range
#'
#' \code{l_range} determines the common L range for all the
#'     oscillation spectrum as
#'     nl_range = (n_min, n_max) & (L_min, L_max)
#'
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc.
#'
#'
#' @param model.df dataframe
#' @param mono logical option (default FALSE) to calculate minimum n value
#' from which the n vector is monotonic (i.e. diff(n)=1).
#'
#' @return It returns a vector with c(l_min, l_max, n_min, n_max)
#'
#'
#'
#' @examples
#' range <- nl_range(df_oscillations)
#'
#' \dontrun{
#'   nl_range(df_oscillations)
#' }
#' @seealso \code{\link{n_range}}, \code{\link{l_range}}


nl_range <- function(model.df, mono = F) {
  ifelse(mono,
         nr <- n_range(model.df, mono = T),
         nr <- n_range(model.df)
         )
  lr <- l_range(model.df)
  nlrange <- c(lr[1], lr[2], nr[1], nr[2])
}
