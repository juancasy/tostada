#' l_sep
#'
#' \code{l_sep} calculates the large frequency separation
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc, and
#' calculates the large spacing defined as
#'    ls = nu_b - nu_a
#' where
#'    nu_b = nu_{n,l}; nu_a = nu_{n-1,l}
#'
#'
#' @param model.df DataFrame
#' @param L Scalar. Mode degree for which the large separation is calculated
#' @return A vector containing the large separation for the given L
#'
#'
#' @examples
#' my_ls <- l_sep(df_oscillations, 0) This calculates the ls for radial modes
#'
#' \dontrun{
#'   l_sep(df_olscillations 0)
#' }
#'
#' @seealso \code{\link{s_sep}}

l_sep <- function(model.df, L) {
  nu <- model.df$sig[which(model.df$l == L)]
  ls <- diff(nu)
}

