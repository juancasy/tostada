#' s_sep
#'
#' \code{s_sep} calculates the small frequency separation
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc, and
#' calculates the small spacing defined as
#'
#'     ss = nu_b - nu_a
#' where
#'     nu_b = nu_{n,l}; nu_a = nu_{n-1,l+2}
#'
#' Note that the provided L is the lowest, so the dataframe must contain the
#' frequencies corresponding to L+2 modes.
#'
#' @param model.df DataFrame
#' @param L Scalar. Lowest degree for which the large separation is calculated
#' @return A vector containing the large separation for the given L
#'
#'
#' @examples
#' \dontrun{
#'  my_ss <- l_sep(df_oscillations, 0) #This calculates the ls for radial modes
#' }
#' \dontrun{
#'   s_sep(df_olscillations, 0)
#' }
#'
#' @seealso \code{\link{l_sep}}



s_sep <- function(model.df, L){
  L <- as.numeric(L)
  nu_b <- model.df$sig[which(model.df$l == L)]
  nu_a <- model.df$sig[which(model.df$l == (L + 2))]
  nu_b <- nu_b[2:length(nu_b)];nu_a <- nu_a[-length(nu_a)]
  ss <- nu_b - nu_a
}
