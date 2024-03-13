#' l_sep
#'
#' \code{l_sep} calculates the large frequency separation
#'
#' This function takes one argument, a dataframe with the synthetic oscillation
#' spectra read from read_graco, read_adipls, read_gyre, or read_losc, and
#' calculates the large spacing defined as the vector
#'    dnu = nu_b - nu_a
#' where
#'    nu_b = nu_{n,l}; nu_a = nu_{n-1,l}
#' Single value result is calculated as
#'    l_sep = median(dnu)
#' When all L are considered then
#'    l_sep = mean(median(dnu,L=0), median(dnu,L=1), ...)
#'
#'
#' @param model.df DataFrame
#' @param L Scalar. Mode degree for which the large separation is calculated
#' @param n_inf Scalar. Minimum radial order (n) considered. If NULL (default)
#' minimum is calculated using tostada::nrange function
#' @param n_sup Scalar. Maximum radial order (n) considered. If NULL (default)
#' maximum is calculated using tostada::nrange function
#' @param vector Logical. Indicates whether user want a single value or the list
#' of spacings for the indicated L.
#' @return Large separation calculated from frequencies for the chosen L value.
#' If L is not provided then l_sep returns the median value from all the Dnu(L)
#' When n range is not specified, l_sep uses tostada::n_range function to
#' obtain the minimum and maximum n (common to all L values). Otherwise the
#' Dnu value is calculated within the range (n_min, n_max).
#' When vector = T l_sep gives a vector with
#'  - If no L specified: [Dnu(L=0), Dnu(L=1), ...(Dnu(L=Lmax)]
#'  - If L specified: [dnu_n2-n1, dnu_n3-n2, dnu_n4-n3,...dnu_n-(n-1)]
#'
#'
#' @examples
#' \dontrun{
#'    my_ls <- l_sep(df_oscillations, L=0, n_inf = 2, n_sup =10) # This
#'    #calculates the median of the ls for radial modes
#'    }
#' \dontrun{
#'   my_ls <- l_sep(df_oscillations, L=0, vector = T) # This
#'    #calculates the ls for radial modes, and gives a vector with values.
#'   }
#'
#' @seealso \code{\link{s_sep}}

l_sep <- function(model.df,
                  L = NULL,
                  n_inf = NULL,
                  n_sup = NULL,
                  vector = FALSE
                  ) {
  l_values <- unique(model.df$l)            # get all L values present in model
  nrange <- tostada::n_range(model.df)      # get all n common range for all L
  with_m <- ("m" %in% colnames(model.df))   # detect if model has m values

  # If nrange is not furnished then we use common nrange for all L
  ifelse(is.null(n_inf), ninf <- nrange[1], ninf <- n_inf)
  ifelse(is.null(n_sup), nsup <- nrange[2], nsup <- n_sup)

  # Case of no m values in model, LS is computed assuming m(L<>0) = 0
  ifelse(with_m,
         {
           if (is.null(L)) {
             dnu_list <- sapply(l_values, function(x){
               median(
                 diff(
                   model.df$sig[model.df$l == x &
                                  model.df$n >= ninf &
                                  model.df$n <= nsup &
                                  model.df$m == 0]))
             })
             dnu_value <- median(dnu_list)
           } else{
             nu_list <- model.df$sig[model.df$l == L &
                                       model.df$n >= ninf &
                                       model.df$n <= nsup &
                                       model.df$m == 0]
             dnu_list <- diff(nu_list)
             dnu_value <- median(dnu_list)
           }
         },
         {
           if (is.null(L)) {
             dnu_list <- sapply(l_values, function(x){
               median(
                 diff(
                   model.df$sig[model.df$l == x &
                                  model.df$n >= ninf &
                                  model.df$n <= nsup]))
             })
             dnu_value <- median(dnu_list)
           } else{
             nu_list <- model.df$sig[model.df$l == L &
                                       model.df$n >= ninf &
                                       model.df$n <= nsup]
             dnu_list <- diff(nu_list)
             dnu_value <- median(dnu_list)
           }
         }
  )
# return result as a vector or as single value
  ifelse(vector,
         return(dnu_list),
         return(mean(dnu_value))
         )
}

