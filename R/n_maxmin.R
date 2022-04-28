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
#' @param model.df dataframe.
#' @param mono logical option (default FALSE) to calculate minimum n value
#' from which the n vector is monotonic (i.e. diff(n)=1).
#'
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


n_maxmin <- function(model.df, mono = F){
  L <- unique(model.df$l)
  ifelse(mono,
         nmin <- sapply(L, function(x){
           nn <- model.df$n[which(model.df$l == x)]
           ifelse(any(diff(nn)>1),
                  min_n <- nn[max(which(diff(nn) > 1) + 1)],
                  min_n <- min(nn))
           return(min_n)
         }),
         nmin <- sapply(L, function(x)
           min(model.df$n[which(model.df$l == x)]))
         )

  max_nmin <- max(nmin)
}

