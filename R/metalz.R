#' Metallicity conversor
#'
#' \code{metalz} returns the conversion from FeH to Z and viceversa
#'
#' This function requires at least one argument to work, either the metallicity
#' as Fe/H or as Z. Avoid to modify default values for primordial helium or
#' variation of Y or X with Z (see details in parameter description).
#'
#' \usage{
#'   metalz(metallicity,z = FALSE, ypr = NULL, dydz = NULL, szx = NULL,prec = 8)
#' }
#'
#' @param metallicity Numeric, scalar. Input metallicty value (default Fe/H).
#' @param z Logic, scalar. If TRUE then metallicity is provided as Z (default F).
#' @param ypr Numeric, scalar. Primordial Helium. If not provided,
#'            default is 0.26627.
#' @param szx Numeric, scalar. Solar Z/H value. IF not provided,
#'            default is  0.0245
#' @param dydz Numeric, scalar. Dy/Dz value. IF not provided,
#'             default is 2.2
#'
#' @param prec Numeric, scalar. Required output precision (default input
#'             precision). It indicates the number of decimals in output.
#' @return A numeric vector containing c(X, Y, Z, Fe/H).  If Fe/H is provided
#'         then Z will be returned. Likewise if Z is provided then Fe/H will be
#'         returned.
#'
#' @examples
#' metalz(feh = 0.0)
#' metalz(z = 0.02)
#' metalz(feh = 0.0, ypr = 0.26, prec = 4)
#'
#' \dontrun{
#' metalz(feh = 0.25, z=0.02)
#' }

metalz <- function(metallicity, z = FALSE,
                   ypr = NULL,
                   dydz = NULL,
                   szx = NULL,
                   prec = 8) {

  #
  # Input checks
  #
  cnd <- (metallicity < 0 && z == T)
  try(if (cnd)
    stop("Z values must be >0", call. = FALSE)
  )



  #
  # Initialisations & defaults
  #
  ifelse(is.null(ypr), my_ypr <- 0.235, my_ypr <- ypr)
  ifelse(is.null(dydz), my_dydz <- 2.2, my_dydz <- dydz)
  ifelse(is.null(szx), my_szx <- 0.0245, my_szx <- szx)

  #
  # Computation of Fe/H or Z
  #

  ifelse(
    z == FALSE,
    {feh <- metallicity
    my_z <- (1 - my_ypr) / ((1 / (my_szx*10^feh)) + (my_dydz + 1))
    y <- my_dydz*my_z + my_ypr
    x <- 1 - y - my_z
    },
    {my_z <- metallicity
    feh <- log10((my_z / my_szx)*(1 / ((1 - my_ypr) - my_z*(my_dydz + 1))))
    y <- my_dydz*my_z + my_ypr
    x <- 1 - y - my_z
    }
  )
  return(signif(c(x, y, my_z, feh), digits = prec))
}
