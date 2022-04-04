#' read_gyre
#'
#' \code{read_gyre} reads GYRE oscillation files
#'
#' This function takes one argument, the GYRE file (.txt) to be read with full
#' path. It stores the result in an R dataframe.
#'
#' \usage{
#'   read_gyrer(filename, header = F)
#' }
#'
#' @param filename String, scalar. GYRE oscillation file.
#' @param header Logic. If TRUE then header of file is read (default F).
#' @return A column-named dataframe containing the following data
#'         l: the mode degree
#'         npg: the mode radial node
#'         np: number of nodes as P mode
#'         ng: onumber of nodes as G mode
#'         sig: frequency in microHz
#'         sigi: imaginary part of the frequency
#'
#'
#'
#' @examples
#' myfreqs <- read_gyre('/myhome/gyrefile.txt')
#'
#' \dontrun{
#'   read_gyre('/myhome/gyrefile.txt')
#' }


read_gyre <- function(filename = NULL,
                      header = FALSE) {
  # Reading the data
  if (header) {
    no_skip <- 6
    #To be used when GYRE files contains actual header
    #header <- scan(filename, what = character(), nlines = 7, quiet = T)
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "npg", "np", "ng", "sig", "sigi")
    )

  }
  else {
    no_skip <- 6
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "npg", "np", "ng", "sig", "sigi")
    )
  }
  data$amp <- 1 # This adds the amplitude variable to the dataframe
  return(data)
}
