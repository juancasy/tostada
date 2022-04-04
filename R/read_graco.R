#' read_graco
#'
#' \code{read_graco} reads GraCo oscillation files
#'
#' This function takes one argument, the GraCo file (.dat) to be read with full
#' path. It stores the result in an R dataframe.
#'
#'
#' @param filename String, scalar. Input GraCo oscillation file
#' @param header Logic. If TRUE then header of file is read (default F)
#' @return A column-named dataframe containing the following data
#'         l: the mode degree
#'         n: the mode radial node
#'         w2: squared oscillation frequency normalized to GMR
#'         sig: oscillation frequency in microHz
#'         P: oscillation period in days
#'         Q: oscillation constant
#'         NP: number of nodes as P mode
#'         NG: number of nodes as G mode
#'
#'
#'
#' @examples
#' myfreqs <- read_graco('/myhome/gracofile.dat')
#'
#' \dontrun{
#'   read_graco('/myhome/gracofile.dat')
#' }
#'
#' @seealso \code{\link{read_adipls}}, \code{\link{read_losc}}, and
#'     \code{\link{read_gyre}}

read_graco <- function(filename = NULL,
                       header = FALSE) {
  # Reading the data
  if (header) {
    no_skip <- 7 # Number of lines to skip
    header <- scan(filename, what = character(), nlines = 7, quiet = T)
    data <-read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "n", "w2", "sig", "p", "q",
                    "np", "ng")
    )

    attr(data,"model") <- header[3]
    attr(data,"mass") <- as.numeric(header[6])
    attr(data,"logteff") <- as.numeric(header[10])
    attr(data,"logg") <- as.numeric(header[14])
  }
  else {
    no_skip <- 1 # Number of lines to skip
    data <-read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "n", "w2", "sig", "p", "q",
                    "np", "ng")
    )

  }
  data$amp <- 1 # This adds the amplitude variable to the dataframe
  #return(data[, c("n", "l", "sig")])
  return(data)
}
