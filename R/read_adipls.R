#' read_adipls
#'
#' \code{read_adipls} reads ADIPLS oscillation files
#'
#' This function takes one argument, the ADIPLS file (.dat) to be read with full
#' path. It stores the result in an R dataframe.
#'
#' \usage{
#'   read_adipls(filename, header = F)
#' }
#'
#' @param filename String, scalar. Input ADIPLS oscillation file.
#' @param header Logic. If TRUE then header of file is read (default F).
#' @param verbose Logic. If TRUE then
#' @return A column-named dataframe containing the following data
#'         l: the mode degree
#'         n: the mode radial node
#'         w2: squared oscillation frequency normalized to GMR
#'         sig: oscillation frequency in microHz
#'         E: energy of the mode
#'         NP: number of nodes as P mode
#'         NG: number of nodes as G mode
#'
#'
#'
#' @examples
#' myfreqs <- read_adipls('/myhome/gracofile.dat')
#'
#' \dontrun{
#'   read_graco('/myhome/gracofile.dat')
#' }


read_adipls <- function(filename = NULL,
                        header = FALSE,
                        verbose = FALSE) {
  # Reading the data
  if (header) {
    no_skip <- 7
    header <- scan(filename, what = character(), nlines = 2, quiet = T)
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "n", "sig", "E", "NP", "NG")
    )
    attr(data, "model") <- header[2]
    attr(data, "comment") <- paste(header[4], header[5], header[6])
  }
  else {
    no_skip <- 7
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("l", "n", "sig", "E", "NP", "NG")
    )
  }
  data$amp <-1 # This adds the amplitude variable to the dataframe
  return(data)
}
