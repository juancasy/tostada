#' read_losc
#'
#' \code{read_losc} reads GraCo oscillation files
#'
#' This function takes one argument, the LOSC file (.txt) to be read with full
#' path. It stores the result in an R dataframe.
#'
#'
#' @param filename String, scalar. Input LOSC oscillation file
#' @param header Logic. If TRUE then header of file is read (default F)
#' @return A column-named dataframe containing the following data
#'         n: the mode radial node
#'         l: the mode degree
#'         sig: oscillation frequency in Hz
#'
#'
#'
#' @examples
#' myfreqs <- read_losc('/myhome/loscfile.dat')
#'
#' \dontrun{
#'   read_losc('/myhome/loscfile.dat')
#' }
#' @seealso \code{\link{read_adipls}}, \code{\link{read_graco}}, and
#'     \code{\link{read_gyre}}






read_losc <- function(filename = NULL,
                      header = FALSE) {
  # Reading the data
  if (header) {
    no_skip <- 0
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("n", "l", "sig")
    )
  }
  else {
    no_skip <- 0
    data = read.csv(
      filename,
      header = FALSE,
      sep = "",
      skip = no_skip,
      stringsAsFactors = FALSE,
      col.names = c("n", "l", "sig")
    )
  }
  data$amp <- 1 # This adds the amplitude variable to the dataframe
  return(data)
}
