#' read_filou
#'
#' \code{read_filou} reads FILOU oscillation files
#'
#' This function takes one argument, the GraCo file (.dat) to be read with full
#' path. It stores the result in an R dataframe.
#'
#'
#' @param filename String, scalar. Input FILOU oscillation file (.frq)
#' @param header Logic. If TRUE then header of file is read (default F)
#' @return A column-named dataframe containing the following data
#'         l: the mode degree
#'         n: the mode radial node
#'         w2: squared oscillation frequency normalized to GMR
#'         sig: oscillation frequency in microHz
#'         fc: oscillation period in days
#'         No: oscillation constant
#'         pc: number of nodes as P mode
#'         I: number of nodes as G mode
#'
#'         All data included in file header is stored as attributes. To see all
#'         attributes use:
#'                attributes(yourobject)
#'         To select one particular attribute use
#'                attr(yourobject, "name-of-attribute")
#'
#'
#'
#'
#' @examples
#' myfreqs <- read_filou('/myhome/filoufile.frq')
#'
#' \dontrun{
#'   read_filou('/myhome/filoufile.frq')
#' }
#'
#' @seealso \code{\link{read_adipls}}, \code{\link{read_losc}},
#'     \code{\link{read_graco}}, and \code{\link{read_gyre}}
#'
#'
read_filou <- function(filename = NULL,
                       header = FALSE) {

# Local function to transform character version to integer
  ver_to_number <- function(x){
    xver <- x %>%                      # x is the version in format xx.yy.zz
      strsplit(.,"v") %>%              # remove v
      unlist() %>%                     # from list to vector
      .[2] %>%                         # keep second term
      strsplit(.,".", fixed = T) %>%   # remove dots
      .[[1]]                           # transform to vector
    as.numeric(
      paste(xver[1], xver[2], xver[3], sep = "")  # to single number
    )
  }

# Reading the data

  no_skip <- 25 # Number of lines to skip
  header <- scan(filename, what = character(), nlines = no_skip, quiet = T)
  num_ver <- ver_to_number(header[2]) # numeric (integer) version

  data <-read.csv(
    filename,
    header = FALSE,
    sep = "",
    skip = no_skip,
    stringsAsFactors = FALSE,
    col.names = c("n", "l", "m", "sig", "fc", "No",
                  "pc", "I0")
  )

  if (num_ver <= 3230){
    attr(data,"code") <- header[1]
    attr(data,"version") <- header[2]
    attr(data,"model") <- header[6]
    attr(data,"sc") <- header[12]
    attr(data,"nodes") <- header[13]
    attr(data,"nshell") <- as.numeric(header[14])
    attr(data,"bso") <- as.numeric(header[15])
    attr(data,"modes") <- header[16]
    attr(data,"mass") <- as.numeric(header[23])
    attr(data,"radius") <- as.numeric(header[24])
    attr(data,"lum") <- as.numeric(header[25])
    attr(data,"teff") <- as.numeric(header[26])
    attr(data,"xc") <- as.numeric(header[30])
    attr(data,"logg") <- as.numeric(header[31])
    attr(data,"age") <- as.numeric(header[32])
    attr(data,"wroti") <- as.numeric(header[33])
    attr(data,"wrots") <- as.numeric(header[38])
    attr(data,"epsilon") <- as.numeric(header[39])
    attr(data,"nuroti") <- as.numeric(header[40])
    attr(data,"nurots") <- as.numeric(header[41])
    attr(data,"gmr3") <- as.numeric(header[48])
    attr(data,"numin") <- as.numeric(header[49])
    attr(data,"numax") <- as.numeric(header[50])
    attr(data,"lmin") <- as.numeric(header[51])
    attr(data,"lmax") <- as.numeric(header[52])
    attr(data,"wnorm") <- as.numeric(header[57])
    attr(data,"nufund") <- as.numeric(header[58])
    attr(data,"nuco") <- as.numeric(header[59])
    attr(data,"vais") <- as.numeric(header[60])
    attr(data,"nucoup") <- as.numeric(header[63])
    attr(data,"ncoup") <- as.numeric(header[64])
  } else {
    attr(data,"code") <- header[1]
    attr(data,"version") <- header[2]
    attr(data,"model") <- header[6]
    attr(data,"sc") <- header[12]
    attr(data,"nodes") <- header[13]
    attr(data,"nshell") <- as.numeric(header[14])
    attr(data,"bso") <- as.numeric(header[15])
    attr(data,"modes") <- header[16]
    attr(data,"mass") <- as.numeric(header[23])
    attr(data,"radius") <- as.numeric(header[24])
    attr(data,"lum") <- as.numeric(header[25])
    attr(data,"teff") <- as.numeric(header[26])
    attr(data,"logg") <- as.numeric(header[27])
    attr(data,"xc") <- as.numeric(header[32])
    attr(data,"x0") <- as.numeric(header[33])
    attr(data,"z0") <- as.numeric(header[34])
    attr(data,"age") <- as.numeric(header[35])
    attr(data,"epsilon") <- as.numeric(header[39])
    attr(data,"nuroti") <- as.numeric(header[40])
    attr(data,"nurots") <- as.numeric(header[41])
    attr(data,"gmr3") <- as.numeric(header[48])
    attr(data,"numin") <- as.numeric(header[49])
    attr(data,"numax") <- as.numeric(header[50])
    attr(data,"lmin") <- as.numeric(header[51])
    attr(data,"lmax") <- as.numeric(header[52])
    attr(data,"wnorm") <- as.numeric(header[57])
    attr(data,"nufund") <- as.numeric(header[58])
    attr(data,"nuco") <- as.numeric(header[59])
    attr(data,"vais") <- as.numeric(header[60])
    attr(data,"nucoup") <- as.numeric(header[63])
    attr(data,"ncoup") <- as.numeric(header[64])
  }

  data$amp <- 1 # This adds the amplitude variable to the dataframe
  #return(data[, c("n", "l", "sig")])
  return(data)
}



