#' read_cestam
#'
#' \code{read_cestam} reads the content of stellar structure files form CESTAM
#' code, and save it to a dataframe.
#'
#'
#' This function takes one argument: the stellar structure file .osc from
#' CESTAM code. It should also compatible with last versions (deprecated)
#' of CESAM2K models as well as MESA (OSC-type) models
#'
#'
#' @param file ASCII file (.osc or .OSC)
#' @return A dataframe with the stellar structure, in which global parameters
#' - common values for the star - are saved as attributes, and variables -
#' physical quantities that vary with the stellar depth- are saved as columns.
#'
#'
#'
#' @examples
#' mystruc <- read_cestam(/my/path/file.osc)
#'
#' \dontrun{
#'   mystruc <- read_cestam(/my/path/file.osc)
#' }
#' @seealso \code{\link{read_gong}}
#'
#'



read_cestam <- function(strFile){

  # Routine to read blocks of several lines in .osc files and save them to a vec
  readBlock = function(cfile, intLineas) {
    dat <- integer() # vector vacio
    blocklines <- readLines(con = cfile, n = intLineas)
    for (i in blocklines) {
      # remove first blank space in line
      s <- sub("^\\s+", "", i)
      # separate the  - that sometimes join different numbers
      v <-
        as.numeric(unlist(strsplit(
          gsub("([0-9])-([0-9])", "\\1 -\\2", s), " "
        )))
      dat <- append(dat, v)
    }
    return(datos)
  }

  # Open file for sequential read
  cfile <- file(description = strFile, open = "r")

  # Header (first 6 lines)
  headerlines <- readLines(con = cfile, n = 6)
  # Establish whether adiabatic or non-adiabatic
  noAdiabaticas <- grepl("non adiabatiques", headerlines[1])
  linea2 <- strsplit(linea[2]," ")
  driver <- linea2[[1]][1]
  version <- linea2[[1]][3]
  contenido <- NA

  # The position (indexes) of some variables depend on the case (adiab, non adiab)
  indices <- NULL
  if(noAdiabaticas){
    contenido <- "Non Adiabatic"
    # define indexes of variable for the non-adiabatic case
    indices <- c(rmasa = 1, rfotom = 2, rlumi = 3, al = 6, edad = 11, rot = 12,
                 r = 1, q = 2, t = 3, p = 4, rho = 5, gamma = 10, aast = 15)
  }else{
    contenido <- "Adiabatic"
    # define indexes of variable for the adiabatic case
    indices <- c(rmasa = 1, rfotom = 2, rlumi = 3, al = 6, edad = 11, rot = 13,
                 r = 1, q = 2, t = 3, p = 4, rho = 5, gamma = 10, aast = 15)
  }



  # Read array of relevant numbers in the model (nshell, nchim, ncz, et.)
  numeros <- strsplit(linea[6]," {1,20}")[[1]]
  # Remove the last shell (center of star) to avoid numerical problems.
  m <- as.integer(numeros[2])-1
  # No. of data included in global data
  numDatosGlobales <- as.integer(numeros[3])
  # No. of chemical elements included in the model
  numElemQuim <- as.integer(numeros[5])
  # No. of data in shell
  numDatosCapa <- as.integer(numeros[4]) + numElemQuim

  # Create a structure for the model
  e <- data.frame(r = double(m),
                  q = double(m),
                  temp = double(m),
                  p = double(m),
                  rho = double(m),
                  gamma = double(m),
                  aast = double(m),
                  teff = double(m),
                  rr = double(m),
                  c1 = double(m),
                  cs2 = double(m),
                  vg = double(m),
                  u = double(m)
  )

  # Load global data
  # block has lines of 5 data each with a total of numDatosGlobales
  datos <- readBlock(cfile, ceiling(numDatosGlobales/5))
  # The no. of data included change with Cesam version. Frozen from CESAM2k

  # Store global data as attributes
  attr(e,"rmasa") <- datos[indices["rmasa"]]
  attr(e,"rfotom") <- datos[indices["rfotom"]]
  attr(e,"rlumi") <- log(datos[indices["rlumi"]]/Constants["slumi"])
  attr(e,"al") <- datos[indices["al"]]
  attr(e,"edad") <- datos[indices["edad"]]
  attr(e,"rot") <- datos[indices["rot"]]
  attr(e,"rlogg") <- log((Constants["gk"]*e$rmasa)/(e$rfotom^2))

}


