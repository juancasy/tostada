#' read_cestam
#'
#' \code{read_cestam} reads the content of stellar structure files form CESTAM
#' code, and save it to a dataframe.
#'
#'
#' This function takes one argument: the stellar structure file .osc from
#' CESTAM code. It should also compatible with last versions (deprecated)
#' of CESAM2K models as well as MESA (OSC-type) models.
#' The input file can be local or web address (http).
#'
#'
#'@section Metadata:
#'
#' \tabular{ll}{
#'    \code{name} \tab  name of the model file \cr
#'    \code{code} \tab  generator code name \cr
#'    \code{version} \tab  version of the code \cr
#'    \code{date} \tab date of the computation \cr
#'    \code{time} \tab time of the computation  \cr
#'  }
#'
#'
#'@section Global variables:
#'
#' \tabular{ll}{
#'    \code{rmass} \tab  mass, cgs \cr
#'    \code{R} \tab  radius, cgs \cr
#'    \code{rphot} \tab  photometric radius, cgs \cr
#'    \code{rlum} \tab luminosity, cgs, log10  \cr
#'    \code{al} \tab convective alpha (dimensionless) \cr
#'    \code{age} \tab age, Myr \cr
#'    \code{rotini} \tab initial angular rotation (in rad/s) \cr
#'    \code{rlogg} \tab surface gravity, cgs, log10 \cr
#'    \code{teff} \tab effective temperature (temp(r=rphot)), K \cr
#'    \code{nshell} \tab number of shells of the model (radial)
#'  }
#'
#' @section Structure variables:
#'
#' \tabular{ll}{
#'    \code{r} \tab  radius profile, cgs, log \cr
#'    \code{q} \tab  mass profile, cgs, log \cr
#'    \code{temp} \tab temperature profile, K  \cr
#'    \code{p} \tab pressure profile, cgs \cr
#'    \code{rho} \tab density profile rho, cgs \cr
#'    \code{gamma} \tab gamma1 parameter, cgs \cr
#'    \code{aast} \tab A constant (BV) \cr
#'    \code{rr} \tab radius normalized to total radius \cr
#'    \code{c1} \tab solar dynamical time \cr
#'    \code{cs2} \tab sound speed squared, cgs \cr
#'    \code{vg} \tab solar dynamical time \cr
#'    \code{rot} \tab rotation profile, rad/s \cr
#'
#'  }
#'
#' @param file ASCII file with a stellar structure compliant with stellar
#' oscillations computation with extension .osc or .OSC.
#' @return A dataframe with the stellar structure, in which global parameters
#' -common values for the star - are saved as attributes, and variables -
#' physical quantities that vary with the stellar depth- are saved as columns.
#'
#'
#'
#'
#' @examples
#' mystruc <- read_cestam("/my/path/file.osc")
#'
#' @examples
#' mystruc_from_web <- read_cestam("http://my-path-to-file/myoscfile.osc")
#'
#' \dontrun{
#'   mystruc <- read_cestam("/my/path/file.osc")
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
      v <- as.numeric(unlist(strsplit(
          gsub("([0-9])-([0-9])", "\\1 -\\2", s), " "
        )))
      dat <- append(dat, v)
    }
    return(dat)
  }

  # Open file for sequential read
  ifelse ((grepl("http://", strFile) | grepl("https://", strFile)),
          cfile <- file(description = strFile, open = "r"),
          cfile <- file(description = strFile, open = "r"))


  # Header (first 6 lines)
  headerlines <- readLines(con = cfile, n = 6)
  # Establish whether adiabatic or non-adiabatic
  noAdiabaticas <- grepl("non adiabatiques", headerlines[1])
  linea2 <- strsplit(headerlines[2]," ")
  driver <- linea2[[1]][1]
  version <- linea2[[1]][3]
  contenido <- NA

  # The position (indexes) of some variables depend on the case (adiab, non adiab)
  indices <- NULL
  if(noAdiabaticas){
    contenido <- "Non Adiabatic"
    # define indexes of variable for the non-adiabatic case
    indices <- c(rmass = 1, rfotom = 2, rlum = 3, al = 6, age = 11, rotini = 12,
                 r = 1, q = 2, t = 3, p = 4, rho = 5, gamma = 10, aast = 15,
                 rot = 16)
  }else{
    contenido <- "Adiabatic"
    # define indexes of variable for the adiabatic case
    indices <- c(rmass = 1, rfotom = 2, rlum = 3, al = 6, age = 11, rotini = 13,
                 r = 1, q = 2, t = 3, p = 4, rho = 5, gamma = 10, aast = 15,
                 rot = 16)
  }



  # Read array of relevant numbers in the model (nshell, nchim, ncz, et.)
  numeros <- strsplit(headerlines[6]," {1,20}")[[1]]
  # Remove the last shell (center of star) to avoid numerical problems.
  m <- as.integer(numeros[2])-1
  # No. of data included in global data
  numDatosGlobales <- as.integer(numeros[3])
  # No. of chemical elements included in the model
  numElemQuim <- as.integer(numeros[5])
  # No. of data in shell
  numDatosCapa <- as.integer(numeros[4]) + numElemQuim

  # Create a structure for the model
  e <- data.frame(r = double(m),     # r*rsol
                  q = double(m),     # log(m/mstar) -1.d38 au centre
                  temp = double(m),     # temparature profile
                  p = double(m),     # pressure profile
                  rho = double(m),     # density profile
                  gamma = double(m),     #
                  aast = double(m),     #
                  rr = double(m),     #
                  c1 = double(m),     #
                  cs2 = double(m),     #
                  vg = double(m),     #
                  u = double(m),     #
                  rot = double(m)     #
  )

  #temp <- double(m)
  # Load global data
  # block has lines of 5 data each with a total of numDatosGlobales
  datos <- readBlock(cfile, ceiling(numDatosGlobales/5))
  # The no. of data included change with Cesam version. Frozen from CESAM2k

  # Store global data as attributes
  attr(e,"rmass") <- datos[indices["rmass"]]
  attr(e,"rfotom") <- datos[indices["rfotom"]]
  attr(e,"rlum") <- log10(datos[indices["rlum"]]/constants$slum)
  attr(e,"alpha") <- datos[indices["al"]]
  attr(e,"age") <- datos[indices["age"]]
  attr(e,"rotini") <- datos[indices["rotini"]]
  attr(e,"rlogg") <- log10((constants$gk*attributes(e)$rmass)/(attributes(e)$rfotom^2))

  # Read arrays of data
  # The adiabatic .osc has numDatosCapa data per shell and 5 data per line
  numLineas <- ceiling(numDatosCapa/5)

  for(i in m:1){
    datos <- readBlock(cfile, numLineas)
    e$r[i] <- datos[indices["r"]]
    e$q[i] <- datos[indices["q"]]
    e$temp[i] <- datos[indices["t"]]
    e$p[i] <- datos[indices["p"]]
    e$rho[i] <- datos[indices["rho"]]
    e$gamma[i] <- datos[indices["gamma"]]
    e$aast[i] <- datos[indices["aast"]]
    e$rot[i] <- datos[indices["rot"]]
  }

  # Converstion of data to log
  e$r <- log(e$r)
  e$q <- log(attributes(e)$rmass * exp(e$q))
  e$p <- log(e$p)
  e$rho <- log(e$rho)

  # Complete data of the model
  rel_t_r <- splinefun(exp(e$r),e$temp)
  teff <- rel_t_r(attributes(e)$rfotom)
  teff <- log10(teff)
  e$rr <- exp(e$r)/attributes(e)$rfotom
  e$c1 <- exp(3*e$r-e$q)*attributes(e)$rmass/(attributes(e)$rfotom^3)
  e$cs2 <- e$gamma*exp(e$p-e$rho)
  e$vg <- constants$gk*exp(e$q-e$r)/e$cs2
  e$u <- 4*pi*exp(e$rho+3*e$r-e$q)
  e$grav <- constants$gk*exp(e$q-2*e$r)
  e$rn2 <- e$grav*e$aast/exp(e$r)
  e$rnn <- e$rn2*(attributes(e)$rfotom^3)/(constants$gk*attributes(e)$rmass)

  # Complete attributes
  attr(e, "teff") <- teff
  attr(e, "ct1") <- e$c1[1]
  attr(e, "R") <- e$r[m]
  attr(e, "version") <- version
  attr(e, "code") <- driver
  attr(e, "nshell") <- m + 1


  # end
  close(cfile)
  return(e)


}


