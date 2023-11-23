#' read_sigspec
#'
#' \code{read_sigspec} reads SigSpec output files
#'
#' @details This function takes one argument, the SigSpec (.dat) to be read with
#'          full path. It stores the result in an R dataframe. The function
#'          reads
#'
#' *Powerspectra files*:
#'
#'  - s000000.dat contains the spectrum of the original time series
#'  - resspec.dat represents the residual spectrum after finishing the
#'    prewhitening sequence.
#'  - s00000i.dat contains the spectrum of the iteration i
#'
#' *Residuals files (timeseries)*
#'  - residuals.dat the residuals after prewhitening all signal components
#'    found significant. The column format is the same as for the time series
#'    input file
#'  - t00000i.dat are the residuals of iteration i
#'
#' *Results files (frequencies)*
#'  - result.dat contains a list of all identified sig maxima
#'  - r00000i.dat are the results of iteration i
#'
#' *Results files (multitrack)*
#'  - m00000i.dat contains the changes in frequency, amplitude and phase of each
#'    signal component in the prewhitening cascade.They are an alternative
#'    representation of the result files. Instead of a file index that refers to
#'    the iteration, the file index of the MultiSine track files m#index#.dat
#'    refers to the index of the component in the result files.
#'
#'  All the details can be found in the SigSpec website and its online manual at
#'  [SigSpec online](https://cuikaiming.com/SigSpec-backup/)
#'
#' @param filename String, scalar. Input SigSpec oscillation file (.dat)
#'
#' @param type String. Type of file to be read: "pwspc", for
#' `s000000.dat`,`resspec.dat`, and `s00000i.dat` files;  "residuals", for
#' `residuals.dat` and `t00000i.dat` files; or "frequencies" (default), for
#' `result.dat` and `r00000i.dat` files.
#'
#' @return A column-named dataframe containing the following data. Depending on
#'         data type the dataframe will contain
#'
#' *Spectra*
#'   * `f`: (float) frequency (inverse time units)
#'   * `sigf`: (float) significance
#'   * `amp`: (float) DFT amplitude (units of observable)
#'   * `phase`: (float) Fourier-space phase angle (rad)
#'   * `phasems`: (float) Fourier-space phase angle of maximum sig (rad)
#'   * `C1,C2`: Two additional columns containing -1 and 0 (see `SigSpec`
#'              manual for more details)
#'
#'  *Residuals*
#'  * `flux`: (float) flux of the lightcurve (units of observable)
#'  * `time`: (float) time (units of observable)
#'
#'  *Results*
#'
#'   * `f`: (float) frequency (inverse time units)
#'   * `sigf`: (float) significance
#'   * `amp`: (float)  amplitude (units of observable)
#'   * `phase`: (float) phase angle (rad)
#'   * `rms`: (float) rms scatter of the time series before prewhitening
#'   * `pps`: (float) point-to-point scatter of the time series before prewhitening
#'   * `sigfcum`: (float) the cumulative sig for all frequency components detected
#'            so far.
#'
#'  *MultiSine tracks*
#'
#'   * `f`: (float) frequency (inverse time units)
#'   * `amp`: (float)  amplitude (units of observable)
#'   * `phase`: (float) phase angle (rad)
#'
#' @examples
#' myfreqs <- read_sigspec('/myhome/results.dat')
#'
#' \dontrun{
#'   read_sigspec('/myhome/results.dat')
#' }
#'
#' @seealso \code{\link{read_corot}}
#'
#' @export
read_sigspec <- function(filename = NULL,
                         type = c("pwspec",
                                  "residuals",
                                  "frequencies",
                                  "mstrack")
                         ){
  switch (type,
          pwspec = {
            data <- read.csv(file = filename, header = FALSE, sep = "", skip = 1,
                             stringsAsFactors = FALSE,
                             col.names = c("f", "sigf", "amp",
                                           "phase","phasems", "C1", "C2"))
          },
          residuals = {
            data <- read.csv(file = filename, header = FALSE, sep = "", skip = 1,
                             stringsAsFactors = FALSE,
                             col.names = c("time", "flux"))
          },
          frequencies = {
            data <- read.csv(file = filename, header = FALSE, sep = "", skip = 1,
                             stringsAsFactors = FALSE,
                             col.names = c("f", "sigf", "amp", "phase",
                                           "rms","pps","sigfcum"))
          },
          mstracks =
            data <- read.csv(file = filename, header = FALSE, sep = "", skip = 1,
                             stringsAsFactors = FALSE,
                             col.names = c("f", "amp", "phase"))
          )
  return(data)
}
