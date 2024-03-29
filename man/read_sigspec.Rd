% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_sigspec.R
\name{read_sigspec}
\alias{read_sigspec}
\title{read_sigspec}
\usage{
read_sigspec(
  filename = NULL,
  type = c("pwspec", "residuals", "frequencies", "mstrack")
)
}
\arguments{
\item{filename}{String, scalar. Input SigSpec oscillation file (.dat)}

\item{type}{String. Type of file to be read: "pwspc", for
\code{s000000.dat},\code{resspec.dat}, and \code{s00000i.dat} files;  "residuals", for
\code{residuals.dat} and \code{t00000i.dat} files; or "frequencies" (default), for
\code{result.dat} and \code{r00000i.dat} files.}
}
\value{
A column-named dataframe containing the following data. Depending on
data type the dataframe will contain

\emph{Spectra}
\itemize{
\item \code{f}: (float) frequency (inverse time units)
\item \code{sigf}: (float) significance
\item \code{amp}: (float) DFT amplitude (units of observable)
\item \code{phase}: (float) Fourier-space phase angle (rad)
\item \code{phasems}: (float) Fourier-space phase angle of maximum sig (rad)
\item \verb{C1,C2}: Two additional columns containing -1 and 0 (see \code{SigSpec}
manual for more details)
}

\emph{Residuals}
\itemize{
\item \code{flux}: (float) flux of the lightcurve (units of observable)
\item \code{time}: (float) time (units of observable)
}

\emph{Results}
\itemize{
\item \code{f}: (float) frequency (inverse time units)
\item \code{sigf}: (float) significance
\item \code{amp}: (float)  amplitude (units of observable)
\item \code{phase}: (float) phase angle (rad)
\item \code{rms}: (float) rms scatter of the time series before prewhitening
\item \code{pps}: (float) point-to-point scatter of the time series before prewhitening
\item \code{sigfcum}: (float) the cumulative sig for all frequency components detected
so far.
}

\emph{MultiSine tracks}
\itemize{
\item \code{f}: (float) frequency (inverse time units)
\item \code{amp}: (float)  amplitude (units of observable)
\item \code{phase}: (float) phase angle (rad)
}
}
\description{
\code{read_sigspec} reads SigSpec output files
}
\details{
This function takes one argument, the SigSpec (.dat) to be read with
full path. It stores the result in an R dataframe. The function
reads

\emph{Powerspectra files}:
\itemize{
\item s000000.dat contains the spectrum of the original time series
\item resspec.dat represents the residual spectrum after finishing the
prewhitening sequence.
\item s00000i.dat contains the spectrum of the iteration i
}

\emph{Residuals files (timeseries)}
\itemize{
\item residuals.dat the residuals after prewhitening all signal components
found significant. The column format is the same as for the time series
input file
\item t00000i.dat are the residuals of iteration i
}

\emph{Results files (frequencies)}
\itemize{
\item result.dat contains a list of all identified sig maxima
\item r00000i.dat are the results of iteration i
}

\emph{Results files (multitrack)}
\itemize{
\item m00000i.dat contains the changes in frequency, amplitude and phase of each
signal component in the prewhitening cascade.They are an alternative
representation of the result files. Instead of a file index that refers to
the iteration, the file index of the MultiSine track files m#index#.dat
refers to the index of the component in the result files.
}

All the details can be found in the SigSpec website and its online manual at
\href{https://cuikaiming.com/SigSpec-backup/}{SigSpec online}
}
\examples{
myfreqs <- read_sigspec('/myhome/results.dat')

\dontrun{
  read_sigspec('/myhome/results.dat')
}

}
\seealso{
\code{\link{read_corot}}
}
