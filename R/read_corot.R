#' read_corot
#'
#' \code{read_corot} reads CoRoT FITS files
#'
#' @details This function reads CoRoT mission FITS files and stores the result in
#' an R dataframe. The function reads
#'
#' * AN2_STAR files. Former Seismo Field. These files correspond to bright stars
#' observed by CoRoT (V<=8). It contains two binary blocks accessed by the hdu
#' parameter.
#' * EN2_STAR_CH files. Former Exo Field. These files correspond to faint stars
#' observed by CoRoT (8<V<12). It contains three binary blocks accessed by the
#' hdu parameter.
#'
#' Details about the content of each file are given below. All the information
#' has been extracted from the publication
#' [CoRoT Legacy Book](https://www.edp-open.org/books/edp-open-books/320-the-
#' corot-legacy-book)
#'
#'#### AN2_STAR (hdu=1)
#'
#'  * DATETT: (double) date of measurement in Terrestrial Time; terrestrial
#'  time, Julian date
#'  * RAWFLUX: (float) Integrated red flux; e- per 32s (32-s or 512-s sampling)
#'  * RAWFLUXDEV: (float) Standard deviation of the 16 exposures of 32 s in
#'  the green channel added for the 512 sampling; e- per 32s
#'  * RAWSTATUS: (int)
#'  * BG: (float) Background flux already subtracted; e- per 32s
#'
#'#### AN2_STAR (hdu=2)
#'
#' * DATEBARTT: (double) date of the measurement in the solar barycentric
#'   reference frame; Solar barycentric Terrestrial Time, Julian day
#' * FLUXBAR: (float) White flux, after the gap filling correction;
#'   e- per 32s
#' * FLUXDEVBAR: (float)
#' * STATUSBAR: (int) Flag of the status
#'
#'#### AN2_STAR (hdu=3)
#' * DATEBARREGTT: (double) date of the measurement in the solar barycentric
#'   reference frame; Solar barycentric Terrestrial Time, Julian day.
#' * FLUXBARREG: (float) White flux, after the gap filling correction;
#'   e- per 32s
#' * FLUXDEVBARREG: (float)
#' * STATUSBARREG: (int) Flag of the status
#'
#'#### EN2_STAR_CH  (hdu=1)
#'
#' * DATE: (string) calendar date, yyyy-mm-ddThh:mm:ss
#' * DATETT: (double) date of measurement in Terrestrial Time; terrestrial
#'   time, Julian date
#' * DATEBARTT: (double ) date of the measurement in the solar barycentric
#'   reference frame; Solar barycentric Terrestrial Time, Julian day
#' * STATUS: (int) Flag of the status
#' * REDFLUX: (float) Integrated red flux; e- per 32s (32s or 512s sampling)
#' * REDFLUXDEV: (float) Standard deviation of the 16 exposures of 32 s in
#'   the green channel added for the 512 sampling; e- per 32s
#' * GREENFLUX: (float) Integrated green flux; e- per 32s (32s or 512s sampling)
#' * GREENFLUXDEV: (float) Standard deviation of the 16 exposures of 32 s in
#'   the green channel added for the 512 sampling; e- per 32s
#' * BLUEFLUX: (float) Integrated blue flux; e- per 32s (32s or 512s sampling)
#' * BLUEFLUXDEV: (float) Standard deviation of the 16 exposures of 32 s in
#'   the green channel added for the 512 sampling; e- per 32s
#' * WHITEFLUX: (float) Integrated white flux; e- per 32s
#' * BG: (float) Background flux already subtracted; e- per 32s
#'
#'#### EN2_STAR_CH (hdu=2)
#'
#' * DATETT: (double) date of measurement in Terrestrial Time; terrestrial
#'   time, Julian date
#' * DATEBARTT: (double ) date of the measurement in the solar barycentric
#'   reference frame; Solar barycentric Terrestrial Time, Julian day
#' * WHITEFLUXFIL: (float) White flux, after the gap filling correction;
#'   e- per 32s
#' * STATUSFIL: (int) Flag of the status
#' * TEXP: (int) Exposure Time; 32s or 512s
#'
#' @param filename String, scalar. Input CoRoT FITS  file (.FITS)
#'
#' @param hdu Numeric, scalar. Data block: 1 (default) or 2,3 (additional data)
#'
#' @returns A column-named dataframe containing the lightcurve and related data
#' as well as the original header as attributes.
#'
#' Header data is stored as attributes. To see all
#' attributes use:
#'      `attributes(yourobject)`
#' To select one particular attribute use
#'      `attr(yourobject, "name-of-attribute")`
#'
#'
#' @examples
#' myLC_exo <- readobs_corot('/myhome/EN2_STAR_CHR_corotid_tini-tfin.fits')
#' myLC_sis <- readobs_corot('/myhome/AN2_STAR_corotid_tini-tfin.fits')
#'
#' \dontrun{
#'   readobs_corot('/myhome/EN2_STAR_CHR_corotid_tini-tfin.fits')
#'}
#'
#' @export
read_corot <- function(corotfits, hdu = 1)
{
  # Read the CoRoT FITS file using FITSio::readFITS and get column names
  df_fits <- FITSio::readFITS(corotfits, hdu = hdu)
  cnames <- df_fits$colNames

  # Store (in df) only the data in dataframe and set column names from cnames
  df <- df_fits %>%
    .$col %>%
    do.call(cbind, .) %>%
    as.data.frame() %>%
    mutate_at(c(2:length(cnames)), as.numeric) %>%
    setNames(cnames)
  # Include header data as attributes to dataframe
  # For AN2 files (former seismic field) files header contains comments
  # at the end of the description. We need to remove those comments before
  # splitting the variables names and their description
  reduced_header <- df_fits$header[!grepl("COMMENT", df_fits$header)]
  att_names <- sapply(strsplit(reduced_header, "="), function(x) x[[1]])
  att_descriptions <- sapply(strsplit(reduced_header, "="), function(x) x[[2]])

  for (i in 1:length(att_names)) {
    attr(df, att_names[i]) <- att_descriptions[i]
  }
  # Return all the data and header info as attributes
  return(df)
}
