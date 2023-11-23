# Sandbox for readobs_sigspec
# We use devtools to test the functions informally.
# Then tests must be done formally with testthat
#
#devtools::load_all()
#

library(dplyr)
library(ggplot2)

pathdrive <- "/Users/jcsuarez/Google Drive/My Drive/Data"
pathdata <- paste0(pathdrive,"/obs/Corot/SField/dScuti/SigsPecSeries/")

starnames <- list.files(pathdata)
files_hd174966 <- list.files(paste0(pathdata,starnames[7]),
                             recursive = T,
                             full.names = T)

files_hd49434 <- list.files(paste0(pathdata,starnames[11]),
                             recursive = T,
                             full.names = T)

# Test for r0000i.dat type files HD174966
rfile <- tostada::read_sigspec(files_hd174966[4], type = "frequencies")

# Test for r0000i.dat type files HD174966
tfile <- tostada::read_sigspec(files_hd174966[16], type = "residuals")

# Test for m0000i.dat type files HD49434
mfile <- tostada::read_sigspec(files_hd49434[59], type = "mstracks")

# Test for s000000.dat file HD49434
s00000file <- tostada::read_sigspec(files_hd49434[140], type = "pwspec")

# Test for s0000i.dat type files HD49434
sfile <- tostada::read_sigspec(files_hd49434[170], type = "pwspec")

# Test for resspec.dat file HD49434
resspec_file <- tostada::read_sigspec(files_hd49434[135], type = "pwspec")

# Test for residuals.dat file HD49434
residuals_file <- tostada::read_sigspec(files_hd49434[134], type = "residuals")

# Test for result.dat file HD49434
result_file <- tostada::read_sigspec(files_hd49434[136], type = "frequencies")
