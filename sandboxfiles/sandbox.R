# This R file is jut for tet purposes during development

source('~/Dropbox/Boulot/dev/r/util/RCustomPackages/miscelanea/seismology/readobs_functions.R')
source('~/Dropbox/Boulot/dev/r/mic/src/MISpecCore.R')

path_rawfiles <- paste(getwd(),"data-raw", sep = "/")

file_graco <- paste0(path_rawfiles,'graco_modelo1_pprima.dat')
file_adipls <- paste0(path_rawfiles,'0-001-asm104zp005df_01300_pxt0_xf1.fobs')
file_gyre <- paste0(path_rawfiles,'1-001-jcd_0100_zdif_z3_present_sun_fgong_lf_gyre.txt')
file_losc <- paste(path_rawfiles,
                   '1-001-jcd_0100_zdif_z3_present_sun_fgong_lf.mod-euler.osc_losc.txt',
                   sep = "/")

file_filou_v23 <- paste(path_rawfiles,'mod_498_g3_Ms526-Zi014_v12.data.frq',
                    sep = "/")
file_filou_v26 <- paste(path_rawfiles,'profile181.data.frq',
                        sep = "/")


test_graco <- read_graco(file_graco, header = T)
# test_adipls <- read_adipls(file_adipls, header = T)

# test_gyre <- read_gyre(file_gyre, header = T)

test_losc <- read_losc(file_losc, header = T)


# Merging data
cc <- merge(test_graco, test_losc, by = c("n", "l"))


# myplot <- ggplot(cc %>% filter(n>5), aes(x = n, y = abs(sig.y-sig.x))) +
#   geom_line(aes(colour = factor(l)))

  # geom_line(aes(y= sig.y), color="red") +
  # geom_line(aes(y= sig.x), color="blue")

#mydiff <- f_diff(test_graco,test_losc)

# plot(test_graco$n[which(test_graco$l ==1)],test_graco$sig[which(test_graco$l ==1)])
# points(test_losc$n[which(test_losc$l ==1)],test_losc$sig[which(test_losc$l ==1)], col = "red")
# plot(test_graco$n[which(test_graco$l ==2)],test_graco$sig[which(test_graco$l ==2)])
# points(test_losc$n[which(test_losc$l ==2)],test_losc$sig[which(test_losc$l ==2)], col = "red")
# plot(test_graco$n[which(test_graco$l ==3)],test_graco$sig[which(test_graco$l ==3)])
# points(test_losc$n[which(test_losc$l ==3)],test_losc$sig[which(test_losc$l ==3)], col = "red")

#listfn <- n_homog_process(list(test_graco,test_losc))


# test_adipls <-read.csv(
#   file_adipls,
#   header = FALSE,
#   sep = "",
#   skip = 7,
#   stringsAsFactors = FALSE,
#   col.names = c("l", "n", "sig", "E", "NP", "NG")
# )
#
#  header <- scan(file_adipls, what = character(), nlines = 2)
#  attr(test_adipls,"model") <- header[2]
#  attr(test_adipls,"comment") <- paste(header[4], header[5], header[6])



# header <- scan(file_graco, what = character(), nlines = 7)
# attr(test_graco,"model") <- header[3]
# attr(test_graco,"mass") <- as.numeric(header[6])
# attr(test_graco,"logteff") <- as.numeric(header[10])
# attr(test_graco,"logg") <- as.numeric(header[14])
#
#
# caca <-  read.csv(file_graco, header = FALSE, sep = "", skip = no_skip,
#                 stringsAsFactors = FALSE,
#                 col.names = c("l", "n", "w2", "sig", "P", "Q",
#                               "NP", "NG"))

no_skip <- 25 # Number of lines to skip
header <- scan(file_filou_v23, what = character(), nlines = no_skip, quiet = T)

## getting the version
ver <- header[2] %>%
strsplit(.,"v") %>%              # remove v
unlist() %>%                     # from list to vector
.[2] %>%                         # keep second term
strsplit(.,".", fixed = T) %>%   # remove dots
.[[1]]                           # transform to vector
num_ver <- as.numeric(
paste(ver[1], ver[2], ver[3], sep = "")  # to single number
)

ver_to_number <- function(x){
  xver <- x %>%                              # x is the version in format xx.yy.zz
    strsplit(.,"v") %>%              # remove v
    unlist() %>%                     # from list to vector
    .[2] %>%                         # keep second term
    strsplit(.,".", fixed = T) %>%   # remove dots
    .[[1]]                           # transform to vector
  as.numeric(
    paste(xver[1], xver[2], xver[3], sep = "")  # to single number
  )
}

num_ver <- ver_to_number(header[2])

test_filou26 <- read_filou(file_filou_v26)


#-----------------------------------------------------------------------------------------------
# HD49933
#-----------------------------------------------------------------------------------------------

mystar <- "hd49933"
xper <- 85

filename <- select_star(mystar,instrument = "CoRoTsolar", location = "laptop")
df.obsdata <- read_ospec(filename, code = "FA", frange = c(50,3000),
                         peaks = T, units = "muHz")

pepe <- get_asegment(df.obsdata, a_seg = 500)

tt <- echelled(pepe, 85, collapse = F)
ttc <- echelled(pepe,85, collapse = T)

pp <- ggplot(tt, aes(x = ech, y = sig)) +
  geom_point()
pp

cc <- ggplot(ttc, aes(x = ech, y = samp)) +
  geom_line()
cc

