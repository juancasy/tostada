# This R file is jut for tet purposes during development


path_sandbox <- './sandboxfiles/'

file_graco <- paste0(path_sandbox,'graco_modelo1_pprima.dat')
file_adipls <- paste0(path_sandbox,'0-001-asm104zp005df_01300_pxt0_xf1.fobs')
file_gyre <- paste0(path_sandbox,'1-001-jcd_0100_zdif_z3_present_sun_fgong_lf_gyre.txt')
file_losc <- paste0(path_sandbox,'1-001-jcd_0100_zdif_z3_present_sun_fgong_lf.mod-euler.osc_losc.txt')

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
