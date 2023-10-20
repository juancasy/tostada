# Sandbox for readobs_corot
# We use devtools to test the functions informally.
# Then tests must be done formally with testthat
#
#devtools::load_all()
#

library(dplyr)
library(ggplot2)
pathdata <- "/Users/jcsuarez/Google Drive/My Drive/Data/obs/Corot/HHColores"

filename1 <- "EN2_STAR_CHR_0102721716_20070203T130553_20070402T070126.fits"
filename2 <- "EN2_STAR_CHR_0102721716_20071023T223035_20080303T093502.fits"
filename3 <- "AN2_STAR_0000007528_20070411T150752_20070508T213552.fits"

star1_file <- paste(pathdata, filename1, sep="/")
star2_file <- paste(pathdata, filename2, sep="/")
star3_file <- paste(pathdata, filename3, sep="/")

my_lc1 <- readobs_corot(star1_file)
my_lc3h1 <- readobs_corot(star3_file)
my_lc3h2 <- readobs_corot(star3_file, hdu = 2)
my_lc3h3 <- readobs_corot(star3_file, hdu = 3)

# PLOTS of EN2_STAR_CHR files
plotlc1 <- ggplot(my_lc1, aes(x = DATETT, y = REDFLUX)) +
  geom_point(aes(color = "R")) +
  geom_point(aes(x = DATETT, y = BLUEFLUX, color = "B")) +
  geom_point(aes(x = DATETT, y = GREENFLUX, color = "G")) +
  labs(x = "DATEBARTT",
       y = "FLUXBAR")+
  theme_bw() +
  scale_color_manual(values = c("R"="red", "B"="blue", "G" = "green")) +
  ggtitle("TEST EN2 FILE hdu=1", subtitle = filename1) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )
plotlc1

# PLOTS of AN2_STAR_ files (hdu = 1)
plotlc3 <- ggplot(my_lc3, aes(x = DATETT, y = RAWFLUX)) +
  geom_point(color = "orange") +
  labs(x = "DATETT",
       y = "RAWFLUX")+
  theme_bw() +
  ggtitle("TEST AN2 FILE hdu=1", subtitle = filename1) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )

plotlc3

# PLOTS of AN2_STAR_ files (hdu = 2)
plotlc3h2 <- ggplot(my_lc3h2, aes(x = DATEBARTT, y = FLUXBAR)) +
  geom_point(color = "orange") +
  labs(x = "DATEBARTT",
       y = "FLUXBAR")+
  theme_bw() +
  ggtitle("TEST AN2 FILE hdu=2", subtitle = filename1) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )

plotlc3h2

# PLOTS of AN2_STAR_ files (hdu = 3)
plotlc3h3 <- ggplot(my_lc3h3, aes(x = DATEBARREGTT, y = FLUXBARREG)) +
  geom_point(color = "orange") +
  labs(x = "DATEBARREGTT",
       y = "FLUXBARREG")+
  theme_bw() +
  ggtitle("TEST AN2 FILE hdu=3", subtitle = filename1) +
  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
  )

plotlc3h3

