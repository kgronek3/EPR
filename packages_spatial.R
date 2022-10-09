# Packages for Spatial Econometrics with R course:

packages_spatial <- c("spData", "terra", "tmap", "leafem", "spdep", "rgdal",
                      "maptools", "sp", "RColorBrewer", "classInt", "maps",
                      "prettymapr")

###############################################################################

for (i in packages_spatial) {
  if (!(i %in% installed.packages()[, 1])) {
    install.packages(i, character.only = TRUE, dependencies = TRUE)
  }
  library(i, character.only = TRUE)
}

# library(GISTools) Zdeprecjonowana paczka
