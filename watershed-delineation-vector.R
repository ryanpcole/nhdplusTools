# Script to delineate nested watershed polygons for OR DWTP
# Q: Should I delineate using NHDPlusHR DEM or should I use some sort of polygon
# overlap - not sure how I would do this. Maybe using reach codes from NHD flowline

# Gonna need the sf library. Let's try to use reach codes to get everything
# US and merge polygons instead of doing a DEM delineation. That way it's closer
# to the data provided by ODEQ.


# Libraries - make sure these are in the NAMESPACE file using roxygen
library(sf)
library(dplyr)
library(progress)
devtools::load_all()


# User args ---------------------------------------------------------------

# Project CRS
proj_crs <- "epsg:4326"

# Path to input points file
pts_input_file <- "testdata/vector/snapped-outlet-pts.shp"

# Directory to store nhdplusHR files
hr_dir <- file.path("testdata",
                    "vector",
                    "NHDPlusHR")


# Input data -------------------------------------------------------------
# Here will go some option to input data for watershed delineation process.
# I want to have a couple of input options:
# 1. Points - the user can input a vector of points (must be on the streamline)
#    to delineate the upstream area. I will probably need to find or create a
#    helper function to snap points on the streams
# 2. COMIDS - if the user already knows the comid/reach id/ etc. of the stream
#    segment they want, they could input that.
#    TODO: look at the data model to see which unique identifier would be best used here

# POINTS - This is probably better, but how do I snap to flowlines?
# If shapefile
input_pts <- sf::st_read(pts_input_file) %>%
  sf::st_transform(proj_crs)

# TODO Add points by lat_long

# COMIDs
# TODO - add comid functionality

# NHD Flowlines - Hires version (only downloads if nhdplushr data hasn't already been downloaded)
# Download all the NHDPlusHR flowline data for those hucs
aoi <- sf::st_as_sfc(sf::st_bbox(input_pts))

huc4 <- get_huc(aoi,
                t_srs = proj_crs,
                type = "huc04")$huc4

input_pts$huc4 <- huc4


download_nhdplushr(hr_dir, unique(huc4))


# RUN THE MAIN FUNCTION ---------------------------------------------------
# This runs for a REALLY LONG TIME if using the nhdplushr catchment polygons
delineated_watersheds <- delineate_watersheds(input_pts,
                                              hr_dir,
                                              crs = proj_crs)

