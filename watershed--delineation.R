# Testing script for watershed delineation using whitebox tools and NHDPlusHR
# flow accumulation raster. The goal is to give an input of a few points and have
# an output file of delineated watershed polygons in GPKG format? Format is still
# up for grabs I think. Not sure what would be the best default for this.

# This will require figuring out how to install whitebox tools as a package depenency
# for another package - I'm not sure how hard this actually is, but I don't know
# how to do it.

# NOTE: whitebox interface creates a lot of intermediate files, hence the need to
# define all the filepaths and folders at the beginning. Change to fit your directory
# structure and file names

# Make sure the vector and raster files are all in the correct projection
# before running this script

# Load nhdplusTools package
devtools::load_all()

library(whitebox)
library(sf)

# initiate whitebox tools
wbt_init()
# TODO Can I make this  multicore?
# wbt_options(max_procs = 16L,
#             verbose = TRUE)

# use this project CRS if using NHDPlusHR rasters from Lower 48
project_crs <- "ESRI:102039"
# TODO Other projections available for other states/territories defined in
# NHDPlusHR manual. Need to add support for other CRS later


# INPUTS ------------------------------------------------------------------

# Working directory - where would you like to download rasters and export files
work_dir <- file.path("testdata", "raster")

# Constants - Needed for whitebox tools
# you might have to play around with these to get good results
# see whitebox tools documenatation and functions for details.
# https://www.whiteboxgeo.com/manual/wbt_book/intro.html
pour_pt_snap_distance <- 1000
# breach_dist <- 10 # Shouldn't need this if using flow accumulation raster
# flat_inc <- 0.1 # Shouldn't need this if using flow accumulation raster
stream_thresh <- 7000

# Path to outlet points file name
# First check working directory
outlet_pts_path <- file.path("testdata", "vector", "usgs-gauges-hyunwoo.shp")

# Downloading flow accumulation raster ------------------------------------
# TODO Reproject outlet_pts to project CRS
# FIX - why does reprojecting to project CRS give the incorrect huc codes? This
# looks to be a bug in get_huc
outlet_pts <- sf::st_read(outlet_pts_path) %>%
  sf::st_transform(crs = project_crs)
bbox <- sf::st_bbox(outlet_pts) %>%
  sf::st_as_sfc()
huc_codes <- nhdplusTools::get_huc(AOI = bbox,
                                   t_srs = project_crs,
                                   type = "huc04") # Only option for raster DL
huc <- huc_codes$huc4
# download the rasters.
# This should avoid download if the rasters already exist in work_dir
# TODO Needs to handle multiple rasters
raster_dir <- nhdplusTools::download_nhdplushr(work_dir,
                                               huc,
                                               download_files = TRUE,
                                               raster = TRUE)

# Define paths for raster processing --------------------------------------
# TODO Figure out how to use temp files for raster processing

# Get the filepath for the flow accumulation raster and d8 raster from work dir.
# These should all be standardized names so use work_dir, subfolder path, and the
# base file names that should be the same among all raster 7zip archives.
flowaccum_raster <- file.path(raster_dir, "fac.tif")

d8_raster <- file.path(raster_dir, "fdr.tif")

# These are names for raster outputs from whitebox tools
streams_raster <- file.path(raster_dir, "whitebox_streams_raster.tif")

snapped_outlet_pts <- file.path(dirname(outlet_pts_path),
                                "snapped-outlet-pts.shp")

# Whitebox tools part -----------------------------------------------------
# TODO REFACTOR THIS PART
# NHDPlusHR flow directions use ESRI pointer
# snap pour points to streams raster

# First ensure outlet points are in the project CRS. I can't do this above since
# it seems to mess with get_huc function
if(sf::st_crs(outlet_pts) != project_crs) {
  sf::st_write()
}

whitebox::wbt_jenson_snap_pour_points(pour_pts = outlet_pts_path,
                            streams = streams_raster,
                            output = snapped_outlet_pts,
                            snap_dist = pour_pt_snap_distance,
                            verbose_mode = TRUE)

# re-defining variables for watershed delineation
# Load the snapped points and make sure they are the correct geometry
watershed_outlet_points <- sf::st_read(snapped_outlet_points)
if(any(sf::st_geometry_type(watershed_outlet_points) != "POINT")) {
  stop("Watershed outlet points are not POINT geometries")
}

# Function to delineate watersheds from d8_raster and snapped outlet points
delineate_watersheds <- function(d8_raster,
                                 outlet_points,
                                 output_polygon_prefix,
                                 work_dir) {

  # Define working temporary filenames
  # TODO: can I do this with tempfiles?
  working_outlet_point <-     "temp_outlet_point.shp"
  working_watershed_raster <- "watershed_raster_inprogress.tif"

  # Find the length of zeros to pad the filenames
  digits <- floor(log10(nrow(outlet_points))) + 1

  # TODO: speed this up with parallel processing
  for (i in seq_len(nrow(outlet_points))) {
    # initialize the output polygon name
    print(paste0("Starting watershed delineation ---- ", i, "/", nrow(outlet_points)))
    output_polygon_filename <- paste0(output_polygon_prefix,
                                      formatC(i,
                                              width = digits,
                                              format = "d",
                                              flag= "0"),
                                      ".shp")[1] # ASSUMES THE FIRST COLUMN HAS SITE NAME
    print(output_polygon_filename)
    # Write a shapefile of the temporary outlet point
    st_write(outlet_points[i,],
             file.path(work_dir, working_outlet_point),
             append = FALSE)
    # use that temporary outlet point to delineate watershed
    whitebox::wbt_watershed(d8_pntr = d8_raster,
                  pour_pts = working_outlet_point,
                  output = working_watershed_raster,
                  wd = work_dir)
    # Convert that raster to a polygon
    watershed_polygon <- paste0(work_dir, "/watershed_polygons/", output_polygon_filename)
    whitebox::wbt_raster_to_vector_polygons(input = working_watershed_raster,
                                  output = watershed_polygon,
                                  wd = work_dir)
    # Add the CRS to the shapefile. First read in the polygon, then apply CRS, then write
    polygon <- sf::st_read(watershed_polygon)
    sf::st_crs(polygon) <- project_crs
    sf::st_write(polygon,
             watershed_polygon,
             append = FALSE)

    # Complete!
    print(paste0("Finished watershed delineation ---- ", i, "/", nrow(outlet_points)))
  }
}

# Test the watershed delineation ------------------------------------------
delineate_watersheds(d)
