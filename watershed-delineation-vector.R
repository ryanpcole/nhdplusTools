# Script to delineate nested watershed polygons for OR DWTP
# Q: Should I delineate using NHDPlusHR DEM or should I use some sort of polygon
# overlap - not sure how I would do this. Maybe using reach codes from NHD flowline

# Gonna need the sf library. Let's try to use reach codes to get everything
# US and merge polygons instead of doing a DEM delineation. That way it's closer
# to the data provided by ODEQ.


# Libraries
library(sf)
library(dplyr)
library(purrr)
library(progress)
devtools::load_all()


# User args ---------------------------------------------------------------

# Project CRS
proj_crs <- "epsg:4326"

# Path to input points file
pts_input_file <- ""

# Directory to store nhdplusHR files
hr_dir <- file.path("data",
                    "geospatial",
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
# 3. Polygon - user could give a polygon, this is what I used to improve the polygons
#    for OR drinking water source areas. However, this is probably a bad idea for
#    general use since any arbitrary polygon will likely cross multiple streamlines
#    and watershed boundaries. I should probably remove this functionality from
#    general purpose code
# Polygons

# POINTS - This is probably better, but how do I snap to flowlines?
# If shapefile
input_pts <- sf::st_read(pts_input_file) %>%
  sf::st_transform(proj_crs)

# TODO Add points by lat_long

# COMIDs
# TODO - add comid functionality

# NHD Flowlines - Hires version (only downloads if nhdplushr data hasn't already been downloaded)
# Download all the NHDPlusHR flowline data for those hucs
huc4 <- get_huc(AOI = input_pts,
                t_srs = proj_crs,
                type = "huc04")
download_nhdplushr(hr_dir, huc4)

# FUNCTIONS ---------------------------------------------------------------
# TODO: Give the user multiple options to delineate watersheds:
# option 1 - use NHDPlusHR catchments
# option 2 - use the USGS get_split_catchment webservice

# TODO: give the user the option to use nhdplusv2 or nhudplushr

# TODO: Function to turn lon lat into sf object

# TODO function to snap point to stream network
# Test sf::st_snap


# Run the algorithm in chunks by huc4 if using nhdplushr dataset

# Borrowed this from smoothr package - https://github.com/mstrimas/smoothr/blob/main/R/fill-holes.r
# Modified it to not use the threshold
fill_all_holes.sfc <- function(x) {
  # check geometry types and get units of feature size
  if (!all(sf::st_is(x, c("POLYGON", "MULTIPOLYGON")))) {
    stop("fill_all_holes() only works for polygon features.")
  }

  x_crs <- sf::st_crs(x)

  # loop over features, potentially multipart
  for (i in seq_along(x)) {
    # split up multipart geometries
    singles <- sf::st_cast(x[i], "POLYGON")
    # loop over single features
    for (j in seq_along(singles)) {
      # skip features with no holes
      if (length(singles[[j]]) > 1) {
        # remove all holes regardless of threshold
        singles[[j]] <- sf::st_polygon(singles[[j]][1])
      }
    }
    # recombine
    if (length(singles) != 1) {
      singles <- sf::st_combine(singles)
    }
    x[[i]] <- singles[[1]]
  }
  # remove empty geometries
  sf::st_sfc(x)
}

# TODO Function to snap points to stream system

# Use nhdplus catchments for each flowline and union them together - this might
# take a while but will likely result in better catchments than from the USGS
# webservice in get_split_catchments
# NOTE: TO use this we need to also to set layers = c("NHDFlowline", "NHDPlusCatchment")
# in get_nhdplushr in the main function
get_upstream_catchment_from_nhdplushr <- function(points,
                                                  flowlines,
                                                  catchments) {
  basin_geometries <- vector(mode = "list",
                             length = nrow(points))
  pb <- progress_bar$new(total = nrow(points))
  pb$tick(0)
  for(i in seq_len(nrow(points))) {
    cat("Delineating catchment ", i, " / ", nrow(points), "\r")
    # Use the COMIDs to identify all the upstream flowlines
    comids <- sort(points$us_comids[[i]])
    flowlines_sub <- filter(flowlines,
                            COMID %in% comids)
    # flowline COMIDs are the same as catchment FEATUREIDs
    catchments_sub <- filter(catchments,
                             FEATUREID %in% comids) %>%
      st_make_valid()
    # May need to simplify first to speed things up
    # it might be faster to break the polygons into lines, union them, and then
    # rebuild the boudary polygon rather than unioning all the plygons together
    catchment_polygon <- st_union(catchments_sub,
                                  is_coverage = TRUE)

    basin_geometries[[i]] <- st_geometry(catchment_polygon)

    pb$tick()
    Sys.sleep(0.01)
  }
  basin_geometries <- do.call(rbind, basin_geometries) %>%
    st_as_sfc() %>%
    st_set_crs(4326)
}


# Get catchments using USGS webservice get_split_catchment
get_split_catchments <- function(points) {
  # NOTE: webservice doesn't work for the Calapooya Creek polygon for Sutherlin,
  # unknown why it doesn't work...
  basin_geometries <- vector(mode = "list",
                             length = nrow(points))
  pb <- progress_bar$new(total = nrow(points))
  pb$tick(0)
  for(i in seq_len(nrow(points))) {

    # Do in more steps to allow error handling
    split_catchment <- get_split_catchment(points[i,])[2,] # Second row is sub-catchment
    if(is.null(split_catchment)) split_catchment <- points[i,]
    basin_geometries[[i]] <- st_geometry(split_catchment)
    pb$tick()
    Sys.sleep(0.01)
  }
  basin_geometries <- do.call(rbind, basin_geometries) %>%
    st_as_sfc() %>%
    st_set_crs(4326) # CRS from result of get_split_catchment
  return(basin_geometries)
}


# Function to get all the watersheds
get_watershed_by_huc <- function(points_same_huc,
                                 nhdplusdir) {

  huc4 <- unique(points_same_huc$huc4)
  if(length(huc4) != 1) stop("ERROR: More than one huc4 (or zero)")

  cat("----- Start watershedding for HUC ", huc4,  "(", nrow(points_same_huc), " polygons) -----\n")

  # THIS BIT SHOULD BE COMMON TO ALL POLYGONS IN A HUC4 FOR SPEED --------
  # Next pull in the nhdplushr flowlines for that polygon's huc
  cat("Getting the flowlines from NHDPlusHR\n")

  # Timing it
  t_getting <- system.time({
    gdb_dir <- file.path(nhdplusdir,
                         substr(huc4, 1, 2))
    nhdhrdata <- get_nhdplushr(gdb_dir,
                               pattern = paste0("NHDPLUS_H_",
                                                huc4,
                                                "_HU4_GDB.gdb"),
                               layers = c("NHDPlusFlowlines",
                                          "NHDPlusCatchment"),
                               proj = proj_crs) # Turns into sf object
    flowlines <- nhdhrdata[[1]]
    nhdcatchments <- nhdhrdata[[2]]
  })
  cat("Time getting NHDPlusHR data ", t_getting["elapsed"], " sec\n")

  # TODO Snap the points to the flowlines


  # Get the catchments for those points
  t_delineating <- system.time({
    catchment_geometries <- get_upstream_catchment_from_nhdplushr(snapped_points,
                                                                  flowlines = flowlines,
                                                                  catchments = nhdcatchments)
  })
  cat("\nCatchment delineation complete! Time elapsed \n")


  return(catchment_geometries)
}


# Wrapper function to find the catchments for all the polygons and return a value
# the "main" function
delineate_watersheds <- function(points,
                                 nhdplusdir,
                                 crs = proj_crs) {

  # Make sure points are a shapefile
  stopifnot(class(points) == "sfc")

  # Split polygons into huc4 groups list
  split_points_by_huc4 <- points %>%
    arrange(huc4, TINWSF_IS) %>%
    split(.$huc4)

  # Get the source watershed for each polygon, but split up by huc4 for speed
  catchments <- lapply(split_points_by_huc4,
                       get_watershed_by_huc,
                       nhdplusdir = nhdplusdir)

  # Figure out how to bind them together and return
  output_catchments <- do.call(rbind, catchments) %>%
    st_make_valid()

  return(output_catchments)
}

# RUN THE MAIN FUNCTION ---------------------------------------------------
# This runs for a REALLY LONG TIME if using the nhdplushr catchment polygons
# delineated_watersheds <- correct_source_watersheds(polygons,
#                                                    hr_dir,
#                                                    crs = proj_crs)

