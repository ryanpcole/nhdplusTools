# Script to delineate nested watershed polygons for OR DWTP
# Q: Should I delineate using NHDPlusHR DEM or should I use some sort of polygon
# overlap - not sure how I would do this. Maybe using reach codes from NHD flowline

# Gonna need the sf library. Let's try to use reach codes to get everything
# US and merge polygons instead of doing a DEM delineation. That way it's closer
# to the data provided by ODEQ.

# TODO: Add some of these functions to my fork of nhdplusTools
# TODO: maybe I could get the catchment without accessing the webservice from
# get_nhdplushr with the catchment data includeded. I could get all the comids
# and union the polygons for those comids as the catchment area


# Libraries
library(sf)
library(dplyr)
library(nhdplusTools)
library(purrr)
library(progress)

# Project CRS
proj_crs <- "epsg:4326"

# Import data -------------------------------------------------------------
# DWSA Polygons
polygons <- st_read("data/geospatial/surface-drinking-water-sources/shapefiles/OR_drinking_water_source_areas.shp") %>%
  mutate(huc4 = substr(SUBBASIN, 1, 4)) %>%
  st_cast("POLYGON") %>%
  mutate(og_polygon_area = as.numeric(st_area(geometry))) %>%
  filter(og_polygon_area > 100)

# NHD Flowlines - Hires version (only downloads if nhdplushr data hasn't already been downloaded)
hr_dir <- file.path("data",
                    "geospatial",
                    "NHDPlusHR")
# Download all the NHDPlusHR flowline data for those hucs
download_nhdplushr(hr_dir, polygons$huc4)

# FUNCTIONS ---------------------------------------------------------------

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


# Function that finds the points which a polygon intersects with the NHDPlusHR
# flowline dataset and returns those points.
# TODO should I add some ranking system for returning the points which priortizes?
# This would mainly help for intakes on or near the Columbia

get_points_largest_drainage <- function(points,
                                        flowlines) {
  comids_all_points <- lapply(points$COMID,
                              get_UT,
                              network = flowlines)
  # Maybe not the best way but w/e
  points$n_upstream_tribs <- lengths(comids_all_points)
  points$us_comids <- comids_all_points
  largest_points <- points %>%
    group_by(TINWSF_IS) %>%
    arrange(TINWSF_IS, desc(n_upstream_tribs), .by_group = TRUE) %>%
    filter(row_number() == 1)

  return(largest_points)
}

# Use nhdplus catchments for each flowline and union them together - this might
# take a while but will likely result in better catchments than from the USGS
# webservice in get_split_catchments
# NOTE: TO use this we need to also to set layers = c("NHDFlowline", "NHDPlusCatchment")
# in get_nhdplushr in the main function
get_upstream_catchment_from_nhdplus <- function(points,
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

# Function to get and merge all polygons intersecting with the COMIDs (ignoring the
# initial comid since it may intersect the downstream polygon)
# Maybe only do this if the contributing area decreases after delineation the other way
get_upstream_polygons <- function(points,
                                  flowlines,
                                  polygons) {
  basin_geometries <- vector(mode = "list",
                             length = nrow(points))
  pb <- progress_bar$new(total = nrow(points))
  pb$tick(0)
  for(i in seq_len(nrow(points))) {
    # Use the COMIDs to identify all the upstream flowlines
    comids <- sort(points$us_comids[[i]])
    flowlines_sub <- filter(flowlines,
                            COMID %in% comids) %>%
      arrange(COMID) %>%
      filter(row_number() != 1)
    # Get the polygons that intersect those flowlines
    intersecting_polygon <- polygons[which(lengths(st_intersects(polygons, flowlines_sub)) > 0),] %>%
      st_union()

    basin_geometries[[i]] <- st_geometry(intersecting_polygon)

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
get_watershed_by_huc <- function(polygons_same_huc,
                                 nhdplusdir) {

  huc4 <- unique(polygons_same_huc$huc4)
  if(length(huc4) != 1) stop("ERROR: More than one huc4 (or zero)")

  cat("----- Start watershedding for HUC ", huc4,  "(", nrow(polygons_same_huc), " polygons) -----\n")

  # First turn the polygon into lines
  lines <- st_cast(polygons_same_huc, "MULTILINESTRING")

  # TODO: What to do with this line?
  tinwsf_is <- polygons_same_huc$TINWSF_IS

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
                               # layers = "NHDPlusCatchment",
                               proj = proj_crs) # Turns into sf object
    flowlines <- nhdhrdata[[1]]
    nhdcatchments <- nhdhrdata[[2]]
  })
  cat("Time getting NHDPlusHR data ", t_getting["elapsed"], " sec\n")
  # Find the intersection points of those flowlines and the polygon lines
  t_intersect <- system.time({

    # TODO How could I speed this up? What does get_split_catchment actually need?
    # First subset to lines that actually intersect then do the intersection
    flowlines_that_intersect <- flowlines[which(lengths(st_intersects(flowlines, lines)) > 0),]
    points <- st_intersection(lines, flowlines_that_intersect) %>%
      arrange(TINWSF_IS, COMID) %>%
      st_cast("POINT")
  })
  cat("Time intersecting flowlines and polygons ", t_intersect["elapsed"], " sec\n")
  # Find the points with the largest drainages (hopefully these are what I want)
  t_large_points <- system.time({
    large_points <- get_points_largest_drainage(points,
                                                flowlines)
  })
  cat("Time getting largest drainages ", t_large_points["elapsed"], " sec\n")
  # Get the catchments for those points
  # TODO Debug HUC 1710 error on point 64/129
  catchment_geometries <- get_upstream_catchment_from_nhdplus(large_points,
                                                              flowlines = flowlines,
                                                              catchments = nhdcatchments)
  cat("\nCatchment delineation complete!\n")
  # Logic to account for polygons that are so small they don't intersect with flowlines
  # If that's the case the plan is to keep the original polygon as the DWSA
  if(!setequal(points$TINWSF_IS, tinwsf_is)) {
    # Polygons that don't intersect flowlines
    missing_tinwsf <- tinwsf_is[!(tinwsf_is %in% points$TINWSF_IS)]
    polygons_missing <- filter(polygons_same_huc,
                               TINWSF_IS %in% missing_tinwsf)
    # These polygons do intersect flowlines
    redo_polygons <- filter(polygons_same_huc,
                            !(TINWSF_IS %in% missing_tinwsf))
    redone_catchments <- st_drop_geometry(redo_polygons)
    st_geometry(redone_catchments) <- catchment_geometries
    catchments <- rbind(redone_catchments, polygons_missing)

  } else {
    catchments <- st_drop_geometry(polygons_same_huc)
    st_geometry(catchments) <- catchment_geometries
  }

  # Change any point geometries (means the webservice couldn't delineate a catchment)
  # into the original polygon - so far this only happens to one of the Sutherlin point,
  # and that catchment looks alright anyway.
  if(any(st_geometry_type(catchments) == "POINT")) {
    # Where is the point geometry
    point_idx <- which(st_geometry_type(catchments) == "POINT")
    # Replace the point geometry with the OG polygon (from polygons_same_huc)
    geometry_vec <- st_geometry(catchments)
    catchments <- st_drop_geometry(catchments)
    geometry_vec[point_idx] <- st_geometry(polygons_same_huc[point_idx,])
    # Put them together
    st_geometry(catchments) <- geometry_vec
    if(any(st_geometry_type(catchments) == "POINT")) browser()
  }
  # Remove holes from catchments
  return(catchments)
}


# Wrapper function to find the catchments for all the polygons and return a value
# the "main" function
correct_source_watersheds <- function(polygons,
                                      nhdplusdir,
                                      crs = proj_crs) {

  # Split polygons into huc4 groups list
  split_polygons_by_huc4 <- polygons %>%
    arrange(huc4, TINWSF_IS) %>%
    split(.$huc4)

  # Get the source watershed for each polygon, but split up by huc4 for speed
  catchments <- lapply(split_polygons_by_huc4,
                       get_watershed_by_huc,
                       nhdplusdir = nhdplusdir)

  # Figure out how to bind them together and return
  dwsa_catchments <- do.call(rbind, catchments) %>%
    st_make_valid()

  return(dwsa_catchments)
}

# RUN THE MAIN FUNCTION ---------------------------------------------------
# This runs for a REALLY LONG TIME if using the nhdplushr catchment polygons
# delineated_watersheds <- correct_source_watersheds(polygons,
#                                                    hr_dir,
#                                                    crs = proj_crs)

