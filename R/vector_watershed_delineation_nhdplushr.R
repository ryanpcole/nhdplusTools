# Borrowed this from smoothr package - https://github.com/mstrimas/smoothr/blob/main/R/fill-holes.r
# Modified it to not use the threshold
#' fill_all_holes
#'
#' @param x An sfc object with polygons that need their holes filled
#'
#' @return An sfc object with holes filled
#' @export
#'
#' @description Borrowed this from smoothr package - https://github.com/mstrimas/smoothr/blob/main/R/fill-holes.r and modified it to not use the threshold
fill_all_holes <- function(x) {
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

#' get_upstream_flowlines
#'
#' @param points an sf object of type POINT or MULTIPOINT
#' @param flowlines an sf object containing nhdplushr flowlines
#'
#' @return an sf object with a list column containing all the upstream COMIDs/hydroseq
#'
#' @description Function to get nearest from points COMID/hydroseq and all upstream
get_upstream_flowlines <- function(points,
                                   flowlines) {
  nearest_flowlines_idx <- sf::st_nearest_feature(points,
                                                  flowlines)
  nearest_flowlines <- flowlines[nearest_flowlines_idx,]
  upstream_comids <- lapply(nearest_flowlines$COMID,
                            get_UT,
                            network = flowlines)

  points$n_upstream_flowlines <- lengths(upstream_comids)
  points$us_comids <- upstream_comids

  return(points)

}



#' get_upstream_catchment_from_nhdplushr
#'
#' @param points an sf object of type POINT or MULTIPOINT containing the the locations
#'               of desired catchment outlets
#' @param flowlines an sf object containing nhdplushr flowlines
#' @param catchments an sf object conatining catchment geometries for each nhdplushr flowline
#' @param crs the coordinate reference system for geoprocessing. Defaults to epsg:4326
#'
#' @return an sfc object of catchment geometries for each point
#'
#'
#' @description Use nhdplus catchments for each flowline and union them together - this might
#' take a while but will likely result in better catchments than from the USGS
#' webservice in get_split_catchments.
#' NOTE: TO use this we need to also to set layers = c("NHDFlowline", "NHDPlusCatchment")
#' in get_nhdplushr in the main function

get_upstream_catchment_from_nhdplushr <- function(points,
                                                  flowlines,
                                                  catchments,
                                                  crs = crs) {
  basin_geometries <- vector(mode = "list",
                             length = nrow(points))
  pb <- progress::progress_bar$new(total = nrow(points))
  pb$tick(0)

  cat("\n")

  for(i in seq_len(nrow(points))) {
    cat("Delineating catchment ", i, " / ", nrow(points), "\r")
    # Use the COMIDs to identify all the upstream flowlines
    comids <- sort(points$us_comids[[i]])
    flowlines_sub <- dplyr::filter(flowlines,
                                   COMID %in% comids)
    # flowline COMIDs are the same as catchment FEATUREIDs
    catchments_sub <- dplyr::filter(catchments,
                                    FEATUREID %in% comids) %>%
      sf::st_make_valid()
    # May need to simplify first to speed things up
    # it might be faster to break the polygons into lines, union them, and then
    # rebuild the boudary polygon rather than unioning all the plygons together
    catchment_polygon <- sf::st_union(catchments_sub,
                                      is_coverage = TRUE)

    basin_geometries[[i]] <- sf::st_geometry(catchment_polygon)

    pb$tick()
    Sys.sleep(0.01)
  }
  basin_geometries <- do.call(rbind, basin_geometries) %>%
    sf::st_sfc(crs = crs) %>%
    sf::st_make_valid()

  return(basin_geometries)
}


#' get_split_catchments
#'
#' @param points an sf object of type POINT or MULTIPOINT containing the the locations
#'               of desired catchment outlets
#'
#' @return an sfc object of catchment polygons for each point
#'
#' @description Get catchments using USGS webservice get_split_catchment
#' NOTE: Sometimes USGS webservice fails, and it uses nhdplusv2 (not high res) so use if this is not
#' recommended.
get_split_catchments <- function(points) {
  # NOTE: webservice doesn't work for the Calapooya Creek polygon for Sutherlin,
  # unknown why it doesn't work...
  basin_geometries <- vector(mode = "list",
                             length = nrow(points))
  pb <- progress::progress_bar$new(total = nrow(points))
  pb$tick(0)
  for(i in seq_len(nrow(points))) {

    # Do in more steps to allow error handling
    split_catchment <- get_split_catchment(points[i,])[2,] # Second row is sub-catchment
    if(is.null(split_catchment)) split_catchment <- points[i,]
    basin_geometries[[i]] <- sf::st_geometry(split_catchment)
    pb$tick()
    Sys.sleep(0.01)
  }
  basin_geometries <- do.call(rbind, basin_geometries) %>%
    sf::st_as_sfc() %>%
    sf::st_set_crs(4326) # CRS from result of get_split_catchment
  return(basin_geometries)
}


# Function to get all the watersheds
#' get_watershed_by_huc
#'
#' @param points_same_huc an sf object of type POINT or MULTIPOINT containing the the locations
#'               of desired catchment outlets. These should all be from the same huc4 (scale of
#'               nhdplushr data download)
#'
#' @param nhdplusdir directory location of nhdplushr data downloads
#' @param crs the coordinate reference system for geoprocessing. Defaults to epsg:4326
#'
#' @return catchments for all the points within a huc4
#'
#' @description function that takes input of points all in the same HUC4, downloads the
#' HUC4 flowlines and catchment data from nhdplushr, and outputs catchments.
get_watershed_by_huc <- function(points_same_huc,
                                 nhdplusdir,
                                 crs = crs) {

  huc4 <- unique(points_same_huc$huc4)
  if(length(huc4) != 1) stop("ERROR: More than one huc4 (or zero)")

  cat("----- Start watershedding for HUC ", huc4,  "(", nrow(points_same_huc), " points) -----\n")

  # THIS BIT SHOULD BE COMMON TO ALL POLYGONS IN A HUC4 FOR SPEED
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
                               layers = c("NHDFlowline",
                                          "NHDPlusCatchment"),
                               proj = crs) # Turns into sf object
    flowlines <- nhdhrdata[[1]]
    nhdcatchments <- nhdhrdata[[2]]
  })
  cat("Time getting NHDPlusHR data ", t_getting["elapsed"], " sec\n")

  # Get all the starting COMIDS after snapping points
  snapped_pts <- get_upstream_flowlines(points_same_huc,
                                        flowlines)
  # Get the catchments for those points
  t_delineating <- system.time({
    catchment_geometries <- get_upstream_catchment_from_nhdplushr(snapped_pts,
                                                                  flowlines = flowlines,
                                                                  catchments = nhdcatchments,
                                                                  crs = crs)
  })
  cat("\nCatchment delineation complete! Time elapsed ",
      t_delineating["elapsed"],
      "\n")

  # Put the catchment geometry list column in the points dataset
  sf::st_geometry(snapped_pts) <- catchment_geometries

  return(snapped_pts)
}


# Wrapper function to find the catchments for all the polygons and return a value
# the "main" function
#' delineate_watersheds
#'
#' @param points an sf object of type POINT or MULTIPOINT containing the the locations
#'               of desired catchment outlets
#' @param nhdplusdir directory location of nhdplushr data downloads
#' @param crs the coordinate reference system for geoprocessing. Defaults to epsg:4326
#'
#' @return watershed areas for each point
#' @export
#'
#' @description Wrapper function to find the catchments for all the points. Can take
#' points from multiple HUC4s
delineate_watersheds <- function(points,
                                 nhdplusdir,
                                 crs = "epsg:4326") {

  # Make sure points are all of geometry type POINT
  stopifnot(sf::st_is(points, "POINT"))

  # Check that there is data in the nhdplusdir we want
  # Do it in a for loop
  point_huc4s <- vector(mode = "character", length = nrow(points))
  for(i in seq_len(nrow(points))) {
    point_geom <- st_geometry(points[i,])
    huc <- get_huc(point_geom)

    # Let's subset all the hucs to the first 4 digits, then pick the huc4 that
    # appears most in the table
    huc4 <- substr(huc$huc12, start = 1, stop = 4)

    point_huc4s[i] <- huc4
  }

  huc4s <- unique(point_huc4s)
  gdb_dirs <- file.path(nhdplusdir,
                        substr(huc4s, 1, 2),
                        paste0("NHDPLUS_H_",
                               huc4s,
                               "_HU4_GDB.gdb"))
  stopifnot("Error: Cannot find GDB of NHDplusHR for all HUC4s. Try downloading using download_nhdplushr" = file.exists(gdb_dirs))

  # Split points into huc4 groups list
  points$huc4 <- point_huc4s

  split_points_by_huc4 <- points %>%
    arrange(.data$huc4) %>%
    group_by(.data$huc4) %>%
    split(.data$huc4)

  # Get the source watershed for each point, but split up by huc4 for speed
  catchments <- lapply(split_points_by_huc4,
                       get_watershed_by_huc,
                       nhdplusdir = nhdplusdir)

  # Bind them together and return
  output_catchments <- do.call(rbind, catchments) %>%
    sf::st_make_valid()

  return(output_catchments)
}
