# Testing the 7zip download and extraction

# TODO: Write documentation- MUST HAVE 7zip installed and accessible from command line
# TODO: If 7z isn't found on path, add manuall. Will need to restart R after adding
# TODO: Check if new download_nhdplushr works with multiple huc4

# load the package
devtools::load_all()

# set the testing directory
test_dir <- "testdata/raster"

# huc4 code
# huc <- "1708" # 1.4 GB extracts
# huc <- "0302" # 1.8 GB extracts
# huc <- "0420" # 2.0 GB # WORKS ON ALL PLATFORMS (archive_extract)
# huc <- "0510" # 2.2 GB # FAILS ON ALL PLATFORMS (archive_extract)
# huc <- "0514" # 2.4 GB # WORKS ON LINUX USING 7z shell command
# huc <- "1021" # 2.7 GB

# These work, but not confirmed that ALL possible dirnames work. USGS may have other
# dirnames i'm not aware of
# huc <- "1709" # This has _with_fac at end of dir
# huc <- "1708" # basic dir name

# load raster data from that huc
test_single<- download_nhdplushr(test_dir, huc, download_files = TRUE,
                                  raster = TRUE)

# Testing that it works If I try to download two rasters from same huc2
# TODO: Find out why it only downloads one
multi_same_huc2 <- c("1708", "1709")

test_multi_same_huc2 <- download_nhdplushr(test_dir,
                                           multi_same_huc2,
                                           download_files = TRUE,
                                           raster = TRUE)


# Testing that it works if I try to download two rasters from different huc2s
multi_diff_huc2 <- c("1708", "0302")

test_multi_diff_huc2 <- download_nhdplushr(test_dir,
                                           multi_diff_huc2,
                                           download_files = TRUE,
                                           raster = TRUE)
