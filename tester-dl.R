# Testing the 7zip download and extraction

# load the package
devtools::load_all()

# set the testing directory
test_dir <- "testdata/raster"

# huc4 code
huc <- "1708"

# load raster data from that huc
test_raster <- download_nhdplushr(test_dir, huc, download_files = TRUE,
                                  raster = TRUE)
