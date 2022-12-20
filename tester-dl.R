# Testing the 7zip download and extraction

# load the package
devtools::load_all()

# set the testing directory
test_dir <- "testdata/raster"

# huc4 code
# huc <- "1708" # 1.4 GB extracts
# huc <- "0302" # 1.8 GB extracts
# huc <- "0420" # 2.0 GB
 huc <- "0510" # 2.2 GB FAILS THIS ONE
# huc <- "0514 # 2.4 GB
# huc <- "1021" # 2.7 GB

# load raster data from that huc
test_raster <- download_nhdplushr(test_dir, huc, download_files = TRUE,
                                  raster = TRUE)
