# Testing the 7zip download and extraction

# TODO: Write documentation- MUST HAVE 7zip installed and accessible from command line

# load the package
devtools::load_all()

# set the testing directory
test_dir <- "testdata/raster"

# huc4 code
huc <- "1708" # 1.4 GB extracts
# huc <- "0302" # 1.8 GB extracts
# huc <- "0420" # 2.0 GB # WORKS ON ALL PLATFORMS (archive_extract)
# huc <- "0510" # 2.2 GB # FAILS ON ALL PLATFORMS (archive_extract)
# huc <- "0514" # 2.4 GB # WORKS ON LINUX USING 7z shell command
# huc <- "1021" # 2.7 GB

# load raster data from that huc
test_raster <- download_nhdplushr(test_dir, huc, download_files = TRUE,
                                  raster = TRUE)
