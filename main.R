r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

install.packages("renv")
renv::restore()

#renv::snapshot()

library(terra)
library(lidR)

isRStudio <- Sys.getenv("RSTUDIO") == "1"

# If running in RStudio, set default values
if (isRStudio) {
  input_file <- "tree_upright.las"
  output_file <- "output.las"
} else {
  # Check if command line arguments are provided
  if (length(commandArgs(trailingOnly = TRUE)) < 2) {
    stop("Usage: Rscript script.R input_file output_file")
  }
  
  # Retrieve input and output file paths from command line arguments
  input_file <- commandArgs(trailingOnly = TRUE)[1]
  output_file <- commandArgs(trailingOnly = TRUE)[2]
}

# print input and output file paths
print(input_file)
print(output_file)


las <- readLAS(input_file)
las_check(las)

las <- filter_duplicates(las)

las <- decimate_points(las, random(1000))

las <- classify_ground(las, algorithm = csf())

dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())

chm <- rasterize_canopy(las, res = 1, algorithm = p2r())

f <- function(x) {x * 0.1 + 3}
heights <- seq(0,30,5)
ws <- f(heights)


ttops <- locate_trees(chm, algorithm = lmf(ws = 2.5))


las <- segment_trees(las = las, algorithm = dalponte2016(chm = chm, treetops = ttops))


length(unique(las$treeID) |> na.omit())


writeLAS(las, output_file)
