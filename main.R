r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

install.packages("renv")
renv::restore()

#renv::snapshot()

library(terra)
library(lidR)



las <- readLAS("tree_upright.las")
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


writeLAS(las, "export.las")
