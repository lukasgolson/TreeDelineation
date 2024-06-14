# Ensure required packages are installed and loaded
ensure_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages)) {
    install.packages(missing_packages)
  }

  invisible(lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      stop(sprintf("Package '%s' failed to load. Please check installation.", pkg))
    }
  }))
}

# Configure script parameters based on execution environment
configure_paths <- function() {
  if (Sys.getenv("RSTUDIO") == "1") {
    return(list(input = "tree_upright.las", output = "output.las"))
  } else {
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) < 2) stop("Usage: Rscript script.R input_file output_file", call. = FALSE)
    return(list(input = args[1], output = args[2]))
  }
}

# Process LAS files with various lidR and terra functions
# Process LAS files with various lidR functions
process_las <- function(input_file, output_file) {
  las <- lidR::readLAS(input_file)
  if (is.null(las)) stop("Error reading LAS file: ", input_file, call. = FALSE)

  # Filter duplicate points
  las <- lidR::filter_duplicates(las)

  # Decimate points to a fixed number
  las <- lidR::decimate_points(las, lidR::random(1000))

  # Classify ground points using the CSF algorithm
  las <- lidR::classify_ground(las, algorithm = lidR::csf(cloth_resolution = 0.25, class_threshold=0.1))

  # Compute Canopy Height Model (CHM)
  chm <- compute_chm(las)

  # Locate treetops in the CHM
  treetops <- locate_treetops(chm)

  # Segment trees using Dalponte2016 algorithm
  las <- lidR::segment_trees(las, algorithm = lidR::dalponte2016(chm = chm, treetops = treetops))

  # Write the processed LAS file
  if (!lidR::writeLAS(las, output_file)) {
    stop("Failed to write LAS file: ", output_file, call. = FALSE)
  }

  message("Processing complete for: ", output_file)
}


# Compute Canopy Height Model (CHM)
compute_chm <- function(las) {
  dtm <- lidR::grid_terrain(las, algorithm = lidR::tin())
  chm <- lidR::grid_canopy(las, 0.5, lidR::p2r())
  return(chm)
}

# Locate treetops using local maximum filter
locate_treetops <- function(chm) {
  ttops <- lidR::find_trees(chm, algorithm = lidR::lmf(ws = 5))
  return(ttops)
}

# Main function to orchestrate the workflow
main <- function() {
  required_packages <- c("terra", "lidR", "RCSF", "future", "magrittr", "sp", "raster")
  ensure_packages(required_packages)
  
  paths <- configure_paths()
  process_las(paths$input, paths$output)
}

# Execute main function if not sourced from another script
if (!interactive()) {
  main()
}
