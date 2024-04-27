# Ensure required packages are installed and loaded
ensure_packages <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if (length(missing_packages)) {
    install.packages(missing_packages)
  }

  lapply(packages, function(pkg) {
    if (!require(pkg, character.only = TRUE, quietly = FALSE)) {
      stop(sprintf("Package '%s' failed to load. Please check installation.", pkg))
    }
  })
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
process_las <- function(input_file, output_file) {
  las <- lidR::readLAS(input_file)
  if (is.null(las)) stop("Error reading LAS file: ", input_file, call. = FALSE)

  las %>%
    lidR::filter_duplicates() %>%
    lidR::decimate_points(lidR::random(1000)) %>%
    lidR::classify_ground(algorithm = lidR::csf()) %>%
    lidR::segment_trees(algorithm = lidR::dalponte2016(chm = compute_chm(las), treetops = locate_treetops(compute_chm(las)))) %>%
    {if (!lidR::writeLAS(., output_file)) stop("Failed to write LAS file: ", output_file, call. = FALSE)}

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
  required_packages <- c("terra", "lidR", "RCSF", "future", "magrittr")
  ensure_packages(required_packages)
  
  paths <- configure_paths()
  process_las(paths$input, paths$output)
}

# Execute main function if not sourced from another script
if (!interactive() && !knitr::is_html_output()) {
  main()
}
