#' Flag individuals with missing census records
#'
#' @param x dataframe containing measurements
#' @param ind_id column name of unique individual IDs in `x`
#' @param census_id column name containing census IDs in `x`. Should sort alpha-numerically, either as `Date`, `character` or `numeric` type
#' @param census_id_vec vector containing all census IDs. Values should match those in `census_id`
#'
#' @details
#' `ind_id` must be unique. When processing multiple plots, either run the function separately for each plot, or ensure that `ind_id` is unique across plots.
#'
#' When processing multiple plots, if `census_id` is not unique across plots, run the function separately for each plot.
#'
#'#' @return dataframe with missing ind_id and census_id combinations that fall between the first and last recorded census for that individual. 
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2, 2),
#'   year = c(2010, 2020, 2010, 2015, 2020),
#'   plot = "Plot_A"
#' )
#' 
#' years <- c(2010, 2015, 2020)
#' 
#' flagRecordMissing(df, "tree_id", "year", years)
#' 
#' @export
#' 
flagRecordMissing <- function(x, ind_id, census_id, census_id_vec) { 

  # Check columns exist
  if (!all(c(ind_id, census_id) %in% names(x))) {
    stop("Columns 'ind_id' or 'census_id' not found in `x`")
  }

  # Sort census ID vector 
  census_id_vec <- sort(census_id_vec)

  # Split by individual ID
  x_split <- split(x, x[[ind_id]])

  # For each individual:
  out <- do.call(rbind, lapply(x_split, function(y) {

    # Find first and last censuses recorded for this specific individual
    first_census <- min(y[[census_id]])
    last_census <- max(y[[census_id]])

    # Find missing censuses within the range of this individual's lifespan in the data
    missing_census <- census_id_vec[
      !census_id_vec %in% y[[census_id]] & 
      census_id_vec < last_census & 
      census_id_vec > first_census
    ]

    # If any missing censuses:
    if (length(missing_census) > 0) {
      # Use first row of this individual as a template for metadata (Plot ID, etc.)
      new_meas <- data.frame(
        unique(y[[ind_id]]),
        missing_census)
      names(new_meas) <- c(ind_id, census_id)
    } else {
      new_meas <- NULL
    }

    # Return
    new_meas
  }))

  return(out)
}
