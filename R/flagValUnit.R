#' Flag censuses with likely unit inconsistencies
#'
#' @param x dataframe containing stem measurements
#' @param target column name in `x` of the numeric variable to check
#' @param census_id column name containing census IDs in `x`
#' @param threshold numeric multiplier to define an outlier census mean. Defaults to 5
#' @param cv_threshold numeric threshold for Coefficient of Variation to detect 
#'     mixed units within a census. Defaults to 50
#'
#' @details
#' This function flags censuses in two ways:
#' 1. **Among-census errors**: The mean of a census is an order of magnitude 
#'    different from the median of all census means.
#' 2. **Within-census errors**: The variation (CV) within a single census is 
#'    extremely high, suggesting a mixture of units (e.g. cm and mm).
#'
#' @return A vector of `census_id` values flagged for inconsistent units.
#' 
#' @examples
#' # 2020 has a mix of units (half 20, half 200)
#' df <- data.frame(
#'   year = c(rep(2010, 10), rep(2020, 10)),
#'   dbh = c(rnorm(10, 20, 2), rnorm(5, 20, 2), rnorm(5, 200, 20))
#' )
#' 
#' flagValUnit(df, "dbh", "year")
#' # [1] 2020
#' 
#' @export
flagValUnit <- function(x, target, census_id, threshold = 5, cv_threshold = 50) {
  
  if (!is.numeric(x[[target]])) {
    stop("The target column must be numeric to check for unit inconsistencies.")
  }

  # Calculate Mean and CV per census
  # We use aggregate to get both stats
  stats_per_census <- aggregate(
    x[[target]], 
    by = list(census = x[[census_id]]), 
    FUN = function(v) {
      m <- mean(v, na.rm = TRUE)
      s <- sd(v, na.rm = TRUE)
      cv <- (s / m) * 100
      return(c(mean = m, cv = cv))
    })
  
  # Flatten the matrix returned by aggregate
  census_ids <- stats_per_census[[1]]
  means <- stats_per_census[[2]][, "mean"]
  cvs <- stats_per_census[[2]][, "cv"]
  
  # Check for Between-Census Shift (Order of Magnitude)
  baseline_mean <- median(means, na.rm = TRUE)
  bad_mean <- means > (baseline_mean * threshold) | means < (baseline_mean / threshold)
  
  # Check for within-census mixture (high variance / bimodality)
  bad_cv <- cvs > cv_threshold
  
  # Combine flags
  out <- census_ids[bad_mean | bad_cv]
  
  return(out)
}
