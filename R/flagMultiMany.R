#' Flag trees where the number of stems exceeds a threshold
#'
#' @param x dataframe containing stem measurements
#' @param tree_id column name of unique tree IDs in `x`
#' @param census_id column name containing census IDs in `x`
#' @param threshold numeric maximum allowable number of stems for a single tree 
#' in a single census.
#'
#' @details
#' This function counts the number of stems associated with each `tree_id` 
#' for every `census_id`. If the count exceeds the `threshold` in any census, 
#' the `tree_id` is flagged.
#'
#' @return A vector of unique `tree_id` values that exceed the stem threshold.
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 1, 2, 2),
#'   year = c(2010, 2010, 2010, 2010, 2010),
#'   dbh = c(10, 12, 14, 20, 22)
#' )
#' 
#' # Flag trees with more than 2 stems
#' flagMultiMany(df, "tree_id", "year", threshold = 2)
#' # [1] 1
#' 
#' @export
flagMultiMany <- function(x, tree_id, census_id, threshold) {
  
  if (!all(c(tree_id, census_id) %in% names(x))) {
    stop("Specified tree_id or census_id columns not found in the dataframe.")
  }

  # Create a table of counts for each Tree/Census combination
  counts <- table(x[[tree_id]], x[[census_id]])
  
  # Identify rows (tree_ids) where any census count > threshold
  bad_trees_logical <- apply(counts, 1, function(row) any(row > threshold))
  
  # Extract tree_id
  out <- names(bad_trees_logical)[bad_trees_logical]
  
  # Return
  return(out)
}
