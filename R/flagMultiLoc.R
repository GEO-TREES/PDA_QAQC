#' Flag trees with stems spread across multiple locations
#'
#' @param x dataframe containing measurements
#' @param tree_id column name of unique tree IDs in `x`
#' @param coord character vector of length 2 containing the column names for 
#' coordinates (e.g. c("x", "y")) in `x`
#' @param threshold numeric maximum allowable distance between any two stems 
#' belonging to the same tree.
#'
#' @details
#' This function calculates the pairwise Euclidean distance between all stems 
#' associated with a single `tree_id`. If any distance exceeds the `threshold`, 
#' the `tree_id` is flagged.
#'
#' @return A vector of `tree_id` values where stems are spaced further apart 
#' than the allowed threshold.
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2),
#'   gx = c(0, 0.5, 10, 25),
#'   gy = c(0, 0.2, 10, 30)
#' )
#' 
#' # Flag Tree 2 if the stems are more than 5 units apart
#' flagMultiLoc(df, "tree_id", c("gx", "gy"), threshold = 5)
#' # [1] 2
#' 
#' @export
flagMultiLoc <- function(x, tree_id, coord, threshold) {
  
  # Ensure coordinate columns exist
  if (!all(coord %in% names(x))) {
    stop("Columns specified in `coord` not found in `x`")
  }

  # Split coordinate data by tree_id
  tree_list <- split(x[, coord, drop = FALSE], x[[tree_id]])

  # Check each tree
  is_too_spread <- vapply(tree_list, function(df_coord) {
    
    # Skip if not multi-stemmed
    if (nrow(df_coord) < 2) {
      return(FALSE)
    } else {
      # Calculate all pairwise Euclidean distances
      distances <- dist(df_coord)
      
      # Check if any distance is greater than the threshold
      return(any(distances > threshold, na.rm = TRUE))
    }
    
  }, FUN.VALUE = logical(1))

  # Return the names of the trees that failed the check
  out <- names(is_too_spread)[is_too_spread]
  
  # Return
  return(out)
}
