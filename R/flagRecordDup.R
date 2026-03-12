#' Flag duplicated measurement records within a census
#'
#' @param x dataframe containing measurements
#' @param ind_id column name of unique individual IDs in `x`
#' @param census_id column name containing census IDs in `x`
#'
#' @details
#' Identifies cases where a `ind_id` is recorded multiple times 
#' within the same `census_id`. It returns the indices of all rows involved in 
#' the duplication.
#'
#' @return An integer vector of row indices where `ind_id` is duplicated 
#' within the same census.
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 2, 3, 1, 1),
#'   census = c(2010, 2010, 2010, 2010, 2010),
#'   dbh = c(10, 15, 20, 11, 14)
#' )
#' 
#' # Tree 1 is measured twice in 2010
#' flagRecordDup(df, "tree_id", "census")
#' # [1] 1 4
#' 
#' @export
flagRecordDup <- function(x, ind_id, census_id) {
  
  # Check columns exist
  if (!all(c(ind_id, census_id) %in% names(x))) {
    stop("Columns 'ind_id' or 'census_id' not found in `x`")
  }

  # Create combined key of individual and census
  # Find duplicates 
  out <- unique(x[
    which(duplicated(paste(x[[ind_id]], x[[census_id]], sep = "::"))),
    ind_id])
  
  return(out)
}
