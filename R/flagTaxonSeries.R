#' Flag individuals where taxonomic name differs among censuses
#'
#' @param x dataframe containing measurements
#' @param taxon_name column name containing taxonomic names in `x`
#' @param ind_id column name containing individual IDs in `x`
#' @param census_id column name containing census IDs in `x`. Should sort alpha-numerically, either as `Date`, `character` or `numeric` type
#'
#' @return values of `ind_id` where taxonomic name differs among censuses
#'
#' @details
#' `ind_id` must be unique. When processing multiple plots, either run the function separately for each plot, or ensure that `ind_id` is unique across plots.
#' 
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 2, 3, 3),
#'   census = c(1, 2, 1, 2, 1, 2),
#'   sp = c("A", "B", "C", "C", "D", "D")
#' )
#' flagTaxonSeries(df, "sp", "id", "census")
#' # [1] 1
#' 
#' @export
#' 
flagTaxonSeries <- function(x, taxon_name, ind_id, census_id) { 

  # Check columns exist
  if (!all(c(ind_id, census_id, taxon_name) %in% names(x))) {
    stop("Columns 'taxon_name', 'ind_id' or 'census_id' not found in `x`")
  }

  # Ensure input is a data frame
  if (!is.data.frame(x)) {
    x <- as.data.frame(x)
  }
  
  # Identify individual IDs with multiple unique taxonomic names
  n_unique <- as.numeric(
    ave(x[[taxon_name]], x[[ind_id]], FUN = function(y) {
      length(unique(y))
    })
  )
  
  # Extract ind_ids with multiple taxonomic names
  out <- unique(x[which(n_unique > 1), ind_id])
  
  # Return 
  return(out)
}
