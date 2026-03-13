* `flagMultiTaxon()` - Flag trees where stems differ in taxonomic name within a census.
#' Flag trees where stems differ in taxonomic name within a census
#'
#' @param x dataframe containing measurements
#' @param target column name containing taxonomic names in `x`
#' @param tree_id column name of unique tree IDs in `x`
#' @param census_id column name containing census IDs in `x`
#'
#' @details
#' This function identifies trees that have more than one unique taxonomic 
#' name assigned to their constituent stems within a single census.
#'
#' @return A vector of `tree_id` values where taxonomic names are inconsistent 
#' within a census.
#' 
#' @examples
#' df <- data.frame(
#'   tree_id = c(1, 1, 2, 2),
#'   year = c(2010, 2010, 2010, 2010),
#'   sp = c("Quercus alba", "Quercus rubra", "Acer rubrum", "Acer rubrum")
#' )
#' 
#' # Tree 1 has two different species names in 2010
#' flagMultiTaxon(df, "sp", "tree_id", "year")
#' # [1] 1
#' 
#' @export
flagMultiTaxon <- function(x, target, tree_id, census_id) {
  
  if (!target %in% names(x)) {
    stop(paste("Column", target, "not found in dataframe."))
  }

  # Create a grouping factor for Tree + Census
  group_key <- paste(x[[tree_id]], x[[census_id]], sep = "::")
  
  # Split the taxonomic names by this group key
  taxa_list <- split(x[[target]], group_key)
  
  # Check if any group has more than 1 unique name
  is_inconsistent <- vapply(taxa_list, function(taxa) {
    length(unique(taxa)) > 1
  }, FUN.VALUE = logical(1))
  
  # Identify the flagged keys (Tree_Census)
  flagged_keys <- names(is_inconsistent)[is_inconsistent]
  
  if (length(flagged_keys) == 0) return(NULL)
  
  # Extract the tree_id part from the flagged keys
  out <- unique(vapply(strsplit(flagged_keys, "::"), `[`, 1, FUN.VALUE = character(1)))
  
  # Return
  return(out)
}

