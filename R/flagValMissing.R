#' Flag measurements with missing values, optionally with conditions
#'
#' @param x dataframe containing measurements
#' @param target column name in `x` where missing values will be checked 
#' @param cond a logical expression defining an optional condition based on other columns in `x`
#'
#' @details
#' Identifies row indices where `target` is `NA` or empty (""), only for rows where the criteria in `cond` are met. Use `&` for #' simultaneous conditions and `|` for alternative conditions.
#'
#' @return integer vector of row indices from `x` that fail the check.
#' 
#' @examples
#' df <- data.frame(
#'   dbh = c(10, NA, 8, NA, NA),
#'   status = c("alive", "alive", "dead", "alive", "dead")
#' )
#' 
#' # Find rows where DBH is missing but the tree is alive
#' flagValMissing(df, "dbh", status == "alive")
#' # [1] 2 4
#' 
#' @export
flagValMissing <- function(x, target, cond = NULL) {
  
  # Check columns exist
  if (!target %in% names(x)) {
    stop(paste0("Column '", target, "' not found in `x`"))
  }

  # Capture the condition
  cond_expr <- substitute(cond)
  
  # Evaluate condition within the data frame environment
  if (missing(cond)) {
    in_scope <- rep(TRUE, nrow(x))
  } else {
    in_scope <- eval(cond_expr, x, parent.frame())
    # Ensure NAs in condition result in FALSE
    in_scope[is.na(in_scope)] <- FALSE
  }
  
  # Check for missingness in the target column
  val <- x[[target]]

  # NA check
  is_missing <- is.na(val)

  # Check for empty/whitespace strings if character
  if (is.character(val)) {
    is_missing <- is_missing | trimws(val) == ""
  }

  # Check for non-finite values (NaN, Inf) if numeric
  if (is.numeric(val)) {
    # !is.finite returns TRUE for NA, NaN, Inf, and -Inf
    is_missing <- !is.finite(val)
  } 

  # Identify indices where both are true
  out <- which(in_scope & is_missing)
  
  # Return
  return(out)
}
