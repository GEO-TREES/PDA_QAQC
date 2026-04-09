################## 
# INTERNAL HELPERS 
################## 

#' Build all pairwise census combinations for one stem
#'
#' @param y single-stem subset of `x`, ordered by `census_date`
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param status `r param_status()`
#' @param pom `r param_pom()`
#' 
#' @return data.frame of all pairwise combinations with growth metrics
#'
#' @keywords internal
#' @noRd
#' 
buildPairs <- function(y, census_date, diam, status, pom) {
  idx <- combn(seq_len(nrow(y)), 2L)
  i0 <- idx[1L, ]
  iT <- idx[2L, ]
  d0 <- y[[diam]][i0]
  dT <- y[[diam]][iT]
  t0 <- y[[census_date]][i0]
  tT <- y[[census_date]][iT]
  tdiff <- as.numeric(tT - t0)
  growth_ann <- (dT - d0) * tdiff / 365.25
  data.frame(
    census_t0 = t0,
    census_tT = tT,
    diam_t0 = d0,
    diam_tT = dT,
    status_t0 = y[[status]][i0],
    status_tT = y[[status]][iT],
    pom_t0 = y[[pom]][i0],
    pom_tT = y[[pom]][iT],
    census_dist = tdiff,
    growth_ann = growth_ann
  )
}

#' Greedily identify the minimum set of census dates responsible for bad pairs
#'
#' Iteratively removes the census date that appears most often across bad
#' pairs until no bad pairs remain or only one pair is left.
#'
#' @param p data.frame of pairs for one stem, containing `census_t0`,
#'   `census_tT`, and `bad_meas`
#' @return numeric vector of bad census dates, or `NULL` if none found
#'
#' @keywords internal
#' @noRd
#' 
findBadCensuses <- function(p) {
  bad_census <- c()
  p_tmp <- p
  while (any(p_tmp$bad_meas) && nrow(p_tmp) > 1L) {
    bad_dates <- c(
      p_tmp$census_t0[p_tmp$bad_meas],
      p_tmp$census_tT[p_tmp$bad_meas]
    )
    counts <- sort(table(bad_dates), decreasing = TRUE)
    worst <- names(counts)[1L]
    p_tmp <- p_tmp[p_tmp$census_t0 != worst & p_tmp$census_tT != worst, ]
    bad_census <- c(bad_census, worst)
  }
  if (length(bad_census) == 0L) {
    return(NULL) 
  } else { 
    return(bad_census)
  }
}

###############
# MAIN FUNCTION
###############

#' Correct diameter timelines using mean growth rate imputation
#'
#' Flags anomalous diameter measurements by computing annual growth rates
#' across all pairwise census combinations for each stem, then imputing
#' flagged values by projecting from good measurements using a mean annual
#' growth rate. The mean rate is drawn from the stem's own history if
#' sufficient good increments exist, otherwise from the plot or group mean.
#'
#' @param x `r param_x_stem()`
#' @param ind_id `r param_id("individual stems")`
#' @param plot_id `r param_plot_id()`. To compute mean growth rates
#' @param census_date `r param_census_date()`
#' @param diam `r param_diam()`
#' @param status `r param_status()`
#' @param pom column name in `x` with point of measurement values. Pairs
#'   spanning a POM change are excluded from growth rate calculations but
#'   are not themselves flagged as errors.
#' @param growth_thresh negative and positive annual growth threshold in cm/year. Pairs
#'   with growth exceeding this threshold are flagged. If a vector of length 2 is provided, the values are interpreted as a range. If a vector of length 1 is provided this value is interpreted as an absolute threshold and is applied to both negative and positive growth increments.
#' @param n_stem_thresh minimum number of good growth increments required to
#'   use stem-level rather than group-level mean imputation 
#' @param digits number of decimal places for the corrected diameter column
#' @param comment `r param_comment()`
#'
#' @details
#' Error detection follows a greedy vote procedure. For each stem, all
#' pairwise growth rates are computed. Pairs where growth exceeds the
#' threshold (excluding POM changes) or where either measurement is missing
#' are flagged. The census date appearing most frequently across bad pairs is
#' removed iteratively until no bad pairs remain. Missing diameter values are
#' also flagged unconditionally.
#'
#' Imputation projects forward and backward from every good anchor
#' measurement using the mean rate, then averages across all projections. If
#' a stem has `>= n_stem_thresh` good increments its own mean rate is used;
#' otherwise the group mean is used. Stems with no good measurements at all
#' cannot be imputed and are silently omitted from the output.
#'
#' @return dataframe of rows in `x` where a diameter correction was applied,
#'   containing the original `ind_id`, `census_date`, and `diam` columns plus:
#'   - `diam_cor`: imputed diameter (rounded to `digits`)
#'   - `correction_method`: `"stem mean"` or `"group mean"`
#'   - `comment` (if provided)
#'
#' @export
#'
#' @examples
#' x <- data.frame(
#'   plot_id = "P1",
#'   stem_id = rep(c("A", "B"), each = 5),
#'   year = rep(c(2000, 2005, 2010, 2015, 2020), 2),
#'   diameter = c(10, 11, 30, 13, 14, # spike at 2010
#'   10, 11, 12, 13, 14),
#'   alive = TRUE,
#'   pom = 1.3
#' )
#'
#' correctDiameterSeries(x,
#'   ind_id = "stem_id",
#'   plot_id = "plot_id",
#'   census_date = "year",
#'   diam = "diameter",
#'   status = "alive",
#'   pom = "pom"
#' )
#'
correctDiameterSeries <- function(x, ind_id, plot_id, census_date, diam, status,
  pom, growth_thresh  = 4, n_stem_thresh = 3L, digits = 1L, comment = NULL) {

  # Input checks -------------------------------------------------------------------
  columnCatch(x, ind_id, plot_id, census_date, diam, status, pom)
  x[[diam]] <- classCatch(x[[diam]], "numeric")
  x[[census_date]] <- classCatch(x[[census_date]], "Date")
  x[[status]] <- classCatch(x[[status]], "logical")
  x[[pom]] <- classCatch(x[[pom]], "numeric")
  growth_thresh <- classCatch(growth_thresh, "numeric")
  n_stem_thresh <- classCatch(n_stem_thresh, "numeric")
  digits <- classCatch(digits, "numeric")

  if (length(growth_thresh) == 1) {
    growth_thresh <- c(-abs(growth_thresh), abs(growth_thresh))
  } else if (length(growth_thresh) > 2) { 
    stop("'growth_thresh' must be a vector of length 1 or 2")
  }

  # Sort, build compound keys ------------------------------------------------------
  x <- x[do.call(order, x[, c(ind_id, census_date), drop = FALSE]), ]
  x$.stem_key <- do.call(paste, c(x[, ind_id, drop = FALSE], sep = "::"))
  x$.group_key <- do.call(paste, c(x[, plot_id, drop = FALSE], sep = "::"))

  # Filter to multi-census stems ---------------------------------------------------
  n_cens <- tapply(x$.stem_key, x$.stem_key, length)
  x_multi <- x[x$.stem_key %in% names(n_cens[n_cens > 1L]), ]
  x_split <- split(x_multi, x_multi$.stem_key)

  # Build pairwise growth tables ---------------------------------------------------
  pairs_list <- lapply(x_split, function(y) {
    p <- buildPairs(y, census_date, diam, status, pom)
    p$.stem_key <- y$.stem_key[1L]
    p$.group_key <- y$.group_key[1L]
    p$pom_change <- p$pom_t0 != p$pom_tT
    p$bad_meas <- (
      (!is.na(p$growth_ann) &
        (p$growth_ann > growth_thresh[2] | p$growth_ann < growth_thresh[1]) &
        !p$pom_change) |
      is.na(p$diam_t0) | is.na(p$diam_tT)
    )
    p
  })
  pairs <- do.call(rbind, pairs_list)
  rownames(pairs) <- NULL

  # Identify bad census dates ------------------------------------------------------
  # Only process stems that have at least one bad pair AND one non-missing pair
  has_bad <- tapply(pairs$bad_meas, pairs$.stem_key, any)
  has_good <- tapply(!pairs$bad_meas, pairs$.stem_key, any)
  bad_stems <- names(has_bad[has_bad & has_good])

  pairs_bad_split <- split(
    pairs[pairs$.stem_key %in% bad_stems, ],
    pairs[pairs$.stem_key %in% bad_stems, ]$.stem_key
  )

  bad_census_df <- do.call(rbind, lapply(names(pairs_bad_split), function(sk) {
    bc <- as.Date(findBadCensuses(pairs_bad_split[[sk]]))
    if (is.null(bc)) {
      return(NULL)
    } else {
      data.frame(.stem_key = sk, bad_census = bc, stringsAsFactors = FALSE)
    }
  }))

  # Add unconditionally flagged missing measurements
  missing_df <- x_multi[is.na(x_multi[[diam]]), c(".stem_key", census_date)]
  names(missing_df)[names(missing_df) == census_date] <- "bad_census"

  all_bad <- unique(rbind(bad_census_df, missing_df))

  if (nrow(all_bad) == 0L) {
    return(NULL)
  }

  # Compute mean growth rates from good pairs -------------------------------------
  # A pair is "good" if neither endpoint is flagged, both are alive, and
  # there is no POM change
  bad_lookup <- paste(all_bad$.stem_key, all_bad$bad_census)
  good_pairs <- pairs[
    !paste(pairs$.stem_key, pairs$census_t0) %in% bad_lookup &
      !paste(pairs$.stem_key, pairs$census_tT) %in% bad_lookup &
      pairs$status_t0 == TRUE & pairs$status_tT == TRUE &
      !pairs$pom_change &
      !is.na(pairs$growth_ann),
  ]

  group_mean <- tapply(good_pairs$growth_ann, good_pairs$.group_key, mean, na.rm = TRUE)

  stem_mean <- tapply(good_pairs$growth_ann, good_pairs$.stem_key, mean, na.rm = TRUE)
  stem_n <- tapply(good_pairs$growth_ann, good_pairs$.stem_key, function(g) { sum(!is.na(g)) })

  # Impute bad measurements -------------------------------------------------------
  bad_stem_keys <- unique(all_bad$.stem_key)

  out <- do.call(rbind, lapply(bad_stem_keys, function(sk) {
    y <- x_split[[sk]]
    bad_dates <- all_bad$bad_census[all_bad$.stem_key == sk]
    gk <- y$.group_key[1L]

    # Anchor: good measurements for this stem
    good_mask <- !y[[census_date]] %in% bad_dates & !is.na(y[[diam]])

    if (!any(good_mask)) {
      return(NULL)
    }

    anchor_diam <- y[[diam]][good_mask]
    anchor_date <- y[[census_date]][good_mask]

    # Choose imputation rate and label
    if (sk %in% names(stem_n)) {
      n_good <- stem_n[[sk]] 
    } else {
      n_good <- 0L
    }
    use_stem <- n_good >= n_stem_thresh

    if (use_stem && sk %in% names(stem_mean)) {
      rate <- stem_mean[[sk]]
    } else if (gk %in% names(group_mean)) {
      rate <- group_mean[[gk]]
    } else {
      rate <- NA_real_
    }

    if (use_stem) {
      meth <- "stem mean" 
    } else { 
      meth <- "group mean"
    }

    if (is.na(rate)) {
      return(NULL)
    }

    # Project from every anchor to every bad date, then average projections
    imputed <- vapply(bad_dates, function(bd) {
      preds <- anchor_diam + rate * (as.numeric(bd - anchor_date) / 365.25)
      mean(preds, na.rm = TRUE)
    }, numeric(1L))

    # Match back to original rows
    row_idx <- match(bad_dates, y[[census_date]])
    rows <- y[row_idx, unique(c(ind_id, plot_id, census_date, diam)), drop = FALSE]
    rows$diam_cor <- round(imputed, digits = digits)
    rows$correction_method <- meth
    rows
  }))

  if (is.null(out) || nrow(out) == 0L) {
    return(NULL)
  }

  if (!is.null(comment)) {
    out$comment <- comment
  }
  rownames(out) <- NULL

  # Return
  return(out)
}

