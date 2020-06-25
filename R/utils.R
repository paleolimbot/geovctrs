
stop_for_non_parseable <- function(x) {
  if (!is.null(attr(x, "problems"))) {
    n <- nrow(attr(x, "problems"))
    abort(
      sprintf(
        "%s geometr%s failed to parse.",
        n,
        if (n == 1) "y" else "ies"
      ),
      class = "parse_error"
    )
  }
  invisible(x)
}

n_distinct <- function(x) {
  length(unique(x))
}

rep_along_or_fail <- function(x, template) {
  x_quo <- rlang::enquo(x)
  template_quo <- rlang::enquo(template)

  if (vec_size(x) == 1) {
    vec_rep(x, times = vec_size(template))
  } else if (vec_size(x) != vec_size(template)) {
    x_label <- rlang::as_label(x_quo)
    abort(
      sprintf(
        "`%s` must be length 1 or the same length as `%s` (%s)",
        rlang::as_label(x_quo), rlang::as_label(template_quo), vec_size(template)
      ),
      class = "rep_len_error"
    )
  } else {
    x
  }
}

rep_len_or_fail <- function(x, size) {
  x_quo <- rlang::enquo(x)

  if (size == 1 || vec_size(x) == size) {
    x
  } else if (vec_size(x) == 1) {
    vec_rep(x, times = size)
  } else {
    x_label <- rlang::as_label(x_quo)
    abort(
      sprintf(
        "`%s` must be length 1 or %s",
        rlang::as_label(x_quo), size
      ),
      class = "rep_len_error"
    )
  }
}

as_part_identifier <- function(xy, ...) {
  # recycle
  all <- vec_recycle_common(xy = xy, ...)

  if (length(all) == 1 || length(all$xy) == 0) {
    return(all)
  }

  # make in-order integers of all ...
  group_vars <- lapply(all[-1], function(x) {
    idx <- x[!duplicated(x)]
    match(x, idx)
  })

  # order so that groups are together
  row_order <- do.call(order, group_vars)

  # recombine and reorder
  lapply(c(all[1], group_vars), `[`, row_order)
}

summarise_srids <- function(srids) {
  # NA here means "safe to inherit"
  srid <- setdiff(unique(srids), NA_integer_)
  final_srid <- srid[1]
  if (length(srid) > 1) {
    rlang::warn(
      sprintf(
        "Summarising SRID: (%s) => %s",
        paste(srid, collapse = ", "),
        final_srid
      )
    )
  }

  final_srid
}

grepl_na <- function(pattern, x, ...) {
  result <- grepl(pattern, x, ...)
  result[is.na(x)] <- NA
  result
}

recycle_parameter <- function(x, ...) {
  if (vec_size(x) == 1) {
    vec_recycle_common(...)
  } else {
    vec_recycle_common(..., .size = vec_size(x))
  }
}
