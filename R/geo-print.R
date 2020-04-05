
#' Format and print geovctrs
#'
#' Format and prints the [geo_summary()] of an geovctr.
#'
#' @inheritParams geo_bbox
#' @param ... Passed to [geo_format()]
#' @param short Use to print a shorter summary
#' @param col Use to colourize the output
#'
#' @export
#'
#' @examples
#' geo_format(head(geo_nc))
#' geo_print(head(geo_nc))
#'
geo_print <- function(x, ..., short = FALSE, col  = TRUE) {
  UseMethod("geo_print")
}

#' @export
geo_print.default <- function(x, ..., short = FALSE, col  = TRUE) {
  geo_print(as_geovctr(x), ..., short = short, col = col)
}

#' @export
geo_print.geovctr <- function(x, ..., short = FALSE, col  = TRUE) {
  cat(paste0("<", class(x)[1], "[", vec_size(x), "]>\n"))

  # using for() so the user can cancel
  for (i in seq_len(vec_size(x))) {
    cat(paste0("[", i, "] ", geo_format(x[i], short = short, col  = col), "\n"))
  }

  invisible(x)
}

#' @rdname geo_print
#' @export
geo_format <- function(x, ..., short = FALSE, col = FALSE) {
  UseMethod("geo_format")
}

#' @export
geo_format.default <- function(x, ..., short = FALSE, col = FALSE) {
  geo_format(as_geovctr(x), ..., short = short, col = col)
}

#' @export
geo_format.geovctr <- function(x, ..., short = FALSE, col = FALSE) {
  if (vec_size(x) == 0) {
    return(character(0))
  }

  summary <- geo_summary(x)

  na <- format_na_type(x, col = col)
  sym <- maybe_green(
    geometry_type_symbol(
      summary$geometry_type,
      (summary$coordinate_dimensions == 3) & !summary$is_empty,
      short = short
    ),
    col = col
  )

  n_sub_geom_str <- ifelse(
    is_multi_geometry_type(summary$geometry_type) & !summary$is_empty,
    maybe_grey(paste0("[", summary$n_geometries, "]"), col = col),
    ""
  )

  n_coord_str <- ifelse(
    summary$is_empty | grepl("point", summary$geometry_type),
    "",
    maybe_grey(paste0("{", summary$n_coordinates, "}"), col = col)
  )

  rect_str <- ifelse(
    summary$is_empty,
    maybe_yellow("EMPTY", col = col),
    ifelse(
      summary$geometry_type == "point",
      maybe_blue(format(geo_xy(field(summary$envelope, "xmin"), field(summary$envelope, "ymin"))), col = col),
      maybe_blue(format(summary$envelope), col = col)
    )
  )

  ifelse(
    geo_is_missing(x),
    na,
    paste0(sym, n_sub_geom_str, n_coord_str, " ", rect_str)
  )
}

geometry_type_symbol <- function(type, use_z, short = FALSE) {
  if (short) {
    sym <- c(
      "point" = "\U00B7",
      "linestring" = "/",
      # "linearring",
      "polygon" = "\U25B3",
      "multipoint" = "\U2234",
      "multilinestring" = "//",
      "multipolygon" = "\U25B3\U25BD",
      "geometrycollection" = "\U2234, /, \U25B3"
    )[type]
    txt <- unname(sym)
  } else {
    txt <- toupper(type)
  }

  ifelse(
    use_z,
    paste0("<", txt, " Z>"),
    paste0("<", txt, ">")
  )

}

format_na_type <- function(x, col = TRUE) {
  maybe_red(paste0("NA_", gsub("geo_", "", class(x)[1]), "_"), col = col)
}

is_multi_geometry_type <- function(type) {
  grepl("^(multi|geometrycollection)", type)
}

maybe_blue <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_blue(...)
  } else {
    paste0(...)
  }
}

maybe_red <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_red(...)
  } else {
    paste0(...)
  }
}

maybe_yellow <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_yellow(...)
  } else {
    paste0(...)
  }
}

maybe_green <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_green(...)
  } else {
    paste0(...)
  }
}

maybe_grey <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_grey(...)
  } else {
    paste0(...)
  }
}
