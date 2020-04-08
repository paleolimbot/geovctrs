
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
geo_print <- function(x, ..., short = FALSE, col = TRUE) {
  UseMethod("geo_print")
}

#' @export
geo_print.default <- function(x, ..., short = FALSE, col = TRUE) {
  geovctr <- as_geovctr(x)
  geo_print(
    as_geovctr(x),
    ...,
    classes = paste0(class(x)[1], "...", class(geovctr)[1]),
    short = short,
    col = col
  )
  invisible(x)
}

#' @export
geo_print.geovctr <- function(x, ..., classes = class(x), short = FALSE, col = TRUE) {
  cat(paste0("<", classes[1], "[", vec_size(x), "]>\n"))

  if (rlang::is_named(x) || !col) {
    out <- stats::setNames(geo_format(x, ..., short = short, col = FALSE), names(x))
    print(out, quote = FALSE)
  } else {
    summary <- geo_summary(x)
    print_default_colour(
      geo_format_summary(summary, class(x), short = short, col = FALSE),
      geo_format_summary(summary, class(x), short = short, col = TRUE),
      ...
    )
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
  geo_format_summary(summary, class(x), short = short, col = col)
}

#' @export
as.character.geovctr <- function(x, ...) {
  # this gives a better summary in the viewer than
  # well-known text, and will eventually be very fast
  format(x, ...)
}


geo_format_summary <- function(summary, class, short, col) {
  na <- format_na_type(class, col = col)
  sym <- maybe_grey(
    geometry_type_symbol(
      summary$geometry_type,
      (summary$coordinate_dimensions == 3) & !summary$is_empty,
      short = short
    ),
    col = col
  )

  coord_str <- ifelse(
    summary$is_empty,
    maybe_grey("EMPTY", col = col),
    ifelse(
      summary$geometry_type == "geometrycollection",
      "",
      ifelse(
        summary$geometry_type == "point",
        maybe_blue(format(summary$first_coordinate), col = col),
        paste0(
          maybe_blue(format(summary$first_coordinate), col = col),
          maybe_grey("\U2026+", summary$n_coordinates - 1, col = col)
        )
      )
    )
  )

  n_sub_geom_str <- ifelse(
    (summary$geometry_type == "geometrycollection")  |
    (is_multi_geometry_type(summary$geometry_type) & !summary$is_empty),
    maybe_grey(paste0("[", summary$n_geometries, "]"), col = col),
    ""
  )

  ifelse(
    is.na(summary$is_empty),
    na,
    paste0(sym, n_sub_geom_str, " ", coord_str)
  )
}

# dymamically exported...see zzz.R
pillar_shaft.geovctr <- function(x, ...) {
  pillar::new_pillar_shaft_simple(geo_format(x, short = TRUE, col = TRUE))
}


print_default_colour <- function(x_no_col, x_col, width = getOption("width")) {
  if (length(x_no_col) == 0) {
    return()
  }

  # calulate widths
  label_width <- nchar(length(x_no_col)) + 3
  max_item_width <- max(nchar(x_no_col))
  item_width <- max((width + 1 - label_width) %/% (max_item_width + 1), 1)
  item_rows <- ((length(x_no_col) - 1) %/% item_width) + 1

  # make everything max width
  x_col <- str_pad_col_right(x_no_col, x_col, width = max_item_width)

  for (row in seq_len(item_rows)) {
    first_index <- ((row - 1) * item_width) + 1
    last_index <- min(first_index + item_width - 1, length(x_no_col))
    cat(
      paste0(
        str_pad_right(paste0("[", first_index, "]"), width = label_width),
        paste0(
          x_col[first_index:last_index],
          collapse = " "
        ),
        "\n"
      )
    )
  }
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
      "geometrycollection" = "<\U2234 / \U25B3>"
    )[type]
    txt <- unname(sym)
  } else {
    txt <- toupper(type)
  }

  ifelse(
    use_z,
    paste0(txt, " Z"),
    txt
  )
}

format_na_type <- function(class, col = TRUE) {
  maybe_red(paste0("NA_", gsub("geo_", "", class[1]), "_"), col = col)
}

is_multi_geometry_type <- function(type) {
  grepl("^(multi|geometrycollection)", type)
}

maybe_blue <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_blue(...)  # nocov
  } else {
    paste0(...)
  }
}

maybe_red <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_red(...) # nocov
  } else {
    paste0(...)
  }
}

maybe_grey <- function(..., col = TRUE) {
  if (col && crayon::has_color()) {
    cli::col_grey(...) # nocov
  } else {
    paste0(...)
  }
}

str_pad_col_right <- function(x_no_col, x_col, width, pad = " ") {
  ifelse(
    nchar(x_no_col) < width,
    paste0(x_col, strrep(pad, width - nchar(x_no_col))),
    x_col
  )
}

str_pad_right <- function(x, width, pad = " ") {
  ifelse(
    nchar(x) < width,
    paste0(x, strrep(pad, width - nchar(x))),
    x
  )
}
