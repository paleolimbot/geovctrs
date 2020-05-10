
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
geo_print <- function(x, ..., col = TRUE) {
  UseMethod("geo_print")
}

#' @export
geo_print.default <- function(x, ..., col = TRUE) {
  geovctr <- as_geovctr(x)
  geo_print(
    as_geovctr(x),
    ...,
    classes = paste0(class(x)[1], "...", class(geovctr)[1]),
    col = col
  )
  invisible(x)
}

#' @export
geo_print.geovctr <- function(x, ..., classes = class(x), col = TRUE) {
  cat(paste0("<", classes[1], "[", vec_size(x), "]>\n"))

  if (length(x)  == 0) {
    return(invisible(x))
  }

  if (rlang::is_named(x) || !col) {
    out <- stats::setNames(geo_format(x, ..., short = short, col = FALSE), names(x))
    print(out, quote = FALSE)
  } else {
    print_default_colour(
      geo_format(x, ..., col = FALSE),
      geo_format(x, ..., col = TRUE),
      ...
    )
  }

  invisible(x)
}

#' @rdname geo_print
#' @export
geo_format <- function(x, ..., col = FALSE) {
  UseMethod("geo_format")
}

#' @export
geo_format.default <- function(x, ..., col = FALSE) {
  geo_format(as_geovctr(x), ..., short = short, col = col)
}

#' @export
geo_format.geovctr <- function(x, ..., col = FALSE) {
  format(x, col = col)
}

#' @export
as.character.geovctr <- function(x, ...) {
  # this gives a fast summary in the viewer for non-WKT types
  format(x, ...)
}

#' @export
print.geovctr <- function(x, ...) {
  geo_print(x, ...)
}

# dymamically exported...see zzz.R
pillar_shaft.geovctr <- function(x, ...) {
  formatted <- format(x, ..., col = TRUE, bracket = FALSE)
  pillar::new_pillar_shaft_simple(formatted)
}

print_default_colour <- function(x_no_col, x_col, ..., width = getOption("width")) {
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

format_wkt_summary <- function(x, ..., trunc_width = 40, col = FALSE) {
  # collapse whitespace, remove leading whitespace
  x <- gsub("\\s+", " ", gsub("^\\s*", "", gsub("\\s*$", "", x)))
  trunc <- substr(x, 1, trunc_width - 1)
  width <- nchar(x)

  abbreved <- ifelse(
    width > (trunc_width - nchar(cli::symbol$ellipsis)),
    paste0(trunc, cli::symbol$ellipsis),
    x
  )

  geom_type_match <- regexpr("[A-Z ]+", abbreved)
  geom_type_start <- as.integer(geom_type_match)
  geom_type_end <- geom_type_start + attr(geom_type_match, "match.length") - 1

  formatted <- ifelse(
    geom_type_match != -1,
    paste0(
      maybe_grey(substr(abbreved, geom_type_start, geom_type_end), col = col),
      maybe_blue(substr(abbreved, geom_type_end + 1, trunc_width), col = col)
    ),
    maybe_blue(abbreved, col = col)
  )

  ifelse(
    is.na(x),
    maybe_red("NA_wkt_", col = col),
    formatted
  )
}

geometry_type_symbol <- function(type, use_z, short = FALSE) {
  if (short) {
    sym <- if (use_utf8()) {
      c(
        "point" = "\U00B7",
        "linestring" = "/",
        # "linearring",
        "polygon" = "\U25B3",
        "multipoint" = "\U2234",
        "multilinestring" = "//",
        "multipolygon" = "\U25B3\U25BD",
        "geometrycollection" = "<\U2234 / \U25B3>"
      )[type]
    } else {
      c(
        "point" = "pt",
        "linestring" = "ls",
        # "linearring",
        "polygon" = "ply",
        "multipoint" = "mpt",
        "multilinestring" = "mls",
        "multipolygon" = "mply",
        "geometrycollection" = "clctn"
      )[type]
    }
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
  maybe_red(paste0("NA_", gsub("geo(vctrs)?_", "", class[1]), "_"), col = col)
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

# mimics the logic from cli:::.onLoad() for symbol ASCII
use_utf8 <- function() {
  isTRUE(cli::symbol$tick == "\U2713")
}
