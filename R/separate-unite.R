
#' Unite and separate components of geometries
#'
#' These functions work like [tidyr::separate()] and [tidyr::unite()],
#' combining information from multiple columns into single column. You
#' can also create and separate columns using [dplyr::mutate()] and
#' the constructors ([geo_xy()], [geo_segment()], and [geo_rect()]).
#'
#' @param data A [tibble::tibble()] or data frame
#' @param col A column name for the united object
#' @param into Column names for the separated vectors
#' @param x,y,z,xmin,ymin,xmax,ymax,start,end Column specifications for x and y coordinate
#'   columns, respectively. Use [dplyr::select()] syntax. These correspond
#'   to the arguments in [geo_xy()], [geo_segment()], and [geo_rect()].
#' @param remove Use `remove = FALSE` to keep the source columns
#'   in the output.
#'
#' @return `data`, with new column(s) `col`/`into`
#' @export
#'
#' @examples
#' tbl <- tibble(a = 1, b = 2, c = 3, d = 4)
#'
#' (united <- unite_xy(tbl, "xy", a, b))
#' separate_xy(united, xy)
#'
#' (united <- unite_xyz(tbl, "xyz", a, b, c))
#' separate_xyz(united, xyz)
#'
#' (united <- unite_rect(tbl, "rect", a, b, c, d))
#' separate_rect(united, rect)
#'
#' # need to modify geo_segment() constructor!
#' # (united <- unite_segment(tbl, "seg", a, b, c, d))
#' # separate_segment(united, seg)
#'
unite_xy <- function(data, col, x, y, remove = TRUE) {
  unite_rcrd(data, col, {{ x }}, {{ y }}, remove = remove, constructor = geo_xy)
}

#' @rdname unite_xy
#' @export
separate_xy <- function(data, col, into = c("x", "y"), remove = TRUE) {
  separate_vctrs_rcrd(
    data, {{ col }},
    into = into,
    remove = remove,
    ptype = geo_xy(),
    fields = c("x", "y")
  )
}

#' @rdname unite_xy
#' @export
unite_xyz <- function(data, col, x, y, z, remove = TRUE) {
  unite_rcrd(data, col, {{ x }}, {{ y }}, {{ z }}, remove = remove, constructor = geo_xyz)
}

#' @rdname unite_xy
#' @export
separate_xyz <- function(data, col, into = c("x", "y", "z"), remove = TRUE) {
  separate_vctrs_rcrd(
    data, {{ col }},
    into = into,
    remove = remove,
    ptype = geo_xyz(),
    fields = c("x", "y", "z")
  )
}

#' @rdname unite_xy
#' @export
unite_segment <- function(data, col, x0, y0, x1, y1, remove = TRUE) {
  unite_rcrd(
    data, col,
    {{ x0 }}, {{ y0 }},
    {{ x1 }}, {{ y1 }},
    remove = remove, constructor = geo_segment
  )
}

#' @rdname unite_xy
#' @export
separate_segment <- function(data, col, into = c("x0", "y0", "x1", "y1"), remove = TRUE) {
  separate_vctrs_rcrd(
    data, {{ col }},
    into = into,
    remove = remove,
    ptype = geo_segment(),
    fields = c("x0", "y0", "x1", "y1")
  )
}

#' @rdname unite_xy
#' @export
unite_rect <- function(data, col, xmin, ymin, xmax, ymax, remove = TRUE) {
  unite_rcrd(
    data, col,
    {{ xmin }}, {{ ymin }},
    {{ xmax }}, {{ ymax }},
    remove = remove,
    constructor = geo_rect
  )
}

#' @rdname unite_xy
#' @export
separate_rect <- function(data, col, into = c("xmin", "ymin", "xmax", "ymax"),
                          remove = TRUE) {
  separate_vctrs_rcrd(
    data, {{ col }},
    into = into,
    remove = remove,
    ptype = geo_rect(),
    fields = c("xmin", "ymin", "xmax", "ymax")
  )
}

unite_rcrd <- function(data, col, ..., remove, constructor)  {
  stopifnot(
    length(col) ==  1, is.character(col)
  )

  dots <- rlang::quos(...)
  cols <- lapply(dots, tidyselect::eval_select, data, strict = TRUE)
  lengths <- vapply(cols, length, integer(1))
  if (any(lengths != 1)) {
    abort("All column references must refer to a unique column")
  }

  values <- lapply(cols, function(x) data[[x]])
  rcrd <- rlang::exec(constructor, !!!values)
  insert_column(data, tibble(!!col := rcrd), source_cols = unlist(cols), remove = remove)
}

separate_vctrs_rcrd <- function(data, col, into, remove, ptype, fields) {
  ptype_data <- vec_data(ptype)
  rcrd_col <- tidyselect::eval_select(enquo(col), data, strict = TRUE)

  stopifnot(
    length(rcrd_col) == 1,
    length(into) == length(fields),
    is.character(into)
  )

  rcrd <- data[[rcrd_col]]
  vec_assert(rcrd, ptype)
  insert_column(data, as_tibble(rcrd)[fields], source_cols = rcrd_col, remove = remove)
}

insert_column <- function(data, df, source_cols, remove) {
  out <- vec_cbind(data, df, .ptype = vec_ptype(data), .name_repair = "check_unique")

  if (remove) {
    out_names <- insert_vector(names(data), names(df), min(source_cols))
    out_names <- setdiff(out_names, names(data)[source_cols])
  } else {
    out_names <- insert_vector(names(data), names(df), min(source_cols) + 1)
  }

  out[out_names]
}

insert_vector <- function(x, y, pos) {
  c(x[seq_len(pos - 1)], y, x[pos - 1 + seq_len(length(x) - pos + 1)])
}
