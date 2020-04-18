
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
#' tbl2 <- tibble(a = geo_xy(1, 2), b = geo_xy(3, 4))
#' (united <- unite_segment(tbl2, "seg", a, b))
#' separate_segment(united, seg)
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
unite_segment <- function(data, col, start, end, remove = TRUE) {
  unite_rcrd(data, col, {{ start }}, {{ end }}, remove = remove, constructor = geo_segment)
}

#' @rdname unite_xy
#' @export
separate_segment <- function(data, col, into = c("start", "end"), remove = TRUE) {
  separate_vctrs_rcrd(
    data, {{ col }},
    into = into,
    remove = remove,
    ptype = geo_segment(),
    fields = c("start", "end")
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
  insert_column(data, col, rcrd, unlist(cols), remove)
}

separate_vctrs_rcrd <- function(data, col, into, remove, ptype, fields) {
  data <- tibble::as_tibble(data)
  ptype_data <- vec_data(ptype)
  rcrd_col <- tidyselect::eval_select(enquo(col), data, strict = TRUE)

  stopifnot(
    length(rcrd_col) == 1,
    length(into) == length(fields),
    is.character(into)
  )

  rcrd <- data[[rcrd_col]]
  vec_assert(rcrd, ptype)

  # this is not the most stylish way to go about this
  for (i in rev(seq_along(fields))) {
    data <- insert_column(
      data,
      into[i],
      field(rcrd, fields[i]),
      rcrd_col,
      remove = remove
    )

    remove <- FALSE
  }

  data
}

insert_column <- function(data, col, value, source_cols, remove) {
  source_cols <- sort(source_cols)

  if (remove) {
    data[[source_cols[1]]] = value
    names(data)[source_cols[1]] <- col
    if (length(source_cols) > 1) {
      data <- data[-source_cols[-1]]
    }

    data
  } else {
    before <- source_cols[1]
    vec_cbind(
      data[seq_len(before - 1)],
      tibble(!!col := value),
      data[before:ncol(data)]
    )
  }
}
