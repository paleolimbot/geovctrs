
validate_provider <- function(x) {
  problem_vctr <- cpp_validate_provider(x)
  has_problems <- !is.na(problem_vctr)

  if (!any(has_problems)) {
    return(x)
  }

  problems_tbl <- tibble(
    row = which(has_problems),
    col = NA_integer_,
    expected = class(x)[1],
    actual = vec_data(x)[has_problems],
    error = gsub("ParseException:\\s*", "", problem_vctr[has_problems])
  )

  warn_problems(problems_tbl)

  attr(x, "problems") <- problems_tbl
  # until subset assignment is reliable for wkb
  indices <- seq_along(x)
  indices[has_problems] <- NA_integer_
  x[indices]
}

# this is readr's warn_problems function
warn_problems <- function(probs) {
  probs <- as.data.frame(probs)
  n <- nrow(probs)

  many_problems <- nrow(probs) > 5
  probs_f <- format(utils::head(probs, 5), justify = "left")
  probs_f[probs_f == "NA"] <- "--"
  probs_f <- rbind(names(probs), probs_f)
  probs_f <- lapply(probs_f, format, justify = "left")

  if (many_problems) {
    width <- vapply(probs_f, function(x) max(nchar(x)), integer(1))
    dots <- vapply(
      width,
      function(i) paste(rep(".", i), collapse = ""),
      FUN.VALUE = character(1)
    )
    probs_f <- Map(c, probs_f, dots)
  }

  probs_f <- rlang::exec(paste, !!!probs_f, sep = " ", collapse = "\n")
  warning(
    n, " parsing failure", if (n > 1)  "s", ".\n",
    probs_f, "\n",
    "See attr(result, 'problems') for more details.\n",
    call. = FALSE, immediate. = TRUE, noBreaks. = TRUE
  )
}
