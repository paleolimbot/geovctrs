
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
