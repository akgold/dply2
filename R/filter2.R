#' Return rows with matching conditions and print diagnostics
#'
#' At this point, `filter2` only works on data.frame objects, rather than the
#' full set of `dplyr::filter`-able objects.
#'
#' @inheritParams dplyr::filter
#' @param .verbose print diagnostics? Defaults to TRUE
#'
#' @return a data frame
#' @export
#'
#' @examples
#' filter2(dplyr::starwars, gender == "female", height < 170)
#' filter2(dplyr::starwars, gender == "female" | height < 170)
filter2 <- function(.data, ..., .verbose = TRUE) {
  n <- nrow(.data)
  df <- dplyr::filter(.data, ...)

  if (.verbose) {
    fil <- lapply(rlang::enquos(...), rlang::quo_text)
    cat(sprintf("\nFiltering for %s.\n\tRows Dropped: %s\n",
                paste(fil, collapse = ", "),
                n - nrow(df)))
  }
  df
}
