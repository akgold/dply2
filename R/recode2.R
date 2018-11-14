#' Manually recode factors with crosstab of old and new
#'
#' Sort of like `forcats::fct_recode`, but allows for multiple values per name and adds some extra options.
#'
#' @param .f a factor (or character vector)
#' @param ... named character vectors, name is new level value(s) are old. Levels not mentioned are left as-is unless `other_to_na` is chosen. Levels can be removed by naming NULL.
#' @param .verbose print diagnostics? Defaults to TRUE
#' @param .ordered order factor according to order supplied? Defaults to FALSE
#' @param .other_to_na unmentioned levels to NA? defaults to FALSE
#'
#' @return .f recoded as specified
#' @export
#'
#' @examples
#' fct_recode2(dplyr::starwars$gender, m_or_f = c("male", "female"),
#' other = c(NA, "none", "hermaphrodite"))
#' fct_recode2(dplyr::starwars$gender, m_or_f = c("male", "female"),
#' other = c(NA, "none", "hermaphrodite"), .ordered = TRUE)
#' fct_recode2(dplyr::starwars$gender, m_or_f = c("male", "female"),
#' other = c("hermaphrodite"), .other_to_na = TRUE)
fct_recode2 <- function(.f, ...,
                        .verbose = TRUE, .ordered = FALSE,
                        .other_to_na = FALSE) {
  old <- .f
  vals <- list(...)

  if (!all(unlist(vals) %in% .f)) {
    warning("Not all values specified in .f")
  }

  .f <- do_recode(.f, vals)

  if (.ordered) .f <- add_order(.f, names(vals))
  if (.other_to_na) .f[!.f %in% names(vals)] <- NA


  if (.verbose) {
    cat("\n")
    print(table(old, new = .f, useNA = "ifany"))
    cat("\n")
  }

  .f
}

do_recode <- function(.f, vals) {
  .f[.f %in% vals[[1]]] <- names(vals)[1]
  if (length(vals) == 1) return(.f)
  do_recode(.f, vals[-1])
}

add_order <- function(.f, levs) {
  factor(.f, levels = levs, ordered = TRUE)
}