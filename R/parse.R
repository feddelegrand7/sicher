# =============================================================================
# Parsing API
# =============================================================================

#' Test whether a value matches a type
#'
#' @description
#' Returns \code{TRUE} or \code{FALSE} instead of throwing an error. Useful in
#' \code{if} conditions, \code{Filter()} and \code{vapply()}.
#'
#' @param value The value to check.
#' @param type A sicher_type, sicher_union or sicher_readonly object.
#'
#' @return A single logical value.
#'
#' @seealso \code{\link{check_type}}, \code{\link{safe_parse}}
#'
#' @examples
#' is_type(1, Numeric)       # TRUE
#' is_type("1", Numeric)     # FALSE
#'
#' Filter(function(x) is_type(x, String), list("a", 1, "b"))
#'
#' @export
is_type <- function(value, type) {
  length(run_type(value, type)$issues) == 0
}

#' Parse a value against a type
#'
#' @description
#' \code{parse_as()} checks \code{value} against \code{type} and returns the
#' parsed value, or throws an error of class \code{"sicher_type_error"} that
#' lists every problem found (like zod's \code{.parse()}).
#'
#' \code{safe_parse()} never throws on invalid data (like zod's
#' \code{.safeParse()}). It returns a result object with:
#' \describe{
#'   \item{success}{\code{TRUE} if the value matches the type.}
#'   \item{data}{The parsed value, or \code{NULL} on failure.}
#'   \item{issues}{A data frame with one row per problem and the columns
#'     \code{path}, \code{message}, \code{expected}, \code{got} and
#'     \code{code}.}
#'   \item{error}{The \code{"sicher_type_error"} condition on failure, or
#'     \code{NULL}.}
#' }
#'
#' The parsed value is usually the input value itself, but it can differ when
#' the type transforms values: \code{\link{Default}()} fills in missing
#' values, \code{\link{Coerce}()} converts them, list types with
#' \code{extra = "drop"} remove unknown fields and \code{\link{FunctionOf}()}
#' wraps functions.
#'
#' @param value The value to parse.
#' @param type A sicher_type, sicher_union or sicher_readonly object.
#' @param context Optional name for the value, used in error messages.
#'
#' @return \code{parse_as()} returns the parsed value. \code{safe_parse()}
#'   returns a list of class \code{"sicher_parse_result"}.
#'
#' @seealso \code{\link{check_type}}, \code{\link{is_type}},
#'   \code{\link{validate_json}}
#'
#' @examples
#' Todo <- create_list_type(list(
#'   title = String,
#'   done  = Default(Bool, FALSE)
#' ))
#'
#' parse_as(list(title = "Write docs"), Todo)
#'
#' result <- safe_parse(list(title = 1, done = "no"), Todo)
#' result$success
#' result$issues
#'
#' @export
parse_as <- function(value, type, context = NULL) {
  res <- run_type(value, type, if (is.null(context)) list() else list(context))
  if (length(res$issues) > 0) {
    sicher_abort(res$issues)
  }
  res$value
}

#' @rdname parse_as
#' @export
safe_parse <- function(value, type, context = NULL) {
  res <- run_type(value, type, if (is.null(context)) list() else list(context))
  success <- length(res$issues) == 0
  structure(
    list(
      success = success,
      data    = if (success) res$value else NULL,
      issues  = issues_to_df(res$issues),
      error   = if (success) NULL else sicher_condition(
        res$issues, c("sicher_type_error", "error")
      )
    ),
    class = "sicher_parse_result"
  )
}

#' Print method for sicher_parse_result
#'
#' @param x A sicher_parse_result object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_parse_result <- function(x, ...) {
  if (x$success) {
    cat("<parse result: success>\n")
    utils::str(x$data, give.attr = FALSE)
  } else {
    n <- nrow(x$issues)
    cat(glue::glue("<parse result: {n} issue{if (n == 1) '' else 's'}>"), "\n", sep = "")
    for (i in seq_len(n)) {
      where <- x$issues$path[i]
      prefix <- if (nzchar(where)) paste0(where, ": ") else ""
      cat("  * ", prefix, x$issues$message[i], "\n", sep = "")
    }
  }
  invisible(x)
}
