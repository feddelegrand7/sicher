# =============================================================================
# Error reports
# =============================================================================

# The raw issues of a safe_parse() result or of a sicher type error.
issues_of <- function(x, fn_name) {
  if (inherits(x, "sicher_parse_result")) {
    return(if (is.null(x$error)) list() else x$error$issues)
  }
  if (inherits(x, "sicher_type_error") || inherits(x, "sicher_type_warning")) {
    return(x$issues)
  }
  stop(glue::glue(
    "{fn_name}() needs a safe_parse() result or a sicher type error ",
    "(e.g. from tryCatch(..., sicher_type_error = function(e) e)); got {class(x)[1]}"
  ), call. = FALSE)
}

#' Error messages grouped by field
#'
#' @description
#' Groups the problems of a failed check by the location of the value they
#' are about, which is the shape a form needs to show each message next to
#' its input (for example in Shiny).
#'
#' @param x A \code{\link{safe_parse}()} result, or a type error caught with
#'   \code{tryCatch(..., sicher_type_error = function(e) e)}.
#'
#' @return A named list with one character vector of messages per location.
#'   Names are paths such as \code{"email"} or \code{"address$city"}; a
#'   problem with the value as a whole has the name \code{""}. The list is
#'   empty when there are no problems.
#'
#' @examples
#' Signup <- create_list_type(list(
#'   email = Matches("@"),
#'   age = Scalar(Between(18, 120))
#' ))
#' res <- safe_parse(list(email = "nope", age = 12), Signup)
#' field_errors(res)
#' field_errors(res)$age
#'
#' @export
field_errors <- function(x) {
  issues <- issues_of(x, "field_errors")
  if (length(issues) == 0) return(structure(list(), names = character(0)))
  paths <- vapply(issues, function(i) format_path(i$path), character(1))
  messages <- vapply(issues, issue_message, character(1))
  split(unname(messages), factor(paths, levels = unique(paths)))
}

#' Problem details for HTTP APIs
#'
#' @description
#' Turns the problems of a failed check into an RFC 9457 "problem details"
#' object, the standard body of a \code{400} or \code{422} response. The
#' result is a plain list, so web frameworks such as plumber can serialize
#' it as JSON.
#'
#' @param x A \code{\link{safe_parse}()} result, or a type error caught with
#'   \code{tryCatch(..., sicher_type_error = function(e) e)}.
#' @param status HTTP status code. Defaults to \code{400}.
#' @param title Short summary of the problem.
#' @param type URI identifying the kind of problem. Defaults to
#'   \code{"about:blank"}.
#' @param instance Optional URI of the request that failed.
#'
#' @return A list with \code{type}, \code{title}, \code{status},
#'   \code{detail}, \code{instance} (if given) and \code{errors}. Each error
#'   has a \code{path} (as in the issues data frame), a \code{pointer} (a
#'   JSON Pointer into the request body, with 0-based array indices), a
#'   \code{message} and a \code{code}.
#'
#' @examples
#' Order <- create_list_type(list(
#'   id = Scalar(Integer),
#'   items = ListOf(create_list_type(list(sku = String, qty = Positive)))
#' ))
#' res <- safe_parse(list(id = 1L, items = list(list(sku = "a", qty = -1))), Order)
#' str(problem_json(res, status = 422))
#'
#' \dontrun{
#' # In a plumber endpoint
#' #* @post /orders
#' function(req, res) {
#'   parsed <- safe_parse(req$body, Order)
#'   if (!parsed$success) {
#'     res$status <- 422
#'     return(problem_json(parsed, status = 422))
#'   }
#'   create_order(parsed$data)
#' }
#' }
#'
#' @export
problem_json <- function(x, status = 400L, title = "Invalid request",
                         type = "about:blank", instance = NULL) {
  issues <- issues_of(x, "problem_json")
  if (length(issues) == 0) {
    stop("problem_json() needs a failed check; this one has no problems", call. = FALSE)
  }
  n <- length(issues)
  out <- list(
    type = type,
    title = title,
    status = as.integer(status),
    detail = if (n == 1) "1 validation error" else glue::glue("{n} validation errors")
  )
  out$detail <- as.character(out$detail)
  if (!is.null(instance)) out$instance <- instance
  out$errors <- lapply(issues, function(i) list(
    path = format_path(i$path),
    pointer = json_pointer(i$path),
    message = issue_message(i),
    code = as.character(i$code)
  ))
  out
}

# RFC 6901 JSON Pointer for an issue path (numeric segments are 1-based in
# R and 0-based in JSON).
json_pointer <- function(path) {
  if (length(path) == 0) return("")
  segs <- vapply(path, function(seg) {
    if (is.numeric(seg)) return(as.character(seg - 1))
    gsub("/", "~1", gsub("~", "~0", as.character(seg), fixed = TRUE), fixed = TRUE)
  }, character(1))
  paste0("/", segs, collapse = "")
}
