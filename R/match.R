# =============================================================================
# Exhaustive matching
# =============================================================================

#' Exhaustive matching on the cases of a type
#'
#' @description
#' Builds a function that checks a value against \code{type} and calls the
#' handler for the case the value belongs to, like \code{match} in Rust or
#' \code{case} in Haskell. The cases are:
#' \itemize{
#'   \item the tags of a \code{\link{DiscriminatedUnion}()};
#'   \item the values of an \code{\link{Enum}()} or \code{\link{Literal}()}
#'     type (the value must then be a single value);
#'   \item the levels of a \code{\link{Factor}(levels = ...)} type (a single
#'     value);
#'   \item the members of a union such as \code{Integer | String | Null},
#'     named by their type names (\code{integer}, \code{string},
#'     \code{null}). The first member that matches is used.
#' }
#'
#' The handlers are checked when \code{match_type()} is called: leaving out
#' a case is an error unless \code{.default} is given, and a handler for a
#' case that does not exist is always an error. Adding a case to a type
#' therefore breaks every matcher that does not handle it, instead of
#' silently returning \code{NULL} like \code{switch()}.
#'
#' @param type A DiscriminatedUnion, Enum, Literal, Factor (with levels) or
#'   union type.
#' @param ... Handlers, named by case. Each handler is a function that is
#'   called with the parsed value (and any extra arguments given to the
#'   matcher).
#' @param .default Optional handler for the cases without their own handler.
#'
#' @return A function \code{function(x, ...)} that checks \code{x} against
#'   \code{type} (throwing a type error if it does not match) and returns the
#'   result of the matching handler.
#'
#' @examples
#' Shape <- DiscriminatedUnion(
#'   "kind",
#'   circle = create_list_type(list(radius = Numeric)),
#'   square = create_list_type(list(side = Numeric))
#' )
#'
#' area <- match_type(Shape,
#'   circle = function(s) pi * s$radius^2,
#'   square = function(s) s$side^2
#' )
#' area(list(kind = "square", side = 3))  # 9
#'
#' # Forgetting a case is an error when the matcher is created
#' try(match_type(Shape, circle = function(s) pi * s$radius^2))
#'
#' Status <- Enum("active", "paused", "closed")
#' label <- match_type(Status,
#'   active = function(x) "Running",
#'   .default = function(x) "Not running"
#' )
#' label("paused")  # "Not running"
#'
#' describe <- match_type(Integer | String,
#'   integer = function(x) "a number",
#'   string = function(x) "some text"
#' )
#' describe("a")
#'
#' @export
match_type <- function(type, ..., .default = NULL) {
  check_type_arg(type, "match_type")
  handlers <- list(...)
  if (length(handlers) > 0 &&
      (is.null(names(handlers)) || any(!nzchar(names(handlers))))) {
    stop(
      "Every handler in match_type() must be named by its case, e.g. circle = function(s) ...",
      call. = FALSE
    )
  }
  for (nm in names(handlers)) {
    if (!is.function(handlers[[nm]])) {
      stop(glue::glue(
        "The handler for '{nm}' must be a function; got {class(handlers[[nm]])[1]}"
      ), call. = FALSE)
    }
  }
  if (!is.null(.default) && !is.function(.default)) {
    stop(glue::glue("`.default` must be a function; got {class(.default)[1]}"), call. = FALSE)
  }
  if (anyDuplicated(names(handlers)) > 0) {
    stop(glue::glue(
      "Duplicate handlers in match_type(): ",
      "{paste(unique(names(handlers)[duplicated(names(handlers))]), collapse = ', ')}"
    ), call. = FALSE)
  }

  cases <- match_cases(type)
  shown_cases <- paste0("'", cases$keys, "'", collapse = ", ")

  unknown <- setdiff(names(handlers), cases$keys)
  if (length(unknown) > 0) {
    hints <- vapply(unknown, function(u) {
      match <- suggest_name(u, setdiff(cases$keys, names(handlers)))
      if (is.null(match)) "" else glue::glue(" (did you mean '{match}'?)")
    }, character(1))
    stop(glue::glue(
      "match_type() has handlers for cases that {type_label(type)} does not have: ",
      "{paste0(\"'\", unknown, \"'\", hints, collapse = ', ')}. ",
      "Its cases are {shown_cases}"
    ), call. = FALSE)
  }

  missing_cases <- setdiff(cases$keys, names(handlers))
  if (length(missing_cases) > 0 && is.null(.default)) {
    stop(glue::glue(
      "match_type() is not exhaustive: no handler for ",
      "{paste0(\"'\", missing_cases, \"'\", collapse = ', ')}. ",
      "Add a handler for each case or a .default handler"
    ), call. = FALSE)
  }

  function(x, ...) {
    value <- parse_as(x, type)
    key <- cases$select(value)
    handler <- if (key %in% names(handlers)) handlers[[key]] else .default
    handler(value, ...)
  }
}

# The case keys of a type and a function giving the key of a parsed value.
match_cases <- function(type) {
  type <- peel(type, c("alias", "describe"))

  if (inherits(type, "sicher_union")) {
    members <- type$types
    keys <- vapply(members, function(m) m$name, character(1))
    if (anyDuplicated(keys) > 0) {
      stop(glue::glue(
        "match_type() cannot tell apart the union members named ",
        "{paste(unique(keys[duplicated(keys)]), collapse = ', ')}"
      ), call. = FALSE)
    }
    return(list(keys = keys, select = function(value) {
      for (i in seq_along(members)) {
        if (is_type(value, members[[i]])) return(keys[[i]])
      }
      keys[[1]]
    }))
  }

  kind <- meta_kind(type)
  meta <- type_meta(type)
  single <- function(value) {
    if (length(value) != 1) {
      stop(glue::glue(
        "match_type() on {type_label(type)} needs a single value; got length {length(value)}"
      ), call. = FALSE)
    }
    as.character(value)
  }

  if (identical(kind, "discriminated")) {
    return(list(
      keys = names(meta$members),
      select = function(value) as.character(value[[meta$key]])
    ))
  }
  if (kind %in% c("enum", "literal")) {
    return(list(keys = unique(vapply(meta$values, as.character, character(1))), select = single))
  }
  if (identical(kind, "factor") && !is.null(meta$levels)) {
    return(list(keys = as.character(meta$levels), select = single))
  }

  stop(glue::glue(
    "match_type() needs a DiscriminatedUnion(), Enum(), Literal(), ",
    "Factor(levels = ...) or a union of types; got {type_label(type)}"
  ), call. = FALSE)
}
