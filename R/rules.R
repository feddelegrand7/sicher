# =============================================================================
# Checks across fields and transforms
# =============================================================================

#' Rules that report problems at a chosen location
#'
#' @description
#' Adds rules on top of a type, like zod's \code{.superRefine()}. Unlike
#' \code{\link{Refine}()}, which reports one problem about the whole value,
#' the rule function can report any number of problems, each at the field it
#' is about. This suits rules that involve several fields ("end must be after
#' start", "a discount code needs a customer id").
#'
#' The base type is checked first; the rule only runs on values that pass
#' it, and sees the parsed value (with defaults filled in and fields renamed
#' from their aliases to their R names).
#'
#' @param type A sicher_type or sicher_union object.
#' @param fn A function \code{function(value, ctx)}. Call
#'   \code{ctx$issue(message, path = NULL)} to report a problem. \code{path}
#'   is relative to the value: a field name such as \code{"end"}, a character
#'   vector such as \code{c("address", "zip")}, or a list mixing field names
#'   and 1-based positions such as \code{list("items", 2, "qty")}. Without a
#'   path the problem is about the whole value. The return value of
#'   \code{fn} is ignored.
#'
#' @return A new sicher_type.
#'
#' @examples
#' Booking <- Check(
#'   create_list_type(list(start = Date, end = Date, adults = Integer, kids = Integer)),
#'   function(x, ctx) {
#'     if (x$end < x$start) ctx$issue("must be after start", path = "end")
#'     if (x$adults == 0L && x$kids > 0L) ctx$issue("children need an adult", path = "kids")
#'   }
#' )
#' res <- safe_parse(
#'   list(start = as.Date("2024-05-10"), end = as.Date("2024-05-01"), adults = 0L, kids = 2L),
#'   Booking
#' )
#' res$issues[, c("path", "message")]
#'
#' @export
Check <- function(type, fn) {
  check_type_arg(type, "Check")
  if (!is.function(fn)) {
    stop(glue::glue(
      "`fn` must be a function(value, ctx); got {class(fn)[1]}"
    ), call. = FALSE)
  }
  type_name <- glue::glue("check<{type_label(type)}>")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      found <- list()
      outer_path <- path
      ctx <- list(issue = function(message, path = NULL) {
        found[[length(found) + 1]] <<- new_issue(
          c(outer_path, as.list(path)), message,
          expected = type_name, code = "custom"
        )
        invisible(NULL)
      })
      outcome <- tryCatch(fn(res$value, ctx), error = function(e) e)
      if (inherits(outcome, "error")) {
        found <- c(found, list(new_issue(
          path,
          glue::glue("check of {type_name} failed with an error: {conditionMessage(outcome)}"),
          expected = type_name, code = "custom"
        )))
      }
      if (length(found) == 0) return(res)
      list(value = res$value, issues = found)
    },
    meta = list(kind = "check", inner = type),
    strict_check = TRUE
  )
}

#' Transform values after checking them
#'
#' @description
#' \code{Transform()} checks a value against \code{type} and then converts it
#' with \code{fn}, like zod's \code{.transform()}. The converted value is
#' what typed variables, typed function arguments, \code{\link{parse_as}()}
#' and \code{\link{safe_parse}()} return. Use it to keep cleaning steps
#' (trimming, lower-casing, converting units) next to the type.
#'
#' \code{Pipe()} chains types: the value parsed by each type is passed on to
#' the next one, like zod's \code{.pipe()}. Checking stops at the first type
#' that fails. It differs from \code{&}, which also passes the value on, in
#' that it takes any number of types and is named as a pipeline.
#'
#' Unlike \code{\link{Coerce}()}, which only converts values that fail the
#' check, \code{Transform()} always runs. \code{\link{to_json_schema}()}
#' describes the input side (what is accepted). \code{\link{dump_as}()} and
#' \code{\link{to_json}()} check their value against the whole type again, so
#' they expect values in the input form.
#'
#' In a list type, a \code{Transform(Optional(type), fn)} field may be left
#' out (it is then not transformed). To give a transformed field a default,
#' put \code{\link{Default}()} outside: \code{Default(Transform(String,
#' toupper), "A")}.
#'
#' @param type A sicher_type or sicher_union object checked before the
#'   transform.
#' @param fn A function converting the checked value.
#' @param output Optional type the converted value must satisfy.
#' @param ... Two or more types for \code{Pipe()}.
#'
#' @return A new sicher_type.
#'
#' @examples
#' Email <- Transform(Scalar(String), function(x) tolower(trimws(x)))
#' parse_as("  Ada@Example.COM ", Email)   # "ada@example.com"
#'
#' # Percent strings to a share between 0 and 1
#' Share <- Pipe(
#'   Coerce(Numeric),
#'   Transform(Numeric, function(x) x / 100),
#'   Between(0, 1)
#' )
#' parse_as("42", Share)   # 0.42
#' try(parse_as("420", Share))
#'
#' @name transforms
#' @export
Transform <- function(type, fn, output = NULL) {
  check_type_arg(type, "Transform")
  if (!is.function(fn)) {
    stop(glue::glue("`fn` must be a function; got {class(fn)[1]}"), call. = FALSE)
  }
  if (!is.null(output)) check_type_arg(output, "Transform")
  type_name <- if (is.null(output)) {
    glue::glue("transform<{type_label(type)}>")
  } else {
    glue::glue("transform<{type_label(type)} -> {type_label(output)}>")
  }
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      converted <- tryCatch(fn(res$value), error = function(e) e)
      if (inherits(converted, "error")) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("transform of {type_name} failed: {conditionMessage(converted)}"),
          expected = type_name, got = get_type_name(res$value),
          received = format_received(res$value), code = "invalid_transform"
        )))
      }
      if (is.null(output)) return(ok_result(converted))
      run_type(converted, output, path)
    },
    meta = list(kind = "transform", input = type, output = output)
  )
}

#' @rdname transforms
#' @export
Pipe <- function(...) {
  types <- list(...)
  if (length(types) < 2) {
    stop("Pipe() needs at least two types, e.g. Pipe(Coerce(Numeric), Between(0, 1))", call. = FALSE)
  }
  for (i in seq_along(types)) {
    if (!is_sicher_type_like(types[[i]])) {
      stop(glue::glue(
        "Argument {i} of Pipe() must be a sicher_type or sicher_union; got {class(types[[i]])[1]}"
      ), call. = FALSE)
    }
  }
  type_name <- paste(vapply(types, type_label, character(1)), collapse = " |> ")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- ok_result(x)
      for (stage in types) {
        res <- run_type(res$value, stage, path)
        if (length(res$issues) > 0) return(list(value = x, issues = res$issues))
      }
      res
    },
    meta = list(kind = "pipe", types = types)
  )
}
