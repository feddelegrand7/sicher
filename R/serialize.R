# =============================================================================
# Output: dump_as() and to_json()
# =============================================================================
#
# dump_value() walks a parsed value together with its type and prepares it
# for the outside world: list fields and data frame columns are renamed to
# their aliases, NULL fields can be dropped, brands are removed and, for
# JSON, atomic vectors that are not declared scalar are marked with I() so
# jsonlite writes them as arrays even when they have one element.

dump_value <- function(x, type, opts, scalar = FALSE) {
  if (is.null(x)) return(NULL)
  if (inherits(type, "sicher_readonly")) type <- type$base_type

  if (inherits(type, "sicher_union")) {
    for (member in type$types) {
      if (length(run_type(x, member)$issues) == 0) {
        return(dump_value(x, member, opts, scalar))
      }
    }
    return(dump_leaf(x, opts, scalar))
  }

  if (!inherits(type, "sicher_type")) return(dump_leaf(x, opts, scalar))
  meta <- type_meta(type)
  kind <- if (is.null(meta)) NA_character_ else meta$kind
  if (is.na(kind)) return(dump_leaf(x, opts, scalar))

  switch(
    kind,
    list = dump_object(x, meta, opts),
    data.frame = dump_data_frame(x, meta, opts),
    list_of = lapply(x, dump_value, type = meta$inner, opts = opts),
    tuple = Map(dump_value, unname(x), meta$types, MoreArgs = list(opts = opts)),
    record = lapply(as.list(x), dump_value, type = meta$values, opts = opts,
                    scalar = is.atomic(x)),
    discriminated = dump_value(x, meta$members[[as.character(x[[meta$key]])]], opts),
    lazy = dump_value(x, meta$resolve(), opts, scalar),
    intersection = dump_value(x, meta$types[[1]], opts, scalar),
    scalar = dump_value(x, meta$inner, opts, scalar = TRUE),
    sized = dump_value(x, meta$inner, opts, scalar = scalar || meta$size == 1),
    literal = dump_leaf(x, opts, scalar = TRUE),
    brand = {
      class(x) <- setdiff(oldClass(x), meta$brand)
      if (length(brand_classes(x)) == 0) x <- strip_brands(x)
      if (length(oldClass(x)) == 0) x <- unclass(x)
      dump_value(x, meta$inner, opts, scalar)
    },
    if (!is.null(meta$inner) && is_sicher_type_like(meta$inner)) {
      dump_value(x, meta$inner, opts, scalar)
    } else {
      dump_leaf(x, opts, scalar)
    }
  )
}

dump_leaf <- function(x, opts, scalar) {
  if (opts$box && !scalar && is.atomic(x) && is.null(dim(x))) I(x) else x
}

dump_object <- function(x, meta, opts) {
  spec <- meta$spec
  keys <- meta$keys
  out <- list()
  for (name in names(x)) {
    value <- x[[name]]
    if (name %in% names(spec)) {
      value <- dump_value(value, spec[[name]], opts)
      if (opts$by_alias) name <- keys[[name]]
    }
    if (is.null(value)) {
      if (!opts$exclude_null) out[name] <- list(NULL)
    } else {
      out[[name]] <- value
    }
  }
  if (length(out) == 0) out <- empty_object()
  out
}

dump_data_frame <- function(x, meta, opts) {
  keys <- meta$keys
  x <- as.data.frame(x)
  if (!opts$by_alias) return(x)
  known <- names(x) %in% names(keys)
  names(x)[known] <- unname(keys[names(x)[known]])
  x
}

dump_options <- function(by_alias, exclude_null, box) {
  if (!is.logical(by_alias) || length(by_alias) != 1 || is.na(by_alias)) {
    stop("`by_alias` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.logical(exclude_null) || length(exclude_null) != 1 || is.na(exclude_null)) {
    stop("`exclude_null` must be TRUE or FALSE", call. = FALSE)
  }
  list(by_alias = by_alias, exclude_null = exclude_null, box = box)
}

#' Prepare a value for output
#'
#' @description
#' Checks \code{value} against \code{type} and returns it ready to be sent
#' to another system: list fields and data frame columns get their external
#' names (see \code{\link{Alias}()} and the \code{alias} argument of
#' \code{\link{create_list_type}()}), \code{NULL} fields can be left out and
#' branded values lose their brand. \code{dump_as()} is the reverse of
#' \code{\link{parse_as}()}: \code{parse_as(dump_as(x, type), type)} gives
#' back \code{x}.
#'
#' The value is parsed first, so defaults are filled in and coercions are
#' applied, and a value that does not match \code{type} raises a type error
#' instead of producing invalid output. This makes \code{dump_as()} a good
#' last step for API responses: the response is guaranteed to match its
#' declared schema.
#'
#' Use \code{\link{to_json}()} to get a JSON string directly.
#'
#' @param value The value to prepare.
#' @param type A sicher type.
#' @param by_alias If \code{TRUE} (the default), fields are named by their
#'   external names; if \code{FALSE}, they keep their R names.
#' @param exclude_null If \code{TRUE}, list fields whose value is
#'   \code{NULL} are left out. Defaults to \code{FALSE}.
#'
#' @return The prepared value: a plain R list, vector or data frame.
#'
#' @examples
#' User <- create_list_type(list(
#'   user_id  = Scalar(Integer),
#'   nickname = Optional(Scalar(String))
#' ), alias = "camel")
#'
#' user <- list(user_id = 1L, nickname = NULL)
#' dump_as(user, User)
#' dump_as(user, User, exclude_null = TRUE)
#' dump_as(user, User, by_alias = FALSE)
#'
#' @export
dump_as <- function(value, type, by_alias = TRUE, exclude_null = FALSE) {
  opts <- dump_options(by_alias, exclude_null, box = FALSE)
  dump_value(parse_as(value, type), type, opts)
}

#' Convert a value to JSON
#'
#' @description
#' Checks \code{value} against \code{type} and writes it as JSON, using the
#' type to decide how R values map to JSON. It is the reverse of
#' \code{\link{validate_json}()}.
#'
#' \itemize{
#'   \item List fields and data frame columns are written under their
#'     external names (see \code{\link{Alias}()}).
#'   \item Values declared with \code{\link{Scalar}()} (or \code{[1]}, or a
#'     \code{\link{Literal}()}) are written as JSON scalars. Other atomic
#'     vectors are written as arrays, even when they have a single element,
#'     so the shape of the output depends on the type and not on the data.
#'   \item \code{NULL} and \code{NA} become \code{null}; \code{NULL} fields
#'     can be left out with \code{exclude_null = TRUE}.
#'   \item Dates are written as \code{"YYYY-MM-DD"} and date-times in ISO
#'     8601 format, matching the formats used by \code{\link{to_json_schema}()}.
#'   \item Data frames are written as arrays of row objects.
#' }
#'
#' The value is parsed first, so a value that does not match \code{type}
#' raises a type error instead of producing invalid JSON.
#'
#' @inheritParams dump_as
#' @param pretty If \code{TRUE}, indent the JSON.
#' @param ... Further arguments passed to \code{jsonlite::toJSON()}, which
#'   override the defaults \code{auto_unbox = TRUE}, \code{null = "null"},
#'   \code{na = "null"}, \code{POSIXt = "ISO8601"} and \code{digits = NA}.
#'
#' @return A JSON string.
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' Order <- create_list_type(list(
#'   order_id = Scalar(Integer),
#'   tags     = String,
#'   coupon   = Optional(Scalar(String))
#' ), alias = "camel")
#'
#' order <- list(order_id = 7L, tags = "gift")
#' to_json(order, Order)
#' to_json(order, Order, exclude_null = TRUE, pretty = TRUE)
#'
#' # Round trip
#' validate_json(to_json(order, Order), Order)
#'
#' @export
to_json <- function(value, type, by_alias = TRUE, exclude_null = FALSE,
                    pretty = FALSE, ...) {
  require_jsonlite("to_json()")
  opts <- dump_options(by_alias, exclude_null, box = TRUE)
  data <- dump_value(parse_as(value, type), type, opts)
  defaults <- list(
    auto_unbox = TRUE, null = "null", na = "null", POSIXt = "ISO8601",
    digits = NA, pretty = pretty
  )
  args <- utils::modifyList(defaults, list(...))
  as.character(do.call(jsonlite::toJSON, c(list(x = data), args)))
}
