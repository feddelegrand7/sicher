# =============================================================================
# JSON Schema export and JSON validation
# =============================================================================

empty_object <- function() structure(list(), names = character(0))

# R vectors have no scalar/array distinction, so an unconstrained atomic type
# such as Numeric accepts both a JSON number and an array of numbers.
scalar_or_array <- function(base, scalar) {
  if (scalar) base else list(anyOf = list(base, list(type = "array", items = base)))
}

# Apply array length constraints to a schema produced by schema_of().
with_array_bounds <- function(schema, min = NULL, max = NULL, unique = FALSE) {
  apply_to_array <- function(s) {
    if (!is.null(min)) s$minItems <- min
    if (!is.null(max)) s$maxItems <- max
    if (unique) s$uniqueItems <- TRUE
    s
  }
  if (identical(schema$type, "array")) {
    return(apply_to_array(schema))
  }
  if (!is.null(schema$anyOf) && length(schema$anyOf) == 2 &&
      identical(schema$anyOf[[2]]$type, "array")) {
    base <- schema$anyOf[[1]]
    array <- apply_to_array(schema$anyOf[[2]])
    scalar_ok <- (is.null(min) || min <= 1) && (is.null(max) || max >= 1)
    if (!is.null(max) && max == 0) return(list(type = "array", maxItems = 0L))
    if (!scalar_ok) return(array)
    if (!is.null(min) && !is.null(max) && min == 1 && max == 1) return(base)
    return(list(anyOf = list(base, array)))
  }
  schema
}

json_value <- function(value) {
  if (is.atomic(value) && length(value) == 1 && is.null(attributes(value))) {
    return(value)
  }
  if (is.atomic(value) && is.null(dim(value))) return(as.list(unname(value)))
  value
}

unsupported_schema <- function(type) {
  stop(glue::glue(
    "Type '{type_label(type)}' cannot be represented in JSON Schema"
  ), call. = FALSE)
}

schema_of <- function(type, scalar = FALSE) {
  if (inherits(type, "sicher_readonly")) {
    schema <- schema_of(type$base_type, scalar)
    schema$readOnly <- TRUE
    return(schema)
  }

  if (inherits(type, "sicher_union")) {
    members <- lapply(type$types, schema_of, scalar = scalar)
    return(list(anyOf = members))
  }

  if (!inherits(type, "sicher_type")) {
    invalid_type_spec(type)
  }

  meta <- type_meta(type)
  kind <- if (is.null(meta)) NA_character_ else meta$kind
  if (is.na(kind)) unsupported_schema(type)

  switch(
    kind,
    integer = scalar_or_array(list(type = "integer"), scalar),
    double = ,
    numeric = scalar_or_array(list(type = "number"), scalar),
    string = scalar_or_array(list(type = "string"), scalar),
    bool = scalar_or_array(list(type = "boolean"), scalar),
    null = list(type = "null"),
    any = empty_object(),
    list_any = list(type = c("array", "object")),
    data.frame_any = list(type = "array", items = list(type = "object")),
    date = scalar_or_array(list(type = "string", format = "date"), scalar),
    datetime = scalar_or_array(list(type = "string", format = "date-time"), scalar),
    factor = scalar_or_array(
      if (is.null(meta$levels)) list(type = "string")
      else list(type = "string", enum = as.list(meta$levels)),
      scalar
    ),
    enum = scalar_or_array(list(enum = lapply(meta$values, json_value)), scalar),
    literal = if (length(meta$values) == 1) {
      list(const = json_value(meta$values[[1]]))
    } else {
      list(enum = lapply(meta$values, json_value))
    },
    between = scalar_or_array(
      list(type = "number", minimum = meta$min, maximum = meta$max), scalar
    ),
    positive = scalar_or_array(list(type = "number", exclusiveMinimum = 0), scalar),
    non_negative = scalar_or_array(list(type = "number", minimum = 0), scalar),
    whole = scalar_or_array(list(type = "integer"), scalar),
    matches = scalar_or_array(list(type = "string", pattern = meta$pattern), scalar),
    scalar = schema_of(meta$inner, scalar = TRUE),
    sized = if (meta$size == 1) {
      schema_of(meta$inner, scalar = TRUE)
    } else {
      with_array_bounds(schema_of(meta$inner, scalar), meta$size, meta$size)
    },
    min_length = with_array_bounds(schema_of(meta$inner, scalar), min = meta$n),
    max_length = with_array_bounds(schema_of(meta$inner, scalar), max = meta$n),
    unique = with_array_bounds(schema_of(meta$inner, scalar), unique = TRUE),
    non_na = ,
    refine = ,
    check = ,
    coerce = ,
    alias = ,
    brand = schema_of(meta$inner, scalar),
    describe = {
      schema <- schema_of(meta$inner, scalar)
      if (!is.null(meta$title)) schema$title <- meta$title
      if (!is.null(meta$description)) schema$description <- meta$description
      if (!is.null(meta$examples)) schema$examples <- lapply(meta$examples, json_value)
      schema
    },
    default = {
      schema <- schema_of(meta$inner, scalar)
      schema$default <- json_value(meta$value)
      schema
    },
    transform = schema_of(meta$input, scalar),
    pipe = schema_of(meta$types[[1]], scalar),
    list_of = list(type = "array", items = schema_of(meta$inner)),
    list = object_schema(meta$spec, meta$extra, meta$keys),
    data.frame = list(
      type = "array",
      items = object_schema(meta$spec, meta$extra, meta$keys, scalar_fields = TRUE)
    ),
    tuple = list(
      type = "array",
      prefixItems = lapply(meta$types, schema_of),
      items = FALSE,
      minItems = length(meta$types),
      maxItems = length(meta$types)
    ),
    record = {
      schema <- list(type = "object", additionalProperties = schema_of(meta$values))
      if (!identical(meta$keys, String)) {
        schema$propertyNames <- schema_of(meta$keys, scalar = TRUE)
      }
      schema
    },
    intersection = list(allOf = lapply(meta$types, schema_of, scalar = scalar)),
    discriminated = list(oneOf = unname(lapply(meta$members, schema_of))),
    unsupported_schema(type)
  )
}

# Properties are named by the fields' external names (their aliases), since
# that is what appears in the JSON.
object_schema <- function(spec, extra, keys = NULL, scalar_fields = FALSE) {
  if (is.null(keys)) {
    keys <- names(spec)
    names(keys) <- keys
  }
  properties <- empty_object()
  for (field in names(spec)) {
    properties[[keys[[field]]]] <- schema_of(spec[[field]], scalar = scalar_fields)
  }
  required <- unname(keys[!vapply(spec, field_is_optional, logical(1))])
  schema <- list(type = "object", properties = properties)
  if (length(required) > 0) schema$required <- as.list(required)
  if (identical(extra, "error")) schema$additionalProperties <- FALSE
  schema
}

#' Convert a type to JSON Schema
#'
#' @description
#' Converts a sicher type to a JSON Schema (draft 2020-12) description, like
#' zod's \code{z.toJSONSchema()}. Useful for documenting APIs (e.g. plumber
#' endpoints) or sharing a schema with other languages.
#'
#' Because R has no separate scalar type, an unconstrained type such as
#' \code{Numeric} becomes "a number or an array of numbers"; wrap it in
#' \code{Scalar()} to get a plain \code{number}. Rules that JSON Schema
#' cannot express (\code{Refine()}, \code{Coerce()}, \code{Brand()},
#' \code{NonNA()}) are dropped and only their base type is described.
#' Fields are named by their \code{\link{Alias}()}, and
#' \code{\link{Describe}()} adds \code{description}, \code{title} and
#' \code{examples} keywords. Types
#' with no JSON equivalent (\code{Function}, \code{Environment},
#' \code{InstanceOf()}, \code{MatrixOf()}, \code{FunctionOf()},
#' \code{Lazy()} and custom \code{create_type()} types) raise an error.
#'
#' @param type A sicher type.
#' @param json If \code{TRUE}, return a JSON string (requires the
#'   \code{jsonlite} package). If \code{FALSE} (the default), return the
#'   schema as a nested R list.
#'
#' @return A nested list, or a JSON string when \code{json = TRUE}.
#'
#' @examples
#' User <- create_list_type(list(
#'   id    = Scalar(Integer),
#'   name  = Scalar(String),
#'   email = Optional(Scalar(String))
#' ))
#' str(to_json_schema(User))
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' cat(to_json_schema(User, json = TRUE))
#'
#' @export
to_json_schema <- function(type, json = FALSE) {
  schema <- c(
    list(`$schema` = "https://json-schema.org/draft/2020-12/schema"),
    schema_of(type)
  )
  if (!isTRUE(json)) return(schema)
  require_jsonlite("to_json_schema(json = TRUE)")
  as.character(jsonlite::toJSON(
    schema, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA
  ))
}

require_jsonlite <- function(what) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop(glue::glue(
      "{what} requires the 'jsonlite' package. Install it with install.packages(\"jsonlite\")"
    ), call. = FALSE)
  }
}

#' Parse and validate JSON
#'
#' @description
#' Parses a JSON string with \code{jsonlite::fromJSON()} and checks the
#' result against a type, returning the parsed value. Handy for checking
#' request bodies in plumber APIs or configuration files.
#'
#' By default JSON arrays of scalars become atomic vectors (so they match
#' \code{Numeric}, \code{String}, ...), JSON objects become named lists (so
#' they match \code{create_list_type()} types) and arrays of objects become
#' lists of lists (so they match \code{ListOf()}). Two jsonlite behaviours
#' are worth knowing: whole numbers such as \code{1} are read as integers
#' (use \code{Numeric} rather than \code{Double} for JSON numbers), and an
#' empty array \code{[]} is read as \code{list()} rather than an empty
#' atomic vector.
#'
#' @param json A JSON string, URL or file path (anything
#'   \code{jsonlite::fromJSON()} accepts).
#' @param type A sicher type.
#' @param safe If \code{TRUE}, return a \code{\link{safe_parse}()} result
#'   instead of throwing on invalid data.
#' @param ... Further arguments passed to \code{jsonlite::fromJSON()}, which
#'   override the defaults \code{simplifyVector = TRUE},
#'   \code{simplifyDataFrame = FALSE} and \code{simplifyMatrix = FALSE}.
#'
#' @return The parsed value, or a \code{"sicher_parse_result"} when
#'   \code{safe = TRUE}.
#'
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' Order <- create_list_type(list(
#'   id    = Scalar(Integer),
#'   items = ListOf(create_list_type(list(sku = String, qty = Whole)))
#' ))
#'
#' validate_json('{"id": 1, "items": [{"sku": "A1", "qty": 2}]}', Order)
#'
#' res <- validate_json('{"id": "x", "items": []}', Order, safe = TRUE)
#' res$issues
#'
#' @export
validate_json <- function(json, type, safe = FALSE, ...) {
  require_jsonlite("validate_json()")
  defaults <- list(simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  args <- utils::modifyList(defaults, list(...))
  data <- do.call(jsonlite::fromJSON, c(list(txt = json), args))
  if (isTRUE(safe)) safe_parse(data, type) else parse_as(data, type)
}
