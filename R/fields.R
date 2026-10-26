# =============================================================================
# Field wrappers: Alias() and Describe()
# =============================================================================
#
# Both wrappers are transparent: they check values exactly like the type they
# wrap (same errors, same type name) and only attach information that list
# and data frame types, the JSON Schema export and to_json() read.

#' Give a field an external name
#'
#' @description
#' Inside \code{\link{create_list_type}()} and
#' \code{\link{create_dataframe_type}()}, \code{Alias()} sets the name a
#' field has in external data (JSON keys, CSV headers, environment
#' variables), while the parsed value keeps the R name. This lets an R
#' schema use \code{snake_case} names while the data uses \code{camelCase}
#' or any other convention.
#'
#' When parsing, the field is read from its alias and, if the alias is not
#' present, from its R name. When a value holds both, the alias wins and the
#' copy under the R name is dropped. Error messages point at the alias,
#' because that is the key that appears in the data. \code{\link{to_json}()},
#' \code{\link{dump_as}()} and \code{\link{to_json_schema}()} write the
#' alias.
#'
#' Outside a list or data frame type, \code{Alias()} has no effect. To rename
#' every field of a type with a naming convention, use the \code{alias}
#' argument of \code{\link{create_list_type}()} instead.
#'
#' @param type A sicher_type or sicher_union object.
#' @param alias A single non-empty string: the external name of the field.
#'
#' @return A new sicher_type.
#'
#' @examples
#' User <- create_list_type(list(
#'   user_id   = Alias(Scalar(Integer), "userId"),
#'   full_name = Alias(Scalar(String), "fullName")
#' ))
#'
#' user <- parse_as(list(userId = 1L, fullName = "Ann Lee"), User)
#' names(user)   # "user_id" "full_name"
#'
#' dump_as(user, User)   # back to the external names
#'
#' @export
Alias <- function(type, alias) {
  check_type_arg(type, "Alias")
  if (!is.character(alias) || length(alias) != 1 || is.na(alias) || !nzchar(alias)) {
    stop("`alias` must be a single non-empty string, e.g. Alias(String, \"userId\")",
         call. = FALSE)
  }
  new_parser_type(
    type_label(type),
    function(x, path) run_type(x, type, path),
    meta = list(kind = "alias", inner = type, alias = alias)
  )
}

#' Describe a type
#'
#' @description
#' Attaches a description, a title and examples to a type. They do not change
#' how values are checked: they are written to the JSON Schema produced by
#' \code{\link{to_json_schema}()} (\code{description}, \code{title} and
#' \code{examples} keywords) and shown when the type is printed. This makes
#' the exported schema usable as API documentation, for example in an
#' OpenAPI specification or a tool definition for a language model.
#'
#' \code{Describe()} works on any type, including whole list and data frame
#' types, which gives the object itself a description.
#'
#' @param type A sicher_type or sicher_union object.
#' @param description Optional single string describing the value.
#' @param title Optional single string: a short title.
#' @param examples Optional examples of valid values: a list of examples, or
#'   an atomic vector whose elements are each one example. Every example
#'   must satisfy \code{type}.
#'
#' @return A new sicher_type.
#'
#' @examples
#' Weather <- Describe(create_list_type(list(
#'   city = Describe(Scalar(String), "City name", examples = c("Paris", "Oslo")),
#'   unit = Describe(Default(Enum("celsius", "fahrenheit"), "celsius"),
#'                   "Temperature unit")
#' )), "Arguments of the weather tool")
#'
#' Weather
#' str(to_json_schema(Weather))
#'
#' @export
Describe <- function(type, description = NULL, title = NULL, examples = NULL) {
  check_type_arg(type, "Describe")
  check_optional_string(description, "description")
  check_optional_string(title, "title")
  if (is.null(description) && is.null(title) && is.null(examples)) {
    stop("Describe() needs at least one of `description`, `title` or `examples`",
         call. = FALSE)
  }
  if (!is.null(examples)) {
    if (!is.list(examples)) examples <- as.list(examples)
    for (i in seq_along(examples)) {
      check_type(examples[[i]], type, context = glue::glue("example {i}"))
    }
  }
  new_parser_type(
    type_label(type),
    function(x, path) run_type(x, type, path),
    meta = list(
      kind = "describe", inner = type,
      description = description, title = title, examples = examples
    )
  )
}

check_optional_string <- function(value, arg) {
  if (!is.null(value) &&
      (!is.character(value) || length(value) != 1 || is.na(value))) {
    stop(glue::glue("`{arg}` must be a single string"), call. = FALSE)
  }
}

# The explicit Alias() of a field, or NULL.
field_alias <- function(type) {
  repeat {
    if (inherits(type, "sicher_readonly")) {
      type <- type$base_type
      next
    }
    kind <- if (inherits(type, "sicher_type")) meta_kind(type) else NA_character_
    if (identical(kind, "alias")) return(type_meta(type)$alias)
    if (!(kind %in% field_wrapper_kinds)) return(NULL)
    type <- type_meta(type)$inner
  }
}

# Description, title and examples of a type, looking through other field
# wrappers. NULL when the type is not described.
type_description <- function(type) {
  repeat {
    if (inherits(type, "sicher_readonly")) {
      type <- type$base_type
      next
    }
    kind <- if (inherits(type, "sicher_type")) meta_kind(type) else NA_character_
    if (identical(kind, "describe")) return(type_meta(type))
    if (!(kind %in% field_wrapper_kinds)) return(NULL)
    type <- type_meta(type)$inner
  }
}

# -----------------------------------------------------------------------------
# Naming conventions
# -----------------------------------------------------------------------------

alias_conventions <- c("camel", "pascal", "snake", "kebab")

split_words <- function(name) {
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name)
  name <- gsub("([A-Z]+)([A-Z][a-z])", "\\1_\\2", name)
  words <- strsplit(name, "[^A-Za-z0-9]+")[[1]]
  tolower(words[nzchar(words)])
}

capitalize <- function(words) {
  paste0(toupper(substring(words, 1, 1)), substring(words, 2))
}

convert_case <- function(name, convention) {
  words <- split_words(name)
  if (length(words) == 0) return(name)
  switch(
    convention,
    camel = paste0(words[1], paste(capitalize(words[-1]), collapse = "")),
    pascal = paste(capitalize(words), collapse = ""),
    snake = paste(words, collapse = "_"),
    kebab = paste(words, collapse = "-")
  )
}

check_alias_arg <- function(alias) {
  if (is.null(alias) || is.function(alias)) return(invisible())
  if (!is.character(alias) || length(alias) != 1 || !(alias %in% alias_conventions)) {
    stop(glue::glue(
      "`alias` must be NULL, a function mapping field names to external names, ",
      "or one of {paste(encodeString(alias_conventions, quote = '\"'), collapse = ', ')}"
    ), call. = FALSE)
  }
}

# The external key of every field: its Alias(), else the `alias` naming
# convention or function applied to the field name, else the field name.
resolve_field_keys <- function(spec, alias, what = "field") {
  check_alias_arg(alias)
  fields <- names(spec)
  keys <- vapply(fields, function(field) {
    explicit <- field_alias(spec[[field]])
    if (!is.null(explicit)) return(explicit)
    if (is.null(alias)) return(field)
    key <- if (is.function(alias)) alias(field) else convert_case(field, alias)
    if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
      stop(glue::glue(
        "The `alias` function must return a single non-empty string; ",
        "it did not for {what} '{field}'"
      ), call. = FALSE)
    }
    key
  }, character(1), USE.NAMES = FALSE)
  names(keys) <- fields

  dup <- unique(keys[duplicated(keys)])
  if (length(dup) > 0) {
    stop(glue::glue(
      "Several {what}s use the same external name: {paste(dup, collapse = ', ')}"
    ), call. = FALSE)
  }
  clash <- fields[keys != fields & keys %in% fields]
  if (length(clash) > 0) {
    stop(glue::glue(
      "The external name of {what} '{clash[1]}' ('{keys[[clash[1]]]}') is also ",
      "the name of another {what}"
    ), call. = FALSE)
  }
  keys
}

# Where a field's value is in the input `present` names: its key if present,
# else its R name if present, else NULL (missing).
field_source <- function(field, key, present) {
  if (key %in% present) return(key)
  if (field %in% present) return(field)
  NULL
}

# Rename the fields (or columns) of a parsed value from their external names
# to their R names. `renames` maps source names to R names. When a value
# holds a field under both names, the external name wins and the copy under
# the R name is dropped.
rename_aliased <- function(x, renames) {
  if (length(renames) == 0) return(x)
  shadowed <- intersect(unname(renames), names(x))
  if (length(shadowed) > 0) {
    x <- x[!(names(x) %in% shadowed)]
  }
  idx <- match(names(renames), names(x))
  names(x)[idx] <- unname(renames)
  x
}
