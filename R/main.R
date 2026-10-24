#' Create a Custom Type
#'
#' @description
#' Creates a new type object for use in the type checking system. A type consists
#' of a name (for error messages) and a checker function (for validation).
#'
#' @param name A single character string representing the type name. This name
#'   will be displayed in error messages when type checking fails.
#' @param checker A function that takes a single argument and returns `TRUE` if
#'   the value matches the type, `FALSE` otherwise. The checker function should
#'   be a predicate (e.g., `is.numeric`, `is.character`).
#'
#' @return An object of class `"sicher_type"` containing:
#' \describe{
#'   \item{name}{The type name as a character string}
#'   \item{check}{The checker function}
#' }
#'
#' @details
#' This is the fundamental building block of the type system. Built-in types
#' like `Integer`, `Double`, and `String` are all created using this function.
#'
#' The checker function should:
#' \itemize{
#'   \item Accept a single argument (the value to check)
#'   \item Return `TRUE` if the value is valid for this type
#'   \item Return `FALSE` if the value is invalid
#'   \item Not throw errors (error handling is done by `check_type`)
#' }
#'
#' @seealso
#' \code{\link{check_type}} for type validation,
#' \code{\link{Scalar}} for creating scalar type variants,
#' \code{\link{Readonly}} for creating readonly type variants
#'
#' @examples
#' # Create a custom positive number
#' Positive <- create_type("positive", function(x) {
#'   is.numeric(x) && all(x > 0)
#' })
#'
#' # Use it in type annotations
#' age %:% Positive %<-% 25
#' try(age <- -5)  # Error: Type error
#'
#' # Create a custom email type
#' Email <- create_type("email", function(x) {
#'   is.character(x) &&
#'     length(x) == 1 &&
#'     grepl("^[^@]+@[^@]+\\.[^@]+$", x)
#' })
#'
#' user_email %:% Email %<-% "user@example.com"
#'
#' # Create a type for even integers
#' EvenInt <- create_type("even_int", function(x) {
#'   is.integer(x) && all(x %% 2 == 0)
#' })
#'
#' value %:% EvenInt %<-% 4L
#' try(value <- 5L)  # Error: Type error
#'
#' # Create a type that checks data frame structure
#' PersonDF <- create_type("person_df", function(x) {
#'   is.data.frame(x) &&
#'     all(c("name", "age") %in% names(x)) &&
#'     is.character(x$name) &&
#'     is.numeric(x$age)
#' })
#'
#' @export
create_type <- function(name, checker) {
  if (!is.character(name) || length(name) != 1) {
    stop(glue::glue(
      "`name` must be a single character string; ",
      "got {class(name)[1]} of length {length(name)}"
    ), call. = FALSE)
  }
  if (!is.function(checker)) {
    stop(glue::glue(
      "`checker` must be a function (e.g. is.numeric, is.character); ",
      "got {class(checker)[1]}"
    ), call. = FALSE)
  }
  structure(
    list(name = name, check = checker),
    class = "sicher_type"
  )
}

# =============================================================================
# Built-in Types (CamelCase to avoid conflicts with base R)
# =============================================================================

#' Built-in Integer Type
#'
#' @description
#' A type that checks for integer vectors.
#'
#' @export
Integer <- with_meta(create_type("integer", is.integer), kind = "integer", coerce = as.integer)

#' Built-in Double Type
#'
#' @description
#' A type that checks for double-precision numeric vectors.
#'
#' @export
Double <- with_meta(create_type("double", is.double), kind = "double", coerce = as.double)

#' Built-in Numeric Type
#'
#' @description
#' A type that checks for numeric vectors (integer or double).
#'
#' @export
Numeric <- with_meta(create_type("numeric", is.numeric), kind = "numeric", coerce = as.numeric)

#' Built-in String Type
#'
#' @description
#' A type that checks for character vectors.
#'
#' @export
String <- with_meta(create_type("string", is.character), kind = "string", coerce = as.character)

#' Built-in Boolean Type
#'
#' @description
#' A type that checks for logical vectors.
#'
#' @export
Bool <- with_meta(create_type("bool", is.logical), kind = "bool", coerce = as.logical)

#' Built-in List Type
#'
#' @description
#' A type that checks for list objects.
#'
#' @export
List <- with_meta(create_type("list", is.list), kind = "list_any", coerce = as.list)

#' Create a homogeneous list type
#'
#' @description
#' Produces a type that validates a list whose every element satisfies the
#' provided element type. This is useful when you expect a list of similar
#' records (e.g. parsed JSON array). You can further constrain the length
#' with the size operator: `ListOf(User)[10]`.
#'
#' @param element_type A sicher_type or sicher_union describing each element.
#'
#' @return A sicher_type that checks the value is a list and that all
#'   elements conform to `element_type`.
#'
#' @examples
#' # Define an inner record type
#' Record <- create_list_type(list(id = Numeric, name = String))
#'
#' # Now require a list of records
#' Records <- ListOf(Record)
#' records %:% Records %<-% list(
#'   list(id = 1, name = "a"),
#'   list(id = 2, name = "b")
#' )
#'
#' # fixed-size list of ten records
#' TenRecs <- Records[10]
#' # will throw if length != 10
#'
#' @export
ListOf <- function(element_type) {
  if (!inherits(element_type, "sicher_type") &&
      !inherits(element_type, "sicher_union")) {
    stop(glue::glue(
      "ListOf() requires a sicher_type or sicher_union as its argument; ",
      "got {class(element_type)[1]}. ",
      "Example: ListOf(String) or ListOf(create_list_type(list(id = Numeric)))"
    ), call. = FALSE)
  }
  type_name <- glue::glue("list<{element_type$name}>")
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.list(x) || is.data.frame(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      issues <- list()
      for (i in seq_along(x)) {
        res <- run_type(x[[i]], element_type, c(path, list(i)))
        issues <- c(issues, res$issues)
        if (length(res$issues) == 0 && !identical(res$value, x[[i]])) {
          x[i] <- list(res$value)
        }
      }
      list(value = x, issues = issues)
    },
    meta = list(kind = "list_of", inner = element_type),
    strict_check = "never"
  )
}

#' Built-in Any Type
#'
#' @description
#' A type that accepts any value.
#'
#' @export
Any <- with_meta(create_type("any", function(x) TRUE), kind = "any")

#' Built-in Null Type
#'
#' @description
#' A type that checks for NULL values.
#'
#' @export
Null <- with_meta(create_type("null", is.null), kind = "null")

#' Built-in Function Type
#'
#' @description
#' A type that checks for function objects.
#'
#' @export
Function <- with_meta(create_type("Function", is.function), kind = "function_any")

#' Built-in DataFrame Type
#'
#' @description
#' A type that checks for data.frame objects.
#'
#' @export
DataFrame <- with_meta(create_type("data.frame", is.data.frame), kind = "data.frame_any", coerce = as.data.frame)

format_enum_value <- function(value) {
  if (is.character(value)) {
    return(encodeString(value, quote = '"'))
  }

  if (length(value) == 1) {
    return(as.character(value))
  }

  deparse(value)[1]
}

enum_value_matches <- function(value, allowed) {
  if (is.numeric(value) && is.numeric(allowed)) {
    return(identical(as.numeric(value), as.numeric(allowed)))
  }

  identical(value, allowed)
}

literal_value_matches <- function(value, allowed) {
  identical(value, allowed)
}

#' Enum Type Factory
#'
#' @description
#' Creates an enumeration type using regular function call syntax. The resulting type only
#' accepts atomic vectors whose elements all belong to the declared set of
#' allowed values.
#'
#' @param ... Allowed scalar values or a single atomic vector of allowed values.
#'
#' @return A new sicher_type that checks all values belong to the enum.
#'
#' @examples
#' status %:% Enum(1, 2, 3) %<-% 2
#' colors %:% Enum("red", "green", "blue") %<-% c("red", "blue")
#' try(colors <- c("yellow", "red"))
#' @export
Enum <- function(...) {
  allowed_args <- list(...)

  if (length(allowed_args) == 0) {
    stop(
      "Enum() requires at least one allowed value, e.g. Enum(1, 2, 3) or Enum(c('a', 'b'))",
      call. = FALSE
    )
  }

  if (length(allowed_args) == 1 && is.atomic(allowed_args[[1]]) && is.null(dim(allowed_args[[1]]))) {
    allowed_values <- as.list(unname(allowed_args[[1]]))
  } else {
    are_scalar_atomic <- vapply(
      allowed_args,
      function(value) is.atomic(value) && is.null(dim(value)) && length(value) == 1,
      logical(1)
    )

    if (!all(are_scalar_atomic)) {
      stop(
        "Enum() values must be scalar atomic values or a single atomic vector of allowed values.",
        call. = FALSE
      )
    }

    allowed_values <- unname(allowed_args)
  }

  if (length(allowed_values) == 0) {
    stop(
      "Enum() requires at least one allowed value.",
      call. = FALSE
    )
  }

  type_name <- glue::glue(
    "enum[{paste(vapply(allowed_values, format_enum_value, character(1)), collapse = ', ')}]"
  )

  with_meta(
    create_type(
      type_name,
      function(x) {
        if (!is.atomic(x) || !is.null(dim(x))) {
          return(FALSE)
        }

        all(vapply(as.list(x), function(value) {
          any(vapply(allowed_values, function(allowed) {
            enum_value_matches(value, allowed)
          }, logical(1)))
        }, logical(1)))
      }
    ),
    kind = "enum", values = allowed_values
  )
}

#' Literal Type Factory
#'
#' @description
#' Creates a literal type inspired by TypeScript literal unions. The resulting
#' type only accepts scalar atomic values that exactly match one of the declared
#' literals, including the underlying R storage mode. For example, `200` and
#' `200L` are treated as different literals.
#'
#' @param ... Allowed scalar atomic literal values.
#'
#' @return A new sicher_type that checks the value is exactly one of the declared literals.
#'
#' @examples
#' direction %:% Literal("left", "right") %<-% "left"
#' direction <- "right"
#' direction <- "left"
#' try(direction <- c("right", "left"))
#' status_code %:% Literal(200, 404) %<-% 200
#' try(status_code <- 500)
#' @export
Literal <- function(...) {
  allowed_values <- unname(list(...))

  if (length(allowed_values) == 0) {
    stop(
      "Literal() requires at least one allowed value, e.g. Literal('left', 'right') or Literal(200, 404)",
      call. = FALSE
    )
  }

  are_scalar_atomic <- vapply(
    allowed_values,
    function(value) is.atomic(value) && is.null(dim(value)) && length(value) == 1,
    logical(1)
  )

  if (!all(are_scalar_atomic)) {
    stop(
      "Literal() values must be scalar atomic values.",
      call. = FALSE
    )
  }

  type_name <- glue::glue(
    "literal[{paste(vapply(allowed_values, format_enum_value, character(1)), collapse = ', ')}]"
  )

  with_meta(
    create_type(
      type_name,
      function(x) {
        if (!is.atomic(x) || !is.null(dim(x)) || length(x) != 1) {
          return(FALSE)
        }

        any(vapply(allowed_values, function(allowed) {
          literal_value_matches(x, allowed)
        }, logical(1)))
      }
    ),
    kind = "literal", values = allowed_values
  )
}

# =============================================================================
# Vector Size Types
# =============================================================================

#' Vector Size Operator for sicher_type
#'
#' @description
#' Creates a type that checks for a specific vector length.
#'
#' @param type A sicher_type object
#' @param size The required length (non-negative integer)
#'
#' @return A new sicher_type that checks for the specified length
#'
#' @examples
#' vec %:% Numeric[3] %<-% c(1, 2, 3)
#' try(vec <- c(1, 2)) # Error: wrong length
#' try(vec <- c("a", "b", "c"))  # Error: wrong type
#'
#' @export
`[.sicher_type` <- function(type, size) {
  is_valid_size <- is.numeric(size) && length(size) == 1 &&
    size >= 0 && size == as.integer(size)
  if (!is_valid_size) {
    stop(glue::glue(
      "Size must be a non-negative integer (e.g. Numeric[3]), got {deparse(size)}"
    ), call. = FALSE)
  }
  size <- as.integer(size)
  type_name <- glue::glue("{type$name}[{size}]")
  new_parser_type(
    type_name,
    function(x, path) {
      if (length(x) != size) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      delegate(x, type, path, type_name)
    },
    meta = list(kind = "sized", inner = type, size = size)
  )
}

# =============================================================================
# List/Object Types
# =============================================================================

#' Create a List Type with Specific Structure
#'
#' @description
#' Creates a type that checks for lists with specific named elements and their types.
#' Similar to object types in TypeScript/JavaScript.
#'
#' @param type_spec A named list where names are field names and values are sicher_type objects
#' @param extra What to do with fields that are not part of \code{type_spec}:
#'   \code{"error"} (the default) rejects them, \code{"allow"} keeps them
#'   unchecked, and \code{"drop"} removes them from the parsed value (similar
#'   to zod's \code{.strict()}, \code{.passthrough()} and \code{.strip()}).
#'   Typed variables and \code{\link{parse_as}()} store the parsed value, so
#'   with \code{"drop"} unknown fields are silently removed.
#' @param alias Optional external names for the fields: a naming convention
#'   (\code{"camel"}, \code{"pascal"}, \code{"snake"} or \code{"kebab"}) or
#'   a function that takes a field name and returns its external name. Each
#'   field is then read from its external name (or, if that is absent, from
#'   its R name) and stored under its R name. Fields wrapped in
#'   \code{\link{Alias}()} keep their own alias. See \code{\link{Alias}()}.
#'
#' @return A sicher_type that validates list structure
#'
#' @examples
#' # Define a User type
#' User <- create_list_type(list(
#'   name = String,
#'   age = Numeric,
#'   preferences = create_list_type(list(
#'     color = String,
#'     movie = String
#'   ))
#' ))
#'
#' # Use it
#' user %:% User %<-% list(
#'   name = "Alice",
#'   age = 25,
#'   preferences = list(color = "red", movie = "batman")
#' )
#'
#' # Read camelCase data into snake_case fields
#' Account <- create_list_type(list(
#'   account_id = Scalar(Integer),
#'   created_at = Scalar(String)
#' ), alias = "camel")
#' parse_as(list(accountId = 1L, createdAt = "2024-01-05"), Account)
#'
#' @export
create_list_type <- function(type_spec, extra = c("error", "allow", "drop"),
                             alias = NULL) {
  extra <- match.arg(extra)

  if (!is.list(type_spec) || is.null(names(type_spec))) {
    stop(glue::glue(
      "`type_spec` must be a named list mapping field names to types; ",
      "got {class(type_spec)[1]}. ",
      "Example: create_list_type(list(name = String, age = Numeric))"
    ), call. = FALSE)
  }

  if (any(names(type_spec) == "")) {
    stop(
      "All elements in `type_spec` must be named. ",
      "Provide a name for every field, e.g. list(name = String, age = Numeric)",
      call. = FALSE
    )
  }

  for (i in seq_along(type_spec)) {
    if (!is_sicher_type_like(type_spec[[i]])) {
      stop(glue::glue(
        "Field '{names(type_spec)[i]}' must be a sicher_type or sicher_union ",
        "(e.g. String, Numeric, Optional(Bool)); ",
        "got {class(type_spec[[i]])[1]}"
      ), call. = FALSE)
    }
  }

  field_names <- names(type_spec)
  keys <- resolve_field_keys(type_spec, alias)
  has_aliases <- any(keys != field_names)
  is_optional <- vapply(type_spec, field_is_optional, logical(1))
  type_labels <- vapply(type_spec, type_label, character(1))
  field_parts <- glue::glue(
    "{field_names}{ifelse(is_optional, '?', '')}: {type_labels}"
  )
  type_name <- glue::glue("{{{paste(field_parts, collapse = ', ')}}}")

  result <- new_parser_type(
    type_name,
    function(x, path) {
      if (!is.list(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }

      issues <- list()
      present <- names(x)
      if (is.null(present)) present <- character(0)
      renames <- character(0)

      for (field in field_names) {
        key <- keys[[field]]
        source <- field_source(field, key, present)
        field_type <- type_spec[[field]]
        if (!is.null(source)) {
          res <- run_type(x[[source]], field_type, c(path, list(source)))
          issues <- c(issues, res$issues)
          if (length(res$issues) == 0 && !identical(res$value, x[[source]])) {
            x[source] <- list(res$value)
          }
          if (source != field) renames[source] <- field
        } else {
          default <- field_default(field_type)
          if (default$has) {
            x[field] <- list(default$value)
          } else if (!field_is_optional(field_type)) {
            issues <- c(issues, list(new_issue(
              c(path, list(key)),
              glue::glue("Missing required field (expected {type_label(field_type)})"),
              expected = type_label(field_type),
              got = "missing",
              code = "missing_field"
            )))
          }
        }
      }

      unknown <- setdiff(present, c(field_names, keys))
      if (length(unknown) > 0) {
        if (extra == "error") {
          for (field in unknown) {
            issues <- c(issues, list(new_issue(
              c(path, list(field)),
              glue::glue(
                "Unexpected field (valid fields: {paste(keys, collapse = ', ')})"
              ),
              expected = "no field",
              got = get_type_name(x[[field]]),
              code = "unexpected_field"
            )))
          }
        } else if (extra == "drop") {
          x <- x[!(names(x) %in% unknown)]
        }
      }

      if (has_aliases && length(issues) == 0) {
        x <- rename_aliased(x, renames)
      }

      list(value = x, issues = issues)
    },
    meta = list(kind = "list", spec = type_spec, extra = extra,
                alias = alias, keys = keys)
  )

  # Store the raw spec so extend() can retrieve it later.
  attr(result, "sicher_spec") <- type_spec
  result
}

#' Extend a structured list type with additional fields
#'
#' @description
#' Creates a new list type by merging the field specification of an existing
#' \code{create_list_type()} type with a set of additional fields.  Analogous
#' to TypeScript interface extension (\code{interface Employee extends Person}).
#'
#' @param base A \code{sicher_type} produced by \code{create_list_type()}.
#'   Must carry a \code{sicher_spec} attribute (all types built with the
#'   patched \code{create_list_type()} above automatically do).
#' @param extra A named list of additional fields in the same format accepted
#'   by \code{create_list_type()} -- names are field names, values are
#'   \code{sicher_type} or \code{sicher_union} objects.
#'
#' @return A new \code{sicher_type} whose required and optional fields are the
#'   union of \code{base}'s fields and \code{extra}'s fields.  Fields in
#'   \code{extra} that share a name with a field in \code{base} \emph{override}
#'   the base field's type (with a warning so the shadowing is never silent).
#'
#' @examples
#' Person <- create_list_type(list(
#'   name = String,
#'   age  = Numeric
#' ))
#'
#' # Basic extension
#' Employee <- extend(Person, list(
#'   role       = String,
#'   department = Optional(String)
#' ))
#'
#' emp %:% Employee %<-% list(name = "Alice", age = 30, role = "Engineer")
#'
#' # Multi-level extension
#' Manager <- extend(Employee, list(
#'   reports = Numeric   # number of direct reports
#' ))
#'
#' mgr %:% Manager %<-% list(
#'   name = "Bob", age = 45, role = "VP", reports = 12
#' )
#'
#' # Field override (emits a warning)
#' DetailedPerson <- extend(Person, list(
#'   age = Integer   # narrows Numeric -> Integer
#' ))
#'
#' @export
extend <- function(base, extra) {
  # ---- validate base --------------------------------------------------------
  if (!inherits(base, "sicher_type")) {
    stop(glue::glue(
      "`base` must be a sicher_type produced by create_list_type(); ",
      "got {class(base)[1]}"
    ), call. = FALSE)
  }

  base_spec <- object_spec(base)

  if (is.null(base_spec)) {
    stop(
      "`base` does not carry a field specification. ",
      "Only types created with create_list_type() or create_dataframe_type() can be extended. ",
      "Primitive types (String, Numeric, ...) and custom create_type() types ",
      "cannot be used as a base for extend().",
      call. = FALSE
    )
  }

  # ---- validate extra -------------------------------------------------------
  if (!is.list(extra) || is.null(names(extra)) || any(names(extra) == "")) {
    stop(
      "`extra` must be a fully named list of fields, ",
      "e.g. list(role = String, department = Optional(String))",
      call. = FALSE
    )
  }

  for (i in seq_along(extra)) {
    if (!inherits(extra[[i]], "sicher_type") &&
        !inherits(extra[[i]], "sicher_union")) {
      stop(glue::glue(
        "Field '{names(extra)[i]}' in `extra` must be a sicher_type or sicher_union; ",
        "got {class(extra[[i]])[1]}"
      ), call. = FALSE)
    }
  }

  # ---- warn on overridden fields --------------------------------------------
  overridden <- intersect(names(base_spec), names(extra))
  if (length(overridden) > 0) {
    warning(glue::glue(
      "extend(): the following field(s) from the base type are being overridden: ",
      "{paste(overridden, collapse = ', ')}. ",
      "The extra type will take precedence."
    ), call. = FALSE)
  }

  # ---- merge: base first, extra fields win on collision --------------------
  merged_spec <- c(
    base_spec[setdiff(names(base_spec), names(extra))],
    extra
  )

  rebuild_object_type(base, merged_spec)
}

#' Create a Data Frame Type with Column Specification
#'
#' @description
#' Builds a type that validates a data frame's column names and their types.
#' Each column is treated as a vector and checked against the provided sicher_type
#' (or union) specification. Optional columns may be declared with `Optional()`.
#'
#' @param col_spec A named list where names are column names and values are
#'   sicher_type or sicher_union objects describing the expected column type.
#' @param extra What to do with columns that are not part of \code{col_spec}:
#'   \code{"error"} (the default), \code{"allow"} or \code{"drop"}. See
#'   \code{\link{create_list_type}()}.
#' @param alias Optional external column names: a naming convention or a
#'   function, as in \code{\link{create_list_type}()}. Columns are read from
#'   their external names and renamed to the R names.
#'
#' @return A sicher_type representing the data frame schema.
#'
#' @examples
#' PersonDF <- create_dataframe_type(list(
#'   name = String,
#'   age = Numeric,
#'   height = Optional(Numeric)
#' ))
#'
#' df %:% PersonDF %<-% data.frame(
#'   name = c("Alice", "Bob"),
#'   age = c(25, 30)
#' )
#'
#' @export
create_dataframe_type <- function(col_spec, extra = c("error", "allow", "drop"),
                                  alias = NULL) {
  extra <- match.arg(extra)

  if (!is.list(col_spec) || is.null(names(col_spec))) {
    stop(glue::glue(
      "`col_spec` must be a named list mapping column names to types; ",
      "got {class(col_spec)[1]}. ",
      "Example: create_dataframe_type(list(name = String, age = Numeric))"
    ), call. = FALSE)
  }
  if (any(names(col_spec) == "")) {
    stop(
      "All columns in `col_spec` must be named. ",
      "Provide a name for every column, e.g. list(name = String, age = Numeric)",
      call. = FALSE
    )
  }

  # Validate each column type specification
  for (i in seq_along(col_spec)) {
    if (!is_sicher_type_like(col_spec[[i]])) {
      stop(glue::glue(
        "Column '{names(col_spec)[i]}' must be a sicher_type or sicher_union ",
        "(e.g. String, Numeric, Optional(Bool)); ",
        "got {class(col_spec[[i]])[1]}"
      ), call. = FALSE)
    }
  }

  col_names <- names(col_spec)
  keys <- resolve_field_keys(col_spec, alias, what = "column")
  has_aliases <- any(keys != col_names)
  is_optional <- vapply(col_spec, field_is_optional, logical(1))
  type_labels <- vapply(col_spec, type_label, character(1))
  col_parts <- glue::glue("{col_names}{ifelse(is_optional, '?', '')}: {type_labels}")
  type_name <- glue::glue("data.frame{{{paste(col_parts, collapse = ', ')}}}")

  result <- new_parser_type(
    type_name,
    function(x, path) {
      if (!is.data.frame(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }

      issues <- list()
      renames <- character(0)
      for (col in col_names) {
        key <- keys[[col]]
        source <- field_source(col, key, names(x))
        col_type <- col_spec[[col]]
        if (!is.null(source)) {
          col_path <- c(path, list(source))
          res <- run_type(x[[source]], col_type, col_path)
          # Positions in a column are row numbers
          res$issues <- lapply(res$issues, function(issue) {
            if (!is.null(issue$index) && identical(issue$path, col_path)) {
              issue$index_label <- "row"
            }
            issue
          })
          issues <- c(issues, res$issues)
          if (length(res$issues) == 0 && !is.null(res$value) &&
              !identical(res$value, x[[source]])) {
            x[[source]] <- res$value
          }
          if (source != col) renames[source] <- col
        } else {
          default <- field_default(col_type)
          if (default$has) {
            x[[col]] <- rep_len(default$value, nrow(x))
          } else if (!field_is_optional(col_type)) {
            issues <- c(issues, list(new_issue(
              c(path, list(key)),
              glue::glue("Missing required column (expected {type_label(col_type)})"),
              expected = type_label(col_type),
              got = "missing",
              code = "missing_field"
            )))
          }
        }
      }

      unknown <- setdiff(names(x), c(col_names, keys))
      if (length(unknown) > 0) {
        if (extra == "error") {
          for (col in unknown) {
            issues <- c(issues, list(new_issue(
              c(path, list(col)),
              glue::glue(
                "Unexpected column (valid columns: {paste(keys, collapse = ', ')})"
              ),
              expected = "no column",
              got = get_type_name(x[[col]]),
              code = "unexpected_field"
            )))
          }
        } else if (extra == "drop") {
          x <- x[, !(names(x) %in% unknown), drop = FALSE]
        }
      }

      if (has_aliases && length(issues) == 0) {
        x <- rename_aliased(x, renames)
      }

      list(value = x, issues = issues)
    },
    meta = list(kind = "data.frame", spec = col_spec, extra = extra,
                alias = alias, keys = keys)
  )

  attr(result, "sicher_spec") <- col_spec
  result
}

#' Create a scalar (length-1) type variant
#'
#' @description
#' Creates a type that only accepts single values (vectors of length 1).
#'
#' @param type A sicher_type object
#'
#' @return A new sicher_type that checks for length 1
#'
#' @examples
#' age %:% Scalar(Integer) %<-% 30L
#' try(age <- c(30L, 40L))  # Error: not scalar
#'
#' @export
Scalar <- function(type) {
  if (!inherits(type, "sicher_type") && !inherits(type, "sicher_union")) {
    stop(glue::glue(
      "Scalar() requires a type argument (sicher_type, e.g. Scalar(Numeric) or Scalar(String)); ",
      "got {class(type)[1]}"
    ), call. = FALSE)
  }
  type_name <- glue::glue("scalar<{type$name}>")
  new_parser_type(
    type_name,
    function(x, path) {
      if (length(x) != 1) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      delegate(x, type, path, type_name)
    },
    meta = list(kind = "scalar", inner = type)
  )
}

#' NonNA modifier — reject values containing NAs
#'
#' @description
#' Creates a type that accepts only non-NA values of the underlying type.
#' Any element equal to \code{NA} causes an immediate error, making silent NA
#' propagation impossible in typed pipelines.
#'
#' @param type A \code{sicher_type} or \code{sicher_union} object.
#'
#' @return A new \code{sicher_type} that first validates the underlying type,
#'   then rejects any value that contains at least one \code{NA}.
#'
#' @examples
#' salary %:% NonNA(Numeric) %<-% c(1800, 2300, 4000)
#' try(salary <- c(1800, NA, 4000))  # Error: value contains NA(s)
#'
#' # Composable with other modifiers
#' tag %:% NonNA(Scalar(String)) %<-% "admin"
#'
#' @export
NonNA <- function(type) {
  if (!inherits(type, "sicher_type") && !inherits(type, "sicher_union")) {
    stop(glue::glue(
      "NonNA() requires a sicher_type or sicher_union argument ",
      "(e.g. NonNA(Numeric), NonNA(String)); got {class(type)[1]}"
    ), call. = FALSE)
  }
  base_name <- type$name
  new_parser_type(
    glue::glue("non_na<{base_name}>"),
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      if (anyNA(res$value)) {
        return(issue_result(res$value, new_issue(
          path,
          glue::glue(
            "expected non_na<{base_name}> (no NA values), ",
            "but value contains NA(s)"
          ),
          expected = glue::glue("non_na<{base_name}>"),
          got = "NA",
          code = "na_value",
          index = failing_positions(res$value, is.na(res$value))
        )))
      }
      res
    },
    meta = list(kind = "non_na", inner = type),
    strict_check = TRUE
  )
}

#' Between — closed-interval numeric range constraint
#'
#' @description
#' Creates a type that accepts numeric values within the closed interval
#' \code{[min, max]}. Every element of a vector must satisfy the bounds.
#' NA values are always rejected.
#'
#' @param min A single non-NA numeric scalar giving the lower bound (inclusive).
#' @param max A single non-NA numeric scalar giving the upper bound (inclusive).
#'
#' @return A new \code{sicher_type} that checks the value is numeric, NA-free,
#'   and that all elements lie within \code{[min, max]}.
#'
#' @examples
#' age   %:% Between(0, 150)   %<-% 30
#' score %:% Between(0.0, 1.0) %<-% 0.95
#' try(score <- 1.5)   # Error: 1.5 is outside [0, 1]
#' try(score <- NA_real_)  # Error: value contains NA(s)
#'
#' # Works with integer storage (is.numeric(1L) is TRUE in R)
#' count %:% Between(0L, 100L) %<-% 42L
#'
#' @export
Between <- function(min, max) {
  if (!is.numeric(min) || length(min) != 1 || is.na(min)) {
    stop(
      "`min` must be a single non-NA numeric scalar (e.g. Between(0, 100))",
      call. = FALSE
    )
  }
  if (!is.numeric(max) || length(max) != 1 || is.na(max)) {
    stop(
      "`max` must be a single non-NA numeric scalar (e.g. Between(0, 100))",
      call. = FALSE
    )
  }
  if (min > max) {
    stop(glue::glue("`min` ({min}) must be <= `max` ({max})"), call. = FALSE)
  }
  numeric_rule_type(
    glue::glue("between[{min}, {max}]"),
    function(x) x >= min & x <= max,
    glue::glue("outside [{min}, {max}]"),
    glue::glue("outside [{min}, {max}]"),
    meta = list(kind = "between", min = min, max = max, coerce = as.numeric)
  )
}

#' Matches — regex-constrained string type
#'
#' @description
#' Creates a type that accepts only character vectors whose every element
#' matches the given Perl-compatible regular expression. NA elements are
#' always rejected. An empty character vector \code{character(0)} passes
#' vacuously; combine with \code{NonEmpty()} if you need at least one element.
#'
#' @param pattern A single non-NA character string used as a PCRE regex
#'   (passed to \code{grepl(..., perl = TRUE)}).
#'
#' @return A new \code{sicher_type} that validates every element against
#'   \code{pattern}.
#'
#' @examples
#' email %:% Matches("^[^@]+@[^@]+\\.[^@]+$") %<-% "user@example.com"
#' try(email <- "not-an-email")   # Error: does not match pattern
#'
#' hex %:% Matches("^#[0-9A-Fa-f]{6}$") %<-% "#FF5733"
#'
#' # Combine with NonEmpty to also require at least one element
#' tags %:% NonEmpty(Matches("^[a-z]+$")) %<-% c("foo", "bar")
#'
#' @export
Matches <- function(pattern) {
  if (!is.character(pattern) || length(pattern) != 1 || is.na(pattern)) {
    stop(
      "`pattern` must be a single non-NA character string ",
      "(e.g. Matches(\"^[A-Z]+$\"))",
      call. = FALSE
    )
  }
  tryCatch(
    withCallingHandlers(
      grepl(pattern, "", perl = TRUE),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    ),
    error = function(e) stop(
      glue::glue("Invalid regex in Matches(): {conditionMessage(e)}"),
      call. = FALSE
    )
  )
  type_name <- glue::glue("matches(\"{pattern}\")")
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.character(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if (anyNA(x)) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("expected {type_name}, but value contains NA(s)"),
          expected = type_name, got = "NA", code = "na_value",
          index = failing_positions(x, is.na(x))
        )))
      }
      fails <- !grepl(pattern, x, perl = TRUE)
      failing <- x[fails]
      if (length(failing) > 0) {
        quoted  <- encodeString(utils::head(failing, 3), quote = "\"")
        preview <- paste(quoted, collapse = ", ")
        if (length(failing) > 3) preview <- paste0(preview, ", ...")
        noun <- if (length(failing) == 1) "value" else "values"
        verb <- if (length(failing) == 1) "does"  else "do"
        return(issue_result(x, new_issue(
          path,
          glue::glue(
            "expected {type_name}, ",
            "but {noun} [{preview}] {verb} not match the pattern"
          ),
          expected = type_name, got = get_type_name(x), code = "invalid_string",
          index = failing_positions(x, fails)
        )))
      }
      ok_result(x)
    },
    meta = list(kind = "matches", pattern = pattern),
    strict_check = TRUE
  )
}

#' NonEmpty modifier — require a non-empty value
#'
#' @description
#' Creates a type that first validates the underlying type, then rejects
#' empty values. For data frames "empty" means zero rows (\code{nrow == 0});
#' for all other objects it means \code{length == 0}.
#'
#' @param type A \code{sicher_type} or \code{sicher_union} object.
#'
#' @return A new \code{sicher_type} that rejects zero-length (or zero-row)
#'   values after the base type check passes.
#'
#' @examples
#' tags %:% NonEmpty(String) %<-% c("r", "types")
#' try(tags <- character(0))   # Error: value must be non-empty
#'
#' items %:% NonEmpty(List) %<-% list(1, 2)
#' try(items <- list())        # Error: value must be non-empty
#'
#' @export
NonEmpty <- function(type) {
  if (!inherits(type, "sicher_type") && !inherits(type, "sicher_union")) {
    stop(glue::glue(
      "NonEmpty() requires a sicher_type or sicher_union argument ",
      "(e.g. NonEmpty(String), NonEmpty(List)); got {class(type)[1]}"
    ), call. = FALSE)
  }
  base_name <- type$name
  new_parser_type(
    glue::glue("non_empty<{base_name}>"),
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      if (value_size(res$value) == 0L) {
        return(issue_result(res$value, new_issue(
          path,
          glue::glue(
            "expected non_empty<{base_name}> (length > 0), ",
            "but got an empty value"
          ),
          expected = glue::glue("non_empty<{base_name}>"),
          got = get_type_name(res$value),
          code = "too_small"
        )))
      }
      res
    },
    meta = list(kind = "min_length", inner = type, n = 1L),
    strict_check = TRUE
  )
}

#' Create a readonly type variant
#'
#' @description
#' Creates a type that prevents reassignment after initial value is set.
#'
#' @param type A sicher_type object
#'
#' @return A readonly type modifier
#'
#' @examples
#' PI %:% Readonly(Double) %<-% 3.14159
#' try(PI <- 3.0)  # Error: cannot reassign readonly
#'
#' @export
Readonly <- function(type) {
  actual_type <- if (inherits(type, "sicher_readonly")) type$base_type else type
  structure(
    list(base_type = actual_type),
    class = c("sicher_readonly", "sicher_type_modifier")
  )
}

#' Create an optional (nullable) type variant
#'
#' @description
#' Creates a type that accepts NULL values in addition to the base type.
#'
#' @param type A sicher_type object
#'
#' @return A union type that includes Null
#'
#' @examples
#' middle_name %:% Optional(String) %<-% NULL
#' middle_name <- "Marie"  # Also OK
#'
#' @export
Optional <- function(type) {
  create_union(type, Null)
}

#' Create a nullable type variant
#'
#' @description
#' Creates a type that accepts \code{NULL} in addition to the base type, like
#' \code{Optional()}. The difference shows up inside
#' \code{\link{create_list_type}()} and \code{\link{create_dataframe_type}()}:
#' an \code{Optional()} field may be left out entirely, while a
#' \code{Nullable()} field must be present but may hold \code{NULL}. This
#' mirrors the distinction between \code{field?: T} and
#' \code{field: T | null} in TypeScript (zod's \code{.optional()} versus
#' \code{.nullable()}).
#'
#' @param type A sicher_type or sicher_union object.
#'
#' @return A union type that includes Null and is marked as nullable.
#'
#' @examples
#' Profile <- create_list_type(list(
#'   name     = String,
#'   nickname = Optional(String),  # may be missing
#'   bio      = Nullable(String)   # must be present, may be NULL
#' ))
#'
#' check_type(list(name = "Ann", bio = NULL), Profile)
#' try(check_type(list(name = "Ann"), Profile))  # Error: 'bio' is missing
#'
#' @export
Nullable <- function(type) {
  check_type_arg(type, "Nullable")
  result <- create_union(type, Null)
  attr(result, "sicher_nullable") <- TRUE
  result
}

# =============================================================================
# Union Types
# =============================================================================

create_union <- function(type1, type2) {
  union_members <- function(type) {
    if (inherits(type, "sicher_union")) return(type$types)
    if (inherits(type, "sicher_readonly")) return(list(type$base_type))
    if (inherits(type, "sicher_type")) return(list(type))
    if (is.null(type)) return(list(Null))
    stop(glue::glue(
      "Union members must be sicher types (e.g. String | Numeric); ",
      "got {class(type)[1]}"
    ), call. = FALSE)
  }

  types_list <- c(union_members(type1), union_members(type2))
  # Drop repeated members (Integer | Numeric | Integer)
  keep <- !vapply(seq_along(types_list), function(i) {
    any(vapply(types_list[seq_len(i - 1)], identical, logical(1), types_list[[i]]))
  }, logical(1))
  types_list <- types_list[keep]
  if (length(types_list) == 1) return(types_list[[1]])
  names_list <- vapply(types_list, function(t) t$name, character(1))

  structure(
    list(name = paste(names_list, collapse = " | "), types = types_list),
    class = "sicher_union"
  )
}

#' Union Type Operator
#'
#' @description
#' S3 methods for the `|` operator to create union types.
#'
#' @param type1 First type (sicher_type or sicher_union object)
#' @param type2 Second type (sicher_type or sicher_union object)
#'
#' @return A union type (sicher_union object)
#'
#' @name union-operator
#' @aliases |.sicher_type |.sicher_union
#' @export
`|.sicher_type` <- function(type1, type2) create_union(type1, type2)

#' @rdname union-operator
#' @export
`|.sicher_union` <- function(type1, type2) create_union(type1, type2)

#' Intersection Type Operator
#'
#' @description
#' S3 methods for the \code{&} operator to create intersection types, like
#' \code{A & B} in TypeScript. A value must satisfy both types.
#'
#' When both sides are structured list types (from
#' \code{\link{create_list_type}()}) the result is a single list type that
#' has the fields of both sides, so \code{Person & Timestamps} accepts a list
#' with all of the fields. Fields present on both sides must satisfy both
#' field types. When both sides are data frame types the columns are merged
#' the same way. Any other combination produces a type that runs both checks.
#'
#' @param type1 First type (sicher_type or sicher_union object)
#' @param type2 Second type (sicher_type or sicher_union object)
#'
#' @return A sicher_type
#'
#' @examples
#' Named <- create_list_type(list(name = String))
#' Aged  <- create_list_type(list(age = Numeric))
#'
#' person %:% (Named & Aged) %<-% list(name = "Ann", age = 31)
#'
#' # Combine a base type with a rule
#' Score <- Numeric & Between(0, 100)
#' check_type(42, Score)
#'
#' @name intersection-operator
#' @aliases &.sicher_type &.sicher_union
#' @export
`&.sicher_type` <- function(type1, type2) create_intersection(type1, type2)

#' @rdname intersection-operator
#' @export
`&.sicher_union` <- `&.sicher_type`

# =============================================================================
# Type Checking Engine
# =============================================================================

get_type_name <- function(value) {
  if (is.null(value)) return("null")
  if (is.data.frame(value)) {
    return(glue::glue("data.frame[{nrow(value)} x {ncol(value)}]"))
  }
  base_type <- if (is.integer(value)) "integer"
               else if (is.double(value)) "double"
               else if (is.character(value)) "string"
               else if (is.logical(value)) "bool"
               else if (is.list(value)) "list"
               else class(value)[1]
  if (length(value) == 0) {
    glue::glue("{base_type}(0)")
  } else if (length(value) != 1) {
    glue::glue("{base_type} of length {length(value)}")
  } else {
    base_type
  }
}

#' Type Checking Function
#'
#' @description
#' Validates that a value conforms to a specified type. This is the core
#' validation function used internally by the type system, but can also be
#' called directly for manual type checking.
#'
#' @param value The value to check
#' @param type A sicher_type, sicher_union, or sicher_readonly object
#' @param context Optional character string describing where the check is occurring
#'   (used in error messages)
#'
#' @return Returns `TRUE` if the value matches the type, otherwise throws an
#'   error of class \code{"sicher_type_error"} with a descriptive message.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Checks if a value matches a type specification
#'   \item Handles union types (checks if value matches any type in the union)
#'   \item Handles readonly types (strips the readonly modifier before checking)
#'   \item Reports every problem it finds in nested lists, data frames and
#'     \code{ListOf()} values, each with its location (e.g.
#'     \code{todos[[2]]$title})
#' }
#'
#' The condition thrown on failure has class \code{"sicher_type_error"} and an
#' \code{issues} element holding the individual problems, so it can be
#' caught with \code{tryCatch(..., sicher_type_error = function(e) e$issues)}.
#' Use \code{\link{is_type}()} or \code{\link{safe_parse}()} for a check
#' that does not throw.
#'
#' @seealso
#' \code{\link{create_type}} for creating custom types,
#' \code{\link{is_type}}, \code{\link{safe_parse}}, \code{\link{parse_as}}
#'
#' @examples
#' # Direct type checking
#' check_type(5L, Integer)  # Returns TRUE
#' try(check_type("hello", Integer))  # Throws error
#'
#' # With context for better error messages
#' try(check_type(5L, String, context = "user_name"))
#'
#' # With union types
#' check_type(5L, Integer | String)  # Returns TRUE
#' try(check_type(5.5, Integer | String))  # Throws error
#'
#' @export
check_type <- function(value, type, context = NULL) {
  res <- run_type(value, type, if (is.null(context)) list() else list(context))
  if (length(res$issues) > 0) {
    sicher_abort(res$issues)
  }
  TRUE
}

# =============================================================================
# Typed Variable Binding
# =============================================================================

create_typed_binding <- function(var_name, value, type, envir) {
  is_readonly <- inherits(type, "sicher_readonly")
  actual_type <- if (is_readonly) type$base_type else type

  value <- enforce_type(value, actual_type, var_name)

  if (exists(var_name, envir = envir, inherits = FALSE)) {
    rm(list = var_name, envir = envir)
  }

  private_env <- new.env(parent = emptyenv())
  private_env$value <- value
  private_env$type <- actual_type
  private_env$readonly <- is_readonly

  makeActiveBinding(
    sym = var_name,
    fun = local({
      penv <- private_env
      vname <- var_name
      function(new_value) {
        if (missing(new_value)) {
          penv$value
        } else {
          if (penv$readonly) {
            stop(
              glue::glue(
                "Cannot reassign readonly variable '{vname}'. ",
                "Remove Readonly() from the type declaration if mutation is needed."
              ),
              call. = FALSE
            )
          }
          penv$value <- enforce_type(new_value, penv$type, vname)
        }
      }
    }),
    env = envir
  )

  invisible(value)
}

# =============================================================================
# Typed Function
# =============================================================================

#' Create a type-checked function
#'
#' @description
#' Wraps a function with runtime type checking for its parameters and,
#' optionally, its return value. This is the function counterpart to the
#' typed variable operators (`\%:\%` / `\%<-\%`), providing a syntax analogous
#' to typed function signatures:
#'
#' \preformatted{
#'   add <- typed_function(
#'     function(x, y) x + y,
#'     params  = list(x = Numeric, y = Numeric),
#'     .return = Numeric
#'   )
#' }
#'
#' @param fn The function to wrap. Its formals are preserved in the wrapper so
#'   callers use the exact same signature.
#' @param params A named list mapping parameter names to their types
#'   (e.g. \code{list(x = Numeric, y = String)}). Only listed parameters are
#'   type-checked on each call; unlisted parameters pass through unchecked.
#'   Defaults to an empty list (no parameter checking). Default values of
#'   listed parameters are checked once, when the typed function is created,
#'   if they are constants (such as \code{1L}, \code{"a"}, \code{c(1, 2)} or
#'   \code{NULL}). Defaults computed from other arguments are not checked.
#' @param .return Optional return type. When \code{NULL} (the default), the
#'   return value is not checked. Accepts any \code{sicher_type} or
#'   \code{sicher_union}.
#'
#' @return A function with the same formals as \code{fn} and S3 class
#'   \code{"sicher_typed_function"} that:
#' \itemize{
#'   \item Validates each listed parameter on every call.
#'   \item Validates the return value when \code{.return} is specified.
#'   \item Delegates all argument passing to \code{fn} unchanged.
#' }
#'
#' @examples
#' # Basic typed function
#' add <- typed_function(
#'   function(x, y) x + y,
#'   params  = list(x = Numeric, y = Numeric),
#'   .return = Numeric
#' )
#' add(1, 2)     # Returns 3
#' try(add("a", 2))   # Error: Type error in 'x': Expected numeric, got string
#'
#' # Optional parameter
#' greet <- typed_function(
#'   function(name, title = NULL) {
#'     if (is.null(title)) paste("Hello,", name)
#'     else paste("Hello,", title, name)
#'   },
#'   params = list(name = String, title = Optional(String))
#' )
#' greet("Alice")                   # "Hello, Alice"
#' greet("Alice", title = "Dr.")    # "Hello, Dr. Alice"
#' try(greet("Alice", title = 42))  # Error: Type error in 'title'
#'
#' # Constant defaults must match their type
#' try(typed_function(function(n = "a") n, params = list(n = Integer)))
#'
#' # Union type in params
#' describe <- typed_function(
#'   function(id) paste("ID:", id),
#'   params  = list(id = String | Numeric),
#'   .return = String
#' )
#' describe("abc")  # "ID: abc"
#' describe(123)    # "ID: 123"
#' try(describe(TRUE)) # Error: Type error in 'id'
#'
#' @export
typed_function <- function(fn, params = list(), .return = NULL) {
  # Validate fn
  if (!is.function(fn)) {
    stop(glue::glue(
      "`fn` must be a function; got {class(fn)[1]}"
    ), call. = FALSE)
  }

  # Validate params
  if (!is.list(params)) {
    stop(glue::glue(
      "`params` must be a named list of types ",
      "(e.g. list(x = Numeric, y = String)); got {class(params)[1]}"
    ), call. = FALSE)
  }
  if (length(params) > 0) {
    if (is.null(names(params)) || any(nchar(names(params)) == 0)) {
      stop(
        "`params` must be a fully named list; every element must have a parameter name.",
        call. = FALSE
      )
    }
    for (nm in names(params)) {
      tp <- params[[nm]]
      if (!inherits(tp, "sicher_type") &&
          !inherits(tp, "sicher_union") &&
          !inherits(tp, "sicher_readonly")) {
        stop(glue::glue(
          "`params${nm}` must be a sicher_type, sicher_union, or sicher_readonly ",
          "(e.g. Numeric, String | Bool, Optional(Integer)); got {class(tp)[1]}"
        ), call. = FALSE)
      }
    }
  }

  # Validate .return
  if (!is.null(.return)) {
    if (!inherits(.return, "sicher_type") &&
        !inherits(.return, "sicher_union")) {
      stop(glue::glue(
        "`.return` must be a sicher_type or sicher_union ",
        "(e.g. Numeric, String | Bool); got {class(.return)[1]}"
      ), call. = FALSE)
    }
  }

  check_param_defaults(fn, params)

  # Rename to avoid masking inside the closure
  .params <- params
  .ret    <- .return

  # Build a wrapper that validates types before delegating to fn
  wrapper <- function() {
    mc     <- match.call()
    caller <- parent.frame()

    # sicher.mode = "off": call fn directly, keeping lazy evaluation
    if (identical(sicher_mode(), "off")) {
      mc[[1]] <- fn
      return(eval(mc, caller))
    }

    # Evaluate all explicitly supplied arguments in the calling environment
    supplied <- as.list(mc[-1])
    evaled   <- lapply(supplied, eval, envir = caller)

    # Type-check each annotated parameter (using the parsed value, so
    # Coerce(), Default() and FunctionOf() apply to arguments too)
    for (nm in names(.params)) {
      if (nm %in% names(evaled)) {
        evaled[nm] <- list(enforce_type(evaled[[nm]], .params[[nm]], nm))
      }
    }

    # Delegate to the original function
    result <- do.call(fn, evaled)

    # Optionally validate the return value
    if (!is.null(.ret)) {
      result <- enforce_type(result, .ret, "<return value>")
    }

    result
  }

  # Preserve fn's formals so the wrapper has the same signature
  formals(wrapper) <- formals(fn)

  # Attach metadata and tag with the sicher_typed_function class
  attr(wrapper, "params")      <- .params
  attr(wrapper, "return_type") <- .ret
  class(wrapper) <- c("sicher_typed_function", "function")

  wrapper
}

# Check the constant defaults of the annotated parameters of `fn`. Other
# defaults may depend on other arguments or have side effects, so they are
# not evaluated here.
check_param_defaults <- function(fn, params) {
  defaults <- formals(fn)
  for (nm in intersect(names(params), names(defaults))) {
    if (identical(defaults[[nm]], quote(expr = ))) next
    expr <- defaults[[nm]]
    if (!is_constant_expr(expr)) next
    value <- tryCatch(list(eval(expr, baseenv())), error = function(e) NULL)
    if (is.null(value)) next
    enforce_type(value[[1]], params[[nm]], glue::glue("{nm} (default)"))
  }
  invisible(NULL)
}

constant_calls <- c(
  "c", "list", "-", "+", "*", "/", ":", "(", "character", "numeric",
  "integer", "double", "logical", "as.integer", "as.numeric", "as.double",
  "as.character", "as.logical", "as.Date"
)

is_constant_expr <- function(expr) {
  if (is.null(expr) || is.atomic(expr)) return(TRUE)
  if (is.symbol(expr)) return(as.character(expr) %in% c("Inf", "NaN"))
  if (!is.call(expr) || !is.symbol(expr[[1]])) return(FALSE)
  as.character(expr[[1]]) %in% constant_calls &&
    all(vapply(as.list(expr)[-1], is_constant_expr, logical(1)))
}

# =============================================================================
# Assignment Operators
# =============================================================================

#' Type annotation operator
#'
#' @description
#' Creates a typed variable annotation used together with `\%<-\%`.
#'
#' @param name Variable name (unevaluated).
#' @param type Type specification (e.g., Integer, String, Double).
#'
#' @return A typed annotation object.
#'
#' @examples
#' x %:% Integer %<-% 5L
#' name %:% String %<-% "Alice"
#' id %:% (Integer | String) %<-% 42L
#'
#' @export
`%:%` <- function(name, type) {
  structure(
    list(var_name = deparse(substitute(name)), type = type),
    class = "sicher_typed_annotation"
  )
}

#' Type-checked assignment operator
#'
#' @description
#' Completes the typed assignment started with `\%:\%`.
#'
#' @param typed_annotation Result of `\%:\%`.
#' @param value Value to assign.
#'
#' @return Invisibly returns the assigned value.
#'
#' @examples
#' x %:% Integer %<-% 5L
#' y %:% Double %<-% 3.14
#' name %:% String %<-% "Bob"
#' flag %:% Bool %<-% TRUE
#'
#' @export
`%<-%` <- function(typed_annotation, value) {
  if (!inherits(typed_annotation, "sicher_typed_annotation")) {
    stop(
      "Use `%<-%` with `%:%`: `x %:% Type %<-% value`. ",
      "Stand-alone use of `%<-%` is not supported.",
      call. = FALSE
    )
  }

  # Global switch
  if (identical(sicher_mode(), "off")) {
    assign(
      typed_annotation$var_name,
      value,
      envir = parent.frame()
    )
    return(invisible(value))
  }

  create_typed_binding(
    typed_annotation$var_name,
    value,
    typed_annotation$type,
    parent.frame()
  )
}

# =============================================================================
# Print Methods
# =============================================================================

#' Print method for sicher_type
#'
#' @param x A sicher_type object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_type <- function(x, ...) {
  cat("<type:", x$name, ">\n")
  info <- type_description(x)
  if (!is.null(info$title)) cat("  title:", info$title, "\n")
  if (!is.null(info$description)) cat("  description:", info$description, "\n")
  alias <- field_alias(x)
  if (!is.null(alias)) cat("  alias:", alias, "\n")
  invisible(x)
}

#' Print method for sicher_union
#'
#' @param x A sicher_union object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_union <- function(x, ...) {
  cat("<type:", x$name, ">\n")
  invisible(x)
}

#' Print method for sicher_readonly
#'
#' @param x A sicher_readonly object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_readonly <- function(x, ...) {
  cat("<type: readonly<", type_label(x$base_type), "> >\n", sep = "")
  invisible(x)
}

#' Print method for sicher_typed_annotation
#'
#' @param x A sicher_typed_annotation object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_typed_annotation <- function(x, ...) {
  type_name <- if (inherits(x$type, "sicher_readonly")) {
    paste0("readonly<", type_label(x$type$base_type), ">")
  } else {
    x$type$name
  }
  cat("<typed:", x$var_name, "%:%", type_name, ">\n")
  invisible(x)
}

#' Print method for sicher_typed_function
#'
#' @param x A sicher_typed_function object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_typed_function <- function(x, ...) {
  p <- attr(x, "params")
  param_str <- if (length(p) == 0L) {
    ""
  } else {
    paste(
      mapply(
        function(nm, tp) paste0(nm, ": ", tp$name),
        names(p), p,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      ),
      collapse = ", "
    )
  }
  ret <- attr(x, "return_type")
  ret_str <- if (is.null(ret)) "" else paste0(": ", ret$name)
  cat(glue::glue("<typed_function ({param_str}){ret_str}>\n"))
  invisible(x)
}
