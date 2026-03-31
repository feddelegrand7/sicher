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
Integer <- create_type("integer", is.integer)

#' Built-in Double Type
#'
#' @description
#' A type that checks for double-precision numeric vectors.
#'
#' @export
Double <- create_type("double", is.double)

#' Built-in Numeric Type
#'
#' @description
#' A type that checks for numeric vectors (integer or double).
#'
#' @export
Numeric <- create_type("numeric", is.numeric)

#' Built-in String Type
#'
#' @description
#' A type that checks for character vectors.
#'
#' @export
String <- create_type("string", is.character)

#' Built-in Boolean Type
#'
#' @description
#' A type that checks for logical vectors.
#'
#' @export
Bool <- create_type("bool", is.logical)

#' Built-in List Type
#'
#' @description
#' A type that checks for list objects.
#'
#' @export
List <- create_type("list", is.list)

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
  create_type(
    glue::glue("list<{element_type$name}>"),
    function(x) {
      if (!is.list(x)) return(FALSE)
      # empty list is OK
      if (length(x) == 0) return(TRUE)
      all(vapply(x, function(el) {
        tryCatch(check_type(el, element_type), error = function(e) FALSE)
      }, logical(1)))
    }
  )
}

#' Built-in Any Type
#'
#' @description
#' A type that accepts any value.
#'
#' @export
Any <- create_type("any", function(x) TRUE)

#' Built-in Null Type
#'
#' @description
#' A type that checks for NULL values.
#'
#' @export
Null <- create_type("null", is.null)

#' Built-in Function Type
#'
#' @description
#' A type that checks for function objects.
#'
#' @export
Function <- create_type("Function", is.function)

#' Built-in DataFrame Type
#'
#' @description
#' A type that checks for data.frame objects.
#'
#' @export
DataFrame <- create_type("data.frame", is.data.frame)

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
  create_type(
    glue::glue("{type$name}[{as.integer(size)}]"),
    function(x) type$check(x) && length(x) == size
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
#' @export
create_list_type <- function(type_spec) {

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
    if (!inherits(type_spec[[i]], "sicher_type") &&
        !inherits(type_spec[[i]], "sicher_union")) {
      stop(glue::glue(
        "Field '{names(type_spec)[i]}' must be a sicher_type or sicher_union ",
        "(e.g. String, Numeric, Optional(Bool)); ",
        "got {class(type_spec[[i]])[1]}"
      ), call. = FALSE)
    }
  }

  field_names  <- names(type_spec)
  is_optional  <- sapply(type_spec, function(t) {
    if (inherits(t, "sicher_union")) {
      any(sapply(t$types, function(ut) identical(ut, Null)))
    } else {
      FALSE
    }
  })
  required_fields <- field_names[!is_optional]
  type_labels     <- sapply(type_spec, function(t) t$name)
  field_parts     <- glue::glue(
    "{field_names}{ifelse(is_optional, '?', '')}: {type_labels}"
  )
  type_name <- glue::glue("{{{paste(field_parts, collapse = ', ')}}}")

  result <- create_type(
    type_name,
    function(x) {
      if (!is.list(x)) return(FALSE)

      if (!all(required_fields %in% names(x))) {
        missing_fields <- setdiff(required_fields, names(x))
        details <- glue::glue(
          "Missing required field(s): {paste(missing_fields, collapse = ', ')} ",
          "(expected fields: {paste(required_fields, collapse = ', ')})"
        )
        stop(type_error(NULL, type_name, "list", x, details), call. = FALSE)
      }

      present_fields <- names(x)
      for (field in present_fields) {
        if (field %in% field_names) {
          if (!check_type(x[[field]], type_spec[[field]], context = field)) {
            return(FALSE)
          }
        } else {
          details <- glue::glue(
            "Unexpected field: '{field}' (valid fields: {paste(field_names, collapse = ', ')})"
          )
          stop(type_error(NULL, type_name, "list", x, details), call. = FALSE)
        }
      }
      TRUE
    }
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

  base_spec <- attr(base, "sicher_spec")

  if (is.null(base_spec)) {
    stop(
      "`base` does not carry a field specification. ",
      "Only types created with create_list_type() can be extended. ",
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

  create_list_type(merged_spec)
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
create_dataframe_type <- function(col_spec) {
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
    if (!inherits(col_spec[[i]], "sicher_type") &&
        !inherits(col_spec[[i]], "sicher_union")) {
      stop(glue::glue(
        "Column '{names(col_spec)[i]}' must be a sicher_type or sicher_union ",
        "(e.g. String, Numeric, Optional(Bool)); ",
        "got {class(col_spec[[i]])[1]}"
      ), call. = FALSE)
    }
  }

  col_names <- names(col_spec)

  is_optional <- sapply(col_spec, function(t) {
    if (inherits(t, "sicher_union")) {
      any(sapply(t$types, function(ut) identical(ut, Null)))
    } else {
      FALSE
    }
  })

  required_cols <- col_names[!is_optional]
  optional_cols <- col_names[is_optional]

  type_labels <- sapply(col_spec, function(t) t$name)
  col_parts <- glue::glue("{col_names}{ifelse(is_optional, '?', '')}: {type_labels}")
  type_name <- glue::glue("data.frame{{{paste(col_parts, collapse = ', ')}}}")

  create_type(
    type_name,
    function(x) {
      if (!is.data.frame(x)) return(FALSE)

      # check required columns exist
      if (!all(required_cols %in% names(x))) {
        missing_cols <- setdiff(required_cols, names(x))
        details <- glue::glue(
          "Missing required column(s): {paste(missing_cols, collapse = ', ')} ",
          "(expected columns: {paste(required_cols, collapse = ', ')})"
        )
        stop(type_error(NULL, type_name, get_type_name(x), NULL, details), call. = FALSE)
      }

      # no extra columns allowed
      if (!all(names(x) %in% col_names)) {
        extra_cols <- setdiff(names(x), col_names)
        details <- glue::glue(
          "Unexpected column(s): {paste(extra_cols, collapse = ', ')} ",
          "(valid columns: {paste(col_names, collapse = ', ')})"
        )
        stop(type_error(NULL, type_name, get_type_name(x), NULL, details), call. = FALSE)
      }

      # check each column type
      for (col in col_names) {
        if (col %in% names(x)) {
          check_type(x[[col]], col_spec[[col]], context = col)
        }
      }

      TRUE
    }
  )
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
  if (!inherits(type, "sicher_type")) {
    stop(glue::glue(
      "Scalar() requires a type argument (sicher_type, e.g. Scalar(Numeric) or Scalar(String)); ",
      "got {class(type)[1]}"
    ), call. = FALSE)
  }
  create_type(
    glue::glue("scalar<{type$name}>"),
    function(x) length(x) == 1 && type$check(x)
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

# =============================================================================
# Union Types
# =============================================================================

create_union <- function(type1, type2) {
  types_list <- list()
  names_list <- character()

  if (inherits(type1, "sicher_union")) {
    types_list <- type1$types
    names_list <- strsplit(type1$name, " \\| ")[[1]]
  } else if (inherits(type1, "sicher_type")) {
    types_list <- list(type1)
    names_list <- type1$name
  } else if (is.null(type1)) {
    types_list <- list(Null)
    names_list <- "null"
  }

  if (inherits(type2, "sicher_union")) {
    types_list <- c(types_list, type2$types)
    names_list <- c(names_list, strsplit(type2$name, " \\| ")[[1]])
  } else if (inherits(type2, "sicher_type")) {
    types_list <- c(types_list, list(type2))
    names_list <- c(names_list, type2$name)
  } else if (is.null(type2)) {
    types_list <- c(types_list, list(Null))
    names_list <- c(names_list, "null")
  }

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

type_error <- function(context = NULL, expected, got,
                       value = NULL, details = NULL) {
  base_msg <- if (is.null(context)) {
    glue::glue("Type error: Expected {expected}, got {got}")
  } else {
    glue::glue("Type error in '{context}': Expected {expected}, got {got}")
  }

  if (!is.null(details)) {
    base_msg <- glue::glue("{base_msg}\n  Details: {details}")
  }

  if (!is.null(value)) {
    value_str <- tryCatch({
      if (length(value) == 0) {
        "(empty)"
      } else if (is.list(value) && !is.data.frame(value)) {
        fields <- names(value)
        if (!is.null(fields) && length(fields) > 0) {
          preview <- paste(utils::head(fields, 6), collapse = ", ")
          if (length(fields) > 6) preview <- glue::glue("{preview}, ...")
          glue::glue("list with fields: [{preview}]")
        } else {
          glue::glue("list of length {length(value)}")
        }
      } else if (is.data.frame(value)) {
        col_preview <- paste(utils::head(names(value), 6), collapse = ", ")
        if (ncol(value) > 6) col_preview <- glue::glue("{col_preview}, ...")
        glue::glue("data.frame[{nrow(value)} x {ncol(value)}] with columns: [{col_preview}]")
      } else if (length(value) == 1) {
        as.character(value)
      } else if (length(value) > 6) {
        glue::glue(
          "[{paste(utils::head(value, 6), collapse = ', ')}, ...] (length: {length(value)})"
        )
      } else {
        glue::glue("[{paste(value, collapse = ', ')}]")
      }
    }, error = function(e) "<unprintable>")

    base_msg <- glue::glue("{base_msg}\n  Received: {value_str}")
  }

  base_msg
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
#' @return Returns `TRUE` invisibly if the value matches the type, otherwise
#'   throws an error with a descriptive message.
#'
#' @details
#' This function:
#' \itemize{
#'   \item Checks if a value matches a type specification
#'   \item Handles union types (checks if value matches any type in the union)
#'   \item Handles readonly types (strips the readonly modifier before checking)
#'   \item Provides detailed error messages when checks fail
#' }
#'
#' @seealso
#' \code{\link{create_type}} for creating custom types
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
  if (inherits(type, "sicher_readonly")) {
    type <- type$base_type
  }

  if (is.null(value)) {
    if (inherits(type, "sicher_union")) {
      has_null <- any(sapply(type$types, function(t) {
        inherits(t, "sicher_type") && identical(t, Null)
      }))
      if (!has_null) {
        stop(type_error(context, type$name, "null", value), call. = FALSE)
      }
      return(TRUE)
    }

    if (inherits(type, "sicher_type")) {
      if (identical(type, Null) || identical(type, Any)) {
        return(TRUE)
      }
    }

    stop(type_error(context, type$name, "null", value), call. = FALSE)
  }

  if (inherits(type, "sicher_type")) {
    if (type$check(value)) return(TRUE)
    stop(
      type_error(context, type$name, get_type_name(value), value),
      call. = FALSE
    )
  }

  if (inherits(type, "sicher_union")) {
    for (t in type$types) {
      if (inherits(t, "sicher_type") && t$check(value)) return(TRUE)
    }
    stop(
      type_error(context, type$name, get_type_name(value), value),
      call. = FALSE
    )
  }

  stop(glue::glue(
    "Invalid type specification: expected a sicher_type, sicher_union, or sicher_readonly ",
    "(e.g. Numeric, String | Bool, Optional(Integer)); ",
    "got {class(type)[1]}"
  ), call. = FALSE)
}

# =============================================================================
# Typed Variable Binding
# =============================================================================

create_typed_binding <- function(var_name, value, type, envir) {
  is_readonly <- inherits(type, "sicher_readonly")
  actual_type <- if (is_readonly) type$base_type else type

  check_type(value, actual_type, context = var_name)

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
          check_type(new_value, penv$type, context = vname)
          penv$value <- new_value
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
#'   Defaults to an empty list (no parameter checking).
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

  # Rename to avoid masking inside the closure
  .params <- params
  .ret    <- .return

  # Build a wrapper that validates types before delegating to fn
  wrapper <- function() {
    mc     <- match.call()
    caller <- parent.frame()

    # Evaluate all explicitly supplied arguments in the calling environment
    supplied <- as.list(mc[-1])
    evaled   <- lapply(supplied, eval, envir = caller)

    # Type-check each annotated parameter
    for (nm in names(.params)) {
      if (nm %in% names(evaled)) {
        check_type(evaled[[nm]], .params[[nm]], context = nm)
      }
    }

    # Delegate to the original function
    result <- do.call(fn, evaled)

    # Optionally validate the return value
    if (!is.null(.ret)) {
      check_type(result, .ret, context = "<return value>")
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
  if (identical(getOption("sicher.mode"), "off")) {
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

#' Print method for sicher_typed_annotation
#'
#' @param x A sicher_typed_annotation object
#' @param ... Additional arguments (ignored)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.sicher_typed_annotation <- function(x, ...) {
  cat("<typed:", x$var_name, "%:%", x$type$name, ">\n")
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
