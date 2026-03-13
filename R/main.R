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
#' \dontrun{
#' age %:% Positive %<-% 25
#' age <- -5  # Error: Type error
#' }
#'
#' # Create a custom email type
#' Email <- create_type("email", function(x) {
#'   is.character(x) &&
#'     length(x) == 1 &&
#'     grepl("^[^@]+@[^@]+\\.[^@]+$", x)
#' })
#'
#' \dontrun{
#' user_email %:% Email %<-% "user@example.com"
#' }
#'
#' # Create a type for even integers
#' EvenInt <- create_type("even_int", function(x) {
#'   is.integer(x) && all(x %% 2 == 0)
#' })
#'
#' \dontrun{
#' value %:% EvenInt %<-% 4L
#' value <- 5L  # Error: Type error
#' }
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
    stop("name must be a single character string", call. = FALSE)
  }
  if (!is.function(checker)) {
    stop("checker must be a function", call. = FALSE)
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
Double  <- create_type("double", is.double)

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
String  <- create_type("string", is.character)

#' Built-in Boolean Type
#'
#' @description
#' A type that checks for logical vectors.
#'
#' @export
Bool    <- create_type("bool", is.logical)

#' Built-in List Type
#'
#' @description
#' A type that checks for list objects.
#'
#' @export
List    <- create_type("list", is.list)

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
#' \dontrun{
#' records %:% Records %<-% list(
#'   list(id = 1, name = "a"),
#'   list(id = 2, name = "b")
#' )
#' }
#'
#' # fixed-size list of ten records
#' TenRecs <- Records[10]
#' # will throw if length != 10
#'
#' @export
ListOf <- function(element_type) {
  if (!inherits(element_type, "sicher_type") &&
      !inherits(element_type, "sicher_union")) {
    stop("ListOf() requires a sicher_type or sicher_union", call. = FALSE)
  }
  create_type(
    sprintf("list<%s>", element_type$name),
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
Any       <- create_type("any", function(x) TRUE)

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
#' \dontrun{
#' vec %:% Numeric[3] %<-% c(1, 2, 3)
#' # vec <- c(1, 2)  # Error: wrong length
#' # vec <- c("a", "b", "c")  # Error: wrong type
#' }
#'
#' @export
`[.sicher_type` <- function(type, size) {
  if (!is.numeric(size) || length(size) != 1 || size < 0 || size != as.integer(size)) {
    stop("Size must be a non-negative integer", call. = FALSE)
  }
  create_type(
    sprintf("%s[%d]", type$name, size),
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
#' \dontrun{
#' user %:% User %<-% list(
#'   name = "Alice",
#'   age = 25,
#'   preferences = list(color = "red", movie = "batman")
#' )
#' }
#'
#' @export
create_list_type <- function(type_spec) {
  if (!is.list(type_spec) || is.null(names(type_spec))) {
    stop("type_spec must be a named list", call. = FALSE)
  }
  if (any(names(type_spec) == "")) {
    stop("All elements in type_spec must be named", call. = FALSE)
  }

  # Validate that all values are sicher_type objects
  for (i in seq_along(type_spec)) {
    if (!inherits(type_spec[[i]], "sicher_type") && !inherits(type_spec[[i]], "sicher_union")) {
      stop(sprintf("Element '%s' must be a sicher_type or sicher_union", names(type_spec)[i]), call. = FALSE)
    }
  }

  field_names <- names(type_spec)

  # Separate required and optional fields
  is_optional <- sapply(type_spec, function(t) {
    if (inherits(t, "sicher_union")) {
      # Check if Null is one of the types in the union
      any(sapply(t$types, function(ut) identical(ut, Null)))
    } else {
      FALSE
    }
  })

  required_fields <- field_names[!is_optional]
  optional_fields <- field_names[is_optional]

  type_name <- sprintf("{%s}", paste(sprintf("%s%s: %s",
                                             field_names,
                                             ifelse(is_optional, "?", ""),
                                             sapply(type_spec, function(t) if (inherits(t, "sicher_type")) t$name else t$name)),
                                     collapse = ", "))

  create_type(
    type_name,
    function(x) {
      if (!is.list(x)) return(FALSE)

      # Check that all required fields are present
      if (!all(required_fields %in% names(x))) {
        missing_fields <- setdiff(required_fields, names(x))
        stop(type_error(context, type_name, "list", x,
                        sprintf("Missing required fields: %s", paste(missing_fields, collapse = ", "))), call. = FALSE)
      }

      # Check that each present field has the correct type
      present_fields <- names(x)
      for (field in present_fields) {
        if (field %in% field_names) {
          if (!check_type(x[[field]], type_spec[[field]], context = field)) {
            return(FALSE)
          }
        } else {
          # Extra fields are not allowed
          stop(type_error(context, type_name, "list", x,
                          sprintf("Unexpected field: %s", field)), call. = FALSE)
        }
      }

      TRUE
    }
  )
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
#' \dontrun{
#' df %:% PersonDF %<-% data.frame(
#'   name = c("Alice", "Bob"),
#'   age = c(25, 30)
#' )
#' }
#'
#' @export
create_dataframe_type <- function(col_spec) {
  if (!is.list(col_spec) || is.null(names(col_spec))) {
    stop("col_spec must be a named list", call. = FALSE)
  }
  if (any(names(col_spec) == "")) {
    stop("All columns must be named", call. = FALSE)
  }

  # Validate each column type specification
  for (i in seq_along(col_spec)) {
    if (!inherits(col_spec[[i]], "sicher_type") && !inherits(col_spec[[i]], "sicher_union")) {
      stop(sprintf("Column '%s' must be a sicher_type or sicher_union", names(col_spec)[i]), call. = FALSE)
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

  type_name <- sprintf("data.frame{%s}", paste(sprintf("%s%s: %s",
                                                       ifelse(is_optional, col_names, ""),
                                                       ifelse(is_optional, "?", ""),
                                                       sapply(col_spec, function(t) if (inherits(t, "sicher_type")) t$name else t$name)),
                                               collapse = ", "))

  create_type(
    type_name,
    function(x) {
      if (!is.data.frame(x)) return(FALSE)

      # check required columns exist
      if (!all(required_cols %in% names(x))) {
        return(FALSE)
      }

      # no extra columns allowed
      if (!all(names(x) %in% col_names)) {
        return(FALSE)
      }

      # check each column type
      for (col in col_names) {
        if (col %in% names(x)) {
          if (!check_type(x[[col]], col_spec[[col]], context = col)) {
            return(FALSE)
          }
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
#' \dontrun{
#' age %:% Scalar(Integer) %<-% 30L
#' # age <- c(30L, 40L)  # Error: not scalar
#' }
#'
#' @export
Scalar <- function(type) {
  if (!inherits(type, "sicher_type")) {
    stop("Scalar() requires a type argument", call. = FALSE)
  }
  create_type(
    sprintf("scalar<%s>", type$name),
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
#' \dontrun{
#' PI %:% Readonly(Double) %<-% 3.14159
#' # PI <- 3.0  # Error: cannot reassign readonly
#' }
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
#' \dontrun{
#' middle_name %:% Optional(String) %<-% NULL
#' middle_name <- "Marie"  # Also OK
#' }
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
  if (is.integer(value)) return("integer")
  if (is.double(value)) return("double")
  if (is.character(value)) return("string")
  if (is.logical(value)) return("bool")
  if (is.list(value)) return("list")
  class(value)[1]
}

type_error <- function(context, expected, got, value = NULL, details = NULL) {
  base_msg <- if (is.null(context)) {
    sprintf("Type error: Expected %s, got %s", expected, got)
  } else {
    sprintf("Type error in '%s': Expected %s, got %s", context, expected, got)
  }

  if (!is.null(details)) {
    base_msg <- sprintf("%s\n  Details: %s", base_msg, details)
  }

  if (!is.null(value)) {
    value_str <- tryCatch({
      if (length(value) == 0) {
        "(empty)"
      } else if (length(value) > 3) {
        sprintf(
          "[%s, ...] (length: %d)",
          paste(utils::head(value, 3), collapse = ", "),
          length(value)
        )
      } else {
        sprintf("[%s]", paste(value, collapse = ", "))
      }
    }, error = function(e) "<unprintable>")

    base_msg <- sprintf("%s\n  Received: %s", base_msg, value_str)
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
#' \dontrun{
#' # Direct type checking
#' check_type(5L, Integer)  # Returns TRUE
#' check_type("hello", Integer)  # Throws error
#'
#' # With context for better error messages
#' check_type(5L, String, context = "user_name")
#'
#' # With union types
#' check_type(5L, Integer | String)  # Returns TRUE
#' check_type(5.5, Integer | String)  # Throws error
#' }
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
    stop(type_error(context, type$name, get_type_name(value), value), call. = FALSE)
  }

  if (inherits(type, "sicher_union")) {
    for (t in type$types) {
      if (inherits(t, "sicher_type") && t$check(value)) return(TRUE)
    }
    stop(type_error(context, type$name, get_type_name(value), value), call. = FALSE)
  }

  stop("Invalid type specification", call. = FALSE)
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
            stop(sprintf("Cannot reassign readonly variable '%s'", vname), call. = FALSE)
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
#' \dontrun{
#' x %:% Integer %<-% 5L
#' name %:% String %<-% "Alice"
#' id %:% (Integer | String) %<-% 42L
#' }
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
#' \dontrun{
#' x %:% Integer %<-% 5L
#' y %:% Double %<-% 3.14
#' name %:% String %<-% "Bob"
#' flag %:% Bool %<-% TRUE
#' }
#'
#' @export
`%<-%` <- function(typed_annotation, value) {
  if (!inherits(typed_annotation, "sicher_typed_annotation")) {
    stop("Use with %:% operator: x %:% type %<-% value", call. = FALSE)
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
