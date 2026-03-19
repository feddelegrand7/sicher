infer_atomic_type <- function(value, base_type, strict) {
  if (!strict) {
    return(base_type)
  }

  if (length(value) == 1) {
    return(Scalar(base_type))
  }

  base_type[length(value)]
}

infer_list_element_type <- function(value, strict) {
  if (is.null(value)) {
    return(Optional(Any))
  }

  infer_type(value, strict = strict)
}

is_named_list <- function(value) {
  names_vec <- names(value)

  if (is.null(names_vec)) {
    return(FALSE)
  }

  all(nzchar(names_vec))
}

has_uniform_types <- function(types) {
  first_type <- types[[1]]

  all(vapply(
    types,
    function(type) {
      inherits(type, class(first_type)) && identical(type$name, first_type$name)
    },
    logical(1)
  ))
}

#' Infer a Type from an R Object
#'
#' @description
#' Infers the most appropriate sicher type constructor for a given R object.
#' By default, inference focuses on the underlying type and does not lock in the
#' observed length of vectors. Set `strict = TRUE` to also infer scalar and
#' fixed-size vector constraints from the example value.
#'
#' @param obj Any R object (primitive, vector, list, data.frame, function, etc.)
#' @param strict Logical scalar. When `FALSE` (default), infer only the base
#'   type shape, such as `Numeric`, `String`, `ListOf(Integer)`, or a
#'   `create_dataframe_type()` schema without fixed lengths. When `TRUE`, also
#'   infer `Scalar()` and `[n]` size constraints from the observed object.
#'
#' @return A sicher_type object (e.g., Numeric, String, create_list_type(...), ListOf(...), etc.)
#'
#' @examples
#' infer_type(42L)                # Integer
#' infer_type(3.14)               # Double
#' infer_type(c(1, 2, 3))         # Double or Numeric, no length constraint
#' infer_type("abc")              # String
#' infer_type(c("a", "b"))        # String
#' infer_type(TRUE)               # Bool
#' infer_type(NULL)               # Null
#' infer_type(function(x) x + 1)  # Function
#' infer_type(list(a = 1, b = "x"))   # create_list_type(list(a = Double, b = String))
#' infer_type(list(1, 2, 3))           # ListOf(Double)
#' infer_type(data.frame(x = 1:3))     # create_dataframe_type(list(x = Integer))
#' infer_type(list(a = NULL, b = 1))   # create_list_type(list(a = Optional(Any), b = Double))
#'
#' # Strict mode keeps observed length constraints
#' infer_type(42L, strict = TRUE)          # Scalar(Integer)
#' infer_type(c("a", "b"), strict = TRUE)  # String[2]
#' @export
infer_type <- function(obj, strict = FALSE) {

  if (!is.logical(strict) || length(strict) != 1 || is.na(strict)) {
    stop("`strict` must be either TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(obj)) {
    return(Null)
  }

  if (is.function(obj)) {
    return(Function)
  }

  if (is.data.frame(obj)) {
    if (ncol(obj) == 0) {
      return(DataFrame)
    }

    col_types <- lapply(obj, infer_type, strict = strict)

    return(create_dataframe_type(col_types))
  }

  if (is.list(obj)) {
    if (is_named_list(obj)) {
      field_types <- lapply(obj, function(value) {
        infer_list_element_type(value, strict = strict)
      })

      return(create_list_type(field_types))
    }

    if (length(obj) == 0) {
      return(List)
    }

    elem_types <- lapply(obj, function(value) {
      infer_list_element_type(value, strict = strict)
    })

    if (has_uniform_types(elem_types)) {
      return(ListOf(elem_types[[1]]))
    }

    return(List)
  }

  if (is.integer(obj)) {
    return(infer_atomic_type(obj, Integer, strict = strict))
  }

  if (is.double(obj)) {
    return(infer_atomic_type(obj, Double, strict = strict))
  }

  if (is.numeric(obj)) {
    return(infer_atomic_type(obj, Numeric, strict = strict))
  }

  if (is.character(obj)) {
    return(infer_atomic_type(obj, String, strict = strict))
  }

  if (is.logical(obj)) {
    return(infer_atomic_type(obj, Bool, strict = strict))
  }

  Any
}
