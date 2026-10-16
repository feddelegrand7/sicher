# =============================================================================
# Intersections and object type internals
# =============================================================================

object_spec <- function(type) {
  if (inherits(type, "sicher_readonly")) type <- type$base_type
  if (!inherits(type, "sicher_type")) return(NULL)
  meta <- type_meta(type)
  if (!is.null(meta) && meta$kind %in% c("list", "data.frame")) {
    return(meta$spec)
  }
  attr(type, "sicher_spec", exact = TRUE)
}

object_kind <- function(type) {
  if (inherits(type, "sicher_readonly")) type <- type$base_type
  kind <- meta_kind(type)
  if (identical(kind, "data.frame")) "data.frame" else "list"
}

object_extra <- function(type) {
  if (inherits(type, "sicher_readonly")) type <- type$base_type
  extra <- type_meta(type)$extra
  if (is.null(extra)) "error" else extra
}

# Build a new list / data frame type with the same kind and `extra` setting
# as `base`, but a different field specification.
rebuild_object_type <- function(base, spec, extra = object_extra(base)) {
  if (identical(object_kind(base), "data.frame")) {
    create_dataframe_type(spec, extra = extra)
  } else {
    create_list_type(spec, extra = extra)
  }
}

require_object_type <- function(type, fn_name) {
  spec <- object_spec(type)
  if (is.null(spec)) {
    stop(glue::glue(
      "{fn_name}() requires a type created with create_list_type() or ",
      "create_dataframe_type(); got {class(type)[1]}"
    ), call. = FALSE)
  }
  spec
}

create_intersection <- function(type1, type2) {
  if (!is_sicher_type_like(type1) || !is_sicher_type_like(type2)) {
    stop(
      "Both sides of `&` must be sicher types (e.g. Numeric & Between(0, 1))",
      call. = FALSE
    )
  }

  kind1 <- meta_kind(if (inherits(type1, "sicher_type")) type1 else NULL)
  kind2 <- meta_kind(if (inherits(type2, "sicher_type")) type2 else NULL)

  # Object & object: merge the fields, like TypeScript does.
  if (!is.na(kind1) && !is.na(kind2) && kind1 == kind2 &&
      kind1 %in% c("list", "data.frame")) {
    spec1 <- object_spec(type1)
    spec2 <- object_spec(type2)
    merged <- spec1
    for (field in names(spec2)) {
      merged[[field]] <- if (field %in% names(spec1)) {
        create_intersection(spec1[[field]], spec2[[field]])
      } else {
        spec2[[field]]
      }
    }
    extras <- c(object_extra(type1), object_extra(type2))
    extra <- if ("error" %in% extras) "error" else if ("drop" %in% extras) "drop" else "allow"
    return(rebuild_object_type(type1, merged, extra = extra))
  }

  wrap <- function(type) {
    if (inherits(type, "sicher_union")) paste0("(", type$name, ")") else type_label(type)
  }
  type_name <- paste(wrap(type1), "&", wrap(type2))

  new_parser_type(
    type_name,
    function(x, path) {
      first <- run_type(x, type1, path)
      input <- if (length(first$issues) == 0) first$value else x
      second <- run_type(input, type2, path)
      issues <- c(first$issues, second$issues)
      if (length(issues) == 0) return(second)
      plain_first <- length(first$issues) == 0 || is_plain_at(first$issues, path)
      plain_second <- length(second$issues) == 0 || is_plain_at(second$issues, path)
      if (plain_first && plain_second) {
        issues <- list(plain_issue(x, type_name, path))
      }
      list(value = x, issues = issues)
    },
    meta = list(kind = "intersection", types = list(type1, type2))
  )
}

# =============================================================================
# Numeric rules
# =============================================================================

#' Built-in Positive Type
#'
#' @description
#' A numeric type whose elements are all strictly greater than zero. \code{NA}
#' values are rejected. Similar to zod's \code{z.number().positive()}.
#'
#' @examples
#' price %:% Positive %<-% 9.99
#' try(price <- 0)   # Error: 0 is not > 0
#'
#' @export
Positive <- numeric_rule_type(
  "positive", function(x) x > 0, "not > 0", "not > 0",
  meta = list(kind = "positive", coerce = as.numeric)
)

#' Built-in NonNegative Type
#'
#' @description
#' A numeric type whose elements are all greater than or equal to zero.
#' \code{NA} values are rejected. Similar to zod's
#' \code{z.number().nonnegative()}.
#'
#' @examples
#' count %:% NonNegative %<-% 0
#' try(count <- -1)   # Error: -1 is not >= 0
#'
#' @export
NonNegative <- numeric_rule_type(
  "non_negative", function(x) x >= 0, "not >= 0", "not >= 0",
  meta = list(kind = "non_negative", coerce = as.numeric)
)

#' Built-in Whole Type
#'
#' @description
#' A numeric type whose elements are all finite whole numbers. Unlike
#' \code{Integer}, the storage mode does not matter: both \code{3L} and
#' \code{3} are accepted, but \code{3.5} is not. Similar to zod's
#' \code{z.number().int()}.
#'
#' @examples
#' n %:% Whole %<-% 3     # a double, but a whole number
#' n <- 10L               # integers are fine too
#' try(n <- 2.5)          # Error: not a whole number
#'
#' @export
Whole <- numeric_rule_type(
  "whole",
  function(x) is.finite(x) & x == round(x),
  "not a whole number",
  "not whole numbers",
  meta = list(kind = "whole", coerce = as.numeric)
)

# =============================================================================
# Length and uniqueness modifiers
# =============================================================================

check_count_arg <- function(n, fn_name) {
  ok <- is.numeric(n) && length(n) == 1 && !is.na(n) && n >= 0 &&
    n == as.integer(n)
  if (!ok) {
    stop(glue::glue(
      "`n` in {fn_name}() must be a single non-negative whole number; got {deparse(n)}"
    ), call. = FALSE)
  }
  as.integer(n)
}

#' MinLength / MaxLength modifiers
#'
#' @description
#' Require a minimum or maximum number of elements. The length is
#' \code{length()} for vectors and lists and \code{nrow()} for data frames,
#' consistent with \code{NonEmpty()} and the \code{[n]} size operator. To
#' limit the number of characters in a string, use \code{\link{Matches}()} or
#' \code{\link{Refine}()} with \code{nchar()}.
#'
#' @param type A sicher_type or sicher_union object.
#' @param n A single non-negative whole number.
#'
#' @return A new sicher_type.
#'
#' @examples
#' tags %:% MinLength(String, 1) %<-% "r"
#' try(tags <- character(0))   # Error: fewer than 1 element
#'
#' top3 %:% MaxLength(Numeric, 3) %<-% c(9, 8, 7)
#' try(top3 <- c(9, 8, 7, 6))  # Error: more than 3 elements
#'
#' @name length-modifiers
#' @export
MinLength <- function(type, n) {
  check_type_arg(type, "MinLength")
  n <- check_count_arg(n, "MinLength")
  type_name <- glue::glue("min_length<{type$name}, {n}>")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      size <- value_size(res$value)
      if (size < n) {
        return(issue_result(res$value, new_issue(
          path,
          glue::glue("expected {type_name} (at least {n} element(s)), but got {size}"),
          expected = type_name, got = get_type_name(res$value), code = "too_small"
        )))
      }
      res
    },
    meta = list(kind = "min_length", inner = type, n = n),
    strict_check = TRUE
  )
}

#' @rdname length-modifiers
#' @export
MaxLength <- function(type, n) {
  check_type_arg(type, "MaxLength")
  n <- check_count_arg(n, "MaxLength")
  type_name <- glue::glue("max_length<{type$name}, {n}>")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      size <- value_size(res$value)
      if (size > n) {
        return(issue_result(res$value, new_issue(
          path,
          glue::glue("expected {type_name} (at most {n} element(s)), but got {size}"),
          expected = type_name, got = get_type_name(res$value), code = "too_big"
        )))
      }
      res
    },
    meta = list(kind = "max_length", inner = type, n = n),
    strict_check = TRUE
  )
}

#' Unique modifier
#'
#' @description
#' Rejects values that contain duplicated elements (for data frames,
#' duplicated rows). Uses \code{anyDuplicated()}.
#'
#' @param type A sicher_type or sicher_union object.
#'
#' @return A new sicher_type.
#'
#' @examples
#' ids %:% Unique(Integer) %<-% c(1L, 2L, 3L)
#' try(ids <- c(1L, 2L, 2L))   # Error: duplicated value 2
#'
#' @export
Unique <- function(type) {
  check_type_arg(type, "Unique")
  type_name <- glue::glue("unique<{type$name}>")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      value <- res$value
      if (anyDuplicated(value) > 0) {
        detail <- if (is.atomic(value)) {
          dups <- unique(value[duplicated(value)])
          preview <- paste(utils::head(dups, 5), collapse = ", ")
          if (length(dups) > 5) preview <- paste0(preview, ", ...")
          glue::glue("duplicated value(s): [{preview}]")
        } else if (is.data.frame(value)) {
          glue::glue("{sum(duplicated(value))} duplicated row(s)")
        } else {
          glue::glue("{sum(duplicated(value))} duplicated element(s)")
        }
        return(issue_result(value, new_issue(
          path,
          glue::glue("expected {type_name}, but found {detail}"),
          expected = type_name, got = get_type_name(value), code = "not_unique"
        )))
      }
      res
    },
    meta = list(kind = "unique", inner = type),
    strict_check = TRUE
  )
}

# =============================================================================
# Refinements and transforms
# =============================================================================

#' Refine a type with a custom rule
#'
#' @description
#' Adds an extra rule on top of an existing type, like zod's
#' \code{.refine()}. The base type is checked first; the predicate only runs
#' on values that already have the right type, so it does not need to guard
#' against them.
#'
#' @param type A sicher_type or sicher_union object.
#' @param predicate A function taking the value and returning \code{TRUE} if
#'   it is valid.
#' @param message Error message used when the predicate does not return
#'   \code{TRUE}. Defaults to a generic message.
#' @param name Optional type name used when printing the type and in error
#'   messages. Defaults to \code{refine<base type>}.
#'
#' @return A new sicher_type.
#'
#' @examples
#' Password <- Refine(
#'   Scalar(String),
#'   function(x) nchar(x) >= 8,
#'   message = "must be at least 8 characters"
#' )
#' pw %:% Password %<-% "correct horse"
#' try(pw <- "short")
#'
#' Even <- Refine(Integer, function(x) all(x %% 2L == 0L), "must be even", name = "even")
#' check_type(c(2L, 4L), Even)
#'
#' @export
Refine <- function(type, predicate, message = NULL, name = NULL) {
  check_type_arg(type, "Refine")
  if (!is.function(predicate)) {
    stop(glue::glue(
      "`predicate` must be a function returning TRUE or FALSE; got {class(predicate)[1]}"
    ), call. = FALSE)
  }
  if (!is.null(message) && (!is.character(message) || length(message) != 1)) {
    stop("`message` must be a single character string", call. = FALSE)
  }
  type_name <- if (is.null(name)) glue::glue("refine<{type$name}>") else name
  new_parser_type(
    type_name,
    function(x, path) {
      res <- run_type(x, type, path)
      if (length(res$issues) > 0) return(res)
      outcome <- tryCatch(predicate(res$value), error = function(e) e)
      if (isTRUE(outcome)) return(res)
      msg <- if (inherits(outcome, "error")) {
        glue::glue("refinement check of {type_name} failed with an error: {conditionMessage(outcome)}")
      } else if (!is.null(message)) {
        message
      } else {
        glue::glue("value does not satisfy {type_name}")
      }
      issue_result(res$value, new_issue(
        path, msg, expected = type_name, got = get_type_name(res$value),
        received = format_received(res$value), code = "custom"
      ))
    },
    meta = list(kind = "refine", inner = type),
    strict_check = TRUE
  )
}

#' Default value for a type
#'
#' @description
#' Replaces \code{NULL} with a default value, like zod's \code{.default()}.
#' Inside \code{\link{create_list_type}()} a field with a default may be left
#' out and is filled in with the default value. Any other value is checked
#' against \code{type}.
#'
#' Typed variables, typed function arguments, \code{\link{parse_as}()} and
#' \code{\link{safe_parse}()} all store the parsed value, so the default is
#' what ends up in the variable.
#'
#' @param type A sicher_type or sicher_union object.
#' @param value The default value. It must itself satisfy \code{type}.
#'
#' @return A new sicher_type.
#'
#' @examples
#' Config <- create_list_type(list(
#'   host = String,
#'   port = Default(Numeric, 8080)
#' ))
#' parse_as(list(host = "localhost"), Config)   # port is filled in
#'
#' retries %:% Default(Integer, 3L) %<-% NULL
#' retries   # 3L
#'
#' @export
Default <- function(type, value) {
  check_type_arg(type, "Default")
  check_type(value, type, context = "default value")
  shown <- if (is.atomic(value) && length(value) == 1) {
    format_enum_value(value)
  } else {
    paste(utils::head(deparse(value), 1), collapse = "")
  }
  type_name <- glue::glue("{type$name} = {shown}")
  new_parser_type(
    type_name,
    function(x, path) {
      if (is.null(x)) return(ok_result(value))
      delegate(x, type, path, type_name)
    },
    meta = list(kind = "default", inner = type, value = value)
  )
}

find_coercer <- function(type) {
  if (inherits(type, "sicher_readonly")) type <- type$base_type
  if (!inherits(type, "sicher_type")) return(NULL)
  meta <- type_meta(type)
  if (is.null(meta)) return(NULL)
  if (is.function(meta$coerce)) return(meta$coerce)
  if (!is.null(meta$inner)) return(find_coercer(meta$inner))
  NULL
}

#' Coerce values before checking them
#'
#' @description
#' Converts values to the target type before checking them, like zod's
#' \code{z.coerce}. Values that already satisfy \code{type} are left alone.
#' Conversion uses the matching \code{as.*()} function for built-in types
#' (\code{as.numeric}, \code{as.integer}, \code{as.character},
#' \code{as.logical}, \code{as.Date}, \code{as.POSIXct}, \code{factor}, ...),
#' or the function you pass as \code{using}. A conversion that introduces new
#' \code{NA} values (e.g. \code{as.numeric("abc")}) is reported as an error
#' instead of silently producing \code{NA}.
#'
#' Typed variables, typed function arguments, \code{\link{parse_as}()} and
#' \code{\link{safe_parse}()} store the converted value.
#'
#' @param type A sicher_type. Built-in types and modifiers of built-in types
#'   (e.g. \code{Scalar(Numeric)}, \code{Between(0, 1)}) know their
#'   conversion; other types need \code{using}.
#' @param using Optional conversion function.
#'
#' @return A new sicher_type.
#'
#' @examples
#' age %:% Coerce(Numeric) %<-% "42"
#' age          # 42 (a number)
#' try(age <- "forty-two")   # Error: cannot convert
#'
#' when %:% Coerce(Date) %<-% "2024-05-01"
#' class(when)  # "Date"
#'
#' @export
Coerce <- function(type, using = NULL) {
  check_type_arg(type, "Coerce")
  coercer <- if (is.null(using)) find_coercer(type) else using
  if (!is.function(coercer)) {
    stop(glue::glue(
      "Coerce() does not know how to convert values to {type_label(type)}; ",
      "pass a conversion function, e.g. Coerce(type, using = as.numeric)"
    ), call. = FALSE)
  }
  type_name <- glue::glue("coerce<{type_label(type)}>")
  new_parser_type(
    type_name,
    function(x, path) {
      first <- run_type(x, type, path)
      if (length(first$issues) == 0 || is.null(x)) return(first)

      converted <- tryCatch(
        withCallingHandlers(
          coercer(x),
          warning = function(w) invokeRestart("muffleWarning")
        ),
        error = function(e) e
      )
      if (inherits(converted, "error")) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("cannot convert value to {type_label(type)}: {conditionMessage(converted)}"),
          expected = type_label(type), got = get_type_name(x),
          received = format_received(x), code = "invalid_coercion"
        )))
      }
      if (is.atomic(x) && length(converted) == length(x) &&
          sum(is.na(converted)) > sum(is.na(x))) {
        bad <- x[is.na(converted) & !is.na(x)]
        preview <- paste(encodeString(as.character(utils::head(bad, 3)), quote = "\""),
                         collapse = ", ")
        if (length(bad) > 3) preview <- paste0(preview, ", ...")
        return(issue_result(x, new_issue(
          path,
          glue::glue("cannot convert [{preview}] to {type_label(type)}"),
          expected = type_label(type), got = get_type_name(x),
          code = "invalid_coercion"
        )))
      }
      run_type(converted, type, path)
    },
    meta = list(kind = "coerce", inner = type)
  )
}

# =============================================================================
# R-specific types
# =============================================================================

#' Factor Type Factory
#'
#' @description
#' Creates a type that checks for factors. When \code{levels} is given, the
#' factor's levels must be exactly \code{levels} (same values, same order).
#'
#' @param levels Optional character vector of expected levels.
#'
#' @return A sicher_type.
#'
#' @examples
#' size %:% Factor(c("S", "M", "L")) %<-% factor("M", levels = c("S", "M", "L"))
#' try(size <- factor("XL"))   # Error: wrong levels
#'
#' any_factor %:% Factor() %<-% factor(c("a", "b"))
#'
#' @export
Factor <- function(levels = NULL) {
  if (!is.null(levels)) {
    if (!is.character(levels) || anyNA(levels) || anyDuplicated(levels) > 0) {
      stop("`levels` must be a character vector of unique, non-NA levels", call. = FALSE)
    }
  }
  type_name <- if (is.null(levels)) {
    "factor"
  } else {
    glue::glue("factor<{paste(encodeString(levels, quote = '\"'), collapse = ', ')}>")
  }
  coerce <- if (is.null(levels)) {
    function(x) factor(x)
  } else {
    function(x) factor(x, levels = levels)
  }
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.factor(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if (!is.null(levels) && !identical(levels(x), levels)) {
        return(issue_result(x, new_issue(
          path,
          glue::glue(
            "expected {type_name}, but the factor has levels ",
            "[{paste(levels(x), collapse = ', ')}]"
          ),
          expected = type_name, got = "factor", code = "invalid_levels"
        )))
      }
      ok_result(x)
    },
    meta = list(kind = "factor", levels = levels, coerce = coerce)
  )
}

#' Built-in Date Type
#'
#' @description
#' A type that checks for \code{Date} vectors.
#'
#' @examples
#' start %:% Date %<-% as.Date("2024-01-01")
#' try(start <- "2024-01-01")   # Error: a string is not a Date
#'
#' @export
Date <- with_meta(
  create_type("date", function(x) inherits(x, "Date")),
  kind = "date", coerce = as.Date
)

#' Built-in DateTime Type
#'
#' @description
#' A type that checks for date-time (\code{POSIXct}) vectors.
#'
#' @examples
#' created %:% DateTime %<-% Sys.time()
#'
#' @export
DateTime <- with_meta(
  create_type("datetime", function(x) inherits(x, "POSIXct")),
  kind = "datetime", coerce = as.POSIXct
)

#' Built-in Environment Type
#'
#' @description
#' A type that checks for environments.
#'
#' @examples
#' cache %:% Environment %<-% new.env()
#'
#' @export
Environment <- with_meta(
  create_type("environment", is.environment),
  kind = "environment"
)

#' Matrix Type Factory
#'
#' @description
#' Creates a type that checks for matrices, optionally with a given element
#' type and dimensions. Named \code{MatrixOf} (like \code{ListOf}) so it does
#' not clash with the \code{Matrix} package.
#'
#' @param type Element type, checked against the matrix's values. Defaults to
#'   \code{Any}.
#' @param nrow,ncol Optional required number of rows and columns.
#'
#' @return A sicher_type.
#'
#' @examples
#' m %:% MatrixOf(Numeric, nrow = 2, ncol = 2) %<-% diag(2)
#' try(m <- matrix(1:6, nrow = 2))   # Error: wrong dimensions
#'
#' @export
MatrixOf <- function(type = Any, nrow = NULL, ncol = NULL) {
  check_type_arg(type, "MatrixOf")
  if (!is.null(nrow)) nrow <- check_count_arg(nrow, "MatrixOf")
  if (!is.null(ncol)) ncol <- check_count_arg(ncol, "MatrixOf")
  dims <- if (is.null(nrow) && is.null(ncol)) {
    ""
  } else {
    glue::glue("[{if (is.null(nrow)) '?' else nrow} x {if (is.null(ncol)) '?' else ncol}]")
  }
  type_name <- glue::glue("matrix<{type_label(type)}>{dims}")
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.matrix(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if ((!is.null(nrow) && nrow(x) != nrow) || (!is.null(ncol) && ncol(x) != ncol)) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("expected {type_name}, but got a {nrow(x)} x {ncol(x)} matrix"),
          expected = type_name, got = glue::glue("matrix[{nrow(x)} x {ncol(x)}]"),
          code = "invalid_dimensions"
        )))
      }
      if (!identical(type, Any)) {
        res <- run_type(as.vector(x), type, path)
        if (is_plain_at(res$issues, path)) {
          return(issue_result(x, plain_issue(x, type_name, path)))
        }
        if (length(res$issues) > 0) return(list(value = x, issues = res$issues))
      }
      ok_result(x)
    },
    meta = list(kind = "matrix", inner = type, nrow = nrow, ncol = ncol)
  )
}

#' Instance-of type
#'
#' @description
#' Creates a type that accepts objects inheriting from a class, like zod's
#' \code{z.instanceof()}. Works with S3 classes (via \code{inherits()}), R6
#' classes and S4 classes (via \code{methods::is()}).
#'
#' @param class A character vector of class names. The value must inherit
#'   from at least one of them.
#'
#' @return A sicher_type.
#'
#' @examples
#' model %:% InstanceOf("lm") %<-% lm(mpg ~ wt, data = mtcars)
#' try(model <- list())   # Error: not an lm
#'
#' when %:% InstanceOf(c("Date", "POSIXct")) %<-% Sys.Date()
#'
#' @export
InstanceOf <- function(class) {
  if (!is.character(class) || length(class) == 0 || anyNA(class)) {
    stop("`class` must be a non-empty character vector of class names", call. = FALSE)
  }
  type_name <- glue::glue("instance_of<{paste(class, collapse = ' | ')}>")
  with_meta(
    create_type(type_name, function(x) {
      inherits(x, class) ||
        (isS4(x) && any(vapply(class, function(cl) methods::is(x, cl), logical(1))))
    }),
    kind = "instance_of", class = class
  )
}

#' Branded types
#'
#' @description
#' \code{Brand()} creates a nominal type on top of a structural one, like
#' zod's \code{.brand()}. A branded value carries the brand as a class, so
#' an \code{OrderId} can no longer be passed where a \code{UserId} is
#' expected even though both are strings. Use \code{brand()} to check a
#' value against the base type and attach the brand.
#'
#' @param type A sicher_type or sicher_union object (the base type).
#' @param brand A single string naming the brand. It is added as a class to
#'   branded values.
#' @param value A value to brand.
#'
#' @return \code{Brand()} returns a sicher_type; \code{brand()} returns
#'   \code{value} with the brand class added.
#'
#' @examples
#' UserId  <- Brand(Scalar(String), "UserId")
#' OrderId <- Brand(Scalar(String), "OrderId")
#'
#' uid <- brand("u-123", UserId)
#' current_user %:% UserId %<-% uid
#'
#' try(current_user <- "u-456")                    # Error: not branded
#' try(current_user <- brand("o-789", OrderId))    # Error: wrong brand
#'
#' @name branded-types
#' @export
Brand <- function(type, brand) {
  check_type_arg(type, "Brand")
  if (!is.character(brand) || length(brand) != 1 || is.na(brand) || !nzchar(brand)) {
    stop("`brand` must be a single non-empty string", call. = FALSE)
  }
  type_name <- glue::glue("{brand}<{type$name}>")
  new_parser_type(
    type_name,
    function(x, path) {
      res <- delegate(x, type, path, type_name)
      if (length(res$issues) > 0) return(res)
      if (!inherits(x, brand)) {
        other <- oldClass(x)
        got_desc <- if (length(other) > 0) {
          glue::glue("a value branded as {paste(other, collapse = '/')}")
        } else {
          glue::glue("an unbranded {get_type_name(x)}")
        }
        return(issue_result(x, new_issue(
          path,
          glue::glue(
            "Expected {type_name}, got {got_desc}. ",
            "Create branded values with brand(value, <type>)"
          ),
          expected = type_name, got = get_type_name(x),
          received = format_received(x), code = "invalid_brand"
        )))
      }
      ok_result(x)
    },
    meta = list(kind = "brand", inner = type, brand = brand)
  )
}

#' @rdname branded-types
#' @export
brand <- function(value, type) {
  if (!identical(meta_kind(if (inherits(type, "sicher_type")) type else NULL), "brand")) {
    stop("`type` must be a branded type created with Brand()", call. = FALSE)
  }
  meta <- type_meta(type)
  value <- parse_as(value, meta$inner)
  class(value) <- unique(c(meta$brand, oldClass(value)))
  value
}

# =============================================================================
# Structural types
# =============================================================================

#' Lazy (recursive) types
#'
#' @description
#' Defers building a type until it is first used, like zod's
#' \code{z.lazy()}. This lets a type refer to itself, which is needed for
#' trees and other recursive structures.
#'
#' @param fn A function with no arguments that returns a sicher type. It is
#'   called once, the first time a value is checked.
#' @param name Name used when printing the type. Defaults to \code{"lazy"}.
#'
#' @return A sicher_type.
#'
#' @examples
#' Tree <- create_list_type(list(
#'   value    = Numeric,
#'   children = ListOf(Lazy(function() Tree, name = "Tree"))
#' ))
#'
#' tree %:% Tree %<-% list(
#'   value = 1,
#'   children = list(
#'     list(value = 2, children = list()),
#'     list(value = 3, children = list())
#'   )
#' )
#'
#' @export
Lazy <- function(fn, name = "lazy") {
  if (!is.function(fn)) {
    stop("`fn` must be a function returning a type, e.g. Lazy(function() Tree)", call. = FALSE)
  }
  cache <- new.env(parent = emptyenv())
  resolve <- function() {
    if (is.null(cache$type)) {
      type <- fn()
      if (!is_sicher_type_like(type)) {
        stop(glue::glue(
          "The function passed to Lazy() must return a sicher type; got {class(type)[1]}"
        ), call. = FALSE)
      }
      cache$type <- type
    }
    cache$type
  }
  new_parser_type(
    name,
    function(x, path) run_type(x, resolve(), path),
    meta = list(kind = "lazy", resolve = resolve)
  )
}

#' Tuple Type Factory
#'
#' @description
#' Creates a type for lists with a fixed number of elements, each with its
#' own type, like TypeScript tuples (\code{[string, number]}) and zod's
#' \code{z.tuple()}.
#'
#' @param ... The element types, in order. They may be named, which only
#'   affects the printed type name.
#'
#' @return A sicher_type.
#'
#' @examples
#' Point <- Tuple(Numeric, Numeric)
#' p %:% Point %<-% list(1.5, 2)
#'
#' Entry <- Tuple(key = String, value = Numeric)
#' try(check_type(list("a", "b"), Entry))   # Error at [[2]]
#'
#' @export
Tuple <- function(...) {
  types <- list(...)
  if (length(types) == 0) {
    stop("Tuple() requires at least one element type, e.g. Tuple(String, Numeric)", call. = FALSE)
  }
  for (i in seq_along(types)) {
    if (!is_sicher_type_like(types[[i]])) {
      stop(glue::glue(
        "Element {i} of Tuple() must be a sicher_type or sicher_union; ",
        "got {class(types[[i]])[1]}"
      ), call. = FALSE)
    }
  }
  labels <- vapply(types, type_label, character(1))
  if (!is.null(names(types))) {
    labels <- ifelse(nzchar(names(types)), paste0(names(types), ": ", labels), labels)
  }
  type_name <- glue::glue("[{paste(labels, collapse = ', ')}]")
  n <- length(types)
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.list(x) || is.data.frame(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if (length(x) != n) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("Expected {type_name} ({n} elements), got a list of length {length(x)}"),
          expected = type_name, got = get_type_name(x), code = "invalid_length"
        )))
      }
      issues <- list()
      for (i in seq_len(n)) {
        res <- run_type(x[[i]], types[[i]], c(path, list(i)))
        issues <- c(issues, res$issues)
        if (length(res$issues) == 0 && !identical(res$value, x[[i]])) {
          x[i] <- list(res$value)
        }
      }
      list(value = x, issues = issues)
    },
    meta = list(kind = "tuple", types = types)
  )
}

#' Record Type Factory
#'
#' @description
#' Creates a type for named collections with arbitrary keys, like
#' TypeScript's \code{Record<K, V>} and zod's \code{z.record()}. Every
#' element must be named; each name is checked against \code{keys} and each
#' value against \code{values}. Both named lists and named atomic vectors
#' are accepted.
#'
#' @param keys Type of the names. Defaults to \code{String} when only one
#'   argument is given. Useful choices are \code{Enum()}, \code{Literal()}
#'   and \code{Matches()}.
#' @param values Type of each element.
#'
#' @return A sicher_type.
#'
#' @examples
#' scores %:% Record(String, Numeric) %<-% list(alice = 90, bob = 85)
#'
#' # One argument: keys default to String
#' prices %:% Record(Numeric) %<-% c(apple = 1.2, pear = 0.8)
#'
#' Flags <- Record(Enum("debug", "verbose"), Bool)
#' try(check_type(list(debug = TRUE, color = FALSE), Flags))  # Error: bad key
#'
#' @export
Record <- function(keys, values) {
  if (missing(values)) {
    values <- keys
    keys <- String
  }
  check_type_arg(keys, "Record")
  check_type_arg(values, "Record")
  type_name <- glue::glue("record<{type_label(keys)}, {type_label(values)}>")
  new_parser_type(
    type_name,
    function(x, path) {
      if (is.data.frame(x) || !(is.list(x) || is.atomic(x)) || is.null(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if (length(x) == 0) return(ok_result(x))
      nms <- names(x)
      if (is.null(nms) || any(is.na(nms) | !nzchar(nms))) {
        return(issue_result(x, new_issue(
          path,
          glue::glue("Expected {type_name}, but not every element is named"),
          expected = type_name, got = get_type_name(x), code = "invalid_keys"
        )))
      }
      issues <- list()
      for (i in seq_along(x)) {
        key <- nms[i]
        key_path <- c(path, list(key))
        key_res <- run_type(key, keys, list())
        if (length(key_res$issues) > 0) {
          issues <- c(issues, list(new_issue(
            key_path,
            glue::glue("Invalid key (expected {type_label(keys)})"),
            expected = type_label(keys), got = "string", code = "invalid_key"
          )))
          next
        }
        res <- run_type(x[[i]], values, key_path)
        issues <- c(issues, res$issues)
        if (is.list(x) && length(res$issues) == 0 && !identical(res$value, x[[i]])) {
          x[i] <- list(res$value)
        }
      }
      list(value = x, issues = issues)
    },
    meta = list(kind = "record", keys = keys, values = values)
  )
}

#' Discriminated Union Type Factory
#'
#' @description
#' Creates a union of list types that share a "discriminator" field, like
#' zod's \code{z.discriminatedUnion()} and TypeScript's tagged unions. The
#' discriminator's value picks which member to check, so errors point at the
#' right fields instead of reporting that nothing in the union matched.
#'
#' @param key Name of the discriminator field.
#' @param ... The member types, created with \code{\link{create_list_type}()}.
#'   Either name each argument with its discriminator value (the field is
#'   then added to the member automatically if it does not have it), or leave
#'   them unnamed and give each member a \code{key = Literal(value)} field.
#'
#' @return A sicher_type.
#'
#' @examples
#' Circle <- create_list_type(list(radius = Numeric))
#' Square <- create_list_type(list(side = Numeric))
#'
#' Shape <- DiscriminatedUnion("kind", circle = Circle, square = Square)
#'
#' shape %:% Shape %<-% list(kind = "circle", radius = 2)
#' shape <- list(kind = "square", side = 3)
#' try(shape <- list(kind = "square", radius = 3))  # Error: points at 'side'
#' try(shape <- list(kind = "hexagon"))             # Error: unknown kind
#'
#' @export
DiscriminatedUnion <- function(key, ...) {
  if (!is.character(key) || length(key) != 1 || is.na(key) || !nzchar(key)) {
    stop("`key` must be a single field name, e.g. DiscriminatedUnion(\"type\", ...)", call. = FALSE)
  }
  members <- list(...)
  if (length(members) == 0) {
    stop("DiscriminatedUnion() requires at least one member type", call. = FALSE)
  }
  for (i in seq_along(members)) {
    if (!identical(meta_kind(members[[i]]), "list")) {
      stop(glue::glue(
        "Every DiscriminatedUnion() member must be created with create_list_type(); ",
        "member {i} is not"
      ), call. = FALSE)
    }
  }

  member_names <- names(members)
  if (!is.null(member_names) && all(nzchar(member_names))) {
    tags <- member_names
    members <- lapply(seq_along(members), function(i) {
      spec <- object_spec(members[[i]])
      if (key %in% names(spec)) return(members[[i]])
      tag_field <- structure(list(Literal(tags[i])), names = key)
      rebuild_object_type(members[[i]], c(tag_field, spec))
    })
  } else if (is.null(member_names) || !any(nzchar(member_names))) {
    tags <- vapply(seq_along(members), function(i) {
      field <- object_spec(members[[i]])[[key]]
      field_meta <- if (inherits(field, "sicher_type")) type_meta(field) else NULL
      if (is.null(field_meta) || !identical(field_meta$kind, "literal") ||
          length(field_meta$values) != 1) {
        stop(glue::glue(
          "Member {i} of DiscriminatedUnion() needs a '{key}' field declared as ",
          "Literal(<value>), or name the member: DiscriminatedUnion(\"{key}\", value = Type)"
        ), call. = FALSE)
      }
      as.character(field_meta$values[[1]])
    }, character(1))
  } else {
    stop("Either name all DiscriminatedUnion() members or none of them", call. = FALSE)
  }

  if (anyDuplicated(tags) > 0) {
    stop(glue::glue(
      "DiscriminatedUnion() discriminator values must be unique; ",
      "duplicated: {paste(unique(tags[duplicated(tags)]), collapse = ', ')}"
    ), call. = FALSE)
  }
  names(members) <- tags

  shown_tags <- paste(encodeString(tags, quote = "\""), collapse = " | ")
  type_name <- glue::glue("discriminated<{key}: {shown_tags}>")

  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.list(x) || is.data.frame(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      key_path <- c(path, list(key))
      if (!(key %in% names(x))) {
        return(issue_result(x, new_issue(
          key_path,
          glue::glue("Missing discriminator field (expected one of {shown_tags})"),
          expected = shown_tags, got = "missing", code = "invalid_discriminator"
        )))
      }
      tag <- x[[key]]
      if (!is.atomic(tag) || length(tag) != 1 || is.na(tag) ||
          !(as.character(tag) %in% tags)) {
        return(issue_result(x, new_issue(
          key_path,
          glue::glue("Invalid discriminator value (expected one of {shown_tags})"),
          expected = shown_tags, got = get_type_name(tag),
          received = format_received(tag), code = "invalid_discriminator"
        )))
      }
      run_type(x, members[[as.character(tag)]], path)
    },
    meta = list(kind = "discriminated", key = key, members = members)
  )
}

#' Function Type Factory
#'
#' @description
#' Creates a type for functions with a given signature, like TypeScript's
#' \code{(x: number) => string} and zod's \code{z.function()}. A value must
#' be a function that accepts the named parameters (or \code{...}).
#'
#' Because the types of a function's parameters cannot be known before it
#' is called, the checked function is wrapped with
#' \code{\link{typed_function}()}: each later call checks its arguments and
#' return value. Typed variables, typed function arguments and
#' \code{\link{parse_as}()} store this wrapped function.
#'
#' @param params A named list mapping parameter names to types.
#' @param .return Optional return type.
#'
#' @return A sicher_type.
#'
#' @examples
#' Formatter <- FunctionOf(list(x = Numeric), .return = String)
#'
#' fmt %:% Formatter %<-% function(x) format(x, nsmall = 2)
#' fmt(3)            # "3.00"
#' try(fmt("a"))     # Error: argument x must be numeric
#'
#' try(fmt <- function(y) y)   # Error: no parameter named x
#'
#' @export
FunctionOf <- function(params = list(), .return = NULL) {
  if (!is.list(params) || (length(params) > 0 &&
      (is.null(names(params)) || any(!nzchar(names(params)))))) {
    stop("`params` must be a named list of types, e.g. list(x = Numeric)", call. = FALSE)
  }
  check_spec_list(params, "params", what = "Parameter")
  if (!is.null(.return)) check_type_arg(.return, "FunctionOf")

  param_str <- paste(
    vapply(names(params), function(nm) paste0(nm, ": ", type_label(params[[nm]])), character(1)),
    collapse = ", "
  )
  ret_str <- if (is.null(.return)) "any" else type_label(.return)
  type_name <- glue::glue("function({param_str}) => {ret_str}")

  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.function(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      formal_names <- names(formals(if (is.primitive(x)) args(x) else x))
      missing_params <- setdiff(names(params), formal_names)
      if (length(missing_params) > 0 && !("..." %in% formal_names)) {
        return(issue_result(x, new_issue(
          path,
          glue::glue(
            "Expected {type_name}, but the function has no parameter(s) named ",
            "{paste(missing_params, collapse = ', ')}"
          ),
          expected = type_name, got = "function", code = "invalid_function"
        )))
      }
      if (is.primitive(x) || inherits(x, "sicher_typed_function")) {
        return(ok_result(x))
      }
      ok_result(typed_function(x, params = params, .return = .return))
    },
    meta = list(kind = "function", params = params, return = .return)
  )
}
