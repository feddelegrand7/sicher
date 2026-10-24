# =============================================================================
# Validation Engine
# =============================================================================
#
# Every check in sicher goes through run_type(), which walks a value and a
# type together and returns:
#
#   list(value = <possibly transformed value>, issues = <list of issues>)
#
# An issue is a small list describing one problem at one location:
#
#   path     list of path segments (character = field name, numeric = index)
#   message  human-readable description, without the "Type error" prefix
#   expected type name that was expected (or NA)
#   got      description of what was received (or NA)
#   received short preview of the received value (or NULL)
#   details  optional extra explanation (or NULL)
#   code     machine-readable kind of issue ("invalid_type", "custom", ...)
#
# Plain create_type() types are leaves: their checker is called and a FALSE
# result (or an error) becomes an issue. Structured types (lists, data
# frames, modifiers, ...) carry a `parse` function so they can report every
# nested issue with its full path and transform the value (Default, Coerce,
# extra = "drop", FunctionOf).

new_issue <- function(path, message, expected = NA_character_,
                      got = NA_character_, received = NULL, details = NULL,
                      code = "custom") {
  list(
    path     = path,
    message  = as.character(message),
    expected = as.character(expected),
    got      = as.character(got),
    received = received,
    details  = details,
    code     = code
  )
}

# The standard "Expected <type>, got <type>" issue.
plain_issue <- function(x, expected, path, details = NULL) {
  got <- get_type_name(x)
  new_issue(
    path     = path,
    message  = glue::glue("Expected {expected}, got {got}"),
    expected = expected,
    got      = got,
    received = format_received(x),
    details  = details,
    code     = "invalid_type"
  )
}

ok_result <- function(value) list(value = value, issues = list())

issue_result <- function(value, ...) list(value = value, issues = list(...))

# TRUE when the only issue is a plain type mismatch at `path` itself. Wrapper
# types use this to report their own name instead of their inner type's name
# (e.g. "Expected scalar<numeric>, got string" instead of "Expected numeric").
is_plain_at <- function(issues, path) {
  length(issues) == 1 &&
    identical(issues[[1]]$code, "invalid_type") &&
    identical(issues[[1]]$path, path)
}

# Run `type` on `x` and, if it fails with a plain mismatch at the same
# location, re-label the failure with `outer_name`.
delegate <- function(x, type, path, outer_name) {
  res <- run_type(x, type, path)
  if (is_plain_at(res$issues, path)) {
    res$issues <- list(plain_issue(x, outer_name, path))
  }
  res
}

is_sicher_type_like <- function(type) {
  inherits(type, "sicher_type") ||
    inherits(type, "sicher_union") ||
    inherits(type, "sicher_readonly")
}

# Build a structured type: a sicher_type with a `parse` function.
#
# `strict_check` controls what `$check()` does on failure: lenient types
# (FALSE, the default) return FALSE when the only problem is a plain
# mismatch on the value itself (so callers can add their own context) and
# throw otherwise; strict types (TRUE) always throw; "never" types always
# return FALSE (kept for ListOf(), whose $check() never threw).
new_parser_type <- function(name, parse, meta = NULL, strict_check = FALSE) {
  name <- as.character(name)
  check <- function(x) {
    res <- parse(x, list())
    if (length(res$issues) == 0) return(TRUE)
    if (identical(strict_check, "never")) return(FALSE)
    if (isFALSE(strict_check) && is_plain_at(res$issues, list())) return(FALSE)
    sicher_abort(res$issues)
  }
  type <- structure(
    list(name = name, check = check, parse = parse),
    class = "sicher_type"
  )
  attr(type, "sicher_meta") <- meta
  type
}

type_meta <- function(type) attr(type, "sicher_meta", exact = TRUE)

meta_kind <- function(type) {
  meta <- type_meta(type)
  if (is.null(meta)) NA_character_ else meta$kind
}

invalid_type_spec <- function(type) {
  hint <- if (is.function(type)) {
    " Did you forget to call a type factory, e.g. Factor() instead of Factor?"
  } else {
    ""
  }
  stop(glue::glue(
    "Invalid type specification: expected a sicher_type, sicher_union, or sicher_readonly ",
    "(e.g. Numeric, String | Bool, Optional(Integer)); ",
    "got {class(type)[1]}.{hint}"
  ), call. = FALSE)
}

run_type <- function(x, type, path = list()) {
  if (inherits(type, "sicher_readonly")) {
    type <- type$base_type
  }

  if (inherits(type, "sicher_union")) {
    return(run_union(x, type, path))
  }

  if (!inherits(type, "sicher_type")) {
    invalid_type_spec(type)
  }

  if (is.function(type$parse)) {
    return(type$parse(x, path))
  }

  # Leaf types: NULL only satisfies Null and Any.
  if (is.null(x)) {
    if (identical(type, Null) || identical(type, Any)) return(ok_result(x))
    return(issue_result(x, plain_issue(x, type$name, path)))
  }

  outcome <- tryCatch(
    type$check(x),
    sicher_type_error = function(e) e,
    error = function(e) e
  )

  if (isTRUE(outcome)) {
    return(ok_result(x))
  }

  if (inherits(outcome, "sicher_type_error")) {
    issues <- lapply(outcome$issues, function(issue) {
      issue$path <- c(path, issue$path)
      issue
    })
    return(list(value = x, issues = issues))
  }

  if (inherits(outcome, "error")) {
    msg <- sub("^Type error: ", "", conditionMessage(outcome))
    return(issue_result(x, new_issue(path, msg, expected = type$name)))
  }

  issue_result(x, plain_issue(x, type$name, path))
}

run_union <- function(x, type, path) {
  if (is.null(x) && union_has_null(type)) {
    return(ok_result(x))
  }
  for (member in type$types) {
    res <- run_type(x, member, path)
    if (length(res$issues) == 0) return(res)
  }
  issue_result(x, plain_issue(x, type$name, path))
}

union_has_null <- function(type) {
  any(vapply(type$types, function(t) identical(t, Null), logical(1)))
}

# Field wrappers: Alias() and Describe() only add information about a field
# and Coerce() only changes how its value is read. Field-level questions (is
# the field optional? does it have a default?) look through them.
field_wrapper_kinds <- c("alias", "describe", "coerce")

peel <- function(type, kinds = field_wrapper_kinds) {
  repeat {
    if (inherits(type, "sicher_readonly")) {
      type <- type$base_type
    } else if (inherits(type, "sicher_type") && meta_kind(type) %in% kinds) {
      type <- type_meta(type)$inner
    } else {
      return(type)
    }
  }
}

# Apply `fn` to the type inside the field wrappers and wrap the result again,
# so e.g. Partial() turns Alias(String, "x") into Alias(Optional(String), "x").
map_core <- function(type, fn) {
  if (inherits(type, "sicher_readonly")) {
    return(Readonly(map_core(type$base_type, fn)))
  }
  kind <- if (inherits(type, "sicher_type")) meta_kind(type) else NA_character_
  if (!(kind %in% field_wrapper_kinds)) return(fn(type))
  meta <- type_meta(type)
  inner <- map_core(meta$inner, fn)
  switch(
    kind,
    alias = Alias(inner, meta$alias),
    describe = Describe(inner, description = meta$description,
                        title = meta$title, examples = meta$examples),
    coerce = Coerce(inner, using = meta$using)
  )
}

# A field may be omitted from a list (or data frame) when it is Optional()
# (a union containing Null that was not built by Nullable()) or has a
# Default().
field_is_optional <- function(type) {
  type <- peel(type)
  if (inherits(type, "sicher_union")) {
    return(union_has_null(type) && !isTRUE(attr(type, "sicher_nullable")))
  }
  identical(meta_kind(type), "default")
}

field_default <- function(type) {
  type <- peel(type)
  if (identical(meta_kind(type), "default")) {
    return(list(has = TRUE, value = type_meta(type)$value))
  }
  list(has = FALSE, value = NULL)
}

# -----------------------------------------------------------------------------
# Paths and messages
# -----------------------------------------------------------------------------

format_path <- function(path) {
  out <- ""
  for (seg in path) {
    if (is.numeric(seg)) {
      out <- paste0(out, "[[", seg, "]]")
    } else if (!nzchar(out)) {
      out <- seg
    } else if (make.names(seg) == seg) {
      out <- paste0(out, "$", seg)
    } else {
      out <- paste0(out, "$`", seg, "`")
    }
  }
  out
}

format_received <- function(value) {
  tryCatch({
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
    } else if (is.function(value)) {
      "<function>"
    } else if (is.environment(value)) {
      "<environment>"
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
}

format_issue <- function(issue) {
  where <- format_path(issue$path)
  msg <- if (nzchar(where)) {
    glue::glue("Type error in '{where}': {issue$message}")
  } else {
    glue::glue("Type error: {issue$message}")
  }
  if (!is.null(issue$details)) {
    msg <- glue::glue("{msg}\n  Details: {issue$details}")
  }
  msg
}

format_issues <- function(issues) {
  if (length(issues) == 1) {
    issue <- issues[[1]]
    msg <- format_issue(issue)
    if (!is.null(issue$received)) {
      msg <- glue::glue("{msg}\n  Received: {issue$received}")
    }
    return(as.character(msg))
  }
  lines <- vapply(issues, function(issue) {
    gsub("\n", "\n    ", format_issue(issue), fixed = TRUE)
  }, character(1))
  paste0(
    length(issues), " type errors found:\n",
    paste0("  * ", lines, collapse = "\n")
  )
}

sicher_condition <- function(issues, class) {
  structure(
    class = c(class, "condition"),
    list(message = format_issues(issues), call = NULL, issues = issues)
  )
}

sicher_abort <- function(issues) {
  stop(sicher_condition(issues, c("sicher_type_error", "error")))
}

sicher_warn <- function(issues) {
  warning(sicher_condition(issues, c("sicher_type_warning", "warning")))
}

sicher_mode <- function() {
  mode <- getOption("sicher.mode", "on")
  if (identical(mode, "off") || identical(mode, "warn")) mode else "on"
}

# Run a type at a top-level entry point (typed variable, typed function
# argument or return value). Honors sicher.mode = "warn": violations are
# reported as warnings and the original value is kept.
enforce_type <- function(value, type, context) {
  res <- run_type(value, type, if (is.null(context)) list() else list(context))
  if (length(res$issues) == 0) return(res$value)
  if (identical(sicher_mode(), "warn")) {
    sicher_warn(res$issues)
    return(value)
  }
  sicher_abort(res$issues)
}

issues_to_df <- function(issues) {
  if (length(issues) == 0) {
    return(data.frame(
      path = character(0), message = character(0), expected = character(0),
      got = character(0), code = character(0), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    path     = vapply(issues, function(i) format_path(i$path), character(1)),
    message  = vapply(issues, function(i) as.character(i$message), character(1)),
    expected = vapply(issues, function(i) as.character(i$expected), character(1)),
    got      = vapply(issues, function(i) as.character(i$got), character(1)),
    code     = vapply(issues, function(i) as.character(i$code), character(1)),
    stringsAsFactors = FALSE
  )
}

check_spec_list <- function(spec, arg, what = "Field") {
  for (i in seq_along(spec)) {
    if (!is_sicher_type_like(spec[[i]])) {
      stop(glue::glue(
        "{what} '{names(spec)[i]}' in `{arg}` must be a sicher_type or sicher_union ",
        "(e.g. String, Numeric, Optional(Bool)); ",
        "got {class(spec[[i]])[1]}"
      ), call. = FALSE)
    }
  }
}

check_type_arg <- function(type, fn_name) {
  if (!is_sicher_type_like(type)) {
    stop(glue::glue(
      "{fn_name}() requires a sicher_type or sicher_union argument; ",
      "got {class(type)[1]}"
    ), call. = FALSE)
  }
}

type_label <- function(type) {
  if (inherits(type, "sicher_readonly")) type <- type$base_type
  type$name
}

with_meta <- function(type, ...) {
  attr(type, "sicher_meta") <- list(...)
  type
}

value_size <- function(x) {
  if (is.data.frame(x)) nrow(x) else length(x)
}

# Numeric rule types (Between, Positive, NonNegative, Whole): numeric,
# NA-free, and every element passes `test`.
numeric_rule_type <- function(type_name, test, desc_single, desc_plural,
                              meta = NULL) {
  type_name <- as.character(type_name)
  new_parser_type(
    type_name,
    function(x, path) {
      if (!is.numeric(x)) {
        return(issue_result(x, plain_issue(x, type_name, path)))
      }
      if (anyNA(x)) {
        return(issue_result(x, new_issue(
          path,
          glue::glue(
            "expected {type_name} (no NA values), ",
            "but value contains NA(s)"
          ),
          expected = type_name, got = "NA", code = "na_value"
        )))
      }
      failing <- x[!test(x)]
      if (length(failing) > 0) {
        preview <- paste(utils::head(failing, 5), collapse = ", ")
        if (length(failing) > 5) preview <- paste0(preview, ", ...")
        single <- length(failing) == 1
        noun <- if (single) "value" else "values"
        verb <- if (single) "is" else "are"
        desc <- if (single) desc_single else desc_plural
        return(issue_result(x, new_issue(
          path,
          glue::glue("expected {type_name}, but {noun} [{preview}] {verb} {desc}"),
          expected = type_name, got = get_type_name(x), code = "out_of_range"
        )))
      }
      ok_result(x)
    },
    meta = meta,
    strict_check = TRUE
  )
}
