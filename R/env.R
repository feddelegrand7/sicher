# =============================================================================
# Settings from environment variables: parse_env()
# =============================================================================

#' Read settings from environment variables
#'
#' @description
#' Reads one environment variable per field of a list type, converts the
#' text to the field's type and checks it, returning a named list of
#' settings. All problems are reported at once, each naming the environment
#' variable, so a misconfigured app fails at startup with a clear message
#' instead of misbehaving later.
#'
#' \strong{Variable names.} The variable for a field is \code{prefix}
#' followed by the field name, in upper case: with \code{prefix = "APP_"},
#' the field \code{db_url} is read from \code{APP_DB_URL}. A field wrapped
#' in \code{\link{Alias}()} is read from its alias exactly, without the
#' prefix, which is useful for standard variables such as
#' \code{DATABASE_URL}. Names are matched case-insensitively unless
#' \code{case_sensitive = TRUE}.
#'
#' \strong{Conversion.} Environment variables are always text, so each
#' value is converted to the field's type. The first of these readings that
#' satisfies the type is used: the text itself, a whole number, a number, a
#' logical (\code{true}/\code{false}, \code{yes}/\code{no},
#' \code{on}/\code{off}, \code{1}/\code{0}, in any case), a date
#' (\code{YYYY-MM-DD}), a date-time, and the conversion
#' \code{\link{Coerce}()} would use for the type. Values that look like a
#' JSON array or object (starting with \code{[} or \code{\{}) are parsed as
#' JSON, which is how vectors and nested lists are given (requires the
#' \code{jsonlite} package).
#'
#' \strong{Missing values.} Variables that are not set, or set to an empty
#' string, count as missing: fields with a \code{\link{Default}()} get their
#' default, \code{\link{Optional}()} fields are left out, and other fields
#' are reported as missing.
#'
#' To load variables from a \code{.env} file first, use
#' \code{readRenviron(".env")}.
#'
#' @param type A type created with \code{\link{create_list_type}()}.
#' @param prefix A string put in front of every field name to build the
#'   variable name, e.g. \code{"APP_"}. Defaults to \code{""}.
#' @param env A named character vector of variables to read from. Defaults
#'   to all environment variables, \code{Sys.getenv()}.
#' @param case_sensitive If \code{TRUE}, variable names must match exactly.
#'   Defaults to \code{FALSE}.
#'
#' @return A named list with one element per field that was set or has a
#'   default, named by the field names.
#'
#' @examples
#' Settings <- create_list_type(list(
#'   db_url  = Scalar(Matches("^postgres://")),
#'   port    = Default(Scalar(Integer), 8080L),
#'   debug   = Default(Scalar(Bool), FALSE),
#'   workers = Optional(Scalar(Whole))
#' ))
#'
#' env <- c(APP_DB_URL = "postgres://localhost/app", APP_DEBUG = "yes")
#' parse_env(Settings, prefix = "APP_", env = env)
#'
#' try(parse_env(Settings, prefix = "APP_", env = c(APP_PORT = "eighty")))
#'
#' @export
parse_env <- function(type, prefix = "", env = Sys.getenv(),
                      case_sensitive = FALSE) {
  # Look through Refine() so rules on the whole settings object are allowed.
  base <- type
  while (inherits(base, "sicher_type") && identical(meta_kind(base), "refine")) {
    base <- type_meta(base)$inner
  }
  spec <- require_object_type(base, "parse_env")
  if (identical(object_kind(base), "data.frame")) {
    stop("parse_env() requires a type created with create_list_type()", call. = FALSE)
  }
  if (!is.character(prefix) || length(prefix) != 1 || is.na(prefix)) {
    stop("`prefix` must be a single string, e.g. \"APP_\"", call. = FALSE)
  }
  if (!is.character(env) || (length(env) > 0 && is.null(names(env)))) {
    stop("`env` must be a named character vector, e.g. Sys.getenv()", call. = FALSE)
  }
  if (!is.logical(case_sensitive) || length(case_sensitive) != 1 || is.na(case_sensitive)) {
    stop("`case_sensitive` must be TRUE or FALSE", call. = FALSE)
  }

  env <- env[!is.na(env) & nzchar(env)]
  normalize <- if (case_sensitive) identity else toupper
  env_names <- normalize(names(env))

  out <- list()
  issues <- list()
  for (field in names(spec)) {
    field_type <- spec[[field]]
    alias <- field_alias(field_type)
    var <- if (is.null(alias)) toupper(paste0(prefix, field)) else alias
    idx <- match(normalize(var), env_names)

    if (!is.na(idx)) {
      var <- names(env)[idx]
      value <- env_value(env[[idx]], field_type)
      res <- run_type(value, field_type, list(var))
      if (!is.character(value) && inherits(peel(field_type), "sicher_union") &&
          is_plain_at(res$issues, list(var))) {
        # Report the rule the value breaks rather than the union mismatch.
        for (member in peel(field_type)$types) {
          member_issues <- run_type(value, member, list(var))$issues
          if (length(member_issues) > 0 && !is_plain_at(member_issues, list(var))) {
            res$issues <- member_issues
            break
          }
        }
      }
      if (is.character(value) && is_plain_at(res$issues, list(var))) {
        res$issues <- list(new_issue(
          list(var),
          glue::glue(
            "cannot convert {encodeString(value, quote = '\"')} to {type_label(field_type)}"
          ),
          expected = type_label(field_type), got = "string",
          code = "invalid_coercion"
        ))
      }
      issues <- c(issues, res$issues)
      out[field] <- list(res$value)
    } else {
      default <- field_default(field_type)
      if (default$has) {
        out[field] <- list(default$value)
      } else if (!field_is_optional(field_type)) {
        issues <- c(issues, list(new_issue(
          list(var),
          glue::glue(
            "Missing required environment variable (expected {type_label(field_type)})"
          ),
          expected = type_label(field_type), got = "missing", code = "missing_field"
        )))
      }
    }
  }
  if (length(issues) > 0) sicher_abort(issues)

  # Rules on the whole settings object, e.g. Refine() on the list type.
  res <- run_type(out, type)
  if (length(res$issues) > 0) sicher_abort(res$issues)
  res$value
}

env_true <- c("true", "t", "yes", "y", "on", "1")
env_false <- c("false", "f", "no", "n", "off", "0")

# Convert the text of an environment variable to a value of `type`: the
# first candidate reading that satisfies the type wins. When none does, the
# text is returned unchanged so the error shows what was set.
env_value <- function(text, type) {
  text <- trimws(text)
  quiet <- function(expr) {
    tryCatch(suppressWarnings(expr), error = function(e) NULL)
  }
  candidates <- list(
    function() if (grepl("^[\\[{]", text)) {
      require_jsonlite("parse_env() for JSON values")
      quiet(jsonlite::fromJSON(text, simplifyVector = TRUE,
                               simplifyDataFrame = FALSE, simplifyMatrix = FALSE))
    },
    function() text,
    function() if (grepl("^[-+]?[0-9]+$", text)) quiet(as.integer(text)),
    function() quiet({
      value <- as.numeric(text)
      if (is.na(value)) NULL else value
    }),
    function() {
      lower <- tolower(text)
      if (lower %in% env_true) TRUE else if (lower %in% env_false) FALSE
    },
    function() if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)) quiet(as.Date(text)),
    function() if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}[T ]", text)) {
      quiet(as.POSIXct(sub("T", " ", sub("Z$", "", text), fixed = TRUE),
                       tz = if (grepl("Z$", text)) "UTC" else ""))
    },
    # Numbers were read exactly above; as.integer() would truncate "80.5".
    function() {
      coercer <- find_coercer(type)
      value <- if (is.function(coercer)) quiet(coercer(text))
      if (!is.numeric(value)) value
    }
  )
  # A reading that has the right type but breaks a rule (e.g. "2" for
  # Between(0, 1)) gives a more useful error than the raw text.
  members <- if (inherits(type, "sicher_union")) {
    Filter(function(t) !identical(t, Null), type$types)
  } else {
    list(type)
  }
  breaks_rule <- function(value) {
    any(vapply(members, function(member) {
      issues <- run_type(value, member)$issues
      length(issues) > 0 && !is_plain_at(issues, list())
    }, logical(1)))
  }
  fallback <- NULL
  for (candidate in candidates) {
    value <- candidate()
    if (is.null(value) || anyNA(value)) next
    if (length(run_type(value, type)$issues) == 0) return(value)
    if (is.null(fallback) && !is.character(value) && breaks_rule(value)) {
      fallback <- list(value)
    }
  }
  if (is.null(fallback)) text else fallback[[1]]
}
