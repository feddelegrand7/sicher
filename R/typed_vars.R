# =============================================================================
# Typed variable introspection
# =============================================================================

find_binding_env <- function(name, env) {
  while (!identical(env, emptyenv())) {
    if (exists(name, envir = env, inherits = FALSE)) return(env)
    env <- parent.env(env)
  }
  NULL
}

# Returns the private environment of a typed variable, or NULL if `name` in
# `env` is not a sicher typed variable.
typed_binding_state <- function(name, env) {
  if (!bindingIsActive(name, env)) return(NULL)
  fun <- activeBindingFunction(name, env)
  state <- environment(fun)$penv
  if (!is.environment(state) || !exists("type", envir = state, inherits = FALSE)) {
    return(NULL)
  }
  state
}

typed_var_name <- function(expr, fn_name) {
  if (!is.name(expr)) {
    stop(glue::glue(
      "{fn_name}() expects a variable name, e.g. {fn_name}(x); got {deparse(expr)[1]}"
    ), call. = FALSE)
  }
  as.character(expr)
}

#' Get the declared type of a typed variable
#'
#' @description
#' Returns the type a variable was declared with using \code{\%:\%}, similar
#' to TypeScript's \code{typeof} in type positions. Returns \code{NULL} for
#' variables that are not typed. To guess a type from a value instead, use
#' \code{\link{infer_type}()}.
#'
#' @param x A variable name (unquoted).
#'
#' @return The declared type (wrapped in \code{Readonly()} for readonly
#'   variables), or \code{NULL}.
#'
#' @examples
#' age %:% Numeric %<-% 30
#' type_of(age)         # <type: numeric >
#'
#' plain <- 1
#' type_of(plain)       # NULL
#'
#' # Reuse a variable's type for another one
#' other_age %:% type_of(age) %<-% 25
#'
#' @export
type_of <- function(x) {
  name <- typed_var_name(substitute(x), "type_of")
  env <- find_binding_env(name, parent.frame())
  if (is.null(env)) {
    stop(glue::glue("object '{name}' not found"), call. = FALSE)
  }
  state <- typed_binding_state(name, env)
  if (is.null(state)) return(NULL)
  if (isTRUE(state$readonly)) Readonly(state$type) else state$type
}

#' Remove the type from a typed variable
#'
#' @description
#' Turns a typed variable back into a regular variable holding the same
#' value. Later assignments are no longer checked. Does nothing for
#' variables that are not typed.
#'
#' @param x A variable name (unquoted).
#'
#' @return The variable's value, invisibly.
#'
#' @examples
#' count %:% Integer %<-% 1L
#' untype(count)
#' count <- "no longer checked"
#'
#' @export
untype <- function(x) {
  name <- typed_var_name(substitute(x), "untype")
  env <- find_binding_env(name, parent.frame())
  if (is.null(env)) {
    stop(glue::glue("object '{name}' not found"), call. = FALSE)
  }
  state <- typed_binding_state(name, env)
  if (is.null(state)) return(invisible(get(name, envir = env)))
  value <- state$value
  rm(list = name, envir = env)
  assign(name, value, envir = env)
  invisible(value)
}
