# =============================================================================
# Object type helpers (TypeScript utility types)
# =============================================================================

collect_field_names <- function(..., fn_name) {
  fields <- unlist(list(...), use.names = FALSE)
  if (!is.character(fields) || length(fields) == 0 || anyNA(fields)) {
    stop(glue::glue(
      "{fn_name}() needs one or more field names, e.g. {fn_name}(Person, \"name\", \"age\")"
    ), call. = FALSE)
  }
  unique(fields)
}

check_known_fields <- function(fields, spec, fn_name) {
  unknown <- setdiff(fields, names(spec))
  if (length(unknown) > 0) {
    stop(glue::glue(
      "{fn_name}(): unknown field(s) {paste(unknown, collapse = ', ')} ",
      "(available fields: {paste(names(spec), collapse = ', ')})"
    ), call. = FALSE)
  }
}

#' Object type utilities: Pick, Omit, Partial, Required, KeyOf
#'
#' @description
#' Derive new list (or data frame) types from existing ones, like
#' TypeScript's utility types and the matching zod methods:
#'
#' \itemize{
#'   \item \code{Pick(type, ...)} keeps only the named fields
#'     (\code{Pick<T, K>}, zod \code{.pick()}).
#'   \item \code{Omit(type, ...)} removes the named fields
#'     (\code{Omit<T, K>}, zod \code{.omit()}).
#'   \item \code{Partial(type)} makes every field optional
#'     (\code{Partial<T>}, zod \code{.partial()}).
#'   \item \code{Required(type)} makes every field required, removing
#'     \code{Optional()} and \code{Default()} wrappers
#'     (\code{Required<T>}, zod \code{.required()}). \code{Nullable()} fields
#'     stay nullable.
#'   \item \code{KeyOf(type)} returns an \code{Enum()} of the field names
#'     (\code{keyof T}, zod \code{.keyof()}).
#' }
#'
#' The derived types keep the \code{extra} setting of the original type.
#'
#' @param type A type created with \code{\link{create_list_type}()} or
#'   \code{\link{create_dataframe_type}()}.
#' @param ... Field names, as separate strings or character vectors.
#'
#' @return A new sicher_type.
#'
#' @examples
#' User <- create_list_type(list(
#'   id    = Integer,
#'   name  = String,
#'   email = Optional(String)
#' ))
#'
#' UserPreview <- Pick(User, "id", "name")
#' NewUser     <- Omit(User, "id")
#' UserPatch   <- Partial(Omit(User, "id"))
#' FullUser    <- Required(User)
#' UserField   <- KeyOf(User)
#'
#' patch %:% UserPatch %<-% list(email = "new@example.com")
#' field %:% UserField %<-% "email"
#'
#' @name object-helpers
#' @export
Pick <- function(type, ...) {
  spec <- require_object_type(type, "Pick")
  fields <- collect_field_names(..., fn_name = "Pick")
  check_known_fields(fields, spec, "Pick")
  rebuild_object_type(type, spec[fields])
}

#' @rdname object-helpers
#' @export
Omit <- function(type, ...) {
  spec <- require_object_type(type, "Omit")
  fields <- collect_field_names(..., fn_name = "Omit")
  check_known_fields(fields, spec, "Omit")
  rebuild_object_type(type, spec[setdiff(names(spec), fields)])
}

#' @rdname object-helpers
#' @export
Partial <- function(type) {
  spec <- require_object_type(type, "Partial")
  spec[] <- lapply(spec, function(field) {
    if (field_is_optional(field)) field else map_core(field, Optional)
  })
  rebuild_object_type(type, spec)
}

make_required <- function(field) {
  map_core(field, make_required_core)
}

make_required_core <- function(field) {
  if (inherits(field, "sicher_type") && identical(meta_kind(field), "default")) {
    return(type_meta(field)$inner)
  }
  if (inherits(field, "sicher_union") && field_is_optional(field)) {
    kept <- Filter(function(t) !identical(t, Null), field$types)
    if (length(kept) == 0) return(Null)
    if (length(kept) == 1) return(kept[[1]])
    return(Reduce(create_union, kept))
  }
  field
}

#' @rdname object-helpers
#' @export
Required <- function(type) {
  spec <- require_object_type(type, "Required")
  spec[] <- lapply(spec, make_required)
  rebuild_object_type(type, spec)
}

#' @rdname object-helpers
#' @export
KeyOf <- function(type) {
  spec <- require_object_type(type, "KeyOf")
  if (length(spec) == 0) {
    stop("KeyOf() needs a type with at least one field", call. = FALSE)
  }
  Enum(names(spec))
}
