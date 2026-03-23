.onLoad <- function(libname, pkgname) {
  op <- options()
  op.sicher <- list(
    sicher.mode = "on"
  )
  toset <- !(names(op.sicher) %in% names(op))
  if (any(toset)) options(op.sicher[toset])
}
