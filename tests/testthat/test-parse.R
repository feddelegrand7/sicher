# Parsing API, path-aware errors and modes

testthat::test_that("is_type returns TRUE/FALSE without throwing", {
  testthat::expect_true(is_type(1, Numeric))
  testthat::expect_false(is_type("a", Numeric))
  testthat::expect_true(is_type(NULL, Optional(String)))
  testthat::expect_false(is_type(list(a = "x"), create_list_type(list(a = Numeric))))
  testthat::expect_error(is_type(1, "not a type"), "Invalid type specification")
})

testthat::test_that("safe_parse reports success and data", {
  res <- safe_parse(list(a = 1), create_list_type(list(a = Numeric)))
  testthat::expect_s3_class(res, "sicher_parse_result")
  testthat::expect_true(res$success)
  testthat::expect_equal(res$data, list(a = 1))
  testthat::expect_equal(nrow(res$issues), 0)
  testthat::expect_null(res$error)
  testthat::expect_output(print(res), "success")
})

testthat::test_that("safe_parse collects every issue with its path", {
  Todo <- create_list_type(list(id = Numeric, title = String, done = Bool))
  value <- list(
    list(id = 1, title = "ok", done = FALSE),
    list(id = "x", title = 2)
  )
  res <- safe_parse(value, ListOf(Todo))
  testthat::expect_false(res$success)
  testthat::expect_null(res$data)
  testthat::expect_equal(res$issues$path, c("[[2]]$id", "[[2]]$title", "[[2]]$done"))
  testthat::expect_equal(res$issues$code, c("invalid_type", "invalid_type", "missing_field"))
  testthat::expect_s3_class(res$error, "sicher_type_error")
  testthat::expect_output(print(res), "3 issues")
})

testthat::test_that("parse_as returns the parsed value or throws all issues", {
  Point <- create_list_type(list(x = Numeric, y = Numeric))
  testthat::expect_equal(parse_as(list(x = 1, y = 2), Point), list(x = 1, y = 2))
  err <- tryCatch(
    parse_as(list(x = "a", y = "b"), Point, context = "pt"),
    sicher_type_error = function(e) e
  )
  testthat::expect_s3_class(err, "sicher_type_error")
  testthat::expect_length(err$issues, 2)
  testthat::expect_match(conditionMessage(err), "2 type errors found")
  testthat::expect_match(conditionMessage(err), "Type error in 'pt\\$x'")
  testthat::expect_match(conditionMessage(err), "Type error in 'pt\\$y'")
})

testthat::test_that("typed variables report full paths for nested errors", {
  Todo <- create_list_type(list(title = String))
  todos %:% ListOf(Todo) %<-% list(list(title = "a"))
  testthat::expect_error(
    todos <- list(list(title = "a"), list(title = 1)),
    "Type error in 'todos\\[\\[2\\]\\]\\$title'"
  )
})

testthat::test_that("data frame column errors include the variable name", {
  Users <- create_dataframe_type(list(id = Integer))
  testthat::expect_error(
    users %:% Users %<-% data.frame(id = c("a", "b")),
    "Type error in 'users\\$id'"
  )
})

testthat::test_that("issues report the positions of failing elements", {
  res <- safe_parse(c(1, -2, 3, -4), Positive)
  testthat::expect_equal(res$issues$index[[1]], c(2L, 4L))
  testthat::expect_match(res$issues$message, "\\(at positions 2, 4\\)$")

  testthat::expect_equal(safe_parse(c("a", NA), NonNA(String))$issues$index[[1]], 2L)
  testthat::expect_equal(safe_parse(c("A", "b"), Matches("^[A-Z]$"))$issues$index[[1]], 2L)
  testthat::expect_equal(safe_parse(c(1, NA), Between(0, 1))$issues$index[[1]], 2L)
  testthat::expect_equal(safe_parse(c(1, 2, 1), Unique(Numeric))$issues$index[[1]], 3L)

  # A single value has no position to report
  single <- safe_parse(-1, Positive)
  testthat::expect_equal(single$issues$index[[1]], integer(0))
  testthat::expect_false(grepl("at position", single$issues$message))

  # Long lists are shortened
  testthat::expect_error(check_type(-(1:8), Positive), "positions 1, 2, 3, 4, 5 and 3 more")
  testthat::expect_identical(safe_parse(1, Positive)$issues$index, list())
})

testthat::test_that("data frame issues report row numbers", {
  Patients <- create_dataframe_type(list(age = Positive, name = NonNA(String)))
  df <- data.frame(age = c(1, -2, -5), name = c("a", NA, "c"))
  res <- safe_parse(df, Patients)
  testthat::expect_equal(res$issues$index, list(c(2L, 3L), 2L))
  testthat::expect_match(res$issues$message[1], "\\(at rows 2, 3\\)$")
  testthat::expect_error(check_type(df, Patients), "Type error in 'name'.*\\(at row 2\\)")

  dups <- safe_parse(data.frame(a = c(1, 1)), Unique(DataFrame))
  testthat::expect_match(dups$issues$message, "\\(at row 2\\)$")
})

testthat::test_that("check_type throws a classed condition", {
  err <- tryCatch(check_type("a", Numeric), error = function(e) e)
  testthat::expect_s3_class(err, "sicher_type_error")
  testthat::expect_equal(err$issues[[1]]$expected, "numeric")
})

testthat::test_that("errors thrown by custom checkers become issues", {
  Picky <- create_type("picky", function(x) stop("picky says no"))
  res <- safe_parse(1, Picky)
  testthat::expect_false(res$success)
  testthat::expect_match(res$issues$message, "picky says no")
})

testthat::test_that("sicher.mode = 'warn' warns and keeps the value", {
  withr::local_options(sicher.mode = "warn")
  testthat::expect_warning(w %:% Numeric %<-% "a", "Expected numeric")
  testthat::expect_equal(w, "a")
  testthat::expect_warning(w <- TRUE, "got bool")
  testthat::expect_true(w)
  testthat::expect_silent(w <- 5)

  f <- typed_function(function(x) x, params = list(x = Numeric))
  testthat::expect_warning(res <- f("a"), "Expected numeric")
  testthat::expect_equal(res, "a")
})

testthat::test_that("sicher.mode = 'off' skips typed function checks", {
  withr::local_options(sicher.mode = "off")
  f <- typed_function(function(x) x, params = list(x = Numeric), .return = Numeric)
  testthat::expect_equal(f("a"), "a")

  # Arguments are passed through unevaluated, as with a plain call
  g <- typed_function(function(x, y) if (missing(y)) x else y, params = list(x = Numeric))
  testthat::expect_equal(g("a"), "a")
  h <- typed_function(function(x, y) x, params = list(x = Numeric))
  testthat::expect_equal(h(1, stop("not forced")), 1)
})

testthat::test_that("sicher.mode = 'off' skips checks on existing typed variables", {
  v %:% Numeric %<-% 1
  withr::with_options(list(sicher.mode = "off"), v <- "a")
  testthat::expect_equal(v, "a")
  testthat::expect_error(v <- "b", "Expected numeric")
})

testthat::test_that("readonly is still enforced in warn mode", {
  withr::local_options(sicher.mode = "warn")
  ro %:% Readonly(Numeric) %<-% 1
  testthat::expect_error(ro <- 2, "readonly")
})

testthat::test_that("type_of returns the declared type", {
  age %:% Numeric %<-% 30
  testthat::expect_identical(type_of(age), Numeric)

  plain <- 1
  testthat::expect_null(type_of(plain))

  ro %:% Readonly(String) %<-% "a"
  testthat::expect_s3_class(type_of(ro), "sicher_readonly")
  testthat::expect_output(print(type_of(ro)), "readonly<string>")

  other %:% type_of(age) %<-% 1
  testthat::expect_error(other <- "a", "Expected numeric")

  testthat::expect_error(type_of(does_not_exist_xyz), "not found")
  testthat::expect_error(type_of("age"), "variable name")
})

testthat::test_that("type_of finds variables in enclosing environments", {
  outer %:% String %<-% "a"
  f <- function() type_of(outer)
  testthat::expect_identical(f(), String)
})

testthat::test_that("untype turns a typed variable back into a plain one", {
  count %:% Integer %<-% 1L
  testthat::expect_equal(untype(count), 1L)
  testthat::expect_false(bindingIsActive("count", environment()))
  count <- "anything"
  testthat::expect_equal(count, "anything")

  plain <- 2
  testthat::expect_equal(untype(plain), 2)
})
