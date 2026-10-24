# Check

testthat::test_that("Check reports problems at the fields they are about", {
  Booking <- Check(
    create_list_type(list(start = Date, end = Date, adults = Integer, kids = Integer)),
    function(x, ctx) {
      if (x$end < x$start) ctx$issue("must be after start", path = "end")
      if (x$adults == 0L && x$kids > 0L) ctx$issue("children need an adult", path = "kids")
    }
  )
  bad <- list(start = as.Date("2024-05-10"), end = as.Date("2024-05-01"), adults = 0L, kids = 2L)
  res <- safe_parse(bad, Booking)
  testthat::expect_equal(res$issues$path, c("end", "kids"))
  testthat::expect_equal(res$issues$message, c("must be after start", "children need an adult"))

  good <- list(start = as.Date("2024-05-01"), end = as.Date("2024-05-10"), adults = 1L, kids = 2L)
  testthat::expect_true(check_type(good, Booking))
})

testthat::test_that("Check runs only after the base type passes", {
  called <- FALSE
  T1 <- Check(create_list_type(list(a = Numeric)), function(x, ctx) called <<- TRUE)
  testthat::expect_error(check_type(list(a = "x"), T1), "Expected numeric")
  testthat::expect_false(called)
})

testthat::test_that("Check paths can be nested and errors in the rule are reported", {
  T1 <- Check(
    create_list_type(list(items = ListOf(Numeric))),
    function(x, ctx) ctx$issue("too big", path = list("items", 2))
  )
  testthat::expect_error(check_type(list(items = list(1, 2)), T1), "Type error in 'items\\[\\[2\\]\\]': too big")
  testthat::expect_error(check_type(1, Check(Numeric, function(x, ctx) ctx$issue("no"))), "Type error: no")
  testthat::expect_error(check_type(1, Check(Numeric, function(x, ctx) stop("boom"))), "failed with an error: boom")
  testthat::expect_error(Check(Numeric, "not a function"), "must be a function")
})

# Transform and Pipe

testthat::test_that("Transform converts values after checking them", {
  Email <- Transform(Scalar(String), function(x) tolower(trimws(x)))
  testthat::expect_equal(parse_as("  Ada@Example.COM ", Email), "ada@example.com")
  testthat::expect_error(parse_as(1, Email), "Expected scalar<string>")

  email %:% Email %<-% " HI "
  testthat::expect_equal(email, "hi")

  Len <- Transform(String, nchar, output = Integer)
  testthat::expect_equal(parse_as("abc", Len), 3L)
  testthat::expect_error(parse_as("abc", Transform(String, nchar, output = String)), "Expected string")
  testthat::expect_error(
    parse_as("abc", Transform(String, function(x) stop("nope"))),
    "transform of transform<string> failed: nope"
  )
})

testthat::test_that("Pipe passes the parsed value through each type", {
  Share <- Pipe(Coerce(Numeric), Transform(Numeric, function(x) x / 100), Between(0, 1))
  testthat::expect_equal(parse_as("42", Share), 0.42)
  testthat::expect_error(parse_as("420", Share), "outside \\[0, 1\\]")
  testthat::expect_error(parse_as("abc", Share), "cannot convert")
  testthat::expect_equal(Share$name, "coerce<numeric> |> transform<numeric> |> between[0, 1]")
  testthat::expect_error(Pipe(Numeric), "at least two types")
  testthat::expect_equal(parse_as("4", Coerce(Transform(Numeric, function(x) x * 2))), 8)
  testthat::expect_equal(parse_as("4", Coerce(Pipe(Numeric, Positive))), 4)

  Settings <- create_list_type(list(port = Transform(Integer, function(x) x + 1L)))
  testthat::expect_equal(parse_env(Settings, env = c(PORT = "80")), list(port = 81L))
  testthat::expect_error(Pipe(Numeric, 1), "Argument 2")
})

testthat::test_that("transformed fields work in list types, schemas and JSON", {
  Form <- create_list_type(list(
    name = Transform(Scalar(String), toupper),
    nick = Transform(Optional(String), toupper),
    tag = Default(Transform(Scalar(String), toupper), "NONE")
  ))
  testthat::expect_equal(parse_as(list(name = "ada"), Form), list(name = "ADA", tag = "NONE"))
  testthat::expect_equal(to_json(list(name = "A"), Form), "{\"name\":\"A\",\"tag\":\"NONE\"}")

  schema <- to_json_schema(Form)
  testthat::expect_equal(schema$properties$name, list(type = "string"))
  testthat::expect_equal(schema$required, list("name"))
  testthat::expect_equal(
    to_json_schema(Pipe(Coerce(Numeric), Between(0, 1)))$anyOf[[1]],
    list(type = "number")
  )
})

# HasMethods

testthat::test_that("HasMethods accepts objects with methods for the generics", {
  Model <- HasMethods("predict", "residuals")
  testthat::expect_true(is_type(lm(mpg ~ wt, data = mtcars), Model))
  testthat::expect_true(is_type(glm(am ~ wt, data = mtcars, family = binomial), Model))
  testthat::expect_error(
    check_type(list(), Model),
    "class 'list' has no predict\\(\\) or residuals\\(\\) method"
  )
  # Default methods do not count; implicit classes do
  testthat::expect_false(is_type(1, HasMethods("print")))
  testthat::expect_true(is_type(Sys.Date(), HasMethods("format")))
})

testthat::test_that("HasMethods finds S3 methods defined by the caller and R6-style fields", {
  summary.sicher_test_obj <- function(object, ...) "summary"
  testthat::expect_true(is_type(structure(1, class = "sicher_test_obj"), HasMethods("summary")))

  obj <- new.env()
  obj$predict <- function() 1
  testthat::expect_true(is_type(obj, HasMethods("predict")))
  testthat::expect_error(HasMethods(), "one or more generic")
  testthat::expect_error(HasMethods(1), "one or more generic")
})
