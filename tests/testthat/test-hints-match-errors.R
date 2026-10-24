# Did-you-mean hints

testthat::test_that("misspelled fields and columns get a suggestion", {
  Person <- create_list_type(list(name = String, age = Numeric))
  testthat::expect_error(
    check_type(list(nmae = "x", age = 1), Person),
    "Unexpected field \\(valid fields: name, age\\)\\. Did you mean 'name'\\?"
  )
  # A field that is already present is not suggested
  res <- safe_parse(list(name = "x", age = 1, nmae = 2), Person)
  testthat::expect_false(grepl("Did you mean", res$issues$message))

  Ages <- create_dataframe_type(list(age = Numeric))
  testthat::expect_error(check_type(data.frame(aeg = 1), Ages), "Did you mean 'age'\\?")
})

testthat::test_that("misspelled Enum and Literal values get a suggestion", {
  Color <- Enum("red", "green", "blue")
  testthat::expect_error(check_type("gren", Color), "got string\\. Did you mean \"green\"\\?")
  testthat::expect_error(check_type("RED", Color), "Did you mean \"red\"\\?")
  testthat::expect_error(
    check_type(c("gren", "blu", "red"), Color),
    "Did you mean \"green\" instead of \"gren\", \"blue\" instead of \"blu\"\\?"
  )
  testthat::expect_error(check_type("gren", Scalar(Color)), "scalar<.*Did you mean \"green\"")
  testthat::expect_error(check_type("lft", Literal("left", "right")), "Did you mean \"left\"\\?")

  far <- safe_parse("purple", Color)
  testthat::expect_false(grepl("Did you mean", far$issues$message))
  testthat::expect_equal(far$issues$code, "invalid_type")
  testthat::expect_false(grepl("Did you mean", safe_parse(5, Enum(1, 2))$issues$message))
})

testthat::test_that("misspelled discriminator values and keys get a suggestion", {
  Shape <- DiscriminatedUnion("kind", circle = create_list_type(list(r = Numeric)))
  testthat::expect_error(check_type(list(kind = "circel", r = 1), Shape), "Did you mean \"circle\"\\?")
  testthat::expect_error(
    check_type(list(kidn = "circle", r = 1), Shape),
    "Found field 'kidn'; did you mean 'kind'\\?"
  )
})

testthat::test_that("suggest_name only suggests close matches", {
  testthat::expect_equal(suggest_name("nmae", c("name", "age")), "name")
  testthat::expect_equal(suggest_name("Name", c("name", "age")), "name")
  testthat::expect_null(suggest_name("xyz", c("name", "age")))
  testthat::expect_null(suggest_name("ab", c("xy")))
  testthat::expect_null(suggest_name("cat", c("bat", "hat")))
})

# match_type

testthat::test_that("match_type dispatches on DiscriminatedUnion tags", {
  Shape <- DiscriminatedUnion(
    "kind",
    circle = create_list_type(list(radius = Numeric)),
    square = create_list_type(list(side = Numeric))
  )
  area <- match_type(Shape,
    circle = function(s) pi * s$radius^2,
    square = function(s, scale = 1) s$side^2 * scale
  )
  testthat::expect_equal(area(list(kind = "square", side = 3)), 9)
  testthat::expect_equal(area(list(kind = "square", side = 3), scale = 2), 18)
  testthat::expect_error(area(list(kind = "hexagon")), "Invalid discriminator value")
})

testthat::test_that("match_type checks handlers when the matcher is created", {
  Shape <- DiscriminatedUnion(
    "kind",
    circle = create_list_type(list(radius = Numeric)),
    square = create_list_type(list(side = Numeric))
  )
  testthat::expect_error(
    match_type(Shape, circle = identity),
    "not exhaustive: no handler for 'square'"
  )
  testthat::expect_error(
    match_type(Shape, circle = identity, sqaure = identity),
    "does not have: 'sqaure' \\(did you mean 'square'\\?\\)"
  )
  testthat::expect_error(
    match_type(Shape, circle = identity, triangle = identity, .default = identity),
    "'triangle'"
  )
  testthat::expect_error(match_type(Shape, identity, identity), "must be named")
  testthat::expect_error(match_type(Shape, circle = 1, square = identity), "must be a function")
  testthat::expect_error(match_type(Numeric, a = identity), "needs a DiscriminatedUnion")
})

testthat::test_that("match_type works with Enum, Literal, Factor and unions", {
  label <- match_type(Enum("active", "paused", "closed"),
    active = function(x) "Running",
    .default = function(x) "Not running"
  )
  testthat::expect_equal(label("active"), "Running")
  testthat::expect_equal(label("paused"), "Not running")
  testthat::expect_error(label(c("active", "paused")), "needs a single value")
  testthat::expect_error(label("stopped"), "Expected enum")

  code <- match_type(Literal(200, 404), `200` = function(x) "ok", `404` = function(x) "missing")
  testthat::expect_equal(code(404), "missing")

  lvl <- match_type(Factor(levels = c("lo", "hi")), lo = function(x) 0, hi = function(x) 1)
  testthat::expect_equal(lvl(factor("hi", levels = c("lo", "hi"))), 1)

  kind <- match_type(Integer | String | Null,
    integer = function(x) "int", string = function(x) "chr", null = function(x) "none"
  )
  testthat::expect_equal(c(kind(1L), kind("a"), kind(NULL)), c("int", "chr", "none"))
})

# field_errors and problem_json

testthat::test_that("field_errors groups messages by path", {
  Signup <- create_list_type(list(email = Matches("@"), age = Scalar(Between(18, 120))))
  res <- safe_parse(list(email = "nope", age = 12), Signup)
  errs <- field_errors(res)
  testthat::expect_named(errs, c("email", "age"))
  testthat::expect_match(errs$age, "outside \\[18, 120\\]")

  testthat::expect_length(field_errors(safe_parse(list(email = "a@b", age = 20), Signup)), 0)

  err <- tryCatch(check_type("a", Numeric), sicher_type_error = function(e) e)
  testthat::expect_equal(
    field_errors(err),
    structure(list("Expected numeric, got string"), names = "")
  )
  testthat::expect_error(field_errors(list()), "safe_parse\\(\\) result")
})

testthat::test_that("problem_json builds an RFC 9457 body", {
  Order <- create_list_type(list(
    id = Scalar(Integer),
    items = ListOf(create_list_type(list(sku = String, qty = Positive)))
  ))
  res <- safe_parse(list(id = "x", items = list(list(sku = "a", qty = -1))), Order)
  body <- problem_json(res, status = 422, instance = "/orders")

  testthat::expect_equal(body$status, 422L)
  testthat::expect_equal(body$type, "about:blank")
  testthat::expect_equal(body$instance, "/orders")
  testthat::expect_equal(body$detail, "2 validation errors")
  testthat::expect_equal(
    vapply(body$errors, `[[`, character(1), "pointer"),
    c("/id", "/items/0/qty")
  )
  testthat::expect_equal(body$errors[[2]]$code, "out_of_range")
  testthat::expect_error(problem_json(safe_parse(1L, Integer)), "no problems")
})

testthat::test_that("json_pointer escapes special characters", {
  testthat::expect_equal(json_pointer(list("a/b", "c~d", 3)), "/a~1b/c~0d/2")
  testthat::expect_equal(json_pointer(list()), "")
})
