# Object helpers, JSON Schema export and JSON validation

User <- create_list_type(list(
  id    = Integer,
  name  = String,
  email = Optional(String),
  bio   = Nullable(String),
  role  = Default(String, "member")
))

testthat::test_that("Pick and Omit select fields", {
  Preview <- Pick(User, "id", "name")
  testthat::expect_equal(names(attr(Preview, "sicher_spec")), c("id", "name"))
  testthat::expect_true(check_type(list(id = 1L, name = "a"), Preview))
  testthat::expect_error(check_type(list(id = 1L, name = "a", email = "e"), Preview), "Unexpected field")
  testthat::expect_equal(names(attr(Pick(User, c("id", "name")), "sicher_spec")), c("id", "name"))

  NewUser <- Omit(User, "id")
  testthat::expect_false("id" %in% names(attr(NewUser, "sicher_spec")))

  testthat::expect_error(Pick(User, "nope"), "unknown field\\(s\\) nope")
  testthat::expect_error(Omit(User), "field names")
  testthat::expect_error(Pick(Numeric, "a"), "create_list_type")
})

testthat::test_that("Partial and Required change optionality", {
  Patch <- Partial(User)
  testthat::expect_true(check_type(list(), Patch))
  testthat::expect_true(check_type(list(name = "a"), Patch))
  testthat::expect_error(check_type(list(name = 1), Patch), "Expected string \\| null")

  Full <- Required(User)
  testthat::expect_error(check_type(list(id = 1L, name = "a", bio = NULL), Full), "'email': Missing")
  full_spec <- attr(Full, "sicher_spec")
  testthat::expect_identical(full_spec$email, String)
  testthat::expect_identical(full_spec$role, String)
  # Nullable fields stay nullable
  testthat::expect_true(isTRUE(attr(full_spec$bio, "sicher_nullable")))

  multi <- attr(Required(create_list_type(list(x = Optional(String | Numeric)))), "sicher_spec")$x
  testthat::expect_equal(multi$name, "string | numeric")
})

testthat::test_that("KeyOf returns an enum of field names", {
  Field <- KeyOf(User)
  testthat::expect_true(check_type("email", Field))
  testthat::expect_error(check_type("password", Field), "Expected enum")
  testthat::expect_error(KeyOf(create_list_type(structure(list(), names = character(0)))), "at least one")
})

testthat::test_that("object helpers keep extra and kind", {
  Loose <- create_list_type(list(a = Numeric, b = Numeric), extra = "allow")
  testthat::expect_true(check_type(list(a = 1, z = 2), Pick(Loose, "a")))

  Table <- create_dataframe_type(list(a = Numeric, b = String))
  Narrow <- Pick(Table, "a")
  testthat::expect_true(check_type(data.frame(a = 1), Narrow))
  testthat::expect_false(is_type(list(a = 1), Narrow))
})

testthat::test_that("extend works on data frame types and keeps extra", {
  Table <- create_dataframe_type(list(a = Numeric), extra = "drop")
  Wider <- extend(Table, list(b = String))
  out <- parse_as(data.frame(a = 1, b = "x", z = TRUE), Wider)
  testthat::expect_equal(names(out), c("a", "b"))
})

testthat::test_that("to_json_schema describes list types", {
  schema <- to_json_schema(User)
  testthat::expect_equal(schema$`$schema`, "https://json-schema.org/draft/2020-12/schema")
  testthat::expect_equal(schema$type, "object")
  testthat::expect_equal(unlist(schema$required), c("id", "name", "bio"))
  testthat::expect_false(schema$additionalProperties)
  testthat::expect_equal(schema$properties$role$default, "member")
  testthat::expect_equal(schema$properties$id$anyOf[[1]]$type, "integer")
})

testthat::test_that("to_json_schema handles scalars, sizes and constraints", {
  testthat::expect_equal(schema_of(Scalar(Numeric)), list(type = "number"))
  testthat::expect_equal(schema_of(Numeric[1]), list(type = "number"))
  testthat::expect_equal(schema_of(Numeric[3])$maxItems, 3L)
  testthat::expect_equal(schema_of(Scalar(Between(0, 1)))$maximum, 1)
  testthat::expect_equal(schema_of(Scalar(Positive))$exclusiveMinimum, 0)
  testthat::expect_equal(schema_of(Scalar(Matches("^a")))$pattern, "^a")
  testthat::expect_equal(schema_of(Literal("a")), list(const = "a"))
  testthat::expect_equal(schema_of(Literal("a", "b"))$enum, list("a", "b"))
  testthat::expect_equal(schema_of(MinLength(String, 2))$minItems, 2L)
  testthat::expect_true(schema_of(Unique(Numeric))$anyOf[[2]]$uniqueItems)
  testthat::expect_equal(schema_of(Optional(Scalar(String)))$anyOf[[2]], list(type = "null"))
  testthat::expect_equal(schema_of(ListOf(Scalar(String)))$items, list(type = "string"))
  testthat::expect_equal(schema_of(Tuple(Scalar(String), Scalar(Numeric)))$maxItems, 2L)
  testthat::expect_equal(schema_of(Record(Scalar(Numeric)))$additionalProperties, list(type = "number"))
  testthat::expect_length(schema_of(Scalar(Numeric) & Scalar(Positive))$allOf, 2)
  testthat::expect_true(schema_of(Readonly(Scalar(String)))$readOnly)
  testthat::expect_equal(schema_of(Scalar(Date))$format, "date")
  testthat::expect_equal(schema_of(Scalar(Factor(c("a", "b"))))$enum, list("a", "b"))
  testthat::expect_equal(schema_of(Scalar(Refine(Numeric, function(x) TRUE))), list(type = "number"))

  Shape <- DiscriminatedUnion(
    "kind",
    circle = create_list_type(list(radius = Scalar(Numeric)))
  )
  testthat::expect_equal(schema_of(Shape)$oneOf[[1]]$properties$kind, list(const = "circle"))

  df_schema <- schema_of(create_dataframe_type(list(x = Integer)))
  testthat::expect_equal(df_schema$items$properties$x, list(type = "integer"))
})

testthat::test_that("to_json_schema rejects types without a JSON equivalent", {
  testthat::expect_error(to_json_schema(Function), "cannot be represented")
  testthat::expect_error(to_json_schema(create_type("custom", is.numeric)), "cannot be represented")
  testthat::expect_error(to_json_schema(Lazy(function() Numeric)), "cannot be represented")
})

testthat::test_that("to_json_schema can return JSON text", {
  testthat::skip_if_not_installed("jsonlite")
  json <- to_json_schema(User, json = TRUE)
  testthat::expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  testthat::expect_equal(parsed$type, "object")
  testthat::expect_equal(unlist(parsed$required), c("id", "name", "bio"))
})

testthat::test_that("validate_json parses and checks JSON", {
  testthat::skip_if_not_installed("jsonlite")
  Order <- create_list_type(list(
    id = Scalar(Integer),
    tags = String,
    items = ListOf(create_list_type(list(sku = String, qty = Whole)))
  ))
  json <- '{"id": 1, "tags": ["a", "b"], "items": [{"sku": "A1", "qty": 2}]}'
  out <- validate_json(json, Order)
  testthat::expect_equal(out$tags, c("a", "b"))
  testthat::expect_equal(out$items[[1]]$qty, 2L)

  bad <- '{"id": "x", "tags": ["a"], "items": [{"sku": 1}]}'
  testthat::expect_error(validate_json(bad, Order), "3 type errors found")
  res <- validate_json(bad, Order, safe = TRUE)
  testthat::expect_false(res$success)
  testthat::expect_equal(res$issues$path, c("id", "items[[1]]$sku", "items[[1]]$qty"))

  # jsonlite reads an empty array as list(), which is not a String
  testthat::expect_false(validate_json('{"id": 1, "tags": [], "items": []}', Order, safe = TRUE)$success)
})
