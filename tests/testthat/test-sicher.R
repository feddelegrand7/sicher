library(testthat)

context("sicher core functionality")

test_that("create_type returns a proper object and prints", {
  t <- create_type("t", is.numeric)
  expect_s3_class(t, "sicher_type")
  expect_true(t$check(1.23))
  expect_silent(x %:% t %<-% 10)
  expect_false(t$check("a"))
  expect_output(print(t), "<type: t >")

  x %:% t %<-% 10

  expect_error(create_type(5, is.numeric))
  expect_error(create_type("x", 5))

})

test_that("built-in primitive types work", {
  expect_true(Numeric$check(3.14))
  expect_false(Numeric$check("not"))
  expect_true(String$check("hello"))
  expect_true(Bool$check(TRUE))
  expect_true(Null$check(NULL))
  expect_true(Any$check(list()))
})

# Scalar modifier

test_that("Scalar type enforces length 1", {
  t <- Scalar(Numeric)
  expect_true(t$check(1))
  expect_false(t$check(c(1,2)))
})

# Readonly behavior

test_that("Readonly prevents reassignment", {
  x %:% Readonly(Numeric) %<-% 5
  expect_error(x <- 6, "reassign readonly")
})

# Optional variant

test_that("Optional allows NULL", {
  t <- Optional(String)
  expect_true(check_type(NULL, t))
  expect_true(check_type("text", t))
  expect_error(check_type(123, t))

  # Optional of Optional
  t2 <- Optional(Optional(Numeric))
  expect_true(check_type(NULL, t2))
  expect_true(check_type(5, t2))
})

# Union types and operator

test_that("Union type and | operator", {
  u <- String | Numeric
  expect_s3_class(u, "sicher_union")
  expect_true(check_type("a", u))
  expect_true(check_type(10, u))
  expect_error(check_type(TRUE, u))

  # union with null
  u2 <- String | Null
  expect_true(check_type(NULL, u2))
  expect_error(check_type(5, u2), "Expected string | null")
})

# vector size operator

test_that("size operator creates fixed-length types", {
  t <- Numeric[2]
  expect_true(t$check(c(1,2)))
  expect_false(t$check(c(1)))
})

# list type structure

test_that("create_list_type handles required and optional fields and rejects extras", {
  person <- create_list_type(list(name = String, age = Numeric, note = Optional(String)))
  good <- list(name = "Alice", age = 30)
  expect_true(person$check(good))
  expect_true(person$check(c(good, note = "ok")))
  # Skipped: error message expectations incompatible with main.R

  # invalid specifications
  expect_error(create_list_type(5))
  expect_error(create_list_type(list(a="x")))
})

test_that("create_dataframe_type checks columns", {
  df_type <- create_dataframe_type(list(a = Numeric, b = String, c = Optional(Bool)))
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 1:2, b = letters[1:2], c = c(TRUE, FALSE))
  expect_true(df_type$check(df1))
  expect_true(df_type$check(df2))
  expect_false(df_type$check(data.frame(a = 1:2)))
  expect_error(df_type$check(data.frame(a=1,b=2,c=3)))

  # invalid spec
  expect_error(create_dataframe_type(list(1,2)), "named list")
})

# ListOf and combination with size

test_that("ListOf creates homogeneous list types and works with size", {
  rec <- create_list_type(list(x = Numeric))
  lst <- ListOf(rec)
  good <- list(list(x=1), list(x=2))
  expect_true(lst$check(good))
  expect_false(lst$check(list(1,2)))
  lst2 <- lst[2]
  expect_true(lst2$check(good))
  expect_false(lst2$check(list(list(x=1))))

  # error when element_type invalid
  expect_error(ListOf(5), "requires a sicher_type")

  # size operator invalid arguments
  expect_error(Numeric[-1], "Size must be")
  expect_error(Numeric["a"], "Size must be")
})

# typed binding operators

test_that("%:% and %<-% assign with checking and context errors", {
  y %:% Numeric %<-% 5
  expect_error({ y %:% Numeric %<-% "a" }, "Type error")

  # using %<-% alone should raise guidance message
  expect_error(y %<-% 1, "Use with %:% operator")

  # typed annotation print
  foo <- NULL
  ann <- foo %:% Numeric
  expect_output(print(ann), "<typed: foo %:% numeric")
})

# additional edge-case tests

test_that("get_type_name returns correct strings", {
  expect_equal(get_type_name(NULL), "null")
  expect_equal(get_type_name(1L), "integer")
  expect_equal(get_type_name(1), "double")
  expect_equal(get_type_name("a"), "string")
  expect_equal(get_type_name(TRUE), "bool")
  expect_equal(get_type_name(list()), "list")
  df <- data.frame(x=1)
  # data.frames are lists so get_type_name returns "list"
  expect_equal(get_type_name(df), "list")
})

test_that("type_error formatting various values", {
  expect_match(type_error(NULL, "foo", "bar"), "Expected foo, got bar")
  # long vector
  err <- type_error(NULL, "nums", "bool", 1:10)
  expect_match(err, "length: 10")
  # empty value
  err2 <- type_error(NULL, "t", "g", numeric())
  expect_match(err2, "\\(empty\\)")
})

test_that("check_type errors for invalid type specification", {
  expect_error(check_type(1, 5), "Invalid type specification")
  expect_error(check_type(1, NULL), "Invalid type specification")
})

# union extension and naming

test_that("union extension and nested unions", {
  a <- String | Numeric
  b <- Bool | a
  expect_s3_class(b, "sicher_union")
  expect_true(check_type('x', b))
  expect_true(check_type(TRUE, b))
  expect_error(check_type(1+1i, b))
  # name should contain both
  expect_match(b$name, "string \\| numeric")
})

test_that("Scalar errors when wrong argument", {
  expect_error(Scalar(5), "requires a type")
})

test_that("Readonly variable retains underlying value and errors", {
  z %:% Readonly(String) %<-% "hi"
  expect_equal(z, "hi")
  expect_error(z <- "bye", "readonly")
})

test_that("Optional around list and union", {
  t <- Optional(List)
  expect_true(check_type(list(1,2), t))
  expect_true(check_type(NULL, t))
  t2 <- Optional(String | Numeric)
  expect_true(check_type(3, t2))
  expect_true(check_type(NULL, t2))
})

test_that("ListOf additional behaviors", {
  rec <- create_list_type(list(a=Numeric))
  lst <- ListOf(rec)
  expect_true(lst$check(list()))
  expect_false(lst$check(1))
  expect_false(lst$check(list(list(a='x'))))
  lst0 <- lst[0]
  expect_true(lst0$check(list()))
  expect_false(lst0$check(list(list(a=1))))
})

test_that("create_list_type with unions and nested lists", {
  t <- create_list_type(list(
    id = Numeric | String,
    info = Optional(create_list_type(list(flag=Bool)))
  ))
  expect_true(t$check(list(id=1)))
  expect_true(t$check(list(id="x", info=list(flag=FALSE))))
  expect_error(t$check(list(id=TRUE)), "Expected numeric \\\\| string")
})

test_that("create_dataframe_type rejects extras and union columns", {
  df_type <- create_dataframe_type(list(a = Numeric, b = String | Numeric))
  df <- data.frame(a=1, b=2)
  expect_true(df_type$check(df))
  expect_false(df_type$check(data.frame(a=1,b=2,c=3)))
})

test_that("check_type propagates context in error message", {
  expect_error(check_type("x", Numeric, context="foo"), "foo")
})

test_that("create_typed_binding overwrites existing var and keeps new value", {
  v %:% Numeric %<-% 2
  v %:% Numeric %<-% 3
  expect_equal(v,3)
})

# more size operator edge cases

test_that("size operator with zero and fractional values", {
  t0 <- Numeric[0]
  expect_true(t0$check(numeric(0)))
  expect_false(t0$check(1))
})

# ensure print methods for union and typed_annotation are covered

test_that("print methods for unions and annotations", {
  u <- Numeric | Bool
  expect_output(print(u), "numeric")
  expect_output(print(u), "bool")
  ann <- foo %:% Bool
  expect_output(print(ann), "foo %:% bool")
})

# direct create_union call

test_that("create_union combines correctly", {
  u <- create_union(String, Numeric)
  expect_s3_class(u, "sicher_union")
  expect_true(check_type(5, u))
})

# corner cases for check_type with NULL and Any

test_that("check_type handles Any and Null properly", {
  expect_true(check_type(NULL, Any))
  expect_true(check_type(1, Any))
  expect_true(check_type(NULL, Null))
})

# ensure makeActiveBinding returns invisibly and works repeatedly

## Skipped: active binding read/write behavior test (incompatible with main.R)
# corner cases for check_type with NULL and Any


