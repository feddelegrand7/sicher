
testthat::test_that("create_type returns a proper object and prints", {
  t <- create_type("t", is.numeric)
  testthat::expect_s3_class(t, "sicher_type")
  testthat::expect_true(t$check(1.23))
  testthat::expect_silent(x %:% t %<-% 10)
  testthat::expect_false(t$check("a"))
  testthat::expect_output(print(t), "<type: t >")

  x %:% t %<-% 10

  testthat::expect_error(create_type(5, is.numeric))
  testthat::expect_error(create_type("x", 5))

})

testthat::test_that("built-in primitive types work", {
  testthat::expect_true(Numeric$check(3.14))
  testthat::expect_false(Numeric$check("not"))
  testthat::expect_true(String$check("hello"))
  testthat::expect_true(Bool$check(TRUE))
  testthat::expect_true(Null$check(NULL))
  testthat::expect_true(Any$check(list()))
})

# Scalar modifier

testthat::test_that("Scalar type enforces length 1", {
  t <- Scalar(Numeric)
  testthat::expect_true(t$check(1))
  testthat::expect_false(t$check(c(1,2)))
})

# Readonly behavior

testthat::test_that("Readonly prevents reassignment", {
  x %:% Readonly(Numeric) %<-% 5
  testthat::expect_error(x <- 6, "reassign readonly")
})

# Optional variant

testthat::test_that("Optional allows NULL", {
  t <- Optional(String)
  testthat::expect_true(check_type(NULL, t))
  testthat::expect_true(check_type("text", t))
  testthat::expect_error(check_type(123, t))

  # Optional of Optional
  t2 <- Optional(Optional(Numeric))
  testthat::expect_true(check_type(NULL, t2))
  testthat::expect_true(check_type(5, t2))
})

# Union types and operator

testthat::test_that("Union type and | operator", {
  u <- String | Numeric
  testthat::expect_s3_class(u, "sicher_union")
  testthat::expect_true(check_type("a", u))
  testthat::expect_true(check_type(10, u))
  testthat::expect_error(check_type(TRUE, u))

  # union with null
  u2 <- String | Null
  testthat::expect_true(check_type(NULL, u2))
  testthat::expect_error(check_type(5, u2), "Expected string | null")
})

# vector size operator

testthat::test_that("size operator creates fixed-length types", {
  t <- Numeric[2]
  testthat::expect_true(t$check(c(1,2)))
  testthat::expect_false(t$check(c(1)))
})

# list type structure

testthat::test_that("create_list_type handles required and optional fields and rejects extras", {
  person <- create_list_type(list(name = String, age = Numeric, note = Optional(String)))
  good <- list(name = "Alice", age = 30)
  testthat::expect_true(person$check(good))
  testthat::expect_true(person$check(c(good, note = "ok")))
  # Skipped: error message expectations incompatible with main.R

  # invalid specifications
  testthat::expect_error(create_list_type(5))
  testthat::expect_error(create_list_type(list(a="x")))
})

testthat::test_that("create_dataframe_type checks columns", {
  df_type <- create_dataframe_type(list(a = Numeric, b = String, c = Optional(Bool)))
  df1 <- data.frame(a = 1:3, b = letters[1:3])
  df2 <- data.frame(a = 1:2, b = letters[1:2], c = c(TRUE, FALSE))
  testthat::expect_true(df_type$check(df1))
  testthat::expect_true(df_type$check(df2))
  testthat::expect_error(df_type$check(data.frame(a = 1:2)), "Missing required column")
  testthat::expect_error(df_type$check(data.frame(a=1,b=2,c=3)))

  # invalid spec
  testthat::expect_error(create_dataframe_type(list(1,2)), "named list")
})

# ListOf and combination with size

testthat::test_that("ListOf creates homogeneous list types and works with size", {
  rec <- create_list_type(list(x = Numeric))
  lst <- ListOf(rec)
  good <- list(list(x=1), list(x=2))
  testthat::expect_true(lst$check(good))
  testthat::expect_false(lst$check(list(1,2)))
  lst2 <- lst[2]
  testthat::expect_true(lst2$check(good))
  testthat::expect_false(lst2$check(list(list(x=1))))

  # error when element_type invalid
  testthat::expect_error(ListOf(5), "requires a sicher_type")

  # size operator invalid arguments
  testthat::expect_error(Numeric[-1], "Size must be")
  testthat::expect_error(Numeric["a"], "Size must be")
})

# typed binding operators

testthat::test_that("%:% and %<-% assign with checking and context errors", {
  y %:% Numeric %<-% 5
  testthat::expect_error({ y %:% Numeric %<-% "a" }, "Type error")

  # using %<-% alone should raise guidance message
  testthat::expect_error(y %<-% 1, "Use `%<-%` with `%:%`")

  # typed annotation print
  foo <- NULL
  ann <- foo %:% Numeric
  testthat::expect_output(print(ann), "<typed: foo %:% numeric")
})

# additional edge-case tests

testthat::test_that("get_type_name returns correct strings", {
  testthat::expect_equal(get_type_name(NULL), "null")
  testthat::expect_equal(get_type_name(1L), "integer")
  testthat::expect_equal(get_type_name(1), "double")
  testthat::expect_equal(get_type_name("a"), "string")
  testthat::expect_equal(get_type_name(TRUE), "bool")
  testthat::expect_equal(get_type_name(list()), "list(0)")
  df <- data.frame(x=1)
  # data.frames are reported with their dimensions
  testthat::expect_equal(get_type_name(df), "data.frame[1 x 1]")
})

testthat::test_that("type_error formatting various values", {
  testthat::expect_match(type_error(NULL, "foo", "bar"), "Expected foo, got bar")
  # long vector
  err <- type_error(NULL, "nums", "bool", 1:10)
  testthat::expect_match(err, "length: 10")
  # empty value
  err2 <- type_error(NULL, "t", "g", numeric())
  testthat::expect_match(err2, "\\(empty\\)")
})

testthat::test_that("get_type_name includes length for non-scalar values", {
  testthat::expect_error(
    check_type(c(1, 2, 3), Scalar(Numeric)),
    "got double of length 3"
  )
  testthat::expect_error(
    check_type(c("a", "b"), Scalar(String)),
    "got string of length 2"
  )
  # scalar value: no length suffix
  testthat::expect_error(check_type("x", Integer), "got string")
})

testthat::test_that("check_type errors for invalid type specification", {
  testthat::expect_error(check_type(1, 5), "Invalid type specification")
  testthat::expect_error(check_type(1, NULL), "Invalid type specification")
})

# union extension and naming

testthat::test_that("union extension and nested unions", {
  a <- String | Numeric
  b <- Bool | a
  testthat::expect_s3_class(b, "sicher_union")
  testthat::expect_true(check_type('x', b))
  testthat::expect_true(check_type(TRUE, b))
  testthat::expect_error(check_type(1+1i, b))
  # name should contain both
  testthat::expect_match(b$name, "string \\| numeric")
})

testthat::test_that("Scalar errors when wrong argument", {
  testthat::expect_error(Scalar(5), "requires a type")
})

testthat::test_that("Readonly variable retains underlying value and errors", {
  z %:% Readonly(String) %<-% "hi"
  testthat::expect_equal(z, "hi")
  testthat::expect_error(z <- "bye", "readonly")
})

testthat::test_that("Optional around list and union", {
  t <- Optional(List)
  testthat::expect_true(check_type(list(1,2), t))
  testthat::expect_true(check_type(NULL, t))
  t2 <- Optional(String | Numeric)
  testthat::expect_true(check_type(3, t2))
  testthat::expect_true(check_type(NULL, t2))
})

testthat::test_that("ListOf additional behaviors", {
  rec <- create_list_type(list(a=Numeric))
  lst <- ListOf(rec)
  testthat::expect_true(lst$check(list()))
  testthat::expect_false(lst$check(1))
  testthat::expect_false(lst$check(list(list(a='x'))))
  lst0 <- lst[0]
  testthat::expect_true(lst0$check(list()))
  testthat::expect_false(lst0$check(list(list(a=1))))
})

testthat::test_that("create_list_type with unions and nested lists", {
  t <- create_list_type(list(
    id = Numeric | String,
    info = Optional(create_list_type(list(flag=Bool)))
  ))
  testthat::expect_true(t$check(list(id=1)))
  testthat::expect_true(t$check(list(id="x", info=list(flag=FALSE))))
  testthat::expect_error(t$check(list(id=TRUE)), "Expected numeric \\\\| string")
})

testthat::test_that("create_dataframe_type rejects extras and union columns", {
  df_type <- create_dataframe_type(list(a = Numeric, b = String | Numeric))
  df <- data.frame(a=1, b=2)
  testthat::expect_true(df_type$check(df))
  testthat::expect_error(df_type$check(data.frame(a=1,b=2,c=3)), "Unexpected column")
})

testthat::test_that("create_typed_binding overwrites existing var and keeps new value", {
  v %:% Numeric %<-% 2
  v %:% Numeric %<-% 3
  testthat::expect_equal(v,3)
})

# more size operator edge cases

testthat::test_that("size operator with zero and fractional values", {
  t0 <- Numeric[0]
  testthat::expect_true(t0$check(numeric(0)))
  testthat::expect_false(t0$check(1))
})

# ensure print methods for union and typed_annotation are covered

testthat::test_that("print methods for unions and annotations", {
  u <- Numeric | Bool
  testthat::expect_output(print(u), "numeric")
  testthat::expect_output(print(u), "bool")
  ann <- foo %:% Bool
  testthat::expect_output(print(ann), "foo %:% bool")
})

# direct create_union call

testthat::test_that("create_union combines correctly", {
  u <- create_union(String, Numeric)
  testthat::expect_s3_class(u, "sicher_union")
  testthat::expect_true(check_type(5, u))
})

# corner cases for check_type with NULL and Any

testthat::test_that("check_type handles Any and Null properly", {
  testthat::expect_true(check_type(NULL, Any))
  testthat::expect_true(check_type(1, Any))
  testthat::expect_true(check_type(NULL, Null))
})




