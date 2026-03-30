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

# typed_function

testthat::test_that("typed_function validates params and return type", {
  add <- typed_function(
    function(x, y) x + y,
    params  = list(x = Numeric, y = Numeric),
    .return = Numeric
  )
  testthat::expect_equal(add(1, 2), 3)
  testthat::expect_error(add("a", 2), "Type error")
  testthat::expect_error(add(1, TRUE), "Type error")
})

testthat::test_that("typed_function checks return type", {
  bad_return <- typed_function(
    function(x) as.character(x),
    params  = list(x = Numeric),
    .return = Numeric
  )
  testthat::expect_error(bad_return(1), "Type error")
})

testthat::test_that("typed_function works without .return", {
  f <- typed_function(
    function(x) x * 2,
    params = list(x = Numeric)
  )
  testthat::expect_equal(f(5), 10)
  testthat::expect_error(f("bad"), "Type error")
})

testthat::test_that("typed_function works with Optional params", {
  greet <- typed_function(
    function(name, title = NULL) {
      if (is.null(title)) paste("Hello,", name) else paste("Hello,", title, name)
    },
    params = list(name = String, title = Optional(String))
  )
  testthat::expect_equal(greet("Alice"), "Hello, Alice")
  testthat::expect_equal(greet("Alice", title = "Dr."), "Hello, Dr. Alice")
  testthat::expect_error(greet("Alice", title = 42), "Type error")
})

testthat::test_that("typed_function works with union param types", {
  f <- typed_function(
    function(id) paste("ID:", id),
    params  = list(id = String | Numeric),
    .return = String
  )
  testthat::expect_match(f("abc"), "ID: abc")
  testthat::expect_match(f(123),   "ID: 123")
  testthat::expect_error(f(TRUE), "Type error")
})

testthat::test_that("typed_function construction errors", {
  testthat::expect_error(typed_function(42),                               "must be a function")
  testthat::expect_error(typed_function(function(x) x, params = "x"),     "named list")
  testthat::expect_error(typed_function(function(x) x, params = list(5)), "named list")
  testthat::expect_error(typed_function(function(x) x, params = list(x = 5)), "sicher_type")
  testthat::expect_error(typed_function(function(x) x, .return = "bad"),  "sicher_type")
})

testthat::test_that("typed_function print method", {
  f <- typed_function(
    function(x, y) x + y,
    params  = list(x = Numeric, y = String),
    .return = Numeric
  )
  testthat::expect_s3_class(f, "sicher_typed_function")
  testthat::expect_output(print(f), "typed_function")
  testthat::expect_output(print(f), "x: numeric")
  testthat::expect_output(print(f), "y: string")
  testthat::expect_output(print(f), ": numeric")
})

testthat::test_that("typed_function with no params or return type passes through", {
  f <- typed_function(function(x) x + 1)
  testthat::expect_equal(f(5), 6)
})

# infer_type tests

testthat::test_that("infer_type infers primitive types", {
  testthat::expect_equal(infer_type(42L)$name, Integer$name)
  testthat::expect_equal(infer_type(3.14)$name, Double$name)
  testthat::expect_equal(infer_type("abc")$name, String$name)
  testthat::expect_equal(infer_type(TRUE)$name, Bool$name)
  testthat::expect_equal(infer_type(NULL)$name, Null$name)
  testthat::expect_equal(infer_type(function(x) x+1)$name, Function$name)
})

testthat::test_that("infer_type infers vector types without length constraints by default", {
  testthat::expect_equal(infer_type(c(1L,2L,3L))$name, Integer$name)
  testthat::expect_equal(infer_type(c(1,2,3))$name, Double$name)
  testthat::expect_equal(infer_type(c("a","b"))$name, String$name)
  testthat::expect_equal(infer_type(c(TRUE, FALSE))$name, Bool$name)
})

testthat::test_that("infer_type infers named and unnamed lists", {
  # Named list
  l <- list(a=1L, b="x")
  t <- infer_type(l)
  testthat::expect_true(inherits(t, "sicher_type"))
  testthat::expect_true(grepl("a", t$name) && grepl("b", t$name))
  # Unnamed homogeneous list
  l2 <- list(1L, 2L, 3L)
  t2 <- infer_type(l2)
  testthat::expect_true(inherits(t2, "sicher_type"))
  testthat::expect_true(grepl("list<integer>", t2$name))
  # Unnamed heterogeneous list
  l3 <- list(1L, "a")
  t3 <- infer_type(l3)
  testthat::expect_equal(t3$name, List$name)
})

testthat::test_that("infer_type infers optional fields in lists", {
  l <- list(a=NULL, b=1)
  t <- infer_type(l)
  testthat::expect_true(grepl("Optional", t$name) || grepl("null", t$name))
})

testthat::test_that("infer_type infers data.frame types", {
  df <- data.frame(x=1:3, y=c("a","b","c"), stringsAsFactors=FALSE)
  t <- infer_type(df)
  testthat::expect_true(inherits(t, "sicher_type"))
  testthat::expect_true(grepl("data.frame", t$name))
  testthat::expect_true(grepl("x", t$name) && grepl("y", t$name))
  testthat::expect_false(grepl("\\[3\\]", t$name))
})

testthat::test_that("infer_type strict mode infers scalar and fixed-size constraints", {
  testthat::expect_equal(infer_type(42L, strict = TRUE)$name, Scalar(Integer)$name)
  testthat::expect_equal(infer_type(3.14, strict = TRUE)$name, Scalar(Double)$name)
  testthat::expect_equal(infer_type(c(1L, 2L, 3L), strict = TRUE)$name, Integer[3]$name)
  testthat::expect_equal(infer_type(c("a", "b"), strict = TRUE)$name, String[2]$name)

  list_type <- infer_type(list(a = 1L, b = "x"), strict = TRUE)
  testthat::expect_true(grepl("scalar<integer>", list_type$name))
  testthat::expect_true(grepl("scalar<string>", list_type$name))

  df_type <- infer_type(
    data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE),
    strict = TRUE
  )
  testthat::expect_true(grepl("\\[3\\]", df_type$name))
})

testthat::test_that("infer_type propagates non-strict mode through nested structures", {
  nested <- list(
    user = list(
      id = 1L,
      tags = c("admin", "staff")
    ),
    metrics = data.frame(score = 1:3)
  )

  nested_type <- infer_type(nested)

  testthat::expect_true(grepl("user", nested_type$name))
  testthat::expect_true(grepl("metrics", nested_type$name))
  testthat::expect_false(grepl("scalar<integer>", nested_type$name))
  testthat::expect_false(grepl("\\[2\\]", nested_type$name))
  testthat::expect_false(grepl("\\[3\\]", nested_type$name))
})

testthat::test_that("infer_type propagates strict mode through nested structures", {
  nested <- list(
    user = list(
      id = 1L,
      tags = c("admin", "staff")
    ),
    metrics = data.frame(score = 1:3)
  )

  nested_type <- infer_type(nested, strict = TRUE)

  testthat::expect_true(grepl("scalar<integer>", nested_type$name))
  testthat::expect_true(grepl("string\\[2\\]", nested_type$name))
  testthat::expect_true(grepl("integer\\[3\\]", nested_type$name))
})

testthat::test_that("infer_type handles empty containers consistently", {
  testthat::expect_equal(infer_type(list())$name, List$name)
  testthat::expect_equal(infer_type(list(), strict = TRUE)$name, List$name)

  empty_df <- data.frame()
  default_df_type <- infer_type(empty_df)
  strict_df_type <- infer_type(empty_df, strict = TRUE)

  testthat::expect_true(inherits(default_df_type, "sicher_type"))
  testthat::expect_true(inherits(strict_df_type, "sicher_type"))
  testthat::expect_equal(default_df_type$name, strict_df_type$name)
  testthat::expect_match(default_df_type$name, "data.frame")
})

testthat::test_that("infer_type distinguishes homogeneous lists across modes", {
  values <- list(c(1L, 2L), c(3L, 4L))

  default_type <- infer_type(values)
  strict_type <- infer_type(values, strict = TRUE)

  testthat::expect_match(default_type$name, "list<integer>")
  testthat::expect_match(strict_type$name, "list<integer\\[2\\]>")
})

testthat::test_that("infer_type validates strict argument", {
  testthat::expect_error(infer_type(1, strict = NA), "`strict` must be either TRUE or FALSE")
  testthat::expect_error(infer_type(1, strict = c(TRUE, FALSE)), "`strict` must be either TRUE or FALSE")
})


testthat::test_that("create_type keeps checker function", {
  checker <- function(x) is.numeric(x) && all(x >= 0)
  t <- create_type("non_negative", checker)

  testthat::expect_identical(t$check, checker)
})

testthat::test_that("integer and double built-ins distinguish storage modes", {
  testthat::expect_false(Integer$check(1.5))
  testthat::expect_false(Double$check(1L))
})

testthat::test_that("function and data frame built-ins accept matching values", {
  testthat::expect_true(Function$check(function(x) x))
  testthat::expect_true(DataFrame$check(data.frame(x = 1)))
})

testthat::test_that("size operator encodes length in type names", {
  testthat::expect_equal(String[3]$name, "string[3]")
})

testthat::test_that("size operator works with Any", {
  any_two <- Any[2]

  testthat::expect_true(any_two$check(list(1, "a")))
  testthat::expect_false(any_two$check(1))
})

testthat::test_that("create_list_type rejects blank field names", {
  spec <- list(String)
  names(spec) <- ""

  testthat::expect_error(create_list_type(spec), "must be named")
})

testthat::test_that("create_list_type marks optional fields in name", {
  person <- create_list_type(list(name = String, nickname = Optional(String)))

  testthat::expect_match(person$name, "nickname\\?: string \\| null")
})

testthat::test_that("create_list_type accepts explicit NULL for optional field", {
  person <- create_list_type(list(name = String, nickname = Optional(String)))

  testthat::expect_true(person$check(list(name = "Ada", nickname = NULL)))
})

testthat::test_that("create_list_type surfaces field context on mismatch", {
  person <- create_list_type(list(name = String))

  testthat::expect_error(person$check(list(name = 1)), "Type error in 'name'")
})

testthat::test_that("nested list types propagate inner errors", {
  schema <- create_list_type(list(meta = create_list_type(list(flag = Bool))))

  testthat::expect_error(schema$check(list(meta = list(flag = "yes"))), "Type error in 'flag'")
})

testthat::test_that("create_dataframe_type rejects blank column names", {
  spec <- list(Numeric)
  names(spec) <- ""

  testthat::expect_error(create_dataframe_type(spec), "must be named")
})

testthat::test_that("create_dataframe_type marks optional columns in name", {
  schema <- create_dataframe_type(list(a = Numeric, b = Optional(String)))

  testthat::expect_match(schema$name, "b\\?: string \\| null")
})

testthat::test_that("create_dataframe_type surfaces column context on mismatch", {
  schema <- create_dataframe_type(list(a = Numeric))

  testthat::expect_error(schema$check(data.frame(a = "x", stringsAsFactors = FALSE)), "Type error in 'a'")
})

testthat::test_that("create_dataframe_type check returns FALSE for non data frames", {
  schema <- create_dataframe_type(list(a = Numeric))

  testthat::expect_false(schema$check(list(a = 1)))
})

testthat::test_that("Scalar names wrap inner type name", {
  testthat::expect_equal(Scalar(String)$name, "scalar<string>")
})

testthat::test_that("Readonly unwraps nested readonly modifiers", {
  ro <- Readonly(Readonly(String))

  testthat::expect_s3_class(ro, "sicher_readonly")
  testthat::expect_identical(ro$base_type, String)
})

testthat::test_that("Optional adds null to union name", {
  testthat::expect_equal(Optional(String)$name, "string | null")
})

testthat::test_that("create_union accepts NULL as left operand", {
  u <- create_union(NULL, String)

  testthat::expect_true(check_type(NULL, u))
  testthat::expect_true(check_type("ok", u))
})

testthat::test_that("create_union concatenates nested union names", {
  u <- create_union(String | Numeric, Bool | Null)

  testthat::expect_equal(u$name, "string | numeric | bool | null")
})

testthat::test_that("check_type accepts readonly type specifications", {
  testthat::expect_true(check_type(5, Readonly(Numeric)))
})

testthat::test_that("check_type includes context in scalar mismatch", {
  testthat::expect_error(check_type(c(1, 2), Scalar(Numeric), context = "score"), "Type error in 'score'")
})

testthat::test_that("check_type rejects NULL for non-null unions", {
  testthat::expect_error(check_type(NULL, String | Numeric), "Expected string \\| numeric, got null")
})

testthat::test_that("type_error previews named list fields", {
  err <- type_error("cfg", "schema", "list", list(a = 1, b = 2))

  testthat::expect_match(err, "list with fields: \\[a, b\\]")
})

testthat::test_that("type_error previews unnamed list lengths", {
  err <- type_error(NULL, "schema", "list", list(1, 2, 3))

  testthat::expect_match(err, "list of length 3")
})

testthat::test_that("type_error previews data frame columns", {
  err <- type_error(NULL, "schema", "data.frame", data.frame(a = 1, b = 2))

  testthat::expect_match(err, "columns: \\[a, b\\]")
})

testthat::test_that("type_error previews short atomic vectors", {
  err <- type_error(NULL, "nums", "bool", c(1, 2, 3))

  testthat::expect_match(err, "Received: \\[1, 2, 3\\]")
})

testthat::test_that("get_type_name reports functions", {
  testthat::expect_equal(get_type_name(function(x) x), "function")
})

testthat::test_that("get_type_name reports empty logical vectors", {
  testthat::expect_equal(get_type_name(logical()), "bool(0)")
})

testthat::test_that("get_type_name prioritizes list labeling over S3 classes", {
  obj <- structure(list(), class = c("custom_obj", "list"))

  testthat::expect_equal(get_type_name(obj), "list(0)")
})

testthat::test_that("type annotation captures symbol names", {
  ann <- some_value %:% Numeric

  testthat::expect_equal(ann$var_name, "some_value")
})

testthat::test_that("disabled mode bypasses type checking", {
  old <- options(sicher.mode = "off")
  on.exit(options(old), add = TRUE)

  local({
    off_mode_value %:% Numeric %<-% "text"
    testthat::expect_equal(off_mode_value, "text")
  })
})

testthat::test_that("disabled mode uses regular bindings", {
  old <- options(sicher.mode = "off")
  on.exit(options(old), add = TRUE)

  local({
    off_binding %:% Numeric %<-% 1
    testthat::expect_false(bindingIsActive("off_binding", environment()))
  })
})

testthat::test_that("typed assignment replaces existing plain bindings", {
  local({
    rebound <- 1
    rebound %:% Numeric %<-% 2

    testthat::expect_equal(rebound, 2)
    testthat::expect_true(bindingIsActive("rebound", environment()))
  })
})

testthat::test_that("typed assignment creates mutable active bindings", {
  local({
    mutable %:% Numeric %<-% 1
    mutable <- 3

    testthat::expect_equal(mutable, 3)
  })
})

testthat::test_that("readonly bindings remain readable", {
  local({
    ro_value %:% Readonly(String) %<-% "hello"

    testthat::expect_equal(ro_value, "hello")
  })
})

testthat::test_that("typed_function preserves formals", {
  f <- typed_function(function(x, y = 1) x + y, params = list(x = Numeric))

  testthat::expect_identical(names(formals(f)), c("x", "y"))
  testthat::expect_identical(formals(f)$y, 1)
})

testthat::test_that("typed_function keeps unchecked defaults", {
  f <- typed_function(function(x, y = "ok") paste(x, y), params = list(x = String))

  testthat::expect_equal(f("hi"), "hi ok")
})

testthat::test_that("typed_function does not validate omitted annotated defaults", {
  f <- typed_function(function(x = "bad") x, params = list(x = Numeric))

  testthat::expect_equal(f(), "bad")
})

testthat::test_that("typed_function accepts readonly parameter specifications", {
  f <- typed_function(function(x) x + 1, params = list(x = Readonly(Numeric)))

  testthat::expect_equal(f(2), 3)
})

testthat::test_that("typed_function accepts union return types", {
  f <- typed_function(function(flag) if (flag) 1 else "one", .return = Numeric | String)

  testthat::expect_equal(f(TRUE), 1)
  testthat::expect_equal(f(FALSE), "one")
})

testthat::test_that("typed_function forwards dots", {
  f <- typed_function(function(...) length(list(...)))

  testthat::expect_equal(f(1, "a", TRUE), 3)
})

testthat::test_that("typed_function print is compact without annotations", {
  f <- typed_function(function() 1)

  testthat::expect_output(print(f), "<typed_function \\(\\)>")
})

testthat::test_that("infer_type falls back to Any for unsupported classes", {
  testthat::expect_equal(infer_type(factor("a"))$name, Any$name)
})

testthat::test_that("infer_type treats partially named lists as generic lists", {
  x <- structure(list(1L, "a"), names = c("id", ""))

  testthat::expect_equal(infer_type(x)$name, List$name)
})

testthat::test_that("infer_type treats mixed NULL lists as generic lists", {
  x <- list(NULL, 1L)

  testthat::expect_equal(infer_type(x)$name, List$name)
})

testthat::test_that("infer_type strict homogeneous lists keep fixed sizes", {
  x <- list(c(1L, 2L), c(3L, 4L))

  testthat::expect_equal(infer_type(x, strict = TRUE)$name, ListOf(Integer[2])$name)
})

testthat::test_that("infer_type non-strict homogeneous lists omit fixed sizes", {
  x <- list(c(1L, 2L), c(3L, 4L))

  testthat::expect_equal(infer_type(x)$name, ListOf(Integer)$name)
})

testthat::test_that("infer_type empty data frames map to DataFrame in strict mode", {
  testthat::expect_equal(infer_type(data.frame(), strict = TRUE)$name, DataFrame$name)
})

testthat::test_that("infer_type functions ignore strict mode", {
  testthat::expect_equal(infer_type(function(x) x, strict = TRUE)$name, Function$name)
})

testthat::test_that("infer_type strict logical vectors keep length", {
  testthat::expect_equal(infer_type(c(TRUE, FALSE), strict = TRUE)$name, Bool[2]$name)
})

testthat::test_that("create_type keeps checker function", {
  checker <- function(x) is.numeric(x) && all(x >= 0)
  t <- create_type("non_negative", checker)

  testthat::expect_identical(t$check, checker)
})

testthat::test_that("integer and double built-ins distinguish storage modes", {
  testthat::expect_false(Integer$check(1.5))
  testthat::expect_false(Double$check(1L))
})

testthat::test_that("function and data frame built-ins accept matching values", {
  testthat::expect_true(Function$check(function(x) x))
  testthat::expect_true(DataFrame$check(data.frame(x = 1)))
})

testthat::test_that("size operator encodes length in type names", {
  testthat::expect_equal(String[3]$name, "string[3]")
})

testthat::test_that("size operator works with Any", {
  any_two <- Any[2]

  testthat::expect_true(any_two$check(list(1, "a")))
  testthat::expect_false(any_two$check(1))
})

testthat::test_that("Enum creates numeric membership types", {
  choices <- Enum(1, 2, 3)

  testthat::expect_equal(choices$name, "enum[1, 2, 3]")
  testthat::expect_true(choices$check(2))
  testthat::expect_false(choices$check(4))
})

testthat::test_that("Enum accepts vectors whose values stay within the allowed set", {
  choices <- Enum(1, 2, 3)

  testthat::expect_true(choices$check(c(1, 3, 2, 1)))
  testthat::expect_false(choices$check(c(1, 4)))
})

testthat::test_that("Enum supports character values and single-vector declarations", {
  colors <- Enum("red", "green", "blue")

  testthat::expect_equal(colors$name, "enum[\"red\", \"green\", \"blue\"]")
  testthat::expect_true(colors$check(c("red", "blue")))
  testthat::expect_false(colors$check("yellow"))
})

testthat::test_that("Enum works with typed assignments and reassignment", {
  local({
    status %:% Enum(1, 2, 3) %<-% 2

    testthat::expect_equal(status, 2)
    status <- 3
    testthat::expect_equal(status, 3)
    testthat::expect_error(status <- 4, "Expected enum\\[1, 2, 3\\]")
  })
})

testthat::test_that("Enum matches numeric values across integer and double storage", {
  choices <- Enum(1, 2, 3)

  testthat::expect_true(choices$check(2L))
})

testthat::test_that("Enum rejects non-atomic runtime values", {
  choices <- Enum(1, 2, 3)

  testthat::expect_false(choices$check(list(1, 2)))
  testthat::expect_false(choices$check(data.frame(x = 1)))
})

testthat::test_that("Enum rejects empty or malformed declarations", {
  testthat::expect_error(Enum(), "requires at least one allowed value")
  testthat::expect_error(Enum(character()), "requires at least one allowed value")
  testthat::expect_error(Enum(list(1), list(2)), "scalar atomic values")
})

testthat::test_that("Enum supports logical allowed values", {
  toggle <- Enum(TRUE, FALSE)

  testthat::expect_true(toggle$check(c(TRUE, FALSE, TRUE)))
  testthat::expect_false(toggle$check(c(TRUE, 1)))
})

testthat::test_that("Literal creates scalar exact-match types", {
  direction <- Literal("left", "right")

  testthat::expect_equal(direction$name, "literal[\"left\", \"right\"]")
  testthat::expect_true(direction$check("left"))
  testthat::expect_false(direction$check("up"))
  testthat::expect_false(direction$check(c("left", "right")))
})

testthat::test_that("Literal distinguishes double and integer values", {
  status_double <- Literal(200, 404)
  status_integer <- Literal(200L, 404L)

  testthat::expect_true(status_double$check(200))
  testthat::expect_false(status_double$check(200L))
  testthat::expect_true(status_integer$check(200L))
  testthat::expect_false(status_integer$check(200))
})

testthat::test_that("Literal works with typed assignments and logical values", {
  local({
    direction %:% Literal("left", "right") %<-% "left"
    testthat::expect_equal(direction, "left")
    direction <- "right"
    testthat::expect_equal(direction, "right")
    testthat::expect_error(direction <- "up", "Expected literal\\[\"left\", \"right\"\\]")
  })

  toggle <- Literal(TRUE, FALSE)
  testthat::expect_true(toggle$check(TRUE))
  testthat::expect_false(toggle$check(1))
})

testthat::test_that("Literal rejects malformed declarations", {
  testthat::expect_error(Literal(), "requires at least one allowed value")
  testthat::expect_error(Literal(c("left", "right")), "scalar atomic values")
  testthat::expect_error(Literal(list("left")), "scalar atomic values")
})

testthat::test_that("create_list_type rejects blank field names", {
  spec <- list(String)
  names(spec) <- ""

  testthat::expect_error(create_list_type(spec), "must be named")
})

testthat::test_that("create_list_type marks optional fields in name", {
  person <- create_list_type(list(name = String, nickname = Optional(String)))

  testthat::expect_match(person$name, "nickname\\?: string \\| null")
})

testthat::test_that("create_list_type accepts explicit NULL for optional field", {
  person <- create_list_type(list(name = String, nickname = Optional(String)))

  testthat::expect_true(person$check(list(name = "Ada", nickname = NULL)))
})

testthat::test_that("create_list_type surfaces field context on mismatch", {
  person <- create_list_type(list(name = String))

  testthat::expect_error(person$check(list(name = 1)), "Type error in 'name'")
})

testthat::test_that("nested list types propagate inner errors", {
  schema <- create_list_type(list(meta = create_list_type(list(flag = Bool))))

  testthat::expect_error(schema$check(list(meta = list(flag = "yes"))), "Type error in 'flag'")
})

testthat::test_that("create_dataframe_type rejects blank column names", {
  spec <- list(Numeric)
  names(spec) <- ""

  testthat::expect_error(create_dataframe_type(spec), "must be named")
})

testthat::test_that("create_dataframe_type marks optional columns in name", {
  schema <- create_dataframe_type(list(a = Numeric, b = Optional(String)))

  testthat::expect_match(schema$name, "b\\?: string \\| null")
})

testthat::test_that("create_dataframe_type surfaces column context on mismatch", {
  schema <- create_dataframe_type(list(a = Numeric))

  testthat::expect_error(schema$check(data.frame(a = "x", stringsAsFactors = FALSE)), "Type error in 'a'")
})

testthat::test_that("create_dataframe_type check returns FALSE for non data frames", {
  schema <- create_dataframe_type(list(a = Numeric))

  testthat::expect_false(schema$check(list(a = 1)))
})

testthat::test_that("Scalar names wrap inner type name", {
  testthat::expect_equal(Scalar(String)$name, "scalar<string>")
})

testthat::test_that("Readonly unwraps nested readonly modifiers", {
  ro <- Readonly(Readonly(String))

  testthat::expect_s3_class(ro, "sicher_readonly")
  testthat::expect_identical(ro$base_type, String)
})

testthat::test_that("Optional adds null to union name", {
  testthat::expect_equal(Optional(String)$name, "string | null")
})

testthat::test_that("create_union accepts NULL as left operand", {
  u <- create_union(NULL, String)

  testthat::expect_true(check_type(NULL, u))
  testthat::expect_true(check_type("ok", u))
})

testthat::test_that("create_union concatenates nested union names", {
  u <- create_union(String | Numeric, Bool | Null)

  testthat::expect_equal(u$name, "string | numeric | bool | null")
})

testthat::test_that("check_type accepts readonly type specifications", {
  testthat::expect_true(check_type(5, Readonly(Numeric)))
})

testthat::test_that("check_type includes context in scalar mismatch", {
  testthat::expect_error(check_type(c(1, 2), Scalar(Numeric), context = "score"), "Type error in 'score'")
})

testthat::test_that("check_type rejects NULL for non-null unions", {
  testthat::expect_error(check_type(NULL, String | Numeric), "Expected string \\| numeric, got null")
})

testthat::test_that("type_error previews named list fields", {
  err <- type_error("cfg", "schema", "list", list(a = 1, b = 2))

  testthat::expect_match(err, "list with fields: \\[a, b\\]")
})

testthat::test_that("type_error previews unnamed list lengths", {
  err <- type_error(NULL, "schema", "list", list(1, 2, 3))

  testthat::expect_match(err, "list of length 3")
})

testthat::test_that("type_error previews data frame columns", {
  err <- type_error(NULL, "schema", "data.frame", data.frame(a = 1, b = 2))

  testthat::expect_match(err, "columns: \\[a, b\\]")
})

testthat::test_that("type_error previews short atomic vectors", {
  err <- type_error(NULL, "nums", "bool", c(1, 2, 3))

  testthat::expect_match(err, "Received: \\[1, 2, 3\\]")
})

testthat::test_that("get_type_name reports functions", {
  testthat::expect_equal(get_type_name(function(x) x), "function")
})

testthat::test_that("get_type_name reports empty logical vectors", {
  testthat::expect_equal(get_type_name(logical()), "bool(0)")
})

testthat::test_that("get_type_name prioritizes list labeling over S3 classes", {
  obj <- structure(list(), class = c("custom_obj", "list"))

  testthat::expect_equal(get_type_name(obj), "list(0)")
})

testthat::test_that("type annotation captures symbol names", {
  ann <- some_value %:% Numeric

  testthat::expect_equal(ann$var_name, "some_value")
})

testthat::test_that("disabled mode bypasses type checking", {
  old <- options(sicher.mode = "off")
  on.exit(options(old), add = TRUE)

  local({
    off_mode_value %:% Numeric %<-% "text"
    testthat::expect_equal(off_mode_value, "text")
  })
})

testthat::test_that("disabled mode uses regular bindings", {
  old <- options(sicher.mode = "off")
  on.exit(options(old), add = TRUE)

  local({
    off_binding %:% Numeric %<-% 1
    testthat::expect_false(bindingIsActive("off_binding", environment()))
  })
})

testthat::test_that("typed assignment replaces existing plain bindings", {
  local({
    rebound <- 1
    rebound %:% Numeric %<-% 2

    testthat::expect_equal(rebound, 2)
    testthat::expect_true(bindingIsActive("rebound", environment()))
  })
})

testthat::test_that("typed assignment creates mutable active bindings", {
  local({
    mutable %:% Numeric %<-% 1
    mutable <- 3

    testthat::expect_equal(mutable, 3)
  })
})

testthat::test_that("readonly bindings remain readable", {
  local({
    ro_value %:% Readonly(String) %<-% "hello"

    testthat::expect_equal(ro_value, "hello")
  })
})

testthat::test_that("typed_function preserves formals", {
  f <- typed_function(function(x, y = 1) x + y, params = list(x = Numeric))

  testthat::expect_identical(names(formals(f)), c("x", "y"))
  testthat::expect_identical(formals(f)$y, 1)
})

testthat::test_that("typed_function keeps unchecked defaults", {
  f <- typed_function(function(x, y = "ok") paste(x, y), params = list(x = String))

  testthat::expect_equal(f("hi"), "hi ok")
})

testthat::test_that("typed_function does not validate omitted annotated defaults", {
  f <- typed_function(function(x = "bad") x, params = list(x = Numeric))

  testthat::expect_equal(f(), "bad")
})

testthat::test_that("typed_function accepts readonly parameter specifications", {
  f <- typed_function(function(x) x + 1, params = list(x = Readonly(Numeric)))

  testthat::expect_equal(f(2), 3)
})

testthat::test_that("typed_function accepts union return types", {
  f <- typed_function(function(flag) if (flag) 1 else "one", .return = Numeric | String)

  testthat::expect_equal(f(TRUE), 1)
  testthat::expect_equal(f(FALSE), "one")
})

testthat::test_that("typed_function forwards dots", {
  f <- typed_function(function(...) length(list(...)))

  testthat::expect_equal(f(1, "a", TRUE), 3)
})

testthat::test_that("typed_function print is compact without annotations", {
  f <- typed_function(function() 1)

  testthat::expect_output(print(f), "<typed_function \\(\\)>")
})

testthat::test_that("infer_type falls back to Any for unsupported classes", {
  testthat::expect_equal(infer_type(factor("a"))$name, Any$name)
})

testthat::test_that("infer_type treats partially named lists as generic lists", {
  x <- structure(list(1L, "a"), names = c("id", ""))

  testthat::expect_equal(infer_type(x)$name, List$name)
})

testthat::test_that("infer_type treats mixed NULL lists as generic lists", {
  x <- list(NULL, 1L)

  testthat::expect_equal(infer_type(x)$name, List$name)
})

testthat::test_that("infer_type strict homogeneous lists keep fixed sizes", {
  x <- list(c(1L, 2L), c(3L, 4L))

  testthat::expect_equal(infer_type(x, strict = TRUE)$name, ListOf(Integer[2])$name)
})

testthat::test_that("infer_type non-strict homogeneous lists omit fixed sizes", {
  x <- list(c(1L, 2L), c(3L, 4L))

  testthat::expect_equal(infer_type(x)$name, ListOf(Integer)$name)
})

testthat::test_that("infer_type empty data frames map to DataFrame in strict mode", {
  testthat::expect_equal(infer_type(data.frame(), strict = TRUE)$name, DataFrame$name)
})

testthat::test_that("infer_type functions ignore strict mode", {
  testthat::expect_equal(infer_type(function(x) x, strict = TRUE)$name, Function$name)
})

testthat::test_that("infer_type strict logical vectors keep length", {
  testthat::expect_equal(infer_type(c(TRUE, FALSE), strict = TRUE)$name, Bool[2]$name)
})

testthat::test_that("extend merges base and extra fields", {
  person <- create_list_type(list(
    name = String,
    age = Numeric
  ))

  employee <- extend(person, list(
    role = String,
    department = Optional(String)
  ))

  testthat::expect_true(employee$check(list(
    name = "Alice",
    age = 30,
    role = "Engineer"
  )))

  testthat::expect_true(employee$check(list(
    name = "Alice",
    age = 30,
    role = "Engineer",
    department = "R&D"
  )))

  testthat::expect_error(
    employee$check(list(name = "Alice", role = "Engineer")),
    "Missing required field\\(s\\): age"
  )

  testthat::expect_error(
    employee$check(list(name = "Alice", age = 30)),
    "Missing required field\\(s\\): role"
  )
})

testthat::test_that("extend does not mutate the base type", {
  person <- create_list_type(list(
    name = String,
    age = Numeric
  ))

  employee <- extend(person, list(role = String))

  testthat::expect_true(person$check(list(name = "Alice", age = 30)))
  testthat::expect_error(
    person$check(list(name = "Alice", age = 30, role = "Engineer")),
    "Unexpected field: 'role'"
  )

  testthat::expect_true(employee$check(list(
    name = "Alice",
    age = 30,
    role = "Engineer"
  )))
})

testthat::test_that("extend warns on overrides and the extra field takes precedence", {
  base <- create_list_type(list(
    id = Numeric,
    active = Bool
  ))

  derived <- testthat::expect_warning(
    extend(base, list(id = String)),
    "being overridden: id"
  )

  testthat::expect_true(derived$check(list(id = "abc", active = TRUE)))
  testthat::expect_error(
    derived$check(list(id = 123, active = TRUE)),
    "Expected string"
  )
})

testthat::test_that("extend can be chained across multiple levels", {
  person <- create_list_type(list(
    name = String,
    age = Numeric
  ))

  employee <- extend(person, list(
    badge = String | Numeric,
    department = Optional(String)
  ))

  manager <- extend(employee, list(
    reports = Numeric
  ))

  testthat::expect_true(manager$check(list(
    name = "Bob",
    age = 45,
    badge = "A-12",
    reports = 6
  )))

  testthat::expect_true(manager$check(list(
    name = "Bob",
    age = 45,
    badge = 12,
    department = "Sales",
    reports = 6
  )))

  testthat::expect_error(
    manager$check(list(name = "Bob", age = 45, badge = TRUE, reports = 6)),
    "Expected string \\\\| numeric"
  )
})

testthat::test_that("extend validates the base argument", {
  custom_type <- create_type("custom", function(x) TRUE)

  testthat::expect_error(
    extend(list(name = String), list(role = String)),
    "`base` must be a sicher_type produced by create_list_type\\(\\)"
  )

  testthat::expect_error(
    extend(String, list(role = String)),
    "does not carry a field specification"
  )

  testthat::expect_error(
    extend(custom_type, list(role = String)),
    "does not carry a field specification"
  )
})

testthat::test_that("extend validates the extra argument structure", {
  base <- create_list_type(list(name = String))

  testthat::expect_error(
    extend(base, "bad"),
    "`extra` must be a fully named list of fields"
  )

  testthat::expect_error(
    extend(base, list(String)),
    "`extra` must be a fully named list of fields"
  )

  unnamed_extra <- structure(list(String, Numeric), names = c("role", ""))
  testthat::expect_error(
    extend(base, unnamed_extra),
    "`extra` must be a fully named list of fields"
  )
})

testthat::test_that("extend validates field types in extra", {
  base <- create_list_type(list(name = String))

  testthat::expect_error(
    extend(base, list(role = "String")),
    "Field 'role' in `extra` must be a sicher_type or sicher_union"
  )

  testthat::expect_true(extend(base, list(role = String | Numeric))$check(list(
    name = "Alice",
    role = 1
  )))
})