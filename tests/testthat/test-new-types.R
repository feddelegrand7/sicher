# Types and modifiers added in 0.2.0

testthat::test_that("Refine adds a custom rule with a custom message", {
  Password <- Refine(Scalar(String), function(x) nchar(x) >= 8, "at least 8 chars")
  testthat::expect_true(check_type("long enough", Password))
  testthat::expect_error(check_type("short", Password, "pw"), "Type error in 'pw': at least 8 chars")
  testthat::expect_error(check_type(1, Password), "Expected scalar<string>")
  testthat::expect_output(print(Password), "refine<scalar<string>>")

  Even <- Refine(Integer, function(x) all(x %% 2L == 0L), name = "even")
  testthat::expect_output(print(Even), "<type: even >")
  testthat::expect_error(check_type(3L, Even), "does not satisfy even")

  Boom <- Refine(Numeric, function(x) stop("kaboom"))
  testthat::expect_error(check_type(1, Boom), "kaboom")

  testthat::expect_error(Refine(Numeric, "not a function"), "must be a function")
  testthat::expect_error(Refine(5, function(x) TRUE), "requires a sicher_type")
})

testthat::test_that("Positive, NonNegative and Whole", {
  testthat::expect_true(check_type(c(1, 2.5), Positive))
  testthat::expect_error(check_type(0, Positive), "not > 0")
  testthat::expect_error(check_type(NA_real_, Positive), "contains NA")
  testthat::expect_error(check_type("1", Positive), "Expected positive")

  testthat::expect_true(check_type(0, NonNegative))
  testthat::expect_error(check_type(c(-1, -2), NonNegative), "values \\[-1, -2\\] are not >= 0")

  testthat::expect_true(check_type(3, Whole))
  testthat::expect_true(check_type(3L, Whole))
  testthat::expect_error(check_type(2.5, Whole), "not a whole number")
  testthat::expect_error(check_type(Inf, Whole), "not a whole number")
})

testthat::test_that("MinLength, MaxLength and Unique", {
  testthat::expect_true(check_type(1:3, MinLength(Integer, 3)))
  testthat::expect_error(check_type(1:2, MinLength(Integer, 3)), "at least 3")
  testthat::expect_true(check_type(1:3, MaxLength(Integer, 3)))
  testthat::expect_error(check_type(1:4, MaxLength(Integer, 3)), "at most 3")
  testthat::expect_error(check_type(data.frame(a = 1:2), MinLength(DataFrame, 3)), "but got 2")
  testthat::expect_error(MinLength(Integer, -1), "non-negative whole number")
  testthat::expect_error(MaxLength(Integer, 1.5), "non-negative whole number")

  testthat::expect_true(check_type(c(1, 2), Unique(Numeric)))
  testthat::expect_error(check_type(c(1, 2, 2), Unique(Numeric)), "duplicated value\\(s\\): \\[2\\]")
  testthat::expect_error(check_type(list(1, 1), Unique(List)), "1 duplicated element")
  testthat::expect_error(check_type(data.frame(a = c(1, 1)), Unique(DataFrame)), "duplicated row")
})

testthat::test_that("Default fills NULL and missing list fields", {
  retries %:% Default(Integer, 3L) %<-% NULL
  testthat::expect_identical(retries, 3L)
  retries <- 5L
  testthat::expect_identical(retries, 5L)
  testthat::expect_error(retries <- "a", "Expected integer = 3")

  Config <- create_list_type(list(host = String, port = Default(Numeric, 8080)))
  testthat::expect_output(print(Config), "port\\?: numeric = 8080")
  testthat::expect_equal(parse_as(list(host = "h"), Config), list(host = "h", port = 8080))
  testthat::expect_equal(parse_as(list(host = "h", port = 1), Config)$port, 1)

  Table <- create_dataframe_type(list(x = Numeric, flag = Default(Bool, TRUE)))
  out <- parse_as(data.frame(x = 1:2), Table)
  testthat::expect_equal(out$flag, c(TRUE, TRUE))

  testthat::expect_error(Default(Numeric, "a"), "default value")
})

testthat::test_that("Coerce converts values before checking them", {
  age %:% Coerce(Numeric) %<-% "42"
  testthat::expect_identical(age, 42)
  testthat::expect_error(age <- "forty", "cannot convert \\[\"forty\"\\] to numeric")

  testthat::expect_identical(parse_as("5", Coerce(Integer)), 5L)
  testthat::expect_identical(parse_as(1, Coerce(String)), "1")
  testthat::expect_identical(parse_as("TRUE", Coerce(Bool)), TRUE)
  testthat::expect_identical(parse_as("0.5", Coerce(Between(0, 1))), 0.5)
  testthat::expect_identical(parse_as("3", Coerce(Scalar(Numeric))), 3)
  testthat::expect_s3_class(parse_as("2024-01-01", Coerce(Date)), "Date")
  testthat::expect_error(parse_as("nope", Coerce(Date)), "cannot convert value to date")
  testthat::expect_equal(
    levels(parse_as("M", Coerce(Factor(c("S", "M"))))),
    c("S", "M")
  )
  testthat::expect_error(parse_as("XL", Coerce(Factor(c("S", "M")))), "cannot convert")

  Custom <- Coerce(create_type("upper", function(x) is.character(x) && all(x == toupper(x))),
                   using = toupper)
  testthat::expect_identical(parse_as("abc", Custom), "ABC")
  testthat::expect_error(Coerce(create_type("t", is.character)), "does not know how")

  add <- typed_function(function(x, y) x + y, params = list(x = Coerce(Numeric), y = Numeric))
  testthat::expect_equal(add("2", 3), 5)
})

testthat::test_that("Nullable fields are required but may be NULL", {
  Profile <- create_list_type(list(
    name = String,
    nickname = Optional(String),
    bio = Nullable(String)
  ))
  testthat::expect_output(print(Profile), "nickname\\?: string \\| null, bio: string \\| null")
  testthat::expect_true(check_type(list(name = "a", bio = NULL), Profile))
  testthat::expect_true(check_type(list(name = "a", bio = "b", nickname = "n"), Profile))
  testthat::expect_error(check_type(list(name = "a"), Profile), "Type error in 'bio': Missing required field")
  testthat::expect_true(check_type(NULL, Nullable(String)))
  testthat::expect_error(Nullable(5), "requires a sicher_type")
})

testthat::test_that("extra = 'allow' and 'drop' on list and data frame types", {
  Strict <- create_list_type(list(a = Numeric))
  Loose <- create_list_type(list(a = Numeric), extra = "allow")
  Strip <- create_list_type(list(a = Numeric), extra = "drop")
  value <- list(a = 1, b = 2)

  testthat::expect_error(check_type(value, Strict), "Unexpected field")
  testthat::expect_equal(parse_as(value, Loose), value)
  testthat::expect_equal(parse_as(value, Strip), list(a = 1))

  stripped %:% Strip %<-% value
  testthat::expect_equal(stripped, list(a = 1))

  DfStrip <- create_dataframe_type(list(a = Numeric), extra = "drop")
  testthat::expect_equal(names(parse_as(data.frame(a = 1, b = 2), DfStrip)), "a")
  testthat::expect_error(create_list_type(list(a = Numeric), extra = "nope"))
})

testthat::test_that("Tuple checks fixed positions", {
  Point <- Tuple(Numeric, Numeric)
  testthat::expect_output(print(Point), "\\[numeric, numeric\\]")
  testthat::expect_true(check_type(list(1, 2), Point))
  testthat::expect_error(check_type(list(1), Point), "2 elements")
  testthat::expect_error(check_type(list(1, "a"), Point), "Type error in '\\[\\[2\\]\\]'")
  testthat::expect_error(check_type(c(1, 2), Point), "Expected \\[numeric, numeric\\]")
  testthat::expect_output(print(Tuple(key = String, value = Numeric)), "key: string, value: numeric")
  testthat::expect_error(Tuple(), "at least one")
  testthat::expect_error(Tuple(String, 5), "Element 2")
})

testthat::test_that("Record checks keys and values", {
  Scores <- Record(String, Numeric)
  testthat::expect_true(check_type(list(a = 1, b = 2), Scores))
  testthat::expect_true(check_type(list(), Scores))
  testthat::expect_true(check_type(c(a = 1, b = 2), Record(Numeric)))
  testthat::expect_error(check_type(list(a = 1, b = "x"), Scores), "Type error in 'b'")
  testthat::expect_error(check_type(list(1, 2), Scores), "not every element is named")
  testthat::expect_error(check_type("a", Scores), "Expected record<string, numeric>")

  Flags <- Record(Enum("debug", "verbose"), Bool)
  testthat::expect_error(
    check_type(list(debug = TRUE, color = FALSE), Flags),
    "Type error in 'color': Invalid key"
  )
})

testthat::test_that("intersections merge list types and combine other checks", {
  Named <- create_list_type(list(name = String))
  Aged <- create_list_type(list(age = Numeric))
  Both <- Named & Aged
  testthat::expect_true(check_type(list(name = "a", age = 1), Both))
  testthat::expect_error(check_type(list(name = "a"), Both), "Missing required field")

  Score <- Numeric & Between(0, 100)
  testthat::expect_output(print(Score), "numeric & between\\[0, 100\\]")
  testthat::expect_true(check_type(50, Score))
  testthat::expect_error(check_type(150, Score), "outside")
  testthat::expect_error(check_type("a", Score), "Expected numeric & between")

  Mixed <- (String | Numeric) & Scalar(Any)
  testthat::expect_output(print(Mixed), "\\(string \\| numeric\\) & scalar<any>")
  testthat::expect_true(check_type(1, Mixed))
  testthat::expect_false(is_type(c(1, 2), Mixed))

  # Overlapping fields must satisfy both field types
  A <- create_list_type(list(x = Numeric))
  B <- create_list_type(list(x = Between(0, 1)))
  testthat::expect_true(check_type(list(x = 0.5), A & B))
  testthat::expect_error(check_type(list(x = 5), A & B), "outside")

  # Unions keep correct member names after intersections
  U <- (Numeric & Positive) | String
  testthat::expect_equal(U$name, "numeric & positive | string")
})

testthat::test_that("DiscriminatedUnion routes on the discriminator", {
  Circle <- create_list_type(list(radius = Numeric))
  Square <- create_list_type(list(side = Numeric))
  Shape <- DiscriminatedUnion("kind", circle = Circle, square = Square)

  testthat::expect_true(check_type(list(kind = "circle", radius = 1), Shape))
  testthat::expect_true(check_type(list(kind = "square", side = 1), Shape))
  testthat::expect_error(check_type(list(kind = "square", radius = 1), Shape), "'side': Missing")
  testthat::expect_error(check_type(list(kind = "hex"), Shape), "Invalid discriminator value")
  testthat::expect_error(check_type(list(radius = 1), Shape), "Missing discriminator")
  testthat::expect_error(check_type("a", Shape), "Expected discriminated")

  # Unnamed members carrying their own Literal() field
  Cat <- create_list_type(list(type = Literal("cat"), meows = Bool))
  Dog <- create_list_type(list(type = Literal("dog"), barks = Bool))
  Pet <- DiscriminatedUnion("type", Cat, Dog)
  testthat::expect_true(check_type(list(type = "dog", barks = TRUE), Pet))

  testthat::expect_error(DiscriminatedUnion("type", Circle), "Literal")
  testthat::expect_error(DiscriminatedUnion("kind", a = Circle, Square), "all DiscriminatedUnion")
  testthat::expect_error(DiscriminatedUnion("kind", a = Numeric), "create_list_type")
  testthat::expect_error(DiscriminatedUnion("type", Cat, Cat), "unique")
})

testthat::test_that("Lazy supports recursive types", {
  Tree <- create_list_type(list(
    value = Numeric,
    children = ListOf(Lazy(function() Tree, name = "Tree"))
  ))
  testthat::expect_output(print(Tree), "list<Tree>")
  leaf <- list(value = 2, children = list())
  testthat::expect_true(check_type(list(value = 1, children = list(leaf, leaf)), Tree))
  testthat::expect_error(
    check_type(list(value = 1, children = list(leaf, list(value = "x", children = list()))), Tree),
    "children\\[\\[2\\]\\]\\$value"
  )
  testthat::expect_error(check_type(1, Lazy(function() 5)), "must return a sicher type")
  testthat::expect_error(Lazy(5), "must be a function")
})

testthat::test_that("InstanceOf checks classes", {
  testthat::expect_true(check_type(structure(list(), class = "foo"), InstanceOf("foo")))
  testthat::expect_true(check_type(Sys.Date(), InstanceOf(c("POSIXct", "Date"))))
  testthat::expect_error(check_type(list(), InstanceOf("foo")), "instance_of<foo>")
  testthat::expect_error(InstanceOf(character(0)), "non-empty")
})

testthat::test_that("Brand and brand() create nominal types", {
  UserId <- Brand(Scalar(String), "UserId")
  OrderId <- Brand(Scalar(String), "OrderId")
  uid <- brand("u1", UserId)
  testthat::expect_s3_class(uid, "UserId")
  testthat::expect_true(check_type(uid, UserId))
  testthat::expect_error(check_type("u1", UserId), "unbranded string")
  testthat::expect_error(check_type(brand("o1", OrderId), UserId), "branded as OrderId")
  testthat::expect_error(check_type(1, UserId), "Expected UserId<scalar<string>>")
  testthat::expect_error(brand(1, UserId), "Expected scalar<string>")
  testthat::expect_error(brand("a", String), "Brand\\(\\)")
})

testthat::test_that("arithmetic on branded values drops the brand", {
  UserId <- Brand(Integer, "UserId")
  OrderId <- Brand(Integer, "OrderId")
  uid <- brand(c(1L, 2L), UserId)

  testthat::expect_identical(uid + 1L, c(2L, 3L))
  testthat::expect_identical(uid == 1L, c(TRUE, FALSE))
  testthat::expect_identical(-uid, c(-1L, -2L))
  testthat::expect_identical(abs(uid), c(1L, 2L))
  testthat::expect_identical(uid + uid, c(2L, 4L))
  testthat::expect_error(check_type(uid + 1L, UserId), "unbranded integer")
  testthat::expect_error(
    uid + brand(1L, OrderId),
    "Cannot combine a value branded UserId with a value branded OrderId"
  )
  testthat::expect_error(uid == brand(1L, OrderId), "using `==`")
})

testthat::test_that("branded values keep the methods of their base class", {
  Day <- Brand(Date, "Day")
  d <- brand(as.Date("2024-01-10"), Day)
  testthat::expect_s3_class(d, c("Day", "sicher_branded", "Date"), exact = TRUE)
  testthat::expect_identical(d + 1, as.Date("2024-01-11"))
  testthat::expect_equal(as.numeric(d - as.Date("2024-01-01")), 9)
})

testthat::test_that("brands can be stacked and are all removed by dump_as()", {
  A <- Brand(Integer, "A")
  AB <- Brand(A, "AB")
  ab <- brand(brand(1L, A), AB)
  testthat::expect_s3_class(ab, c("AB", "A", "sicher_branded"), exact = TRUE)
  testthat::expect_true(check_type(ab, AB))
  testthat::expect_identical(dump_as(ab, AB), 1L)
})

testthat::test_that("FunctionOf checks and wraps functions", {
  Formatter <- FunctionOf(list(x = Numeric), .return = String)
  testthat::expect_output(print(Formatter), "function\\(x: numeric\\) => string")

  fmt %:% Formatter %<-% function(x) format(x)
  testthat::expect_s3_class(fmt, "sicher_typed_function")
  testthat::expect_equal(fmt(1), "1")
  testthat::expect_error(fmt("a"), "Type error in 'x'")
  testthat::expect_error(fmt <- function(y) y, "no parameter\\(s\\) named x")
  testthat::expect_error(fmt <- 5, "Expected function")

  bad_return <- parse_as(function(x) x, Formatter)
  testthat::expect_error(bad_return(1), "<return value>")

  testthat::expect_true(is_type(function(...) NULL, Formatter))
  testthat::expect_true(is_type(sum, FunctionOf(list(x = Numeric))))
  testthat::expect_output(print(FunctionOf()), "function\\(\\) => any")
  testthat::expect_error(FunctionOf(list(Numeric)), "named list")
})

testthat::test_that("Factor, Date, DateTime, Environment and MatrixOf", {
  testthat::expect_true(check_type(factor("a"), Factor()))
  testthat::expect_true(check_type(factor("M", levels = c("S", "M")), Factor(c("S", "M"))))
  testthat::expect_error(check_type(factor("M"), Factor(c("S", "M"))), "has levels \\[M\\]")
  testthat::expect_error(check_type("a", Factor()), "Expected factor")
  testthat::expect_error(Factor(c("a", "a")), "unique")

  testthat::expect_true(check_type(as.Date("2024-01-01"), Date))
  testthat::expect_error(check_type("2024-01-01", Date), "Expected date")
  testthat::expect_true(check_type(Sys.time(), DateTime))
  testthat::expect_error(check_type(Sys.Date(), DateTime), "Expected datetime")
  testthat::expect_true(check_type(new.env(), Environment))
  testthat::expect_error(check_type(list(), Environment), "Expected environment")

  testthat::expect_true(check_type(diag(2), MatrixOf()))
  testthat::expect_true(check_type(diag(2), MatrixOf(Numeric, nrow = 2, ncol = 2)))
  testthat::expect_error(check_type(matrix(1:6, 2), MatrixOf(Numeric, 2, 2)), "2 x 3 matrix")
  testthat::expect_error(check_type(matrix("a"), MatrixOf(Numeric)), "Expected matrix<numeric>")
  testthat::expect_error(check_type(1:3, MatrixOf()), "Expected matrix<any>")
  testthat::expect_output(print(MatrixOf(Numeric, nrow = 3)), "matrix<numeric>\\[3 x \\?\\]")
})

testthat::test_that("infer_type recognises factors and dates", {
  testthat::expect_equal(infer_type(factor(c("a", "b")))$name, "factor<\"a\", \"b\">")
  testthat::expect_identical(infer_type(Sys.Date()), Date)
  testthat::expect_equal(infer_type(Sys.time(), strict = TRUE)$name, "scalar<datetime>")
})

testthat::test_that("using a factory without calling it gives a hint", {
  testthat::expect_error(check_type(1, Factor), "Did you forget to call")
})
