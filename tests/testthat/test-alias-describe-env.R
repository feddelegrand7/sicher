# Aliases, dump_as() / to_json(), Describe() and parse_env()

AliasUser <- create_list_type(list(
  user_id    = Scalar(Integer),
  full_name  = Alias(Scalar(String), "name"),
  created_at = Coerce(Scalar(Date)),
  nick_name  = Optional(Scalar(String)),
  role       = Default(Scalar(String), "member"),
  tags       = String
), alias = "camel")

alias_input <- list(
  userId = 1L, name = "Ann", createdAt = "2024-01-05", tags = c("a", "b")
)

# -----------------------------------------------------------------------------
# Alias() and the alias argument
# -----------------------------------------------------------------------------

testthat::test_that("aliased fields are read from their external names", {
  user <- parse_as(alias_input, AliasUser)
  testthat::expect_equal(names(user), c("user_id", "full_name", "created_at", "tags", "role"))
  testthat::expect_equal(user$full_name, "Ann")
  testthat::expect_equal(user$created_at, as.Date("2024-01-05"))
  testthat::expect_equal(user$role, "member")
})

testthat::test_that("aliased fields can also be given by their R names", {
  user <- parse_as(
    list(user_id = 1L, full_name = "Ann", created_at = "2024-01-05", tags = "a"),
    AliasUser
  )
  testthat::expect_equal(user$user_id, 1L)
  testthat::expect_equal(user$full_name, "Ann")
})

testthat::test_that("the external name wins when both names are present", {
  user <- parse_as(c(alias_input, list(user_id = 99L)), AliasUser)
  testthat::expect_equal(user$user_id, 1L)
  testthat::expect_equal(sum(names(user) == "user_id"), 1L)
})

testthat::test_that("errors point at the external names", {
  err <- tryCatch(
    parse_as(list(userId = "x", createdAt = "2024-01-05", fooBar = 1), AliasUser),
    sicher_type_error = function(e) e
  )
  paths <- vapply(err$issues, function(i) format_path(i$path), character(1))
  testthat::expect_true(all(c("userId", "name", "tags", "fooBar") %in% paths))
  testthat::expect_match(conditionMessage(err), "valid fields: userId, name, createdAt")
})

testthat::test_that("typed variables store values under the R names", {
  aliased_user %:% AliasUser %<-% alias_input
  testthat::expect_equal(aliased_user$full_name, "Ann")
  testthat::expect_error(aliased_user <- list(userId = "x"), "userId")
})

testthat::test_that("naming conventions convert field names", {
  testthat::expect_equal(
    vapply(c("user_id", "userID", "HTTPServer", "created.at", "x"),
           convert_case, character(1), convention = "camel", USE.NAMES = FALSE),
    c("userId", "userId", "httpServer", "createdAt", "x")
  )
  testthat::expect_equal(convert_case("user_id", "pascal"), "UserId")
  testthat::expect_equal(convert_case("userId", "snake"), "user_id")
  testthat::expect_equal(convert_case("user_id", "kebab"), "user-id")
})

testthat::test_that("alias accepts a function", {
  Upper <- create_list_type(list(first_name = String), alias = toupper)
  testthat::expect_equal(parse_as(list(FIRST_NAME = "a"), Upper), list(first_name = "a"))
  testthat::expect_error(
    create_list_type(list(a = String), alias = function(x) c("x", "y")),
    "single non-empty string"
  )
})

testthat::test_that("invalid aliases are rejected", {
  testthat::expect_error(Alias(String, ""), "non-empty string")
  testthat::expect_error(Alias(String, c("a", "b")), "non-empty string")
  testthat::expect_error(Alias("x", "a"), "Alias\\(\\) requires")
  testthat::expect_error(
    create_list_type(list(a = Alias(String, "x"), b = Alias(String, "x"))),
    "same external name: x"
  )
  testthat::expect_error(
    create_list_type(list(a = Alias(String, "b"), c = Alias(String, "d"), b = Alias(String, "e"))),
    "also the name of another field"
  )
  testthat::expect_error(create_list_type(list(a = String), alias = "upper"), "one of")
})

testthat::test_that("Alias() is transparent outside list types", {
  testthat::expect_true(is_type("a", Alias(String, "x")))
  testthat::expect_error(check_type(1, Alias(String, "x")), "Expected string, got double")
  testthat::expect_equal(Alias(Scalar(Integer), "x")$name, "scalar<integer>")
})

testthat::test_that("optional and default fields stay optional inside wrappers", {
  T1 <- create_list_type(list(
    a = Alias(Default(Numeric, 1), "A"),
    b = Describe(Optional(Numeric), "b"),
    c = Coerce(Default(Integer, 2L)),
    d = Alias(Describe(Optional(String), "d"), "D")
  ))
  testthat::expect_equal(parse_as(list(), T1), list(a = 1, c = 2L))
})

testthat::test_that("object utilities keep aliases", {
  testthat::expect_equal(parse_as(list(userId = 2L), Pick(AliasUser, "user_id")), list(user_id = 2L))
  testthat::expect_equal(parse_as(list(name = "x"), Partial(AliasUser))$full_name, "x")
  testthat::expect_error(parse_as(list(name = 1), Partial(AliasUser)), "'name'")

  Strict <- Required(AliasUser)
  testthat::expect_error(parse_as(alias_input, Strict), "'nickName': Missing required field")

  Extended <- extend(AliasUser, list(extra_field = Numeric))
  parsed <- parse_as(c(alias_input, list(extraField = 3)), Extended)
  testthat::expect_equal(parsed$extra_field, 3)

  Merged <- AliasUser & create_list_type(list(zip_code = String))
  testthat::expect_equal(parse_as(c(alias_input, list(zipCode = "1")), Merged)$zip_code, "1")
})

testthat::test_that("data frame columns can be aliased", {
  DF <- create_dataframe_type(list(first_name = String, age = Numeric), alias = "pascal")
  df <- parse_as(data.frame(FirstName = c("a", "b"), Age = c(1, 2)), DF)
  testthat::expect_equal(names(df), c("first_name", "age"))
  testthat::expect_error(parse_as(data.frame(FirstName = 1), DF), "'FirstName': Expected string")
  testthat::expect_error(parse_as(data.frame(FirstName = 1), DF), "'Age': Missing required column")
})

# -----------------------------------------------------------------------------
# dump_as() and to_json()
# -----------------------------------------------------------------------------

testthat::test_that("dump_as() renames fields to their external names", {
  user <- parse_as(alias_input, AliasUser)
  dumped <- dump_as(user, AliasUser)
  testthat::expect_equal(names(dumped), c("userId", "name", "createdAt", "tags", "role"))
  testthat::expect_equal(names(dump_as(user, AliasUser, by_alias = FALSE)), names(user))
  testthat::expect_equal(parse_as(dumped, AliasUser), user)
})

testthat::test_that("dump_as() renames data frame columns unless by_alias = FALSE", {
  DF <- create_dataframe_type(list(first_name = String), alias = "camel")
  df <- data.frame(first_name = "a")
  testthat::expect_equal(names(dump_as(df, DF)), "firstName")
  testthat::expect_equal(names(dump_as(df, DF, by_alias = FALSE)), "first_name")
})

testthat::test_that("dump_as() handles NULL fields", {
  user <- c(parse_as(alias_input, AliasUser), list(nick_name = NULL))
  testthat::expect_true("nickName" %in% names(dump_as(user, AliasUser)))
  testthat::expect_false("nickName" %in% names(dump_as(user, AliasUser, exclude_null = TRUE)))
})

testthat::test_that("dump_as() validates the value first", {
  testthat::expect_error(dump_as(list(user_id = "x"), AliasUser), "Expected scalar<integer>")
  testthat::expect_error(dump_as(list(), AliasUser, by_alias = NA), "TRUE or FALSE")
  testthat::expect_error(dump_as(list(), AliasUser, exclude_null = "yes"), "TRUE or FALSE")
})

testthat::test_that("dump_as() removes brands and walks nested types", {
  Id <- Brand(Scalar(String), "Id")
  Item <- create_list_type(list(item_id = Id), alias = "camel")
  out <- dump_as(list(list(item_id = brand("a", Id))), ListOf(Item))
  testthat::expect_equal(out, list(list(itemId = "a")))
})

testthat::test_that("to_json() writes aliases and follows the type's shape", {
  testthat::skip_if_not_installed("jsonlite")
  user <- parse_as(alias_input, AliasUser)
  testthat::expect_equal(
    to_json(user, AliasUser),
    '{"userId":1,"name":"Ann","createdAt":"2024-01-05","tags":["a","b"],"role":"member"}'
  )
  one_tag <- user
  one_tag$tags <- "a"
  testthat::expect_match(to_json(one_tag, AliasUser), '"tags":["a"]', fixed = TRUE)
  testthat::expect_match(to_json(one_tag, AliasUser), '"userId":1,', fixed = TRUE)
})

testthat::test_that("to_json() writes nulls or leaves them out", {
  testthat::skip_if_not_installed("jsonlite")
  user <- c(parse_as(alias_input, AliasUser), list(nick_name = NULL))
  testthat::expect_match(to_json(user, AliasUser), '"nickName":null', fixed = TRUE)
  testthat::expect_false(grepl("nickName", to_json(user, AliasUser, exclude_null = TRUE)))
  testthat::expect_equal(to_json(NA_character_, Scalar(String)), "null")
})

testthat::test_that("to_json() round-trips through validate_json()", {
  testthat::skip_if_not_installed("jsonlite")
  user <- parse_as(alias_input, AliasUser)
  testthat::expect_equal(validate_json(to_json(user, AliasUser), AliasUser), user)
})

testthat::test_that("to_json() handles dates, data frames, records and unions", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::expect_equal(
    to_json(as.POSIXct("2024-01-05 10:00:00", tz = "UTC"), Scalar(DateTime)),
    '"2024-01-05T10:00:00"'
  )
  DF <- create_dataframe_type(list(first_name = String, age = Numeric), alias = "camel")
  df <- data.frame(first_name = c("a", "b"), age = c(3, 4))
  testthat::expect_equal(
    to_json(df, DF),
    '[{"firstName":"a","age":3},{"firstName":"b","age":4}]'
  )
  testthat::expect_equal(
    to_json(df, DF, by_alias = FALSE),
    '[{"first_name":"a","age":3},{"first_name":"b","age":4}]'
  )
  testthat::expect_equal(to_json(c(a = 1, b = 2), Record(String, Numeric)), '{"a":1,"b":2}')

  Circle <- create_list_type(list(kind = Literal("circle"), radius_cm = Scalar(Numeric)),
                             alias = "camel")
  Shape <- DiscriminatedUnion("kind", Circle)
  testthat::expect_equal(to_json(list(kind = "circle", radius_cm = 2), Shape),
                         '{"kind":"circle","radiusCm":2}')

  Nested <- create_list_type(list(inner_value = Optional(Circle)), alias = "camel")
  testthat::expect_equal(
    to_json(list(inner_value = list(kind = "circle", radius_cm = 1)), Nested),
    '{"innerValue":{"kind":"circle","radiusCm":1}}'
  )
  testthat::expect_equal(to_json(list(), create_list_type(list(a = Optional(String)))), "{}")
})

testthat::test_that("to_json() passes options to jsonlite", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::expect_match(to_json(list(a = 1), create_list_type(list(a = Numeric)), pretty = TRUE), "\n")
  testthat::expect_equal(to_json(pi, Scalar(Numeric), digits = 2), "3.14")
})

# -----------------------------------------------------------------------------
# JSON Schema
# -----------------------------------------------------------------------------

testthat::test_that("JSON Schema uses the external names", {
  schema <- to_json_schema(AliasUser)
  testthat::expect_equal(
    names(schema$properties),
    c("userId", "name", "createdAt", "nickName", "role", "tags")
  )
  testthat::expect_equal(unlist(schema$required), c("userId", "name", "createdAt", "tags"))
})

# -----------------------------------------------------------------------------
# Describe()
# -----------------------------------------------------------------------------

testthat::test_that("Describe() adds metadata to the JSON Schema", {
  City <- Describe(Scalar(String), "City name", title = "City", examples = c("Paris", "Oslo"))
  schema <- to_json_schema(City)
  testthat::expect_equal(schema$type, "string")
  testthat::expect_equal(schema$description, "City name")
  testthat::expect_equal(schema$title, "City")
  testthat::expect_equal(schema$examples, list("Paris", "Oslo"))

  Args <- Describe(create_list_type(list(city = City)), "Tool arguments")
  schema <- to_json_schema(Args)
  testthat::expect_equal(schema$description, "Tool arguments")
  testthat::expect_equal(schema$properties$city$description, "City name")
})

testthat::test_that("Describe() keeps the default in the schema", {
  Unit <- Describe(Default(Scalar(Enum("c", "f")), "c"), "Unit")
  schema <- to_json_schema(Unit)
  testthat::expect_equal(schema$default, "c")
  testthat::expect_equal(schema$description, "Unit")
})

testthat::test_that("Describe() does not change checking", {
  City <- Describe(Scalar(String), "City name")
  testthat::expect_equal(City$name, "scalar<string>")
  testthat::expect_true(is_type("Paris", City))
  testthat::expect_error(check_type(1, City), "Expected scalar<string>, got double")
  described_city %:% City %<-% "Oslo"
  testthat::expect_error(described_city <- 1, "scalar<string>")
})

testthat::test_that("Describe() validates its arguments", {
  testthat::expect_error(Describe(String), "at least one")
  testthat::expect_error(Describe(String, 1), "`description` must be a single string")
  testthat::expect_error(Describe(String, title = c("a", "b")), "`title` must be a single string")
  testthat::expect_error(Describe(Scalar(String), examples = list(1)), "example 1")
  testthat::expect_error(Describe(1, "x"), "Describe\\(\\) requires")
})

testthat::test_that("described types print their description and alias", {
  testthat::expect_output(print(Describe(String, "A name", title = "Name")), "description: A name")
  testthat::expect_output(print(Describe(String, "A name", title = "Name")), "title: Name")
  testthat::expect_output(print(Alias(String, "fullName")), "alias: fullName")
})

testthat::test_that("object utilities work on described list types", {
  Args <- Describe(create_list_type(list(a = String, b = Numeric)), "Args")
  testthat::expect_equal(names(object_spec(Pick(Args, "a"))), "a")
  testthat::expect_true(check_type(list(a = "x"), Partial(Args)))
})

# -----------------------------------------------------------------------------
# parse_env()
# -----------------------------------------------------------------------------

Settings <- create_list_type(list(
  db_url  = Scalar(Matches("^postgres://")),
  port    = Default(Scalar(Integer), 8080L),
  debug   = Default(Scalar(Bool), FALSE),
  ratio   = Optional(Scalar(Between(0, 1))),
  hosts   = Optional(String),
  start   = Optional(Scalar(Date)),
  api_key = Alias(Scalar(String), "API_KEY"),
  level   = Default(Enum("info", "debug"), "info")
))

testthat::test_that("parse_env() reads, converts and fills in settings", {
  testthat::skip_if_not_installed("jsonlite")
  env <- c(
    APP_DB_URL = "postgres://db", APP_PORT = "9000", APP_DEBUG = "Yes",
    APP_RATIO = "0.5", APP_HOSTS = '["a", "b"]', APP_START = "2024-01-02",
    API_KEY = "secret", APP_LEVEL = "debug", PATH = "/bin"
  )
  cfg <- parse_env(Settings, prefix = "APP_", env = env)
  testthat::expect_identical(cfg$port, 9000L)
  testthat::expect_true(cfg$debug)
  testthat::expect_equal(cfg$ratio, 0.5)
  testthat::expect_equal(cfg$hosts, c("a", "b"))
  testthat::expect_equal(cfg$start, as.Date("2024-01-02"))
  testthat::expect_equal(cfg$api_key, "secret")
  testthat::expect_equal(cfg$level, "debug")
})

testthat::test_that("parse_env() uses defaults and skips optional fields", {
  cfg <- parse_env(Settings, prefix = "APP_",
                   env = c(APP_DB_URL = "postgres://db", API_KEY = "k", APP_PORT = ""))
  testthat::expect_equal(names(cfg), c("db_url", "port", "debug", "api_key", "level"))
  testthat::expect_identical(cfg$port, 8080L)
  testthat::expect_false(cfg$debug)
})

testthat::test_that("parse_env() reads lenient logicals", {
  Flags <- create_list_type(list(flag = Scalar(Bool)))
  for (value in c("true", "TRUE", "yes", "on", "1", "y")) {
    testthat::expect_true(parse_env(Flags, env = c(FLAG = value))$flag)
  }
  for (value in c("false", "no", "off", "0", "N")) {
    testthat::expect_false(parse_env(Flags, env = c(FLAG = value))$flag)
  }
})

testthat::test_that("parse_env() keeps text for string fields", {
  Code <- create_list_type(list(code = Scalar(String)))
  testthat::expect_identical(parse_env(Code, env = c(CODE = "0123"))$code, "0123")
})

testthat::test_that("parse_env() reports every problem by variable name", {
  err <- tryCatch(
    parse_env(Settings, prefix = "APP_", env = c(
      APP_DB_URL = "mysql://db", APP_PORT = "80.5", APP_DEBUG = "maybe", APP_RATIO = "2"
    )),
    sicher_type_error = function(e) e
  )
  msg <- conditionMessage(err)
  testthat::expect_match(msg, "5 type errors")
  testthat::expect_match(msg, "'APP_DB_URL': expected matches")
  testthat::expect_match(msg, "'APP_PORT': cannot convert \"80.5\"")
  testthat::expect_match(msg, "'APP_DEBUG': cannot convert \"maybe\"")
  testthat::expect_match(msg, "'APP_RATIO': expected between\\[0, 1\\], but value \\[2\\]")
  testthat::expect_match(msg, "'API_KEY': Missing required environment variable")
})

testthat::test_that("parse_env() matches names case-insensitively by default", {
  env <- c(app_db_url = "postgres://db", api_key = "k")
  testthat::expect_equal(parse_env(Settings, prefix = "APP_", env = env)$db_url, "postgres://db")
  testthat::expect_error(
    parse_env(Settings, prefix = "APP_", env = env, case_sensitive = TRUE),
    "'APP_DB_URL': Missing required environment variable"
  )
  err <- tryCatch(
    parse_env(Settings, prefix = "APP_", env = c(app_db_url = "x", api_key = "k")),
    error = function(e) conditionMessage(e)
  )
  testthat::expect_match(err, "'app_db_url'")
})

testthat::test_that("parse_env() reads the real environment by default", {
  testthat::skip_if_not_installed("withr")
  withr::with_envvar(c(SICHER_TEST_DB_URL = "postgres://env", API_KEY = "k"), {
    cfg <- parse_env(Settings, prefix = "SICHER_TEST_")
    testthat::expect_equal(cfg$db_url, "postgres://env")
  })
})

testthat::test_that("parse_env() applies rules on the whole settings object", {
  Range <- Refine(
    create_list_type(list(low = Scalar(Integer), high = Scalar(Integer))),
    function(x) x$low < x$high, message = "low must be below high"
  )
  testthat::expect_equal(parse_env(Range, env = c(LOW = "1", HIGH = "5")), list(low = 1L, high = 5L))
  testthat::expect_error(parse_env(Range, env = c(LOW = "5", HIGH = "1")), "low must be below high")
})

testthat::test_that("parse_env() validates its arguments", {
  testthat::expect_error(parse_env(Numeric), "create_list_type")
  testthat::expect_error(parse_env(create_dataframe_type(list(a = String))), "create_list_type")
  testthat::expect_error(parse_env(Settings, prefix = 1), "`prefix`")
  testthat::expect_error(parse_env(Settings, env = "x"), "named character vector")
  testthat::expect_error(parse_env(Settings, case_sensitive = NA), "TRUE or FALSE")
})
