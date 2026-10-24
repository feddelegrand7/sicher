
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sicher

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sicher)](https://CRAN.R-project.org/package=sicher)
[![R-CMD-check](https://github.com/feddelegrand7/sicher/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/feddelegrand7/sicher/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

**sicher** (German for *safe* or *certain* and pronounced *zeesher*) is
an R package that brings runtime type safety to R programming. Declare
types for your variables and have them enforced automatically on every
assignment, catching type errors early and making your code more robust
and self-documenting. 🛡️

**sicher** implements runtime type safety in R by attaching types to
variables via active bindings rather than traditional attributes or
classes. When a variable is declared with a type (e.g.,
`x %:% Numeric %<-% 5`), it is replaced by an active binding that wraps
the value inside a closure with a private environment storing both the
value and its associated type.

Every read and write to the variable is intercepted: reads return the
stored value, while writes trigger validation through the type’s checker
function before updating. This design provides strong guarantees that
values cannot violate their declared types, enabling expressive and
composable type definitions (including unions, structured lists, and
data frame schemas).

## 📦 Installation

You can install the `sicher` package from `CRAN` with:

``` r
install.packages("sicher")
```

or install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("feddelegrand7/sicher")
```

## 🚀 Quick Start

``` r
library(sicher)

# Annotate a variable with a type, then assign a value
name %:% String  %<-% "Alice"
age  %:% Numeric %<-% 30

# The type is enforced on every subsequent assignment
age <- "thirty"   # Error: Type error
#> Error: Type error in 'age': Expected numeric, got string
#> Received: thirty
```

## ✨ Core Features

### 🧩 Built-in Types

sicher ships with a complete set of primitive and container types:

| Type          | Checks                                         |
|---------------|------------------------------------------------|
| `Integer`     | `is.integer()`                                 |
| `Double`      | `is.double()`                                  |
| `Numeric`     | `is.numeric()`                                 |
| `String`      | `is.character()`                               |
| `Bool`        | `is.logical()`                                 |
| `List`        | `is.list()`                                    |
| `DataFrame`   | `is.data.frame()`                              |
| `Function`    | `is.function()`                                |
| `Any`         | always passes                                  |
| `Null`        | `is.null()`                                    |
| `Date`        | `inherits(x, "Date")`                          |
| `DateTime`    | `inherits(x, "POSIXct")`                       |
| `Environment` | `is.environment()`                             |
| `Positive`    | numeric, all `> 0`, no `NA`                    |
| `NonNegative` | numeric, all `>= 0`, no `NA`                   |
| `Whole`       | numeric whole numbers (`3` or `3L`, not `3.5`) |

A few more are built with factory functions:

| Factory | Checks |
|----|----|
| `Factor(levels = NULL)` | a factor, optionally with exactly these levels |
| `MatrixOf(type = Any, nrow = NULL, ncol = NULL)` | a matrix with the given element type and dimensions |
| `InstanceOf(class)` | an object inheriting from `class` (S3, S4 or R6) |

``` r
x    %:% Integer   %<-% 42L
y    %:% Double    %<-% 3.14
flag %:% Bool      %<-% TRUE
df   %:% DataFrame %<-% data.frame(a = 1:3)

price %:% Positive %<-% 9.99
price <- 0      # Error: not > 0
#> Error: Type error in 'price': expected positive, but value [0] is not > 0

size %:% Factor(c("S", "M", "L")) %<-% factor("M", levels = c("S", "M", "L"))
model %:% InstanceOf("lm") %<-% lm(mpg ~ wt, data = mtcars)
```

### ⚙️ Operators

| Operator | Purpose                         |
|----------|---------------------------------|
| `%:%`    | Annotate a variable with a type |
| `%<-%`   | Assign a value (type-checked)   |

### 🔧 Type Modifiers

#### 1️⃣ `Scalar()` — single-element values

``` r
single %:% Scalar(Numeric) %<-% 42
single <- c(1, 2, 3)   # Error: length > 1
#> Error: Type error in 'single': Expected scalar<numeric>, got double of length 3
#> Received: [1, 2, 3]
```

#### 🔒 `Readonly()` — immutable variables

``` r
PI %:% Readonly(Double) %<-% 3.14159
PI <- 3.0   # Error: cannot reassign readonly variable
#> Error: Cannot reassign readonly variable 'PI'. Remove Readonly() from the type declaration if mutation is needed.
```

#### ❓ `Optional()` — nullable values

``` r
middle_name %:% Optional(String) %<-% NULL   # OK
middle_name <- "Marie"                        # Also OK
middle_name <- 123                            # Error: not string or null
#> Error: Type error in 'middle_name': Expected string | null, got double
#> Received: 123
```

#### 🚫 `NonNA()` — reject NA values

Prevents any `NA` from entering a typed variable — a common source of
silent bugs in data pipelines. The underlying type is validated first;
`NonNA` adds an extra NA-free guarantee on top.

``` r
salary %:% NonNA(Numeric) %<-% c(1800, 2300, 4000)
salary <- c(1800, NA, 4000)   # Error: value contains NA(s)
#> Error: Type error in 'salary': expected non_na<numeric> (no NA values), but value contains NA(s) (at position 2)
```

Composes freely with other modifiers:

``` r
clean_tag %:% NonNA(Scalar(String)) %<-% "admin"
clean_tag <- NA_character_   # Error
#> Error: Type error in 'clean_tag': expected non_na<scalar<string>> (no NA values), but value contains NA(s)
```

#### 📐 `Between(min, max)` — closed-interval range

Accepts numeric values within the closed interval `[min, max]`. Every
element of a vector must satisfy the bounds. `NA` values are always
rejected.

``` r
age   %:% Between(0, 150)   %<-% 30
score %:% Between(0.0, 1.0) %<-% 0.95
score <- 1.5    # Error: 1.5 is outside [0, 1]
#> Error: Type error in 'score': expected between[0, 1], but value [1.5] is outside [0, 1]
score <- NA     # Error: value contains NA(s)
#> Error: Type error in 'score': Expected between[0, 1], got bool
#> Received: NA
```

#### 🔍 `Matches(pattern)` — regex-constrained strings

Validates every element of a character vector against a Perl-compatible
regex. The pattern is compiled eagerly at construction time, so typos in
the regex are caught immediately. An empty `character(0)` passes
vacuously; pair with `NonEmpty()` to also require at least one element.

``` r
email %:% Matches("^[^@]+@[^@]+\\.[^@]+$") %<-% "user@example.com"
email <- "not-an-email"   # Error: does not match pattern
#> Error: Type error in 'email': expected matches("^[^@]+@[^@]+\.[^@]+$"), but value ["not-an-email"] does not match the pattern

hex_color %:% NonEmpty(Matches("^#[0-9A-Fa-f]{6}$")) %<-% c("#FF5733", "#1A2B3C")
```

#### 📭 `NonEmpty()` — reject empty vectors and lists

Rejects zero-length vectors, strings, and lists. For data frames it
checks `nrow > 0`. The base type is validated first, then the length is
checked.

``` r
tags %:% NonEmpty(String) %<-% c("r", "types")
tags <- character(0)   # Error: length > 0
#> Error: Type error in 'tags': expected non_empty<string> (length > 0), but got an empty value
tags <- "r"            # OK — length 1 is fine
```

#### 📏 `MinLength()`, `MaxLength()` and `Unique()`

Bound the number of elements (rows for data frames) or reject
duplicates. Lengths are counted with `length()`, like `NonEmpty()` and
`[n]`; to limit the number of characters in a string, use `Matches()` or
`Refine()`.

``` r
top3 %:% MaxLength(Numeric, 3) %<-% c(9, 8, 7)
top3 <- c(9, 8, 7, 6)   # Error: more than 3 elements
#> Error: Type error in 'top3': expected max_length<numeric, 3> (at most 3 element(s)), but got 4

ids %:% Unique(Integer) %<-% c(1L, 2L, 3L)
ids <- c(1L, 2L, 2L)    # Error: duplicated value
#> Error: Type error in 'ids': expected unique<integer>, but found duplicated value(s): [2] (at position 3)
```

#### ✅ `Refine()` — custom rules on top of a type

The base type is checked first, then your predicate runs, and its
message is used when it fails.

``` r
Password <- Refine(
  Scalar(String),
  function(x) nchar(x) >= 8,
  message = "must be at least 8 characters"
)

pw %:% Password %<-% "correct horse battery"
pw <- "short"   # Error: must be at least 8 characters
#> Error: Type error in 'pw': must be at least 8 characters
#> Received: short
```

#### 🧮 `Check()` — rules across fields

`Refine()` reports one problem about the whole value. `Check()` lets a
rule report any number of problems, each at the field it is about:

``` r
Booking <- Check(
  create_list_type(list(start = Date, end = Date, adults = Integer, kids = Integer)),
  function(x, ctx) {
    if (x$end < x$start) ctx$issue("must be after start", path = "end")
    if (x$adults == 0L && x$kids > 0L) ctx$issue("children need an adult", path = "kids")
  }
)

res <- safe_parse(
  list(start = as.Date("2024-05-10"), end = as.Date("2024-05-01"), adults = 0L, kids = 2L),
  Booking
)
res$issues[, c("path", "message")]
#>   path                message
#> 1  end    must be after start
#> 2 kids children need an adult
```

#### 🪄 `Default()` and `Coerce()` — transform values

`Default(type, value)` replaces `NULL` (or a missing list field) with a
default. `Coerce(type)` converts values with the matching `as.*()`
function before checking them, and reports values that cannot be
converted instead of silently producing `NA`. A typed variable stores
the transformed value.

``` r
retries %:% Default(Integer, 3L) %<-% NULL
retries
#> [1] 3

age %:% Coerce(Numeric) %<-% "42"
age             # now a number
#> [1] 42
age <- "forty"  # Error: cannot convert
#> Error: Type error in 'age': cannot convert ["forty"] to numeric

when %:% Coerce(Date) %<-% "2024-05-01"
class(when)
#> [1] "Date"
```

#### 🔁 `Transform()` and `Pipe()` — cleaning steps inside the type

`Transform(type, fn)` checks a value, then converts it with `fn`. Unlike
`Coerce()`, it always runs. `Pipe()` chains types, passing each parsed
value to the next:

``` r
Email <- Transform(Scalar(String), function(x) tolower(trimws(x)))
parse_as("  Ada@Example.COM ", Email)
#> [1] "ada@example.com"

Share <- Pipe(Coerce(Numeric), Transform(Numeric, function(x) x / 100), Between(0, 1))
parse_as("42", Share)
#> [1] 0.42
parse_as("420", Share)   # Error: outside [0, 1]
#> Error: Type error: expected between[0, 1], but value [4.2] is outside [0, 1]
```

#### 🕳️ `Nullable()` — present, but possibly `NULL`

`Optional()` fields may be left out of a list entirely. `Nullable()`
fields must be present but may hold `NULL`.

``` r
Profile <- create_list_type(list(
  name     = String,
  nickname = Optional(String),  # may be missing
  bio      = Nullable(String)   # must be present, may be NULL
))

profile %:% Profile %<-% list(name = "Ann", bio = NULL)
profile <- list(name = "Ann")   # Error: 'bio' is missing
#> Error: Type error in 'profile$bio': Missing required field (expected string | null)
```

### 🔀 Union Types

Accept more than one type with `|`:

``` r
id %:% (String | Numeric) %<-% "user123"
id <- 456   # Also OK
id <- TRUE  # Error: not string or numeric
#> Error: Type error in 'id': Expected string | numeric, got bool
#> Received: TRUE
```

### 🏷️ Enum Types

Restrict a value to an explicit set of allowed values with `Enum()`:

``` r
status %:% Enum("draft", "published", "archived") %<-% "draft"
status <- "published"   # OK
status <- "deleted"     # Error: not one of the allowed enum values
#> Error: Type error in 'status': Expected enum["draft", "published", "archived"], got string
#> Received: deleted

priority %:% Enum(1, 2, 3) %<-% 2
priority <- 3            # OK
priority <- 5            # Error
#> Error: Type error in 'priority': Expected enum[1, 2, 3], got double
#> Received: 5
```

### 🔒 Literal Types

Use `Literal()` when the value itself is part of the type:

``` r
direction %:% Literal("left", "right") %<-% "left"
direction <- "right"                 # OK
direction <- c("left", "right")    # Error: Literal only accepts scalar values
#> Error: Type error in 'direction': Expected literal["left", "right"], got string of length 2
#> Received: [left, right]

status_code %:% Literal(200, 404) %<-% 200
status_code <- 404                    # OK
status_code <- 200L                   # Error: 200L is an integer, not the double literal 200
#> Error: Type error in 'status_code': Expected literal[200, 404], got integer
#> Received: 200
```

### 📏 Size-constrained Vectors

Append `[n]` to any type to require an exact vector length:

``` r
coords %:% Numeric[3] %<-% c(1, 2, 3)
coords <- c(4, 5, 6)   # OK — same length
coords <- c(1, 2)      # Error: wrong length
#> Error: Type error in 'coords': Expected numeric[3], got double of length 2
#> Received: [1, 2]
```

### 📋 Structured List Types

Define object-like schemas with `create_list_type()`:

``` r
Person <- create_list_type(list(
  name  = String,
  age   = Numeric,
  email = Optional(String)   # nullable field
))

person %:% Person %<-% list(name = "Alice", age = 30, email = "alice@example.com")

person <- list(name = "Bob")   # Error: missing required field 'age'
#> Error: Type error in 'person$age': Missing required field (expected numeric)
```

By default, fields that are not part of the schema are rejected. Use
`extra = "allow"` to accept them unchecked or `extra = "drop"` to remove
them:

``` r
Loose <- create_list_type(list(name = String), extra = "drop")
parse_as(list(name = "Alice", internal_id = 99), Loose)
#> $name
#> [1] "Alice"
```

Use `extend()` when a new schema should reuse an existing list type
instead of redefining every field:

``` r
Employee <- extend(Person, list(
  role       = String,
  department = Optional(String)
))

employee %:% Employee %<-% list(
  name = "Alice",
  age = 30,
  role = "Engineer"
)

employee <- list(
  name = "Alice",
  age = 30,
  role = "Engineer",
  department = "Platform"
)
```

If `extra` reuses a field name from the base type, the new field
definition overrides the old one and `extend()` emits a warning so the
change is explicit.

### 🧰 Object Utilities

Derive new schemas from existing ones. They work on both list and data
frame types:

| Helper            | What it does                    |
|-------------------|---------------------------------|
| `Pick(type, ...)` | keep only some fields           |
| `Omit(type, ...)` | drop some fields                |
| `Partial(type)`   | make every field optional       |
| `Required(type)`  | make every field required       |
| `KeyOf(type)`     | an `Enum()` of the field names  |
| `A & B`           | merge the fields of two schemas |

``` r
User <- create_list_type(list(
  id    = Integer,
  name  = String,
  email = Optional(String)
))

UserPatch <- Partial(Omit(User, "id"))
patch %:% UserPatch %<-% list(email = "new@example.com")

Timestamps <- create_list_type(list(created = Date))
StoredUser <- User & Timestamps
StoredUser
#> <type: {id: integer, name: string, email?: string | null, created: date} >

sort_by %:% KeyOf(User) %<-% "name"
sort_by <- "password"   # Error: not a field of User
#> Error: Type error in 'sort_by': Expected enum["id", "name", "email"], got string
#> Received: password
```

### 🗄️ Data Frame Schemas

Validate column names and types with `create_dataframe_type()`:

``` r
UserTable <- create_dataframe_type(list(
  id       = Integer,
  username = String,
  active   = Bool
))

users %:% UserTable %<-% data.frame(
  id       = 1:2,
  username = c("alice", "bob"),
  active   = c(TRUE, FALSE)
)

# Wrong column type fails immediately
users <- data.frame(
  id       = c("1", "2"),   # Error: id must be integer
  username = c("alice", "bob"),
  active   = c(TRUE, FALSE)
)
#> Error: Type error in 'users$id': Expected integer, got string of length 2
#> Received: [1, 2]
```

### 📦 Homogeneous Lists with `ListOf()`

Validate every element of a list against the same type:

``` r
TodoItem <- create_list_type(list(
  id        = Numeric,
  title     = String,
  completed = Bool
))

TodoList <- ListOf(TodoItem)

todos %:% TodoList %<-% list(
  list(id = 1, title = "Buy milk",  completed = FALSE),
  list(id = 2, title = "Read book", completed = TRUE)
)

todos <- list(
  list(id = 1, title = "Buy milk", completed = FALSE),
  list(wrong = "shape")   # Error: element does not match TodoItem
)
#> Error: 4 type errors found:
#>   * Type error in 'todos[[2]]$id': Missing required field (expected numeric)
#>   * Type error in 'todos[[2]]$title': Missing required field (expected string)
#>   * Type error in 'todos[[2]]$completed': Missing required field (expected bool)
#>   * Type error in 'todos[[2]]$wrong': Unexpected field (valid fields: id, title, completed)
```

### 🧬 More Type Shapes

#### `Tuple()` — fixed positions

``` r
Point <- Tuple(Numeric, Numeric)
p %:% Point %<-% list(1.5, 2)
p <- list(1.5, "2")   # Error at [[2]]
#> Error: Type error in 'p[[2]]': Expected numeric, got string
#> Received: 2
```

#### `Record()` — named collections with any keys

``` r
scores %:% Record(String, Numeric) %<-% list(alice = 90, bob = 85)

Flags <- Record(Enum("debug", "verbose"), Bool)
flags %:% Flags %<-% list(debug = TRUE)
flags <- list(debug = TRUE, color = FALSE)   # Error: invalid key
#> Error: Type error in 'flags$color': Invalid key (expected enum["debug", "verbose"])
```

#### `DiscriminatedUnion()` — tagged unions

The discriminator picks which member to check, so errors point at the
right field instead of “nothing in the union matched”:

``` r
Circle <- create_list_type(list(radius = Numeric))
Square <- create_list_type(list(side = Numeric))
Shape  <- DiscriminatedUnion("kind", circle = Circle, square = Square)

shape %:% Shape %<-% list(kind = "circle", radius = 2)
shape <- list(kind = "square", radius = 3)   # Error: 'side' is missing
#> Error: 2 type errors found:
#>   * Type error in 'shape$side': Missing required field (expected numeric)
#>   * Type error in 'shape$radius': Unexpected field (valid fields: kind, side)
```

#### `Lazy()` — recursive types

``` r
Tree <- create_list_type(list(
  value    = Numeric,
  children = ListOf(Lazy(function() Tree, name = "Tree"))
))

tree %:% Tree %<-% list(value = 1, children = list(
  list(value = 2, children = list())
))
```

#### `Brand()` — nominal types

Two IDs that are both strings can still be told apart. `brand()` checks
a value and attaches the brand:

``` r
UserId  <- Brand(Scalar(String), "UserId")
OrderId <- Brand(Scalar(String), "OrderId")

current_user %:% UserId %<-% brand("u-123", UserId)
current_user <- brand("o-789", OrderId)   # Error: wrong brand
#> Error: Type error in 'current_user': Expected UserId<scalar<string>>, got a value branded as OrderId. Create branded values with brand(value, <type>)
#> Received: o-789
current_user <- "u-456"                   # Error: not branded
#> Error: Type error in 'current_user': Expected UserId<scalar<string>>, got an unbranded string. Create branded values with brand(value, <type>)
#> Received: u-456
```

#### `HasMethods()` — anything with the right methods

`InstanceOf("lm")` only accepts linear models. `HasMethods("predict")`
accepts any object that `predict()` works on:

``` r
Model <- HasMethods("predict", "residuals")

model %:% Model %<-% lm(mpg ~ wt, data = mtcars)
model <- glm(am ~ wt, data = mtcars, family = binomial)
model <- list()   # Error: no predict() or residuals() method
#> Error: Type error in 'model': Expected has_methods[predict, residuals], but class 'list' has no predict() or residuals() method
#> Received: (empty)
```

#### `FunctionOf()` — function signatures

The function is wrapped with `typed_function()`, so every later call is
checked too:

``` r
Formatter <- FunctionOf(list(x = Numeric), .return = String)

fmt %:% Formatter %<-% function(x) format(x, nsmall = 2)
fmt(3)
#> [1] "3.00"
fmt("a")                    # Error: argument x
#> Error: Type error in 'x': Expected numeric, got string
#> Received: a
fmt <- function(y) y        # Error: no parameter named x
#> Error: Type error in 'fmt': Expected function(x: numeric) => string, but the function has no parameter(s) named x
```

### 🎯 Exhaustive matching with `match_type()`

`match_type()` builds a function that checks a value and calls the
handler for its case. Cases are the tags of a `DiscriminatedUnion()`,
the values of an `Enum()` or `Literal()`, the levels of a `Factor()`, or
the members of a union. Handlers are checked when the matcher is
created, so a forgotten case fails right away, not on the rare call that
reaches it:

``` r
area <- match_type(Shape,
  circle = function(s) pi * s$radius^2,
  square = function(s) s$side^2
)
area(list(kind = "square", side = 3))
#> [1] 9

match_type(Shape, circle = function(s) pi * s$radius^2)   # Error: no handler for 'square'
#> Error: match_type() is not exhaustive: no handler for 'square'. Add a handler for each case or a .default handler

label <- match_type(Enum("active", "paused", "closed"),
  active = function(x) "Running",
  .default = function(x) "Not running"
)
label("paused")
#> [1] "Not running"
```

### 🧬 Generic Types

Generic types need no special syntax: write a function that takes types
and returns a type.

``` r
Paginated <- function(T) {
  create_list_type(list(items = ListOf(T), total = Scalar(Integer)))
}

page %:% Paginated(User) %<-% list(
  items = list(list(id = 1L, name = "Ann")),
  total = 1L
)
```

### 🛠️ Custom Types

Use `create_type()` to define your own validator with any predicate
function:

``` r
Even <- create_type("even", function(x) is.numeric(x) && all(x %% 2 == 0))

value %:% Even %<-% 4
value <- 3   # Error
#> Error: Type error in 'value': Expected even, got double
#> Received: 3
```

Use `Literal()` for exact scalar values, `Enum()` for membership in a
finite set that may also allow vectors of allowed values, and
`create_type()` when the rule is more general than membership in a
predefined list.

## 🔤 Typed Functions

Use `typed_function()` to wrap any function with runtime type checks on
its parameters and, optionally, its return value — a typed function
signature for R:

``` r
# Basic typed function — checks params and return type
add <- typed_function(
  function(x, y) x + y,
  params  = list(x = Numeric, y = Numeric),
  .return = Numeric
)

add(1, 2)     # Returns 3
#> [1] 3
add("a", 2)   # Error: Type error in 'x'
#> Error: Type error in 'x': Expected numeric, got string
#> Received: a
```

``` r
Num_not_inf <- create_type(
  name = "Num_not_inf", 
  checker = function(x) {
    is.numeric(x) && !is.infinite(x)
  }
)


divide <- function(a, b) {
  return(a / b)
}

divide_safe <- typed_function(
  fn = divide, 
  params = list(
    a = Num_not_inf, 
    b = Num_not_inf
  ), 
  .return = Num_not_inf
)


divide_safe(10, 2) # works normally
#> [1] 5
divide_safe(10, 0) # fails as 10/0 returns Inf
#> Error: Type error in '<return value>': Expected Num_not_inf, got double
#> Received: Inf
```

``` r
# Optional parameter
greet <- typed_function(
  function(name, title = NULL) {
    if (is.null(title)) paste("Hello,", name)
    else paste("Hello,", title, name)
  },
  params = list(name = String, title = Optional(String))
)

greet("Alice")                   # "Hello, Alice"
#> [1] "Hello, Alice"
greet("Alice", title = "Dr.")    # "Hello, Dr. Alice"
#> [1] "Hello, Dr. Alice"
greet("Alice", title = 42)       # Error: Type error in 'title'
#> Error: Type error in 'title': Expected string | null, got double
#> Received: 42
```

``` r
# Union type in params
describe <- typed_function(
  function(id) paste("ID:", id),
  params  = list(id = String | Numeric),
  .return = String
)

describe("abc")   # "ID: abc"
#> [1] "ID: abc"
describe(123)     # "ID: 123"
#> [1] "ID: 123"
describe(TRUE)    # Error: Type error in 'id'
#> Error: Type error in 'id': Expected string | numeric, got bool
#> Received: TRUE
```

You can also define an object to expect as a return value:

``` r
Person <- create_list_type(
  type_spec = list(
    name = String, 
    age = Numeric
  )
)

get_person_info_as_list <- function(name, age) {
  return(list(
    name = name, 
    age = age
  ))
}

get_person_info_as_message <- function(name, age) {
  return(
    paste("Hi my name is ", name, " I'm ", age, " years old")
  )
}

get_person_info_as_list_safe <- typed_function(
  fn = get_person_info_as_list, 
  params = list(name = String, age = Numeric), 
  .return = Person
)

get_person_info_as_list_safe(name = "Omar", age = 30) # works fine
#> $name
#> [1] "Omar"
#> 
#> $age
#> [1] 30

get_person_info_as_message_safe <- typed_function(
  fn = get_person_info_as_message, 
  params = list(name = String, age = Numeric), 
  .return = Person
)

# Should fail as the function does not return a Person list anymore
get_person_info_as_message_safe(name = "Omar", age = 30) 
#> Error: Type error in '<return value>': Expected {name: string, age: numeric}, got string
#> Received: Hi my name is  Omar  I'm  30  years old
```

## 🌍 Function usage

``` r
# Catch bad payroll data early instead of getting silent NAs
calculate_mean_payroll <- function(salaries) {
  salaries %:% Numeric %<-% salaries
  mean(salaries)
}

calculate_mean_payroll(c(1800, 2300, 4000))   # Works fine
#> [1] 2700

calculate_mean_payroll(c(1800, "2300", 4000)) # Error: type mismatch
#> Error: Type error in 'salaries': Expected numeric, got string of length 3
#> Received: [1800, 2300, 4000]
```

## 🧪 Validating Data

Typed variables are one way to use a type; you can also check values
directly:

| Function                  | Returns                                      |
|---------------------------|----------------------------------------------|
| `is_type(value, type)`    | `TRUE` or `FALSE`                            |
| `check_type(value, type)` | `TRUE`, or throws                            |
| `parse_as(value, type)`   | the parsed value, or throws                  |
| `safe_parse(value, type)` | a result with `success`, `data` and `issues` |

Errors report **every** problem, each with its location:

``` r
raw_todos <- list(
  list(id = 1, title = "Buy milk", completed = FALSE),
  list(id = "2", title = 3)
)

parse_as(raw_todos, ListOf(TodoItem), context = "todos")
#> Error: 3 type errors found:
#>   * Type error in 'todos[[2]]$id': Expected numeric, got string
#>   * Type error in 'todos[[2]]$title': Expected string, got double
#>   * Type error in 'todos[[2]]$completed': Missing required field (expected bool)

result <- safe_parse(raw_todos, ListOf(TodoItem))
result$success
#> [1] FALSE
result$issues
#>              path                                message expected     got
#> 1        [[2]]$id           Expected numeric, got string  numeric  string
#> 2     [[2]]$title            Expected string, got double   string  double
#> 3 [[2]]$completed Missing required field (expected bool)     bool missing
#>            code index
#> 1  invalid_type      
#> 2  invalid_type      
#> 3 missing_field
```

The error is a condition of class `sicher_type_error` with the same
issues in `e$issues`, so it can be handled with
`tryCatch(..., sicher_type_error = function(e) ...)`.

Misspelled field names, enum values and union tags come with a
suggestion:

``` r
Person <- create_list_type(list(name = String, status = Enum("active", "inactive")))
check_type(list(nmae = "Ada", status = "actve"), Person)
#> Error: 3 type errors found:
#>   * Type error in 'name': Missing required field (expected string)
#>   * Type error in 'status': Expected enum["active", "inactive"], got string. Did you mean "active"?
#>   * Type error in 'nmae': Unexpected field (valid fields: name, status). Did you mean 'name'?
```

### 🧾 Error reports for forms and APIs

`field_errors()` groups messages by field, ready to show next to Shiny
inputs. `problem_json()` builds an RFC 9457 “problem details” body for a
`400` or `422` response, with a JSON Pointer to each bad value:

``` r
Signup <- create_list_type(list(email = Matches("@"), age = Scalar(Between(18, 120))))
res <- safe_parse(list(email = "nope", age = 12), Signup)

field_errors(res)
#> $email
#> [1] "expected matches(\"@\"), but value [\"nope\"] does not match the pattern"
#> 
#> $age
#> [1] "expected between[18, 120], but value [12] is outside [18, 120]"
str(problem_json(res, status = 422))
#> List of 5
#>  $ type  : chr "about:blank"
#>  $ title : chr "Invalid request"
#>  $ status: int 422
#>  $ detail: chr "2 validation errors"
#>  $ errors:List of 2
#>   ..$ :List of 4
#>   .. ..$ path   : chr "email"
#>   .. ..$ pointer: chr "/email"
#>   .. ..$ message: chr "expected matches(\"@\"), but value [\"nope\"] does not match the pattern"
#>   .. ..$ code   : chr "invalid_string"
#>   ..$ :List of 4
#>   .. ..$ path   : chr "age"
#>   .. ..$ pointer: chr "/age"
#>   .. ..$ message: chr "expected between[18, 120], but value [12] is outside [18, 120]"
#>   .. ..$ code   : chr "out_of_range"
```

### 🌐 JSON

`validate_json()` parses JSON with jsonlite and checks it in one step,
which is handy for plumber request bodies. `to_json_schema()` exports a
type as JSON Schema:

``` r
Order <- create_list_type(list(
  id    = Scalar(Integer),
  items = ListOf(create_list_type(list(sku = Scalar(String), qty = Scalar(Whole))))
))

validate_json('{"id": 7, "items": [{"sku": "A1", "qty": 2}]}', Order)
#> $id
#> [1] 7
#> 
#> $items
#> $items[[1]]
#> $items[[1]]$sku
#> [1] "A1"
#> 
#> $items[[1]]$qty
#> [1] 2
validate_json('{"id": "7", "items": [{"sku": "A1"}]}', Order)
#> Error: 2 type errors found:
#>   * Type error in 'id': Expected scalar<integer>, got string
#>   * Type error in 'items[[1]]$qty': Missing required field (expected scalar<whole>)

cat(to_json_schema(Order, json = TRUE))
#> {
#>   "$schema": "https://json-schema.org/draft/2020-12/schema",
#>   "type": "object",
#>   "properties": {
#>     "id": {
#>       "type": "integer"
#>     },
#>     "items": {
#>       "type": "array",
#>       "items": {
#>         "type": "object",
#>         "properties": {
#>           "sku": {
#>             "type": "string"
#>           },
#>           "qty": {
#>             "type": "integer"
#>           }
#>         },
#>         "required": [
#>           "sku",
#>           "qty"
#>         ],
#>         "additionalProperties": false
#>       }
#>     }
#>   },
#>   "required": [
#>     "id",
#>     "items"
#>   ],
#>   "additionalProperties": false
#> }
```

### 🔎 Inspecting typed variables

``` r
age %:% Numeric %<-% 30
type_of(age)
#> <type: numeric >

backup_age %:% type_of(age) %<-% 31   # reuse a variable's type

untype(age)   # back to a regular variable
age <- "no longer checked"
```

## 🔍 Type inference

You can automatically infer the type for an R object using
`infer_type()`:

``` r
# Primitives
infer_type(42L)                # Integer
#> <type: integer >
infer_type(3.14)               # Double
#> <type: double >
infer_type("abc")              # String
#> <type: string >
infer_type(TRUE)               # Bool
#> <type: bool >
infer_type(NULL)               # Null
#> <type: null >
infer_type(function(x) x + 1)  # Function
#> <type: Function >

# Default mode infers types, not observed lengths
infer_type(c(1L, 2L, 3L))      # Integer
#> <type: integer >
infer_type(c(1, 2, 3))         # Double
#> <type: double >
infer_type(c("a", "b"))        # String
#> <type: string >
infer_type(c(TRUE, FALSE))     # Bool
#> <type: bool >

# Named and unnamed lists
infer_type(list(a = 1L, b = "x")) # create_list_type(list(a = Integer, b = String))
#> <type: {a: integer, b: string} >
infer_type(list(1L, 2L, 3L))   # ListOf(Integer)
#> <type: list<integer> >
infer_type(list(1L, "a"))      # List
#> <type: list >
infer_type(list(a = NULL, b = 1)) # create_list_type(list(a = Optional(Any), b = Double))
#> <type: {a?: any | null, b: double} >

# Data frames
infer_type(data.frame(x = 1:3, y = c("a", "b", "c"), stringsAsFactors = FALSE))
#> <type: data.frame{x: integer, y: string} >
# create_dataframe_type(list(x = Integer, y = String))

# Use strict = TRUE to also infer Scalar() and [n] size constraints
infer_type(42L, strict = TRUE)           # Scalar(Integer)
#> <type: scalar<integer> >
infer_type(c("a", "b"), strict = TRUE) # String[2]
#> <type: string[2] >
infer_type(data.frame(x = 1:3), strict = TRUE)
#> <type: data.frame{x: integer[3]} >
```

## Global Options

The behavior of **sicher** can be controlled globally using R’s
`options()` mechanism. These options allow you to switch between strict
type enforcement and a fully disabled mode depending on your use case.

### `sicher.mode`

Controls how typed variables (`%:%` and `%<-%`) and typed functions
(`typed_function()`) behave.

#### Values

- **`"on"`** *(default)*  
  Enables full runtime type enforcement.
  - Typed variables are implemented as active bindings  
  - All assignments are validated against their declared type  
  - Type violations result in immediate errors
- **`"warn"`**  
  Reports type violations as warnings instead of errors and keeps the
  assigned value. Useful when adding sicher to existing code: you see
  every violation without breaking anything. Applies to typed variables
  and typed functions (`Readonly()` is still enforced).
- **`"off"`**  
  Disables the type system entirely (`Readonly()` is still enforced on
  variables declared before the option was set).
  - Typed annotations are ignored  
  - Assignments behave like standard R assignments (`<-`)  
  - Typed functions call the original function without checking
    arguments or the return value  
  - No validation is performed  
  - No active bindings are created

### How to set options

You can configure the mode globally, for example, disabling strict type
checking with:

``` r
options(sicher.mode = "off")
```

Or enable strict typing with:

``` r
options(sicher.mode = "on") # this is the default
```

### When to use each mode

- **`"on"`** *(default)* **(strict mode)** Use in situations where
  correctness and safety are important:
  - Package development
  - Data validation pipelines
  - Production systems where type guarantees are desired
  - APIs or functions expecting structured inputs In this mode, sicher
    actively enforces types at runtime and prevents invalid assignments.
- **`"off"`** **(disabled mode)** Use when you want to bypass the type
  system entirely:
  - Performance-sensitive code where validation overhead is not desired
  - Debugging or testing environments where strict typing is temporarily
    unnecessary
  - Running code in environments where active bindings may interfere
    with other tools or workflows
  - Interoperability with code that assumes standard R assignment
    semantics In this mode, typed annotations are effectively ignored,
    and variables behave like regular R objects.

### Example

``` r
options(sicher.mode = "warn")
my_var_w %:% Numeric %<-% "10"   # warns, but assigns
#> Warning: Type error in 'my_var_w': Expected numeric, got string
#> Received: 10
options(sicher.mode = "on")
```

``` r
my_var_x %:% Numeric %<-% 10   # validated and bound with type enforcement

my_var_x <- "string" # trigger an error
#> Error: Type error in 'my_var_x': Expected numeric, got string
#> Received: string
```

``` r
options(sicher.mode = "off")
my_var_y %:% Numeric %<-% 10 
my_var_y <- "string" # nothing happens
```

It can also be temporarily scoped:

``` r
withr::with_options(
  list(sicher.mode = "off"),
  {
    my_var_y %:% Numeric %<-% 10
    my_var_y <- "string"
  }
)
```

## 📚 Learn More

Full documentation and worked examples are available at the [package
website](https://feddelegrand7.github.io/sicher/).

## Code of Conduct

Please note that the sicher project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
