
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sicher <a><img src="man/figures/sicher_logo.png" align="right" height="200"/></a>

<!-- badges: start -->

<!-- badges: end -->

**sicher** (German for *safe* or *certain* and pronounced *zeesher*) is
an R package that brings runtime type safety to R programming, inspired
by TypeScript for JavaScript. Declare types for your variables and have
them enforced automatically on every assignment, catching type errors
early and making your code more robust and self-documenting. 🛡️

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

Install the development version from GitHub:

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

| Type        | Checks            |
|-------------|-------------------|
| `Integer`   | `is.integer()`    |
| `Double`    | `is.double()`     |
| `Numeric`   | `is.numeric()`    |
| `String`    | `is.character()`  |
| `Bool`      | `is.logical()`    |
| `List`      | `is.list()`       |
| `DataFrame` | `is.data.frame()` |
| `Function`  | `is.function()`   |
| `Any`       | always passes     |
| `Null`      | `is.null()`       |

``` r
x    %:% Integer   %<-% 42L
y    %:% Double    %<-% 3.14
flag %:% Bool      %<-% TRUE
df   %:% DataFrame %<-% data.frame(a = 1:3)
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

Use `Literal()` when the value itself is part of the type, similar to
TypeScript literal types:

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
#> Error: Type error: Expected {name: string, age: numeric, email?: string | null}, got list
#> Details: Missing required field(s): age (expected fields: name, age)
#> Received: list with fields: [name]
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
#> Error: Type error in 'id': Expected integer, got string of length 2
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
#> Error: Type error in 'todos': Expected list<{id: numeric, title: string, completed: bool}>, got list of length 2
#> Received: list of length 2
```

### 🛠️ Custom Types

Use `create_type()` to define your own validator with any predicate
function:

``` r
Positive <- create_type("positive", function(x) is.numeric(x) && all(x > 0))

value %:% Positive %<-% 5
value <- -1   # Error
#> Error: Type error in 'value': Expected positive, got double
#> Received: -1
```

Use `Literal()` for TypeScript-style exact scalar values, `Enum()` for
membership in a finite set that may also allow vectors of allowed
values, and `create_type()` when the rule is more general than
membership in a predefined list.

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

Controls how typed assignments (`%:%` and `%<-%`) behave.

#### Values

- **`"on"`** *(default)*  
  Enables full runtime type enforcement.
  - Typed variables are implemented as active bindings  
  - All assignments are validated against their declared type  
  - Type violations result in immediate errors
- **`"off"`**  
  Disables the type system entirely.
  - Typed annotations are ignored  
  - Assignments behave like standard R assignments (`<-`)  
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
