
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sicher 🔐

<!-- badges: start -->

<!-- badges: end -->

**sicher** (German for *safe* or *certain*) is an R package that brings
runtime type safety to R programming — inspired by TypeScript for
JavaScript. Declare types for your variables and have them enforced
automatically on every assignment, catching type errors early and making
your code more robust and self-documenting. 🛡️

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
#>   Received: [thirty]
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
#> Error: Type error in 'single': Expected scalar<numeric>, got double
#>   Received: [1, 2, 3]
```

#### 🔒 `Readonly()` — immutable variables

``` r
PI %:% Readonly(Double) %<-% 3.14159
PI <- 3.0   # Error: cannot reassign readonly variable
#> Error: Cannot reassign readonly variable 'PI'
```

#### ❓ `Optional()` — nullable values

``` r
middle_name %:% Optional(String) %<-% NULL   # OK
middle_name <- "Marie"                        # Also OK
middle_name <- 123                            # Error: not string or null
#> Error: Type error in 'middle_name': Expected string | null, got double
#>   Received: [123]
```

### 🔀 Union Types

Accept more than one type with `|`:

``` r
id %:% (String | Numeric) %<-% "user123"
id <- 456   # Also OK
id <- TRUE  # Error: not string or numeric
#> Error: Type error in 'id': Expected string | numeric, got bool
#>   Received: [TRUE]
```

### 📏 Size-constrained Vectors

Append `[n]` to any type to require an exact vector length:

``` r
coords %:% Numeric[3] %<-% c(1, 2, 3)
coords <- c(4, 5, 6)   # OK — same length
coords <- c(1, 2)      # Error: wrong length
#> Error: Type error in 'coords': Expected numeric[3], got double
#>   Received: [1, 2]
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
#> Error in type$check(value): object 'context' not found
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
#> Error: Type error in 'id': Expected integer, got string
#>   Received: [1, 2]
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
#> Error: Type error in 'todos': Expected list<{id: numeric, title: string, completed: bool}>, got list
#>   Received: [list(id = 1, title = "Buy milk", completed = FALSE), list(wrong = "shape")]
```

### 🛠️ Custom Types

Use `create_type()` to define your own validator with any predicate
function:

``` r
Positive <- create_type("positive", function(x) is.numeric(x) && all(x > 0))

value %:% Positive %<-% 5
value <- -1   # Error
#> Error: Type error in 'value': Expected positive, got double
#>   Received: [-1]
```

## 🌍 Real-World Example

``` r
# Catch bad payroll data early instead of getting silent NAs
calculate_mean_payroll <- function(salaries) {
  salaries %:% Numeric %<-% salaries
  mean(salaries)
}

calculate_mean_payroll(c(1800, 2300, 4000))   # Works fine
#> [1] 2700

calculate_mean_payroll(c(1800, "2300", 4000)) # Error: type mismatch
#> Error: Type error in 'salaries': Expected numeric, got string
#>   Received: [1800, 2300, 4000]
```

## 📚 Learn More

Full documentation and worked examples are available at the [package
website](https://feddelegrand7.github.io/sicher/).
