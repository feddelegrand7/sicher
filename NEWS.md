# sicher 0.2.0

## Validating data

-   New `is_type(value, type)`: returns `TRUE`/`FALSE` and never throws for invalid data.
-   New `parse_as(value, type)`: returns the parsed value or throws.
-   New `safe_parse(value, type)`: returns a `sicher_parse_result` with `success`, `data`, `issues` (a data frame with `path`, `message`, `expected`, `got` and `code`) and `error`.
-   New `validate_json(json, type)`: parses JSON with jsonlite and checks the result in one step (`safe = TRUE` returns a `safe_parse()` result).
-   New `to_json_schema(type)`: exports a type as a JSON Schema (draft 2020-12) list, or as a JSON string with `json = TRUE`.

## Better error messages

-   Errors now report **every** problem instead of stopping at the first one, and each problem includes its full location, for example `todos[[2]]$title` or `users$id`. `ListOf()` errors now say which element failed.
-   Missing fields and unexpected fields in list and data frame types are reported once per field, e.g. `Type error in 'person$age': Missing required field (expected numeric)`.
-   Type errors are now conditions of class `sicher_type_error` with an `issues` element, so they can be caught with `tryCatch(..., sicher_type_error = function(e) e$issues)`.
-   Using a type factory without calling it (e.g. `Factor` instead of `Factor()`) now gives a hint.

## New types

-   `Tuple(...)`: fixed-length lists with a type per position.
-   `Record(keys, values)`: named lists or named vectors with arbitrary keys (`Record<K, V>`, `z.record()`).
-   `DiscriminatedUnion(key, ...)`: tagged unions where the `key` field picks the member to check, so errors point at the right fields (`z.discriminatedUnion()`).
-   `Lazy(fn)`: recursive types such as trees (`z.lazy()`).
-   `InstanceOf(class)`: objects inheriting from an S3, S4 or R6 class (`z.instanceof()`).
-   `Brand(type, brand)` and `brand(value, type)`: nominal types, so a `UserId` cannot be used where an `OrderId` is expected.
-   `FunctionOf(params, .return)`: function signatures. Checked functions are wrapped with `typed_function()`, so their later calls are checked too.
-   `Factor(levels)`, `Date`, `DateTime`, `Environment` and `MatrixOf(type, nrow, ncol)` for common R objects. `MatrixOf` is named so it does not clash with the Matrix package.
-   `Positive`, `NonNegative` and `Whole` numeric types.
-   Intersection operator `&`. Two list types (or two data frame types) are merged into one type with the fields of both; any other combination must satisfy both sides.

## New modifiers

-   `Refine(type, predicate, message)`: adds a custom rule with a custom error message on top of a type.
-   `MinLength(type, n)`, `MaxLength(type, n)` and `Unique(type)`. Lengths are counted with `length()` (`nrow()` for data frames), like `NonEmpty()` and `[n]`.
-   `Nullable(type)`: like `Optional()`, but inside list and data frame types the field must be present (`field: T | null` versus `field?: T`).
-   `Default(type, value)`: replaces `NULL` with a default and fills in missing list fields.
-   `Coerce(type, using = NULL)`: converts values before checking them, for example `"42"` to `42`. A conversion that would introduce `NA` is reported as an error.

## Object utilities

-   `Pick()`, `Omit()`, `Partial()`, `Required()` and `KeyOf()` derive new schemas from list and data frame types.
-   `create_list_type()` and `create_dataframe_type()` gain an `extra` argument: `"error"` (default, unchanged behaviour), `"allow"` to keep unknown fields, or `"drop"` to remove them.
-   `extend()` now also works on data frame types and keeps the base type's `extra` setting.

## Typed variables and functions

-   Typed variables and `typed_function()` arguments and return values now store the *parsed* value. This only makes a difference for types that transform values (`Default()`, `Coerce()`, `extra = "drop"`, `FunctionOf()`); for every other type the stored value is exactly the assigned one.
-   New `type_of(x)` returns the declared type of a typed variable (`NULL` for regular variables), so one variable's type can be reused for another: `y %:% type_of(x) %<-% value`.
-   New `untype(x)` turns a typed variable back into a regular variable.
-   New `sicher.mode = "warn"` option: type violations are reported as warnings and the assigned value is kept, which helps when adding sicher to existing code. `Readonly()` is still enforced.

## Other changes

-   `Scalar()` now also accepts union types.
-   `infer_type()` now infers `Factor()`, `Date` and `DateTime` instead of `Any`.
-   Union type names are built from their members, so unions that contain intersections or other unions print correctly.
-   Readonly types now have a print method (`<type: readonly<string> >`).
-   The package now declares `R (>= 4.0.0)` (needed for `type_of()`) and imports `methods` (used by `InstanceOf()` for S4 classes). jsonlite is suggested for `validate_json()` and `to_json_schema(json = TRUE)`.
-   The hex logo was removed from the README and the website.

# sicher 0.1.1

-   Adding the `Literal` type check
-   Adding the `extend` method to extend a preexisting `list` schema.
-   Adding `NonNA()` modifier — rejects any value that contains at least one `NA`.
-   Adding `Between(min, max)` — closed-interval numeric range constraint.
-   Adding `Matches(pattern)` — PCRE regex-constrained string type.
-   Adding `NonEmpty()` modifier — rejects zero-length vectors, lists, and zero-row data frames.

# sicher 0.1.0

-   Initial CRAN submission.
