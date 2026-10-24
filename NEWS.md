# sicher 0.3.0

## Field aliases and output

-   New `Alias(type, alias)`: gives a list field or data frame column an external name (a JSON key, a CSV header, an environment variable). Values are read from the alias, or from the R name when the alias is absent, and stored under the R name. Error messages point at the alias, since that is the key that appears in the data.
-   `create_list_type()` and `create_dataframe_type()` gain an `alias` argument to rename every field with a naming convention (`"camel"`, `"pascal"`, `"snake"`, `"kebab"`) or a function, e.g. `create_list_type(list(user_id = Integer), alias = "camel")` reads `userId`. Fields wrapped in `Alias()` keep their own alias.
-   Aliases are kept by `extend()`, `Pick()`, `Omit()`, `Partial()`, `Required()` and `&`.
-   New `dump_as(value, type, by_alias = TRUE, exclude_null = FALSE)`: the reverse of `parse_as()`. It checks the value, then renames fields to their aliases, optionally leaves out `NULL` fields and removes brands, so the result is ready to send to another system.
-   New `to_json(value, type, ...)`: the reverse of `validate_json()`. Values declared with `Scalar()` are written as JSON scalars and other vectors as arrays (even with one element), so the shape of the output follows the type and not the data. `NULL` and `NA` become `null`, dates and date-times are written in ISO 8601 format, and invalid values raise a type error instead of producing invalid JSON.
-   `to_json_schema()` names properties by their aliases.

## Describing types

-   New `Describe(type, description, title, examples)`: attaches documentation to a type without changing how it checks values. `to_json_schema()` writes it as `description`, `title` and `examples`, which makes the exported schema usable as API documentation or as a tool definition for a language model. Examples are checked against the type. Described types show their description when printed.

## Settings from environment variables

-   New `parse_env(type, prefix = "", env = Sys.getenv(), case_sensitive = FALSE)`: reads one environment variable per field of a list type (`prefix` + field name in upper case, or the field's `Alias()`), converts the text to the field's type (numbers, lenient logicals such as `yes`/`no`/`on`/`off`, dates, JSON arrays and objects) and checks it. Unset or empty variables fall back to `Default()` values, and every problem is reported at once with the name of the variable, so a misconfigured app fails at startup with a clear message.

## Matching, rules and transforms

-   New `match_type(type, ..., .default = NULL)`: builds a function that checks a value and calls the handler for its case, which is a `DiscriminatedUnion()` tag, an `Enum()` or `Literal()` value, a `Factor(levels = ...)` level or a member of a union. Missing handlers (without `.default`) and handlers for cases the type does not have are errors when the matcher is created.
-   New `Check(type, fn)`: rules that report any number of problems, each at the field it is about, through `ctx$issue(message, path)`. Suited to rules across fields such as "end must be after start".
-   New `Transform(type, fn, output = NULL)`: checks a value, then converts it. New `Pipe(...)`: chains types, passing each parsed value to the next.
-   New `HasMethods(...)`: accepts any object with methods for the given generics (S3, S4, or R6 fields), e.g. `HasMethods("predict")` for any model.

## Error messages and reports

-   Misspelled field names, data frame columns, `Enum()` and `Literal()` values and `DiscriminatedUnion()` tags now come with a suggestion, e.g. `Unexpected field (valid fields: name, age). Did you mean 'name'?`.
-   New `field_errors(x)`: the messages of a failed `safe_parse()` (or a caught type error) grouped by field, for showing errors next to form inputs.
-   New `problem_json(x, status = 400L, ...)`: an RFC 9457 "problem details" body for HTTP error responses, with a JSON Pointer to each bad value.

## Other changes

-   Fields wrapped in `Coerce()` are now treated like the type they wrap when deciding whether a field is optional, so `Coerce(Default(Integer, 1L))` and `Coerce(Optional(Integer))` fields may be left out of a list.
-   Issues for vectors and data frame columns now give the positions of the failing elements, for example `Type error in 'age': expected positive, but values [-2, -5] are not > 0 (at rows 2, 3)`. This applies to `Positive`, `NonNegative`, `Whole`, `Between()`, `NonNA()`, `Matches()` and `Unique()`. The issues data frame returned by `safe_parse()` gains an `index` list column with these positions.
-   Unions no longer repeat a member: `Integer | Numeric | Integer` is `integer | numeric`.

## Bug fixes

-   `typed_function()` now checks the default values of annotated parameters when the typed function is created, so `typed_function(function(n = "a") n, params = list(n = Integer))` is an error instead of returning `"a"` when `n` is omitted. Only constant defaults (such as `1L`, `"a"`, `c(1, 2)` or `NULL`) are checked; defaults computed from other arguments, like `n = length(x)`, are not evaluated. A `NULL` default now needs an `Optional()` or `Nullable()` type. The check follows `sicher.mode`.

-   `options(sicher.mode = "off")` now also turns off the checks of `typed_function()` and of typed variables that were declared before the option was set. Before, only new `%<-%` declarations were affected. In `"off"` mode a typed function calls the original function directly, so arguments are evaluated lazily as usual.
-   Arithmetic, comparisons and math functions on branded values now return plain values without the brand (before, `brand(1L, UserId) + 1L` kept the `UserId` class although the result was never checked). Combining two values with different brands, such as `user_id + order_id`, is now an error. Branded values get the extra class `"sicher_branded"`, placed after their brands. On R versions before 4.3.0, combining a branded value with an unbranded value of a class that has its own arithmetic (such as `Date`) gives an "Incompatible methods" warning.

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
