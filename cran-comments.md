── R CMD check results ──────────────────── sicher 0.1.0 ────
Duration: 9.9s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

- Adding the `Literal` type check
- Adding the `extend` method to extend a preexisting `list` schema.
- Adding `NonNA()` modifier — rejects any value that contains at least one `NA`.
- Adding `Between(min, max)` — closed-interval numeric range constraint.
- Adding `Matches(pattern)` — PCRE regex-constrained string type.
- Adding `NonEmpty()` modifier — rejects zero-length vectors, lists, and zero-row data frames.


── R CMD check results ───────────────────────── sicher 0.1.0 ──── Duration: 7.3s

❯ checking for future file timestamps ... NOTE unable to verify current time

0 errors ✔ \| 0 warnings ✔ \| 1 note ✖

-   Fixed the following issues:

#### Comments from Benjamin Altmann

Please omit the redundant "for R" at the end your title in the description.

Some code lines in examples are commented out. Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (\> 5 sec), can be wrapped in \donttest{}. -\> check_type.Rd I believe you want to show an error message? In that case, please wrap the example in try(). That way the error is thrown but the execution not halted.

Please fix and resubmit.
