# CRAN Submission Comments

## Resubmission

This is a resubmission. In this version I have:

* Replaced `\dontrun{}` with `if(interactive()){}` for Shiny app examples,
  as these functions are intended for interactive use only.

## Recent improvements

* Added Google Fonts fallback to system fonts for offline/restricted environments
* Added folder name validation for Windows-incompatible characters
* Improved cross-platform path handling using `file.path()`

## Cross-platform bug fixes

* Fixed "invalid subscript type 'list'" error on Linux/Windows by adding defensive
  type coercion for column name inputs restored from saved state
* Added `safe_character()` helper to handle list-to-vector coercion consistently
* Fixed `intersect()` calls in state restoration that could preserve list structure
* Added UTF-8 encoding to CSV file reading for proper character handling on Windows
* Improved metrics list conversion to safely handle NULL and mixed types

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission.

## Note explanations

* NOTE 1: "New submission" - This is expected for a new package.
* NOTE 2: The NOTE about HTML Tidy is a local environment issue and does not affect
  the package functionality.

## Dependency note

This package depends on 'quallmer' which is available on CRAN.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.1
* GitHub Actions (ubuntu-latest), R release
* GitHub Actions (windows-latest), R release
* GitHub Actions (macOS-latest), R release

## Downstream dependencies

There are currently no downstream dependencies for this package.
