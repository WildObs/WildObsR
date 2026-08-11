# Zachary Amir — R Coding Style

This guide follows the [tidyverse style guide](https://style.tidyverse.org/) except where
stated below. It documents only **deviations and additions** — anything tidyverse already
covers and I actually follow (spaces around infix operators, `snake_case`, no trailing
whitespace, `#` space after hash) is not restated here. Every rule below was derived by
counting occurrences across my real code: the `WildObsR` package (`R/`, 25 files, 2,770
code lines), two analysis-script repos (AWT invasives, SEA trophic cascades), and the
camDB final-report Rmd — ~24,000 code lines total. Percentages in parentheses are the
measured consistency. **Commenting is covered separately and completely in
[`zachary_amir_R_commenting_style_guide.md`](./zachary_amir_R_commenting_style_guide.md)
— follow that document for all comment questions; this one does not duplicate it.**

Two audiences differ legitimately and are split where they do: **package code**
(`WildObsR/R/`) and **analysis scripts** (`.R`/`.Rmd`/`.qmd` in analysis repos).

---

## 1. Syntax

### Use `<-` to define functions (69%, 35 of 51 defs in `R/`)

```r
find_closest_match <- function(missing_placename, reference_df) {
```
<sub>`R/find_closest_match.R`</sub>

Exceptions: 16 functions use `=`, concentrated in the oldest exported entry points
(`AUS_state_locator`, `matrix_generator`, `spatial_hexagon_generator`). Do not "fix"
these; match the file you are already in.

### Use `=` for assignment inside analysis scripts (83%; 3,182 vs 671)

```r
slurm = Sys.getenv("SLURM_ARRAY_TASK_ID")
slurm = as.numeric(slurm) #imports as character var, not numeric
```
<sub>`AWT_invasives_vs_natives/scripts/HPC_code/HPC_AWT_co-abundance_model.R:24`</sub>

In package code this splits 59/41 and is an **open question** — see §7.

### Never use right assignment `->` or `<<-`

0 uses of `->` and 0 of `<<-` across all package and analysis code.

### Use `%>%`, not `|>` (93%; 70 vs 5 in sources 1-4)

```r
s2 <- covs_dat %>%
  dplyr::group_by(dplyr::across(dplyr::all_of(row_col))) %>%
  dplyr::summarise(...) %>%
  tidyr::unnest(cols = date)
```
<sub>`R/matrix_generator.R:256`</sub>

The package re-exports `%>%` via `R/utils-pipe.R` and `@importFrom magrittr %>%`. Never
use the magrittr `.` placeholder (0 uses) or `%<>%` (0 uses).

### Spell out `TRUE` / `FALSE` in package code (96%; 130 vs 6)

```r
changed_deployment_id = FALSE
```
<sub>`R/AUS_state_locator.R:44`</sub>

In analysis scripts this drops to 60% and is an **open question** — see §7. The 86
abbreviated uses are almost all argument values: `na.rm = T`, `parallel = T`,
`row.names = F`.

### Use double quotes (97%; 6,166 vs 171)

Single quotes appear only when nesting inside a double-quoted string.

### Use explicit `return()` in package code

56 `return()` calls across 51 functions in `R/`; 0 uses of `invisible()`. Analysis
scripts barely define functions, so this is a package rule only.

### Use `1:n` indexing, not `seq_along()` (89%; 219 vs 28)

```r
for(i in 1:length(unique(deps$locationID))){
```
<sub>`R/update_temporally_overlapping_deployments.R:130`</sub>

This is a deliberate deviation from tidyverse, recorded because it is what the code
does. **Caveat worth knowing:** `1:length(x)` iterates backwards `1, 0` when `x` is
empty, which `seq_along(x)` avoids. Prefer `seq_along()` in genuinely new package code
where the vector can be empty; do not churn existing loops.

### Braces: opening brace on the same line, `} else` on the same line (>99%)

```r
if ("deployment_id" %in% colnames(deps)) {
  colnames(deps)[colnames(deps) == "deployment_id"] = "deploymentID"
  changed_deployment_id = TRUE
}
```
<sub>`R/AUS_state_locator.R:53`</sub>

Only 2 of 1,078 blocks put `{` on its own line; only 2 of 212 put `else` on its own line.

### Write `function(` with no space (99.4%; 157 vs 1)

### Indent with 2 spaces, never tabs (99%; 1,585 vs 21 four-space, 0 tabs)

### Target ~100 characters per line, not 80

Measured 90th percentile: **s1 = 90, s2 = 94, s3 = 99, s4 = 88**. 15-22% of lines exceed
80 characters and 5-9% exceed 100. Treat **100 as the soft limit** and 80 as a
non-binding preference; do not reflow existing code to 80.

---

## 2. Naming

### Functions: `snake_case`, verb-or-noun-first, no prefix (88%; 45 of 51)

```r
survey_and_deployment_generator, update_temporally_overlapping_deployments,
find_nearest_neighbor_distance, apply_schema_types, extract_metadata
```

Names are long and fully spelled out — no abbreviation. Two systematic exceptions:

**Domain acronyms keep their capitalisation** (5 functions):
`AUS_state_locator`, `IBRA_classification`, `UTM_coord_generator`,
`locationName_buffer_CAPAD`, `long_to_UTM_zone`.

**Camtrap DP field names keep their upstream camelCase** — `locationName`,
`deploymentID`, `projectName` — inside otherwise-snake_case names. Never rename a
Camtrap DP field to satisfy a naming rule.

### Objects: `snake_case` or short lowercase (92%; 2,837 of 3,067)

Short loop-local names (`t`, `du`, `new_d`, `prob_deps`, `s2`) are normal and accepted
in tight scopes. `dot.case` appears 197 times but only for JAGS/model settings
(`n.iter`, `n.burnin`) mirroring that API — keep it there, nowhere else.

### Arguments: lowercase or `snake_case` (95%; 230 of 243)

```r
matrix_generator(covs_dat, obs_dat, row_col, site_covs, obs_covs)
```

### Files in `R/`: one exported function per file, file named exactly after it

21 of 25 files hold exactly one exported function with a matching name. The exceptions
are deliberate:

| File | Role |
|---|---|
| `utils.R` | 14 unexported helpers, no `@export` |
| `utils-pipe.R` | `%>%` re-export only |
| `data.R` | dataset documentation only |
| `WildObsR-package.R` | package-level docs |

Helpers used by one function only live at the bottom of that function's file
(`normalize_values` in `extract_metadata.R`, `check_overlaps` in
`spatial_hexagon_generator.R`). Helpers used by two or more move to `utils.R`.

---

## 3. Roxygen (package code)

Tag order, as used: `@details`, `@param`, `@return`, `@examples`, `@author`,
`@keywords`, `@seealso`, `@importFrom`, `@export`.

Measured usage in `R/`: `@param` 86, `@importFrom` 55, `@return` 32, `@author` 25,
`@details` 23, `@export` 19, `@examples` 19, `@keywords` 18, `@seealso` 10.

### Always include `@author` (25 uses)

Unusual versus most R packages, and consistent here. Keep it.

### Write the title as a capitalised sentence with no trailing period

```r
#' Verify Column Matches Between Two Dataframes
```
<sub>`R/verify_col_match.R:1`</sub>

### Use `@details` with `\enumerate{}` for multi-branch logic

```r
#' @details
#' The function compares the values in a specified column from two dataframes:
#' \enumerate{
#'   \item If all values match, it prints a confirmation message ...
#'   \item If there are mismatches, the function checks the number of mismatched values ...
#' }
```
<sub>`R/verify_col_match.R:6`</sub>

### Make `@examples` runnable and self-contained — build the data inline

```r
#' @examples
#' df1 <- data.frame(ID = c(1, 2, 3, 4), Name = c("Alice", "Bob", "Charlie", "David"))
#' df2 <- data.frame(ID = c(1, 2, 4, 5), Name = c("Alice", "Bob", "Charlie", "Eve"))
#'
#' # Verify column match for 'ID'
#' verify_col_match(df1, df2, col = "ID")
```
<sub>`R/verify_col_match.R:21`</sub>

Note examples use `<-` even in files whose bodies use `=`.

### Declare every import with `@importFrom pkg fun1 fun2` — never `@import`

```r
#' @importFrom terra vect project aggregate disagg expanse centroids intersect extract ext crop buffer
```
<sub>`R/spatial_hexagon_generator.R`</sub>

---

## 4. Dependencies

### Package code: never call `library()`; always qualify with `pkg::` (100%; 0 vs 234)

Recurring packages in `R/`: `dplyr` (56), `terra` (43), `purrr` (28), `sf` (12),
`curl` (12), `jsonlite` (11), `httr` (10), `frictionless` (10), `mongolite` (8).

### Scripts: `library(tidyverse)` first, then domain packages, one per line with a `##` comment

```r
library(tidyverse)        ## For basic data wrangling
```
<sub>`AWT_invasives_vs_natives/scripts/step2_AWT_combine_results_from_HPC.Rmd:16`</sub>

`tidyverse` is first in all 5 script sources. Never use `require()` (0 uses).

### Use base R for string handling (93%; 1,345 vs 108 `stringr`)

`paste()` (425) and `paste0()` (116) are the workhorses; `grepl`/`gsub`/`sub` follow.
`sprintf()` is rare (5) and `glue()` unused (0) in sources 1-4.

### Use `data.frame()`, not `tibble()` (97%; 198 vs 7)

### Use `dplyr` verbs for table manipulation (63%; 190 vs 111 base)

Below 90%, so noted: `merge()`, `subset()` and `do.call()` still appear 111 times,
often mid-pipeline (`t = merge(s2, s, by = row_col)`, `R/matrix_generator.R:268`).
Acceptable, but prefer `dplyr::left_join()` in new code.

---

## 5. Behaviour

### Validate inputs at the top of the function, then `stop()` (62 `stop()` vs 17 `warning()` vs 7 `message()` in `R/`)

`stop()` is the default for anything that would corrupt downstream results;
`warning()` is for recoverable data-quality issues (empty or invariant covariates);
`message()` is rare. Use base conditions only — 0 uses of `cli::`, `rlang::abort()`, or
`stopifnot()`.

### Address the user in the second person and say what to do next

```r
stop("Latitude and/or longitude could not be found in your dataframe. Please make sure to provide lat...")
```
<sub>`R/AUS_state_locator.R`</sub>

```r
stop("You have provided site-level covariates that contain NA values in the covariates table and this...")
```
<sub>`R/matrix_generator.R`</sub>

The pattern is: **what went wrong → which object → what the user should do.**

### Build messages with `stop(paste(...))`, interpolating the offending value

```r
stop(paste("The latitude column you have specified:", lat_col,
           "is not present in the provided dataframe"))
```
<sub>`R/IBRA_classification.R`</sub>

10 of 10 constructed messages use `paste()`; `sprintf` and `glue` are unused here.

**Do not wrap `print()` inside `stop()`** — `stop(print("..."))` prints the message
twice and appears twice in `R/IBRA_classification.R`. Use `stop("...")`. This is a bug
to fix on sight, not a style to copy.

### Guard with `%in%`, `is.null()`, `is.na()` — in that order of frequency

`%in%` (471), `is.na()` (125), `is.null()` (91), `missing()` (7). `missing()` is used
only for arguments with no default; everything else defaults to `NULL` and is checked
with `is.null()`.

### Use `if` / `else if` chains, not `switch()` (919 vs 1)

`switch()` is used once in the entire corpus. `ifelse()` (14) and `dplyr::case_when()`
(1) are for vectorised recoding only.

---

## 6. Simplicity and factoring

These are observations from the measurements, offered as targets rather than
descriptions of current code.

### Split functions over ~250 lines

Six functions in `R/` exceed 180 lines: `matrix_generator` (641),
`survey_and_deployment_generator` (453), `resample_covariates_and_observations` (418),
`locationName_buffer_CAPAD` (254), `update_temporally_overlapping_deployments` (226),
`locationName_verification_CAPAD` (182). Each mixes validation, reshaping, and output
assembly in one body. Extract the validation block first — it is the most repeated and
most independently testable part.

### Factor out the repeated validation preamble

The "check column exists → rename to Camtrap DP name → set a `changed_*` flag → rename
back at the end" pattern runs 7 times inside `AUS_state_locator` alone (`changed_lat1`
… `changed_lon3`). A single helper — roughly
`standardise_coord_cols(deps)` returning the renamed data plus the original names —
would replace ~20 lines per call site. `rename_or_add_column()` in `utils.R` already
does part of this.

### Deduplicate the `frictionless::add_resource()` block

`dp = frictionless::add_resource(package = dp,` appears **8 times verbatim** in `R/`.
Wrap it in one helper taking the resource name and data.

### Prefer `purrr::map_*` or `*apply` over `for` when building a result vector

`for` loops (268) outnumber `*apply` (174) and `purrr` (24) in sources 1-4. Many loops
pre-allocate then fill by index, which `purrr::map_dfr()` or `vapply()` expresses in one
line. Loops that mutate shared state or need early exit should stay as loops.

---

## 7. Resolved conventions 

The items below were measured **under 60% consistency** or appeared only in the
weak-evidence sources. Each is now decided. Rationale is given where the decision
was not obvious.

### Formatting — delegated to `styler`

These three were near-coin-flips in the measurements, so they are settled by
adopting the tidyverse defaults and letting tooling enforce them. The point of a
formatting rule is that you never think about it again.

- **Assignment: `<-`, always.** `=` is reserved for function arguments. The 59%
  `=` measurement split by file, not by role — it is authorial drift, not a
  convention. `<-` is also the near-universal expectation in R packages.
- **Space before `{`: yes.** `if (nrow(x) == 0) {`, not `if (nrow(x) == 0){`.
- **Space after `if`/`for`/`while`: yes.** `if (`, not `if(`.

This inverts the 91-94% no-space habit in the analysis scripts. That habit is
adopted-against deliberately: `styler` implements tidyverse spacing out of the box,
so choosing tidyverse costs nothing ongoing, while codifying the no-space habit
would require either manual discipline or a custom transformer.

Run `styler::style_pkg()` — its tidyverse style handles both the spacing and the
`=` → `<-` conversion, and it correctly leaves `=` alone inside argument lists.
Style one file and inspect the diff before running it across the package.

### Logical values

- **Always `TRUE` / `FALSE`. Never `T` / `F`, including inside argument values.**
  So `na.rm = TRUE`, not `na.rm = T`. This is not aesthetic: `T` and `F` are
  ordinary variables and can be reassigned, so abbreviated forms are a live bug
  source. Package code is already 96% compliant; scripts now match.

### Iteration

- **No blanket rule. Choose by what the loop does:**
  - `for` when the body is more than a couple of lines, has side effects, or emits
    progress. It is also the most legible construct for our less-technical users.
  - `vapply()` / `lapply()` when building a vector or list from a short expression.
    Prefer `vapply()` where the return type is known — it fails loudly on surprises.
  - `purrr` only in analysis scripts that already use it. **Do not add `purrr` to
    the package `Imports` for stylistic reasons.**

### Pipes

- **`%>%` (magrittr), not `|>`.** `dplyr` is already imported, so `%>%` costs
  nothing, and it is what sources 1-4 actually use (70 vs 5). The 337 native-pipe
  uses in `code_data cleaning` are drift. Do not mix the two in one file. Switching
  the codebase to `|>` later is a defensible choice, but not one to make mid-release.

### Messages, warnings, and errors

- **`message()` is the correct mechanism for progress and status output** — not
  `cat()` or `print()`. The 306-vs-7 measurement is a volume artefact of a long
  pipeline script, not drift. Use it sparingly in package code; users can silence
  it with `suppressMessages()`, which is not true of `cat()`.
- **Construct message text with `sprintf()` when interpolating two or more values
  or formatting a number**; `paste0()` for simple concatenation. `sprintf("%d of
  %d deployments failed", n_bad, n_total)` is more readable than a `paste()` chain.
- **Do not use `glue()`.** It adds a dependency for no gain over `sprintf()`.
- **Do not use `stopifnot()`.** It produces errors like `is.character(x) is not
  TRUE`, which is meaningless to an ecologist. Validate with an explicit
  `if (...) stop("...")` and a message that says what to do about it. This is a
  hard rule — clear errors are a primary design constraint of this package.


### Rejected outright

- **`<<-` superassignment.** Makes data flow untraceable and triggers `R CMD check`
  global-variable notes. If a value must persist across calls, pass it explicitly
  or use an environment deliberately, with a comment saying why.
- **`data.table` / `setDT()`.** Mixing `data.table` and `dplyr` idioms in one
  codebase is the fastest route to unreadable code. If a genuine, measured
  bottleneck justifies it, isolate it inside a single function with a comment
  stating the benchmark that motivated it.
- **`dot.case` function names** (e.g. `clean.deployments()`). Beyond
  inconsistency, dots collide with S3 method dispatch — `clean.deployments` is
  indistinguishable from a `clean()` method for class `deployments`. Use
  `snake_case` for all functions and objects.
- **`UpperCamel` argument names.** `snake_case` for arguments, matching everything
  else.

### Enforcement

Formatting rules above are enforced by `styler` and checked in CI by `lintr`.
Minimum `.lintr` configuration:

- `assignment_linter()` — `<-` only
- `T_and_F_symbol_linter()` — no `T` / `F`
- `brace_linter()`, `paren_body_linter()` — spacing
- `undesirable_function_linter(c("stopifnot", "sapply", "attach"))`
- `object_name_linter(styles = "snake_case")`
- `line_length_linter(<your 90th-percentile value from §2>)`

Anything not covered by a linter is a review responsibility, not a CI gate.
