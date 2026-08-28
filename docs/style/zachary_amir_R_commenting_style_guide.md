# R Commenting & Style Guide (camDB / WildObs)

## Purpose

This guide defines how comments and structure should look in R code I write or
that is written for me (including by Claude Code). The goal is one throughline:

> **A reader who knows English but not R should be able to follow the logic top
> to bottom.** Comments are dense, plain-spoken, and intent-first.

When in doubt, narrate *intent* ("give an informative error message"), not
*syntax* ("call stop with a string").

---

## The core grievance to avoid

Do **not** front-load explanation into verbose docstring-style headers and then
leave the function body sparse. The roxygen header is a formal contract and stays
complete (see below) — but the *explanation of how the logic works* belongs
**inline, next to the code it describes**, in plain English. Narrative in the
wrong register (formal, full-sentence, capitalized, syntax-restating) or the
wrong place (hoisted to the top instead of beside the line) is the failure mode.

---

## 1. Two registers: the header vs. the body

**Roxygen header — keep it formal and complete.** `#'` title, `@description`
(spell out non-obvious logic, priority rules, ordering guarantees), `@param`,
`@return` (note hard-`stop()` conditions). This does not collapse. It is the
contract.

**Function body — plain-English inline narration.** Everything inside the braces
is conversational, lowercase, intent-first, positioned *above* the line it
describes.

---

## 2. The comment ladder: `#`, `##`, `###`

Three levels, escalating in scope:

- **`#` — line-level translation ("what").** The floor. *Almost every line* gets
  one, even simple code. A piped `dplyr` chain gets a `#` above each step in the
  pipe. This is near-universal and applies regardless of complexity.
- **`##` — sub-step or conceptual note ("why").** Explains a decision, a gotcha,
  or groups a few related lines. Used more heavily as code gets complicated.
- **`### Section header ----` — major section.** Plain-English, sentence-style
  ("First, extract the PI's organization as lead"), with a trailing `----` so it
  shows in the RStudio outline. Reserved for important/complicated code with
  genuinely distinct task-blocks; not needed for simple straight-line scripts.

The `#` floor is never removed by adding higher levels — complexity *adds*
structure on top of baseline narration, it doesn't replace it. But see §4a: the
floor means *one clear line*, not a paragraph, and never the same fact twice.

### Multi-line comment blocks: `###` opener, `##` body

A comment block of **three or more lines** opens with `###` and continues with
`##`. The `###` line is the heading; the `##` lines are the explanation.

```r
### statewide RE + biodiversity status shapefile
## despite the folder name this is RE version 12.1 and is NOT remnant-only:
## cleared land is explicitly mapped, so absence of vegetation is data, not a hole
re_path <- paste0("/Users/zachary_amir/Dropbox/ECL spatial layers repository/",
                  "Australian spatial layers GIS data/QLD/QLD regional ecosystems 2019/",
                  "Biodiversity_status_of_remnant_regional_ecosystems.shp")
```

**Two-line blocks stay all `##`:**

```r
## only keep occurrence records from this year onwards
## single point of control -- change it here and the whole workflow follows
year_min <- 2000
```

The exception is a two-line block whose first line is a genuine heading — most
often a question the second line answers. Then use `###` / `##`:

```r
### What to do with records whose coordinate uncertainty is missing entirely?
## TRUE means keeping them, because dropping them may remove older data
keep_missing_coord_uncertainty <- TRUE
```

Write the `###` opener as a heading, not a sentence fragment: a noun phrase
("Cut lines that sever Cooloola from the mainland") or an imperative
("Establish landmarks to check the Cooloola boundary is behaving").

### Comments inside multi-line pipe chains

A multi-line `|>` chain gets the same `#` floor as any other multi-line block:
one comment per pipe step, sitting directly above the step it describes, with
an `##` header above the whole chain naming the overall goal.

```r
## Load all data packages into a list
dp_list <- dp_paths |>
  # save each package ID as the name in the list
  set_names(dp_import) |>
  # and use frictionless's read_package across all paths
  map(read_package)
```

**Do not** hoist all the step comments above the chain as a block, even though
the code below is otherwise unchanged:

```r
# DON'T -- comments detached from the steps they explain
## Load all data packages into a list
# save each package ID as the name in the list
# and use frictionless's read_package across all paths
dp_list <- dp_paths |>
  set_names(dp_import) |>
  map(read_package)
```

The hoisted version forces the reader to hold all three comments in their head
before matching them back to the pipe steps below. Each comment stays glued to
its own line.

### Never use banner rules

**Never** mark a section with a run of hashes or dashes:

```r
###############################################################################
## DON'T DO THIS
###############################################################################
```

This is not an aesthetic preference. RStudio's **Jump To** menu — the section
navigator in the editor status bar — builds a section from any comment line
ending in four or more `-`, `=`, or `#`. A banner rule therefore registers as a
nameless section and fills the navigator with meaningless entries. The navigator
is the primary way these scripts get moved around in, so this is a hard rule.

For the same reason, a bare `###` heading with **no** trailing `----` is safe and
does *not* create a navigator entry. That is exactly why `###` works as a
multi-line block opener without polluting the outline. Only `### Name ----`
creates a section, and it should always have a real name.

### The spacer stack

Before a `###` section, use a three-line ramp for vertical breathing room,
preceded by **two blank lines**:

```r


#
##
### First, extract the PI's organization as lead (require exactly one) ----
```

This is a deliberate rhythm for sectioning load-bearing logic. Optional for
simple code.

---

## 3. Closing-brace comments

Tag closing braces with `# end <what>` so nested scopes stay legible.

- **Control-flow braces** (`if`, `for`, `while`, function): tagged **by
  default**.
- **Once anything is nested inside anything else: mandatory.** Nesting is exactly
  where you lose track of which `}` closes what.
- **Deeply nested data structures (3+ brackets deep):** also tag the closer —
  e.g. a `list(list(list(...)))`. (Shallow `list()` literals stay untagged; only
  3-deep-or-more earns a tag.)

Examples of the voice: `# end null condition`, `# end per contrib`,
`# end PI ROR condition`, `# end function`.

---

## 4. Narrate by de-densifying

Prefer one statement per line so each gets its own comment, over terse
multi-statement lines. Readability of the narration beats compactness of the
code.

```r
# DON'T — compact, under-narrated
if (...) { pi_entry <- c; break }

# DO — each step narrated
# check if PI is in the role
if (!is.null(c$role) && tolower(c$role) == "principalinvestigator") {
  # if yes, save that entry
  pi_entry <- c
  # and end the loop
  break
} # end condition
```

---

## 4a. Say it once, and say it where it happens

Comment density is not the goal — comprehension is. Repetition is what produces
reader fatigue, so:

- **Do not comment on code because of something that happens elsewhere in the
  script.** If the note is about how a value gets *used*, it belongs at the point
  of use, not at the point of definition. A config file should say what a knob
  is; the script that turns the knob explains what turning it does.
- **Never state the same fact twice** in one script. The second statement is the
  one to delete.
- **No changelog comments.** A comment describes what the code does now and why
  — never what it used to do, what changed, or what a past version of an API,
  function, or argument used to mean. If that history matters, it belongs in a
  dedicated changelog file (`NEWS.md`, a commit message, a GitHub issue), never
  inline. Apply the history test: if a comment only makes sense to someone who
  already knows the "before" state, it fails and gets rewritten as a plain
  statement of the current contract.

```r
# DON'T -- changelog buried in a code comment
## WildObsR changed this argument's meaning: it used to take the hexagon apothem
## in metres, and now takes the cell AREA in square metres, labelling the scale
## itself. Passing the old 930.6 is silently accepted and builds 930.6 m2 cells,
## a 16 m apothem, which is roughly one cell per camera. 3e6 m2 gives the 3 km2
## cell this analysis has always used, and resolves to the same 930.6 m apothem.
scales <- c(3e6)

# DO -- states the current contract only
## sampling unit: 3 km2 hexagons (spatial_hexagon_generator takes cell area in m2)
scales <- c(3e6)
```

- **A long rationale does not belong in the code.** If a decision needs several
  paragraphs to defend, put it in `CLAUDE.md` or a `README.md` and leave a one-line
  pointer:

```r
### distance (metres) to bridge gaps between sand masses before splitting into components
## Tested at 0 m: all five islands separate cleanly at correct areas.
## See CLAUDE.md decision 2 for more info
bridge_distance_m <- 0
```

Exceptions exist — a genuinely surprising gotcha earns its inline paragraph — but
it must be succinct, and it must be the only place that fact is stated.

- **Narrated failure scenarios follow the per-line rule, same as pipe chains.**
  A comment describing what a check guards against sits directly above the line
  it explains — not hoisted above a multi-line `if`/`stop()` block as a single
  block comment covering the whole thing.

```r
## Prepare to merge deployments and covariates together,
# but first make sure equal deploymentID values (i.e., the foreign key)
if (length(setdiff(deps$deploymentID, covs$deploymentID)) +
    length(setdiff(covs$deploymentID, deps$deploymentID)) != 0) {
  ## hard stop if there is a deployments mismatch
  stop("Mismatched deploymentID values between covariates and deployments in: ",
       dp$name,
       "\nRe-curate that data package before including it here.")
} # end deploymentID check
```

Abbreviate freely once a term is established: `REs`, `ALA`, `RE version 12.1`.

---

## 5. Comment position

Default position is **on their own line, above the code**. Prose explanation is
always above.

**Aligned trailing comments are the correct form for list-like blocks**, where
each line is a short label on one item and the block reads as a table. Use a
single `#`, and align the comments into a column:

```r
## Load libraries
library(tidyverse) # For basic data wrangling
library(sf)        # For everything spatial
library(here)      # For project-relative paths so this runs on any machine
library(galah)     # For pulling occurrence records out of the ALA
```

```r
## build bounding box coordinates in EPSG:4326
xmin <- 152.50     # Cooloola western tip
xmax <- 153.60     # Point Lookout eastern tip
ymin <- -28.00     # S. Stradbroke southern tip
ymax <- -24.60     # K'gari northern tip
```

Note what those trailing comments do: they carry **information the code cannot**
— which geographic feature sets each bound. A trailing comment that restates the
code (`xmin <- 152.50 # set xmin`) is worse than none.

A trailing comment is also the right place for a short human annotation on a
decision, initialled:

```r
cooloola_boundary_approved <- TRUE # ZDA interactively did this w/ Claude Code
```

Anything longer than a short label goes above the line, not trailing.

---

## 6. Voice

- **Casual, conversational, imperative.** "grab the ROR", "init empty list",
  "skip to the next", "cant have null contribs this late in the game".
- **Correct spelling.** The register is casual but the spelling is right — no
  `partern`, `funct`, `orchid`, `dont`. Casual ≠ sloppy.
- **Editorialize where it helps.** Comments may carry reasoning and a little
  voice ("lead org wins", "schema forbids one org holding two roles"), not just
  mechanical translation.
- **Lowercase, minimal terminal punctuation** for body comments — they read as
  running annotation.

---

## 7. Error messages

Hard `stop()` for unexpected failures. Write the message to be *useful at 9pm six
months from now*:

- **Lead with the offending identifier** (`dp$id`).
- **Name the function inline** so the source is obvious from the log alone.
- **Two-line structure via `\n`:** line 1 = what happened; line 2 = what to do
  about it.

```r
stop(dp$id, " returned empty dp[['contributors']] during RAiD creation.\n",
     "Ensure contributors are properly formatted prior to PID minting.")
```

---

## 8. TODOs live inline

Put `## TODO:` exactly where the relevant code should change or be added, not in
a header or external tracker. The location *is* the information.

```r
pi_ror <- pi_entry$ROR
# ensure its valid
## TODO: Probably just replace this with wildobs_ror to ensure code runs
if (!valid_ror(pi_ror)) { ... }
```

---

## 9. Simplicity over cleverness

Prefer explicit, debuggable branches over terse idioms — especially when the
explicit form lets you attach a real error message. Example: an explicit
`is.null()` check with a hard `stop()` beats `x %||% list()` silently swallowing
the empty case, because the explicit form fails loudly and tells the reader why.

---

## 10. Inherited conventions

Native pipe `|>`; `snake_case`; `### Name ----` section headers where used;
`message()` not `cat()` (except HPC scripts, where `cat()` goes to `.out`); CSS
colors only in DT/htmlwidgets.

Section headers are **three** hashes, not four — see §2. Four hashes still create
a valid RStudio section, but three is what the codebase uses and mixing the two
splits the outline visually for no reason.

---

## 11. Analysis script file headers

An analysis script opens with a `###` title line carrying the filename and a
one-line purpose, then `##` prose. No banner rules above or below it.

```r
### 00_config.R -- every knob for the SEQ sand mass species detection map
##
## Scientific motivation:
##
## The southeast Queensland sand masses -- K'gari, Cooloola, Bribie, Mulgumpin
## ... [ prose ] ...
##
## This file defines objects only. It runs no analysis and writes no files, so
## it is safe to source at the top of any other script.
```

Keep the header to what a reader needs before reading the code: what the script
is for, and any constraint on how it may be run. Method detail and justification
go in `CLAUDE.md` or a `README.md` (§4a), not here.

---

## Appendix: worked example (heavy register)

A reference for what the full treatment looks like end-to-end — roxygen contract
on top, plain-English narration throughout the body, the comment ladder, spacer
stacks, and closing-brace tags.

```r
#' Build the RAiD organisation block from a DP's contributors
#'
#' @description Collects distinct organisation RORs across all contributors plus
#'   the WildObs lead org, deduplicated by ROR, and assigns roles by priority so
#'   an ROR emitted at higher priority is never repeated:
#'   (1) the PI's org -> Lead Research Organisation (182), exactly one;
#'   (2) the WildObs lead org -> Facility (187), unless it equals the PI org;
#'   (3) every remaining distinct contributor org -> Partner Organisation (184).
#' @param dp list. Data package object.
#' @param wildobs_ror character. ROR URL for the WildObs lead organisation.
#' @param start_date_str character. ISO date used for the role startDate.
#' @return list of organisation entries. Hard `stop()`s if the PI has no ROR.
.build_raid_organisations <- function(dp, wildobs_ror, start_date_str) {
  ## Establish RAiD organisation.role vocabulary (schemaUri 359 for all roles)
  # Lead Research Organisation
  role_lead     <- "https://vocabulary.raid.org/organisation.role.schema/182"
  # Facility
  role_facility <- "https://vocabulary.raid.org/organisation.role.schema/187"
  # Partner Organisation
  role_partner  <- "https://vocabulary.raid.org/organisation.role.schema/184"
  # and the schema that defines the roles
  role_schema   <- "https://vocabulary.raid.org/organisation.role.schema/359"

  ## a quick helper to store one organisation list carrying a single role
  make_org <- function(ror, role_id) {
    list(
      id        = ror,
      schemaUri = "https://ror.org/",
      # role can be a list of lists
      role      = list(list(
        id        = role_id,
        schemaUri = role_schema,
        startDate = start_date_str
      )) # end role list
    ) # end org list
  } # end org helper

  ## a quick helper to determine if a ROR is usable
  # i.e., not NULL, not NA, and has characters
  valid_ror <- function(r) !is.null(r) && !is.na(r) && nzchar(r)

  ## grab all contributors from this data package
  contribs <- dp$contributors
  # cant have null contribs this late in the game
  if (is.null(contribs)) {
    stop(dp$id, " returned empty dp[['contributors']] during RAiD creation.\n",
         "Ensure contributors are properly formatted prior to PID minting.")
  } # end null contrib

  #
  ##
  ### First, extract the PI's organization as lead (require exactly one) ----

  # init empty value
  pi_entry <- NULL
  # for each contributor
  for (c in contribs) {
    # check if PI is in the role
    if (!is.null(c$role) && tolower(c$role) == "principalinvestigator") {
      # if yes, save that entry
      pi_entry <- c
      # and end the loop
      break
    } # end condition
  } # end per contrib

  ## validate we got a good PI (non null)
  if (is.null(pi_entry)) {
    stop(dp$id, " had no contributor flagged principalInvestigator in .build_raid_organisations.\n",
         "Lead organisation cannot be assigned")
  } # end null PI condition

  ## extract the ROR and ensure its valid
  pi_ror <- pi_entry$ROR
  ## TODO: probably just replace this with wildobs_ror to ensure code runs
  if (!valid_ror(pi_ror)) {
    stop(dp$id, " principalInvestigator has no ROR in .build_raid_organisations.\n",
         "Lead organisation cannot be assigned")
  } # end null PI ROR condition

  ## save this ROR as first entry in org list w/ lead org schema value
  organisation_block <- list(make_org(pi_ror, role_lead))
  ## init a vector of RORs already added to the block
  seen <- pi_ror

  #
  ##
  ### Second, ensure WildObs doesnt clash w/ lead org ----
  ## schema forbids one org w/ two roles, so lead org wins

  # if WildObs ROR is valid AND its not already in the seen vector
  if (valid_ror(wildobs_ror) && !(wildobs_ror %in% seen)) {
    # add it as a new org to the block w/ facility role
    organisation_block[[length(organisation_block) + 1]] <- make_org(wildobs_ror, role_facility)
    # and mark it seen
    seen <- c(seen, wildobs_ror)
  } # end condition

  #
  ##
  ### Finally, save all remaining orgs as partner org ----

  # for each contributor
  for (c in contribs) {
    # grab the ROR
    ror <- c$ROR
    # if its not valid or already included, skip it
    if (!valid_ror(ror) || ror %in% seen) next
    # otherwise add it as a new org to the block w/ partner role
    organisation_block[[length(organisation_block) + 1]] <- make_org(ror, role_partner)
    # and mark it seen
    seen <- c(seen, ror)
  } # end per contributor

  # return the org block list
  organisation_block
} # end function
```
