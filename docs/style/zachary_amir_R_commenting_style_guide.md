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
structure on top of baseline narration, it doesn't replace it.

### The spacer stack

Before a `###` section in heavy code, use a three-line ramp for vertical
breathing room:

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

## 5. Comments go above, not trailing

Default position is **on their own line, above the code**. Avoid right-trailing
comments. (The one tolerated exception is a short label on a constant
assignment, but even those read better moved above.)

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

## 10. Inherited conventions (unchanged)

Native pipe `|>`; `snake_case`; `#### Name ----` section headers where used;
`message()` not `cat()` (except HPC scripts, where `cat()` goes to `.out`); CSS
colors only in DT/htmlwidgets.

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
