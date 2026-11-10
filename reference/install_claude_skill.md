# Install a WildObsR Skill for Claude

Copies a Claude skill that ships with WildObsR into your Claude skills
folder, so Claude (for example Claude Code) understands WildObs data
when you work with it.

## Usage

``` r
install_claude_skill(
  skill = "wildobsr-data",
  dest = "~/.claude/skills",
  overwrite = FALSE
)
```

## Arguments

- skill:

  Character string. The skill to install. Defaults to `"wildobsr-data"`,
  currently the only skill bundled with WildObsR.

- dest:

  Character string. The folder that holds your Claude skills. Defaults
  to `"~/.claude/skills"`.

- overwrite:

  Logical. Replace the skill if it is already installed in `dest`.
  Defaults to `FALSE`.

## Value

The path to the installed skill folder, invisibly. Stops if `skill` is
not bundled with WildObsR, if it is already installed and
`overwrite = FALSE`, or if the copy fails.

## Details

A skill is a folder of plain-text instructions that Claude reads when a
task needs it. The `wildobsr-data` skill explains how WildObs data
packages are structured: how the tables join, what each field means,
which analyses the data can support, and the WildObs additions to
Camtrap DP. It contains documentation only, no code.

The skill's counts of projects and records describe the public database
on the date given in the skill, so treat them as approximate. They drift
between releases as projects are added and as embargoes are set,
extended or released, which moves projects between `open` and `partial`.
The skill tells Claude to check each project's sharing preference when
it runs, rather than relying on those counts.

The function works as follows:

1.  It checks that `skill` is one of the skills bundled with this
    version of WildObsR.

2.  If that skill is already installed in `dest`, it stops unless
    `overwrite = TRUE`, so a copy you have edited is never replaced
    silently.

3.  It copies the skill folder into `dest`, creating `dest` if needed.

The default `dest`, `~/.claude/skills`, makes the skill available to
Claude Code in every project. To limit it to one project, point `dest`
at that project's `.claude/skills` folder. Re-run with
`overwrite = TRUE` after updating WildObsR to get the skill that matches
the new version.

## Author

Zachary Amir & Claude Opus 5.5

## Examples

``` r
# Install into a temporary folder to see what gets copied
path <- install_claude_skill(dest = tempdir())
#> Installed the wildobsr-data skill to /tmp/RtmpWQjC1M/wildobsr-data
#> Start a new Claude Code session for Claude to pick it up.
list.files(path, recursive = TRUE)
#> [1] "SKILL.md"              "reference/metadata.md" "reference/tables.md"  

if (FALSE) { # \dontrun{
# Install for Claude Code in every project
install_claude_skill()

# After updating WildObsR, refresh the installed copy
install_claude_skill(overwrite = TRUE)
} # }
```
