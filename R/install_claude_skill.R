#' Install a WildObsR Skill for Claude
#'
#' Copies a Claude skill that ships with WildObsR into your Claude skills folder, so
#' Claude (for example Claude Code) understands WildObs data when you work with it.
#'
#' @details
#' A skill is a folder of plain-text instructions that Claude reads when a task needs
#' it. The `wildobsr-data` skill explains how WildObs data packages are structured:
#' how the tables join, what each field means, which analyses the data can support,
#' and the WildObs additions to Camtrap DP. It contains documentation only, no code.
#'
#' The function works as follows:
#' \enumerate{
#'   \item It checks that `skill` is one of the skills bundled with this version of
#'     WildObsR.
#'   \item If that skill is already installed in `dest`, it stops unless
#'     `overwrite = TRUE`, so a copy you have edited is never replaced silently.
#'   \item It copies the skill folder into `dest`, creating `dest` if needed.
#' }
#'
#' The default `dest`, `~/.claude/skills`, makes the skill available to Claude Code in
#' every project. To limit it to one project, point `dest` at that project's
#' `.claude/skills` folder. Re-run with `overwrite = TRUE` after updating WildObsR to get
#' the skill that matches the new version.
#'
#' @param skill Character string. The skill to install. Defaults to `"wildobsr-data"`,
#'   currently the only skill bundled with WildObsR.
#' @param dest Character string. The folder that holds your Claude skills. Defaults
#'   to `"~/.claude/skills"`.
#' @param overwrite Logical. Replace the skill if it is already installed in `dest`.
#'   Defaults to `FALSE`.
#'
#' @return The path to the installed skill folder, invisibly. Stops if `skill` is not
#'   bundled with WildObsR, if it is already installed and `overwrite = FALSE`, or if the
#'   copy fails.
#'
#' @examples
#' # Install into a temporary folder to see what gets copied
#' path <- install_claude_skill(dest = tempdir())
#' list.files(path, recursive = TRUE)
#'
#' \dontrun{
#' # Install for Claude Code in every project
#' install_claude_skill()
#'
#' # After updating WildObsR, refresh the installed copy
#' install_claude_skill(overwrite = TRUE)
#' }
#'
#' @author Zachary Amir & Claude Opus 5.5
#'
#' @export
install_claude_skill <- function(skill = "wildobsr-data", dest = "~/.claude/skills",
                                 overwrite = FALSE) {

  ## find the skills bundled with this installed copy of WildObsR
  skills_root <- system.file("claude-skills", package = "WildObsR")
  # each skill is one folder inside it
  available <- list.dirs(skills_root, full.names = FALSE, recursive = FALSE)

  ## make sure they asked for exactly one skill we actually ship
  if (!is.character(skill) || length(skill) != 1 || !skill %in% available) {
    # tell them what they can choose from instead
    stop(sprintf("'%s' is not a skill bundled with WildObsR.\nAvailable skills: %s",
                 paste(skill, collapse = ", "), paste(available, collapse = ", ")),
         call. = FALSE)
  } # end skill check

  # where the skill folder will end up, with ~ expanded to the user's home
  target <- file.path(path.expand(dest), skill)

  ## never silently replace a copy that might have been edited
  if (dir.exists(target) && !isTRUE(overwrite)) {
    stop(sprintf("The %s skill is already installed at %s.\n", skill, target),
         "Set overwrite = TRUE to replace it with the version from this WildObsR release.",
         call. = FALSE)
  } # end existing install check

  ## if overwriting, clear the old copy first so removed files dont linger
  if (dir.exists(target)) {
    unlink(target, recursive = TRUE)
  } # end overwrite condition

  # make sure the skills folder exists
  dir.create(path.expand(dest), recursive = TRUE, showWarnings = FALSE)
  # copy the whole skill folder across
  copied <- file.copy(file.path(skills_root, skill), path.expand(dest), recursive = TRUE)

  ## a failed copy usually means dest is not writable
  if (!isTRUE(copied) || !file.exists(file.path(target, "SKILL.md"))) {
    stop(sprintf("Could not copy the %s skill into %s.\n", skill, path.expand(dest)),
         "Check that the folder exists and that you have permission to write to it.",
         call. = FALSE)
  } # end copy check

  # let them know where it went and what to do next
  message(sprintf("Installed the %s skill to %s\n", skill, target),
          "Start a new Claude Code session for Claude to pick it up.")

  # hand back the path in case they want it
  return(invisible(target))
} # end function
