## Tests for install_claude_skill() ----

## Every test installs into its own temporary folder, so nothing here ever
## touches the real ~/.claude/skills on the machine running the tests.

## a fresh, empty folder per test, inside the session temp folder R deletes on exit
fresh_dest <- function() {
  # a unique path that does not exist yet
  dest <- tempfile("skills_")
  # create it so each test starts from an empty skills folder
  dir.create(dest)
  return(dest)
} # end helper

test_that("install_claude_skill copies the wildobsr-data skill", {
  dest <- fresh_dest()

  path <- suppressMessages(install_claude_skill(dest = dest))

  # the skill lands in its own folder named after the skill
  expect_equal(normalizePath(path), normalizePath(file.path(dest, "wildobsr-data")))
  # with its instructions and reference files
  expect_true(file.exists(file.path(path, "SKILL.md")))
  expect_true(file.exists(file.path(path, "reference", "tables.md")))
  expect_true(file.exists(file.path(path, "reference", "metadata.md")))
})

test_that("the bundled skill declares the name it is installed under", {
  dest <- fresh_dest()
  path <- suppressMessages(install_claude_skill(dest = dest))

  # Claude matches a skill by the name in its frontmatter, so it must equal the folder
  skill_md <- readLines(file.path(path, "SKILL.md"))
  expect_true("name: wildobsr-data" %in% skill_md)
})

test_that("install_claude_skill says where it installed the skill", {
  dest <- fresh_dest()

  expect_message(install_claude_skill(dest = dest), "Installed the wildobsr-data skill")
})

test_that("install_claude_skill refuses to replace an existing copy by default", {
  dest <- fresh_dest()
  suppressMessages(install_claude_skill(dest = dest))

  # a second install without overwrite must stop and point at the fix
  expect_error(install_claude_skill(dest = dest), "overwrite = TRUE")
})

test_that("install_claude_skill replaces an existing copy when asked", {
  dest <- fresh_dest()
  path <- suppressMessages(install_claude_skill(dest = dest))

  # simulate a stale file left over from an older release
  writeLines("stale", file.path(path, "old_file.md"))

  suppressMessages(install_claude_skill(dest = dest, overwrite = TRUE))

  # the fresh copy is there and the stale file is gone
  expect_true(file.exists(file.path(path, "SKILL.md")))
  expect_false(file.exists(file.path(path, "old_file.md")))
})

test_that("install_claude_skill errors on a skill that is not bundled", {
  dest <- fresh_dest()

  # the error lists what is available so the user can correct the name
  expect_error(install_claude_skill(skill = "not-a-skill", dest = dest),
               "Available skills: wildobsr-data")
  expect_error(install_claude_skill(skill = c("a", "b"), dest = dest),
               "not a skill bundled")
})

test_that("install_claude_skill creates the destination folder if needed", {
  dest <- file.path(fresh_dest(), "does", "not", "exist")

  path <- suppressMessages(install_claude_skill(dest = dest))

  expect_true(file.exists(file.path(path, "SKILL.md")))
})
