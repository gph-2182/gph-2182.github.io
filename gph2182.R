## ---------------------------------------------------------------------------
## GPH-GU 2182 Statistical Programming in R  |  course helpers
##
## Load these commands with one line in the RStudio Console:
##
##     source("https://gph-2182.github.io/gph2182.R")
##
## Then:
##     gph_setup()        once per computer: connect RStudio to GitHub
##     gph_start(2)       open Weekly Exercise 2 as an RStudio project
##     gph_check()        run the autograder's checks on your own machine
##     gph_submit()       stage, commit, push, and report the result
##     gph_doctor()       tell me what is wrong with my setup
##     gph_help()         show the loop again
## ---------------------------------------------------------------------------

.gph_version <- "2026-09-10c"

.gph_org       <- "gph-2182"
.gph_classroom <- "gph-gu-2182-fall-2026"
.gph_home_default <- "~/gph2182"
.gph_site      <- "https://gph-2182.github.io"
.gph_n_max     <- 8L

## -- small output helpers ---------------------------------------------------

.gph_utf8 <- function() isTRUE(l10n_info()[["UTF-8"]])
.gph_ok   <- function(...) cat(if (.gph_utf8()) "✓ " else "[ok] ", ..., "\n", sep = "")
.gph_no   <- function(...) cat(if (.gph_utf8()) "✗ " else "[!!] ", ..., "\n", sep = "")
.gph_dot  <- function(...) cat("  ", ..., "\n", sep = "")
.gph_rule <- function(title) {
  cat("\n", title, "\n", strrep("-", max(nchar(title), 10)), "\n", sep = "")
}

.gph_stop <- function(...) stop(paste0(...), call. = FALSE)

## -- identity and naming ----------------------------------------------------

#' Your GitHub login, read from the token RStudio has stored.
#' Returns NULL if no usable token is present.
.gph_login <- function() {
  if (!requireNamespace("gh", quietly = TRUE)) return(NULL)
  tryCatch(gh::gh("/user")$login, error = function(e) NULL)
}

.gph_n <- function(exercise) {
  if (missing(exercise) || length(exercise) != 1 || !is.numeric(exercise) ||
      is.na(exercise) || exercise != as.integer(exercise)) {
    .gph_stop("Give the exercise number, for example gph_start(2).")
  }
  n <- as.integer(exercise)
  if (n < 1L || n > .gph_n_max) {
    .gph_stop("There are only exercises 1 to ", .gph_n_max, ". You asked for ", n, ".")
  }
  n
}

.gph_spec <- function(n, login) {
  sprintf("%s/%s-weekly-exercise-%02d-%s", .gph_org, .gph_classroom, n, tolower(login))
}

.gph_accept_url <- function(n) {
  sprintf("https://classroom50.org/%s/%s/assignments/weekly-exercise-%02d/accept",
          .gph_org, .gph_classroom, n)
}

.gph_inclass_spec <- function(login) {
  sprintf("%s/%s-in-class-exercises-%s", .gph_org, .gph_classroom, tolower(login))
}

.gph_inclass_accept_url <- function() {
  sprintf("https://classroom50.org/%s/%s/assignments/in-class-exercises/accept",
          .gph_org, .gph_classroom)
}

.gph_repo_exists <- function(spec) {
  tryCatch({ gh::gh(paste0("/repos/", spec)); TRUE }, error = function(e) FALSE)
}

#' owner/repo for the repository open in this project, from its git remote.
.gph_slug <- function(path = ".") {
  url <- tryCatch(gert::git_remote_list(repo = path)$url[1], error = function(e) NA_character_)
  if (is.na(url)) return(NA_character_)
  sub("\\.git$", "", sub("^.*github\\.com[:/]", "", url))
}

.gph_exercise_here <- function(path = ".") {
  slug <- .gph_slug(path)
  src  <- if (is.na(slug)) basename(normalizePath(path, mustWork = FALSE)) else slug
  m <- regmatches(src, regexpr("weekly-exercise-([0-9]{2})", src))
  if (!length(m)) return(NA_integer_)
  as.integer(sub("weekly-exercise-", "", m))
}

.gph_open <- function(path) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::openProject(path)
  } else {
    .gph_dot("Open this folder in RStudio: ", path)
  }
  invisible(path)
}

## -- where exercises live ---------------------------------------------------

.gph_config_file <- function() file.path(path.expand("~"), ".gph2182")

.gph_home_get <- function() {
  f <- .gph_config_file()
  if (file.exists(f)) {
    p <- trimws(readLines(f, warn = FALSE))
    p <- p[nzchar(p)]
    if (length(p) && dir.exists(p[1])) return(p[1])
  }
  path.expand(.gph_home_default)
}

#' Has this student already got course repositories somewhere? If so, that
#' folder is where the next one belongs, whatever it happens to be called.
.gph_infer_home <- function() {
  pat <- paste0("^", .gph_classroom, "-")
  cands <- character()
  for (d in .gph_recent_projects()) {
    if (dir.exists(d) && grepl(pat, basename(d))) cands <- c(cands, dirname(d))
  }
  if (!length(cands)) {
    roots <- unique(c(.gph_home_get(), path.expand(c("~", "~/Desktop", "~/Documents"))))
    for (r in roots[dir.exists(roots)]) {
      kids <- tryCatch(list.dirs(r, recursive = FALSE, full.names = TRUE),
                       error = function(e) character())
      hit <- kids[grepl(pat, basename(kids))]
      if (length(hit)) { cands <- c(cands, r); break }
      for (k in kids[!startsWith(basename(kids), ".")][1:min(60, length(kids))]) {
        gk <- tryCatch(list.dirs(k, recursive = FALSE, full.names = TRUE),
                       error = function(e) character())
        if (any(grepl(pat, basename(gk)))) { cands <- c(cands, k); break }
      }
      if (length(cands)) break
    }
  }
  if (!length(cands)) return(NULL)
  names(sort(table(cands), decreasing = TRUE))[1]
}

#' Decide, once, where this student keeps course projects.
#'
#' Order: a folder they already chose, then the folder their existing course
#' repositories already live in, then ask, then the default. The answer is
#' remembered so this never asks twice.
.gph_resolve_home <- function() {
  f <- .gph_config_file()
  if (file.exists(f)) {
    p <- trimws(readLines(f, warn = FALSE)); p <- p[nzchar(p)]
    if (length(p) && dir.exists(p[1])) return(p[1])
  }
  guess <- .gph_infer_home()
  if (!is.null(guess)) {
    writeLines(guess, f)
    .gph_ok("Keeping course projects where your others already are:")
    .gph_dot("  ", guess)
    .gph_dot('(to use a different folder: gph_start(N, where = "your/folder"))')
    return(guess)
  }
  default <- path.expand(.gph_home_default)
  if (interactive()) {
    cat("\nWhere should your course projects live?\n")
    cat("  Press Enter for ", default, ", or type a folder path.\n", sep = "")
    ans <- trimws(readline("Folder: "))
    chosen <- if (nzchar(ans)) normalizePath(path.expand(ans), mustWork = FALSE) else default
  } else {
    chosen <- default
  }
  dir.create(chosen, recursive = TRUE, showWarnings = FALSE)
  writeLines(chosen, f)
  .gph_ok("Course projects will live in ", chosen)
  .gph_dot('(to use a different folder: gph_start(N, where = "your/folder"))')
  chosen
}

#' Show or change the folder your exercises are kept in.
#'
#' .gph_where()               show it
#' .gph_where("~/Desktop/r")  change it, and remember the change
.gph_where <- function(path = NULL) {
  if (is.null(path)) {
    cur <- .gph_home_get()
    .gph_dot("New exercises go into: ", cur)
    if (!dir.exists(cur)) .gph_dot("(it does not exist yet; it is created when you need it)")
    .gph_dot('Change it with .gph_where("path/to/your/folder")')
    return(invisible(cur))
  }
  path <- normalizePath(path.expand(path), mustWork = FALSE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) {
    .gph_no("Could not create ", path)
    return(invisible(FALSE))
  }
  writeLines(path, .gph_config_file())
  .gph_ok("New exercises will go into ", path)
  invisible(path)
}

#' Every project RStudio remembers you opening.
.gph_recent_projects <- function() {
  local_app <- Sys.getenv("LOCALAPPDATA", "")
  files <- c(
    file.path(path.expand("~"), ".local", "share", "rstudio", "monitored", "lists", "project_mru"),
    file.path(path.expand("~"), ".rstudio-desktop", "monitored", "lists", "project_mru"),
    if (nzchar(local_app)) file.path(local_app, "RStudio", "monitored", "lists", "project_mru"),
    if (nzchar(local_app)) file.path(local_app, "RStudio-Desktop", "monitored", "lists", "project_mru")
  )
  out <- character()
  for (f in files) {
    if (file.exists(f)) {
      out <- c(out, tryCatch(readLines(f, warn = FALSE), error = function(e) character()))
    }
  }
  out <- trimws(out)
  out <- out[nzchar(out)]
  unique(dirname(path.expand(out)))
}

#' Does the git remote in `dir` point at `spec` (owner/repo)?
.gph_remote_is <- function(dir, spec) {
  url <- tryCatch(gert::git_remote_list(repo = dir)$url[1], error = function(e) NA_character_)
  if (is.na(url)) return(FALSE)
  identical(tolower(sub("\\.git$", "", sub("^.*github\\.com[:/]", "", url))), tolower(spec))
}

#' Search one root, breadth-first, for a directory called `target`.
#'
#' Returns hits plus whether the search finished. Each root gets its own
#' budget so that one enormous cloud-synced tree cannot starve the search of
#' the places a student is actually likely to have used.
.gph_scan_root <- function(root, target, max_depth = 3L, budget = 2500L, deadline = NULL) {
  frontier <- root
  seen <- 0L
  for (depth in seq_len(max_depth + 1L)) {
    hit <- frontier[basename(frontier) == target]
    if (length(hit)) return(list(hits = hit, complete = TRUE))
    nxt <- character()
    for (d in frontier) {
      if (seen > budget) return(list(hits = character(), complete = FALSE))
      if (!is.null(deadline) && Sys.time() > deadline) {
        return(list(hits = character(), complete = FALSE))
      }
      kids <- tryCatch(list.dirs(d, recursive = FALSE, full.names = TRUE),
                       error = function(e) character())
      if (!length(kids)) next
      kids <- kids[!basename(kids) %in% .gph_skip_dirs & !startsWith(basename(kids), ".")]
      seen <- seen + length(kids)
      nxt <- c(nxt, kids)
    }
    if (!length(nxt)) break
    frontier <- unique(nxt)
  }
  list(hits = character(), complete = TRUE)
}

.gph_skip_dirs <- c("Library", "Applications", "Music", "Movies", "Pictures",
                    "node_modules", "renv", "packrat", ".git", ".Trash",
                    "site_libs", "_freeze")

#' Hunt for an existing copy, likeliest places first, with a wall-clock cap.
#'
#' `complete = FALSE` means the search ran out of room, so a negative result
#' is not proof the student has no copy. Callers must say so.
.gph_scan <- function(target, seconds = 8) {
  deadline <- Sys.time() + seconds
  roots <- c(
    .gph_home_get(),
    path.expand(c("~/Desktop", "~/Documents", "~/Downloads", "~/GitHub",
                  "~/Documents/GitHub", "~/Projects", "~/src"))
  )
  depths <- rep(3L, length(roots))
  roots  <- c(roots, path.expand("~"))
  depths <- c(depths, 2L)
  cloud  <- Sys.glob(path.expand(c("~/Dropbox*", "~/OneDrive*",
                                   "~/Library/CloudStorage/*")))
  roots  <- c(roots, cloud)
  depths <- c(depths, rep(3L, length(cloud)))

  keep   <- dir.exists(roots) & !duplicated(roots)
  roots  <- roots[keep]
  depths <- depths[keep]

  complete <- TRUE
  for (i in seq_along(roots)) {
    res <- .gph_scan_root(roots[i], target, max_depth = depths[i], deadline = deadline)
    if (length(res$hits)) return(list(hits = res$hits, complete = TRUE))
    if (!res$complete) complete <- FALSE
  }
  list(hits = character(), complete = complete)
}

#' The cheap checks: is it open, in the remembered folder, or in RStudio's
#' recent-projects list?
.gph_find_quick <- function(spec) {
  name <- basename(spec)
  if (.gph_remote_is(".", spec)) {
    return(list(path = normalizePath(".", mustWork = FALSE), how = "open"))
  }
  p <- file.path(.gph_home_get(), name)
  if (dir.exists(p)) return(list(path = p, how = "home"))
  for (d in .gph_recent_projects()) {
    if (!dir.exists(d)) next
    if (identical(basename(d), name) || .gph_remote_is(d, spec)) {
      return(list(path = d, how = "recent"))
    }
  }
  NULL
}

.gph_cloudy <- function(path) {
  grepl("dropbox|onedrive|cloudstorage|google ?drive|icloud", path, ignore.case = TRUE)
}

.gph_fetch <- function(spec, accept_url, what, not_accepted_hint = NULL,
                       after_open = NULL) {
  name <- basename(spec)

  hit <- .gph_find_quick(spec)
  if (!is.null(hit)) return(.gph_use_existing(hit, what, after_open))

  if (!.gph_repo_exists(spec)) {
    .gph_no("You have not accepted ", what, " yet.")
    .gph_dot("")
    .gph_dot("Accept it here, then run the same command again:")
    .gph_dot("  ", accept_url)
    if (!is.null(not_accepted_hint)) { .gph_dot(""); for (l in not_accepted_hint) .gph_dot(l) }
    return(invisible(FALSE))
  }
  .gph_ok("Found ", what, " on GitHub: ", spec)

  home <- .gph_resolve_home()
  .gph_dot("Checking whether you already have it somewhere ...")
  scan <- .gph_scan(name)
  found <- scan$hits[dir.exists(scan$hits)]
  if (length(found)) return(.gph_use_existing(list(path = found[1], how = "scan"), what, after_open))

  dir.create(home, recursive = TRUE, showWarnings = FALSE)
  .gph_dot("Downloading into ", home)
  if (!isTRUE(scan$complete)) {
    .gph_dot("")
    .gph_dot("I could not search your whole computer. If you know you already have")
    .gph_dot("this somewhere, stop and open that project instead: gph_check() and")
    .gph_dot("gph_submit() work from inside any copy.")
    .gph_dot("")
  }
  usethis::create_from_github(spec, destdir = home, open = TRUE)
}

.gph_use_existing <- function(hit, n, after_open = NULL) {
  if (identical(hit$how, "open")) {
    .gph_ok(if (is.numeric(n)) paste("Weekly Exercise", n) else n, " is already open. Nothing to download.")
    .gph_dot("Next: gph_check(), then gph_submit()")
    if (!is.null(after_open)) for (l in after_open) .gph_dot(l)
    return(invisible(hit$path))
  }
  .gph_ok("You already have ", if (is.numeric(n)) paste("Weekly Exercise", n) else n, " at:")
  .gph_dot("  ", hit$path)
  st <- tryCatch(gert::git_status(repo = hit$path), error = function(e) NULL)
  if (!is.null(st) && nrow(st) > 0) {
    .gph_dot("It has ", nrow(st), " change(s) not yet committed. They are safe: I am")
    .gph_dot("opening this copy rather than downloading a second one.")
  } else {
    .gph_dot("Opening it. I did not download a second copy.")
  }
  if (.gph_cloudy(hit$path)) {
    .gph_dot("")
    .gph_dot("Note: this is inside a cloud-synced folder (Dropbox, OneDrive, iCloud).")
    .gph_dot("Git and sync clients fight over the same files. It will probably work,")
    .gph_dot("but a plain local folder is safer.")
  }
  if (!is.null(after_open)) for (l in after_open) .gph_dot(l)
  .gph_open(hit$path)
}

## -- .gph_autoload -----------------------------------------------------------

.gph_rc    <- function() file.path(path.expand("~"), ".Rprofile")
.gph_cache <- function() file.path(path.expand("~"), ".gph2182-helpers.R")
.gph_mark  <- c("# >>> GPH-GU 2182 helpers >>>", "# <<< GPH-GU 2182 helpers <<<")

#' Load the course commands automatically in every R session on this computer.
#'
#' Without this you must run the source() line once in each new session. This
#' saves a copy of the helpers in your home folder and loads it at startup, so
#' it keeps working offline. Undo with .gph_autoload(remove = TRUE).
.gph_autoload <- function(remove = FALSE) {
  rc <- .gph_rc()
  old <- if (file.exists(rc)) readLines(rc, warn = FALSE) else character()

  i <- which(trimws(old) == .gph_mark[1])
  j <- which(trimws(old) == .gph_mark[2])
  if (length(i) && length(j) && j[1] >= i[1]) old <- old[-(i[1]:j[1])]

  if (isTRUE(remove)) {
    if (length(old)) writeLines(old, rc) else unlink(rc)
    unlink(.gph_cache())
    .gph_ok("Automatic loading removed.")
    .gph_dot("You will need the source() line once per session again.")
    return(invisible(TRUE))
  }

  src <- tryCatch(
    suppressWarnings(readLines(paste0(.gph_site, "/gph2182.R"), warn = FALSE)),
    error = function(e) NULL
  )
  if (is.null(src) || !length(src)) {
    .gph_no("Could not fetch the helpers just now.")
    .gph_dot("Check your internet connection and run .gph_autoload() again.")
    return(invisible(FALSE))
  }
  writeLines(src, .gph_cache())

  writeLines(c(old, .gph_mark[1],
               'if (interactive()) try(suppressWarnings(source("~/.gph2182-helpers.R")), silent = TRUE)',
               .gph_mark[2]), rc)

  .gph_ok("Done. The course commands will now load in every R session.")
  .gph_dot("Restart R to see it work: Session > Restart R, or just reopen RStudio.")
  .gph_dot("Run gph_setup() again any time to pick up newer versions.")
  .gph_dot("Undo with .gph_autoload(remove = TRUE)")
  invisible(TRUE)
}

## -- gph_help ---------------------------------------------------------------

#' Print the weekly loop.
gph_help <- function() {
  cat("
GPH-GU 2182  |  seven commands, and that is all of them
=======================================================

ONCE per computer
  gph_setup()      connect to GitHub, and load these in every session

ONCE for the whole semester
  gph_inclass()    open the in-class repository you reuse every week

ONCE per exercise
  gph_start(N)     download and open weekly exercise N

EVERY time, until it passes
  gph_check()      run the autograder's checks on your own machine
  gph_submit()     stage, commit, push, and report the result

WHEN STUCK
  gph_doctor()     check everything and say what is wrong
  gph_help()       print this

Pushing many times is normal. Only your last push before the deadline is
graded, so a failing check on an early try costs you nothing.

Keep projects elsewhere:  gph_start(N, where = \"your/folder\")
")
  invisible(NULL)
}

## -- gph_setup --------------------------------------------------------------

#' One-time setup: install what is missing, set your Git identity, store a token.
gph_setup <- function() {
  .gph_rule("Step 1 of 4: packages")
  need <- setdiff(c("usethis", "gitcreds", "gh", "gert"),
                  rownames(installed.packages()))
  if (length(need)) {
    .gph_dot("Installing: ", paste(need, collapse = ", "))
    utils::install.packages(need)
  }
  missing_still <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_still)) {
    .gph_no("Could not install: ", paste(missing_still, collapse = ", "))
    .gph_dot("Install them yourself, then run gph_setup() again.")
    return(invisible(FALSE))
  }
  .gph_ok("Packages ready")

  .gph_rule("Step 2 of 4: who you are")
  nm <- tryCatch(gert::git_config_global(), error = function(e) NULL)
  cur_name  <- if (!is.null(nm)) nm$value[nm$name == "user.name"][1]  else NA_character_
  cur_email <- if (!is.null(nm)) nm$value[nm$name == "user.email"][1] else NA_character_

  if (is.na(cur_name) || is.na(cur_email) || !nzchar(cur_name) || !nzchar(cur_email)) {
    if (!interactive()) {
      .gph_no("Git does not know your name and email yet.")
      .gph_dot("Run this, with your own details:")
      .gph_dot('  usethis::use_git_config(user.name = "Your Name", user.email = "you@nyu.edu")')
      return(invisible(FALSE))
    }
    new_name  <- trimws(readline("Your full name: "))
    new_email <- trimws(readline("The email on your GitHub account: "))
    if (!nzchar(new_name) || !nzchar(new_email)) {
      .gph_no("Both are required. Run gph_setup() again.")
      return(invisible(FALSE))
    }
    usethis::use_git_config(user.name = new_name, user.email = new_email)
    .gph_ok("Saved as: ", new_name, " <", new_email, ">")
  } else {
    .gph_ok("Already set: ", cur_name, " <", cur_email, ">")
  }

  .gph_rule("Step 3 of 4: your GitHub token")
  login <- .gph_login()
  if (!is.null(login)) {
    .gph_ok("Token works. GitHub sees you as: ", login)

    .gph_rule("Step 4 of 4: loading these commands automatically")
    rc <- .gph_rc()
    if (file.exists(rc) && any(trimws(readLines(rc, warn = FALSE)) == .gph_mark[1])) {
      .gph_ok("Already on. The commands load in every R session.")
    } else {
      .gph_dot("Without this you must run the source() line once per session.")
      ans <- if (interactive()) tolower(trimws(readline("Load them automatically from now on? [Y/n]: "))) else "n"
      if (ans %in% c("", "y", "yes")) .gph_autoload() else
        .gph_dot("Left off. Run gph_setup() again if you change your mind.")
    }

    .gph_rule("Setup complete")
    .gph_dot("Next: accept the exercise on Classroom 50, then run gph_start(N).")
    return(invisible(TRUE))
  }

  .gph_no("No working token yet. Two steps, in order:")
  .gph_dot("")
  .gph_dot("1. usethis::create_github_token()")
  .gph_dot("   A GitHub page opens. Set Expiration to Custom, then Dec 31 2026.")
  .gph_dot("   Leave the ticked boxes alone. Click Generate token and COPY it.")
  .gph_dot("")
  .gph_dot("2. gitcreds::gitcreds_set()")
  .gph_dot("   Paste the token when it asks.")
  .gph_dot("")
  .gph_dot("Then run gph_setup() again to confirm.")
  invisible(FALSE)
}

## -- gph_start --------------------------------------------------------------

#' Download and open a weekly exercise as an RStudio project.
#'
#' Works out which repository is yours, puts it wherever you keep your course
#' projects, and opens it. If you already have it, this opens that copy rather
#' than making a second one. Pass `where` to choose the folder explicitly.
gph_start <- function(exercise, where = NULL) {
  n <- .gph_n(exercise)
  if (!is.null(where)) .gph_where(where)

  login <- .gph_login()
  if (is.null(login)) {
    .gph_no("I cannot tell who you are on GitHub, so I cannot find your repository.")
    .gph_dot("Run gph_setup() first, then try gph_start(", n, ") again.")
    return(invisible(FALSE))
  }
  .gph_ok("GitHub sees you as: ", login)

  .gph_fetch(
    spec       = .gph_spec(n, login),
    accept_url = .gph_accept_url(n),
    what       = paste("Weekly Exercise", n),
    not_accepted_hint = c(
      "If that page says you are not a member of the organization, accept the",
      "emailed invitation first, then link your NetID at",
      paste0("  https://classroom50.org/", .gph_org, "/", .gph_classroom, "/onboard")
    )
  )
}

#' Download and open your in-class exercise repository.
#'
#' You accept this one once for the whole semester and reuse it every week.
gph_inclass <- function(where = NULL) {
  if (!is.null(where)) .gph_where(where)

  login <- .gph_login()
  if (is.null(login)) {
    .gph_no("I cannot tell who you are on GitHub, so I cannot find your repository.")
    .gph_dot("Run gph_setup() first, then try gph_inclass() again.")
    return(invisible(FALSE))
  }
  .gph_ok("GitHub sees you as: ", login)

  .gph_fetch(
    spec       = .gph_inclass_spec(login),
    accept_url = .gph_inclass_accept_url(),
    what       = "your in-class exercise repository",
    not_accepted_hint = c(
      "You accept this one only once, and reuse it for every in-class exercise.",
      "If that page says you are not a member of the organization, accept the",
      "emailed invitation first, then link your NetID at",
      paste0("  https://classroom50.org/", .gph_org, "/", .gph_classroom, "/onboard")
    ),
    after_open = c(
      "",
      "To get this week's worksheet, run this inside the project:",
      "  source(\"get_worksheet.R\"); get_worksheet(3)",
      "using the current week's number."
    )
  )
}

## -- gph_check --------------------------------------------------------------

#' Run the autograder's own checks locally, in a separate R process.
#'
#' A separate process matters twice over: some exercise repos ship a test
#' runner that calls quit(), which would close RStudio if run in your session,
#' and a clean process cannot be fooled by objects left over in your
#' Environment. What passes here is what passes on GitHub.
gph_check <- function(quiet = FALSE) {
  runner <- "tests/run_tests.R"
  if (!file.exists(runner)) {
    .gph_no("No ", runner, " here, so this is not an exercise project.")
    .gph_dot("Open one with gph_start(N), then run gph_check() again.")
    return(invisible(NA))
  }
  if (!file.exists("exercise.qmd")) {
    .gph_no("No exercise.qmd here. Open an exercise project with gph_start(N).")
    return(invisible(NA))
  }

  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!quiet) cat("Running the same checks the autograder runs ...\n\n")
  code <- suppressWarnings(system2(
    rscript,
    c("--no-save", "--no-restore", "--no-init-file", shQuote(runner)),
    stdout = "", stderr = ""
  ))
  passed <- identical(as.integer(code), 0L)
  cat("\n")
  if (passed) {
    .gph_ok("Everything passes. Send it in with gph_submit()")
  } else {
    .gph_no("Not everything passes yet. Fix the FAIL lines above, then run gph_check() again.")
    .gph_dot("You can still submit a partial answer; only your last push is graded.")
  }
  invisible(passed)
}

## -- gph_submit -------------------------------------------------------------

.gph_wait_run <- function(slug, sha, timeout = 150) {
  cat("\nYour work is already submitted. Now waiting for GitHub's check,\n")
  cat("which usually takes under a minute. Safe to stop waiting at any time.\n")
  cat("waiting ")
  deadline <- Sys.time() + timeout
  repeat {
    runs <- tryCatch(
      gh::gh(paste0("/repos/", slug, "/actions/runs"), head_sha = sha, per_page = 1)$workflow_runs,
      error = function(e) NULL
    )
    if (length(runs) && identical(runs[[1]]$status, "completed")) {
      cat("\n")
      return(runs[[1]]$conclusion)
    }
    if (Sys.time() > deadline) { cat("\n"); return(NA_character_) }
    cat(".")
    Sys.sleep(4)
  }
}

#' Stage everything, commit, push, and report what GitHub says.
gph_submit <- function(message = NULL, wait = TRUE) {
  if (!requireNamespace("gert", quietly = TRUE)) {
    .gph_stop("The gert package is missing. Run gph_setup() first.")
  }
  info <- tryCatch(gert::git_info(repo = "."), error = function(e) NULL)
  if (is.null(info)) {
    .gph_no("This folder is not a Git repository, so there is nothing to push.")
    .gph_dot("Open your exercise with gph_start(N). Do not open the folder by hand.")
    return(invisible(FALSE))
  }

  if (is.null(message)) {
    n <- .gph_exercise_here()
    message <- if (is.na(n)) {
      paste("Update", format(Sys.time(), "%b %d %H:%M"))
    } else {
      sprintf("Weekly exercise %d: update %s", n, format(Sys.time(), "%b %d %H:%M"))
    }
  }

  gert::git_add(".", repo = ".")
  staged <- tryCatch(gert::git_status(staged = TRUE, repo = "."), error = function(e) NULL)
  if (!is.null(staged) && nrow(staged) > 0) {
    gert::git_commit(message, repo = ".")
    .gph_ok("Committed ", nrow(staged), " file(s): ", message)
  } else {
    .gph_dot("Nothing new to commit; checking whether anything still needs pushing.")
  }

  push_err <- NULL
  pushed <- tryCatch({ gert::git_push(repo = ".", verbose = FALSE); TRUE },
                     error = function(e) { push_err <<- conditionMessage(e); FALSE })
  if (!pushed) {
    .gph_dot("First push did not go through (", push_err, ").")
    .gph_dot("Pulling anything new from GitHub, then pushing again.")
    ok <- tryCatch({
      gert::git_pull(repo = ".")
      gert::git_push(repo = ".", verbose = FALSE)
      TRUE
    }, error = function(e) { .gph_no("Push failed: ", conditionMessage(e)); FALSE })
    if (!ok) {
      .gph_dot("Run gph_doctor() and send its output if you cannot see why.")
      return(invisible(FALSE))
    }
  }
  .gph_ok("Pushed to GitHub")

  slug <- .gph_slug()
  if (is.na(slug)) return(invisible(TRUE))
  actions <- paste0("https://github.com/", slug, "/actions")

  if (!isTRUE(wait)) {
    .gph_dot("Results will appear here in a minute or two: ", actions)
    return(invisible(TRUE))
  }

  sha <- tryCatch(gert::git_commit_id(repo = "."), error = function(e) NA_character_)
  concl <- if (is.na(sha)) NA_character_ else .gph_wait_run(slug, sha)

  if (identical(concl, "success")) {
    .gph_ok("GitHub checked your work: everything passes. You are done.")
  } else if (identical(concl, "failure")) {
    .gph_no("GitHub checked your work: something still fails.")
    .gph_dot("Run gph_check() to see which questions, fix them, and gph_submit() again.")
  } else {
    .gph_dot("Still running. Your work is submitted either way; the result will")
    .gph_dot("appear here shortly: ", actions)
  }
  invisible(TRUE)
}

## -- gph_doctor -------------------------------------------------------------

#' Print a checklist of everything that has to be true, and what is not.
gph_doctor <- function() {
  .gph_rule("Your setup")
  cat("helpers version ", .gph_version, "\n", sep = "")

  for (p in c("usethis", "gitcreds", "gh", "gert")) {
    if (requireNamespace(p, quietly = TRUE)) .gph_ok("package ", p) else .gph_no("package ", p, " is missing (run gph_setup())")
  }

  cfg <- tryCatch(gert::git_config_global(), error = function(e) NULL)
  gname  <- if (!is.null(cfg)) cfg$value[cfg$name == "user.name"][1]  else NA_character_
  gemail <- if (!is.null(cfg)) cfg$value[cfg$name == "user.email"][1] else NA_character_
  if (!is.na(gname) && nzchar(gname)) .gph_ok("git name: ", gname) else .gph_no("git name is not set (run gph_setup())")
  if (!is.na(gemail) && nzchar(gemail)) .gph_ok("git email: ", gemail) else .gph_no("git email is not set (run gph_setup())")

  login <- .gph_login()
  if (!is.null(login)) .gph_ok("GitHub token works, you are ", login) else .gph_no("no working GitHub token (run gph_setup())")

  rc <- .gph_rc()
  auto <- file.exists(rc) && any(trimws(readLines(rc, warn = FALSE)) == .gph_mark[1])
  if (auto) .gph_ok("commands load automatically each session") else
    .gph_dot("commands load only when you source() them (gph_setup() can fix that)")

  .gph_rule("This project")
  .gph_dot("folder: ", getwd())
  info <- tryCatch(gert::git_info(repo = "."), error = function(e) NULL)
  if (is.null(info)) {
    .gph_no("not a Git repository, so nothing here can be pushed")
    .gph_dot("open an exercise with gph_start(N) instead of opening the folder")
    return(invisible(FALSE))
  }
  slug <- .gph_slug()
  .gph_ok("git repository, branch ", info$shorthand)
  if (!is.na(slug)) .gph_ok("GitHub repository: ", slug) else .gph_no("no GitHub remote")

  in_class <- !is.na(slug) && grepl("in-class-exercises", slug, fixed = TRUE)
  if (in_class) {
    sheets <- sort(list.files(".", pattern = "^week-[0-9]{2}-inclass\\.qmd$"))
    if (length(sheets)) {
      .gph_ok(length(sheets), " worksheet(s) here: ", paste(sheets, collapse = ", "))
    } else {
      .gph_no("no worksheets here yet")
      .gph_dot('get them with: source("get_worksheet.R"); get_worksheet(N)')
    }
  } else if (file.exists("exercise.qmd")) {
    .gph_ok("exercise.qmd is here")
  } else {
    .gph_no("no exercise.qmd here")
  }

  st <- tryCatch(gert::git_status(repo = "."), error = function(e) NULL)
  n_dirty <- if (is.null(st)) NA_integer_ else nrow(st)
  if (!is.na(n_dirty)) {
    if (n_dirty == 0) .gph_ok("no uncommitted changes") else .gph_no(n_dirty, " uncommitted change(s): run gph_submit()")
  }

  ab <- tryCatch(gert::git_ahead_behind(repo = "."), error = function(e) NULL)
  if (!is.null(ab)) {
    if (ab$ahead > 0)  .gph_no(ab$ahead, " commit(s) not yet on GitHub: run gph_submit()") else .gph_ok("everything committed is on GitHub")
    if (ab$behind > 0) .gph_no(ab$behind, " commit(s) on GitHub you do not have: gph_submit() will pull them")
  }

  if (in_class) {
    .gph_dot("in-class work has no autograder; it is graded on completion")
  } else if (!is.na(slug) && !is.null(login)) {
    runs <- tryCatch(gh::gh(paste0("/repos/", slug, "/actions/runs"), per_page = 1)$workflow_runs,
                     error = function(e) NULL)
    if (length(runs)) {
      r <- runs[[1]]
      lbl <- if (identical(r$status, "completed")) r$conclusion else r$status
      if (identical(lbl, "success")) .gph_ok("last GitHub check: passed") else .gph_no("last GitHub check: ", lbl)
    } else {
      .gph_dot("GitHub has not checked anything yet (push once with gph_submit())")
    }
  }
  invisible(TRUE)
}

## -- banner used by each exercise project's .Rprofile ----------------------

.gph_banner <- function() {
  slug <- .gph_slug()
  in_class <- !is.na(slug) && grepl("in-class-exercises", slug, fixed = TRUE)
  n <- .gph_exercise_here()
  cat("\n")
  if (in_class) {
    cat("GPH-GU 2182  |  in-class exercises\n")
    cat("This week's worksheet:  source(\"get_worksheet.R\"); get_worksheet(N)\n")
    cat("Add both partners' names, then commit and push before class ends.\n")
  } else if (!is.na(n)) {
    cat("GPH-GU 2182  |  Weekly Exercise ", n, "\n", sep = "")
    cat("Edit exercise.qmd, then:  gph_check()  ->  gph_submit()\n")
  } else {
    cat("GPH-GU 2182 helpers loaded.\n")
    cat("Weekly exercise: gph_start(N).  In-class repository: gph_inclass()\n")
  }
  cat("Stuck? gph_doctor()   Full loop? gph_help()\n\n")
  invisible(NULL)
}

invisible(NULL)
