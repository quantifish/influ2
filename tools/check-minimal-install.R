# Run from the repository root: Rscript --vanilla tools/check-minimal-install.R
# No libraries are uninstalled or modified. Only this temporary library is used
# by installation and smoke checks; the producer alone can use optional packages.
local({
  source_dir <- normalizePath(".", winslash = "/", mustWork = TRUE)
  description <- read.dcf(file.path(source_dir, "DESCRIPTION"))[1L, ]
  stopifnot(identical(unname(description["Package"]), "influ2"))
  installed <- installed.packages()
  installed <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
  dependency_names <- function(fields) {
    fields <- fields[!is.na(fields)]
    if (!length(fields)) return(character())
    names <- trimws(unlist(strsplit(gsub("\\([^)]*\\)", "", fields), ",")))
    setdiff(names[nzchar(names)], "R")
  }
  direct <- dependency_names(description[c("Depends", "Imports", "LinkingTo")])
  missing <- setdiff(direct, installed[, "Package"])
  if (length(missing)) stop("Install required dependencies first: ", paste(missing, collapse = ", "))
  # Dependencies are already installed binaries: their LinkingTo headers were
  # needed when they were built, not to run them (e.g. cpp11 for isoband).
  # influ2's own build-time requirements, if any, are included in direct above.
  required <- unique(c(direct, unlist(tools::package_dependencies(direct,
    db = installed, which = c("Depends", "Imports"), recursive = TRUE))))
  missing <- setdiff(required, installed[, "Package"])
  if (length(missing)) stop("Missing required dependencies: ", paste(missing, collapse = ", "))

  # R always adds its default library. Allow its base/recommended packages,
  # but reject any extra installed packages rather than silently weaken the test.
  core <- installed.packages(lib.loc = .Library)
  core_allowed <- core[core[, "Priority"] %in% c("base", "recommended"), "Package"]
  forbidden <- setdiff(unique(c(dependency_names(description["Suggests"]),
    "rstan", "StanHeaders", "RcppParallel")), c(required, core_allowed))
  required_absent <- c("brms", "rstan", "glmmTMB", "sdmTMB", "tinyVAST",
    "posterior", "bayesplot", "TMB", "DHARMa", "fmesher", "callr", "pkgload", "testthat")
  if (length(intersect(required_absent, required))) {
    stop("An optional package has become a mandatory dependency: ",
      paste(intersect(required_absent, required), collapse = ", "))
  }
  forbidden <- unique(c(forbidden, required_absent))
  leaked <- setdiff(core[, "Package"], c(core_allowed, required))
  if (length(leaked)) {
    stop("The default R library contains extra packages: ", paste(leaked, collapse = ", "),
      ". Run this check with an R installation whose default library contains only ",
      "base/recommended packages and mandatory dependencies. No library was changed.")
  }

  directory <- tempfile("influ2-minimal-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  library_dir <- file.path(directory, "library")
  dir.create(library_dir)
  copy <- required[!required %in% core[core[, "Priority"] %in% "base", "Package"]]
  for (package in copy) {
    from <- find.package(package, lib.loc = installed[package, "LibPath"])
    if (!file.copy(from, library_dir, recursive = TRUE, copy.date = TRUE)) {
      stop("Could not stage dependency: ", package)
    }
  }
  configuration <- list(library = normalizePath(library_dir, winslash = "/"),
    allowed = unique(c("influ2", required, core_allowed)), forbidden = forbidden,
    source = source_dir, results = file.path(directory, "saved-results.rds"),
    expected = file.path(directory, "expected-views.rds"))
  config_path <- file.path(directory, "configuration.rds")
  saveRDS(configuration, config_path)
  worker <- file.path(source_dir, "tools", "tests", "minimal-install-worker.R")
  isolated_env <- c(R_LIBS = library_dir, R_LIBS_USER = "NULL", R_LIBS_SITE = "NULL",
    R_ENVIRON = "/dev/null", R_ENVIRON_USER = "/dev/null",
    R_PROFILE = "/dev/null", R_PROFILE_USER = "/dev/null")
  run <- function(command, args, isolated = FALSE) {
    if (isolated) {
      previous <- Sys.getenv(names(isolated_env), unset = NA_character_)
      on.exit({
        Sys.unsetenv(names(previous)[is.na(previous)])
        do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
      }, add = TRUE)
      do.call(Sys.setenv, as.list(isolated_env))
    }
    status <- system2(command, shQuote(args))
    if (status != 0L) stop("Minimal-installation command failed (status ", status, ").")
  }
  rscript <- file.path(R.home("bin"), "Rscript")
  run(rscript, c("--vanilla", worker, "preflight", config_path), isolated = TRUE)

  previous_dir <- setwd(directory)
  on.exit(setwd(previous_dir), add = TRUE, after = FALSE)
  run(file.path(R.home("bin"), "R"),
    c("CMD", "build", "--no-build-vignettes", "--no-manual", source_dir))
  archive <- file.path(directory, paste0("influ2_", description["Version"], ".tar.gz"))
  stopifnot(file.exists(archive))
  run(file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "-l", library_dir, archive), isolated = TRUE)

  # Calculate compact results with the normal backend libraries available,
  # then reopen them in a separate process that cannot find those libraries.
  run(rscript, c("--vanilla", worker, "produce", config_path))
  run(rscript, c("--vanilla", worker, "check", config_path), isolated = TRUE)
  message("Minimal-installation check passed: installation, core calculations, ",
    "saved results, and optional-package errors.")
})
