# Documentation-only reuse of identical native fits. No fitted models are
# shipped, and each distinct specification is still fitted by its own backend.
# Set INFLU2_EXAMPLE_CACHE to a shared temporary directory when rendering several
# articles in separate R processes. NULL bypasses storage for parity checks.
example_model <- local({
  schema <- 2L

  versions <- function(backend) {
    packages <- switch(backend, glm = "stats", MASS = c("stats", "MASS"),
      glmmTMB = c("stats", "glmmTMB", "TMB", "Matrix", "RcppEigen"))
    stats::setNames(lapply(packages, function(package) {
      # Header-only build dependencies need not remain in a binary user's
      # runtime library. Record absence without making them mandatory.
      if (!length(find.package(package, quiet = TRUE)))
        return(list(Version = NA_character_, Built = NA_character_))
      description <- utils::packageDescription(package)
      list(Version = description$Version, Built = description$Built)
    }), packages)
  }

  hash <- function(value, serialise = TRUE) {
    path <- tempfile("influ2-example-key-")
    on.exit(unlink(path))
    connection <- file(path, "wb")
    tryCatch(writeBin(if (serialise) serialize(value, NULL, version = 3L) else value,
      connection), finally = close(connection))
    unname(tools::md5sum(path))
  }

  portable_data <- function(x) {
    if (is.environment(x) || is.function(x) ||
        typeof(x) %in% c("externalptr", "weakref") || inherits(x, "connection")) return(FALSE)
    children <- c(if (is.list(x) || is.pairlist(x) || is.call(x) || is.expression(x))
      as.list(x), attributes(x))
    all(vapply(children, portable_data, logical(1)))
  }

  canonical <- function(expression) {
    if (!is.call(expression)) stop("Supply a native fitting call to example_model().", call. = FALSE)
    name <- paste(deparse(expression[[1L]], width.cutoff = 500L), collapse = "")
    backend <- switch(name, glm = "glm", `stats::glm` = "glm",
      `MASS::glm.nb` = "MASS", `glmmTMB::glmmTMB` = "glmmTMB", NULL)
    if (is.null(backend)) stop("example_model() supports only stats::glm(), MASS::glm.nb(), and glmmTMB::glmmTMB().", call. = FALSE)
    package <- switch(backend, glm = "stats", MASS = "MASS", glmmTMB = "glmmTMB")
    if (!requireNamespace(package, quietly = TRUE)) stop("Package '", package,
      "' is required for this worked example.", call. = FALSE)
    constructor <- switch(backend, glm = stats::glm, MASS = MASS::glm.nb,
      glmmTMB = glmmTMB::glmmTMB)
    matched <- match.call(definition = constructor, call = expression, expand.dots = TRUE)
    arguments <- as.list(matched)[-1L]
    if (is.null(arguments$data) || is.null(arguments$formula)) stop(
      "Example fits require explicit formula and data arguments.", call. = FALSE)
    supplied_data <- arguments$data
    arguments$data <- quote(.example_data)
    # match.call() names positional arguments but retains supplied named order.
    position <- match(names(arguments), names(formals(constructor)))
    arguments <- arguments[order(is.na(position), position, names(arguments))]
    head <- switch(backend, glm = quote(stats::glm), MASS = quote(MASS::glm.nb),
      glmmTMB = quote(glmmTMB::glmmTMB))
    list(call = as.call(c(list(head), arguments)), backend = backend,
      supplied_data = supplied_data)
  }

  valid_model <- function(model, backend) {
    inherits(model, switch(backend, glm = "glm", MASS = "negbin", glmmTMB = "glmmTMB")) &&
      inherits(tryCatch(stats::formula(model), error = function(e) NULL), "formula")
  }

  fit_native <- function(call, private) eval(call, envir = private)

  restore_native <- function(model, backend) {
    if (backend != "glmmTMB") return(model)
    previous <- TMB::openmp(DLL = "glmmTMB")
    on.exit(TMB::openmp(n = as.integer(previous), autopar = attr(previous, "autopar"),
      DLL = "glmmTMB"))
    # Public native reconstruction of serialised pointers, not update()/refitting.
    glmmTMB::up2date(model)
  }

  function(data, fit, cache_dir = Sys.getenv("INFLU2_EXAMPLE_CACHE",
      file.path(tempdir(), "influ2-example-models"))) {
    had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
    if (had_seed) previous_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit({
      if (had_seed) assign(".Random.seed", previous_seed, envir = globalenv())
      else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    })
    spec <- canonical(substitute(fit))
    data <- force(data)
    if (!is.data.frame(data) || !portable_data(data)) stop(
      "Example data must be a self-contained data frame without live state.", call. = FALSE)
    supplied <- tryCatch(eval(spec$supplied_data, envir = parent.frame()), error = function(e) {
      stop("Could not resolve the native example's data: ", conditionMessage(e), call. = FALSE)
    })
    if (!identical(data, supplied)) stop("The native fitting data differ from example_model()'s data.", call. = FALSE)
    option_names <- c("contrasts", "na.action", if (spec$backend == "glmmTMB")
      c("glmmTMB.cores", "glmmTMB.autopar", "glmmTMB_openmp_debug"))
    options_used <- options()[option_names]
    identity <- list(schema = schema, call = spec$call, data = data,
      R = R.version[c("version.string", "platform", "arch", "os", "system", "crt")],
      packages = versions(spec$backend), options = options_used)
    key <- hash(identity)
    path <- NULL
    if (!is.null(cache_dir)) {
      if (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir) || !nzchar(cache_dir))
        stop("cache_dir must be NULL or one directory path.", call. = FALSE)
      if (!dir.exists(cache_dir) && !dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE))
        stop("Could not create the example-model cache directory.", call. = FALSE)
      path <- file.path(cache_dir, paste0(key, ".rds"))
    }
    if (!is.null(path) && file.exists(path)) {
      record <- tryCatch(readRDS(path), error = function(e) stop(
        "Malformed example-model cache: ", conditionMessage(e), call. = FALSE))
      if (!is.list(record) || !identical(names(record), c("identity", "model", "checksum")) ||
          !identical(record$identity, identity) || !is.raw(record$model) ||
          !identical(record$checksum, hash(record$model, serialise = FALSE))) stop(
        "Malformed or stale example-model cache; remove its temporary entry and retry.", call. = FALSE)
      model <- tryCatch(unserialize(record$model), error = function(e) stop(
        "Malformed cached example model: ", conditionMessage(e), call. = FALSE))
      if (!valid_model(model, spec$backend)) stop("The cached example has the wrong native model class.", call. = FALSE)
      return(restore_native(model, spec$backend))
    }
    private <- new.env(parent = asNamespace("stats"))
    private$.example_data <- data
    private_formula <- tryCatch(eval(spec$call$formula, envir = private), error = function(e) stop(
      "Could not resolve the isolated example formula: ", conditionMessage(e), call. = FALSE))
    if (!inherits(private_formula, "formula")) stop("Example fits require a literal formula.", call. = FALSE)
    environment(private_formula) <- private
    model <- tryCatch(fit_native(spec$call, private), error = function(e) stop(
      "Could not fit the isolated example model: ", conditionMessage(e), call. = FALSE))
    if (!valid_model(model, spec$backend)) stop("The native example did not return the expected model class.", call. = FALSE)
    # A bare private symbol would fail in native update() from another caller.
    # Embed the formula object (and its small data-only environment) instead of
    # embedding a printed copy of every observation or retaining a knit frame.
    # Preserve native additions such as glm.nb's fitted init.theta and
    # glmmTMB's component formulas: update()/step refits rely on these.
    model$call[[1L]] <- spec$call[[1L]]
    model$call$formula <- private_formula
    model$call$data <- substitute(get(".example_data", envir = environment(FORMULA)),
      list(FORMULA = private_formula))
    if (!is.null(path)) {
      bytes <- serialize(model, NULL, version = 3L)
      record <- list(identity = identity, model = bytes, checksum = hash(bytes, serialise = FALSE))
      staging <- tempfile("influ2-example-write-", tmpdir = cache_dir)
      on.exit(unlink(staging), add = TRUE)
      saveRDS(record, staging, version = 3L)
      if (!file.rename(staging, path)) stop("Could not store the temporary example model.", call. = FALSE)
    }
    model
  }
})
