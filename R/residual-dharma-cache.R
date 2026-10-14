# A private, temporary column-major binary cache. Joint simulations are written
# once and reread by observation slab; neither the cache nor its connection is
# retained in a public residual object.
.resid_dharma_storage <- function(storage, residual_method, retain_dharma) {
  storage <- match.arg(storage, c("auto", "disk", "memory"))
  if (residual_method != "dharma") {
    if (storage != "auto") stop("`dharma_storage` is only used with `residual_method = 'dharma'`.", call. = FALSE)
    return("none")
  }
  if (retain_dharma && storage == "disk") {
    stop("`retain_dharma = TRUE` requires full in-memory DHARMa simulations; use `dharma_storage = 'memory'` or 'auto'.", call. = FALSE)
  }
  if (storage == "auto") if (retain_dharma) "memory" else "disk" else storage
}

.resid_dharma_cache_new <- function(n, nsim, maximum = 256, batch_size = 25L) {
  bytes <- maximum * 1024^2
  batch <- min(batch_size, nsim, floor(bytes / (24 * as.double(n))))
  # Even small jobs use at least two slabs; large jobs never read more than
  # 1,000 observations at once, regardless of a generous matrix budget.
  rows <- min(1000L, ceiling(n / 2), floor(bytes / (32 * as.double(nsim))))
  if (batch < 1 || rows < 1) {
    stop("The bounded DHARMa workspace cannot fit one response column and one observation slab within `dharma_max_mb`. Increase that limit or reduce `nsim`.", call. = FALSE)
  }
  cache <- new.env(parent = emptyenv())
  cache$n <- as.integer(n)
  cache$nsim <- as.integer(nsim)
  cache$batch_size <- as.integer(batch)
  cache$row_block_size <- as.integer(rows)
  cache$matrix_mb <- 8 * as.double(n) * as.double(nsim) / 1024^2
  cache$working_mb <- max(24 * as.double(n) * batch,
    32 * as.double(nsim) * rows) / 1024^2
  cache$written <- 0L
  cache$path <- tempfile("influ2-dharma-", fileext = ".bin")
  cache$connection <- tryCatch(file(cache$path, open = "w+b"), error = function(e) {
    if (file.exists(cache$path)) unlink(cache$path)
    stop("Could not create the temporary DHARMa simulation cache: ", conditionMessage(e), call. = FALSE)
  })
  cache
}

.resid_dharma_cache_close <- function(cache) {
  if (!is.null(cache$connection)) {
    try(close(cache$connection), silent = TRUE)
    # Connection identifiers can be reused by R. Never close a stale handle
    # twice: it might now identify a caller's graphics/output connection.
    cache$connection <- NULL
  }
  if (file.exists(cache$path)) unlink(cache$path)
  invisible(NULL)
}

.resid_dharma_cache_write <- function(cache, simulations, ids) {
  if (is.null(cache$connection) || !is.numeric(ids) || !length(ids) ||
      any(!is.finite(ids)) || any(ids != floor(ids)) ||
      !identical(as.integer(ids), seq.int(cache$written + 1L,
      length.out = length(ids))) || cache$written + length(ids) > cache$nsim ||
      !is.matrix(simulations) || !is.numeric(simulations) ||
      !identical(dim(simulations), c(cache$n, length(ids))) ||
      any(!is.finite(simulations))) {
    stop("Invalid or out-of-order DHARMa cache simulation batch.", call. = FALSE)
  }
  # Write one complete column at a time, avoiding a double copy of the batch.
  seek(cache$connection, where = 8 * as.double(cache$n) * cache$written,
    origin = "start", rw = "write")
  for (j in seq_along(ids)) {
    writeBin(as.double(simulations[, j]), cache$connection, size = 8L, endian = "little")
  }
  cache$written <- cache$written + length(ids)
  if (cache$written == cache$nsim) flush(cache$connection)
  invisible(NULL)
}

.resid_dharma_cache_read <- function(cache, first, last) {
  if (is.null(cache$connection) || cache$written != cache$nsim ||
      !is.numeric(first) || !is.numeric(last) || length(first) != 1L || length(last) != 1L ||
      !is.finite(first) || !is.finite(last) || first < 1 || last > cache$n ||
      first > last || first != floor(first) || last != floor(last) ||
      last - first + 1L > cache$row_block_size) {
    stop("Invalid observation slab or incomplete DHARMa simulation cache.", call. = FALSE)
  }
  flush(cache$connection)
  size <- as.integer(last - first + 1L)
  simulations <- matrix(NA_real_, size, cache$nsim)
  for (j in seq_len(cache$nsim)) {
    seek(cache$connection, where = 8 * (as.double(j - 1L) * cache$n + first - 1L),
      origin = "start", rw = "read")
    column <- readBin(cache$connection, what = "double", n = size, size = 8L, endian = "little")
    if (length(column) != size || any(!is.finite(column))) {
      stop("The temporary DHARMa simulation cache is incomplete or invalid.", call. = FALSE)
    }
    simulations[, j] <- column
  }
  simulations
}

.resid_dharma_cache_quantiles <- function(cache, observed, integer_response, seed) {
  state <- DHARMa::getRandomState(seed)
  on.exit(state$restoreCurrent(), add = TRUE)
  pit <- numeric(cache$n)
  for (first in seq.int(1L, cache$n, by = cache$row_block_size)) {
    last <- min(cache$n, first + cache$row_block_size - 1L)
    ids <- seq.int(first, last)
    # One seed stream, original observation order. Untied rows consume no PIT
    # randomisation, just as in a single native getQuantile() call.
    slab_pit <- DHARMa::getQuantile(.resid_dharma_cache_read(cache, first, last),
      observed[ids], integerResponse = integer_response, method = "PIT")
    .resid_dharma_validate_pit(slab_pit, length(ids))
    pit[ids] <- slab_pit
  }
  .resid_dharma_validate_pit(pit, length(observed))
  pit
}

.resid_dharma_cache_read_column <- function(cache, id) {
  if (is.null(cache$connection) || cache$written != cache$nsim || !is.numeric(id) || length(id) != 1L ||
      !is.finite(id) || id < 1 || id > cache$nsim || id != floor(id)) {
    stop("Invalid simulation column or incomplete DHARMa simulation cache.", call. = FALSE)
  }
  flush(cache$connection)
  seek(cache$connection, where = 8 * as.double(id - 1L) * cache$n,
    origin = "start", rw = "read")
  column <- readBin(cache$connection, what = "double", n = cache$n,
    size = 8L, endian = "little")
  if (length(column) != cache$n || any(!is.finite(column))) {
    stop("The temporary DHARMa simulation cache is incomplete or invalid.", call. = FALSE)
  }
  column
}
