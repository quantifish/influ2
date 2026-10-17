# Random-effect design columns grouped into one influence term per grouping
# factor, named after the factor as for brms group-level terms. Terms that
# share a factor, such as (1 | vessel) + (0 + depth | vessel), form one term.
# Each term's CDI categories are the factor's fitted levels.
.random_group_terms <- function(groups, sizes, factors, data, focus, backend) {
  ends <- cumsum(sizes)
  columns <- lapply(seq_along(sizes), function(k) {
    seq.int(ends[k] - sizes[k] + 1L, length.out = sizes[k])
  })
  term_columns <- lapply(
    split(columns, factor(groups, levels = unique(groups))),
    unlist, use.names = FALSE
  )
  random_data <- data
  identified <- stats::setNames(logical(length(term_columns)), names(term_columns))
  for (group in names(term_columns)) {
    values <- factors[[group]]
    if (is.null(values) || length(values) != nrow(data)) next
    if (group %in% names(data) &&
        !identical(as.character(data[[group]]), as.character(values))) {
      stop("The ", backend, " grouping data do not match the fitted random-effect rows.",
        call. = FALSE)
    }
    # Every grouping variable is categorical, even when it is also a numeric
    # focus. Preserve the focus's declared/numeric order so the fixed and
    # random components still align, without binning its fitted group IDs.
    random_data[[group]] <- if (identical(group, focus)) {
      factor(as.character(values), levels = .focus_info(data, focus)$levels)
    } else values
    identified[[group]] <- TRUE
  }
  list(term_columns = term_columns, data = random_data, identified = identified)
}

.random_terms_table <- function(identified, component) {
  data.frame(term = names(identified), component = rep(component, length(identified)),
    identified = unname(identified), stringsAsFactors = FALSE)
}
