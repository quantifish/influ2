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
    # The focus column keeps its own type so all components share its levels.
    if (!identical(group, focus)) random_data[[group]] <- values
    identified[[group]] <- TRUE
  }
  list(term_columns = term_columns, data = random_data, identified = identified)
}

.random_terms_table <- function(identified, component) {
  data.frame(term = names(identified), component = rep(component, length(identified)),
    identified = unname(identified), stringsAsFactors = FALSE)
}
