#' Jensen-Shannon Divergence
#'
#' @param formula A formula of the form 'x1 ~ x2 + x3 + x4 + x5'. In this
#'   example, the JSD assumes x1 is the group variable and x2 through x5 are
#'   covariates. All variabels must be categorical (character or factor).
#' @param data A data frame containing all variables mentioned in 'formula'.
#' @return A named list with entries for each variable on the righthand side of
#'   `formula`. See details.
#' @details This function produces objects of class `jsd_balance`, each of which
#'   is a list containing the following elements:
#'
#'   1. freqs - the cell frequencies of the 2x2 table of the grouping variable
#'   against the covariate.
#'
#'   2. cell_contribs - individual cell contributions to the Jensen-Shannon
#'   divergence (JSD).
#'
#'   3. group_contribs - group-level contributions to the JSD. These are the
#'   column sums of `cell_contribs`.
#'
#'   4. cov_contribs - covariate-level contributions to the JSD. These are the
#'   row sums of `cell_contribs`.
#'
#'   5. jsd - overall JSD measure. This is the sum of all elements in
#'   `cell_contribs`.
#'
#' @examples
#' g_chop <- chop(glucose, glucose = list(cuts = c(0,109,125,300)))
#' jsd_balance(cohort ~ glucose, data = g_chop)
#' @rdname JSD
#' @export
jsd_balance <- function(formula, data) {

  group_var <- rlang::f_lhs(formula)
  if (!rlang::is_symbol(group_var, name = colnames(data))) {
    stop("Lefthand side of 'formula' must be a single column name in 'data'")
  }

  group_vctr <- data[[rlang::as_string(group_var)]]

  if (!is.factor(group_vctr) && !is.character(group_vctr)) {
    if (is.numeric(group_vctr)) {
      stop(
        "Lefthand side of formula must be a factor or character vector.",
        "\nRun chop() first."
      )
    } else {
      stop(
        "Lefthand side of formula must be a numeric, factor, or character vector"
      )
    }
  }

  if (anyNA(group_vctr)) stop("data must not have any missing values")

  covars <-
    formula %>%
    terms() %>%
    attr("variables") %>%
    rlang::call_args() %>%
    `[`(-1L)

  covars %>%
    rlang::set_names() %>%
    lapply(
      function(x) {

        if (!rlang::is_symbol(x, name = colnames(data))) {
          stop("Each variable on righthand side of formula must be in data")
        }

        vec <- data[[rlang::as_string(x)]]

        if (anyNA(vec)) stop("data must not have any missing values")

        freqs <- rlang::eval_tidy(rlang::expr(table(!!x, !!group_var)), data)

        group_dists <- sweep(freqs, 2L, apply(freqs, 2L, sum), "/")

        common_dists <- apply(group_dists, 1L, mean)

        cell_contribs <-
          sweep(group_dists, 1L, common_dists, function(x, y) x * log2(x / y)) /
          length(unique(group_vctr))

        group_contribs <- apply(cell_contribs, 2L, sum)

        cov_contribs <- apply(cell_contribs, 1L, sum)

        jsd <- sum(cell_contribs)

        list(
          freqs = freqs,
          cell_contribs = cell_contribs,
          group_contribs = group_contribs,
          cov_contribs = cov_contribs,
          jsd = jsd
        ) %>%
          structure(class = "jsd_balance")
      }
    )
}
