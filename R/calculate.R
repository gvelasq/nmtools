#' Calculate PsN bootstrap confidence intervals
#'
#' @description
#' `calculate_ci()` calculates confidence intervals from PsN bootstraps.
#'
#' @usage
#' calculate_ci(x, parameter = "THETA3", ci_width = 95, exclusions = c(1, 2))
#'
#' @param x A tibble corresponding to PsN bootstrap output. `calculate_ci()` is meant to be run after running `combine_bootstraps()` but will also work with an imported `raw_results_run#.csv` file created by PsN.
#' @param parameter The parameter of interest. Defaults to `THETA3`.
#' @param ci_width The desired confidence interval width. Defaults to `95` for a 95`%` confidence interval.
#' @param exclusions A numeric vector corresponding to the desired PsN exclusions. Defaults to `c(1, 2)` for exclusions 1 and 2, the default PsN exclusions. The available exclusions are:
#'
#' - Exclusion 1: Skip if `minimization_successful == 0`.
#' - Exclusion 2: Skip if `estimate_near_boundary == 1`.
#' - Exclusion 3: Skip if `covariance_step_successful == 0`.
#' - Exclusion 4: Skip if `covariance_step_warnings == 1`.
#'
#' @return
#' A tibble that contains:
#'
#' - `n_original`: The total number of rows.
#' - `n_used`: The number of successful runs used to calculate the confidence interval.
#' - `n_excluded`: The number of excluded runs.
#' - `parameter`: The parameter of interest.
#' - `ci_width`: The width of the confidence interval.
#' - `lower_ci`: The lower value of the calculated confidence interval.
#' - `point_estimate`: The point estimate from the original dataset.
#' - `upper_ci`: The upper value of the calculated confidence interval.
#'
#' `calculate_ci()` also provides verbose output in the console listing the values of `n_original`, `n_used`, `n_excluded`, the exclusions used, and a printed tibble with the variables above.
#'
#' @aliases
#' calculate
#' calculate_ci
#'
#' @rdname calculate
#' @export
calculate_ci <- function(x, parameter = "THETA3", ci_width = 95, exclusions = c(1, 2)) {
  if (!is.data.frame(x)) {
    rlang::abort("Must provide a data frame or tibble with PsN bootstrap output")
  }
  if (!is.character(parameter)) {
    rlang::abort("Must provide a character vector of the parameter of interest")
  }
  parameter <- rlang::arg_match(parameter, values = c("ofv", "THETA1", "THETA2", "THETA3", "THETA4", "OMEGA(1,1)", "SIGMA(1,1)"))
  ci_width <- as.character(ci_width)
  ci_width <- rlang::arg_match(ci_width, values = c("90", "95", "99", "99.9"))
  if (!is.numeric(exclusions)) {
    rlang::abort("Must provide a numeric vector of exclusions")
  }
  n_original <- nrow(x)
  n_excluded <- 0L
  if (1 %in% exclusions) {
    n_used <- nrow(x)
    x <- dplyr::filter(x, minimization_successful == 1)
    n_excluded <- n_excluded + (n_used - nrow(x))
  }
  if (2 %in% exclusions) {
    n_used <- nrow(x)
    x <- dplyr::filter(x, estimate_near_boundary == 0)
    n_excluded <- n_excluded + (n_used - nrow(x))
  }
  if (3 %in% exclusions) {
    n_used <- nrow(x)
    x <- dplyr::filter(x, covariance_step_successful == 1)
    n_excluded <- n_excluded + (n_used - nrow(x))
  }
  if (4 %in% exclusions) {
    n_used <- nrow(x)
    x <- dplyr::filter(x, covariance_step_warnings == 0)
    n_excluded <- n_excluded + (n_used - nrow(x))
  }
  n_used <- nrow(x)
  assertthat::are_equal(n_original, n_used + n_excluded)
  if (ci_width == "90") {
    psn_coef <- 1.6449
  }
  if (ci_width == "95") {
    psn_coef <- 1.96
  }
  if (ci_width == "99") {
    psn_coef <- 2.5758
  }
  if (ci_width == "99.9") {
    psn_coef <- 3.2905
  }
  cat(glue::glue("Observations in original dataset:  n = {n_original}"), sep = "\n")
  cat(glue::glue("Observations excluded:             n = {n_excluded} (exclusions "), sep = "")
  cat(glue::glue_collapse(glue::glue("{exclusions}"), sep = ", "), sep = "")
  cat(")", sep = "\n")
  cat(glue::glue("Observations used to calculate CI: n = {n_used}"), sep = "\n")
  glue::glue_collapse(glue::glue("{exclusions}"), sep = ", ")
  x_boot <- dplyr::slice(x, -1)
  point_estimate <- x[[glue::glue("{parameter}")]][[1]]
  lower_ci <- point_estimate - (psn_coef * sd(x_boot[[glue::glue("{parameter}")]]))
  upper_ci <- point_estimate + (psn_coef * sd(x_boot[[glue::glue("{parameter}")]]))
  output <- tibble::tibble(n_original, n_used, n_excluded, parameter, ci_width, lower_ci, point_estimate, upper_ci)
  cat("\n")
  print.data.frame(output, row.names = FALSE)
  invisible(output)
}
