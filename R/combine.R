#' Combine PsN bootstraps
#'
#' @description
#' `combine_bootstraps()` extracts and row-binds model estimates of PsN bootstraps into a tibble.
#'
#' @usage
#' combine_bootstraps(path, runs = "bs_run1", out = "bootstrap_combined.csv")
#'
#' @param path The path to the working directory containing PsN bootstrap folders. Defaults to `here::here()`.
#' @param runs A character vector of folder names to be searched. Defaults to `bs_run1` per default PsN settings.
#' @param out The name of the output .csv file. Defaults to `bootstrap_combined.csv`.
#'
#' @details
#' `combine_bootstraps()` searches inside all folders specified by the `runs` argument for model estimates of PsN bootstraps.
#'
#' By PsN convention, each bootstrap folder has a file of the general form `raw_results_run#.csv` that contains estimates for the original and bootstrapped models.
#' The first row in each `raw_results_run#.csv` file corresponds to the original dataset.
#' To avoid duplicating the row corresponding to the original dataset, `combine_bootstraps()` keeps only the first row of the first .csv file passed to the `runs` argument.
#' `combine_bootstraps()` throws an informative warning if the column names or the first row of each `raw_results_run#.csv` file are not identical.
#'
#' `combine_bootstraps()` creates a new .csv file in the working directory, specified by the `out` argument, that contains the combined estimates of PsN bootstraps.
#'
#' @return
#' A tibble containing the row-bound model estimates of PsN bootstraps.
#' @aliases
#' combine
#' combine_bootstraps
#'
#' @rdname combine
#' @export
combine_bootstraps <- function(path, runs = "bs_run1", out = "bootstrap_combined.csv") {
  path_set <- FALSE
  if (rlang::is_missing(path)) {
    path_set <- TRUE
    path <- here::here()
  }
  if (!fs::dir_exists(path)) {
    rlang::abort("Must provide a valid path to a working directory")
  }
  path <- fs::path_expand(path)
  if (path_set) {
    cat(glue::glue("Path (set automatically): {path}"), sep = "\n")
  } else {
    cat(glue::glue("Path: {path}"), sep = "\n")
  }
  if (!is.character(runs)) {
    rlang::abort("Must provide a character vector of bootstrap run folder names")
  }
  cat("Folders:", glue::glue_collapse(glue::glue("{runs}"), sep = ", "), "\n")
  x <- tibble::tibble(folder = runs, folder_path = path)
  x <- dplyr::mutate(x, folder_path = fs::path(folder_path, folder), path_exists = fs::dir_exists(folder_path))
  if (!all(x$path_exists)) {
    rlang::abort("Must provide valid path(s) to bootstrap run folder names")
  }
  x <- dplyr::mutate(x, path = fs::dir_ls(folder_path, regexp = "[/]raw(.*)[.]csv$"))
  x <- suppressMessages(dplyr::mutate(x, raw_file = purrr::map(path, ~ readr::read_csv(.))))
  nms <- purrr::map(x$raw_file, ~ colnames(.))
  nms_identical <- all(purrr::map_lgl(nms, identical, nms[[1]]))
  if (!nms_identical) {
    rlang::warn("Column names in individual bootstrap runs (raw_results_run#.csv) are not identical")
  }
  model_0 <- purrr::map_dfr(x$raw_file, ~ dplyr::slice(., 1))
  model_0_n_distinct <- dplyr::n_distinct(model_0)
  if (model_0_n_distinct != 1L) {
    rlang::warn("Unequal first row (model 0) across bootstrap runs")
  }
  x <- dplyr::select(x, -folder_path, -path_exists)
  unnested <- tidyr::unnest(x)
  unnested_firstrow <- dplyr::slice(unnested, 1)
  if (unnested_firstrow$model != 0L) {
    rlang::warn("First row of merged dataset does not correspond to the original dataset (model 0)")
  }
  unnested <- dplyr::filter(unnested, model != 0L)
  merged <- suppressWarnings(dplyr::bind_rows(unnested_firstrow, unnested))
  merged <- dplyr::mutate(merged, path = fs::as_fs_path(path))
  readr::write_csv(merged, glue::glue("{path}/{out}"))
  cat(glue::glue("Output: {path}/{out}"), sep = "\n")
  invisible(merged)
}
