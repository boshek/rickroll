#' Generate a dataframe of installed packages
#'
#' @return dataframe of all packages installed on a system
#' @export
lib <- function() {
  pkgs <- utils::installed.packages()
  as.data.frame(pkgs, stringsAsFactors = FALSE)
}

#' calculate sizes
#'
#' @param df a data.frame
#'
#' @return df with a lib_size column
#' @noRd
calculate_sizes <- function(df) {
  df$lib_size <- map_dbl(
    df$Library,
    \(x) sum(fs::file_size(fs::dir_ls(x, recurse = TRUE)))
  )
  df
}

#' Provides a brief summary of the package libraries on your machine
#'
#' @param sizes Should the sizes of the libraries be calculated?
#'   Logical; default `FALSE`.
#'
#' @return A data.frame containing the count of packages in each of the user's
#'   libraries. A `lib_size` column is included if `sizes = TRUE`.
#' @export
#'
#' @examplesb_s
#' lib_summary()
#' lib_summary(sizes = TRUE)
lib_summary <- function(sizes = FALSE) {
  if (!is.logical(sizes)) {
    stop("sizes should be a logical (TRUE/FALSE) value", call. = FALSE)
  }
  pkg_df <- lib()
  pkg_df <- table(pkg_df$LibPath)
  pkg_df <- as.data.frame(pkg_df, stringsAsFactors = FALSE)
  names(pkg_df) <- c("Library", "n_packages")

  if (sizes) {
    pkg_df <- calculate_sizes(pkg_df)
  }
  pkg_df
}
