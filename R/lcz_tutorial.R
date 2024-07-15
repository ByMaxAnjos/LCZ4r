#' LCZ4r Tutorials
#'
#' Opens LCZ4r tutorial in a web browser. This function requires an internet connection.
#'
#' @param x Character string. Either "general" for general functions or "local" for local functions. Default is "general".
#'
#' @return Opens the LCZ4r tutorial in a web browser.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lcz_tutorial("general")
#' lcz_tutorial("local")
#' }
lcz_tutorial <- function(x = "general") {
  if (!is.character(x) || length(x) != 1) {
    stop("Argument 'x' must be a single character string: either 'general' or 'local'.")
  }

  valid_values <- c("general", "local")
  if (!(x %in% valid_values)) {
    stop("Invalid value for argument 'x'. Choose either 'general' or 'local'.")
  }

  urls <- list(
    general = "https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html",
    local = "https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html"
  )

  utils::browseURL(urls[[x]])
  invisible(NULL)
}
