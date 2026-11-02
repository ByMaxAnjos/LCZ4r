#' LCZ4r tutorials
#'
#' Opens an LCZ4r tutorial in your default browser. Requires an internet
#' connection.
#'
#' @param x `"general"` (default) for general functions or `"local"` for local
#'   functions.
#' @param lang Language used for console messages and errors. See
#'   [lcz_langs()]. Defaults to `"en"`.
#'
#' @return Invisibly, `NULL`. Called for its side effect (opening a browser).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' lcz_tutorial("general")
#' lcz_tutorial("local", lang = "pt")
#' }
lcz_tutorial <- function(x = "general", lang = "en") {
  if (!is.character(x) || length(x) != 1L) {
    lcz_abort("err_tutorial", lang = lang)
  }
  x <- match.arg(x, c("general", "local"))

  urls <- c(
    general = "https://bymaxanjos.github.io/LCZ4r/articles/Introd_general_LCZ4r.html",
    local = "https://bymaxanjos.github.io/LCZ4r/articles/Introd_local_LCZ4r.html"
  )

  utils::browseURL(urls[[x]])
  invisible(NULL)
}
