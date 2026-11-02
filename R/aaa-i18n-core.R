# LCZ4r internationalisation engine -------------------------------------------
#
# Every exported function gains a `lang = "en"` argument.  Translations live in
# R/i18n-<lang>.R files, each of which registers one flat dictionary through
# `lcz_register_translation()` (called at package load time).  This file is
# prefixed with `aaa-` so that it is collated before the dictionaries.
#
# Design goals:
#   * zero hard dependency on JSON/YAML (works offline, no parsing failures);
#   * graceful fallback: requested language -> English -> key name;
#   * plural-free, placeholder-based strings ({arg}, {method}, ...);
#   * one file per language, so adding a language is a self-contained PR.

.lcz_i18n <- new.env(parent = emptyenv())
.lcz_i18n$lang <- "en"
.lcz_i18n$catalog <- list()
.lcz_i18n$warned <- character()

# Canonical language codes shipped with the package.  Order matters only for
# documentation/printing.
.lcz_langs <- c("en", "pt", "es", "fr", "zh", "de", "it", "ja", "ko", "ru", "ar")

# Human-readable names, kept in English on purpose: they are metainformation
# about the translation, not content that itself needs translating.
.lcz_lang_names <- c(
  en = "English",
  pt = "Portugu\u00eas",
  es = "Espa\u00f1ol",
  fr = "Fran\u00e7ais",
  zh = "\u4e2d\u6587\uff08\u7b80\u4f53\uff09",
  de = "Deutsch",
  it = "Italiano",
  ja = "\u65e5\u672c\u8a9e",
  ko = "\ud55c\uad6d\uc5b4",
  ru = "\u0420\u0443\u0441\u0441\u043a\u0438\u0439",
  ar = "\u0627\u0644\u0639\u0631\u0628\u064a\u0629"
)

#' Register a translation dictionary
#'
#' Internal helper used by the `R/i18n-*.R` files.  Not exported: this is a
#' developer API.
#'
#' @param lang Two-letter language code.
#' @param dict Named list of `key = "translation"` pairs.
#' @return Invisibly, `TRUE`.
#' @keywords internal
#' @noRd
lcz_register_translation <- function(lang, dict) {
  lang <- tolower(substr(lang, 1L, 2L))
  existing <- .lcz_i18n$catalog[[lang]]
  if (is.null(existing)) {
    .lcz_i18n$catalog[[lang]] <- dict
  } else {
    # Later registrations win, which lets a package user override a string.
    .lcz_i18n$catalog[[lang]] <- utils::modifyList(existing, dict)
  }
  invisible(TRUE)
}

# Normalise user input: "pt-BR", "pt_BR", "PT" -> "pt"; "zh-Hans" -> "zh".
.lcz_normalise_lang <- function(lang) {
  if (is.null(lang) || !length(lang) || is.na(lang) || !nzchar(lang)) {
    return(NULL)
  }
  code <- tolower(gsub("_", "-", as.character(lang)[1L]))
  code <- sub("-.*$", "", code)
  aliases <- c(
    pb = "pt", br = "pt",
    gb = "en", us = "en", uk = "en",
    cn = "zh", tw = "zh", hk = "zh", sg = "zh",
    jp = "ja", kr = "ko",
    gr = "el"
  )
  if (code %in% names(aliases)) code <- aliases[[code]]
  if (!code %in% .lcz_langs) {
    return(NULL)
  }
  code
}

#' Available languages
#'
#' Lists the languages supported by LCZ4r for console messages, plot labels and
#' LCZ class names.
#'
#' @return A named character vector: language codes as names, language names as
#'   values.
#'
#' @examples
#' lcz_langs()
#'
#' @export
lcz_langs <- function() {
  stats::setNames(unname(.lcz_lang_names), names(.lcz_lang_names))
}

#' Set the default language
#'
#' Stores the session-wide default used when `lang = NULL` (or omitted) in
#' LCZ4r functions.
#'
#' @param lang Two-letter language code. See [lcz_langs()] for the available
#'   codes. `"en"` restores the default.
#'
#' @return Invisibly, the previous language code.
#'
#' @examples
#' old <- lcz_set_lang("pt")
#' lcz_set_lang(old)
#'
#' @export
lcz_set_lang <- function(lang = "en") {
  code <- .lcz_normalise_lang(lang)
  if (is.null(code)) {
    stop(sprintf(
      "Unsupported language '%s'. Available: %s.",
      paste(lang, collapse = ", "), paste(.lcz_langs, collapse = ", ")
    ), call. = FALSE)
  }
  old <- getOption("LCZ4r.lang", "en")
  options(LCZ4r.lang = code)
  invisible(old)
}

#' Current default language
#'
#' @return A two-letter language code.
#' @keywords internal
#' @noRd
lcz_get_lang <- function() {
  code <- .lcz_normalise_lang(getOption("LCZ4r.lang", "en"))
  if (is.null(code)) "en" else code
}

# Resolve a `lang` argument (never errors: bad input falls back to English and
# warns once per session, so a typo cannot break a long-running analysis).
lcz_resolve_lang <- function(lang = NULL) {
  if (is.null(lang)) {
    return(lcz_get_lang())
  }
  code <- .lcz_normalise_lang(lang)
  if (is.null(code)) {
    key <- paste0("lang:", as.character(lang)[1L])
    if (!key %in% .lcz_i18n$warned) {
      .lcz_i18n$warned <- c(.lcz_i18n$warned, key)
      warning(sprintf(
        "Unsupported language '%s'; falling back to English. See lcz_langs().",
        as.character(lang)[1L]
      ), call. = FALSE)
    }
    return("en")
  }
  code
}

# Tiny placeholder renderer: "{arg}" is replaced by the value of `arg`.
.lcz_render <- function(template, args) {
  if (!length(args)) {
    return(template)
  }
  for (nm in names(args)) {
    value <- paste(as.character(args[[nm]]), collapse = ", ")
    template <- gsub(paste0("{", nm, "}"), value, template, fixed = TRUE)
  }
  template
}

#' Translate a message key
#'
#' Low-level translator.  Most users should rely on the `lang` argument of the
#' functions instead of calling this directly.
#'
#' @param key Message key, e.g. `"msg_files_saved"`.
#' @param lang Language code. Defaults to the session language.
#' @param ... Named values used to fill `{placeholders}` in the template.
#'
#' @return A character string. Unknown keys return the key itself, which makes
#'   missing translations easy to spot during development.
#'
#' @examples
#' lcz_tr("msg_files_saved", path = "LCZ4r_output", lang = "pt")
#'
#' @export
lcz_tr <- function(key, lang = NULL, ...) {
  lang <- lcz_resolve_lang(lang)
  dict <- .lcz_i18n$catalog[[lang]]
  template <- NULL
  if (!is.null(dict) && !is.null(dict[[key]])) {
    template <- dict[[key]]
  } else {
    en <- .lcz_i18n$catalog[["en"]]
    if (!is.null(en) && !is.null(en[[key]])) template <- en[[key]]
  }
  if (is.null(template)) {
    return(key)
  }
  .lcz_render(template, list(...))
}

# Console helpers -------------------------------------------------------------
# `call. = FALSE` keeps error messages readable inside long pipelines.

# Internal: translate then emit. `type` is one of message/warning/error.
.lcz_signal <- function(type, key, lang = NULL, ..., .call = FALSE) {
  txt <- lcz_tr(key, lang = lang, ...)
  switch(type,
    message = base::message(txt),
    warning = base::warning(txt, call. = .call, immediate. = FALSE),
    error = base::stop(txt, call. = .call)
  )
  invisible(txt)
}

#' @keywords internal
#' @noRd
lcz_msg <- function(key, lang = NULL, ...) {
  .lcz_signal("message", key, lang = lang, ...)
}

#' @keywords internal
#' @noRd
lcz_warn <- function(key, lang = NULL, ...) {
  .lcz_signal("warning", key, lang = lang, ...)
}

#' @keywords internal
#' @noRd
lcz_abort <- function(key, lang = NULL, ...) {
  .lcz_signal("error", key, lang = lang, ...)
}

# LCZ metadata ---------------------------------------------------------------

# Canonical LCZ identifiers, in plotting order.
.lcz_ids <- c(1:10, 11:17)

#' LCZ class names in a given language
#'
#' @param lang Language code.
#' @return A character vector of 17 names ordered by LCZ class (1-17).
#' @keywords internal
#' @noRd
lcz_class_names <- function(lang = NULL) {
  lang <- lcz_resolve_lang(lang)
  keys <- paste0("lcz_name_", 1:17)
  vapply(keys, lcz_tr, character(1), lang = lang, USE.NAMES = FALSE)
}

#' LCZ palette
#'
#' Official LCZ colours (Stewart & Oke 2012 / Demuzere et al. 2022), plus a
#' colour-vision-deficiency-safe alternative.
#'
#' @param inclusive Logical. Use the CVD-safe palette.
#' @param named Logical. Return a named vector keyed by LCZ class.
#' @return A character vector of 17 hex colours.
#' @keywords internal
#' @noRd
lcz_palette <- function(inclusive = FALSE, named = TRUE) {
  cols <- if (isTRUE(inclusive)) {
    c(
      "#E16A86", "#D8755E", "#C98027", "#B48C00", "#989600", "#739F00",
      "#36A631", "#00AA63", "#00AD89", "#00ACAA", "#00A7C5", "#009EDA",
      "#6290E5", "#9E7FE5", "#C36FDA", "#D965C6", "#E264A9"
    )
  } else {
    c(
      "#910613", "#D9081C", "#FF0A22", "#C54F1E", "#FF6628", "#FF985E",
      "#FDED3F", "#BBBBBB", "#FFCBAB", "#565656", "#006A18", "#00A926",
      "#628432", "#B5DA7F", "#000000", "#FCF7B1", "#656BFA"
    )
  }
  if (isTRUE(named)) stats::setNames(cols, .lcz_ids) else cols
}

# Parameter metadata ---------------------------------------------------------

# Internal parameter labels: short code -> translation key for the long name.
.lcz_param_keys <- c(
  SVF = "param_svf", AR = "param_ar", BSF = "param_bsf", ISF = "param_isf",
  PSF = "param_psf", TSF = "param_tsf", HRE = "param_hre", TRC = "param_trc",
  SAD = "param_sad", SAL = "param_sal", AH = "param_ah"
)

# Human-readable label for a parameter code such as "SVFmean".
.lcz_param_label <- function(code, lang = NULL) {
  lang <- lcz_resolve_lang(lang)
  stat <- sub("^(SVF|AR|BSF|ISF|PSF|TSF|HRE|TRC|SAD|SAL|AH|z0).*$", "\\1", code)
  if (identical(stat, "z0")) {
    return(lcz_tr("param_z0", lang = lang))
  }
  suffix <- sub("^.*?(min|max|mean)$", "\\1", code)
  if (!suffix %in% c("min", "max", "mean")) suffix <- "mean"
  prefix_key <- switch(suffix,
    min = "stat_min", max = "stat_max", mean = "stat_mean", "stat_mean"
  )
  stat_lab <- lcz_tr(prefix_key, lang = lang)
  param_lab <- lcz_tr(.lcz_param_keys[[stat]], lang = lang)
  if (identical(lang, "en")) {
    paste(stat_lab, param_lab)
  } else {
    paste0(param_lab, " (", stat_lab, ")")
  }
}

.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("LCZ4r.lang"))) {
    options(LCZ4r.lang = "en")
  }
  invisible()
}
