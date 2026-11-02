test_that("all language catalogues define exactly the same keys", {
  codes <- names(lcz_langs())
  expect_identical(codes, c("en", "pt", "es", "fr", "zh", "de", "it", "ja", "ko", "ru", "ar"))

  keys <- lapply(codes, function(code) {
    env <- environment(lcz_tr)
    catalog <- get(".lcz_i18n", envir = env)$catalog
    names(catalog[[code]])
  })
  ref <- keys[[1]]
  expect_true(length(ref) > 80)
  for (i in seq_along(keys)) {
    expect_setequal(keys[[i]], ref)
  }
})

test_that("translation falls back to English for unknown keys", {
  expect_identical(lcz_tr("definitely_not_a_key", lang = "pt"), "definitely_not_a_key")
  expect_identical(lcz_tr("lcz_class", lang = "pt"), "Classe LCZ")
  expect_identical(lcz_tr("lcz_class", lang = "en"), "LCZ class")
})

test_that("lcz_set_lang() changes the session default and validates input", {
  old <- lcz_set_lang("pt")
  expect_identical(getOption("LCZ4r.lang"), "pt")
  expect_identical(lcz_tr("time"), "Tempo")        # lang = NULL -> option
  lcz_set_lang(old)

  expect_error(lcz_set_lang("xx"), "Unsupported language")
})

test_that("language aliases are normalised", {
  old <- lcz_set_lang("pt-BR")
  expect_identical(getOption("LCZ4r.lang"), "pt")
  lcz_set_lang("zh-Hans")
  expect_identical(getOption("LCZ4r.lang"), "zh")
  lcz_set_lang(old)
})

test_that("the 17 LCZ names are translated, non-empty and complete", {
  for (code in names(lcz_langs())) {
    nm <- lcz_class_names(code)
    expect_length(nm, 17L)
    expect_false(anyNA(nm))
    expect_true(all(nzchar(nm)))
    expect_identical(nm[17], lcz_tr("lcz_name_17", lang = code))
  }
  expect_false(identical(lcz_class_names("pt"), lcz_class_names("en")))
})

test_that("lcz_palette() returns 17 named colours in both variants", {
  p <- lcz_palette()
  expect_length(p, 17L)
  expect_identical(names(p), as.character(c(1:10, 11:17)))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", p)))

  p2 <- lcz_palette(inclusive = TRUE)
  expect_length(p2, 17L)
  expect_false(identical(p, p2))

  expect_length(lcz_palette(named = FALSE), 17L)
})

test_that("parameter labels are composed from the dictionary", {
  expect_identical(.lcz_param_label("SVFmean", "en"), "Mean Sky View Factor")
  expect_identical(.lcz_param_label("SVFmin", "pt"), "Fator de Visão do Céu (Mínimo)")
  expect_identical(.lcz_param_label("z0", "es"), "Longitud de Rugosidad")
})

test_that("LCZ class names follow the published terminology", {
  # Class G is a land-cover type ("water bodies"), not the substance "water"
  expect_identical(lcz_class_names("en")[17], "Water")
  expect_identical(lcz_class_names("pt")[17], "Corpos d'água")
  expect_identical(lcz_class_names("es")[17], "Cuerpos de agua")
  expect_identical(lcz_class_names("fr")[17], "Surface en eau")
  expect_identical(lcz_class_names("de")[17], "Gew\u00e4sser")
  expect_identical(lcz_class_names("it")[17], "Corpi idrici")
  expect_identical(lcz_class_names("zh")[17], "\u6c34\u4f53")
  expect_identical(lcz_class_names("ja")[17], "\u6c34\u57df")
  expect_identical(lcz_class_names("ko")[17], "\uc218\uc5ed")
  expect_identical(lcz_class_names("ru")[17], "\u0412\u043e\u0434\u043e\u0451\u043c\u044b")
  expect_identical(lcz_class_names("ar")[17], "\u0645\u0633\u0637\u062d\u0627\u062a \u0645\u0627\u0626\u064a\u0629")

  expect_identical(lcz_class_names("pt")[9], "Ocupa\u00e7\u00e3o esparsa")
  expect_identical(lcz_class_names("fr")[1], "Ensemble compact de tours")

  for (code in names(lcz_langs())) {
    nm <- lcz_class_names(code)
    expect_equal(length(unique(nm)), 17L, info = code)
  }
})
