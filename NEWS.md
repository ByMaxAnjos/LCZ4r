# LCZ4r 0.2.0

## Breaking changes

- `lcz_get_parameters(iselect = ...)` now really returns only the selected
  layers. Previously `iselect` was silently ignored while `istack = TRUE`
  (the default), so code that relied on receiving the full stack must drop
  `iselect` or set `istack = TRUE` explicitly.
- `lcz_get_parameters(istack = FALSE)` now returns a named list of rasters, as
  documented, instead of throwing an error.
- `lcz_get_parameters(ishp = TRUE)` dissolves polygons by class (one row per
  LCZ class) instead of returning every contiguous patch.
- `lcz_cal_area()` returns `lcz`, `lcz.name`, `lcz.col`, `count`, `area_km2`
  and `area_perc`; `Area_Km2` is gone.
- `raster` is no longer a dependency; all raster operations use `terra`.
- `tidyr` moved from `Suggests` to `Imports` (it was already used at runtime).
- Default plot titles are now empty and captions default to a translated
  "Source: LCZ4r" (previously hardcoded English strings).

## New features

- **`lang` argument in every exported function** (`"en"` by default).
  11 languages: English, Portuguese, Spanish, French, Chinese (Simplified),
  German, Italian, Japanese, Korean, Russian and Arabic.
- `lcz_langs()`, `lcz_set_lang()` and `lcz_tr()` for programmatic access.
- Localised LCZ class names, axis labels, legends, plot titles and console
  messages.
- Language aliases (`pt-BR`, `zh-Hans`, `es-419`, …) and graceful fallback to
  English for unsupported codes.
- `lcz_plot_map(max_pixels = )` to keep national/continental maps fast and
  memory-bounded.
- `lcz_fit_variogram()`: three-level fallback for unstable variogram fits.
- Unified visual grammar: `lcz_theme_journal()`, `lcz_theme_map()`,
  `lcz_theme_series()`, `lcz_palette()` and translated class names shared by
  every plot.
- `lcz_save_figure()`: journal-sized export (120/190 mm, 600 dpi, PDF/SVG).
- `dev/` workflow, agent briefs, `dev/check-i18n.R` and `dev/benchmark.R`.
- `testthat` suite covering i18n, parameters, areas, plots, time series,
  diagnostics and the figure system.

## Terminology review (LCZ class names)

All 17 class names were reviewed against published national nomenclatures.
Highlights:

- Class G is a land-cover type, so "Water" is translated as *water bodies*
  (`pt`: "Corpos d'água", `es`: "Cuerpos de agua", `fr`: "Surface en eau",
  `de`: "Gewässer", `it`: "Corpi idrici", `zh`: 水体, `ja`: 水域, `ko`: 수역,
  `ru`: Водоёмы, `ar`: مسطحات مائية).
- French now follows the Cerema national LCZ nomenclature
  (*Ensemble compact de tours*, *Bâtiments bas de grande emprise*, …).
- German follows the LCZ nomenclature used in German urban-climate reports
  (*Kompakte hohe Bebauung*, *Gewässer*, …).
- Spanish follows the Argentine peer-reviewed nomenclature (SciELO/AJEA).
- Russian follows the published LCZ table (Vestnik SGUGIT; Obukhov 2024).
- Chinese/Japanese/Korean follow, respectively, the GEE Chinese LCZ catalogue,
  Mochida et al. and KEI/KPA usage.
- Portuguese: "Esparsamente construída" → "Ocupação esparsa";
  "Leve de baixa altura" → "Construções leves de baixa altura";
  "Grande de baixa altura" → "Grandes construções de baixa altura".

The full 17 × 11 table with sources is in `dev/terminology.md`.

## Figure design (Nature/Science-level)

- New `lcz_theme_journal()` and `lcz_save_figure()`: 120/190 mm column widths,
  600 dpi or vector output, sans-serif 11 pt type scale, left-aligned titles and no top/right spines.
- `lcz_save_figure()` writes PDF (cairo, fonts embedded), SVG (svglite) or
  PNG/TIFF (ragg), falling back gracefully when optional packages are absent.
- Colour-blind-safe Okabe-Ito palette is now the default for categorical series
  (`palette = "okabe"`); sequential variables use viridis/magma and anomalies a
  symmetric blue-white-red scale.
- `lcz_uhi_intensity(group = TRUE)` no longer uses a dual y-axis: it now draws
  two aligned panels, (a) urban/rural temperature and (b) UHI intensity.
- `lcz_anomaly()` uses a symmetric diverging scale, ordered bars and a corrected
  dot plot (horizontal axis = air temperature, colour = anomaly).
- `lcz_ts()` uses 0.55 pt lines, adaptive date labels, “LCZ n” facet labels and
  warming stripes centred on the series mean.
- `lcz_plot_map()` gains optional `scalebar`/`north_arrow`, drawn in data
  coordinates without extra dependencies, and a single-column legend by default (`legend_ncol = 2` available) so the map keeps its area.
- `lcz_plot_parameters()` and `lcz_plot_interp()` use horizontal 70-75 mm
  colour bars, units in the scale title and a translated legend title.
- Figure heights adapt to content: anomalies grow with the number of stations
  and area charts with the number of classes.
- Full specification and checklist: `dev/design-system.md`.

## Bug fixes

- Removed `options(warn = -1)` from the map getters; it permanently silenced
  all warnings in the user's session.
- Fixed the unreachable `z0 = 1` branch for LCZ class 2.
- Fixed `ldw` typo in `lcz_uhi_intensity()` (line width was ignored).
- Fixed urban/rural station matching by name, which collapsed groups when a
  station name appeared at several coordinates; selection now uses location.
- Fixed vector recycling when `Turban`/`Trural` contain several stations.
- Fixed `lcz_ts()` producing an empty plot when no station could be assigned
  to an LCZ class.
- Fixed `gstat.cv(nfold = 5)` failing on time steps with fewer than five
  observations.
- Fixed `lcz_plot_map()` mixing `raster` and `terra` and copying whole rasters.
- Fixed `lcz_plot_interp()` copying whole rasters just to compute min/max.
- Replaced deprecated `tibble::as.tibble()`.
- Corrected the TRC unit in `lcz_plot_parameters()` (`[-]`, not `[m]`).
- Area labels in `lcz_cal_area()` now match the km² axis (were percentages).

## Diagnostics and robustness

- `lcz_uhi_intensity()` no longer emits openair's "duplicate dates" warning:
  stations are averaged within each reference group before the time
  aggregation, so `timeAverage()` sees one row per group per timestamp.

- `lcz_check_raster()` now selects the layer named `lcz` (or `class`) in
  multi-layer stacks instead of blindly taking layer 2. This fixes failures when
  a stack's first layer is the LCZ map.
- `lcz_uhi_intensity()` now reports how many stations matched the map and which
  LCZ classes were found before failing, instead of a generic "no urban/rural
  stations" message.
- When no station falls inside the map, the error now explicitly points to the
  raster extent/CRS and the station coordinates.

## Performance

Measured on a 300 × 300 raster (90k cells), R 4.5.1:

| Function | Before | After | Speed-up |
|---|---|---|---|
| `lcz_get_parameters(istack = TRUE)` | 8.35 s | 0.10 s | ~83× |
| `lcz_ts(extract.method = "two.step")` | 2.65 s | 0.16 s | ~16× |
| `lcz_plot_map()` | 0.25 s | 0.07 s | ~3.6× |
| `lcz_cal_area(iplot = FALSE)` | 0.10 s | 0.03 s | ~3.3× |

## Documentation

- New package-level help page: `?LCZ4r` lists the general and local functions,
  languages and the figure system.
- README (English, Portuguese, Spanish and Chinese) now documents the 11
  languages, the journal figure system and quick-start examples for maps and
  UHI figures.
- `dev/design-system.md`: figure specification and review checklist.
- `dev/terminology.md`: 17 LCZ classes x 11 languages with sources.
- `AUDITORIA.md`: technical and scientific audit of the 0.2.0 changes.

## Internal

- Translation engine in `R/aaa-i18n-core.R`, one file per language.
- Shared helpers in `R/lcz_utils.R` and `R/lcz_map_helpers.R`.
- The three map getters now delegate to a single clip/download pipeline.

## Known limitations

- `lcz_anomaly_map()` and `lcz_interp_eval()` can still fail on very small
  station networks (< 5 stations per LCZ class); this predates 0.2.0.
- The `SAD` (surface admittance) rows of the LCZ parameter table mix units and
  need to be verified against Stewart & Oke (2012), Table 3.
- Vignettes live in `vignettes-<lang>/` and are not built by `R CMD build`.

## Translation status

The `de`, `it`, `ja`, `ko`, `ru` and `ar` catalogues were written for this
release and should be reviewed by native speakers before a CRAN submission.
