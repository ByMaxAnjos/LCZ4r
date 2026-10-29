## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* The URL https://uco.berlin/ (data source of `lcz_data`) returns 403 to
  automated checkers but is valid in a web browser.

## Comments

* Functions that download LCZ maps (`lcz_get_map()`, `lcz_get_map_euro()`,
  `lcz_get_map_usa()`, `lcz_get_map_generator()`) require internet access, so
  their examples are wrapped in `\donttest{}`. If the remote resource is not
  available they return `NULL` with an informative message instead of an error.
* Interpolation examples (`lcz_interp_map()`, `lcz_anomaly_map()`,
  `lcz_interp_eval()`) take more than 5 seconds and are wrapped in `\donttest{}`.
* All other examples run on a small LCZ map shipped in `inst/extdata`.
* Functions only write files when the user explicitly sets `isave = TRUE`.
