# snic 0.6.1

* Added a `NEWS.md` file to track changes to the package.
* Added `terra_is_working()` and guards around CRS-dependent examples, 
  tests, and vignettes so that CRAN checks no longer fail when `terra` 
  cannot access PROJ data (notably on Windows `oldrel`).
* Fixed minor issues in vignettes and improve figures' legend format.
