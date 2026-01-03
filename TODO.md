# TODO: Package Improvements

## Completed

### CRAN/Bioconductor Sidebar Check Timeout (Fixed 2026-01-02)

- **Problem**: pkgdown’s `data_home_sidebar_links` function always calls
  `cran_link()` which makes HTTP requests to cloud.r-project.org and
  bioconductor.org, causing timeouts
- **Root cause**: The function eagerly evaluates ALL default sidebar
  components even when they’re not in the structure
- **Solution**: Use custom sidebar HTML (`pkgdown/sidebar.html`) which
  bypasses the default component generation entirely
- See commit with this fix for details

## Open Issues

### Vignette Data Downloads

- Vignette `enrollment_hooks.Rmd` dynamically fetches data from FLDOE
- If FLDOE servers are slow, vignette build may timeout
- **Mitigations in place**:
  - Package uses caching (rappdirs) for downloaded data
  - httr timeout set to 300 seconds in download functions
- **If issues occur**: Consider pre-computing vignette data or using
  knitr caching

## Date

2026-01-02
