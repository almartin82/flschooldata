# TODO: pkgdown Build Issues

## Current Status
pkgdown build is failing due to network timeout issues.

## Error Details

### Issue 1: CRAN/Bioconductor Sidebar Check Timeout
- pkgdown attempts to check cloud.r-project.org and bioconductor.org for package availability
- Default httr2 timeout of 10 seconds is insufficient when network is slow
- Error: "Timeout was reached [cloud.r-project.org]: Connection timed out after 10001 milliseconds"

### Issue 2: FLDOE Data Download Timeout
- Vignette `enrollment_hooks.Rmd` dynamically fetches data from FLDOE
- FLDOE servers (www.fldoe.org) are timing out on data downloads
- Error: "Failed to download membership data for year 2014"

## Potential Solutions

1. **For CRAN sidebar timeout**: Add to _pkgdown.yml to disable CRAN link:
   ```yaml
   home:
     sidebar:
       structure: [links, license, community, authors, dev]
   ```

2. **For vignette data downloads**:
   - Use pre-computed/cached data in vignettes instead of live fetches
   - Or increase download timeout in package code
   - Or add `eval = FALSE` to chunks when network is unavailable

## Date
2026-01-01
