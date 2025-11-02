- Changed all internal data formats to use `tibble()` rather than `list()`). This isn't very exciting from an end user perspective, but enables:
- Added the ability to download and upload your current progress as a zipped set of CSV files at any time.

  - I hope to fix this over the next few days, but I'd appreciate if you could test the current functionality and let me know if you encounter any issues.
  - If you want to open the file and inspect the CSVs, you can do this. You can also edit them in Excel before reuploading. I tend to bounce between the app and Excel based on the sort of data I'm inputting.
- Aquatic Sediment is now properly available as a subcompartment.
- w/w dry weight and wet weight units (both conventional SI and ppm/ppb/etc.) can now be selected as options.
- When adding sites from the map you can now choose what precision to report coordinates at.
- Sample REPLICATE has been changed to SUBSAMPLE and now takes text values. This allows splitting samples into e.g. different sediment core depths
- MEASURED_SD has been replaced with UNCERTAINTY_TYPE, UNCERTAINTY_UPPER, UNCERTAINTY_LOWER and MEASURED_N, which allow more precise reporting of uncertainty.
