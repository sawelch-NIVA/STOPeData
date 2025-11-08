##### Convenience
- Download extracted/entered data as a .zip of CSVs, then reupload via the landing page to continue later.
- The app should now crash less often, and return at least vaguely informative error notifications when something does go wrong.
- Increased LLM token size limit. We can extract more data, but the cost per paper has doubled to $0.20.

##### Sites
- When adding sites from the map you can now choose what precision to report coordinates at.

##### Compartments
- Added *Aquatic Sediment*, *Sludge*, and *Porewater* as subcompartments of *Aquatic*

##### Samples
- Sample REPLICATE has been changed to SUBSAMPLE and now takes text values. This allows splitting samples into e.g. different sediment core depths, different combinations of tissues and organisms, etc.

##### Measurements
- w/w dry weight and wet weight units (both conventional SI and ppm/ppb/etc.) can now be selected as options.
- MEASURED_SD has been replaced with UNCERTAINTY_TYPE, UNCERTAINTY_UPPER, UNCERTAINTY_LOWER and MEASURED_N, which allow more precise reporting of uncertainty.
- EXPERIMENTAL: As part of the extraction process the LLM now scores the paper and its job extracting data from it. I don't know how effective this is, but I'd be interested to hear all your experiences with it.