- This module consolidates all setup data and presents sample-parameter
  combinations for measurement data entry.
- Entered data can be downloaded at any time, even if not fully complete or valid. Click "Save Table Data" to make it available for download.
- Enter measured concentrations, detection limits, and associated metadata for each combination. Where levels below detection or quantification (LOD, LOQ) limits are reported, use the MEASURED_FLAG dropdowns and dedicated LOD/LOQ columns.
- Numbers default to displaying 4 d.p., but full precision is retained (e.g. a lot more than 4 d.p.)
- Columns can be sorted by clicking the column name (numeric, chronological, alphabetical order).
- **Due to issues with data entry, I've disabled some visual validation feedback temporarily.**
- **If summarised data are reported (averages, SDs, ranges, etc.), report these using the new UNCERTAINTY_UPPER/_LOWER and MEASURED_N columns. Where MEASURED_N = 1 or otherwise too low for summary statistics, report UNCERTAINTY_TYPE as "Not Relevant".**
