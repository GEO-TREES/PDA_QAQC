# GEO-TREES tree inventory quality control toolbox

This repository contains code for quality assurance (QA) and quality control (QC) of GEO-TREES raw tree inventory data (L0) before it is processed in the [GEO-TREES PDA_processing workflow](https://github.com/GEO-TREES/PDA_processing) to generate estimates of above-ground woody biomass density (AGBD).

This toolbox has been designed so it can be used to QA/QC any plot-based tree inventory dataset.

This QA/QC toolbox has incorporated ideas from many other previous efforts, including:

* The SEOSAW R package (`seosawr`) -- https://bitbucket.org/miombo/seosaw/
* The ATFS DataHarmonization Shiny App -- https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization
* The EcoFoG `ForestData` R package -- https://github.com/EcoFoG/ForestData
* The ForestPlots.net `BiomasaFP` R package -- https://github.com/ForestPlots/BiomasaFP
* The ForestGEO `fgeo` R package ecosystem -- https://forestgeo.github.io/fgeo.biomass/

## Workflow 

The functions in the toolbox are designed to act on data following the GEO-TREES tree inventory data format. See the [GEO-TREES Tree Inventory Guidelines, _Section 17: Data format_](https://docs.google.com/document/d/1Q5Wy_LbNEa0tNaGVntL0Bku41Z3t-TeU85_IxZuC6Uc/edit?usp=sharing). 

<!-- For help organising your data according to this format we recommend using the [ATFS-DataHarmonization](https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization) Shiny app. -->

The toolbox contains many functions designed to check a single aspect of data quality in a tree inventory dataset. These functions are bundled into higher-level convenience functions that allow you to run many related checks in a single function call. 

Some functions require multi-census data to identify potential issues, others require only single census data.

Functions in the toolbox are split into two main groups, those that __flag__ potential data quality issues (`flag*()`), and others which attempt to __correct__ these issues (`correct*()`).

Flag functions typically return ID values which uniquely identify the suspect record. The particular set of ID values depends on the scope of the data quality issue, e.g. measurement, stem, tree, census, plot.

Correction functions typically return adjusted values of one or more variables along with the necessary ID values required to uniquely identify the corrected record.

## Functions

Below, is a description of all base-level functions, grouped by theme. 

### Missing records

#### Flags

`flagMissingRecord()` - Flag stems with missing census records.

`flagEmptyRecord()` - Flag stem measurements where particular values are empty and must be filled, with user specification.

* Site ID
* Plot ID
* Stem ID
* Census ID
* Measurement date 

#### Corrections

Create empty stem measurement records where a stem is missing from a census, after it has recruited.

### Measurement date

#### Flags 

Flag stem measurements where the measurement date is an outlier within the census.

Flag stem measurments that are in the future.

### Duplicate records

#### Flags

Flag stem measurements where tree ID + stem ID is duplicated.

### Superfluous records

#### Flags

Flag stem measurements which were probably back-filled and not measured in the field, e.g. pre-recruitment.

### Multi-stemmed trees

#### Flags

Flag trees where the number of stems is greater than a defined threshold.

### Taxonomy 

#### Flags

Flag stem measurements where taxonomic name is missing.

Flag stems where taxonomic name differs among censuses.

Flag trees where stems differ in taxonomic name within a census.

#### Corrections

Where taxonomic name is missing from a stem within a tree where all other stems have the same taxonomic name, assign this name to all missing values.

Where taxonomic name differs among censuses for a stem, assign the most recent taxonomic name (excluding missing values) to all censuses. This assumes that taxonomic identification is refined with each successive census. 

Suggest basic orthographic corrections to taxonomic names. Return a table for user to check.

### Mortality

#### Flags

Flag stem measurements with missing mortality status.

Flag stems where mortality timeline is inconsistent.

Flag stem measurements where mortality column contains more than alive/dead (1/0, TRUE/FALSE).

#### Corrections

Impute missing mortality status records using a stepwise algorithm. 

Interpolate missing records if preceded and followed by records with an identical mortality status. 

Where missing records occur in the first or final census, impute by extrapolation if preceded or proceeded by two or more records with an identical mortality status. 

When neither extrapolation or interpolation can be applied, e.g. for a stem with no mortality status information, or where preceding and proceeding mortality status differs, assume dead if no diameter measurement recorded, and alive if there is a diameter measurement. 

To correct for resurrection, correct all records to alive prior to when the stem was last recorded as alive. 

### POM

#### Flags

Flag stem measurements with missing POM.

Flag stem measurements with a POM greater than a reasonable defined maximum.

#### Corrections

Impute missing POM records using a stepwise algorithm. 

Missing POMs are extrapolated from the last non-missing POM value. 

If missing values occur at the start of the census period, back-fill using the next non-missing value.

When neither extrapolation or back-filling can be applied, e.g. for a stem with no POM records, fill missing POM records with the default POM for the plot.

### Geo-location

#### Flags 

Flag stems where stem geo-location changes among censuses by more than a defined distance threshold.

Flag stem measurements where stem geo-location is outside the plot.

Flag trees where any pairwise distance between stems is greater than a defined threshold.

Flag stem measurements where stem geo-location is outside the subplot (Requires XY coordinates of subplots).

Flag stems where X grid present but Y grid not, or vice versa.

Flag censuses where the subplot geo-location is bigger than the plot.

#### Corrections

For multi-stemmed trees, identify outliers based on stems with greatest pairwise distances, similar to process for diameter timeline outliers.

Move stems where stem geo-location is outside the subplot to the centre of the subplot?

Where stem geo-location differs among censuses for a stem, assign the most recent stem geo-location (excluding missing values) to all censuses. This assumes that stem geo-location is refined with each successive census. 

Where either X or Y is missing, estimate location using subplot.

### Diameter 

#### Flags

Flag stem measurements where diameter less than plot minimum diameter threshold.

Flag stem measurements where diameter greater than a sensible defined maximum.

Flag stem measurements where pairwise comparisons of census diameters show that annual diameter growth rate is larger or smaller than pre-defined limits, see SECO code.

Flag stem measurements where diameter is missing but stem is alive.

Flag censuses where rounded diameter values are unusually common.

Flag censuses where the variance of measurement data suggests a mix of units 


#### Corrections

Linear interpolation (regression) needs at least:

* one diameter measurement available either side of the missing value
* at least two diameter measurements after the missing value, for recruiting census only

Optionally infer growth rate from average growth rates of taxonomic level (species, genus, family [maybe]) where linear interpolation data is not available for that stem.

Discriminate "permanent shifts" and "punctual errors". Punctual errors are offset by a complementary decrease or increase in size at a later census.

Must handle Point of Measurement (POM) changes across censuses, e.g. Cushman et al. (2021), Casey Ryan. Or choose not to correct these values.

### Height 

#### Flags 

Stem measurements where height is an outlier according to a diameter-height model.

Flag stem measurements where height is beyond basic biophysical limits: height-diameter ratio > 4 or < 0.05.

### Recruitment - i.e. census ingrowth

#### Flags 

Flag as "overgrown recruit" if diameter in recruiting census is greater than a defined value greater than the 

#### Corrections

Use linear interpolation to estimate diameter in pre-recruit census. Flag as "overgrown recruit" if diameter estimate in pre-recruit census above minimum diameter threshold. 

If data for linear interpolation is not available, optionally infer growth from average growth 

### Growth form

#### Flags

Flag taxa from known alternative growth-form groups, e.g. liana, bamboo, tree fern, banana, etc. that are not already labelled as such.

#### Corrections

### Plot meta-data cross-checking

#### Flags

Flag plots where they said they didn't measure dead stems, but there are dead stems in the data, and vice versa.

Flag plots where they said they didn't measure lianas, but there are lianas in the data.

Flag plots where they said they didn't measure fallen stems, but there are fallen stems in the data.

### Condition

#### Flags

Flag stems with inconsistent condition timelines such as fallen to standing, following user specification.

Flag stems with inconsistent condition combinations such as fallen and standing, following user specification.

#### Corrections

