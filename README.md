# Seasonal Habitats and Migrations of Arctic Grayling of the Lower Colville River Relative to the Nuiqsut Subsistence Fishery Area

This project used radiotelemetry to describe the seasonal habitats and migrations of Arctic grayling Thymallus arcticus in the Lower Colville River drainage relative to the Nuiqsut Fishery Area (NFA), a subsistence fishery, during critical time periods including overwintering, spring spawning, and summer feeding. During July, September, and October 2019, 167 radio tags were deployed in adult Arctic grayling (greater than or equal to 330mm FL) in the Lower Colville River drainage downstream of the Killik River mouth. Periodic aerial surveys were used to locate the radiotagged fish during migrations and seasonal periods between July 2019 and June 2022

## Operational Plan

The Operational Plan associated with this project may be found at

https://www.adfg.alaska.gov/FedAidPDFs/ROP.SF.3F.2019.10.pdf


## Repository Contents

This repository contains the data and analysis scripts associated with two analysis efforts, with a folder dedicated to each.

### /Colville_analysis_ab

#### Colville Grayling Telemetry 12-11-23 _ ab.R

The analysis, conducted by April Behr in December 2023, is used in the current draft of the FDS report.

* River network import & cleanup with `riverdist`
* Network kernel density
* Plots of sequential distance traveled between surveys
* Summary stats of sequential distances (written to .xlsx)
* Minimum observed home range, and comparison between fish tagged inside/outside NSF

#### /data

* Raw river shapefile read by `riverdist`
* telemetry data spreadsheet

#### /analysis output

Plots and tables associated with this effort

### /Colville_analysis_mtSupplemental

#### /data

flight_data.Rdata is an R workspace with objects created by Colville_analysis_ab/Colville Grayling Telemetry 12-11-23 _ ab.R

#### /R

Colville Grayling Telemetry 12-11-23 _ ab_mt.R extends April's analysis.  It has the following sections:
* The first contains April Behr's analysis, which is used in the current FDS draft.
   - MT modification: file paths are changed as necessary for repo file structure
   - MT modification: code writing output to external files (.jpg, .csv) is commented out
* The second contains some supplemental analysis by Matt Tyers (Jan 2024)
   - Implementing a new visualization, linking sequential observations by line segments on map
   - Visualizing survival data, and relationships with individual-level variables
   - Applying a Bayesian survival model to explore such relationships
   - verifying some of the proportions presented in the FDS draft
   
#### ColvilleGrayling_addlAnalysis.Rmd

This is a markdown file that summarizes the additional (MT) analysis and describes the Bayesian model in more detail.  Output is written to a Word file.