**Feature Extraction Shiny App**

<https://cpm-lab.shinyapps.io/VHAFeatureExtractionExplorer/>

This Shiny App was created to explore the results of feature extractions in VHA data on KDI. For each domain (Conditions, Procedures, Labs, Vitals, Medications), explore summaries of the records that were captured for each feature. For example, the number of records of each ICD10 and ICD9 code for a certain condition, as well as the number and percentage of patients in the cohort with that code.

For Labs and Vitals, users can also see the breakdown by numerical categories (if categories were defined for that feature). The number and percentage of patients that *ever* had a measurement in that range are shown, so the percentages may add up to more than 100%.

Elements (drug names, ICD codes, etc.) with fewer than 100 patients are not shown.

To use the app, click the above link. Alternatively, run via:

```{r}
library(shiny)
shiny::runGitHub("ESRD-DRS", "cpm-lab", subdir = "KDI/ShinyApp")
```
