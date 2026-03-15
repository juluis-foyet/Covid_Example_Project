# COVID-19 Example Data
Juluis Visnel Foyet
2026-03-12

-   [The dataset](#the-dataset)
-   [Packages Importation](#packages-importation)
-   [Data Importation](#data-importation)
-   [Data Exploration](#data-exploration)
    -   [Race and Ethnicity Formating](#race-and-ethnicity-formating)
    -   [Missing values](#missing-values)
-   [Demographics](#demographics)
    -   [Gender](#gender)
    -   [Race](#race)
    -   [Ethnicity](#ethnicity)
    -   [Age](#age)
    -   [Demographics by gender](#demographics-by-gender)
-   [Symptoms](#symptoms)
    -   [Overall](#overall)
    -   [By Gender](#by-gender)
-   [Covid Outcome](#covid-outcome)
    -   [By Gender](#by-gender-1)
-   [Weekly trend of Outcomes](#weekly-trend-of-outcomes)
    -   [Hospitalization](#hospitalization)
        -   [n Cases](#n-cases)
        -   [% of Hospitalization from n
            Cases](#of-hospitalization-from-n-cases)
        -   [n Deads](#n-deads)
        -   [n Deads from Covid](#n-deads-from-covid)
-   [Bamm!!](#bamm)

# The dataset

The dataset used in this project is an example Covid data of 82,101
observations and 31 variables relating to demographics, symptoms and
outcomes of Covid (Hospitalization and dead). It can be downloaded at
<https://github.com/appliedepi/epiRhandbook_eng/tree/master/data/covid_example_data>

# Packages Importation

``` r
library(readxl) # To import excel datasets
library(tidyverse) # Data wrangling and Viz
library(freqtables)
library(gt)
library(patchwork)
```

# Data Importation

After importing the dataset, I use the `glimpse` function to have a feel
about the size and the variables. R seems to have accurately captured
the type of each variable.

``` r
df <- read_excel('covid_example_data.xlsx')

glimpse(df)
```

    Rows: 82,101
    Columns: 31
    $ PID                    <chr> "3a85e6992a5ac52f", "c6b5281d5fc50b96", "53495a…
    $ reprt_creationdt_FALSE <dttm> 2020-03-22, 2020-02-01, 2020-02-10, 2020-03-20…
    $ case_dob_FALSE         <dttm> 2004-11-08, 1964-06-07, 1944-04-06, 1964-06-25…
    $ case_age               <dbl> 16, 57, 77, 57, 56, 65, 47, 61, 36, 42, 74, 27,…
    $ case_gender            <chr> "Male", "Male", "Female", "Female", "Male", "Ma…
    $ case_race              <chr> "WHITE", "WHITE", "BLACK", "BLACK", "WHITE", "B…
    $ case_eth               <chr> "NON-HISPANIC/LATINO", "NON-HISPANIC/LATINO", "…
    $ case_zip               <dbl> 30308, 30308, 30315, 30213, 30004, 30314, 30313…
    $ Contact_id             <chr> "Yes-Symptomatic", "Yes-Symptomatic", "Yes-Symp…
    $ sym_startdt_FALSE      <dttm> 2020-03-20, 2020-01-28, 2020-02-10, 2021-05-19…
    $ sym_fever              <chr> "Yes", "No", "Yes", "No", "Yes", "Yes", "No", "…
    $ sym_subjfever          <chr> "Yes", "No", NA, "Yes", "Yes", "Yes", "No", "Ye…
    $ sym_myalgia            <chr> "No", "Yes", "Yes", "Yes", "Yes", "No", "Unk", …
    $ sym_losstastesmell     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, "Yes", NA, …
    $ sym_sorethroat         <chr> "Yes", "No", "Yes", "Yes", "No", "Unk", "Yes", …
    $ sym_cough              <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"…
    $ sym_headache           <chr> "Yes", "No", NA, "Yes", "No", "Unk", "Yes", "No…
    $ sym_resolved           <chr> "No, still symptomatic", "No, still symptomatic…
    $ sym_resolveddt_FALSE   <dttm> NA, NA, NA, NA, NA, 2020-02-21, NA, NA, NA, NA…
    $ contact_household      <chr> "Yes", "No", NA, "No", "No", "No", "No", "No", …
    $ hospitalized           <chr> "No", "No", "Yes", NA, "Yes", "Yes", "Yes", "No…
    $ hosp_admidt_FALSE      <dttm> NA, NA, 2020-02-08, NA, 2020-02-26, 2020-01-27…
    $ hosp_dischdt_FALSE     <dttm> NA, NA, NA, NA, NA, 2020-02-21, NA, NA, NA, 20…
    $ died                   <chr> "No", "No", "No", "No", NA, "Yes", "No", NA, "N…
    $ died_covid             <chr> "No", "No", "No", "No", NA, "Yes", "No", NA, "N…
    $ died_dt_FALSE          <dttm> NA, NA, NA, NA, NA, 2020-02-21, NA, NA, NA, NA…
    $ confirmed_case         <chr> "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"…
    $ covid_dx               <chr> "Confirmed", "Confirmed", "Confirmed", "Confirm…
    $ pos_sampledt_FALSE     <dttm> 2020-03-22, 2020-02-01, 2020-02-10, 2021-01-17…
    $ latitude_JITT          <dbl> 33.776645460, 33.780510140, 33.730233310, 33.55…
    $ longitude_JITT         <dbl> -84.385685230, -84.389474740, -84.384251890, -8…

# Data Exploration

## Race and Ethnicity Formating

From glimpse, I noticed that race and ethnicity are in all caps. I have
a preference for cap only at the beginning of each word.

I also choose to focus on confirmed cases.

``` r
df <- df %>% 
  filter(confirmed_case == "Yes") %>% 
  mutate(
    case_race = str_to_title(case_race),
    case_eth = str_to_title(case_eth),
    sym_myalgia = str_to_title(sym_myalgia) # The symptom analysis below revealed that the case is no consistent here.
  )
```

## Missing values

Up to 7 (`died_df_FALSE`, `hosp_dischdt_FALSE`, `hosp_admdt_FALSE`,
`sym_resolveddt_FALSE`, `sym_losstastesmell`, `died_covid`,
`sym_resolved`) variables have missing values for more than 50% of
observations. The good news is that some of this are not missing per se,
but not applicable. For example, if a person did not died,
`died_df_FALSE` does not apply to them.

``` r
df %>% 
  summarise(
    across(everything(), ~sum(is.na(.)))
  ) %>% 
  gather(
    Variables, n_NAs, 1:31
  ) %>% 
  arrange(-n_NAs) %>% 
  mutate(Perc_NAs = round((n_NAs*100)/nrow(df), 2)) %>% 
  gt() %>% 
  fmt_number(columns = n_NAs, use_seps = T, decimals = 0)
```

<div id="rzguplgolk" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#rzguplgolk table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#rzguplgolk thead, #rzguplgolk tbody, #rzguplgolk tfoot, #rzguplgolk tr, #rzguplgolk td, #rzguplgolk th {
  border-style: none;
}

#rzguplgolk p {
  margin: 0;
  padding: 0;
}

#rzguplgolk .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#rzguplgolk .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#rzguplgolk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#rzguplgolk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#rzguplgolk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rzguplgolk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rzguplgolk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#rzguplgolk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#rzguplgolk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#rzguplgolk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#rzguplgolk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#rzguplgolk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#rzguplgolk .gt_spanner_row {
  border-bottom-style: hidden;
}

#rzguplgolk .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#rzguplgolk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#rzguplgolk .gt_from_md > :first-child {
  margin-top: 0;
}

#rzguplgolk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#rzguplgolk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#rzguplgolk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#rzguplgolk .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#rzguplgolk .gt_row_group_first td {
  border-top-width: 2px;
}

#rzguplgolk .gt_row_group_first th {
  border-top-width: 2px;
}

#rzguplgolk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rzguplgolk .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#rzguplgolk .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#rzguplgolk .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rzguplgolk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#rzguplgolk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#rzguplgolk .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#rzguplgolk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#rzguplgolk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#rzguplgolk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rzguplgolk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rzguplgolk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#rzguplgolk .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#rzguplgolk .gt_left {
  text-align: left;
}

#rzguplgolk .gt_center {
  text-align: center;
}

#rzguplgolk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#rzguplgolk .gt_font_normal {
  font-weight: normal;
}

#rzguplgolk .gt_font_bold {
  font-weight: bold;
}

#rzguplgolk .gt_font_italic {
  font-style: italic;
}

#rzguplgolk .gt_super {
  font-size: 65%;
}

#rzguplgolk .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#rzguplgolk .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#rzguplgolk .gt_indent_1 {
  text-indent: 5px;
}

#rzguplgolk .gt_indent_2 {
  text-indent: 10px;
}

#rzguplgolk .gt_indent_3 {
  text-indent: 15px;
}

#rzguplgolk .gt_indent_4 {
  text-indent: 20px;
}

#rzguplgolk .gt_indent_5 {
  text-indent: 25px;
}

#rzguplgolk .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#rzguplgolk div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="Variables"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Variables</th>
<th id="n_NAs" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">n_NAs</th>
<th id="Perc_NAs"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Perc_NAs</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left" headers="Variables">died_dt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">80,357</td>
<td class="gt_row gt_right" headers="Perc_NAs">97.92</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">hosp_dischdt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">78,564</td>
<td class="gt_row gt_right" headers="Perc_NAs">95.74</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">hosp_admidt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">77,079</td>
<td class="gt_row gt_right" headers="Perc_NAs">93.93</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_resolveddt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">65,772</td>
<td class="gt_row gt_right" headers="Perc_NAs">80.15</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_losstastesmell</td>
<td class="gt_row gt_right" headers="n_NAs">50,711</td>
<td class="gt_row gt_right" headers="Perc_NAs">61.80</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">died_covid</td>
<td class="gt_row gt_right" headers="n_NAs">42,285</td>
<td class="gt_row gt_right" headers="Perc_NAs">51.53</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_resolved</td>
<td class="gt_row gt_right" headers="n_NAs">42,279</td>
<td class="gt_row gt_right" headers="Perc_NAs">51.52</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_subjfever</td>
<td class="gt_row gt_right" headers="n_NAs">37,895</td>
<td class="gt_row gt_right" headers="Perc_NAs">46.18</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_startdt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">37,465</td>
<td class="gt_row gt_right" headers="Perc_NAs">45.65</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">died</td>
<td class="gt_row gt_right" headers="n_NAs">36,819</td>
<td class="gt_row gt_right" headers="Perc_NAs">44.87</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">contact_household</td>
<td class="gt_row gt_right" headers="n_NAs">36,725</td>
<td class="gt_row gt_right" headers="Perc_NAs">44.75</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">hospitalized</td>
<td class="gt_row gt_right" headers="n_NAs">32,473</td>
<td class="gt_row gt_right" headers="Perc_NAs">39.57</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_sorethroat</td>
<td class="gt_row gt_right" headers="n_NAs">32,231</td>
<td class="gt_row gt_right" headers="Perc_NAs">39.28</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">Contact_id</td>
<td class="gt_row gt_right" headers="n_NAs">32,194</td>
<td class="gt_row gt_right" headers="Perc_NAs">39.23</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_myalgia</td>
<td class="gt_row gt_right" headers="n_NAs">32,127</td>
<td class="gt_row gt_right" headers="Perc_NAs">39.15</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_headache</td>
<td class="gt_row gt_right" headers="n_NAs">32,008</td>
<td class="gt_row gt_right" headers="Perc_NAs">39.00</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_cough</td>
<td class="gt_row gt_right" headers="n_NAs">31,620</td>
<td class="gt_row gt_right" headers="Perc_NAs">38.53</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">sym_fever</td>
<td class="gt_row gt_right" headers="n_NAs">31,567</td>
<td class="gt_row gt_right" headers="Perc_NAs">38.47</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_race</td>
<td class="gt_row gt_right" headers="n_NAs">2,627</td>
<td class="gt_row gt_right" headers="Perc_NAs">3.20</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_eth</td>
<td class="gt_row gt_right" headers="n_NAs">2,571</td>
<td class="gt_row gt_right" headers="Perc_NAs">3.13</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">longitude_JITT</td>
<td class="gt_row gt_right" headers="n_NAs">164</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.20</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">pos_sampledt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">119</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.15</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_gender</td>
<td class="gt_row gt_right" headers="n_NAs">60</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.07</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">latitude_JITT</td>
<td class="gt_row gt_right" headers="n_NAs">58</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.07</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_dob_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">47</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.06</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_age</td>
<td class="gt_row gt_right" headers="n_NAs">47</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.06</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">case_zip</td>
<td class="gt_row gt_right" headers="n_NAs">13</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.02</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">PID</td>
<td class="gt_row gt_right" headers="n_NAs">0</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.00</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="Variables">reprt_creationdt_FALSE</td>
<td class="gt_row gt_right" headers="n_NAs">0</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.00</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">confirmed_case</td>
<td class="gt_row gt_right" headers="n_NAs">0</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.00</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="Variables">covid_dx</td>
<td class="gt_row gt_right" headers="n_NAs">0</td>
<td class="gt_row gt_right" headers="Perc_NAs">0.00</td>
</tr>
</tbody>
</table>

</div>

# Demographics

This section will include age, gender, race, and ethnicity of cases, and
I choose to stratify by gender.

## Gender

Females are the most represented number of cases in this dataset
(52.78%). A very small proportion of cases (0.42%) are of unknown
gender.

``` r
df %>% 
  select(case_gender) %>% 
  na.omit() %>% 
  freq_table(case_gender) %>% 
  select(cat, n, percent) %>% 
  rename(Gender = cat) %>%
  gt() %>% 
  grand_summary_rows(
    columns = c(n, percent),
    fns = sum ~ sum(.),
    fmt = ~fmt_number(., use_seps = T, decimals = 0)
  ) %>% 
  fmt_number(columns = percent, decimals = 2) %>% 
  fmt_number(columns = n, use_seps = T, decimals = 0)
```

<div id="jquvrxsthp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jquvrxsthp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#jquvrxsthp thead, #jquvrxsthp tbody, #jquvrxsthp tfoot, #jquvrxsthp tr, #jquvrxsthp td, #jquvrxsthp th {
  border-style: none;
}

#jquvrxsthp p {
  margin: 0;
  padding: 0;
}

#jquvrxsthp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jquvrxsthp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#jquvrxsthp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jquvrxsthp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jquvrxsthp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jquvrxsthp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jquvrxsthp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jquvrxsthp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jquvrxsthp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jquvrxsthp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jquvrxsthp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jquvrxsthp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jquvrxsthp .gt_spanner_row {
  border-bottom-style: hidden;
}

#jquvrxsthp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#jquvrxsthp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jquvrxsthp .gt_from_md > :first-child {
  margin-top: 0;
}

#jquvrxsthp .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jquvrxsthp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jquvrxsthp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#jquvrxsthp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#jquvrxsthp .gt_row_group_first td {
  border-top-width: 2px;
}

#jquvrxsthp .gt_row_group_first th {
  border-top-width: 2px;
}

#jquvrxsthp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jquvrxsthp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#jquvrxsthp .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#jquvrxsthp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jquvrxsthp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jquvrxsthp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jquvrxsthp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#jquvrxsthp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jquvrxsthp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jquvrxsthp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jquvrxsthp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jquvrxsthp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jquvrxsthp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#jquvrxsthp .gt_left {
  text-align: left;
}

#jquvrxsthp .gt_center {
  text-align: center;
}

#jquvrxsthp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jquvrxsthp .gt_font_normal {
  font-weight: normal;
}

#jquvrxsthp .gt_font_bold {
  font-weight: bold;
}

#jquvrxsthp .gt_font_italic {
  font-style: italic;
}

#jquvrxsthp .gt_super {
  font-size: 65%;
}

#jquvrxsthp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#jquvrxsthp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#jquvrxsthp .gt_indent_1 {
  text-indent: 5px;
}

#jquvrxsthp .gt_indent_2 {
  text-indent: 10px;
}

#jquvrxsthp .gt_indent_3 {
  text-indent: 15px;
}

#jquvrxsthp .gt_indent_4 {
  text-indent: 20px;
}

#jquvrxsthp .gt_indent_5 {
  text-indent: 25px;
}

#jquvrxsthp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#jquvrxsthp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="a::stub" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="Gender" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Gender</th>
<th id="n" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">n</th>
<th id="percent"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">percent</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td id="stub_1_1" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_1 Gender">Female</td>
<td class="gt_row gt_right" headers="stub_1_1 n">43,280</td>
<td class="gt_row gt_right" headers="stub_1_1 percent">52.78</td>
</tr>
<tr>
<td id="stub_1_2" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_2 Gender">Male</td>
<td class="gt_row gt_right" headers="stub_1_2 n">38,376</td>
<td class="gt_row gt_right" headers="stub_1_2 percent">46.80</td>
</tr>
<tr>
<td id="stub_1_3" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_3 Gender">Unknown</td>
<td class="gt_row gt_right" headers="stub_1_3 n">346</td>
<td class="gt_row gt_right" headers="stub_1_3 percent">0.42</td>
</tr>
<tr>
<td id="grand_summary_stub_1"
class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
data-quarto-table-cell-role="th" scope="row">sum</td>
<td
class="gt_row gt_left gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 Gender">—</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 n">82,002</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 percent">100</td>
</tr>
</tbody>
</table>

</div>

## Race

The black race is the most represented (44.11%), followed by the white
race (39.76%). Unknown races, Asians, American Indians/Alaska Natives,
and Native Hawaiians/Pacific Islanders represent minorities in this
dataset, with less than 5% representativity each.

``` r
df %>% 
  select(case_race) %>% 
  na.omit() %>% 
  freq_table(case_race) %>% 
  select(cat, n, percent) %>% 
  rename(Race = cat) %>% 
  arrange(-percent) %>% 
  gt() %>% 
  grand_summary_rows(
    columns = c(n, percent),
    fns = sum ~ sum(.),
    fmt = ~fmt_number(., use_seps = T, decimals = 0)
  ) %>% 
  fmt_number(columns = percent, decimals = 2) %>% 
  fmt_number(columns = n, use_seps = T, decimals = 0)
```

<div id="pdbwsmznsw" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pdbwsmznsw table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pdbwsmznsw thead, #pdbwsmznsw tbody, #pdbwsmznsw tfoot, #pdbwsmznsw tr, #pdbwsmznsw td, #pdbwsmznsw th {
  border-style: none;
}

#pdbwsmznsw p {
  margin: 0;
  padding: 0;
}

#pdbwsmznsw .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pdbwsmznsw .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pdbwsmznsw .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pdbwsmznsw .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pdbwsmznsw .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pdbwsmznsw .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdbwsmznsw .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pdbwsmznsw .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pdbwsmznsw .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pdbwsmznsw .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pdbwsmznsw .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pdbwsmznsw .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pdbwsmznsw .gt_spanner_row {
  border-bottom-style: hidden;
}

#pdbwsmznsw .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#pdbwsmznsw .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pdbwsmznsw .gt_from_md > :first-child {
  margin-top: 0;
}

#pdbwsmznsw .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pdbwsmznsw .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pdbwsmznsw .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#pdbwsmznsw .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#pdbwsmznsw .gt_row_group_first td {
  border-top-width: 2px;
}

#pdbwsmznsw .gt_row_group_first th {
  border-top-width: 2px;
}

#pdbwsmznsw .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdbwsmznsw .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pdbwsmznsw .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pdbwsmznsw .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdbwsmznsw .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdbwsmznsw .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pdbwsmznsw .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pdbwsmznsw .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pdbwsmznsw .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdbwsmznsw .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pdbwsmznsw .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdbwsmznsw .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pdbwsmznsw .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdbwsmznsw .gt_left {
  text-align: left;
}

#pdbwsmznsw .gt_center {
  text-align: center;
}

#pdbwsmznsw .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pdbwsmznsw .gt_font_normal {
  font-weight: normal;
}

#pdbwsmznsw .gt_font_bold {
  font-weight: bold;
}

#pdbwsmznsw .gt_font_italic {
  font-style: italic;
}

#pdbwsmznsw .gt_super {
  font-size: 65%;
}

#pdbwsmznsw .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pdbwsmznsw .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pdbwsmznsw .gt_indent_1 {
  text-indent: 5px;
}

#pdbwsmznsw .gt_indent_2 {
  text-indent: 10px;
}

#pdbwsmznsw .gt_indent_3 {
  text-indent: 15px;
}

#pdbwsmznsw .gt_indent_4 {
  text-indent: 20px;
}

#pdbwsmznsw .gt_indent_5 {
  text-indent: 25px;
}

#pdbwsmznsw .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#pdbwsmznsw div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="a::stub" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="Race" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Race</th>
<th id="n" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">n</th>
<th id="percent"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">percent</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td id="stub_1_1" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_1 Race">Black</td>
<td class="gt_row gt_right" headers="stub_1_1 n">35,037</td>
<td class="gt_row gt_right" headers="stub_1_1 percent">44.11</td>
</tr>
<tr>
<td id="stub_1_2" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_2 Race">White</td>
<td class="gt_row gt_right" headers="stub_1_2 n">31,584</td>
<td class="gt_row gt_right" headers="stub_1_2 percent">39.76</td>
</tr>
<tr>
<td id="stub_1_3" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_3 Race">Other</td>
<td class="gt_row gt_right" headers="stub_1_3 n">5,860</td>
<td class="gt_row gt_right" headers="stub_1_3 percent">7.38</td>
</tr>
<tr>
<td id="stub_1_4" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_4 Race">Unknown</td>
<td class="gt_row gt_right" headers="stub_1_4 n">3,720</td>
<td class="gt_row gt_right" headers="stub_1_4 percent">4.68</td>
</tr>
<tr>
<td id="stub_1_5" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_5 Race">Asian</td>
<td class="gt_row gt_right" headers="stub_1_5 n">3,071</td>
<td class="gt_row gt_right" headers="stub_1_5 percent">3.87</td>
</tr>
<tr>
<td id="stub_1_6" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_6 Race">American
Indian/Alaska Native</td>
<td class="gt_row gt_right" headers="stub_1_6 n">84</td>
<td class="gt_row gt_right" headers="stub_1_6 percent">0.11</td>
</tr>
<tr>
<td id="stub_1_7" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_7 Race">Native
Hawaiian/Pacific Islander</td>
<td class="gt_row gt_right" headers="stub_1_7 n">79</td>
<td class="gt_row gt_right" headers="stub_1_7 percent">0.10</td>
</tr>
<tr>
<td id="grand_summary_stub_1"
class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
data-quarto-table-cell-role="th" scope="row">sum</td>
<td
class="gt_row gt_left gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 Race">—</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 n">79,435</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 percent">100</td>
</tr>
</tbody>
</table>

</div>

## Ethnicity

Non-Hispanos/Latinos largely dominate (78.81%).

``` r
df %>% 
  select(case_eth) %>% 
  na.omit() %>% 
  freq_table(case_eth) %>% 
  select(cat, n, percent) %>% 
  rename(Ethnicity = cat) %>% 
  arrange(-percent) %>% 
  gt() %>% 
  grand_summary_rows(
    columns = c(n, percent),
    fns = sum ~ sum(.),
    fmt = ~fmt_number(., use_seps = T, decimals = 0)
  ) %>% 
  fmt_number(columns = percent, decimals = 2) %>% 
  fmt_number(columns = n, use_seps = T, decimals = 0)
```

<div id="brdvlmeuli" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#brdvlmeuli table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#brdvlmeuli thead, #brdvlmeuli tbody, #brdvlmeuli tfoot, #brdvlmeuli tr, #brdvlmeuli td, #brdvlmeuli th {
  border-style: none;
}

#brdvlmeuli p {
  margin: 0;
  padding: 0;
}

#brdvlmeuli .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#brdvlmeuli .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#brdvlmeuli .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#brdvlmeuli .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#brdvlmeuli .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#brdvlmeuli .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#brdvlmeuli .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#brdvlmeuli .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#brdvlmeuli .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#brdvlmeuli .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#brdvlmeuli .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#brdvlmeuli .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#brdvlmeuli .gt_spanner_row {
  border-bottom-style: hidden;
}

#brdvlmeuli .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#brdvlmeuli .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#brdvlmeuli .gt_from_md > :first-child {
  margin-top: 0;
}

#brdvlmeuli .gt_from_md > :last-child {
  margin-bottom: 0;
}

#brdvlmeuli .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#brdvlmeuli .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#brdvlmeuli .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#brdvlmeuli .gt_row_group_first td {
  border-top-width: 2px;
}

#brdvlmeuli .gt_row_group_first th {
  border-top-width: 2px;
}

#brdvlmeuli .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#brdvlmeuli .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#brdvlmeuli .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#brdvlmeuli .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#brdvlmeuli .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#brdvlmeuli .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#brdvlmeuli .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#brdvlmeuli .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#brdvlmeuli .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#brdvlmeuli .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#brdvlmeuli .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#brdvlmeuli .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#brdvlmeuli .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#brdvlmeuli .gt_left {
  text-align: left;
}

#brdvlmeuli .gt_center {
  text-align: center;
}

#brdvlmeuli .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#brdvlmeuli .gt_font_normal {
  font-weight: normal;
}

#brdvlmeuli .gt_font_bold {
  font-weight: bold;
}

#brdvlmeuli .gt_font_italic {
  font-style: italic;
}

#brdvlmeuli .gt_super {
  font-size: 65%;
}

#brdvlmeuli .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#brdvlmeuli .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#brdvlmeuli .gt_indent_1 {
  text-indent: 5px;
}

#brdvlmeuli .gt_indent_2 {
  text-indent: 10px;
}

#brdvlmeuli .gt_indent_3 {
  text-indent: 15px;
}

#brdvlmeuli .gt_indent_4 {
  text-indent: 20px;
}

#brdvlmeuli .gt_indent_5 {
  text-indent: 25px;
}

#brdvlmeuli .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#brdvlmeuli div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="a::stub" class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th id="Ethnicity"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Ethnicity</th>
<th id="n" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">n</th>
<th id="percent"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">percent</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td id="stub_1_1" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left"
headers="stub_1_1 Ethnicity">Non-Hispanic/Latino</td>
<td class="gt_row gt_right" headers="stub_1_1 n">62,649</td>
<td class="gt_row gt_right" headers="stub_1_1 percent">78.81</td>
</tr>
<tr>
<td id="stub_1_2" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left"
headers="stub_1_2 Ethnicity">Hispanic/Latino</td>
<td class="gt_row gt_right" headers="stub_1_2 n">8,621</td>
<td class="gt_row gt_right" headers="stub_1_2 percent">10.85</td>
</tr>
<tr>
<td id="stub_1_3" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row"></td>
<td class="gt_row gt_left" headers="stub_1_3 Ethnicity">Not
Specified</td>
<td class="gt_row gt_right" headers="stub_1_3 n">8,221</td>
<td class="gt_row gt_right" headers="stub_1_3 percent">10.34</td>
</tr>
<tr>
<td id="grand_summary_stub_1"
class="gt_row gt_left gt_stub gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
data-quarto-table-cell-role="th" scope="row">sum</td>
<td
class="gt_row gt_left gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 Ethnicity">—</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 n">79,491</td>
<td
class="gt_row gt_right gt_grand_summary_row gt_first_grand_summary_row gt_last_summary_row"
headers="grand_summary_stub_1 percent">100</td>
</tr>
</tbody>
</table>

</div>

## Age

Age spans from 0 to 106 years, with an average of 39.69 and a standard
deviation of 19.15.

``` r
p1 <- ggplot(df %>% filter(case_age>0), aes(x = case_age, y=" "))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(0, 120, 20), expand = c(.01,0))+
  theme_bw()+
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 11, colour = "black")
  )

p2 <- ggplot(df %>% filter(case_age>0), aes(x = case_age))+
  geom_histogram(color = "black", fill = "white")+
  scale_x_continuous(breaks = seq(0, 120, 20), expand = c(.01,0))+
  theme_bw()+
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )

p1+p2+
  plot_layout(ncol = 1, heights = c(.5,1))
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-8-1.png)

## Demographics by gender

``` r
# Compute the frequency distribution of race and ethnicity for per gender.
race_eth_gender <- c("case_race", "case_eth") %>% # a function from purrr that works like a loop.
  map(\(i){
    df %>% 
      select(case_gender, !!rlang::sym(i)) %>% 
      na.omit() %>%
      freq_table(!!rlang::sym(i), case_gender)
  }) %>% 
  list_rbind()

# Format the result from above into a publication ready table.
race_eth_gender %>% 
  freq_format(recipe = "n (percent_row)", name = 'perc', digits = 2) %>% 
  select(row_var, row_cat, col_cat, perc) %>% 
  spread(col_cat, perc, fill = 0) %>% 
  left_join(
    race_eth_gender %>% 
      group_by(row_var, row_cat) %>% 
      summarise(Total = paste0(sum(n), " (100)"), .groups = "drop"),
    by = join_by(row_var, row_cat)
  ) %>% 
  gt(
    groupname_col = "row_var",
    row_group_as_column = T,
    rowname_col = "row_cat"
  ) %>% 
  tab_spanner(
    label = "Case Gender",
    columns = Female:Unknown
  )
```

<div id="meodwfkqfi" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#meodwfkqfi table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#meodwfkqfi thead, #meodwfkqfi tbody, #meodwfkqfi tfoot, #meodwfkqfi tr, #meodwfkqfi td, #meodwfkqfi th {
  border-style: none;
}

#meodwfkqfi p {
  margin: 0;
  padding: 0;
}

#meodwfkqfi .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#meodwfkqfi .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#meodwfkqfi .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#meodwfkqfi .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#meodwfkqfi .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#meodwfkqfi .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#meodwfkqfi .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#meodwfkqfi .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#meodwfkqfi .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#meodwfkqfi .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#meodwfkqfi .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#meodwfkqfi .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#meodwfkqfi .gt_spanner_row {
  border-bottom-style: hidden;
}

#meodwfkqfi .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#meodwfkqfi .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#meodwfkqfi .gt_from_md > :first-child {
  margin-top: 0;
}

#meodwfkqfi .gt_from_md > :last-child {
  margin-bottom: 0;
}

#meodwfkqfi .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#meodwfkqfi .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#meodwfkqfi .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#meodwfkqfi .gt_row_group_first td {
  border-top-width: 2px;
}

#meodwfkqfi .gt_row_group_first th {
  border-top-width: 2px;
}

#meodwfkqfi .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#meodwfkqfi .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#meodwfkqfi .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#meodwfkqfi .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#meodwfkqfi .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#meodwfkqfi .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#meodwfkqfi .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#meodwfkqfi .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#meodwfkqfi .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#meodwfkqfi .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#meodwfkqfi .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#meodwfkqfi .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#meodwfkqfi .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#meodwfkqfi .gt_left {
  text-align: left;
}

#meodwfkqfi .gt_center {
  text-align: center;
}

#meodwfkqfi .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#meodwfkqfi .gt_font_normal {
  font-weight: normal;
}

#meodwfkqfi .gt_font_bold {
  font-weight: bold;
}

#meodwfkqfi .gt_font_italic {
  font-style: italic;
}

#meodwfkqfi .gt_super {
  font-size: 65%;
}

#meodwfkqfi .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#meodwfkqfi .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#meodwfkqfi .gt_indent_1 {
  text-indent: 5px;
}

#meodwfkqfi .gt_indent_2 {
  text-indent: 10px;
}

#meodwfkqfi .gt_indent_3 {
  text-indent: 15px;
}

#meodwfkqfi .gt_indent_4 {
  text-indent: 20px;
}

#meodwfkqfi .gt_indent_5 {
  text-indent: 25px;
}

#meodwfkqfi .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#meodwfkqfi div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" style="width:100%;"
data-quarto-postprocess="true" data-quarto-disable-processing="false"
data-quarto-bootstrap="false">
<colgroup>
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr class="gt_col_headings gt_spanner_row">
<th colspan="2" rowspan="2" id="a::stub"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="colgroup"></th>
<th colspan="3" id="Case Gender"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Case Gender
</div></th>
<th rowspan="2" id="Total"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Total</th>
</tr>
<tr class="gt_col_headings">
<th id="Female" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Female</th>
<th id="Male" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Male</th>
<th id="Unknown"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Unknown</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="case_eth stub_2_1 stub_1">case_eth</td>
<td id="stub_2_1" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Hispanic/Latino</td>
<td class="gt_row gt_right" headers="case_eth stub_2_1 Female">4547
(52.74)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_1 Male">4060
(47.09)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_1 Unknown">14
(0.16)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_1 Total">8621
(100)</td>
</tr>
<tr>
<td id="stub_2_2" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Non-Hispanic/Latino</td>
<td class="gt_row gt_right" headers="case_eth stub_2_2 Female">33627
(53.68)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_2 Male">28894
(46.13)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_2 Unknown">117
(0.19)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_2 Total">62638
(100)</td>
</tr>
<tr>
<td id="stub_2_3" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Not Specified</td>
<td class="gt_row gt_right" headers="case_eth stub_2_3 Female">3879
(47.28)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_3 Male">4171
(50.83)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_3 Unknown">155
(1.89)</td>
<td class="gt_row gt_right" headers="case_eth stub_2_3 Total">8205
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="7" class="gt_row gt_left gt_stub_row_group"
headers="case_race stub_2_4 stub_1">case_race</td>
<td id="stub_2_4" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">American Indian/Alaska
Native</td>
<td class="gt_row gt_right" headers="case_race stub_2_4 Female">46
(54.76)</td>
<td class="gt_row gt_right" headers="case_race stub_2_4 Male">38
(45.24)</td>
<td class="gt_row gt_right" headers="case_race stub_2_4 Unknown">0</td>
<td class="gt_row gt_right" headers="case_race stub_2_4 Total">84
(100)</td>
</tr>
<tr>
<td id="stub_2_5" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Asian</td>
<td class="gt_row gt_right" headers="case_race stub_2_5 Female">1449
(47.18)</td>
<td class="gt_row gt_right" headers="case_race stub_2_5 Male">1617
(52.65)</td>
<td class="gt_row gt_right" headers="case_race stub_2_5 Unknown">5
(0.16)</td>
<td class="gt_row gt_right" headers="case_race stub_2_5 Total">3071
(100)</td>
</tr>
<tr>
<td id="stub_2_6" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Black</td>
<td class="gt_row gt_right" headers="case_race stub_2_6 Female">19916
(56.86)</td>
<td class="gt_row gt_right" headers="case_race stub_2_6 Male">15046
(42.96)</td>
<td class="gt_row gt_right" headers="case_race stub_2_6 Unknown">65
(0.19)</td>
<td class="gt_row gt_right" headers="case_race stub_2_6 Total">35027
(100)</td>
</tr>
<tr>
<td id="stub_2_7" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Native Hawaiian/Pacific
Islander</td>
<td class="gt_row gt_right" headers="case_race stub_2_7 Female">38
(48.10)</td>
<td class="gt_row gt_right" headers="case_race stub_2_7 Male">40
(50.63)</td>
<td class="gt_row gt_right" headers="case_race stub_2_7 Unknown">1
(1.27)</td>
<td class="gt_row gt_right" headers="case_race stub_2_7 Total">79
(100)</td>
</tr>
<tr>
<td id="stub_2_8" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Other</td>
<td class="gt_row gt_right" headers="case_race stub_2_8 Female">3024
(51.61)</td>
<td class="gt_row gt_right" headers="case_race stub_2_8 Male">2797
(47.74)</td>
<td class="gt_row gt_right" headers="case_race stub_2_8 Unknown">38
(0.65)</td>
<td class="gt_row gt_right" headers="case_race stub_2_8 Total">5859
(100)</td>
</tr>
<tr>
<td id="stub_2_9" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">Unknown</td>
<td class="gt_row gt_right" headers="case_race stub_2_9 Female">1733
(46.74)</td>
<td class="gt_row gt_right" headers="case_race stub_2_9 Male">1842
(49.68)</td>
<td class="gt_row gt_right" headers="case_race stub_2_9 Unknown">133
(3.59)</td>
<td class="gt_row gt_right" headers="case_race stub_2_9 Total">3708
(100)</td>
</tr>
<tr>
<td id="stub_2_10" class="gt_row gt_left gt_stub"
data-quarto-table-cell-role="th" scope="row">White</td>
<td class="gt_row gt_right" headers="case_race stub_2_10 Female">15830
(50.13)</td>
<td class="gt_row gt_right" headers="case_race stub_2_10 Male">15691
(49.69)</td>
<td class="gt_row gt_right" headers="case_race stub_2_10 Unknown">55
(0.17)</td>
<td class="gt_row gt_right" headers="case_race stub_2_10 Total">31576
(100)</td>
</tr>
</tbody>
</table>

</div>

``` r
# Viz
ggplot(data = race_eth_gender, aes(x = row_cat, y = percent_row, fill = col_cat))+
  geom_col(position = "dodge")+
  facet_wrap(~row_var, scales = "free_x", space = "free_x")+
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0,60), expand = c(.01,0))+
  labs(y = "Percentage (%)", fill = "Gender")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 15, hjust = 1),
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-10-1.png)

``` r
p1 <- ggplot(df %>% filter(case_age>0 & !is.na(case_gender)), aes(x = case_age, y=" ", fill = case_gender))+
  geom_boxplot()+
  scale_x_continuous(breaks = seq(0, 120, 20), expand = c(.01,0))+
  theme_bw()+
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 11, colour = "black")
  )

p2 <- ggplot(df %>% filter(case_age>0 & !is.na(case_gender)),
             aes(x = case_age, fill = case_gender, fill = case_gender))+
  geom_histogram(color = "black")+
  scale_x_continuous(breaks = seq(0, 120, 20), expand = c(.01,0))+
  theme_bw()+
  theme(
    # axis.title.y = element_blank(),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )

p1+p2+
  plot_layout(ncol = 1, heights = c(.5,1), guides = "collect")&
  theme(legend.position = "top")
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-11-1.png)

# Symptoms

## Overall

This analysis reveals that top 3 Covid symptoms include cough
(prevalence = 43.49%), headache (43.29%) and loss of taste-smell
(40.59%). The least prevalent symptom is sore throat (25.10%).

``` r
names(df %>% 
        select(
          starts_with("sym_"),
          -c(sym_startdt_FALSE, sym_resolved, sym_resolveddt_FALSE)
        )) %>% 
  map(\(i){
    df %>% 
      select(!!rlang::sym(i)) %>% 
      na.omit() %>% 
      freq_table(!!rlang::sym(i))
  }) %>% 
  list_rbind() %>% 
  freq_format(recipe = "n (percent)", name = 'perc', digits = 2) %>% 
  select(var, cat, perc) %>% 
  spread(cat, perc) %>% 
  gt %>% 
  tab_spanner(
    label = "Symptom Occurrence",
    columns = No:Yes
  )
```

<div id="okcymetqdt" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#okcymetqdt table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#okcymetqdt thead, #okcymetqdt tbody, #okcymetqdt tfoot, #okcymetqdt tr, #okcymetqdt td, #okcymetqdt th {
  border-style: none;
}

#okcymetqdt p {
  margin: 0;
  padding: 0;
}

#okcymetqdt .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#okcymetqdt .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#okcymetqdt .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#okcymetqdt .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#okcymetqdt .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#okcymetqdt .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okcymetqdt .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#okcymetqdt .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#okcymetqdt .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#okcymetqdt .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#okcymetqdt .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#okcymetqdt .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#okcymetqdt .gt_spanner_row {
  border-bottom-style: hidden;
}

#okcymetqdt .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#okcymetqdt .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#okcymetqdt .gt_from_md > :first-child {
  margin-top: 0;
}

#okcymetqdt .gt_from_md > :last-child {
  margin-bottom: 0;
}

#okcymetqdt .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#okcymetqdt .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#okcymetqdt .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#okcymetqdt .gt_row_group_first td {
  border-top-width: 2px;
}

#okcymetqdt .gt_row_group_first th {
  border-top-width: 2px;
}

#okcymetqdt .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#okcymetqdt .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#okcymetqdt .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#okcymetqdt .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okcymetqdt .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#okcymetqdt .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#okcymetqdt .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#okcymetqdt .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#okcymetqdt .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#okcymetqdt .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#okcymetqdt .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#okcymetqdt .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#okcymetqdt .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#okcymetqdt .gt_left {
  text-align: left;
}

#okcymetqdt .gt_center {
  text-align: center;
}

#okcymetqdt .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#okcymetqdt .gt_font_normal {
  font-weight: normal;
}

#okcymetqdt .gt_font_bold {
  font-weight: bold;
}

#okcymetqdt .gt_font_italic {
  font-style: italic;
}

#okcymetqdt .gt_super {
  font-size: 65%;
}

#okcymetqdt .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#okcymetqdt .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#okcymetqdt .gt_indent_1 {
  text-indent: 5px;
}

#okcymetqdt .gt_indent_2 {
  text-indent: 10px;
}

#okcymetqdt .gt_indent_3 {
  text-indent: 15px;
}

#okcymetqdt .gt_indent_4 {
  text-indent: 20px;
}

#okcymetqdt .gt_indent_5 {
  text-indent: 25px;
}

#okcymetqdt .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#okcymetqdt div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<colgroup>
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
<col style="width: 25%" />
</colgroup>
<thead>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="var"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">var</th>
<th colspan="3" id="Symptom Occurrence"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Symptom Occurrence
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="No" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">No</th>
<th id="Unk" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Unk</th>
<th id="Yes" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Yes</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left" headers="var">sym_cough</td>
<td class="gt_row gt_right" headers="No">27452 (54.42)</td>
<td class="gt_row gt_right" headers="Unk">1054 (2.09)</td>
<td class="gt_row gt_right" headers="Yes">21936 (43.49)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_fever</td>
<td class="gt_row gt_right" headers="No">33936 (67.21)</td>
<td class="gt_row gt_right" headers="Unk">1446 (2.86)</td>
<td class="gt_row gt_right" headers="Yes">15113 (29.93)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_headache</td>
<td class="gt_row gt_right" headers="No">27175 (54.29)</td>
<td class="gt_row gt_right" headers="Unk">1211 (2.42)</td>
<td class="gt_row gt_right" headers="Yes">21668 (43.29)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_losstastesmell</td>
<td class="gt_row gt_right" headers="No">18092 (57.71)</td>
<td class="gt_row gt_right" headers="Unk">534 (1.70)</td>
<td class="gt_row gt_right" headers="Yes">12725 (40.59)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_myalgia</td>
<td class="gt_row gt_right" headers="No">29197 (58.47)</td>
<td class="gt_row gt_right" headers="Unk">1219 (2.44)</td>
<td class="gt_row gt_right" headers="Yes">19519 (39.09)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_sorethroat</td>
<td class="gt_row gt_right" headers="No">36085 (72.41)</td>
<td class="gt_row gt_right" headers="Unk">1237 (2.48)</td>
<td class="gt_row gt_right" headers="Yes">12509 (25.10)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">sym_subjfever</td>
<td class="gt_row gt_right" headers="No">30446 (68.93)</td>
<td class="gt_row gt_right" headers="Unk">1023 (2.32)</td>
<td class="gt_row gt_right" headers="Yes">12698 (28.75)</td>
</tr>
</tbody>
</table>

</div>

## By Gender

``` r
symp_by_gender <- names(df %>% 
                          select(
                            starts_with("sym_"),
                            -c(sym_startdt_FALSE, sym_resolved, sym_resolveddt_FALSE)
                          )) %>% 
  map(\(i){
    df %>% 
      select(case_gender, !!rlang::sym(i)) %>% 
      na.omit() %>%
      freq_table(case_gender, !!rlang::sym(i))
  }) %>% 
  list_rbind()

# Format the result from above into a publication ready table.
symp_by_gender %>% 
  freq_format(recipe = "n (percent_row)", name = 'perc', digits = 2) %>% 
  select(col_var, row_cat, col_cat, perc) %>% 
  spread(col_cat, perc, fill = 0) %>% 
  left_join(
    symp_by_gender %>% 
      group_by(col_var, row_cat) %>% 
      summarise(Total = paste0(sum(n), " (100)"), .groups = "drop"),
    by = join_by(col_var, row_cat)
  ) %>% 
  rename("Case Gender" = row_cat) %>% 
  gt(
    groupname_col = "col_var",
    row_group_as_column = T
  ) %>% 
  tab_spanner(
    label = "Symptom Occurrence",
    columns = No:Yes
  )
```

<div id="hqfqsqaedy" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#hqfqsqaedy table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#hqfqsqaedy thead, #hqfqsqaedy tbody, #hqfqsqaedy tfoot, #hqfqsqaedy tr, #hqfqsqaedy td, #hqfqsqaedy th {
  border-style: none;
}

#hqfqsqaedy p {
  margin: 0;
  padding: 0;
}

#hqfqsqaedy .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hqfqsqaedy .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#hqfqsqaedy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hqfqsqaedy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hqfqsqaedy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqfqsqaedy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqfqsqaedy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hqfqsqaedy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hqfqsqaedy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hqfqsqaedy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hqfqsqaedy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hqfqsqaedy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hqfqsqaedy .gt_spanner_row {
  border-bottom-style: hidden;
}

#hqfqsqaedy .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#hqfqsqaedy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hqfqsqaedy .gt_from_md > :first-child {
  margin-top: 0;
}

#hqfqsqaedy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hqfqsqaedy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hqfqsqaedy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#hqfqsqaedy .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#hqfqsqaedy .gt_row_group_first td {
  border-top-width: 2px;
}

#hqfqsqaedy .gt_row_group_first th {
  border-top-width: 2px;
}

#hqfqsqaedy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqfqsqaedy .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#hqfqsqaedy .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#hqfqsqaedy .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqfqsqaedy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqfqsqaedy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hqfqsqaedy .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#hqfqsqaedy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hqfqsqaedy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hqfqsqaedy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqfqsqaedy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqfqsqaedy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hqfqsqaedy .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#hqfqsqaedy .gt_left {
  text-align: left;
}

#hqfqsqaedy .gt_center {
  text-align: center;
}

#hqfqsqaedy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hqfqsqaedy .gt_font_normal {
  font-weight: normal;
}

#hqfqsqaedy .gt_font_bold {
  font-weight: bold;
}

#hqfqsqaedy .gt_font_italic {
  font-style: italic;
}

#hqfqsqaedy .gt_super {
  font-size: 65%;
}

#hqfqsqaedy .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#hqfqsqaedy .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#hqfqsqaedy .gt_indent_1 {
  text-indent: 5px;
}

#hqfqsqaedy .gt_indent_2 {
  text-indent: 10px;
}

#hqfqsqaedy .gt_indent_3 {
  text-indent: 15px;
}

#hqfqsqaedy .gt_indent_4 {
  text-indent: 20px;
}

#hqfqsqaedy .gt_indent_5 {
  text-indent: 25px;
}

#hqfqsqaedy .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#hqfqsqaedy div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" style="width:100%;"
data-quarto-postprocess="true" data-quarto-disable-processing="false"
data-quarto-bootstrap="false">
<colgroup>
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
<col style="width: 16%" />
</colgroup>
<thead>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="a::stub"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th rowspan="2" id="Case-Gender"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Case Gender</th>
<th colspan="3" id="Symptom Occurrence"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Symptom Occurrence
</div></th>
<th rowspan="2" id="Total"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Total</th>
</tr>
<tr class="gt_col_headings">
<th id="No" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">No</th>
<th id="Unk" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Unk</th>
<th id="Yes" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Yes</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_cough stub_2_1 stub_1">sym_cough</td>
<td class="gt_row gt_left"
headers="sym_cough stub_2_1 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_1 No">14563
(53.76)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_1 Unk">542
(2.00)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_1 Yes">11984
(44.24)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_1 Total">27089
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_cough stub_2_2 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_2 No">12833
(55.17)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_2 Unk">508
(2.18)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_2 Yes">9921
(42.65)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_2 Total">23262
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_cough stub_2_3 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_3 No">53
(63.86)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_3 Unk">4
(4.82)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_3 Yes">26
(31.33)</td>
<td class="gt_row gt_right" headers="sym_cough stub_2_3 Total">83
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_fever stub_2_4 stub_1">sym_fever</td>
<td class="gt_row gt_left"
headers="sym_fever stub_2_4 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_4 No">18990
(70.11)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_4 Unk">734
(2.71)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_4 Yes">7361
(27.18)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_4 Total">27085
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_fever stub_2_5 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_5 No">14877
(63.80)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_5 Unk">708
(3.04)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_5 Yes">7732
(33.16)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_5 Total">23317
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_fever stub_2_6 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_6 No">65
(76.47)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_6 Unk">4
(4.71)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_6 Yes">16
(18.82)</td>
<td class="gt_row gt_right" headers="sym_fever stub_2_6 Total">85
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_headache stub_2_7 stub_1">sym_headache</td>
<td class="gt_row gt_left"
headers="sym_headache stub_2_7 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_7 No">13649
(50.76)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_7 Unk">610
(2.27)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_7 Yes">12631
(46.97)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_7 Total">26890
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_headache stub_2_8 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_8 No">13469
(58.37)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_8 Unk">597
(2.59)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_8 Yes">9008
(39.04)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_8 Total">23074
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_headache stub_2_9 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_9 No">54
(65.85)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_9 Unk">4
(4.88)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_9 Yes">24
(29.27)</td>
<td class="gt_row gt_right" headers="sym_headache stub_2_9 Total">82
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_losstastesmell stub_2_10 stub_1">sym_losstastesmell</td>
<td class="gt_row gt_left"
headers="sym_losstastesmell stub_2_10 Case Gender">Female</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_10 No">9307 (54.59)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_10 Unk">266 (1.56)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_10 Yes">7476 (43.85)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_10 Total">17049 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_losstastesmell stub_2_11 Case Gender">Male</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_11 No">8750 (61.43)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_11 Unk">266 (1.87)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_11 Yes">5229 (36.71)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_11 Total">14245 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_losstastesmell stub_2_12 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_losstastesmell stub_2_12 No">34
(62.96)</td>
<td class="gt_row gt_right" headers="sym_losstastesmell stub_2_12 Unk">2
(3.70)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_12 Yes">18 (33.33)</td>
<td class="gt_row gt_right"
headers="sym_losstastesmell stub_2_12 Total">54 (100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_myalgia stub_2_13 stub_1">sym_myalgia</td>
<td class="gt_row gt_left"
headers="sym_myalgia stub_2_13 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_13 No">15488
(57.79)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_13 Unk">613
(2.29)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_13 Yes">10698
(39.92)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_13 Total">26799
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_myalgia stub_2_14 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_14 No">13647
(59.22)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_14 Unk">603
(2.62)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_14 Yes">8796
(38.17)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_14 Total">23046
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_myalgia stub_2_15 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_15 No">58
(70.73)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_15 Unk">3
(3.66)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_15 Yes">21
(25.61)</td>
<td class="gt_row gt_right" headers="sym_myalgia stub_2_15 Total">82
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_sorethroat stub_2_16 stub_1">sym_sorethroat</td>
<td class="gt_row gt_left"
headers="sym_sorethroat stub_2_16 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_16 No">18913
(70.65)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_16 Unk">626
(2.34)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_16 Yes">7232
(27.01)</td>
<td class="gt_row gt_right"
headers="sym_sorethroat stub_2_16 Total">26771 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_sorethroat stub_2_17 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_17 No">17103
(74.46)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_17 Unk">607
(2.64)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_17 Yes">5260
(22.90)</td>
<td class="gt_row gt_right"
headers="sym_sorethroat stub_2_17 Total">22970 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_sorethroat stub_2_18 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_18 No">64
(78.05)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_18 Unk">4
(4.88)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_18 Yes">14
(17.07)</td>
<td class="gt_row gt_right" headers="sym_sorethroat stub_2_18 Total">82
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="sym_subjfever stub_2_19 stub_1">sym_subjfever</td>
<td class="gt_row gt_left"
headers="sym_subjfever stub_2_19 Case Gender">Female</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_19 No">16679
(70.38)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_19 Unk">510
(2.15)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_19 Yes">6511
(27.47)</td>
<td class="gt_row gt_right"
headers="sym_subjfever stub_2_19 Total">23700 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_subjfever stub_2_20 Case Gender">Male</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_20 No">13721
(67.25)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_20 Unk">510
(2.50)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_20 Yes">6172
(30.25)</td>
<td class="gt_row gt_right"
headers="sym_subjfever stub_2_20 Total">20403 (100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="sym_subjfever stub_2_21 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_21 No">41
(71.93)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_21 Unk">3
(5.26)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_21 Yes">13
(22.81)</td>
<td class="gt_row gt_right" headers="sym_subjfever stub_2_21 Total">57
(100)</td>
</tr>
</tbody>
</table>

</div>

From the table above and the graph below, the prevalence of headache,
loss of taste/smell and sore throat seems higher in females, while fever
seems more prevalent in males. The prevalences of cough and myalgia
differ by less than 1% between both genders. These results need to be
perceived carefully, since about 1-3% of cases have unknown symptom
status, talkless of about 39-62% of participants having missing values
for symptom status.

``` r
# Viz
ggplot(symp_by_gender, aes(x = row_cat, y = percent_row, fill = col_cat))+
  geom_col(color = "black")+
  geom_text(data = symp_by_gender %>% filter(col_cat == "Yes"),
            aes(label = paste0(round(percent_row,1),"%")),
            position = position_stack(vjust = .5), color = "white")+
  facet_wrap(~col_var, scales = "free_x", nrow = 2)+
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(.01,0))+
  scale_fill_manual(values = c("#00BA38", "grey", "#F8766D"))+
  labs(y = "Percentage (%)", fill = "Symptom Presence")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-14-1.png)

# Covid Outcome

Overall, 10.68% of cases were hospitalized, 3.76% died, and 3.36% died
from covid.

``` r
c("hospitalized", "died", "died_covid") %>% 
  map(\(i){
    df %>% 
      select(!!rlang::sym(i)) %>% 
      na.omit() %>% 
      freq_table(!!rlang::sym(i))
  }) %>% 
  list_rbind() %>% 
  freq_format(recipe = "n (percent)", name = 'perc', digits = 2) %>% 
  select(var, cat, perc) %>% 
  spread(cat, perc, fill = 0) %>% 
  gt() %>% 
  tab_spanner(
    label = "Outcome Occurrence",
    columns = No:Yes
  )
```

<div id="alkifgzgij" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#alkifgzgij table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#alkifgzgij thead, #alkifgzgij tbody, #alkifgzgij tfoot, #alkifgzgij tr, #alkifgzgij td, #alkifgzgij th {
  border-style: none;
}

#alkifgzgij p {
  margin: 0;
  padding: 0;
}

#alkifgzgij .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#alkifgzgij .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#alkifgzgij .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#alkifgzgij .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#alkifgzgij .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#alkifgzgij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#alkifgzgij .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#alkifgzgij .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#alkifgzgij .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#alkifgzgij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#alkifgzgij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#alkifgzgij .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#alkifgzgij .gt_spanner_row {
  border-bottom-style: hidden;
}

#alkifgzgij .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#alkifgzgij .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#alkifgzgij .gt_from_md > :first-child {
  margin-top: 0;
}

#alkifgzgij .gt_from_md > :last-child {
  margin-bottom: 0;
}

#alkifgzgij .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#alkifgzgij .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#alkifgzgij .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#alkifgzgij .gt_row_group_first td {
  border-top-width: 2px;
}

#alkifgzgij .gt_row_group_first th {
  border-top-width: 2px;
}

#alkifgzgij .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#alkifgzgij .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#alkifgzgij .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#alkifgzgij .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#alkifgzgij .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#alkifgzgij .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#alkifgzgij .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#alkifgzgij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#alkifgzgij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#alkifgzgij .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#alkifgzgij .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#alkifgzgij .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#alkifgzgij .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#alkifgzgij .gt_left {
  text-align: left;
}

#alkifgzgij .gt_center {
  text-align: center;
}

#alkifgzgij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#alkifgzgij .gt_font_normal {
  font-weight: normal;
}

#alkifgzgij .gt_font_bold {
  font-weight: bold;
}

#alkifgzgij .gt_font_italic {
  font-style: italic;
}

#alkifgzgij .gt_super {
  font-size: 65%;
}

#alkifgzgij .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#alkifgzgij .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#alkifgzgij .gt_indent_1 {
  text-indent: 5px;
}

#alkifgzgij .gt_indent_2 {
  text-indent: 10px;
}

#alkifgzgij .gt_indent_3 {
  text-indent: 15px;
}

#alkifgzgij .gt_indent_4 {
  text-indent: 20px;
}

#alkifgzgij .gt_indent_5 {
  text-indent: 25px;
}

#alkifgzgij .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#alkifgzgij div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<colgroup>
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="var"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">var</th>
<th colspan="4" id="Outcome Occurrence"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Outcome Occurrence
</div></th>
</tr>
<tr class="gt_col_headings">
<th id="No" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">No</th>
<th id="Under-Review"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Under Review</th>
<th id="Unknown"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Unknown</th>
<th id="Yes" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Yes</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_left" headers="var">died</td>
<td class="gt_row gt_right" headers="No">43383 (95.89)</td>
<td class="gt_row gt_right" headers="Under Review">0</td>
<td class="gt_row gt_right" headers="Unknown">158 (0.35)</td>
<td class="gt_row gt_right" headers="Yes">1702 (3.76)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">died_covid</td>
<td class="gt_row gt_right" headers="No">38318 (96.33)</td>
<td class="gt_row gt_right" headers="Under Review">123 (0.31)</td>
<td class="gt_row gt_right" headers="Unknown">0</td>
<td class="gt_row gt_right" headers="Yes">1336 (3.36)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="var">hospitalized</td>
<td class="gt_row gt_right" headers="No">44295 (89.32)</td>
<td class="gt_row gt_right" headers="Under Review">0</td>
<td class="gt_row gt_right" headers="Unknown">0</td>
<td class="gt_row gt_right" headers="Yes">5294 (10.68)</td>
</tr>
</tbody>
</table>

</div>

### By Gender

``` r
out_by_gender <- c("hospitalized", "died", "died_covid") %>% 
  map(\(i){
    df %>% 
      select(case_gender, !!rlang::sym(i)) %>% 
      na.omit() %>%
      freq_table(case_gender, !!rlang::sym(i))
  }) %>% 
  list_rbind()

# Format the result from above into a publication ready table.
out_by_gender %>% 
  freq_format(recipe = "n (percent_row)", name = 'perc', digits = 2) %>% 
  select(col_var, row_cat, col_cat, perc) %>% 
  spread(col_cat, perc, fill = 0) %>% 
  left_join(
    out_by_gender %>% 
      group_by(col_var, row_cat) %>% 
      summarise(Total = paste0(sum(n), " (100)"), .groups = "drop"),
    by = join_by(col_var, row_cat)
  ) %>% 
  rename("Case Gender" = row_cat) %>% 
  gt(
    groupname_col = "col_var",
    row_group_as_column = T
  ) %>% 
  tab_spanner(
    label = "Outcome Occurrence",
    columns = No:Yes
  )
```

<div id="pxmpcvbvwx" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#pxmpcvbvwx table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#pxmpcvbvwx thead, #pxmpcvbvwx tbody, #pxmpcvbvwx tfoot, #pxmpcvbvwx tr, #pxmpcvbvwx td, #pxmpcvbvwx th {
  border-style: none;
}

#pxmpcvbvwx p {
  margin: 0;
  padding: 0;
}

#pxmpcvbvwx .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#pxmpcvbvwx .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#pxmpcvbvwx .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pxmpcvbvwx .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pxmpcvbvwx .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pxmpcvbvwx .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pxmpcvbvwx .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#pxmpcvbvwx .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#pxmpcvbvwx .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#pxmpcvbvwx .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pxmpcvbvwx .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pxmpcvbvwx .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#pxmpcvbvwx .gt_spanner_row {
  border-bottom-style: hidden;
}

#pxmpcvbvwx .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#pxmpcvbvwx .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#pxmpcvbvwx .gt_from_md > :first-child {
  margin-top: 0;
}

#pxmpcvbvwx .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pxmpcvbvwx .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#pxmpcvbvwx .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#pxmpcvbvwx .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#pxmpcvbvwx .gt_row_group_first td {
  border-top-width: 2px;
}

#pxmpcvbvwx .gt_row_group_first th {
  border-top-width: 2px;
}

#pxmpcvbvwx .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pxmpcvbvwx .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#pxmpcvbvwx .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#pxmpcvbvwx .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pxmpcvbvwx .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pxmpcvbvwx .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pxmpcvbvwx .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#pxmpcvbvwx .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pxmpcvbvwx .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pxmpcvbvwx .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pxmpcvbvwx .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pxmpcvbvwx .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#pxmpcvbvwx .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#pxmpcvbvwx .gt_left {
  text-align: left;
}

#pxmpcvbvwx .gt_center {
  text-align: center;
}

#pxmpcvbvwx .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pxmpcvbvwx .gt_font_normal {
  font-weight: normal;
}

#pxmpcvbvwx .gt_font_bold {
  font-weight: bold;
}

#pxmpcvbvwx .gt_font_italic {
  font-style: italic;
}

#pxmpcvbvwx .gt_super {
  font-size: 65%;
}

#pxmpcvbvwx .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#pxmpcvbvwx .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#pxmpcvbvwx .gt_indent_1 {
  text-indent: 5px;
}

#pxmpcvbvwx .gt_indent_2 {
  text-indent: 10px;
}

#pxmpcvbvwx .gt_indent_3 {
  text-indent: 15px;
}

#pxmpcvbvwx .gt_indent_4 {
  text-indent: 20px;
}

#pxmpcvbvwx .gt_indent_5 {
  text-indent: 25px;
}

#pxmpcvbvwx .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#pxmpcvbvwx div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" style="width:100%;"
data-quarto-postprocess="true" data-quarto-disable-processing="false"
data-quarto-bootstrap="false">
<colgroup>
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
<col style="width: 14%" />
</colgroup>
<thead>
<tr class="gt_col_headings gt_spanner_row">
<th rowspan="2" id="a::stub"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col"></th>
<th rowspan="2" id="Case-Gender"
class="gt_col_heading gt_columns_bottom_border gt_left"
data-quarto-table-cell-role="th" scope="col">Case Gender</th>
<th colspan="4" id="Outcome Occurrence"
class="gt_center gt_columns_top_border gt_column_spanner_outer"
data-quarto-table-cell-role="th" scope="colgroup"><div
class="gt_column_spanner">
Outcome Occurrence
</div></th>
<th rowspan="2" id="Total"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Total</th>
</tr>
<tr class="gt_col_headings">
<th id="No" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">No</th>
<th id="Under-Review"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Under Review</th>
<th id="Unknown"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Unknown</th>
<th id="Yes" class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Yes</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="died stub_2_1 stub_1">died</td>
<td class="gt_row gt_left"
headers="died stub_2_1 Case Gender">Female</td>
<td class="gt_row gt_right" headers="died stub_2_1 No">23263
(96.23)</td>
<td class="gt_row gt_right" headers="died stub_2_1 Under Review">0</td>
<td class="gt_row gt_right" headers="died stub_2_1 Unknown">74
(0.31)</td>
<td class="gt_row gt_right" headers="died stub_2_1 Yes">837 (3.46)</td>
<td class="gt_row gt_right" headers="died stub_2_1 Total">24174
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left" headers="died stub_2_2 Case Gender">Male</td>
<td class="gt_row gt_right" headers="died stub_2_2 No">20062
(95.49)</td>
<td class="gt_row gt_right" headers="died stub_2_2 Under Review">0</td>
<td class="gt_row gt_right" headers="died stub_2_2 Unknown">83
(0.40)</td>
<td class="gt_row gt_right" headers="died stub_2_2 Yes">865 (4.12)</td>
<td class="gt_row gt_right" headers="died stub_2_2 Total">21010
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="died stub_2_3 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="died stub_2_3 No">54 (98.18)</td>
<td class="gt_row gt_right" headers="died stub_2_3 Under Review">0</td>
<td class="gt_row gt_right" headers="died stub_2_3 Unknown">1
(1.82)</td>
<td class="gt_row gt_right" headers="died stub_2_3 Yes">0</td>
<td class="gt_row gt_right" headers="died stub_2_3 Total">55 (100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="died_covid stub_2_4 stub_1">died_covid</td>
<td class="gt_row gt_left"
headers="died_covid stub_2_4 Case Gender">Female</td>
<td class="gt_row gt_right" headers="died_covid stub_2_4 No">20539
(96.69)</td>
<td class="gt_row gt_right"
headers="died_covid stub_2_4 Under Review">71 (0.33)</td>
<td class="gt_row gt_right" headers="died_covid stub_2_4 Unknown">0</td>
<td class="gt_row gt_right" headers="died_covid stub_2_4 Yes">633
(2.98)</td>
<td class="gt_row gt_right" headers="died_covid stub_2_4 Total">21243
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="died_covid stub_2_5 Case Gender">Male</td>
<td class="gt_row gt_right" headers="died_covid stub_2_5 No">17728
(95.92)</td>
<td class="gt_row gt_right"
headers="died_covid stub_2_5 Under Review">52 (0.28)</td>
<td class="gt_row gt_right" headers="died_covid stub_2_5 Unknown">0</td>
<td class="gt_row gt_right" headers="died_covid stub_2_5 Yes">703
(3.80)</td>
<td class="gt_row gt_right" headers="died_covid stub_2_5 Total">18483
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="died_covid stub_2_6 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="died_covid stub_2_6 No">47
(100.00)</td>
<td class="gt_row gt_right"
headers="died_covid stub_2_6 Under Review">0</td>
<td class="gt_row gt_right" headers="died_covid stub_2_6 Unknown">0</td>
<td class="gt_row gt_right" headers="died_covid stub_2_6 Yes">0</td>
<td class="gt_row gt_right" headers="died_covid stub_2_6 Total">47
(100)</td>
</tr>
<tr class="gt_row_group_first">
<td rowspan="3" class="gt_row gt_left gt_stub_row_group"
headers="hospitalized stub_2_7 stub_1">hospitalized</td>
<td class="gt_row gt_left"
headers="hospitalized stub_2_7 Case Gender">Female</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_7 No">23800
(89.78)</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_7 Under Review">0</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_7 Unknown">0</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_7 Yes">2709
(10.22)</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_7 Total">26509
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="hospitalized stub_2_8 Case Gender">Male</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_8 No">20411
(88.78)</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_8 Under Review">0</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_8 Unknown">0</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_8 Yes">2580
(11.22)</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_8 Total">22991
(100)</td>
</tr>
<tr>
<td class="gt_row gt_left"
headers="hospitalized stub_2_9 Case Gender">Unknown</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_9 No">76
(93.83)</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_9 Under Review">0</td>
<td class="gt_row gt_right"
headers="hospitalized stub_2_9 Unknown">0</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_9 Yes">5
(6.17)</td>
<td class="gt_row gt_right" headers="hospitalized stub_2_9 Total">81
(100)</td>
</tr>
</tbody>
</table>

</div>

From the table and the graph, a greater proportion of male cases died
from covid or another cause. A greater proportion of males got
hospitalized too.

``` r
# Viz
ggplot(out_by_gender, aes(x = row_cat, y = percent_row, fill = col_cat))+
  geom_col(color = "black")+
  geom_text(data = out_by_gender %>% filter(col_cat == "Yes"),
            aes(label = paste0(round(percent_row,1),"%")),
            position = position_stack(vjust = .5), color = "white")+
  facet_wrap(~col_var, scales = "free_x", nrow = 1)+
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(.01,0))+
  scale_fill_manual(values = c("#00BA38", "grey", "grey", "#F8766D"))+
  labs(y = "Percentage (%)", fill = "Outcome Occured")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title.x = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-17-1.png)

# Weekly trend of Outcomes

-   Given the number of NAs for hospitalization date and the dead date,
    I used the report creation date to study weekly trends of cases,
    hospitalization and dead.

-   To capture the weekly trend, instead of using week (1-53), ISO week
    or epidemiological week, I decided to floor report creation date to
    the nearest week. The resulting date version, does not make a lot of
    sense by its own, but it is very convenient for visualization. Below
    is a table illustrating the date flooring.

``` r
tibble(
  Original_Date = seq(dmy("08032026"), dmy("17032026"), "1 day")
) %>% 
  mutate(
    Floored_Date = floor_date(Original_Date, unit = "week")
  ) %>% 
  gt() %>% 
  fmt_date(date_style = "wday_day_month_year")
```

<div id="dyzgtcxnlg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#dyzgtcxnlg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}

#dyzgtcxnlg thead, #dyzgtcxnlg tbody, #dyzgtcxnlg tfoot, #dyzgtcxnlg tr, #dyzgtcxnlg td, #dyzgtcxnlg th {
  border-style: none;
}

#dyzgtcxnlg p {
  margin: 0;
  padding: 0;
}

#dyzgtcxnlg .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dyzgtcxnlg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#dyzgtcxnlg .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dyzgtcxnlg .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dyzgtcxnlg .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dyzgtcxnlg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dyzgtcxnlg .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dyzgtcxnlg .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dyzgtcxnlg .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dyzgtcxnlg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dyzgtcxnlg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dyzgtcxnlg .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dyzgtcxnlg .gt_spanner_row {
  border-bottom-style: hidden;
}

#dyzgtcxnlg .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#dyzgtcxnlg .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dyzgtcxnlg .gt_from_md > :first-child {
  margin-top: 0;
}

#dyzgtcxnlg .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dyzgtcxnlg .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dyzgtcxnlg .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#dyzgtcxnlg .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#dyzgtcxnlg .gt_row_group_first td {
  border-top-width: 2px;
}

#dyzgtcxnlg .gt_row_group_first th {
  border-top-width: 2px;
}

#dyzgtcxnlg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dyzgtcxnlg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#dyzgtcxnlg .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#dyzgtcxnlg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dyzgtcxnlg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dyzgtcxnlg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dyzgtcxnlg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}

#dyzgtcxnlg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dyzgtcxnlg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dyzgtcxnlg .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dyzgtcxnlg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dyzgtcxnlg .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dyzgtcxnlg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#dyzgtcxnlg .gt_left {
  text-align: left;
}

#dyzgtcxnlg .gt_center {
  text-align: center;
}

#dyzgtcxnlg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dyzgtcxnlg .gt_font_normal {
  font-weight: normal;
}

#dyzgtcxnlg .gt_font_bold {
  font-weight: bold;
}

#dyzgtcxnlg .gt_font_italic {
  font-style: italic;
}

#dyzgtcxnlg .gt_super {
  font-size: 65%;
}

#dyzgtcxnlg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}

#dyzgtcxnlg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#dyzgtcxnlg .gt_indent_1 {
  text-indent: 5px;
}

#dyzgtcxnlg .gt_indent_2 {
  text-indent: 10px;
}

#dyzgtcxnlg .gt_indent_3 {
  text-indent: 15px;
}

#dyzgtcxnlg .gt_indent_4 {
  text-indent: 20px;
}

#dyzgtcxnlg .gt_indent_5 {
  text-indent: 25px;
}

#dyzgtcxnlg .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}

#dyzgtcxnlg div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>

<table class="gt_table" data-quarto-postprocess="true"
data-quarto-disable-processing="false" data-quarto-bootstrap="false">
<thead>
<tr class="gt_col_headings">
<th id="Original_Date"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Original_Date</th>
<th id="Floored_Date"
class="gt_col_heading gt_columns_bottom_border gt_right"
data-quarto-table-cell-role="th" scope="col">Floored_Date</th>
</tr>
</thead>
<tbody class="gt_table_body">
<tr>
<td class="gt_row gt_right" headers="Original_Date">Sunday 8 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Monday 9 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Tuesday 10 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Wednesday 11 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Thursday 12 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Friday 13 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Saturday 14 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 8 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Sunday 15 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 15 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Monday 16 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 15 March
2026</td>
</tr>
<tr>
<td class="gt_row gt_right" headers="Original_Date">Tuesday 17 March
2026</td>
<td class="gt_row gt_right" headers="Floored_Date">Sunday 15 March
2026</td>
</tr>
</tbody>
</table>

</div>

## Hospitalization

### n Cases

…

``` r
p1 <- df %>% 
  mutate(
    hosp_week = floor_date(reprt_creationdt_FALSE, unit = "week")
  ) %>% 
  select(hosp_week, case_gender) %>% 
  na.omit() %>% 
  group_by(hosp_week, case_gender) %>% 
  count() %>% 
  ggplot(aes(x = hosp_week, y = n, fill = case_gender))+
  geom_col(color = "grey")+
  scale_y_continuous(breaks = seq(0, 3500, 500), expand = c(.01,0))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
               limits = c(ymd("2019-12-01"), ymd("2021-08-27")))+
  scale_fill_manual(values = c("#F8766D", "#00BA38", "#619CFF"))+
  labs(y = "n Cases", x = "Report Creation Date")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

### % of Hospitalization from n Cases

…

``` r
hospi_week <- df %>% 
  mutate(
    hosp_week = floor_date(reprt_creationdt_FALSE, unit = "week")
  ) %>% 
  group_by(hosp_week) %>% 
  mutate(n = n()) %>%
  group_by(hosp_week, n) %>% 
  summarise(n_hospi = sum(hospitalized == "Yes", na.rm = T), .groups = "drop") %>% 
  mutate(perc_hospi = (n_hospi*100)/n)


hospi_week_gender <- df %>% 
  mutate(
    hosp_week = floor_date(reprt_creationdt_FALSE, unit = "week")
  ) %>% 
  group_by(hosp_week, case_gender) %>% 
  mutate(n = n()) %>% 
  group_by(hosp_week, case_gender, n) %>% 
  summarise(n_hospi = sum(hospitalized == "Yes", na.rm = T), .groups = "drop") %>% 
  mutate(perc_hospi = (n_hospi*100)/n) %>% 
  ungroup() %>% na.omit()

p2 <- ggplot(hospi_week, aes(x = hosp_week, y = perc_hospi))+
  geom_line(aes(color = "_Global"))+
  geom_point(aes(color = "_Global"))+
  geom_line(data = hospi_week_gender, 
            aes(x = hosp_week, y = perc_hospi, colour = case_gender))+
  geom_point(data = hospi_week_gender, 
             aes(x = hosp_week, y = perc_hospi, colour = case_gender))+
  scale_y_continuous(breaks = seq(0, 100, 20), expand = c(.01,0))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
               limits = c(ymd("2019-12-01"), ymd("2021-08-27")))+
  scale_color_manual(values = c("black", "#F8766D", "#00BA38", "#619CFF"))+
  labs(y = "% Hospitalized from n Cases", x = "Report Creation Date")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

### n Deads

…

``` r
p3 <- df %>% 
  mutate(
    hosp_week = floor_date(reprt_creationdt_FALSE, unit = "week")
  ) %>% 
  select(hosp_week, case_gender, died) %>% 
  na.omit() %>% 
  group_by(hosp_week, case_gender, died) %>% 
  count() %>% 
  filter(died == "Yes") %>% 
  ggplot(aes(x = hosp_week, y = n, fill = case_gender))+
  geom_col(color = "grey")+
  scale_y_continuous(breaks = seq(0, 70, 10), expand = c(.01,0))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
               limits = c(ymd("2019-12-01"), ymd("2021-08-27")))+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  labs(y = "n Deaths", x = "Report Creation Date")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

### n Deads from Covid

``` r
p4 <- df %>% 
  mutate(
    hosp_week = floor_date(reprt_creationdt_FALSE, unit = "week")
  ) %>% 
  select(hosp_week, case_gender, died_covid) %>% 
  na.omit() %>% 
  group_by(hosp_week, case_gender, died_covid) %>% 
  count() %>% 
  filter(died_covid == "Yes") %>% 
  ggplot(aes(x = hosp_week, y = n, fill = case_gender))+
  geom_col(color = "grey")+
  scale_y_continuous(breaks = seq(0, 70, 10), expand = c(.01,0))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y",
               limits = c(ymd("2019-12-01"), ymd("2021-08-27")))+
  scale_fill_manual(values = c("#F8766D", "#00BA38"))+
  labs(y = "n Deaths from Covid", x = "Report Creation Date")+
  theme_bw()+
  theme(
    legend.position = "top",
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, colour = "black")
  )
```

# Bamm!!

``` r
p1+p2+p3+p4+
  plot_layout(ncol = 1)
```

![](Covid_Project_Juluis.markdown_strict_files/figure-markdown_strict/unnamed-chunk-23-1.png)
