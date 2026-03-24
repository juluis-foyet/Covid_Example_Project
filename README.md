# COVID-19 Example Data
Juluis Visnel Foyet
2026-03-12

- [The dataset](#the-dataset)
- [Packages Importation](#packages-importation)
- [Data Importation](#data-importation)
- [Data Exploration](#data-exploration)
  - [Race and Ethnicity Formating](#race-and-ethnicity-formating)
  - [Missing values](#missing-values)
- [Demographics](#demographics)
  - [Gender](#gender)
  - [Race](#race)
  - [Ethnicity](#ethnicity)
  - [Age](#age)
  - [Demographics by gender](#demographics-by-gender)
- [Symptoms](#symptoms)
  - [Overall](#overall)
  - [By Gender](#by-gender)
- [Covid Outcome](#covid-outcome)
  - [By Gender](#by-gender-1)
- [Weekly trend of Outcomes](#weekly-trend-of-outcomes)
  - [Hospitalization](#hospitalization)
    - [n Cases](#n-cases)
    - [% of Hospitalization from n
      Cases](#-of-hospitalization-from-n-cases)
    - [n Deads](#n-deads)
    - [n Deads from Covid](#n-deads-from-covid)
- [Bamm!!](#bamm)

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
  fmt_number(columns = n_NAs, use_seps = T, decimals = 0) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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
  fmt_number(columns = n, use_seps = T, decimals = 0) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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
  fmt_number(columns = n, use_seps = T, decimals = 0) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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
  fmt_number(columns = n, use_seps = T, decimals = 0) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-8-1.png)

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
  ) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-10-1.png)

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-11-1.png)

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
  ) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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
  ) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-14-1.png)

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
  ) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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
  ) %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-17-1.png)

# Weekly trend of Outcomes

- Given the number of NAs for hospitalization date and the dead date, I
  used the report creation date to study weekly trends of cases,
  hospitalization and dead.

- To capture the weekly trend, instead of using week (1-53), ISO week or
  epidemiological week, I decided to floor report creation date to the
  nearest week. The resulting date version, does not make a lot of sense
  by its own, but it is very convenient for visualization. Below is a
  table illustrating the date flooring.

``` r
tibble(
  Original_Date = seq(dmy("08032026"), dmy("17032026"), "1 day")
) %>% 
  mutate(
    Floored_Date = floor_date(Original_Date, unit = "week")
  ) %>% 
  gt() %>% 
  fmt_date(date_style = "wday_day_month_year") %>% 
  gt::as_rtf()
```

!!!!!RAW-KNITR-CONTENT{

}RAW-KNITR-CONTENT!!!!!

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

![](Covid_Project_Juluis_files/figure-commonmark/unnamed-chunk-23-1.png)
