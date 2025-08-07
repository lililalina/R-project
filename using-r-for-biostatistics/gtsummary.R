library(gtsummary)
head(trial)

# Creating table of summary statistics
trial |> tbl_summary(include = c(trt, age, grade))

#Creating table of summary statistics split by treatment group
trial |>
  tbl_summary(by=trt, include = c(age, grade)) |>
  add_p()

#Modifying tbl_summary() arguments
trial |>
  tbl_summary(
    by = trt,
    include = c(age, grade),
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 1,
    label = age ~ "Patient Age"
  ) |>
  add_p() |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(all_stat_cols() ~ "**Summary Statistics**")

# Adding some functions to the table
trial |>
  tbl_summary(by = trt, includ = c(age, grade)) |>
  add_p(pvalue_fun = label_style_pvalue(digits = 2)) |>
  add_overall() |>
  add_n() |>
  modify_header(label ~ "**Variable**") |>
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Treatment Received**") |>
  modify_footnote_header("Median (IQR) or Frequency (%)", columns = all_stat_cols()) |>
  modify_caption("**Table 1. Patient Characteristics**") |>
  bold_labels()

