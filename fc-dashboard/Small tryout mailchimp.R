library(janitor)
library(purrr)
library(dplyr)
library(vroom)
library(lubridate)
library(stringr)
library(ggplot2)
library(carutools)
library(scales)
library(Cairo)
library(tidyr)
library(forcats)


monthify <- function(x){
  month <- month(x)
  year <- year(x)
  make_date(year, month)
}

weekify <- function(x){
  week <- week(x)
  year <- year(x)

  weeks_to_add <- map_vec(week, \(x) period(x, "weeks"))

  make_date(year) + weeks_to_add
}


mailchimp <-
  vroom("fc-dashboard/app-inputs/mailchimp.csv", show_col_types = FALSE) |>
  clean_names() |>
  mutate(month = monthify(email_sent_time),
         week = weekify(email_sent_time))


mailchimp <-
  mutate(
    mailchimp,

    email_type =
      case_when(
        str_detect(email_name, "CAConnected") ~ "Connected",
        str_detect(email_name, "Prayer Points") ~ "Prayer",
        str_detect(email_name, "Inside Out") ~ "Inside Out",
        str_detect(email_name, "Appeal") ~ "Appeals",
        str_detect(email_name, "Supporter News") ~ "Supporter news",
        .default = "Other") |>
      ordered() |>
      fct_relevel("Other", after = Inf)
    )



summarised <-
  mailchimp |>
  group_by(month, email_type) |>
  summarise(mean_click_rate = mean(click_rate, na.rm = TRUE))

ggplot(summarised, aes(x = month, y = mean_click_rate)) +

  geom_hline(yintercept = 0.021,
             colour = ca_maroon(),
             linewidth = 1.3,
             linetype = "dashed") +

  geom_line(colour = ca_cyan(),
            linewidth = 1.5) +

  facet_wrap(~ email_type) +

  ggtitle("Click Rate Over Time") +

  xlab("Time") +

  scale_y_continuous(name = "Click rate", labels = percent_format()) +

  theme_minimal() +

  theme(text = element_text(family = "Trebuchet MS")) +

  ggtitle("Online Engagement")
