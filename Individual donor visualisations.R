library(ggplot2)
library(dplyr)
library(forcats)
library(carutools)
library(vroom)
library(lubridate)
library(openssl)

decrypt_data <- function(path, key_path = "fc-dashboard/app-secrets/rsa-key.RDS", ...){

  envelope <- readRDS(path)
  key <- readRDS(key_path)

  data <- decrypt_envelope(envelope$data, envelope$iv, envelope$session, key = key)

  data |>
    rawToChar() |>
    I() |>
    vroom(...)
}

raisers_edge_query_1 <-
  decrypt_data("fc-dashboard/app-inputs/raisers-edge-query_1_encrypted-csv.RDS",
               col_types = "dcficccff") |>
  mutate(gift_date = dmy(gift_date))



#Graph of age groups vs amount of donors

ordered <- mutate(raisers_edge_query_1,
                  gift_payment_type =
                    ordered(gift_payment_type) |>
                    fct_infreq())


ordered |> ggplot(aes(x = gift_payment_type)) +
           geom_bar(fill = ca_pal_cyan()[2], colour = "black") +
           theme_minimal() +
           labs(x = "Gift payment type",
                y = "Number of payments",
                title = "Popularity of payment methods") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))



# Raw numbers of first time, regular & lapsed donors


#First Time
first_time <- mutate(query1$`Gift Date`, )

donor_id <- str_c("d_", 1:(round(nrow(query_1)/3)))
query_1$donor_id <- sample(donor_id, nrow(query_1), replace = TRUE)

#Lapsed -  haven't given in the last 3 years
time_ago <- function(n, period, ...){
  Sys.Date() - period(n, period, ...)
}

  query_1 <-
  query_1 |>
    mutate(old_donation = gift_date < time_ago(3, "months"))

query_1 <-
  query_1 |>
  mutate(
    donor_type =
    case_when(
      all(old_donation)         ~ "Lapsed",
      # n() > 1           ~ "Regular",
      n() == 1 && !old_donation ~ "First time",
      .default = "Other"
      ),
    .by = donor_id) |>
  mutate(donor_type = factor(donor_type))

#Regular

query_1 <-
  mutate(query_1,
         standing_order = str_detect(gift_type, "Recurring"),
         recent_gift = gift_date > time_ago(4, "months"),

         recent_recurrent = standing_order & recent_gift) |>
  mutate(regular_donor = any(recent_recurrent), .by = donor_id)




#first_time_donors <- function(x, since = Sys.Date() - 30){

  # 1. Group data by gift-date being either side of `since` (i.e 2 groups)
  # 2. Summarise unique donors
  # 3. Ungroup data
  # 4. Count number of donors

#}

