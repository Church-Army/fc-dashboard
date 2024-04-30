library(ggplot2)
library(dplyr)
library(forcats)
library(carutools)


#Graph of age groups vs amount of donors

ordered <- mutate(query1,
                  `Gift Payment Type` =
                    ordered(`Gift Payment Type`) |>
                    fct_infreq())


ordered |> ggplot(aes(x = `Gift Payment Type`)) +
           geom_bar(fill = ca_pal_cyan()[2], colour = "black") +
           theme_minimal() +
           labs(x = "Gift payment type",
                y = "Number of payments",
                title = "Popularity of payment methods") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))



# Raw numbers of first time, regular & lapsed donors

    #regular donors give at least quarterly (for the last 2 quarters)
    #lapsed donors haven't given in the last 3 years


first_time <- mutate(query1$`Gift Date`, )
regular <-
lapsed <-

  query1 <- query1 |>
  mutate(
    old_or_recent = ifelse(`Gift Date` < (Sys.Date() - 1095), "Old Donation", "Recent Donation")
  )

donor_id_count()


  query1 <- query1 |>
  mutate(
    type_of_donor = case_when(old_or_recent = "Old Donation" ~ "Lapsed Donor",
                              donor)
  )


#first_time_donors <- function(x, since = Sys.Date() - 30){

  # 1. Group data by gift-date being either side of `since` (i.e 2 groups)
  # 2. Summarise unique donors
  # 3. Ungroup data
  # 4. Count number of donors

#}

