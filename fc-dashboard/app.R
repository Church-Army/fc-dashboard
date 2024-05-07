library(shiny)
library(scales)
library(palmerpenguins)
library(carutools)
library(vroom)
library(lubridate)
library(janitor)
library(tidyverse)
library(openssl)


placeholder_plot <-
  ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm,
                       colour = species)) +
  geom_point() +
  theme_ca("black")

#-------------------------------------------------------------------------------

## Helpers --------------------------------------------------------------------
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


theme_set(
  theme_minimal(base_size = 18) +
    theme(text = element_text(family = "Trebuchet MS"))
  )



decrypt_data <- function(path, key_path = "app-secrets/rsa-key.RDS", ...){

  envelope <- readRDS(path)
  key <- readRDS(key_path)

  data <- decrypt_envelope(envelope$data, envelope$iv, envelope$session, key = key)

  data |>
    rawToChar() |>
    I() |>
    vroom(...)
}

## Read in Data ----------------------------------------------------------------

### Raiser's edge data ---------------------------------------------------------

query_1 <-
  decrypt_data("app-inputs/raisers-edge-query_1_encrypted-csv.RDS",
               col_types = "dcficccff") |>
  mutate(gift_date = dmy(gift_date))

query_2 <- decrypt_data("app-inputs/raisers-edge-query_2_encrypted-csv.RDS",
                        col_types = "dcficccff")


donor_id <- str_c("d_", 1:(round(nrow(query_1)/3)))
query_1$donor_id <- sample(donor_id, nrow(query_1), replace = TRUE)



### Mailchimp data -------------------------------------------------------------
mailchimp <-
  vroom("app-inputs/mailchimp.csv") |>
  clean_names() |>
  mutate(month = monthify(email_sent_time),
         week = weekify(email_sent_time),
         weekday = wday(email_sent_time, label = TRUE))

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
#-------------------------------------------------------------------------------
ui <- fluidPage(

tabsetPanel(

    tabPanel("Homepage",

             div(style = c("font-size:30px; text-align:center; font-family:Trebuchet MS"),

               p("In March, we recieved"),

               p(textOutput("received_this_month", inline = TRUE),
                 div(" from", style = "color:black"),
                 style = "color:orange"),

               p(textOutput("donors_this_month", inline = TRUE),
                  style = "color:orange"),

               p(" donors.")),



             plotOutput("giving_overview_plot")
             ),




    tabPanel("Individual donor stats",

             sidebarLayout(
               sidebarPanel(

                 checkboxGroupInput("donation_form", "Form of donation",
                             choices = c("Standing Order", "Direct Debit", "Personal Cheque",
                                         "Voucher", "Cash", "Credit Card", "Business Cheque",
                                         "Other", "PayPal"),
                             selected = c("Standing Order", "Direct Debit", "Personal Cheque",
                                          "Voucher", "Cash", "Credit Card", "Business Cheque",
                                          "Other", "PayPal")),

                 checkboxGroupInput("donation_reservation", "Reserved for:",
                             choices = c("General unrestricted", "Centres of Mission",
                                         "Marylebone", "Amber", "Ruby", "Personal support",
                                         "MYCN"),
                             selected = c("General unrestricted", "Centres of Mission",
                                         "Marylebone", "Amber", "Ruby", "Personal support",
                                         "MYCN")
                             ),
                 dateRangeInput("individual_donation_dates",
                           "Donate between",
                           start = Sys.Date() - 60,
                           end = Sys.Date())
               ),
               mainPanel(

                 plotOutput("forms_times_donation_plot")
                 )
             )),

    tabPanel("Organisation donor stats",

             plotOutput("organisation_donor_plot")
             ),

    tabPanel("Online engagement",
             plotOutput("online_engagement_plot"),
             plotOutput("mailchimp_weekday_plot")
    )))


server <- function(input, output){



max_date <- max(query_1$gift_date)

max_month <- month(max_date)

max_year <- year(max_date)

month_start <- make_date(max_year, max_month)

month_end <- month_start + period(1, "months") - period(1, "days")


  #### Homepage ----------------------------------------------------------------
  output$received_this_month <- renderText({

  month_in_question <- filter(query_1, gift_date >= month_start, gift_date <= month_end)

     label_dollar(prefix = "£")(sum(month_in_question$gift_amount_gbp))

     })

  output$month_label <- renderText(month.name[max_month])




  output$donors_this_month <- renderText({

    month_in_question <- filter(query_1, gift_date >= month_start, gift_date <= month_end)

    unique_donors <- n_distinct(month_in_question$donor_id)

    label_comma()(unique_donors)

    })


  #### Individual donors -------------------------------------------------------

  output$forms_times_donation_plot <- renderPlot({
    processed <-
      query_1 |>
      mutate(new_fund_description =
               case_when(
                 fund_description == "General unrestricted"   ~ "General unrestricted",
                 str_detect(fund_description, "^COM ")        ~ "Centres of Mission",
                 str_detect(fund_description, "^Marylebone ") ~ "Marylebone",
                 str_detect(fund_description, "^Amber ")      ~ "Amber",
                 str_detect(fund_description, "^Ruby ")       ~ "Ruby",
                 str_detect(fund_description, "^Pers Supp ")  ~ "Personal support",
                 str_detect(fund_description, "^MYCN ")       ~ "MYCN"
                 ) |>
               factor()
             ) |>
      filter(
        gift_date > input$individual_donation_dates[1],
        gift_date < input$individual_donation_dates[2],
        gift_payment_type %in% input$donation_form,
        new_fund_description %in% input$donation_reservation
      ) |>

      arrange(gift_date) |>
      mutate(cumulative_gift_gbp = cumsum(gift_amount_gbp))

      ggplot(processed,
             aes(x = gift_date, y = cumulative_gift_gbp)) +
        geom_line(colour = ca_purple(),
                  linewidth = 1.5) +
      labs(x = "Gift Date",
           y = "Cumulative gift amounts",
           title = "Cumulative giving from individual donors") +
        theme(text = element_text(family = "Trebuchet MS"))

  })

  #### Organisation donors -----------------------------------------------------

  output$organisation_donor_plot <- renderPlot({
    placeholder_plot +
      ggtitle("Organisation donations")

  })
  #### Online engagement -------------------------------------------------------

  output$online_engagement_plot <- renderPlot({

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

      ggtitle("Email click rates")

  })



  output$mailchimp_weekday_plot <- renderPlot({


    emails_by_weekday <-

      mutate(mailchimp, n_days = n(), .by = weekday) |>

      summarise(click_rate = mean(click_rate),
                mean_emails = n()/unique(n_days),
                .by = c(email_type, weekday))

    average_click_rate <-
      mutate(mailchimp, email_type = "All emails") |>
      summarise(click_rate = mean(click_rate), .by = c(email_type, weekday))

    rbind(emails_by_weekday) |>

      ggplot(aes(y = click_rate,
                 x = weekday,
                 fill = email_type,
                 colour = email_type
                 )) +

      geom_point(
                 alpha = 0.75, shape = 21, stroke = 1.5,
                 size = 5) +

      ca_scale_fill_discrete(name = "Email type") +
      ca_scale_colour_discrete(name = "Email type") +

      scale_y_continuous(labels = percent_format()) +

      labs(
        x = "Weekday",
        y = "Click rate",
        title = "Average click rate by email type and weekday-sent"
      )

  })
}

# Run the application
shinyApp(ui = ui, server = server)
