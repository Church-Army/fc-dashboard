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

decrypt_data <- function(path, key_path = "app-secrets/rsa-key.RDS"){

  envelope <- readRDS(path)
  key <- readRDS(key_path)

  data <- decrypt_envelope(envelope$data, envelope$iv, envelope$session, key = key)

  data |>
    rawToChar() |>
    I() |>
    vroom()
}

## Read in Data ----------------------------------------------------------------

### Raiser's edge data ---------------------------------------------------------

query_1 <- decrypt_data("app-inputs/raisers-edge-query_1_encrypted-csv.RDS")

query_2 <- decrypt_data("app-inputs/raisers-edge-query_2_encrypted-csv.RDS")

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

## TODO:
## * Create function that applies default theme elements to each plot

# Define UI for application that draws a histogram
ui <- fluidPage(

  tabsetPanel(

    tabPanel("Homepage",

               p("We have recived", textOutput("received_this_month", inline = TRUE),
               " so far this month",
               " from ", textOutput("donors_this_month", inline = TRUE), " donors.",
               style = "font-size: 25px;"),

             plotOutput("giving_overview_plot")
             ),

    tabPanel("Individual donor stats",

             sidebarLayout(
               sidebarPanel(

                 checkboxGroupInput("donation_form", "Form of donation",
                             choices = c("Cash", "Cheque", "BACS")),
                 selectInput("donation_reservation", "Reserved for:",
                             choices = c("All", "Unreserved", "Waterways", "Amber",
                                         "Marylebone")
                             ),
                 dateRangeInput("individual_donation_dates",
                           "Donate between",
                           start = Sys.Date() - 30,
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
             )
))


server <- function(input, output){

  #### Homepage ----------------------------------------------------------------
  output$received_this_month <- renderText(label_dollar(prefix = "£")(sum(query_1$`Gift Amount`)))

  output$donors_this_month   <- renderText(label_comma()(100))


  #### Individual donors -------------------------------------------------------

  output$forms_times_donation_plot <- renderPlot({
    placeholder_plot +
      ggtitle("Individual donations")

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

      theme_minimal() +

      theme(text = element_text(family = "Trebuchet MS")) +

      ggtitle("Online Engagement")

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

      geom_point(aes(size = mean_emails),
                 alpha = 0.75, shape = 21, stroke = 1.5) +

      geom_line(data = average_click_rate, aes(group = 1),
                linetype = "dashed", linewidth = 1, alpha = 0.8) +

      ca_scale_fill_discrete(name = "Email type") +
      ca_scale_colour_discrete(name = "Email type") +
      scale_size_area(name = "Average emails", max_size = 9) +

      scale_y_continuous(labels = percent_format()) +

      theme_minimal() +

      labs(
        x = "Weekday",
        y = "Click rate",
        title = "Average click rate by email type and weekday-sent"
      )

  })
}

# Run the application
shinyApp(ui = ui, server = server)
