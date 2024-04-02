library(shiny)
library(scales)
library(palmerpenguins)
library(carutools)
library(ggplot2)

placeholder_plot <-
  ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm,
                       colour = species)) +
  geom_point() +
  theme_ca("black")

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

             plotOutput("online_engagement_plot")
             )
))


server <- function(input, output){

  #### Homepage ----------------------------------------------------------------
  output$received_this_month <- renderText(label_dollar(prefix = "£")(50000))

  output$donors_this_month   <- renderText(label_comma()(100))

  output$giving_overview_plot <- renderPlot({
    ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm,
                         colour = species)) +
      geom_point() +
      theme_ca("black") +
      ggtitle("Donation summary") +
      facet_wrap(~ species, dir = "v")
  })

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
    placeholder_plot +
      ggtitle("Online Engagement")

  })
}

# Run the application
shinyApp(ui = ui, server = server)
