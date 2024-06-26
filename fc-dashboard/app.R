library(shiny)
library(scales)
library(carutools)
library(vroom)
library(lubridate)
library(janitor)
library(tidyverse)
library(openssl)
library(PostcodesioR)
library(sf)
library(readr)
library(fs)
library(plotly)
library(elementalist)
library(Microsoft365R)
library(AzureAuth)
library(AzureGraph)

#------------------------------------------------------------------------------

## postcode read helpers ------------------------------------------------------

bulk_my_postcodes <- function(postcodes){

  postcodes <-
    split(postcodes, ceiling(seq_along(postcodes)/100)) |>
    unname()

  map(postcodes,
      \(x){
        bulk_postcode_lookup(list(postcodes = x))
      }) |>
    list_c()
}

get_region <- function(x){
  reg <- pluck(x, "result", "region")
  if(is.null(reg)) reg <- pluck(x, "result", "country")
  reg
}

line_breaks <- function(x, n){


}

## ggplot theme defaults -------------------------------------------------------
theme_set(
  theme_minimal(base_size = 18) +
    theme(text = element_text(family = "Trebuchet MS")))

### Parameters for Teams integration ----------------------

app      <- "cf81189c-b1be-492e-929e-6e47c3706346"
tenant   <- "ChurchArmy787"
redirect <- "https://church-army.shinyapps.io/FCtest"
resource <- c("https://graph.microsoft.com/.default", "openid")
secret <- readLines("app-secrets/microsoft-app-secret")

orange <- function(...) span(..., style = "color:#E84619")
#-------------------------------------------------------------------------------

## UI function returns the normal UI if the initial request to the Azure App
## is succesful
ui_function <- function(request){

  opts <- parseQueryString(request$QUERY_STRING)

  if(is.null(opts$code))
  {
    auth_uri <- build_authorization_uri(resource, tenant, app,
                                        redirect_uri = redirect, version = 2)

    redir_js <- str_c('location.replace("', auth_uri, '");')

    tags$script(HTML(redir_js))
  }

  else ui

}

ui <- fluidPage(

tabsetPanel(

    tabPanel("Home page",

             br(),
             br(),
             br(),
             h1("Hello Fundraising and Comms!", style = "text-align:center; font-family:Trebuchet MS; font-size:60px"),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),
             br(),

             fluidRow(
               column(width = 6,
                      div(style = "font-size:30px; text-align:center; font-family:Trebuchet MS",
                            p("In", textOutput("month_label", inline = TRUE), "we received"),
                            p(orange(textOutput("received_this_month", inline = TRUE)),
                              "from"),
                            p(orange(
                              textOutput("donors_this_month", inline = TRUE)),
                            " individual donors."))),

               column(width = 6, plotOutput("income_sources_plot"))),

             fluidRow(verbatimTextOutput("debug"))),

    tabPanel("Individual donor stats",


             sidebarLayout(
               sidebarPanel(
                 fluidRow(column(width = 6, checkboxGroupInput("donation_form", "Form of donation",
                             choices = c("Standing Order", "Direct Debit", "Personal Cheque",
                                         "Voucher", "Cash", "Credit Card", "Business Cheque",
                                         "Other", "PayPal"),
                             selected = c("Standing Order", "Direct Debit", "Personal Cheque",
                                          "Voucher", "Cash", "Credit Card", "Business Cheque",
                                          "Other", "PayPal"))),

                          column(width = 6, checkboxGroupInput("donation_reservation", "Reserved for:",
                             choices = c("General unrestricted", "Centres of Mission",
                                         "Marylebone", "Amber", "Ruby", "Personal support",
                                         "MYCN"),
                             selected = c("General unrestricted", "Centres of Mission",
                                         "Marylebone", "Amber", "Ruby", "Personal support",
                                         "MYCN"))
                             )),
                 fluidRow(dateRangeInput("individual_donation_dates",
                           "Donated between",
                           start = ymd("2024-04-04") - 60,
                           end = ymd("2024-04-04"))),
               width = 3),
               mainPanel(

                 plotOutput("forms_times_donation_plot"),

                 plotOutput("uk_regions_map")
                 )
             )),

    tabPanel("Organisation donor stats",
             fluidRow(column(width = 6, plotOutput("parish_donor_plot")),
                     column(width = 6, plotOutput("trust_donor_plot"))),
             br(),
             fluidRow(plotOutput("trust_and_stat_plot"))),

    tabPanel("Online engagement",
             fluidRow(column(width = 6, plotOutput("online_engagement_plot")),
                      column(width = 6, plotOutput("mailchimp_weekday_plot"))),
             br(),
             fluidRow(plotlyOutput("socials_engagement_plot"))

             )))


server <- function(input, output, session){

  ### Teams authentification dance === === === === === === === === === === ===

  # Get options from query string in the URL
  opts <- parseQueryString(isolate(session$clientData$url_search))

  # If there is no 'code' query parameter, exit the server function
  if(is.null(opts$code)) return()

  ## Authenticate  === === === === === === === === === === === === === === ===
  # These parameters are specified before the UI ^
  token <- get_azure_token(resource, tenant, app,
                           auth_type = "authorization_code",
                           authorize_args = list(redirect_uri = redirect),
                           version = 2,
                           use_cache = FALSE,
                           auth_code = opts$code,
                           password = secret)

  ## New Login === === === === === === === === === === === === === === === ===

  token <-
    ms_graph$
    new(token = token)

  ## Get teams files   === === === === === === === === === === === === === ===
  teams_files <-
    token$
    get_drive(drive_id = "b!4WCLmJ7_-UyvqHz3uE14S-2-qWBOmtZJtta4oBuaQm1dJpBBQY92Q4vJbp10i5Gn")$
    get_item("General/files-for-app")

  ## Read inputs and save locally === === === === === === === === === === ===
  teams_inputs <- dir_create("teams-inputs")

  read_teams <- function(x, ..., files = teams_files){

    files$
      get_item(x)$
      download(path(teams_inputs, x), overwrite = TRUE)

    read_csv(path(teams_inputs, x), ...)
  }

  ### Read query-1 -------------------------------------------------------------
  query_1 <-
    read_teams("raisers-edge-data.CSV", col_types = "ccccccccccc") |>
    clean_names() |>
    mutate(gift_amount = parse_number(gift_amount),
           gift_date = dmy(gift_date)) |>
    mutate(
      week  = round_date(gift_date, "week"),
      month = round_date(gift_date, "month")
    ) |>
    distinct()

  individual <- filter(query_1, constituency_code == "Individual")

  individual <- filter(query_1, constituency_code == "Individual")

  ### Read mailchimp  ----------------------------------------------------------
  mailchimp <- read_teams("mailchimp-data.csv") |>
    clean_names() |>
    mutate(month = round_date(email_sent_time, "month"),
           week  = round_date(email_sent_time, "week"),
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

  ### Read meltwater ----------------------------------------------------------

  meltwater <- read_teams("meltwater-data.csv")

  get_tweet <- function(x){
    str_extract(x, "(?<=\'textSubtitle\'\\: \").+(?=\")") |>
      str_remove_all("[^[:alnum:][:punct:] ]") |>
      str_sub(1,50) |>
      str_c("...") |>
      strwrap(width = 20) |>
      str_c(collapse = "\n")
  }

  meltwater <-
    rowwise(meltwater) |>
    mutate(tweet_body = get_tweet(`social text`)) |>
    ungroup()

max_date <- max(individual$gift_date)

max_date <- max_date - period(1, "months")

max_month <- month(max_date)

max_year <- year(max_date)

month_start <- make_date(max_year, max_month)

month_end <- month_start + period(1, "months") - period(1, "days")

output$month_label <- renderText({month.name[max_month]})


## Home Page--------------------------------------------------------------------

output$income_sources_plot <- renderPlot({

  summarise(query_1, gift_amount = sum(gift_amount), .by = constituency_code) |>

    mutate(gift_amount_relative = gift_amount/sum(gift_amount),
           gift_amount_label = if_else(gift_amount_relative > 0.05, label_dollar(prefix = "£")(gift_amount), ""),
           constituency_code = fct_reorder(constituency_code, gift_amount)) |>
    arrange(-gift_amount) |>

  ggplot(aes(x = factor(1), y = gift_amount, fill = constituency_code)) +

    geom_col() +
    geom_text(aes(label = gift_amount_label),
              position = position_stack(0.5),
              angle = 90,
              size = 6) +



    coord_radial(
      theta = "y",
      expand = FALSE,
      rotate_angle = TRUE
    ) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))
    ) +
  ggtitle("Our sources of income over the last 10 years") +
  ca_scale_fill_discrete(name = "Constituency Code")

})

#### Individual donors -------------------------------------------------------

 output$received_this_month <- renderText({

  month_in_question <- filter(individual, gift_date >= month_start, gift_date <= month_end)

  sum(month_in_question$gift_amount) |>
    round() |>
    label_dollar(prefix = "£")()

     })

  output$month_label <- renderText(month.name[max_month])




  output$donors_this_month <- renderText({

    month_in_question <- filter(individual, gift_date >= month_start, gift_date <= month_end)

    unique_donors <- n_distinct(month_in_question$constituent_id)

    label_comma()(unique_donors)

    })



  output$forms_times_donation_plot <- renderPlot({
    processed <-
      individual |>
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
      mutate(cumulative_gift_gbp = cumsum(gift_amount))

      ggplot(processed,
             aes(x = gift_date, y = cumulative_gift_gbp)) +
        geom_line(colour = ca_purple(),
                  linewidth = 1.5) +
        theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +
      labs(x = "Gift Date",
           y = "Cumulative gift amounts",
           title = "Cumulative giving from individual donors") +
        scale_y_continuous(labels = label_comma()) +
        theme(text = element_text(family = "Trebuchet MS"))

  })


  #### Individual donors map --------------------------------------------------

  output$uk_regions_map <- renderPlot({

    postcodes_on_file <- file_exists("app-inputs/previous-postcodes.csv")

    if(!postcodes_on_file) previous_postcodes <- tibble(postcode = character(), region = character())
    else previous_postcodes <- vroom("app-inputs/previous-postcodes.csv",
                                     col_types = "cc",
                                     altrep = FALSE)

    query_1_filtered <-
      filter(query_1,
             gift_date > input$individual_donation_dates[1],
             gift_date < input$individual_donation_dates[2],
             gift_payment_type %in% input$donation_form
             ) |>
      mutate(clean_postcode =
               str_remove_all(preferred_postcode, " ") |>
               str_squish())

    clean_postcodes <- unique(query_1_filtered$clean_postcode)

    unprocessed <- !clean_postcodes %in% previous_postcodes$postcode

    results <-
      bulk_my_postcodes(clean_postcodes[unprocessed]) |>
      map(get_region)

    not_null <- !map_lgl(results, is.null)

   postcodes <-
     rbind(
       tibble(
         postcode = clean_postcodes[unprocessed][not_null],
         region   = as.character(results[not_null])
         ),
       previous_postcodes
       )

   vroom_write(postcodes, "app-inputs/previous-postcodes.csv", delim = ",")

   query_1_filtered <- left_join(query_1_filtered, postcodes, by = c(clean_postcode = "postcode"))

   region_shapes <- read_sf("app-inputs/gb-regions.geojson")

   region_gifts <-
     group_by(query_1_filtered, region) |>
     count()

   region_shapes <-
     rename(region_shapes, region = nuts118nm) |>
     mutate(region = str_remove(region, " \\(England\\)"))

   plot_data <- left_join(region_shapes, region_gifts, by = "region")

   ggplot(plot_data) +
     geom_sf(aes(fill = n)) +
     scale_fill_gradient(low = "white", high = ca_cyan()) +
     labs(
       x = NULL,
       y = NULL,
       title = "Where are our donors from?",
       caption = "NB this map is does not respond to the 'reserved for' field"
     ) +
     theme(
       axis.text = element_blank(),
       panel.grid = element_blank(),
       plot.caption = element_text(size = 9))

   })


  #### Organisation donors -----------------------------------------------------

  output$parish_donor_plot <- renderPlot({

    just_parish <- query_1 %>%
      filter(constituency_code == "Parish")

    just_parish <- summarise(just_parish, gift_amount = sum(gift_amount), .by = month)

    ggplot(just_parish, aes(x = month,
                            y = gift_amount)) +
      geom_line(colour = ca_green(),
                linewidth = 1.5) +
      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

      labs( x = "Time",
            y = "Gift amount",
            title = "Parish giving over time") +
      scale_y_continuous(labels = label_comma())

  })


  output$trust_donor_plot <- renderPlot({

    just_trust <- query_1 |>
      filter(constituency_code == "Trust")

    just_trust <- summarise(just_trust, gift_amount = sum(gift_amount), .by = month)




    ggplot(just_trust, aes(x = month,
                           y = gift_amount)) +
      geom_line(colour = ca_dark_teal(),
                linewidth = 1.5) +
      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

      labs( x = "Time",
            y = "Gift amount",
            title = "Trust giving over time") +
    scale_y_continuous(labels = label_comma(), limits = c(0, NA))


  })



  output$trust_and_stat_plot <- renderPlot({

    trust_and_stat <- query_1 |>
      filter(constituency_code == "Trust" | constituency_code == "Parish")

    trust_and_stat <- summarise(trust_and_stat, gift_amount = sum(gift_amount), .by = month)

    ggplot(trust_and_stat, aes(x = month,
                        y = gift_amount)) +

      geom_hline(yintercept = 160433,
                 colour = ca_gold(),
                 linewidth = 1.3,
                 linetype = "dashed") +

      geom_line(colour = ca_maroon(),
                linewidth = 1.5) +

      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

      labs( x = "Time",
            y = "Gift amount",
            title = "Amount raised through trusts and stats vs target") +
      scale_y_continuous(labels = label_comma(), limits = c(0, NA))







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

      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

      scale_y_continuous(name = "Click rate", labels = percent_format()) +

      ggtitle("Which of our emails have yield the highest click rates?")

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

      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

      labs(
        x = "Weekday",
        y = "Click rate",
        title = "Does the day of the week an email is sent affect the engagement?",
        subtitle = "Average click rate by email type and weekday-sent"
      )

  })


  output$socials_engagement_plot <- renderPlotly({

    melt_plot <-
      meltwater |>
      mutate(source = str_to_title(source),
             source =
               case_match(
                 source,
                 "Youtube" ~ "YouTube",
                 .default = source
               )) |>
      ggplot(aes(x = `created time`,
                           y = reactions,
                           group = source,
                           colour = source,
                           text = tweet_body)) +

  geom_line(size = 1.5) +

  geom_point(size = 3) +

      theme_minimal() +

      theme(plot.background  = element_rect_round(fill = "gray96", colour = "gray96", radius = unit(10, "pt"))) +

  labs(x = "Time of post",
       y = "Engagement",
       title = "Engagement in social media posts") +

  ca_scale_colour_discrete(name = "Media")

    ggplotly(melt_plot, tooltip = "text")
    })

   }

# Run the application
shinyApp(ui = ui_function, server = server)
