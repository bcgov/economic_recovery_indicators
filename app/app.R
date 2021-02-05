## load libraries ----
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rsconnect)
library(plotly)
library(lubridate)
library(janitor)
library(cansim)
library(DT)
library(scales)
library(purrr)

## chart theme/functions ----
source("scripts/chart_theme.R")
source("scripts/functions.R")

## load data ----
non_cansim_data <- readRDS("data/non_cansim_data.rds")
non_cansim_stats <- non_cansim_data %>%
  get_mom_stats() %>%
  get_yoy_stats() %>%
  get_ytd_stats() %>%
  arrange(title)
  
exports_data <- readRDS("data/exports_data.rds")
exports_stats <- exports_data %>%
  get_yoy_stats() %>%
  get_ytd_stats() %>%
  arrange(title)

## get cansim data ----
cansim_v <- c("v121514675", "v807928", "v52367245", "v42170715", "v32214475", "v2057793", "v2064706", 
              "v1593420", "v64540928", "v2064705", "v41692462", "v121515395", "v121518515",
              "v121524275", "v2064719", "v2064710", "v2064728", "v2064715", "v2064724", 
              "v64542176", "v64543328", "v64541136", "v2064732", "v41692463", "v41692495")
cansim_t <- c("<b>Non-Residential Building Permits</b><br>($Thousands, SA)",
              "<b>Manufacturing Sales</b><br>($Thousands, SA)",
              "<b>Retail Trade</b><br>($Thousands, SA)",
              "<b>Food Services and Drinking Places</b><br>($Thousands, SA)",
              "<b>Visitor Entries</b><br>(Persons, SA)",
              "<b>Employment</b><br>(Thousands, SA)", 
              "<b>Participation Rate</b><br>(%, SA)",
              "<b>Average Hourly Wage Earnings</b><br>($, NSA)",
              "<b>Employment Insurance Beneficiaries (1)</b><br>(Persons, NSA)",
              "<b>Unemployment Rate</b><br>(%, SA)",
              "<b>Consumer Price Index</b><br>(All Items, NSA)",
              "<b>Non-Residential Building Permits Industrial</b>",
              "<b>Non-Residential Building Permits Commercial</b>",
              "<b>Non-Residential Building Permits Institutional and Governmental</b>",
              "<b>Employment Females</b>",
              "<b>Employment Males</b>",
              "<b>B.C. Employment Youth, 15-24 years of age</b><br>(Thousands, SA)",
              "<b>Participation Rate Males</b>",
              "<b>Participation Rate Females</b>",
              "<b>Employment Insurance Beneficiaries Males</b>",
              "<b>Employment Insurance Beneficiaries Females</b>",
              "<b>Employment Insurance Beneficiaries Youth (15-24 yo)</b>",
              "<b>Unemployment Rate Youth (15-24 yo)</b>",
              "<b>Consumer Price Index</b><br>(Food, NSA)",
              "<b>Consumer Price Index</b><br>(Shelter, NSA)")
titles <- data.frame(
  vector = cansim_v,
  title = factor(x = cansim_t, levels = cansim_t),
  label = c("Non-Residential Building Permits", "Manufacturing Sales", "Retail Trade", 
            "Food Services and Drinking Places", "Visitor Entries", "Employment", "Participation Rate", 
            "Average Hourly Wage Earnings", "Employment Insurance Beneficiaries", "Unemployment Rate",
            "Consumer Price Index", "Non-Residential Building Permits Industrial",
            "Non-Residential Building Permits Commercial", 
            "Non-Residential Building Permits Institutional and Governmental",
            "Employment Females", "Employment Males", "B.C. Employment Youth, 15-24 years of age", 
            "Participation Rate Males", "Participation Rate Females", 
            "Employment Insurance Beneficiaries Males", "Employment Insurance Beneficiaries Females",
            "Employment Insurance Beneficiaries Youth (15-24 yo)", "Unemployment Rate Youth (15-24 yo)",
            "Consumer Price Index", "Consumer Price Index"),
  filter_var = c("overall", "businesses", "businesses", "businesses", "businesses", "bcians", 
                 "bcians", "bcians", "bcians", "bcians", "bcians", rep("chart", 14))
)

cansim_data <- cansim::get_cansim_vector(
  vectors = cansim_v,
  start_time = "2010-01-01") %>%
  mutate(REF_DATE = ymd(REF_DATE, truncated = 2)) %>%
  janitor::clean_names() %>%
  left_join(titles, by = c("vector")) %>%
  select(title, label, filter_var, ref_date, value)

cansim_stats <- cansim_data %>%
  get_mom_stats() %>%
  get_yoy_stats() %>%
  get_ytd_stats() %>%
  arrange(title)

## all_data ----
temp <- non_cansim_data$title %>% unique() %>% as.character()
levels_all <- c(## Overall Economy
                temp[1],            #"<b>International Merchandise Exports</b><br>($Thousands, SA)", 
                cansim_t[1],        #"<b>Non-Residential Building Permits</b><br>($Thousands, SA)
                temp[2],            #"<b>US Housing Starts</b><br>(Thousands, SAAR)"
                ## Businesses
                cansim_t[2:3], temp[3], cansim_t[4:5], temp[4],
                ## British Columbians
                cansim_t[6:25], temp[5:7])

all_data <- bind_rows(cansim_data, non_cansim_data) %>%
  mutate(title = factor(title, levels = levels_all)) %>%
  get_mom_stats() %>%
  get_yoy_stats() %>%
  get_ytd_stats() %>%
  arrange(title)

## start of app ----
# UI demonstrating column layouts
ui <- function(req) {
  fluidPage(useShinydashboard(),
            title = "BC Economic Indicators",
  theme = "bootstrap.css",
  HTML("<html lang='en'>"),
  fluidRow(
    column(width = 12,
           style = "background-color:#003366; border-bottom:2px solid #fcba19;position:fixed;z-index:10000",
           tags$header(class="header", style="padding:0 0px 0 0px; display:flex; height:80px;
           width:100%;",
             tags$div(class="banner", style="display:flex; justify-content:flex-start; align-items:center; margin: 0 10px 0 10px",
               a(href="https://www2.gov.bc.ca/gov/content/data/about-data-management/bc-stats",
                 img(src = "bcstats_logo_rev.png", title = "BC Stats", height = "80px", alt = "British Columbia - BC Stats"),
                 onclick="gtag"
               ),
               h1("British Columbia - Economic Recovery Indicators", style="font-weight:400; color:white; margin: 5px 5px 0 18px;")
             )
           )
    ),
    # column(width=12,
    #        style = "margin-top:100px",
    #         tags$fieldset(
    #               tags$legend(h2("Some heading here")),
    #               p("Some text in a paragraph here.",
    #               style="font-size:14px; color:#494949"),
    #               br()
    #         )
    # ),
    ## Make changes to this column
    column(width = 12,
           style = "margin-top:100px",
             tabsetPanel(id = "tabs",
               tabPanel("Summary",
                        value = 1,
                        tags$fieldset(
                          tags$legend(h3("Summary Type")),
                          selectInput(
                            inputId = "summary_type",
                            label = NULL,
                            choices = c("Economic Recovery Indicators", "Exports by Destination and Commodity", "Exports by Commodity"),
                            selected = "Economic Recovery Indicators")
                        ),
                        conditionalPanel(condition = "input.summary_type == 'Economic Recovery Indicators'",
                                         #style="background-color:#F2F2F2",
                                         br(),
                                         tags$div(
                                           style="margin-left:15px;margin-bottom:20px",
                                           h3("B.C. Summary - Key Economic Recovery Indicators")),
                                         box(title = "OVERALL ECONOMY", status = "primary",
                                             solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                                             DT::dataTableOutput("ERI_overall")),
                                         box(title = "BUSINESSES", status = "primary",
                                             solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                             DT::dataTableOutput("ERI_businesses")),
                                         box(title = "BRITISH COLUMBIANS", status = "primary",
                                             solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                                             DT::dataTableOutput("ERI_bcians")),
                                         tags$div(
                                           style="margin-left:15px",
                                             '(1) This dataset does not include persons who received the Canada Emergency Response Benefit (CERB). 
                                             Between March and September 2020, CERB was introduced and the number of EI recipients dropped
                                             significantly as persons could not receive both. The CERB program ended on September 27, 2020
                                             and eligibility rules for EI were changed, resulting in a dramatic increase in the
                                             number of EI beneficiaries in October 2020.', 
                                             br(),
                                             'Note: Due to data collection issues and resulting quality concerns, information on 
                                             TransLink weekly ridership is no longer being published. If you’d like more information,
                                             please contact BCStats: BC.Stats@gov.bc.ca'),
                                         br()
                                         ),
                        conditionalPanel(condition = "input.summary_type == 'Exports by Destination and Commodity'",
                                         #style="background-color:#F2F2F2",
                                         br(),
                                         tags$div(
                                           style="margin-left:15px;margin-bottom:20px",
                                           h3("B.C. Summary - Key Exports by Destination and Commodity")),
                                         box(title = "EXPORTS BY DESTINATION AND COMMODITY ($Thousands, NSA)", 
                                             status = "primary",
                                             solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                                             DT::dataTableOutput("Exp_dest")),
                                         tags$div(
                                           style="margin-left:15px",
                                           'Note: Trends for latest reporting month compared to previous month are not available since data 
                                           are not seasonally adjusted.'),
                                         br(),
                                         tags$div(
                                           style="margin-left:15px",
                                           "For more information on other commodities or countries visit: ", 
                                           tags$a('https://www2.gov.bc.ca/gov/content/data/statistics/business-industry-trade/trade/trade-data')),
                                         br()
                        ),
                        conditionalPanel(condition = "input.summary_type == 'Exports by Commodity'",
                                        # style="background-color:#F2F2F2",
                                         br(),
                                         tags$div(
                                           style="margin-left:15px;margin-bottom:20px",
                                           h3("B.C. Summary - Key Exports by Commodity")),
                                         box(title = "EXPORTS BY COMMODITY ($Thousands, NSA)", status = "primary",
                                             solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                                             DT::dataTableOutput("Exp_overall")),
                                         tags$div(
                                           style="margin-left:15px",
                                           'Note: Trends for latest reporting month compared to previous month are not available since data 
                                           are not seasonally adjusted.'),
                                           br(),
                                         tags$div(
                                           style="margin-left:15px",
                                           "For more information on other commodities or countries visit: ", 
                                           tags$a('https://www2.gov.bc.ca/gov/content/data/statistics/business-industry-trade/trade/trade-data')),
                                         br()
                        )
                       ),
               tabPanel("Detailed Summary",
                        value = 2,
                       # style="background-color:#F2F2F2",
                        br(),
                        tags$div(
                          style="margin-left:15px;margin-bottom:20px",
                          h3("B.C. Detailed Summary - Key Economic Recovery Indicators")),
                        box(title = "OVERALL ECONOMY", status = "primary",
                            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
                            DT::dataTableOutput("DET_overall")
                           ),
                        box(title = "BUSINESSES", status = "primary",
                            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                            DT::dataTableOutput("DET_businesses")
                          ),
                        box(title = "BRITISH COLUMBIANS", status = "primary",
                            solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = TRUE,
                            DT::dataTableOutput("DET_bcians")
                           ),
                        tags$div(
                          style="margin-left:15px",
                          '(1) This dataset does not include persons who received the Canada Emergency Response Benefit (CERB). 
                                             Between March and September 2020, CERB was introduced and the number of EI recipients dropped
                                             significantly as persons could not receive both. The CERB program ended on September 27, 2020
                                             and eligibility rules for EI were changed, resulting in a dramatic increase in the
                                             number of EI beneficiaries in October 2020.', 
                          br(),
                          'Note: Due to data collection issues and resulting quality concerns, information on 
                                             TransLink weekly ridership is no longer being published. If you’d like more information,
                                             please contact BCStats: BC.Stats@gov.bc.ca'),
                        br()
                        ),
               tabPanel("Charts",
                        value = 3,
                        #style="background-color:#F2F2F2",
                        br(),
                        tags$fieldset(
                          tags$legend(h3("Indicator")),
                          selectInput(
                            inputId = "indicator",
                            label = NULL,
                            choices = titles$label,
                            selected = titles$label[1])
                        ),
                        tags$fieldset(
                        plotlyOutput(outputId = "charts"),
                        uiOutput(outputId = "caption")
                        ),
                        br(),
                        # Data table of chart to go here   
                        br()
               ),
               type = "tabs"
             )
    ), ## End of column to make changes to
    column(width = 12,
           style = "background-color:#003366; border-top:2px solid #fcba19;",

            tags$footer(class="footer",
              tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                    tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                  tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                )
              )
             )
    )
  )
)}

## Define server logic ----
server <- function(input, output, session) {
  
  ## Tab 1: Key Economic Recovery Indicators ----
  
  output$ERI_overall <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "overall") %>%
      format_summary_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  output$ERI_businesses <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "businesses") %>%
      format_summary_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  output$ERI_bcians <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "bcians") %>%
      format_summary_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
  })
  
  ## Tab 1: Exports by Country ----
  
  output$Exp_dest <- DT::renderDataTable({
    
    data <- exports_stats %>%
      filter(destination != "World") %>%
      format_summary_data() %>%
      ## make sure indentation spaces are read by DT
      mutate(INDICATOR = str_replace_all(INDICATOR, "     ", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")) %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               "pageLength" = 33,  ## show all rows
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  ## Tab 1: Exports ----
  
  output$Exp_overall <- DT::renderDataTable({
    
    data <- exports_stats %>%
      filter(destination == "World") %>%
      format_summary_data() %>%
      mutate(INDICATOR = str_replace(INDICATOR, pattern = "World" , replacement = "ALL")) %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  ## Tab 2: Detailed Summary ----
  
  output$DET_overall <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "overall") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
    
  })
  
  output$DET_businesses <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "businesses") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
    
  })
  
  output$DET_bcians <- DT::renderDataTable({
    
    data <- all_data %>%  #cansim_stats %>%
      filter(filter_var == "bcians") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
  })
  
  ## Tab 3: Charts ----
  
  get_data <- reactive({
    
    req(input$indicator)
    
    line_chart <- cansim_data %>% 
        filter(label == input$indicator)
    
    list(line_chart = line_chart)
  })
  
  output$charts <- renderPlotly({

    if(is.null(get_data()$line_chart)){
      NULL
    }

    else{

      p <- ggplot(get_data()$line_chart,
                  aes(x = ref_date, y = value)) +
        geom_line() +
        bcstats_chart_theme +
        labs(x = NULL,
             y = NULL, 
             title = get_data()$line_chart$title %>% head(1) %>% as.character()) +
        scale_x_date(limits = c(min(get_data()$line_chart$ref_date),
                                max(get_data()$line_chart$ref_date) + months(3)),
                     expand = c(0,0),
                     date_breaks = "6 months",
                     date_labels = "%b\n%Y" )

      p <- ggplotly(p)

    }
  })

  
  output$caption <- renderUI({
    
    ## This will have to be more dynamic when we get other data sources
    ## Possibly add "Source" as a column to titles dataframe at top of app
    if(str_detect(get_data()$line_chart$title, "(1)")) {
      HTML("Source: Statistics Canada <br>
           (1) This dataset does not include persons who received the Canada Emergency Response 
           Benefit (CERB). Between March and September 2020, CERB was introduced and the number of 
           EI recipients dropped significantly as persons could not receive both. The CERB program 
           ended on September 27, 2020 and eligibility rules for EI were changed, resulting in a 
           dramatic increase in the number of EI beneficiaries in October 2020.")
      
    }else {
      HTML("Source: Statistics Canada")
    }
    
  })

}

## Knit together ui and server ----
shinyApp(ui = ui, server = server)
