## Economic Recovery Indicators app ----
## app title: Economic-Indicators
## app ID: 3622761


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

options(scipen = 999999999)  ## so chart axes read properly

## chart theme/functions ----
source("scripts/chart_theme.R")
source("scripts/functions.R")

## load data ----
non_cansim_data <- readRDS("data/non_cansim_data.rds") %>%
  filter(str_sub(ref_date, start = 1, end = 4) >= 2010)

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

titles_all <- read_csv("indicators_list.csv")
labels_all <- titles_all %>% select(label) %>% pull()
charts_multi <- titles_all %>% group_by(order) %>% tally() %>% filter(n > 1) %>% pull(order)

## get cansim data ----
titles <- titles_all %>% filter(dataset == "cansim_auto") %>% select(-dataset)
cansim_v <- titles %>% select(vector) %>% pull()
cansim_t <- titles %>% select(title) %>% pull()
titles <- titles %>% mutate(title = factor(x = cansim_t, levels = cansim_t))

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

## merge data ----
all_data <- bind_rows(cansim_data, non_cansim_data) %>%
  mutate(title = factor(title, levels = unique(titles_all$title))) %>%
  left_join(titles_all %>% select(order, title, line, source), by = "title")

all_stats <- all_data %>%
  get_mom_stats() %>%
  get_yoy_stats() %>%
  get_ytd_stats() %>%
  arrange(title)

## get Consumer Price Index % change Year-Over-Year ----
cpi_yoy <- all_data %>%
  filter(label == "Consumer Price Index") %>%
  get_yoy_stats() %>%
  filter(!is.na(yoy_pct)) %>%
  mutate(title = "<b>Consumer Price Index %</b><br>Change Year-over-Year",
         yoy_pct = janitor::round_half_up(yoy_pct, digits = 1))

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
               ## Summary ----
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
               ## Detailed Summary ----
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
               ## Charts ----
               tabPanel("Charts",
                        value = 3,
                        #style="background-color:#F2F2F2",
                        br(),
                        column(width = 3, 
                        tags$fieldset(
                          tags$legend(h3("Indicator")),
                          #radioButtons(inline = TRUE,
                            selectInput( ## drop-down list
                            inputId = "indicator",
                            label = NULL,
                            choices = unique(labels_all),
                            selected = labels_all[1],
                            selectize = FALSE,
                            size = length(unique(labels_all)),
                            width = "100%")
                        )),
                        column(width = 9,
                        tags$fieldset(
                          br(),br(),br(),
                        plotlyOutput(outputId = "charts"),
                        uiOutput(outputId = "caption")
                        )),
                        br(),
                        # Data table of chart to go here if wanted
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

## define server logic ----
server <- function(input, output, session) {
  
  ## Tab 1: Key Economic Recovery Indicators ----
  
  output$ERI_overall <- DT::renderDataTable({
    
    data <- all_stats %>%  #cansim_stats %>%
      filter(filter_var == "overall") %>%
      format_summary_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  output$ERI_businesses <- DT::renderDataTable({
    
    data <- all_stats %>%  #cansim_stats %>%
      filter(filter_var == "businesses") %>%
      format_summary_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:4)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, escape = FALSE, filter = "none")
    
  })
  
  output$ERI_bcians <- DT::renderDataTable({
    
    data <- all_stats %>%  #cansim_stats %>%
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
    
    data <- all_stats %>%  #cansim_stats %>%
      filter(filter_var == "overall") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
    
  })
  
  output$DET_businesses <- DT::renderDataTable({
    
    data <- all_stats %>%  #cansim_stats %>%
      filter(filter_var == "businesses") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
    
  })
  
  output$DET_bcians <- DT::renderDataTable({
    
    data <- all_stats %>%  #cansim_stats %>%
      filter(filter_var == "bcians") %>%
      format_detailed_data() %>%
      datatable(options = list(columnDefs = list(list(className = 'dt-center', targets = 2:8)),
                               sDom = 't'), ## to remove filtering, pagination, etc. http://legacy.datatables.net/usage/options
                rownames = FALSE, container = custom_headers, escape = FALSE, filter = "none")
  })
  
  ## Tab 3: Charts ----
  
  get_data <- reactive({
    
    req(input$indicator)
    
    line_chart <- all_data %>% 
        filter(label == input$indicator)
    
    ## tables display units, chart displays in thousands
    if(input$indicator == "Housing Starts") {
      line_chart <- line_chart %>%
        mutate(value = janitor::round_half_up(value/1000, digits = 0),
               title = str_replace(title, pattern = "Units", replacement = "Thousands"))
    }
    ## tables display values, chart displays y-o-y
    if(input$indicator == "Consumer Price Index") {
      line_chart <- cpi_yoy %>% 
        select(-value) %>%
        rename(value = yoy_pct)
    }
    
    list(line_chart = line_chart)
  })
  
  output$charts <- renderPlotly({

    if(is.null(get_data()$line_chart)){
      NULL
      
    } else {
      
      # prep plot
      p <- ggplot(get_data()$line_chart,
                  aes(x = ref_date, y = value)) +
        bcstats_chart_theme +
        scale_x_date(limits = c(min(get_data()$line_chart$ref_date),
                                max(get_data()$line_chart$ref_date) + months(3)),
                     expand = c(0,0),
                     date_breaks = "6 months",
                     date_labels = "%b %Y")
                     # date_labels = "%b\n%Y")
      
      ## multi-line charts need geom_line colored by line, with no-title legend
      if(any(get_data()$line_chart$order %in% charts_multi)) {
        
        p <- p +
          labs(x = NULL,
               y = NULL, 
               title = get_data()$line_chart$label %>% head(1) %>% as.character()) +
            geom_line(aes(color = line)) +              # color each line differently
            scale_color_manual(values = line_colors) +  # use specified colors (up to 4)
            bcstats_chart_theme +
            theme(legend.title = element_blank(),
                  legend.position = "bottom"            # this isn't working: plotly ONLY places legend right (ignores all other positions)
                  )
       
        ## And, if CPI chart, add horizontal line at 0
        if(input$indicator == "Consumer Price Index") {
          p <- p +
            # scale_y_continuous(labels = scales::label_percent(scale = 1)) +
            geom_hline(yintercept = 0)

        }
      } else {
        
        p <- p + 
          geom_line() + 
          labs(x = NULL,
               y = NULL, 
               title = get_data()$line_chart$title %>% head(1) %>% as.character())
        
      }

      p <- ggplotly(p)

    }
  })

  
  output$caption <- renderUI({
    
   
    if(any(str_detect(get_data()$line_chart$title, "\\(1\\)"))) {
      HTML(paste0("Source: ", get_data()$line_chart$source %>% head(1), " <br>
           (1) This dataset does not include persons who received the Canada Emergency Response 
           Benefit (CERB). Between March and September 2020, CERB was introduced and the number of 
           EI recipients dropped significantly as persons could not receive both. The CERB program 
           ended on September 27, 2020 and eligibility rules for EI were changed, resulting in a 
           dramatic increase in the number of EI beneficiaries in October 2020."))
      
    } else {
      HTML(paste0("Source: ", get_data()$line_chart$source %>% head(1)))
    }
    
  })

}

## knit together ui and server ----
shinyApp(ui = ui, server = server)
