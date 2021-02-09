# stat functions ----

get_mom_stats <- function(df) {

  df %>%
    group_by(title) %>%
    arrange(ref_date) %>%
    mutate(mom_val = lag(value),
           mom_chg = (value - lag(value, n = 1)),
           mom_pct = ((value / lag(value, n = 1)) - 1) * 100) %>%
    ungroup()
}

get_yoy_stats <- function(df) {

  df %>%
    group_by(title) %>%
    arrange(ref_date) %>%
    mutate(yoy_val = lag(value, n = 12),
           yoy_chg = (value - lag(value, n = 12)),
           yoy_pct = ((value / lag(value, n = 12)) - 1) * 100) %>%
    ungroup()
    
}

## Returns only last period of data
get_ytd_stats <- function(df) {
  
  get_ytd_helper<- function(df_data) {
    df_data <- df_data %>%
      arrange(ref_date)
    
    if(any(str_detect(df_data$title2, "%"), str_detect(df_data$title2, "Employment"), str_detect(df_data$title2, "Average"))) {
      curr_ytd_val <- (zoo::rollsumr(df_data$value, month(df_data$ref_date) %>% tail(1)) %>% tail(1)) / month(df_data$ref_date) %>% tail(1)
      prev_ytd_val <- (zoo::rollsumr(lag(df_data$value, 12), month(df_data$ref_date) %>% tail(1)) %>% tail(1)) / month(df_data$ref_date) %>% tail(1)
      
    }else {
      curr_ytd_val <- zoo::rollsumr(df_data$value, month(df_data$ref_date) %>% tail(1)) %>% tail(1)
      prev_ytd_val <- zoo::rollsumr(lag(df_data$value, 12), month(df_data$ref_date) %>% tail(1)) %>% tail(1)
    }
    
    ytd_chg <- curr_ytd_val - prev_ytd_val
    ytd_pct <- ((curr_ytd_val/ prev_ytd_val)-1) * 100
    
    df_data %>% slice_tail() %>%
      bind_cols(curr_ytd_val = curr_ytd_val, 
                prev_ytd_val = prev_ytd_val, 
                ytd_chg = ytd_chg, 
                ytd_pct = ytd_pct)
  }
  
  
  df <- df %>% 
    mutate(title2 = title) %>%
    group_by(title) %>%
    nest() %>%
    mutate(data2 = purrr::map(data, get_ytd_helper)) %>%
    unnest(cols = c(data2)) %>%
    select(-data, -title2) %>%
    ungroup()
  
}

## Format data functions ----

format_summary_data <- function(data) {
  
  ## Key economic recover indicators have m-o-m but no estimate
  if(exists("mom_val", data)){
    data %>%
      mutate(month = month(ref_date, label = TRUE, abbr = TRUE),
             icon_mom = case_when(mom_chg > 0 ~ as.character(tags$i(class ="fas fa-arrow-alt-circle-up fa-2x", style = "color:#5ec467")),
                                  mom_chg < 0 ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-down fa-2x", style = "color:red")),
                                  TRUE ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-right fa-2x", style = "color:#f5d236"))),
             icon_yoy = case_when(yoy_chg > 0 ~ as.character(tags$i(class ="fas fa-arrow-alt-circle-up fa-2x", style = "color:#5ec467")),
                                  yoy_chg < 0 ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-down fa-2x", style = "color:red")),
                                  TRUE ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-right fa-2x", style = "color:#f5d236"))),
             icon_ytd = case_when(ytd_chg > 0 ~ as.character(tags$i(class ="fas fa-arrow-alt-circle-up fa-2x", style = "color:#5ec467")),
                                  ytd_chg < 0 ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-down fa-2x", style = "color:red")),
                                  TRUE ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-right fa-2x", style = "color:#f5d236")))) %>%
      select(`INDICATOR` = title, 
             `Reference Month` = month, 
             `Compared to Previous Month` = icon_mom, 
             `Compared to 12 Months Ago` = icon_yoy, 
             `Year-to-Date Compared to Same Period 12 Months Ago` = icon_ytd)
    
  } 
  
  ## exports have estimate but no m-o-m
  else{
    
    data %>%
      mutate(month = month(ref_date, label = TRUE, abbr = TRUE),
             Estimate = prettyNum(value, big.mark = ","),
             icon_yoy = case_when(yoy_chg > 0 ~ as.character(tags$i(class ="fas fa-arrow-alt-circle-up fa-2x", style = "color:#5ec467")),
                                  yoy_chg < 0 ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-down fa-2x", style = "color:red")),
                                  TRUE ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-right fa-2x", style = "color:#f5d236"))),
             icon_ytd = case_when(ytd_chg > 0 ~ as.character(tags$i(class ="fas fa-arrow-alt-circle-up fa-2x", style = "color:#5ec467")),
                                  ytd_chg < 0 ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-down fa-2x", style = "color:red")),
                                  TRUE ~ as.character(tags$i(class = "fas fa-arrow-alt-circle-right fa-2x", style = "color:#f5d236")))) %>%
      select(INDICATOR,
             `Reference Month` = month, 
             Estimate, 
             `Compared to 12 Months Ago` = icon_yoy, 
             `Year-to-Date Compared to Same Period 12 Months Ago` = icon_ytd)
    
  }
  
  
}

format_detailed_data <- function(data){
  
  data %>%
    mutate(month = month(ref_date, label = TRUE, abbr = TRUE),
           Estimate = case_when(str_detect(title, "%") ~ paste0(round_half_up(value, 1), "%"),
                                str_detect(title, "Average Hourly Wage Earnings") ~ prettyNum(round_half_up(value, 2), big.mark = ","),
                                str_detect(title, "Consumer Price Index") ~ prettyNum(round_half_up(value, 1), big.mark = ","),
                                TRUE ~ prettyNum(round_half_up(value), big.mark = ",")),
           mom_chg = case_when(str_detect(title, "%") | str_detect(title, "Average Hourly Wage Earnings") ~ prettyNum(round_half_up(mom_chg, 2), big.mark = ","),
                               str_detect(title, "Consumer Price Index") ~ prettyNum(round_half_up(mom_chg, 1), big.mark = ","),
                               TRUE ~ prettyNum(round_half_up(mom_chg), big.mark = ",")),
           yoy_chg = case_when(str_detect(title, "%") | str_detect(title, "Average Hourly Wage Earnings") ~ prettyNum(round_half_up(yoy_chg, 2), big.mark = ","),
                               str_detect(title, "Consumer Price Index") ~ prettyNum(round_half_up(yoy_chg, 1), big.mark = ","),
                               TRUE ~ prettyNum(round_half_up(yoy_chg), big.mark = ",")),
           ytd_chg = case_when(str_detect(title, "Average Hourly Wage Earnings") ~ prettyNum(round_half_up(ytd_chg, 2), big.mark = ","),
                               str_detect(title, "%") | str_detect(title, "Consumer Price Index") ~ prettyNum(round_half_up(ytd_chg, 1), big.mark = ","),
                               TRUE ~ prettyNum(round_half_up(ytd_chg), big.mark = ",")),
           mom_pct = case_when(str_detect(title, "%") ~ "n.a.",
                               TRUE ~ paste0(round_half_up(mom_pct, 1), "%")),
           yoy_pct = case_when(str_detect(title, "%") ~ "n.a.",
                               TRUE ~ paste0(round_half_up(yoy_pct, 1), "%")),
           ytd_pct = case_when(str_detect(title, "%") ~ "n.a.",
                               TRUE ~ paste0(round_half_up(ytd_pct, 1), "%"))) %>%
    select(`INDICATOR` = title, 
           `Reference Month` = month, 
           Estimate,
           mom_chg, mom_pct,
           yoy_chg, yoy_pct,
           ytd_chg, ytd_pct)
  
}

## Datatable headers for Detailed Summary ----

custom_headers <- htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'INDICATOR'),
        th(rowspan = 2, 'Reference Month'),
        th(rowspan = 2, 'Estimate'),
        th(colspan = 2, 'Compared to Previous Month', style="text-align:center"),
        th(colspan = 2, 'Compared to 12 Months Ago', style="text-align:center"),
        th(colspan = 2, 'Year-to-Date Compared to Same Period 12 Months Ago', style="text-align:center")
      ),
      tr(
        lapply(rep(c('# Chg', '% Chg'), 3), th)
      )
    )
  ))





