# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic benchmarks for selected countries.
# Takuya Yamanaka, October 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.1"

## load packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(kableExtra)

# selected countries
select <- c("AO",	"AR",	"AZ",	"BD",	"BY",	"BZ",	"BO",	"BR",	"KH",	"CF",	"CL",	"CN",	"CO",	"CG",	"CR",	"CU",	"KP",	"CD",	"DO",	"EC",	"SV",	"ET",	"GA",	"GT",	"GY",	"HT",	"HN", "IN",	"ID",	"JM",	"KE",	"KG",	"LA",	"LS",	"LR",	"MX",	"MN",	"MZ",	"MM",	"NA",	"NP",	"NI",	"NG",	"PK",	"PA",	"PG",	"PY",	"PE",	"PH",	"MD","RU",	"SL","ZA",	"SR",	"TJ",	"TH",	"UG",	"UA",	"TZ",	"UY",	"UZ",	"VE",	"VN",	"ZM",	"ZW")
  
## load notifications, TPT and strategy datasets
### data is as of 18 October 2024
str <- read.csv("latest_strategy_2024-10-18.csv", na.strings = "") |>
  select(country:year,plhiv_all_screen_data_available:m_wrd_tat_lt_48h,m_wrd)

tpt <- read.csv("latest_contacts_tpt_2024-10-18.csv", na.strings = "") |>
  select(iso2,newinc_con:newinc_con_screen)

notif <- read.csv("latest_notifications_2024-10-18.csv", na.strings = "") |>
  select(iso2,new_labconf:tbdeaths_vr)

bench <- read.csv("dx_benchmark_exp.csv")

df <- str |>
  left_join(tpt, by = "iso2") |>
  left_join(notif, by = "iso2") |>
  filter(!is.na(plhiv_all_screen_data_available)|iso2 %in% select) |> 
  # Restrict to the variables needed for the benchmarks
  select(iso2, country, year,
         newinc_con_screen, newinc_con,
         plhiv_all_screen, plhiv_all,
         plhiv_new_screen, plhiv_new,
         prisoners_screen, prisoners,
         miners_screen, miners,
         district_cxr, district,
         district_wrd,
         phcf_wrd, phcf,
         newinc_rdx, c_newinc,
         wrd_test_capacity, presumptive,
         m_wrd_error_rate_lte_5pct, m_wrd,
         presumptive_wrd, presumptive,
         r_rlt_new, r_rlt_ret,
         pulm_labconf_new, pulm_labconf_ret,
         rr_dst_rlt_fq, rr_new, rr_ret,
         rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqu_lzdr,
         rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqu_lzds,
         rr_fqr_bdqr_lzdu, rr_fqr_bdqs_lzdu,
         rr_fqr,
         newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx,
         new_labconf, new_clindx, ret_rel_labconf, ret_rel_clindx,
         district_monitor_pos_rate,
         m_wrd_tat_lt_48h)
  

year <- max(df$year)

rm(str, tpt, notif)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- 
  navbarPage(
    "TB Diagnostic Benchmarks",
    header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "uad_table.css")),
    tabPanel(
      "Country benchmarks",
      fluidPage(theme = shinytheme("sandstone"),
      # --------------------- each country ---------------------#
      fluidPage(
        
        fluidRow(tags$div(id = "page_header",
                          HTML("Select a country"),
                          uiOutput(outputId = "country"))
        ),
        
        fluidRow(
          tags$div(
            style="padding: 10px 20px 10px 20px;class: ms-fontWeight-bold;",
            h4(paste0("WHO benchmarks of universal access to rapid TB diagnostics, ", year))
          )
        ),
        
        fluidRow(
          column(width = 12,
                 tags$div(
                   class   = "cw-container",
                   checked = NA,
                   htmlOutput("uad_benchmarks", quoted=TRUE))
          )
        ),
        
        fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                          textOutput(outputId = "page_footer"))
        ),
        
        fluidRow(tags$div(id = "link",
                          style = "padding-left: 20px; padding-right: 20px;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0("For further details of TB diagnostic benchmarks, see
                                  <a href='https://iris.who.int/handle/10665/366854' target='_blank'>
                            WHO standard: universal access to rapid tuberculosis diagnostics</a>."))
        )),
        
        
        fluidRow(tags$div(id = "metadata",
                          style = "padding: 20px; font-style: italic; font-size: 80%;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0(app_version,
                                      ", Source code on <a href='https://github.com/yamanakatakuya/dx_benchmarks' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
        ))
      ))
    

    
  ),
  
  tabPanel(
    "Details of benchmarks",
  
    # --------------------- each country ---------------------#
    fluidPage(
      
  
      fluidRow(
        tags$div(
          style="padding: 10px 20px 10px 20px;class: ms-fontWeight-bold;",
          h4(paste0("Summary: Numerator and denominator used for WHO benchmarks of universal access to rapid TB diagnostics"))
        )
      ),
      
      fluidRow(
               tags$div(style = "padding-left: 20px; padding-right: 20px;",
                 class   = "cw-container",
                 checked = NA,
                 htmlOutput("table_bench"))
      ),
      
      fluidRow(tags$div(id = "link2",
                        style = "padding-left: 20px; padding-right: 20px;",
                        
                        # Add app version number and links to GTB and Github
                        HTML(paste0("For further details of TB diagnostic benchmarks, see
                                  <a href='https://iris.who.int/handle/10665/366854' target='_blank'>
                            WHO standard: universal access to rapid tuberculosis diagnostics</a>."))
      )),
      
      
      fluidRow(tags$div(id = "metadata2",
                        style = "padding: 20px; font-style: italic; font-size: 80%;",
                        
                        # Add app version number and links to GTB and Github
                        HTML(paste0(app_version,
                                    ", Source code on <a href='https://github.com/yamanakatakuya/dx_benchmarks' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
      ))
      
    )    
  )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {
  
  # output for a country selection
  output$country <- renderUI({
    
    already_selected <- input$iso2
    
    # Create a named list for selectInput
    country_list <- df |>
      select(iso2, country) |>
      arrange(country)
    
    country_list <- setNames(country_list[,"iso2"], country_list[,"country"])
    
    selectInput(inputId = "iso2",
                label = "",
                choices = country_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })
 
# Better row sum ----
# This function sums rows ignoring NAs unless all are NA
# [rowSums() returns 0 instead of NA if all are NA and you use na.rm=TRUE]
# use it like this
# df$snu <- sum_of_row(df[c('new_sn', 'new_su')])
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  # Flush out any NaN's
  summed <- ifelse(is.nan(summed), NA, summed)
  return(summed)
}

# Function to format score based on value range
format_score <- function(x) {
  if (is.na(x)) {
    return("—")
  } else if (x > 100) {
    return("&gt;100*")  # showing >100
  } else if (x >= 10) {
    return(sprintf("%.0f", x))  # No decimal for scores >= 10
  } else if (x >= 1| x==0) {
    return(sprintf("%.1f", x))  # One decimal for scores >= 1 and < 10
  } else {
    return(sprintf("%.2f", x))  # Two decimals for scores < 1
  }
}

append_benchmark_row_html <- function(html_string,
                                      show_as_step_start,
                                      show_as_step_end,
                                      step_number,
                                      step_text,
                                      benchmark_number,
                                      benchmark_text,
                                      numerator,
                                      denominator){
 
  # This function appends the html for a new row to the input html_string with the following structure:
  #
  # <tr @row_class>
  #   <td class='step-@step_number'>@step_number</td>
  #     <td>@step_text</td>
  #     <td>@benchmark_number</td>
  #     <td>@benchmark_text</td>
  #     <td>@numerator</td>
  #     <td>@denominator</td>
  #     <td><span class='hbar' style='width: @width_pct%; background-color: var(--step-@step_number-colour);' title='@benchmark_text: @percentage%'>@percentage</span></td>
  #  </tr>
  
  # Calculate the percentage from the numerator and denominator
  percentage <- ifelse(denominator==0, 
                       NA,
                       numerator * 100 / denominator)

  paste0(html_string,
         ifelse(show_as_step_end == TRUE,
                "<tr class='step-end'>",
                "<tr>"
         ),
         "<td class='step-", step_number, "'>",
         ifelse(show_as_step_start == TRUE,
                step_number,
                "&nbsp;"
         ),
         "</td>",
         "<td>", 
         ifelse(show_as_step_start == TRUE,
                step_text,
                "&nbsp;"
         ),        
         "</td>",
         "<td>", benchmark_number, "</td>",
         "<td>", benchmark_text, "</td>",
         "<td>", 
         ifelse(is.na(numerator), 
                "—", 
                format(numerator, big.mark = " ", scientific = FALSE)
         ), 
         "</td>",
         "<td>", 
         ifelse(is.na(denominator), 
                "—", 
                format(denominator, big.mark = " ", scientific = FALSE)
         ), 
         "</td>",
         "<td>", 
         ifelse(is.na(percentage), 
                "", 
                paste0("<span class='hbar' style='width: ",
                       ifelse(percentage > 100,
                              "100",
                              percentage
                       ),
                       "%; background-color: var(--step-", step_number, "-colour);' title='",
                       benchmark_text,
                       ": ",
                       format_score(percentage),
                       "%'>",
                       format_score(percentage),
                       "&nbsp;</span>"
                )
         ), 
         "</td>
      </tr>"
         
  )
  
}


output$uad_benchmarks <- renderText({
  
  # This function builds the html table of results for the selected country
  
  req(input$iso2)
  
  benchmark_data <- df |> 
    filter(iso2 == input$iso2)
  
  html_table <- "<table id='uad'>
 <thead>
  <tr>
	  <th colspan='2'>Step</th>
	  <th colspan='2'>Benchmark</th>
	  <th>Numerator</th>
	  <th>Denominator</th>
	  <th>Percentage</th>
 </tr>
</thead>
<tbody>" |> 
    
    append_benchmark_row_html(show_as_step_start = TRUE,
                              show_as_step_end = FALSE,
                              step_number = "1",
                              step_text = "Increase the number of people with presumptive TB in care",
                              benchmark_number = "1A",
                              benchmark_text = "Percentage of household contacts who were evaluated for TB disease and TB infection",
                              numerator = benchmark_data$newinc_con_screen,
                              denominator = benchmark_data$newinc_con) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "1",
                              step_text = "",
                              benchmark_number = "1B",
                              benchmark_text = "Percentage of people living with HIV who were screened for TB",
                              numerator = benchmark_data$plhiv_all_screen,
                              denominator = benchmark_data$plhiv_all) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "1",
                              step_text = "",
                              benchmark_number = "1C",
                              benchmark_text = "Percentage of people newly diagnosed with HIV who were screened for TB",
                              numerator = benchmark_data$plhiv_new_screen,
                              denominator = benchmark_data$plhiv_new) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "1",
                              step_text = "",
                              benchmark_number = "1D",
                              benchmark_text = "Percentage of people in prison are screened for TB",
                              numerator = benchmark_data$prisoners_screen,
                              denominator = benchmark_data$prisoners) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "1",
                              step_text = "",
                              benchmark_number = "1E",
                              benchmark_text = "Percentage of miners exposed to silica dust who were screened for TB",
                              numerator = benchmark_data$miners_screen,
                              denominator = benchmark_data$miners) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = TRUE,
                              step_number = "1",
                              step_text = "",
                              benchmark_number = "2",
                              benchmark_text = "Percentage of districts in which chest X-ray is used regularly for TB screening",
                              numerator = benchmark_data$district_cxr,
                              denominator = benchmark_data$district) |> 
    
    append_benchmark_row_html(show_as_step_start = TRUE,
                              show_as_step_end = FALSE,
                              step_number = "2",
                              step_text = "Increase access to WRDs",
                              benchmark_number = "3",
                              benchmark_text = "Percentage of districts in which all facilities have a TB diagnostic algorithm requiring use of a WHO-recommended rapid diagnostic test as the initial diagnostic test for all people with presumptive TB",
                              numerator = benchmark_data$district_wrd,
                              denominator = benchmark_data$district) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "2",
                              step_text = "",
                              benchmark_number = "4",
                              benchmark_text = "Percentage of primary health-care facilities with access to WHO-recommended rapid diagnostic tests",
                              numerator = benchmark_data$phcf_wrd,
                              denominator = benchmark_data$phcf) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "2",
                              step_text = "",
                              benchmark_number = "5",
                              benchmark_text = "Percentage of new and relapse cases tested using a WHO-recommended rapid diagnostic test as the initial diagnostic test",
                              numerator = benchmark_data$newinc_rdx,
                              denominator = benchmark_data$c_newinc) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = TRUE,
                              step_number = "2",
                              step_text = "",
                              benchmark_number = "6",
                              benchmark_text = "Percentage of tests required to test all people with presumptive TB that can be performed with existing instruments",
                              numerator = benchmark_data$wrd_test_capacity,
                              denominator = benchmark_data$presumptive) |> 
    
    append_benchmark_row_html(show_as_step_start = TRUE,
                              show_as_step_end = FALSE,
                              step_number = "3",
                              step_text = "Increase WRD and drug resistance testing",
                              benchmark_number = "7",
                              benchmark_text = "Percentage of sites providing molecular WHO-recommended rapid diagnostics testing for TB with annual error rates of 5% or less",
                              numerator = benchmark_data$m_wrd_error_rate_lte_5pct,
                              denominator = benchmark_data$m_wrd) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "3",
                              step_text = "",
                              benchmark_number = "8",
                              benchmark_text = "Percentage of people with presumptive TB tested with a WHO-recommended rapid diagnostic test",
                              numerator = benchmark_data$presumptive_wrd,
                              denominator = benchmark_data$presumptive) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "3",
                              step_text = "",
                              benchmark_number = "9A",
                              benchmark_text = "Percentage of people diagnosed with new and previously treated bacteriologically confirmed pulmonary TB with test results for rifampicin",
                              numerator = sum_of_row(benchmark_data |> select(r_rlt_new, r_rlt_ret)),
                              denominator = sum_of_row(benchmark_data |> select(pulm_labconf_new, pulm_labconf_ret))) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "3",
                              step_text = "",
                              benchmark_number = "9B",
                              benchmark_text = "Percentage of people with resistance to rifampicin and with test results for susceptibility to fluoroquinolones",
                              numerator = benchmark_data$rr_dst_rlt_fq,
                              denominator = sum_of_row(benchmark_data |> select(rr_new, rr_ret))) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "3",
                              step_text = "",
                              benchmark_number = "9C",
                              benchmark_text = "Percentage of people with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for bedaquiline",
                              numerator = sum_of_row(benchmark_data |> select(rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqr_lzdu, rr_fqr_bdqs_lzdu)),
                              denominator = benchmark_data$rr_fqr) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = TRUE,
                              step_number = "3",
                              step_text = "",
                              benchmark_number = "9D",
                              benchmark_text = "Percentage of people with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for linezolid",
                              numerator = sum_of_row(benchmark_data |> select(rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqu_lzdr, rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqu_lzds)),
                              denominator = benchmark_data$rr_fqr) |> 
    
    append_benchmark_row_html(show_as_step_start = TRUE,
                              show_as_step_end = FALSE,
                              step_number = "4",
                              step_text = "Increase WRD-based diagnosis",
                              benchmark_number = "10",
                              benchmark_text = "Percentage of new and relapse pulmonary bacteriologically confirmed and clinically diagnosed cases tested with a WHO-recommended rapid diagnostic test",
                              numerator = sum_of_row(benchmark_data |> select(newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx)),
                              denominator = sum_of_row(benchmark_data |> select(new_labconf, new_clindx, ret_rel_labconf, ret_rel_clindx))) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = FALSE,
                              step_number = "4",
                              step_text = "",
                              benchmark_number = "11",
                              benchmark_text = "Percentage of districts that monitored test positivity rate",
                              numerator = benchmark_data$district_monitor_pos_rate,
                              denominator = benchmark_data$district) |> 
    
    append_benchmark_row_html(show_as_step_start = FALSE,
                              show_as_step_end = TRUE,
                              step_number = "4",
                              step_text = "",
                              benchmark_number = "12",
                              benchmark_text = "Percentage of laboratories that achieved a turn-around time within 48 hours for at least 80% of samples received for WHO-recommended rapid diagnostic testing",
                              numerator = benchmark_data$m_wrd_tat_lt_48h,
                              denominator = benchmark_data$m_wrd) |> 
    
    paste0("</tbody></table>")
  
  return(html_table)
  
})


  output$page_footer <- renderText({
    paste0("* Value above 100%. Benchmark 6: testing capacity above 100% does not imply universal access but simply sufficient capacity is available. The distribution and accessibility of such capacity may still be inadequate requiring optimization. Other benchmarks: values above 100% may be due to data errors."
    )
  })
  
  output$table_bench <- function(){
    
    # This function builds the static table with the benchmark definitions
    
    step_colors <- c("#064871", "","","","","",
                     "#DA471F", "","","",
                     "#0096B3", "","","","","",
                     "#AD176E", "","")
    
    # Create a kableExtra table
    kable(bench, "html", escape = FALSE, col.names = c("Step", "Benchmark", "Numerator", "Denominator"), table.attr = "style='width:50%;'") |>
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) |>
      row_spec(0, background = "#00A14C", color = "white") |>  # Header row styling
      column_spec(1, extra_css = "text-align: center;", background = step_colors, color = "white", width = "10px") |>
      column_spec(2, extra_css = "text-align: center;", width = "10px") |>
      column_spec(3, width = "500px") |>
      column_spec(4, width = "350px") |>
      collapse_rows(columns = 1, valign = "middle") #|>  # Merge cells for "Step"
  }
  
}  


# Run the application
shinyApp(ui = ui, server = server)

