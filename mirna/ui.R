#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking "Run App" above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinythemes)
library(highcharter)
library(shinyjs)
#library(shinydashboard)
library(visNetwork)


# Define UI for application 
shinyUI(
    navbarPage(title = "miR-Gyn-Explorer",#theme = shinythemes::shinytheme("cosmo"), #theme = shinythemes::themeSelector(),#, 
               id = "top_tabs",
               
               tabPanel("Home", value = 4,
                        ## Tutorial /manual page for app
                        tags$style(".header{
                        margin-top: 100px;
                        margin-right: 50px;
                        margin-left: 50px;
                                   margin-bottom: 100px;
                                   font-size: 40px;
                          font-weight: bold;}" 
                                   ),
                        tags$style(".tool_{
                        margin-left: 50px;
                                  font-size: 30px;
                                   }"),
                        tags$style(".image{
                        margin-left: 50px;
                                  font-size: 35px;
                                   }"),
                        
                        
                        tags$div(class = "header", checked = NA,
                                 tags$p("miR-Gyn-Explorer: Tool to explore microRNA expression profiles of cancers from female reproductive organs and breast tissue.")),
                        
                        tags$div(class = "tool_",
                          tags$p("Using this tool, user can compare and load the cancer stage-wise miRNA differential expression.",
                                        tags$br("We have compared miRNA expression available for clinical stages of cancers of Uterus, endometrium, ovaries, cervix and breast.
                                The microRNA-Seq data is downloaded from TCGA program. Along with miRNA expression, the tool provides interface to explore mRNA and long-non coding RNA expression patterns too. Following are the cancer projects used in the study:"),
                                        tags$br("Breast Invasive Carcinoma (TCGA-BRCA)"),
                                        tags$br("Uterine Corpus Endometrial Carcinoma (TCGA-UCEC)"),
                                        tags$br("Uterine Carcinosarcoma (TCGA-UCS)"),
                                        tags$br("Cervical Squamous Cell Carcinoma and Endocervical Adenocarcinoma (TCGA-CESC)"),
                                        tags$br("Ovarian Serous Cystadenocarcinoma (TCGA-OV)"),
                                        tags$br("Samples were divided into cancer stages and miRNA expression was compared for each type."))),
                        
                        tags$div(class = "image",
                                 tags$p("Workflow Overview:"),
                          #tags$img(height = 100, width = 100, src = "workflow_app.png"),
                          imageOutput('image')
                        )
                        ),
               
               tabPanel("Differential miRNA analysis", value = 1,
                        sidebarLayout(
                          sidebarPanel(
                            conditionalPanel(condition = "input.top_tabs == 1",
                                             conditionalPanel(condition ="input.tabs == 1",
                                                              selectInput("stage", label = h3("Stage-Wise Differential Expression Analysis"),
                                                                          choices = list("Normal vs Stage I", "Between All stages of cancer")),
                                                              
                                                              conditionalPanel(condition = "input.stage == 'Normal vs Stage I'",
                                                                               selectInput("cancerN", label = h3("Cancer Type"),
                                                                                           choices = list("TCGA-BRCA" = "Data/data_all/isomir/TCGA-BRCA-combined_mirna_data.rda",
                                                                                                          "TCGA-UCEC" = "Data/data_all/isomir/TCGA-UCEC-combined_mirna_data.rda")),
                                                                               
                                                                               selectInput("plotsN", label = h3("Plot Type"),
                                                                                           choices = list("VolcanoPlot", "BoxPlot")),
                                                                               
                                                                               # Only show this panel if the plot type is a histogram
                                                                               conditionalPanel(condition = "input.plotsN == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("mirnaN", "miRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a miRNA"))
                                                                               ),
                                                                              
                                                              ),
                                                              
                                                              conditionalPanel(condition = "input.stage == 'Between All stages of cancer'",
                                                                               tags$div(title="Select the cancer Type for DEmiRNA",
                                                                                        selectInput("cancer",label = h3("Cancer Type"),
                                                                                                    choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/isomir/TCGA-BRCA_SII_I-combined_mirna_data.rda",
                                                                                                                                               "TCGA-UCEC" = "Data/data_all/isomir/TCGA-UCEC_SII_I-combined_mirna_data.rda",
                                                                                                                                               "TCGA-UCS" = "Data/data_all/isomir/TCGA-UCS_SII_I-combined_mirna_data.rda",
                                                                                                                                               "TCGA-CESC" = "Data/data_all/isomir/TCGA-CESC_SII_I-combined_mirna_data.rda"),
                                                                                                                   
                                                                                                                   "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/isomir/TCGA-BRCA_SIII_II-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/data_all/isomir/TCGA-UCEC_SIII_II-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/data_all/isomir/TCGA-UCS_SIII_II-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/data_all/isomir/TCGA-CESC_SIII_II-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-OV" = "Data/data_all/isomir/TCGA-OV_SIII_II-combined_mirna_data.rda"),
                                                                                                                   
                                                                                                                   "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/isomir/TCGA-BRCA_SIV_III-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/data_all/isomir/TCGA-UCEC_SIV_III-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/data_all/isomir/TCGA-UCS_SIV_III-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/data_all/isomir/TCGA-CESC_SIV_III-combined_mirna_data.rda",
                                                                                                                                                 "TCGA-OV" = "Data/data_all/isomir/TCGA-OV_SIV_III-combined_mirna_data.rda")),
                                                                                                    selected = "Normal - Stage I")),
                                                                               useShinyjs(),
                                                                               #js function to reset a button, variableName is the button name whose value we want to reset
                                                                               tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){Shiny.onInputChange(variableName, null); 
                                                                               });
                                                                                           "),
                                                                               
                                                                               selectInput("plots", label = h3("Plot Type"),
                                                                                           choices = list("VolcanoPlot", "BoxPlot")),
                                                                               
                                                                               # Only show this panel if the plot type is a histogram
                                                                               conditionalPanel(condition = "input.plots == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("mirna", "miRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a miRNA"))
                                                                               ),
                                                                               
                                                                               actionButton("surv_new", "Show Survival Analysis"),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new != 0",
                                                                                                selectInput("surv", label = h3("Select Survival Analysis"),
                                                                                                            choices = list("Cox-Proportional Hazard Model" = "coxph",
                                                                                                                           "log-rank test" = "km"), selected = "coxph")),
                                                                               conditionalPanel(condition = "input.surv_new != 0",
                                                                                                conditionalPanel(condition = "input.surv == 'coxph'",
                                                                                                                 ## select the count matrix
                                                                                                                 selectInput("cancer3",label = h3("Cancer Type"),
                                                                                                                             choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SII_I_mirna_iso_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SII_I_mirna_iso_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SII_I_mirna_iso_surv_coxph.rda",
                                                                                                                                                                        "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SII_I_mirna_iso_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIII_II_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIII_II_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIII_II_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIII_II_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIII_II_mirna_iso_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIV_III_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIV_III_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIV_III_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIV_III_mirna_iso_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIV_III_mirna_iso_surv_coxph.rda")),
                                                                                                                             selected = "Stage I - Stage II")
                                                                                                                 )
                                                                                                ),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new !=0",
                                                                                                conditionalPanel(condition = "input.surv == 'km'",
                                                                                                                 selectInput("sep", label = h3("Split by"),
                                                                                                                             choices = list("Median" = "median",
                                                                                                                                            "Mean" = "mean",
                                                                                                                                            "First Quantile" = "1stQu",
                                                                                                                                            "Third Quantile" = "3rdQu"))
                                                                                                                 ),
                                                                                                
                                                                                                conditionalPanel(condition = "input.surv == 'km'",
                                                                                                                 conditionalPanel(condition = "input.sep == 'median'",
                                                                                                                                  selectInput("cancer_median",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SII_I_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SII_I_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SII_I_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SII_I_mirna_surv_km_logRK_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIII_II_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIII_II_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIII_II_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIII_II_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIII_I_mirna_surv_km_logRK_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIV_III_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIV_III_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIV_III_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIV_III_mirna_surv_km_logRK_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIV_III_mirna_surv_km_logRK_median.rda")),
                                                                                                                                              selected = "Stage I - Stage II")
                                                                                                                                  ),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep == 'mean'",
                                                                                                                                  selectInput("cancer_mean",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SII_I_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SII_I_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SII_I_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SII_I_mirna_surv_km_logRK_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIII_II_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIII_II_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIII_II_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIII_II_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIII_II_mirna_surv_km_logRK_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIV_III_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIV_III_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIV_III_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIV_III_mirna_surv_km_logRK_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIV_III_mirna_surv_km_logRK_mean.rda")),
                                                                                                                                              selected = "Stage I - Stage II")
                                                                                                                                  ),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep == '1stQu'",
                                                                                                                                  selectInput("cancer_1stQu",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SII_I_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SII_I_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SII_I_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SII_I_mirna_surv_km_logRK_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIII_II_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIII_II_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIII_II_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIII_II_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIII_II_mirna_surv_km_logRK_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIV_III_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIV_III_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIV_III_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIV_III_mirna_surv_km_logRK_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIV_III_mirna_surv_km_logRK_1stQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")
                                                                                                                                  ),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep == '3rdQu'",
                                                                                                                                  selectInput("cancer_3rdQu",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SII_I_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SII_I_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SII_I_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SII_I_mirna_surv_km_logRK_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIII_II_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIII_II_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIII_II_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIII_II_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIII_II_mirna_surv_km_logRK_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/survival_data/TCGA-BRCA_SIV_III_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/data_all/survival_data/TCGA-UCEC_SIV_III_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/data_all/survival_data/TCGA-UCS_SIV_III_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/data_all/survival_data/TCGA-CESC_SIV_III_mirna_surv_km_logRK_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/data_all/survival_data/TCGA-OV_SIV_III_mirna_surv_km_logRK_3rdQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")
                                                                                                                                  )
                                                                                                                 )
                                                                                                )
                                                                               ),
                                                              
                                                              tags$head(
                                                                HTML(
                                                                  "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count3', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                                                                )
                                                              ),
                                                              textOutput("keepAlive3")
                                             ),
                                             
                                             conditionalPanel(condition ="input.tabs == 2",
                                                              
                                                              
                                                              selectInput("cancer1",label = h3("Cancer Type"),
                                                                          choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/data_all/isomir_target/targets/TCGA-BRCA_SI_NT_mirna_targtes.rda",
                                                                                                                   "TCGA-UCEC" = "Data/data_all/isomir_target/targets/TCGA-UCEC_SI_NT_mirna_targtes.rda"),
                                                                                         "Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/isomir_target/targets/TCGA-BRCA_SII_I_mirna_targtes.rda",
                                                                                                                     "TCGA-UCEC" = "Data/data_all/isomir_target/targets/TCGA-UCEC_SII_I_mirna_targtes.rda",
                                                                                                                     "TCGA-UCS" = "Data/data_all/isomir_target/targets/TCGA-UCS_SII_I_mirna_targtes.rda",
                                                                                                                     "TCGA-CESC" = "Data/data_all/isomir_target/targets/TCGA-CESC_SII_I_mirna_targtes.rda"),
                                                                                         
                                                                                         "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/isomir_target/targets/TCGA-BRCA_SIII_II_mirna_targtes.rda",
                                                                                                                       "TCGA-UCEC" = "Data/data_all/isomir_target/targets/TCGA-UCEC_SIII_II_mirna_targtes.rda",
                                                                                                                       "TCGA-UCS" = "Data/data_all/isomir_target/targets/TCGA-UCS_SIII_II_mirna_targtes.rda",
                                                                                                                       "TCGA-CESC" = "Data/data_all/isomir_target/targets/TCGA-CESC_SIII_II_mirna_targtes.rda",
                                                                                                                       "TCGA-OV" = "Data/data_all/isomir_target/targets/TCGA-OV_SIII_II_mirna_targtes.rda"),
                                                                                         
                                                                                         "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/isomir_target/targets/TCGA-BRCA_SIV_III_mirna_targtes.rda",
                                                                                                                       "TCGA-UCEC" = "Data/data_all/isomir_target/targets/TCGA-UCEC_SIV_III_mirna_targtes.rda",
                                                                                                                       "TCGA-UCS" = "Data/data_all/isomir_target/targets/TCGA-UCS_SIV_III_mirna_targtes.rda",
                                                                                                                       "TCGA-CESC" = "Data/data_all/isomir_target/targets/TCGA-CESC_SIV_III_mirna_targtes.rda",
                                                                                                                       "TCGA-OV" = "Data/data_all/isomir_target/targets/TCGA-OV_SIV_III_mirna_targtes.rda")),
                                                                          selected = "Normal - Stage I"),
                                                              
                                                              selectInput("db", label = h3("Select Target Database"),
                                                                          choices = list("miRTarBase" = "mirtarbase",
                                                                                         "miRecords" = "mirecords",
                                                                                         "TarBase" = "tarbase")),
                                                              
                                                              actionButton("interaction_button", "Show miRNA Target Interactions"),
                                                              
                                                              conditionalPanel("input.interaction_button != 0 ",
                                                                               selectInput("int_", label = h3("Select Interactions"),
                                                                                           choices = list("miRNA-mRNA Interactions" = "mi_mrna", 
                                                                                                          "miRNA-lncRNA Interactions" = "mi_lnc")),
                                                                               
                                                                               conditionalPanel(condition = "input.int_ == 'mi_mrna'",
                                                                                                #input DEG list file for mRNA
                                                                                                selectInput("mrna_mat1", label = h3("Validated interactions of DEmiRNAs and DEGs"),
                                                                                                            choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-BRCA_SI_NT_mi_mrna_int_2lfc.rda",
                                                                                                                                                     "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCEC_SI_NT_mi_mrna_int_2lfc.rda"),
                                                                                                                           
                                                                                                                           "Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-BRCA_SII_I_mi_mrna_int_2lfc.rda",
                                                                                                                                                       "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCEC_SII_I_mi_mrna_int_2lfc.rda",
                                                                                                                                                       "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCS_SII_I_mi_mrna_int_2lfc.rda",
                                                                                                                                                       "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-CESC_SII_I_mi_mrna_int_2lfc.rda"),
                                                                                                                           
                                                                                                                           "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-BRCA_SIII_II_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCEC_SIII_II_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCS_SIII_II_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-CESC_SIII_II_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-OV_SIII_II_mi_mrna_int_2lfc.rda"),
                                                                                                                           
                                                                                                                           "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-BRCA_SIV_III_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCEC_SIV_III_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-UCS_SIV_III_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-CESC_SIV_III_mi_mrna_int_2lfc.rda",
                                                                                                                                                         "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_mrna_int/TCGA-OV_SIV_III_mi_mrna_int_2lfc.rda")),
                                                                                                            selected = "Normal - Stage I")
                                                                                                ),
                                                                               
                                                                               conditionalPanel(condition = "input.int_ == 'mi_lnc'",
                                                                                                
                                                                                                selectInput("int_type", label = h3("Validated/predicted interactions"),
                                                                                                            choices = list("Validated", "Predicted")),
                                                                               
                                                                                                conditionalPanel(condition = "input.int_type == 'Validated'",
                                                                                                                 selectInput("val_int", label = h3("Validated interactions of DEmiRNAs and DElnRNAs"),
                                                                                                                             choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SI_NT_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                      "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SI_NT_mi_lnc_int_val_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SI_NT_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                        "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SII_I_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                        "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SII_I_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                        "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SII_I_mi_lnc_int_val_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SIII_II_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SIII_II_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SIII_II_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SIII_II_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-OV_SIII_II_mi_lnc_int_val_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SIV_III_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SIV_III_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SIV_III_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SIV_III_mi_lnc_int_val_2lfc.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-OV_SIV_III_mi_lnc_int_val_2lfc.rda")),
                                                                                                                             selected = "Normal - Stage I")
                                                                                                                 ),
                                                                                                
                                                                                                conditionalPanel(condition = "input.int_type == 'Predicted'",
                                                                                                                 selectInput("pred_int", label = h3("Predicted interactions of DEmiRNAs and DElncRNAs"),
                                                                                                                             choices = list("Normal - StageI" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SI_NT_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                     "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SI_NT_mi_lnc_int_pred_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage I - Stage II" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SII_I_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                        "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SII_I_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                        "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SII_I_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                        "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SII_I_mi_lnc_int_pred_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage II - Stage III" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SIII_II_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SIII_II_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SIII_II_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SIII_II_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-OV_SIII_II_mi_lnc_int_pred_2lfc.rda"),
                                                                                                                                            
                                                                                                                                            "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-BRCA_SIV_III_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCEC_SIV_III_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-UCS_SIV_III_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-CESC_SIV_III_mi_lnc_int_pred_2lfc.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/data_all/isomir_target/miRNA_int/mirna_lnc_int/TCGA-OV_SIV_III_mi_lnc_int_pred_2lfc.rda")),
                                                                                                                             selected = "Normal - Stage I")
                                                                                                                 )
                                                                                                )
                                                                               ),
                                                              
                                                              actionButton("func_annot", "miRNA Target Functional Annotations & Protein Expression"),
                                                              
                                                              conditionalPanel(condition = "input.func_annot != 0",
                                                                               
                                                                               selectInput("annot_exp", label = h3("Annotations & Expression"),
                                                                                           choices = list("GO", "KEGG", "HPA")),
                                                                               
                                                                               conditionalPanel(condition = "input.annot_exp == 'GO'",
                                                                                                selectInput("go_file", label = h3("Gene Ontology of miRNA-targets"),
                                                                                                            choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SI_NT_GO.rda",
                                                                                                                                                     "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SI_NT_GO.rda"),
                                                                                                                           
                                                                                                                           "Stage I - Stage II" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SII_I_GO.rda",
                                                                                                                                                       "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SII_I_GO.rda",
                                                                                                                                                       "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SII_I_GO.rda",
                                                                                                                                                       "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SII_I_GO.rda"),
                                                                                                                           
                                                                                                                           "Stage II - Stage III" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SIII_II_GO.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SIII_II_GO.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SIII_II_GO.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SIII_II_GO.rda",
                                                                                                                                                         "TCGA-OV" = "Data/go_plots_kegg/TCGA-OV_SIII_II_GO.rda"),
                                                                                                                           
                                                                                                                           "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SIV_III_GO.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SIV_III_GO.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SIV_III_GO.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SIV_III_GO.rda",
                                                                                                                                                         "TCGA-OV" = "Data/go_plots_kegg/TCGA-OV_SIV_III_GO.rda")))
                                                                                                ),
                                                                               
                                                                               conditionalPanel(condition = "input.annot_exp == 'KEGG'",
                                                                                                selectInput("kegg_file", label = h3("Singificant Pathways of miRNA-targets"),
                                                                                                            choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SI_NT_kegg.rda",
                                                                                                                                                     "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SI_NT_kegg.rda"),
                                                                                                                           
                                                                                                                           "Stage I - Stage II" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SII_I_kegg.rda",
                                                                                                                                                       "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SII_I_kegg.rda",
                                                                                                                                                       "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SII_I_kegg.rda",
                                                                                                                                                       "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SII_I_kegg.rda"),
                                                                                                                           
                                                                                                                           "Stage II - Stage III" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SIII_II_kegg.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SIII_II_kegg.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SIII_II_kegg.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SIII_II_kegg.rda",
                                                                                                                                                         "TCGA-OV" = "Data/go_plots_kegg/TCGA-OV_SIII_II_kegg.rda"),
                                                                                                                           
                                                                                                                           "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/go_plots_kegg/TCGA-BRCA_SIV_III_kegg.rda",
                                                                                                                                                         "TCGA-UCEC" = "Data/go_plots_kegg/TCGA-UCEC_SIV_III_kegg.rda",
                                                                                                                                                         "TCGA-UCS" = "Data/go_plots_kegg/TCGA-UCS_SIV_III_kegg.rda",
                                                                                                                                                         "TCGA-CESC" = "Data/go_plots_kegg/TCGA-CESC_SIV_III_kegg.rda",
                                                                                                                                                         "TCGA-OV" = "Data/go_plots_kegg/TCGA-OV_SIV_III_kegg.rda"))
                                                                                                            )
                                                                                                ),
                                                                               
                                                                               conditionalPanel(condition = "input.annot_exp == 'HPA'",
                                                                                                #select cancer type
                                                                                                selectInput("cancertype", label = h3("Protein Expression: Pathology data for Cancer-type and Tissue"),
                                                                                                            choices = list("Breast" = "breast cancer",
                                                                                                                           "Endometrium" = "endometrial cancer",
                                                                                                                           "Ovary" = "ovarian cancer",
                                                                                                                           "Cervix" = "cervical cancer"))
                                                                                                )
                                                                               ),
                                                              
                                                              tags$head(
                                                                HTML(
                                                                  "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count4', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                                                                )
                                                              ),
                                                              textOutput("keepAlive4")
                                             ),
                                             
                                             conditionalPanel(condition="input.tabs == 3",
                                                              ## select the count matrix
                                                              selectInput("cancer2",label = h3("Cancer Type"),
                                                                          choices = list("Normal - Stage I" = list("TCGA-BRCA" = "Data/network/TCGA-BRCA_SI_NT_cor_deg.rda",
                                                                                                                   "TCGA-UCEC" = "Data/network/TCGA-UCEC_SI_NT_cor_deg.rda"),
                                                                                         
                                                                                         "Stage I - Stage II" = list("TCGA-BRCA" = "Data/network/TCGA-BRCA_SII_I_cor_deg.rda",
                                                                                                                     "TCGA-UCEC" = "Data/network/TCGA-UCEC_SII_I_cor_deg.rda",        
                                                                                                                     "TCGA-UCS" = "Data/network/TCGA-UCS_SII_I_cor_deg.rda",
                                                                                                                     "TCGA-CESC" = "Data/network/TCGA-CESC_SII_I_cor_deg.rda"),
                                                                                         
                                                                                         "Stage II - Stage III" = list("TCGA-BRCA" = "Data/network/TCGA-BRCA_SIII_II_cor_deg.rda",
                                                                                                                       "TCGA-UCEC" = "Data/network/TCGA-UCEC_SIII_II_cor_deg.rda",
                                                                                                                       "TCGA-UCS" = "Data/network/TCGA-UCS_SIII_II_cor_deg.rda",
                                                                                                                       "TCGA-CESC" = "Data/network/TCGA-CESC_SIII_II_cor_deg.rda",
                                                                                                                       "TCGA-OV" = "Data/network/TCGA-OV_SIII_II_cor_deg.rda"),
                                                                                         
                                                                                         "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/network/TCGA-BRCA_SIV_III_cor_deg.rda",
                                                                                                                       "TCGA-UCEC" = "Data/network/TCGA-UCEC_SIV_III_cor_deg.rda",
                                                                                                                       "TCGA-UCS" = "Data/network/TCGA-UCS_SIV_III_cor_deg.rda",
                                                                                                                       "TCGA-CESC" = "Data/network/TCGA-CESC_SIV_III_cor_deg.rda",
                                                                                                                       "TCGA-OV" = "Data/network/TCGA-OV_SIV_III_cor_deg.rda")),
                                                                          selected = "Stage I - Stage II"),
                                                              
                                                              sliderInput("cutoff", "Correlation threshold",
                                                                          min = 0.5, max = 1, value = 0.75),
                                                              
                                                              #selectInput("enrichment", label = h3("Enrichment Analysis of correlated mRNA"),
                                                                          #choices = list("Select Genelist" = "genelist")),
                                                              tags$head(
                                                                HTML(
                                                                  "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count5', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                                                                )
                                                              ),
                                                              textOutput("keepAlive5")
                                             )
                                             )
                            ),
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            tabsetPanel(id = "tabs",
                                        tabPanel("Differential miRNA analysis",value = 1,
                                                 conditionalPanel(condition = "input.stage == 'Normal vs Stage I'",
                                                                  DT::dataTableOutput("DEmiRNA")
                                                 ),
                                                 conditionalPanel(condition = "input.stage == 'Between All stages of cancer'",
                                                                  DT::dataTableOutput("DEmiRNA_stage"), uiOutput("popup")
                                                 ),
                                                 
                                                 conditionalPanel(condition = "input.stage == 'Normal vs Stage I'",
                                                                  conditionalPanel(condition = "input.plotsN == 'VolcanoPlot'",
                                                                                   "Volcano-Plot", highchartOutput("volcanoPlotN", height = "500px")
                                                                  ),
                                                                  conditionalPanel(condition = "input.plotsN == 'BoxPlot'",
                                                                                   "Violin Plot", plotOutput("vioPlotN")
                                                                  )
                                                 ),
                                                 
                                                 
                                                 conditionalPanel(condition = "input.stage == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.plots == 'VolcanoPlot'",
                                                                                   "Volcano-Plot", highchartOutput("volcanoPlot", height = "500px")
                                                                  ),
                                                                  conditionalPanel(condition = "input.plots == 'BoxPlot'",
                                                                                   "Violin Plot", plotOutput("vioPlot")
                                                                  )
                                                 ),
                                                 
                                                 conditionalPanel(condition = "input.stage == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.surv_new !=0",
                                                                                   conditionalPanel(condition = "input.surv == 'coxph'",
                                                                                                    "Cox-Proportaional Hazard Model", DT::dataTableOutput("survival_cox")
                                                                                   ),
                                                                                   conditionalPanel(condition = "input.surv == 'km'",
                                                                                                    "log-rank test", DT::dataTableOutput("survival_KM")
                                                                                   )
                                                                  )
                                                                  
                                                 )
                                                 
                                        ),
                                        tabPanel("miRNA-Targets",value = 2, 
                                                 
                                                 DT::dataTableOutput("miRTarget"),
                                                 
                                                 
                                                 conditionalPanel("input.interaction_button != 0 ",
                                                                  conditionalPanel(condition = "input.int_ == 'mi_mrna'",
                                                                                   "miRNA-mRNA Interactions", uiOutput("ui_mrna_val")
                                                                                   ),
                                                                  
                                                                  conditionalPanel(condition = "input.int_ == 'mi_lnc'",
                                                                                   conditionalPanel(condition = "input.int_type == 'Validated'",
                                                                                                    "miRNA-lncRNA validated interactions", uiOutput("ui_val")
                                                                                   )),
                                                                  
                                                                  conditionalPanel(condition = "input.int_ == 'mi_lnc'",
                                                                                   conditionalPanel(condition = "input.int_type == 'Predicted'",
                                                                                                    "miRNA-lncRNA predicted interactions", uiOutput("ui_pred")
                                                                                   ))
                                                                  
                                                                  ),
                                                 
                                                 
                                                 conditionalPanel(condition = "input.func_annot != 0",
                                                                  conditionalPanel(condition = "input.annot_exp == 'GO'",
                                                                                   "Functional Annotations", plotOutput("GO", width = "85%",height = "1200px")
                                                                                   ),
                                                                  
                                                                  conditionalPanel(condition = "input.annot_exp == 'KEGG'",
                                                                                   "KEGG Pathways", plotOutput("KEGG", width = "85%",height = "1000px")
                                                                                   ),
                                                                  
                                                                  conditionalPanel(condition = "input.annot_exp == 'HPA'",
                                                                                   "Cancer Patholog Data", DT::dataTableOutput("hpa") 
                                                                                   )
                                                                  )
                                                 ),
                                        
                                        
                                        tabPanel("Network",value = 3, 
                                                 h3("Network of differentially regulated miRNa-mRNA-lncRNA"), 
                                                 h3("Hub network modules are identified using fastgreedy method."),
                                                  h3("The nodes of network represent miRNA(sqaure shape), mRNA(circle shape) and lncRNA(trianle shape)
                                                    For each generated subnetwork, Assortivity coefficient is calculated"),
                                                 
                                                 visNetworkOutput("visnetwork",height="800px"),
                                                 h3("Assortivity coefficient of Network", textOutput("assort")),
                                                 
                                                 h3("Table of correlation between network node can be viewed below by clicking on the a specific node of network!"),DT::dataTableOutput("edge_tbl"),
                                                 
                                                 "Enrichment analysis of highly correlated mRNA and lncRNAs", 
                                                 h3("The genelist of generated network module can be downloaded PPI network identification."), downloadButton("download_button", label = "Download_genelist")
                                                                  
                                                 )
                                        )
                            )
                          )
                        ),
               
               tabPanel("Differential mRNA analysis", value = 2,
                        useShinyjs(),
                        #js function to reset a button, variableName is the button name whose value we want to reset
                        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                        Shiny.onInputChange(variableName, null);
                        });
                                    "),
                        
                        sidebarLayout(
                          sidebarPanel(
                            conditionalPanel(condition = "input.top_tabs == 2",
                                             conditionalPanel(condition = "input.tabs_mrna == 1",
                                                              selectInput("stage_mrna", label = h3("Stage-Wise Differential Expression Analysis"),
                                                                          choices = list("Normal vs Stage I", "Between All stages of cancer")),
                                                              
                                                              conditionalPanel(condition = "input.stage_mrna == 'Normal vs Stage I'",
                                                                               selectInput("cancerN_mrna", label = h3("Cancer Type"),
                                                                                           choices = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-BRCA_SI_NT.rda",
                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCEC_SI_NT.rda")),
                                                                               
                                                                               actionButton("plots_action", "Visualization"),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action != 0",
                                                                                                selectInput("plotsN_mrna", label = h3("Plot Type"),
                                                                                                            choices = list("VolcanoPlot", "BoxPlot"))),
                                                                               
                                                                               # Only show this panel if the plot type is a histogram
                                                                               conditionalPanel(condition = "input.plots_action != 0",
                                                                                                conditionalPanel(condition = "input.plotsN_mrna == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("mrnaN", "mRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a mRNA"))
                                                                                                ))
                                                              ),
                                                              
                                                              conditionalPanel(condition = "input.stage_mrna == 'Between All stages of cancer'",
                                                                               tags$div(title="Select the cancer Type for DEGs",
                                                                                        selectInput("cancer_mrna",label = h3("Cancer Type"),
                                                                                                    choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-BRCA_SII_I.rda",
                                                                                                                                               "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCEC_SII_I.rda",
                                                                                                                                               "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCS_SII_I.rda",
                                                                                                                                               "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-CESC_SII_I.rda"),
                                                                                                                   
                                                                                                                   "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-BRCA_SIII_II.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCEC_SIII_II.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCS_SIII_II.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-CESC_SIII_II.rda",
                                                                                                                                                 "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-OV_SIII_II.rda"),
                                                                                                                   
                                                                                                                   "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-BRCA_SIV_III.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCEC_SIV_III.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-UCS_SIV_III.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-CESC_SIV_III.rda",
                                                                                                                                                 "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/pro_coding/TCGA-OV_SIV_III.rda")),
                                                                                                    selected = "Stage I - Stage II")),
                                                                               useShinyjs(),
                                                                               #js function to reset a button, variableName is the button name whose value we want to reset
                                                                               tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){Shiny.onInputChange(variableName, null); 
                                                                               });
                                                                                           "),
                                                                               
                                                                               actionButton("plots_action_stage", "Visualization"),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action_stage != 0",
                                                                               selectInput("plots_mrna", label = h3("Plot Type"),
                                                                                           choices = list("VolcanoPlot", "BoxPlot"))),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action_stage != 0",
                                                                               conditionalPanel(condition = "input.plots_mrna == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("mrna", "mRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a mRNA"))
                                                                                                )),
                                                                               
                                                                               actionButton("surv_new_mrna", "Show Survival Analysis"),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_mrna != 0",
                                                                                                selectInput("surv_mrna", label = h3("Select Survival Analysis"),
                                                                                                            choices = list("Cox-Proportional Hazard Model" = "coxph_mrna",
                                                                                                                           "log-rank test" = "km_mrna"), selected = "coxph_mrna")),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_mrna != 0",
                                                                                                conditionalPanel(condition = "input.surv_mrna == 'coxph_mrna'",
                                                                                                                 ## select the count matrix
                                                                                                                 selectInput("cancer3_mrna",label = h3("Cancer Type"),
                                                                                                                             choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SII_I_mRNA_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SII_I_mRNA_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_mRNA_surv_coxph.rda",
                                                                                                                                                                        "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SII_I_mRNA_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIII_II_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIII_II_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIII_II_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIII_II_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIII_II_mRNA_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIV_III_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIV_III_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIV_III_mRNA_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIV_III_mRNA_surv_coxph.rda")),
                                                                                                                             selected = "Stage I - Stage II")
                                                                                                )
                                                                               ),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_mrna !=0",
                                                                                                conditionalPanel(condition = "input.surv_mrna == 'km_mrna'",
                                                                                                                 selectInput("sep_mrna", label = h3("Split by"),
                                                                                                                             choices = list("Median" = "median_mrna",
                                                                                                                                            "Mean" = "mean_mrna",
                                                                                                                                            "First Quantile" = "1stQu_mrna",
                                                                                                                                            "Third Quantile" = "3rdQu_mrna"))),
                                                                                                
                                                                                                conditionalPanel(condition = "input.surv_mrna == 'km_mrna'",
                                                                                                                 conditionalPanel(condition = "input.sep_mrna == 'median_mrna'",
                                                                                                                                  selectInput("cancer_median_mrna",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SII_I_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SII_I_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SII_I_mrna_surv_KMLR_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIII_II_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIII_II_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIII_II_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIII_II_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIII_II_mrna_surv_KMLR_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIV_III_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIV_III_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIV_III_mrna_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIV_III_mrna_surv_KMLR_median.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_mrna == 'mean_mrna'",
                                                                                                                                  selectInput("cancer_mean_mrna",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SII_I_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SII_I_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SII_I_mrna_surv_KMLR_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIII_II_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIII_II_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIII_II_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIII_II_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIII_II_mrna_surv_KMLR_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIV_III_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIV_III_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIV_III_mrna_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIV_III_mrna_surv_KMLR_mean.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_mrna == '1stQu_mrna'",
                                                                                                                                  selectInput("cancer_1stQu_mrna",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SII_I_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SII_I_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SII_I_mrna_surv_KMLR_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIII_II_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIII_II_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIII_II_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIII_II_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIII_II_mrna_surv_KMLR_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIV_III_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIV_III_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIV_III_mrna_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIV_III_mrna_surv_KMLR_1stQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_mrna == '3rdQu_mrna'",
                                                                                                                                  selectInput("cancer_3rdQu_mrna",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SII_I_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SII_I_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SII_I_mrna_surv_KMLR_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIII_II_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCEC_SIII_II_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SIII_II_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SIII_II_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-OV_SIII_II_mrna_surv_KMLR_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-BRCA_SIV_III_mrna_surv_KMLR_3rdQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")))
                                                                               )
                                                                               
                                                              ),
                                                              tags$head(
                                                                HTML(
                                                                  "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count6', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                                                                )
                                                              ),
                                                              textOutput("keepAlive6")
                                                              )
                                             
                                             )
                          ),
                
                          mainPanel(
                            tabsetPanel(id = "tabs_mrna",
                                        tabPanel("Differential mRNA analysis", value = 1,
                                                 conditionalPanel(condition = "input.stage_mrna == 'Normal vs Stage I'",
                                                                  DT::dataTableOutput("DEG")
                                                                  ),
                                                 
                                                 conditionalPanel(condition = "input.stage_mrna == 'Between All stages of cancer'",
                                                                          DT::dataTableOutput("DEG_stage"), uiOutput("popup_mrna")
                                                                  ),
                                                 conditionalPanel(condition = "input.stage_mrna == 'Normal vs Stage I'",
                                                                  conditionalPanel(condition = "input.plots_action != 0",
                                                                  conditionalPanel(condition = "input.plotsN_mrna == 'VolcanoPlot'",
                                                                                   "Volcano-Plot", highchartOutput("volcanoPlotN_mrna", height = "500px")
                                                                                   ),
                                                                  
                                                                  conditionalPanel(condition = "input.plotsN_mrna == 'BoxPlot'",
                                                                                           "Violin Plot", plotOutput("vioPlotN_mrna")
                                                                                   ))
                                                                  ),
                                                 
                                                 conditionalPanel(condition = "input.stage_mrna == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.plots_action_stage != 0",
                                                                                   conditionalPanel(condition = "input.plots_mrna == 'VolcanoPlot'",
                                                                                           "Volcano-Plot", highchartOutput("volcanoPlot_mrna", height = "500px")
                                                                                           ),
                                                                                   
                                                                                   conditionalPanel(condition = "input.plots_mrna == 'BoxPlot'",
                                                                                           "Violin Plot", plotOutput("vioPlot_mrna")
                                                                                           ))
                                                                  ),
                                                 
                                                 conditionalPanel(condition = "input.stage_mrna == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.surv_new_mrna !=0",
                                                                                           conditionalPanel(condition = "input.surv_mrna == 'coxph_mrna'",
                                                                                                            "Cox-Proportaional Hazard Model", DT::dataTableOutput("survival_cox_mrna")
                                                                                                            ),
                                                                                   
                                                                                   conditionalPanel(condition = "input.surv_mrna == 'km_mrna'",
                                                                                                            "log-rank test", DT::dataTableOutput("survival_KM_mrna")
                                                                                                    )
                                                                                   )
                                                                  )
                                                 )
                                        )
                            )
                          )
                        ),
               
               tabPanel("Differential lncRNA analysis", value = 3,
                        useShinyjs(),
                        #js function to reset a button, variableName is the button name whose value we want to reset
                        tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                        Shiny.onInputChange(variableName, null);
                        });
                                    "),
                        
                        sidebarLayout(
                          sidebarPanel(
                            conditionalPanel(condition = "input.top_tabs == 3",
                                             conditionalPanel(condition = "input.tabs_lnc == 1",
                                                              selectInput("stage_lnc", label = h3("Stage-Wise Differential Expression Analysis"),
                                                                          choices = list("Normal vs Stage I", "Between All stages of cancer")),
                                                              
                                                              conditionalPanel(condition = "input.stage_lnc == 'Normal vs Stage I'",
                                                                               selectInput("cancerN_lnc", label = h3("Cancer Type"),
                                                                                           choices = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_BRCA_RNA_Seq_lnc_SI_NT.rda",
                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCEC_RNA_Seq_lnc_SI_NT.rda")),
                                                                               
                                                                               actionButton("plots_action_lnc", "Visualization"),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action_lnc != 0",
                                                                               selectInput("plotsN_lnc", label = h3("Plot Type"),
                                                                                           choices = list("VolcanoPlot", "BoxPlot"))),
                                                                               
                                                                               # Only show this panel if the plot type is a histogram
                                                                               conditionalPanel(condition = "input.plots_action_lnc != 0",
                                                                               conditionalPanel(condition = "input.plotsN_lnc == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("lncN", "lncRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a lncRNA"))
                                                                                                ))
                                                              ),
                                                              
                                                              conditionalPanel(condition = "input.stage_lnc == 'Between All stages of cancer'",
                                                                               tags$div(title="Select the cancer Type for DEGs",
                                                                                        selectInput("cancer_lnc",label = h3("Cancer Type"),
                                                                                                    choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_BRCA_RNA_Seq_lnc_SII_I.rda",
                                                                                                                                               "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCEC_RNA_Seq_lnc_SII_I.rda",
                                                                                                                                               "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCS_RNA_Seq_lnc_SII_I.rda",
                                                                                                                                               "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_CESC_RNA_Seq_lnc_SII_I.rda"),
                                                                                                                   
                                                                                                                   "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_BRCA_RNA_Seq_lnc_SIII_II.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCEC_RNA_Seq_lnc_SIII_II.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCS_RNA_Seq_lnc_SIII_II.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_CESC_RNA_Seq_lnc_SIII_II.rda",
                                                                                                                                                 "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_OV_RNA_Seq_lnc_SIII_II.rda"),
                                                                                                                   
                                                                                                                   "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_BRCA_RNA_Seq_lnc_SIV_III.rda",
                                                                                                                                                 "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCEC_RNA_Seq_lnc_SIV_III.rda",
                                                                                                                                                 "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_UCS_RNA_Seq_lnc_SIV_III.rda",
                                                                                                                                                 "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_CESC_RNA_Seq_lnc_SIV_III.rda",
                                                                                                                                                 "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/RNA_seq_lnc/TCGA_OV_RNA_Seq_lnc_SIV_III.rda")),
                                                                                                    selected = "Stage I - Stage II")),
                                                                               
                                                                               useShinyjs(),
                                                                               #js function to reset a button, variableName is the button name whose value we want to reset
                                                                               tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){Shiny.onInputChange(variableName, null); 
                                                                               });
                                                                                           "),
                                                                               
                                                                               actionButton("plots_action_lnc_stage", "Visualization"),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action_lnc_stage != 0",
                                                                               selectInput("plots_lnc", label = h3("Plot Type"),
                                                                                           choices = list("VolcanoPlot", "BoxPlot"))),
                                                                               
                                                                               conditionalPanel(condition = "input.plots_action_lnc_stage != 0",
                                                                               conditionalPanel(condition = "input.plots_lnc == 'BoxPlot'",
                                                                                                ## select gene from list
                                                                                                selectizeInput("lnc", "lncRNA", choices = "", selected = "",multiple = FALSE, options = list(placeholder = "Search for a lncRNA"))
                                                                                                )),
                                                                               
                                                                               actionButton("surv_new_lnc", "Show Survival Analysis"),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_lnc != 0",
                                                                                                selectInput("surv_lnc", label = h3("Select Survival Analysis"),
                                                                                                            choices = list("Cox-Proportional Hazard Model" = "coxph_lnc",
                                                                                                                           "log-rank test" = "km_lnc"), selected = "coxph_lnc")),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_lnc != 0",
                                                                                                conditionalPanel(condition = "input.surv_lnc == 'coxph_lnc'",
                                                                                                                 ## select the count matrix
                                                                                                                 selectInput("cancer3_lnc",label = h3("Cancer Type"),
                                                                                                                             choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SII_I_lnc_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SII_I_lnc_surv_coxph.rda",
                                                                                                                                                                        "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SII_I_lnc_surv_coxph.rda",
                                                                                                                                                                        "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SII_I_lnc_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIII_II_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIII_II_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIII_II_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIII_II_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIII_II_lnc_surv_coxph.rda"),
                                                                                                                                            
                                                                                                                                            "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIV_III_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIV_III_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIV_III_lnc_surv_coxph.rda",
                                                                                                                                                                          "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIV_III_lnc_surv_coxph.rda")),
                                                                                                                             selected = "Stage I - Stage II")
                                                                                                )
                                                                               ),
                                                                               
                                                                               conditionalPanel(condition = "input.surv_new_lnc !=0",
                                                                                                conditionalPanel(condition = "input.surv_lnc == 'km_lnc'",
                                                                                                                 selectInput("sep_lnc", label = h3("Split by"),
                                                                                                                             choices = list("Median" = "median_lnc",
                                                                                                                                            "Mean" = "mean_lnc",
                                                                                                                                            "First Quantile" = "1stQu_lnc",
                                                                                                                                            "Third Quantile" = "3rdQu_lnc"))),
                                                                                                
                                                                                                conditionalPanel(condition = "input.surv_lnc == 'km_lnc'",
                                                                                                                 conditionalPanel(condition = "input.sep_lnc == 'median_lnc'",
                                                                                                                                  selectInput("cancer_median_lnc",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SII_I_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SII_I_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-UCS_SII_I_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/mRNA/survival_data/TCGA-CESC_SII_I_lnc_surv_KMLR_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIII_II_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIII_II_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIII_II_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIII_II_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIII_II_lnc_surv_KMLR_median.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIV_III_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIV_III_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIV_III_lnc_surv_KMLR_median.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIV_III_mrna_lnc_KMLR_median.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_lnc == 'mean_lnc'",
                                                                                                                                  selectInput("cancer_mean_lnc",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SII_I_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SII_I_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SII_I_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SII_I_lnc_surv_KMLR_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIII_II_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIII_II_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIII_II_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIII_II_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIII_II_lnc_surv_KMLR_mean.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIV_III_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIV_III_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIV_III_lnc_surv_KMLR_mean.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIV_III_lnc_surv_KMLR_mean.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_lnc == '1stQu_lnc'",
                                                                                                                                  selectInput("cancer_1stQu_lnc",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SII_I_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SII_I_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SII_I_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SII_I_lnc_surv_KMLR_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIII_II_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIII_II_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIII_II_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIII_II_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIII_II_lnc_surv_KMLR_1stQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIV_III_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIV_III_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIV_III_lnc_surv_KMLR_1stQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIV_III_lnc_surv_KMLR_1stQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")),
                                                                                                                 
                                                                                                                 conditionalPanel(condition = "input.sep_lnc == '3rdQu_lnc'",
                                                                                                                                  selectInput("cancer_3rdQu_lnc",label = h3("Cancer Type"),
                                                                                                                                              choices = list("Stage I - Stage II" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SII_I_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SII_I_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SII_I_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                         "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SII_I_lnc_surv_KMLR_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage II - Stage III" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIII_II_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCEC_SIII_II_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-UCS_SIII_II_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-CESC_SIII_II_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-OV_SIII_II_lnc_surv_KMLR_3rdQu.rda"),
                                                                                                                                                             
                                                                                                                                                             "Stage III - Stage IV" = list("TCGA-BRCA" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCEC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-UCS" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-CESC" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_3rdQu.rda",
                                                                                                                                                                                           "TCGA-OV" = "Data/mRNA_lncRNA/Data/lncRNA/survival_data/TCGA-BRCA_SIV_III_lnc_surv_KMLR_3rdQu.rda")),
                                                                                                                                              selected = "Stage I - Stage II")))
                                                                               )
                                                                               
                                                              ),
                                                              tags$head(
                                                                HTML(
                                                                  "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count7', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
                                                                )
                                                              ),
                                                              textOutput("keepAlive7")
                                             )
                                             
                            )
                            
                          ),
                          mainPanel(
                            tabsetPanel(id = "tabs_lnc",
                                        tabPanel("Differential lncRNA analysis", value = 1,
                                                 conditionalPanel(condition = "input.stage_lnc == 'Normal vs Stage I'",
                                                                  DT::dataTableOutput("DEG_lnc")
                                                 ),
                                                 
                                                 conditionalPanel(condition = "input.stage_lnc == 'Between All stages of cancer'",
                                                                  DT::dataTableOutput("DEG_lnc_stage"), uiOutput("popup_lnc")
                                                 ),
                                                 conditionalPanel(condition = "input.stage_lnc == 'Normal vs Stage I'",
                                                                  conditionalPanel(condition = "input.plots_action_lnc != 0",
                                                                  conditionalPanel(condition = "input.plotsN_lnc == 'VolcanoPlot'",
                                                                                   "Volcano-Plot", highchartOutput("volcanoPlotN_lnc", height = "500px")
                                                                  ),
                                                                  
                                                                  conditionalPanel(condition = "input.plotsN_lnc == 'BoxPlot'",
                                                                                   "Violin Plot", plotOutput("vioPlotN_lnc")
                                                                  ))
                                                 ),
                                                 
                                                 conditionalPanel(condition = "input.stage_lnc == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.plots_action_lnc_stage != 0",
                                                                  conditionalPanel(condition = "input.plots_lnc == 'VolcanoPlot'",
                                                                                   "Volcano-Plot", highchartOutput("volcanoPlot_lnc", height = "500px")
                                                                  ),
                                                                  
                                                                  conditionalPanel(condition = "input.plots_lnc == 'BoxPlot'",
                                                                                   "Violin Plot", plotOutput("vioPlot_lnc")
                                                                  ))
                                                 ),
                                                 
                                                 conditionalPanel(condition = "input.stage_lnc == 'Between All stages of cancer'",
                                                                  conditionalPanel(condition = "input.surv_new_lnc !=0",
                                                                                   conditionalPanel(condition = "input.surv_lnc == 'coxph_lnc'",
                                                                                                    "Cox-Proportaional Hazard Model", DT::dataTableOutput("survival_cox_lnc")
                                                                                   ),
                                                                                   
                                                                                   conditionalPanel(condition = "input.surv_lnc == 'km_lnc'",
                                                                                                    "log-rank test", DT::dataTableOutput("survival_KM_lnc")
                                                                                   )
                                                                  )
                                                 )
                                        )
                            )
                            
                          )
                        )
                        
               ),
               tabPanel("Tutorial Page", value = 5,
                        mainPanel(includeHTML("tutorial_page.html"))
                        )
               
               )
    )





    
