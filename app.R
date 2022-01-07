rm(list = ls()) 
options(shiny.maxRequestSize=10000*1024^2)
#install_github("timelyportfolio/sweetalertR")

options(bitmapType='cairo')

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(htmlwidgets)
library(shinyRGL)
library(shinyalert)
library(shinyhelper)
library(rmarkdown)
library(RColorBrewer)
library(dashboardthemes)
library(survival)
library(survminer)
library(survMisc)
library(data.table)
library(MASS)
library(reshape2)
library(ggplot2)
library(plotly)
library(HDInterval)
library(yarrr)
library(plot.matrix)
library(forestplot)
library(GSVA)
library(genefilter)
library(igraph)
library(forcats)
library(png)
library(ggcorrplot)
library(plyr)
library(dplyr)
library(tidyr)
library(corrplot)
library(heatmaply)

source("modules/render_html_page_module.R")
source("functions/find_cutp.R")
source("functions/waterfallplot.R")
source("functions/generate_boxplot.R")
source("functions/callGSVA.R")
source("functions/callZSCORE.R")
source("functions/os_data_compute.R")
source("functions/cor_test.R")

userdatabase = "users/user_database.txt"
geneSymboldatabase = "data/geneSymbols.txt"

CSS <- "
/* CSS for the checkboxes */
  .pretty input[value=One]~.state label:after, 
  .pretty input[value=One]~.state label:before {
  background-color: red;
  }
"

# define js function for opening urls in new tab/window
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

datasets_URL <- "https://bbisr.shinyapps.winship.emory.edu/SurvivalGenie/Datasets.html"
programs_URL <- "https://bbisr.shinyapps.winship.emory.edu/SurvivalGenie/Programs.html"
roadmap_URL <- "https://bbisr.shinyapps.winship.emory.edu/SurvivalGenie/roadmap.png"

shinyApp(
  
  ui = #tags$body(class="sidebar-mini control-sidebar-open", 
  
    dashboardPagePlus(
      enable_preloader = TRUE,
      loading_duration = 2,
      
      
      header = dashboardHeaderPlus(
        
        #title = "SurvivalGenie",
        tags$li(class = "dropdown",
                tags$style(".main-header {max-height: 60px}"),
                tags$style(".main-header .logo {height: 60px}")
        ),
        title = span(
          tags$img(src="tool_logo.png", title = "SurvivalGenie", height = "100%", width="100%", style="padding-top:1px; padding-bottom:1px; padding-left:10px; padding-right:10px")),
        
        #tags$li(a(href = 'https://winshipcancer.emory.edu/bios/staff/dwivedi-bhakti.html',
        #          target = "_blank",
        #          img(src = 'myProfile.png',
        #              title = "Bhakti Dwivedi", height = "50px"),
        #          style = "padding-top:1px; padding-bottom:1px;"),
        #        class = "dropdown"),
        
        tags$li(a(href = 'https://winshipcancer.emory.edu/research/shared-resources/bioinformatics-and-systems-biology.html',
                  target = "_blank",
                  img(src = 'Winship_Comprehensive_WhiteLogo.png',
                      title = "BISB Winship Emory", height = "60px"),
                  style = "padding-top:1px; padding-bottom:1px;"),
                class = "dropdown"),
        
        tags$li(a(href = 'https://www.choa.org/medical-services/cancer-and-blood-disorders',
                  target = "_blank",
                  img(src = 'aflac_logo.png',
                      title = "AFLAC", height = "60px"),
                  style = "padding-top:1px; padding-bottom:1px;"),
                class = "dropdown"),
        
        tags$li(actionLink(inputId = "openPolicy", 
                           label = "", 
                           icon = icon("info")),
                class = "dropdown"),
        
        tags$li(actionLink(
          inputId = "openContact", 
          label = "", 
          icon = icon("envelope")),
          class = "dropdown"),
        
        fixed = FALSE,
        enable_rightsidebar = TRUE,
        rightSidebarIcon = "bars",
        
        left_menu = tagList(
          
          dropdownBlock(
            id = "program",
            title = 
              tags$span(
                tags$b("Select Program"),
                style="color: white; font-size:18px;"
              ),
            icon = "fas fa-layer-group",
            conditionalPanel(condition = "input.left_sidebar == 'surv_map'",
                             
                             
                             helper(
                               
                             awesomeRadio(
                               inputId = "checkprogram_map",
                               label = "Select cancer program",
                               program <- c("TCGA"),
                               status = "info",
                               selected = "TCGA"
                             ),
                             
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "programsHelp",
                             size = "l")
            ),
            conditionalPanel(condition = "input.left_sidebar == 'surv_sigs' | 
                           input.left_sidebar == 'surv_cls' |  
                           input.left_sidebar == 'surv_gene' |
                           input.left_sidebar == 'surv_ratio' |
                           input.left_sidebar == 'surv_tils' |  
                           input.left_sidebar == 'surv_tmb' |
                           input.left_sidebar == 'surv_gep'",
                             
                            helper(

                             awesomeRadio(
                               inputId = "checkprogram",
                               label = "Select cancer program",
                               program <- c("TCGA", "TARGET", "MMRF", "CPTAC", "NCICCR", "CGCI","WCDT", "OHSU", "ORGANOID", "CTSP", "HCMI"),
                               #choices <- c("TCGA","TARGET", "MMRF", "NCICCR", "CPTAC", "CGCI"),
                               status = "info",
                               selected = "TCGA"
                             ),
                             
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "programsHelp",
                             size = "l")
            )),
          
          dropdownBlock(
            id = "disease",
            title = 
              tags$span(
                tags$b("Select Dataset"),
                style="color: white; font-size:18px;"
              ),
            icon = "database",
            conditionalPanel(condition = "input.left_sidebar == 'surv_gene'",
                             
                          helper(
                             uiOutput(outputId = "checkdataset_gene", 
                                      label = "",
                                      value = NULL,
                                      width = "100px"
                             ),
                             
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "datasetsHelp",
                             size = "l")
            ),
            conditionalPanel(condition = "input.left_sidebar == 'surv_ratio'",
                          helper(
                             uiOutput(outputId = "checkdataset_ratio", 
                                      label = "",
                                      value = NULL,
                                      width = "100px"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "datasetsHelp",
                             size = "l")
            ),
            conditionalPanel(condition = "input.left_sidebar == 'surv_map'",
                          helper(
                             uiOutput(outputId = "checkdataset_for_MAP", 
                                      label = "",
                                      value = NULL,
                                      width = "100px"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "datasetsHelp",
                             size = "l"),
                          
                             tags$span(
                               tags$b("Digital quantification of TILs is only available for these 13 TCGA datasets"),
                               style="color: red; font=size:100%;"
                             )
            ),
            conditionalPanel(condition = "input.left_sidebar == 'surv_sigs' |
                           input.left_sidebar == 'surv_cls' | 
                           input.left_sidebar == 'surv_tils' | 
                           input.left_sidebar == 'surv_tmb' |
                           input.left_sidebar == 'surv_gep'",
                             
                           helper(
                             uiOutput(outputId = "checkdataset", 
                                      label = "",
                                      value = NULL,
                                      width = "100px"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "datasetsHelp",
                             size = "l")
            )
          ),
          dropdownBlock(
            id = "tumor",
            title = 
              tags$span(
                tags$b("Select Tumor"),
                style="color: white; font-size:18px;"
              ),
            icon = "users",
            conditionalPanel(condition = "input.left_sidebar != 'loginpage'",
                             
                          helper(
                             awesomeRadio(
                               inputId = "checktumor",
                               label = "Select tumor type",
                               choices = c("Primary" = "primary", "Recurrent" = "recurrent", "Metastatic" = "metastatic"),
                               status = "info"#,
                               #selected = "none"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "tumorHelp",
                             size = "l")
            )
          ),
          dropdownBlock(
            id = "groups",
            title = 
              tags$span(
                tags$b("Select Groups"),
                style="color: white; font-size:18px;"
              ),
            icon = "hourglass",
            conditionalPanel(condition = "input.left_sidebar != 'loginpage'",
                             
                        helper(
                             awesomeRadio(
                               inputId = "checkgroup",
                               label = "Define high/low groups using",
                               choices = list("Mean" = "mean", "Median" = "median", "Percentile" = "percentile", "Cutp"= "cutp"),
                               status = "info",
                               selected = "median"
                             ),
                          icon = "question-circle",
                          colour = "orange",
                          type = "markdown",
                          content = "groupHelp",
                          size = "l"),
                             
                             conditionalPanel(condition = "input.checkgroup == 'percentile'",
                                              
                                              sliderInput(inputId="upper_per",
                                                          label="upper",
                                                          value=0.75,
                                                          min=0.1, max=1.0),
                                              
                                              sliderInput(inputId="lower_per",
                                                          label="lower",
                                                          value=0.25,
                                                          min=0.1, max=1.0)
                                              
                             ),
                             conditionalPanel(condition = "input.checkgroup == 'cutp'",
                                              tags$span(
                                                tags$b("This option takes longer to finish. Do you still want to proceed?"),
                                                style="color: red; font=size:100%;"
                                              ),
                                              br(),
                                              tags$span(
                                                prettyCheckbox(
                                                  inputId = "checkgroup_cutp",
                                                  label = tags$b("YES!"), 
                                                  shape="square",
                                                  status = "danger",
                                                  icon = icon("check")
                                                ), style="color: blue; font=size:100%;"
                                              )
                                              
                             )
            )
          ),
          dropdownBlock(
            id = "surv",
            title = 
              tags$span(
                tags$b("Select Survival"),
                style="color: white; font-size:18px;"
              ),
            icon = icon("line-chart", class="topicon"),
            
            conditionalPanel(condition = "input.left_sidebar != 'loginpage'",
                             
                        helper(
                             awesomeRadio(
                               inputId = "checksurvival",
                               label = "Select Survival (Univariate)",
                               choices = c("Overall" = "Overall", "Event-Free" = "EventFree"), #, "RelapseFree" = "RelapseFree"),
                               status = "info",
                               selected = "Overall"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "survivalHelp",
                             size = "l"),
            
            
                             conditionalPanel(condition = "input.checksurvival == 'EventFree'",
                                              tags$span(
                                                tags$b("This option only works with TARGET datasets!"),
                                                style="color: red; font=size:100%;"
                                              )
                             )
                             
            )
          ),
          dropdownBlock(
            id = "cells",
            title = 
              tags$span(
                tags$b("Select TILs"),
                style="color: white; font-size:18px;"
              ),
            icon = icon("th", class="topicon"),
            
            conditionalPanel(condition = "input.left_sidebar != 'loginpage'",
                          helper(   
                             awesomeRadio(
                               inputId = "checktils",
                               label = "Select cell matrix", 
                               choices = c("LM22" = "LM22", "LM6" = "LM6"),
                               selected = "LM22",
                               status = "warning"
                             ),
                             icon = "question-circle",
                             colour = "orange",
                             type = "markdown",
                             content = "TILsHelp",
                             size = "l"),
                          
                             prettyToggle(
                               inputId = "checkpvalue",
                               label_on = "apply p<=0.05", 
                               label_off = "Keep all",
                               value=FALSE,
                               icon_on = icon("check"),
                               icon_off = icon("remove")
                               #status = "warning"
                             ),
                             
                             #tags$span(
                             #   prettyCheckbox(
                             #     inputId = "check_correlation",
                             #     label = tags$b("Correlate with cell proportions"),
                             #     value = TRUE,
                             #     status = "primary",
                             #     #icon = icon("check"),
                             #     #shape = "square",
                             #     fill = TRUE
                             #     #width = NULL
                             #   )#, style="color: white; font=size:100%;"
            )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_cls'",
                           actionBttn(
                             inputId = 'run_cls',
                             label = "RUN",
                             style = "fill",
                             size = 'm',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_sigs'",
                           actionBttn(
                             inputId = 'run_sigs',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_ratio'",
                           actionBttn(
                             inputId = 'run_ratio',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_gene'",
                           actionBttn(
                             inputId = 'run_gene',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_tils'",
                           actionBttn(
                             inputId = 'run_tils',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_map'",
                           actionBttn(
                             inputId = 'run_map',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_gep'",
                           actionBttn(
                             inputId = 'run_gep',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          ),
          conditionalPanel(condition = "input.left_sidebar == 'surv_tmb'",
                           actionBttn(
                             inputId = 'run_tmb',
                             label = "RUN",
                             style = "fill",
                             size = 'l',
                             color = "royal"
                           )
          )
          
        )
        
      ),
      
      collapse_sidebar = TRUE,
      rightsidebar = rightSidebar(
        #background = "dark"
        #uiOutput("side_bar"),
        #title = "Right Sidebar"
        tags$span(
          tags$b(uiOutput("counter")),
          style="color: white; font-size:18px; background-color: #ffffff;"),
        br(),
        br(),
        actionBttn(inputId = "roadmap", 
                            label = "Quick Start", 
                            style = "fill",
                            color = "danger",
                            size = "s")
      
      ),
      sidebar_fullCollapse = TRUE,
      
      dashboardSidebar(
        sidebarMenuOutput("menu")
      ),
      
      
      dashboardBody(
        
        tags$head(tags$style(HTML('

                                        /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #ffffff; #white
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #ffffff; # white
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: ##006bc2;
                                }   

                                /* navbar (rest of the header) */
                                 .skin-blue .control-sidebar-bg{
                                background-color: #4a4a43; #grey
                                 }    

                                /* navbar (rest of the header) */
                                 .skin-blue .control-sidebar{
                                background-color: #4a4a43; #grey
                                }    

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #4a4a43; #grey
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #FF6347; #tomato
                                color: #ffffff;
                                font-weight: bold;
                                }

                                /* other links in the sidebarmenu */
                                ##.skin-blue .main-sidebar .sidebar .sidebar-menu .sidebar-menuitem a{
                                ##background-color: rgb(255,125,125);
                                ###color: rgb(255,255,255)
                                ##}

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #ffffff;
                                }

                                .sidebar-menu li a { 
                                    font-size: 15px;
                                }

                                main-sidebar{ width: 300px;}")),

                                .topicon {color:#E87722})

                                ')),
                  tags$body(class="skin-blue sidebar-mini control-sidebar-open")),
        
        # use a bit of shinyEffects
        setShadow(class = "dropdown-menu"),
        
        useShinyjs(),
        extendShinyjs(text = js_code, functions = 'browseURL'),
        
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
        ##tags$script(src = "message-handler.js"),
        
        
        tabItems(
          tabItem(tabName= "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                  fluidRow(
                    column(12,
                           align="center",
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           #wellPanel(
                           h5(tags$em("Collected for tool analytics purposes only")),
                           tags$hr(),
                           textInput("email", "Email address", "", width = "250px", value = NULL),
                           textInput("company", "Company name", "", width = "250px", value= NULL),
                           useShinyalert(),
                           actionBttn(inputId = 'go',label = "Submit",
                                      style = "fill",
                                      color = "primary")
                    ))),
          tabItem(tabName="intro", 
                  fluidRow(
                      htmlOutput("introhtml"),
                  )),
          #tabItem(tabName="roadmap", 
          #        fluidRow(
          #          wellPanel(
          #            imageOutput("roadmap", height = "auto")
          #          )
          #        )),
          tabItem(tabName="manual", 
                  column(10,
                         #div(style="color:#337ab7; text-align:center;", titlePanel(h2("Step-by-step guide"))),
                         #br(),
                         tags$iframe(src = "Step-by-step-guide.pdf", width = 1500, height = 2000)
                  )
          ),
          
          tabItem(tabName="surv_tils", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution and Correlation amongst Immune Cell Types",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "switch_icc",
                          label = "Switch plots",
                          value = TRUE,
                          width = NULL
                        )
                      ),
                      br(),
                      conditionalPanel(condition = "!input.switch_icc",
                                       plotlyOutput("icc_corr_plot", height="600px", width="auto") %>% withSpinner(type=5)
                                       
                      ),
                      conditionalPanel(condition = "input.switch_icc",
                                       plotOutput("icc_plot", height="600px") %>% withSpinner(type=5)
                      )
                    ),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "tils_choice",
                          label="Switch plots",
                          value=TRUE,
                          width=NULL
                        )
                      ),
                      plotOutput("os_pval", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot", height="600px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          
          tabItem(tabName="surv_sigs", 
                  fluidRow(
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "gsva_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("wt_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_SIG", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_SIG",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_SIG", height="200px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_tmb", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "tmb_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("tmb_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_TMB",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      #sidebar_width = 10,
                      plotOutput("os_pval_TMB", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_TMB",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_TMB", height="200px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_map", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "map_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("map_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_MAP",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_MAP", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_MAP",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_MAP", height="200px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_gep", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "gep_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("gep_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_GEP",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader = FALSE,
                      status = "warning", 
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_GEP", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_GEP",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #),
                    #column(12,
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_GEP", height="200px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_gene", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "gene_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("GENE_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_GENE",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_GENE", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_GENE",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #column(12,
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_GENE", height="400px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_ratio", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "ratio_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("RATIO_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_RATIO",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_RATIO", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_RATIO",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #column(12,
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_RATIO", height="400px", width = "auto") %>% withSpinner(type=5)
                    )
                  )),
          tabItem(tabName="surv_cls", 
                  fluidRow(
                    #column(12,
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Distribution of High and Low Sample Groups",
                      closable = TRUE,
                      collapsible=TRUE,
                      enable_sidebar = TRUE,
                      #sidebar_width = 10,
                      sidebar_start_open = FALSE,
                      sidebar_content = tagList(
                        checkboxInput(
                          inputId = "cls_choice",
                          label="Switch plots",
                          value=FALSE,
                          width=NULL
                        )
                      ),
                      #verbatimTextOutput("text"),
                      plotOutput("CLS_plot", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Correlation with Immune Cell Lymphocytes Cell Signature",
                      plotOutput("CORR_plot_CLS",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #),
                    boxPlus(
                      width=6,
                      height='auto',
                      solidHeader=FALSE,
                      status="warning",
                      title="Kaplan-Meier Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("os_pval_CLS", height="600px") %>% withSpinner(type=5)
                    ),
                    boxPlus(
                      width=6,
                      height="auto",
                      solidHeader=FALSE,
                      status="warning",
                      closable = TRUE,
                      collapsible=TRUE,
                      title="Hazard-Ratio Plot",
                      plotlyOutput("hr_scp_CLS",height="600px", width="auto") %>% withSpinner(type=5)
                    ),
                    #column(12,
                    boxPlus(
                      width=12,
                      height="auto",
                      solidHeader = FALSE,
                      status = "warning", 
                      title = "Forest Plot",
                      closable = TRUE,
                      collapsible=TRUE,
                      plotOutput("hrplot_CLS", height="400px", width = "auto") %>% withSpinner(type=5)
                    )
                  ))
          #tabItem(tabName="surv_nab", 
          #        h3("Coming soon...")
          #)
          
        )),
      
      footer = dashboardFooter(
        left_text = "Please Cite Dwivedi, B. and Bhasin, M (2020). A web-based portal for single cell data, gene-ratio, and cell composition-based survival analyses [Unpublished]",
        right_text = "© Emory University"
      )
    ),
  server = function(input, output, session) {
    
    observeEvent(input$datasets,{
        js$browseURL(datasets_URL)
    })
    
    observeEvent(input$programs,{
        js$browseURL(programs_URL)
    })
    
    observeEvent(input$roadmap,{
      js$browseURL(roadmap_URL)
    })
  
    output$counter <- renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- 0
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("Visitor#: ", counter)
    })
    
    
    observe_helpers()
    
    onevent("mouseenter", "rightsidebarCollapsed", shinyjs::removeCssClass(selector = "body", class = "rightsidebar-collapse"))
    onevent("mouseleave", "rightsidebarCollapsed", shinyjs::addCssClass(selector = "body", class = "rightsidebar-collapse"))
    
    output$menu <- renderMenu({
      sidebarMenu(
        id="left_sidebar",
        menuItem("Welcome!", tabName = "loginpage", icon = icon("home"))
      )
    })
    
    observeEvent(input$go, {
      
      validate(
        need(input$email, paste0("Please provide valid input")),
        need(input$company, paste0("Please provide valid input")),
        need(trimws(input$email) != "", paste0("Please provide valid input")),
        need(trimws(input$company) != "", paste0("Please provide valid input"))
      )
      #session$sendCustomMessage(type = 'testmessage', message = 'Thank you!')
      shinyalert("Thank you!", type = "success")
      data <- paste0(Sys.Date(), ",", input$email, ",", input$company, "\n")
      cat(data, file=userdatabase, append=TRUE)
      
      isolate({updateTabItems(session, "left_sidebar", "intro")})
      
      output$menu <- renderMenu({
        sidebarMenu(
          id="left_sidebar",
          menuItem("About", tabName = "default", icon = icon("home"), startExpanded = TRUE,
                   #menuItem("Quick start", tabName = "roadmap", icon = icon("")),  
                   menuItem("Introduction", tabName = "intro", icon = icon("")), 
                   menuItem("Manual", tabName = "manual", icon = icon(""))),  
          menuItem("SELECT Analysis Type:", tabName = "Analysis", icon = icon("dashboard"), startExpanded = TRUE, 
                   menuItem("Gene-based:", tabName = "geneAnalysis", icon = icon("th"), startExpanded = FALSE, selected=TRUE,
                            menuItem("scRNA CLUSTERs", tabName = "surv_cls", icon = icon("")),
                            menuItem("GENE SETS", tabName = "surv_sigs", icon = icon("")),
                            menuItem("Genes RATIO", tabName = "surv_ratio", icon = icon("")),
                            menuItem("SINGLE Gene", tabName = "surv_gene", icon = icon(""))),
                   menuItem("Cell-based:", tabName = "cellAnalysis", icon = icon("gear"), startExpanded = FALSE,
                            menuItem("CIBERSOFT TILs", tabName = "surv_tils", icon = icon("")),
                            menuItem("DIGITAL TIL%", tabName = "surv_map", icon = icon(""))),
                   menuItem("Profile-based", tabName = "surv_gep", icon = icon("bar-chart")),
                   menuItem("Mutation-based:", tabName = "surv_tmb", icon = icon("bar-chart"))
          ),
          conditionalPanel("input.left_sidebar == 'surv_tils'",
                           boxPlus(
                             title = "Show output",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "success",
                             width = NULL,
                             id = "datasource",
                             uiOutput(outputId="cellSelector",
                                      label = "",
                                      value=NULL, width=NULL)
                           )
                           
          ),
          conditionalPanel("input.left_sidebar == 'surv_sigs'",
                           
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                             selectInput(inputId="sigs_choice", 
                                         label=tags$span(
                                           "Choose a gene set"
                                           ,style="color: black; font=size:100%;"),
                                         choices = list("Use Pre-Defined" = "LD", "Upload my own" = "UD"), 
                                         selectize = TRUE, 
                                         width=NULL),
                             conditionalPanel(condition = "input.sigs_choice == 'LD'",
                                              uiOutput(outputId = "checksiglist",
                                                       label = "",
                                                       value=NULL, width=NULL),
                                              uiOutput(outputId = "showsiglist",
                                                       label="",
                                                       value=NULL, width=NULL)
                             ),
                             conditionalPanel(condition = "input.sigs_choice == 'UD'",
                                              #fileInput(inputId="file", label="Upload your ", width=NULL, multiple=FALSE),
                                              helper(shiny::textAreaInput(inputId="user_geneSet",
                                                                          label=tags$span(
                                                                            "Enter your list of genes:"
                                                                            ,style="color: black; font=size:100%;"),
                                                                          #value = paste("For example, TP53", "EGFR", sep="\n"),
                                                                          width = NULL,
                                                                          height = "300px"),
                                                     icon = "question-circle",
                                                     colour = "orange",
                                                     type = "markdown",
                                                     content = "geneSetFormat",
                                                     size = "l"),
                                              
                                              textInput("geneSet", 
                                                        label=tags$span(
                                                          "Name your list"
                                                          ,style="color: black; font=size:100%;"),
                                                        value="user_list", width="200px"),
                                              actionBttn(
                                                inputId = 'check_user_geneSet',
                                                label = "Check my List",
                                                style = "fill",
                                                color = "warning"
                                              )
                             )
                           )
                           
          ),
          conditionalPanel("input.left_sidebar == 'surv_cls'",
                           
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                             helper(
                               selectInput(inputId="clsfiletype", 
                                           label=tags$span(
                                             "Choose a dataset:"
                                             ,style="color: black; font=size:100%;"),
                                           choices=c("Use Example data"="ED","Upload my own"="LD", "Provide File URL"="URL"), selectize = TRUE, width=NULL),
                               icon = "question-circle",
                               colour = "orange",
                               type = "markdown",
                               content = "clusterFormat",
                               size = "l"),
                               
                             conditionalPanel(condition = "input.clsfiletype == 'URL'",
                              				 textInput("file_url", 
                                                        label=tags$span(
                                                        "Type the scWizard URL"
                                                        ,style="color: black; font=size:100%;"),
                                                        placeholder="e.g., https://bhasinlab.bmi.emory.edu/scwizard/scw_out/1623434401_heart0609.top10.markers.csv", value="", width="200px"),
                                             actionBttn(
                                                inputId = 'check_url_geneSet',
                                                label = "Check genes",
                                                style = "fill",
                                                color = "warning"
                                             )
                                             
                             ),
                             
                             conditionalPanel("input.clsfiletype=='LD'",                                  
                                              shiny::fileInput(inputId="clsfile", 
                                                               label=tags$span(
                                                                 "Upload a '.txt' file"
                                                                 ,style="color: black; font=size:100%;"),
                                                               width=NULL, multiple=FALSE),
                                              actionBttn(
                                                inputId = 'check_cluster_genes',
                                                label = "Check genes",
                                                style = "fill",
                                                color = "warning"
                                              ))),
                                            boxPlus(
                                              title = "Show output",
                                              solidHeader = TRUE,
                                              collapsible = TRUE,
                                              #background="grey"
                                              status = "success",
                                              width = NULL,
                                              id = "datasource",
                                              
                                              uiOutput(outputId = "clusterSelector", 
                                                       label=tags$span(
                                                         "Select your cluster"
                                                         ,style="color: black; font=size:100%;"),
                                                       value = NULL,
                                                       width = "100px"
                                              ))
          ),
          conditionalPanel("input.left_sidebar == 'surv_gene'",
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                             helper(shiny::textInput(inputId="user_gene", 
                                                     label=tags$span(
                                                       "Input your gene",
                                                       style="color: black; font=size:100%;"),
                                                     #value = paste("For example, TP53", "TSPAN6", sep="\n"),
                                                     width = "200px",
                                                     placeholder = "e.g., TP53"),
                                    #helper(shiny::actionButton("go", "click me!"),
                                    icon = "question-circle",
                                    colour = "orange",
                                    type = "markdown",
                                    content = "geneFormat",
                                    size = "l") ,
                             actionBttn(
                               inputId = 'check_user_gene',
                               label = "Check my gene",
                               style = "fill",
                               color = "warning"
                             )
                           ),
                           boxPlus(
                             title = "Show output",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "success",
                             width = NULL,
                             id = "datasource",
                             
                             uiOutput(outputId = "diseaseSelector", 
                                      value = NULL,
                                      width = "100px"
                             ))
          ),
          conditionalPanel("input.left_sidebar == 'surv_ratio'",
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                             helper(shiny::textInput(inputId="user_geneA", 
                                                     label=tags$span(
                                                       "Input your gene A",
                                                       style="color: black; font=size:100%;"),
                                                     #value = paste("For example, TP53", "TSPAN6", sep="\n"),
                                                     width = "200px",
                                                     placeholder = "e.g., TP53"),
                                    #helper(shiny::actionButton("go", "click me!"),
                                    icon = "question-circle",
                                    colour = "orange",
                                    type = "markdown",
                                    content = "geneRatioFormat",
                                    size = "l") ,
                             textInput(inputId="user_geneB", 
                                       label=tags$span(
                                         "Input your gene B",
                                         style="color: black; font=size:100%;"),
                                       #value = paste("For example, TP53", "TSPAN6", sep="\n"),
                                       width = "200px",
                                       placeholder = "e.g., EGFR"),
                             actionBttn(
                               inputId = 'check_user_ratio',
                               label = "Check my genes",
                               style = "fill",
                               color = "warning"
                             )),
                           
                           boxPlus(
                             title = "Show output",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "success",
                             width = NULL,
                             id = "datasource",
                             
                             uiOutput(outputId = "diseaseSelector_ratio", 
                                      value = NULL,
                                      width = "100px"
                             ))
                           
          ),
          conditionalPanel("input.left_sidebar == 'surv_tmb'",
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                          helper(
                            
                            radioButtons(
                               inputId = "mutation_type",
                               label=tags$span(
                                 "Select TMB estimate:",
                                 style="color: black; font=size:100%;"),
                               choiceNames = list(
                                 tags$span(style="color: black; font=size:100%;","Non-synonymous"),
                                 tags$span(style="color: black; font=size:100%;","Exonic"),
                                 tags$span(style="color: black; font=size:100%;","All mutations")
                               ),
                               choiceValues = c("nonsynonymous_TMB","exonic_TMB","raw_TMB"),
                               selected = "nonsynonymous_TMB"
                             ),
                            icon = "question-circle",
                            colour = "orange",
                            type = "markdown",
                            content = "tmbInputHelp",
                            size = "l")
                           )
          ),
          conditionalPanel("input.left_sidebar == 'surv_map'",
                           boxPlus(
                             title = "Show output",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "success",
                             width = NULL,
                             id = "datasource",
                             
                             selectInput(inputId="tils_per", 
                                         label=tags$span(
                                           "Choose digital measure"
                                           ,style="color: black; font=size:100%;"),
                                         choices=c("TIL%"="TP"), selectize = TRUE, width=NULL)
                           )
          ),
          conditionalPanel("input.left_sidebar == 'surv_gep'",
                           
                           boxPlus(
                             title = "Select Input",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             #background="grey"
                             status = "warning",
                             width = NULL,
                             id = "datasource",
                             
                             helper(
                               selectInput(inputId="gepfiletype", 
                                           label=tags$span(
                                             "Choose a data file"
                                             ,style="color: black; font=size:100%;"),
                                           choices=c("Use Example GEP"="ED","Upload my own"="LD"), selectize = TRUE, width=NULL),
                               icon = "question-circle",
                               colour = "orange",
                               type = "markdown",
                               content = "gepFormat",
                               size = "l"),
                             
                             conditionalPanel("input.gepfiletype=='LD'",    
                                              
                                              shiny::fileInput(inputId="gepfile", 
                                                               label=tags$span(
                                                                 "Upload a '.txt' file"
                                                                 ,style="color: black; font=size:100%;"),
                                                               width=NULL, multiple=FALSE),
                                              actionBttn(
                                                inputId = 'check_gep_genes',
                                                label = "Check genes",
                                                style = "fill",
                                                color = "warning"
                                              )
                             )
                           )
          ))
        
      })
    })
    
    output$picture <- renderImage({
      return(list(src = "www/overview.png",contentType = "image/png",alt = "Overview", style="display: block; margin-left: auto; margin-right: auto;"))
    }, deleteFile = FALSE) #where the src is wherever you have the picture
    
    output$roadmap <- renderImage({
      return(list(src = "www/roadmap.png",contentType = "image/png",alt = "Roadmap", style="display: block; margin-left: auto; margin-right: auto;"))
    }, deleteFile = FALSE) #where the src is wherever you have the picture
    
    observeEvent(input$openPolicy, {
      showModal(
        modalDialog(title = 
                      tags$span(
                        tags$b(h2("Publication Acknowledgement Policy")), 
                        style="color: blue; font-size:18px;"),
                    h3("The National Cancer Institute requires that publications acknowledge the Winship Cancer Institute Cancer Center Support Grant (CCSG), and it is tracking compliance. If a Winship Cancer Institute CCSG-supported Shared Resource provided data, designed the study, performed analyses, provided results used in your publication, and/or provided any systems or services that were used for the work that resulted in your publication, please include the following statement in the acknowledgment section of your publication(s):"),
                    tags$i(tags$b(h3("Research reported in this publication was supported in part by the Biostatistics and Systems Biology shared resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health."))),
                    easyClose = TRUE,
                    size = "l",
                    footer = modalButton(h3("OK"))
        )
      )
    })
    
    observeEvent(input$openContact, {
      showModal(
        modalDialog(title = 
                      tags$span(
                        tags$b(h2("Contact us")), 
                        style="color: blue; font-size:18px;"),
                    
                    tags$span(h3("Please post your questions and issues",
                                 tags$a(href ="https://github.com/BhaktiDwivedi/SurvivalGenie/issues", "here"))),
                    easyClose = TRUE,
                    size = "l",
                    footer = modalButton(h3("OK"))
        ))
    })
    
    
    getCellNames <- read.table(paste0("data/cellTypes_names.txt"), sep="\t", header=T)
    
    
    getSummary <- reactive({
      stat <- paste0("program_stat.txt")
      readFile <- paste0("data/",stat)
      p_stat <- read.table(readFile, sep="\t", header=T)
    })
    
    output$pg_plot <- renderPlotly ({
      req(getSummary())
      p_stat = getSummary()
      
      p_stat$Program <- factor(p_stat$Program, levels = p_stat$Program[order(p_stat$Datasets)])
      
      bp <- ggplot(data=p_stat,  aes(x=Program, y=Datasets)) +
        geom_bar(stat="identity") +
        #scale_y_continuous(labels=p_stat$Number) + 
        #theme(axis.title.y = element_text(colour="black", face="bold", size=20.0)) + 
        #theme(axis.title.x = element_text(colour="black", face="italic", size=14.0)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12.0)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=12.0)) + 
        ylab("\nNumber of Datasets\n") + 
        xlab("") + 
        labs(fill = "") + 
        coord_flip() +
        theme(legend.position = "none")
      
      bp <- ggplotly(bp, tooltip=c("Program", "Datasets"), height=6, width=6)
      
      saveWidget(bp, "Programs.html")
    })
    
    getDisSummary <- function (){
      stat <- paste0("summary.txt")
      readFile <- paste0("data/",stat)
      p_stat <- read.table(readFile, sep="\t", header=T)
      primary <- p_stat$Dataset[p_stat$Primary<20]
      recurrent <- p_stat$Dataset[p_stat$Recurrent<20]
      metastatic <- p_stat$Dataset[p_stat$Metastatic<20]
      other_tag <- p_stat$Dataset[p_stat$clinical_OS_tag>0] ## this line can be removed later
      data = list(p_stat=p_stat, primary=primary, recurrent=recurrent, metastatic=metastatic, other_tag=other_tag)
    }
    
    output$ds_plot <- renderPlotly ({
      req(getDisSummary()$p_stat)
      p_stat = getDisSummary()$p_stat
      p_stat$Dataset<- factor(p_stat$Dataset, levels = rev(p_stat$Dataset[order(p_stat$Clinical.Survival)]))
      p_stat <- p_stat[,c(2,5:7)]
      
      dt <- melt(p_stat)
      dt <- dt %>%
        arrange(desc(value)) %>%
        mutate(text = paste0(Dataset, "\n", variable, ": ", value, "\n"))
      
      myColors <- brewer.pal(3,"Set1")
      #myColors <- c("lightcoral", "coral3", "darkred")
      #myColors <- c("grey80", "grey70", "grey60") 
      #myColors <- c("#A4D3EE", "#B0E2FF", "#9BC4E2") 
      
      bp <- ggplot(data=dt,  aes(x=Dataset, y=value, fill=variable, text=text)) +
        geom_bar(stat="identity") +
        scale_fill_manual(values = myColors) + 
        #scale_y_continuous(labels=p_stat$Number) + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=12.0),
              axis.title.x = element_text(colour="black", face="italic", size=14.0),
              panel.background = element_rect(fill="NA"),
              panel.border = element_rect(colour = "black", fill="NA"),
              panel.grid.major.y = element_line(colour="NA"),
              panel.grid.minor = element_line(colour = "NA"),
              axis.text.x = element_text(hjust = 0.5, vjust = 0.5, angle=90, colour="black", face="bold", size=10.0),
              axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", size=11.0),
              legend.position = c(0.95,0.95),
              legend.justification = c("right", "top"),
              legend.box.just = "right",
              legend.margin = margin(6, 6, 6, 6)
              #coord_flip()
        ) + 
        ylab("No. of patient samples") + 
        xlab("") + 
        labs(fill = "")
      #coord_flip()
      
      bp <- ggplotly(bp, tooltip="text", height=700, width=1000) %>% 
        layout(legend = list(orientation = "v",
                             x=0.8,
                             y=1.0))
      
      saveWidget(bp, "Datasets.html")
      
    })
    
    #callModule(htmlServer, "programs", path = "Programs.html")
    #callModule(htmlServer, "datasets", path = "Datasets.html")
    
    output$introhtml <- renderUI({
      tags$iframe(src = "about.html", height=1000, width=1500)
      #print(myintro)
    })
    
    ##List of options output
    output$program <- renderUI({
      program <- c("TCGA", "TARGET", "MMRF", "CPTAC", "BEATAML1.0", "NCICCR", "CGCI","WCDT", "OHSU", "ORGANOID", "CTSP", "HCMI")
      selectizeInput(inputId = "checkprogram", "Select a program", as.list(program), options=list(maxOptions=length(program)))
    })
    
    output$checkdataset <- renderUI({
      TCGA <- c("TCGA-ACC","TCGA-BLCA","TCGA-BRCA (Breast Cancer)"="TCGA-BRCA","TCGA-CESC","TCGA-CHOL","TCGA-COAD","TCGA-DLBC","TCGA-ESCA","TCGA-GBM","TCGA-HNSC","TCGA-KICH","TCGA-KIRC","TCGA-KIRP","TCGA-LAML","TCGA-LGG","TCGA-LIHC","TCGA-LUAD","TCGA-LUSC","TCGA-MESO","TCGA-OV","TCGA-PAAD","TCGA-PCPG","TCGA-PRAD","TCGA-READ","TCGA-SARC","TCGA-SKCM","TCGA-STAD","TCGA-TGCT","TCGA-THCA","TCGA-THYM","TCGA-UCEC","TCGA-UCS","TCGA-UVM")
      TARGET <- c("TARGET-ALL-Phase 1 (B-ALL)"="TARGET-ALL-P1","TARGET-ALL-Phase 2"="TARGET-ALL-P2", "TARGET-ALL-Phase 2 (B-ALL)"="TARGET-ALL-P2-Bcell","TARGET-ALL-Phase 2 (T-ALL)"="TARGET-ALL-P2-Tcell", "TARGET-ALL-Phase 3 (ALAL)"="TARGET-ALL-P3","TARGET-AML","TARGET-CCSK","TARGET-NBL","TARGET-OS","TARGET-RT","TARGET-WT")
      MMRF <- c("MMRF-COMMPASS")
      CPTAC <- c("CPTAC-2","CPTAC-3")
      #BEATAML1.0 <- c("BEATAML1.0")
      NCICCR <- c("NCICCR-DLBCL")
      CGCI <- c("CGCI-BLGSP","CGCI-HTMCP-CC")
      #CMI <- c("CMI-MBC","CMI-ASC") ## recently made available Aug 2020; have to download
      WCDT <- c("WCDT-MCRPC")
      OHSU <- c("OHSU-CNL")
      ORGANOID <- c("ORGANOID-PANCREATIC")
      CTSP <- c("CTSP-DLBCL1")
      HCMI <- c("HCMI-CMDC")
      disease <- c(TCGA, TARGET, MMRF, CPTAC, NCICCR, CGCI, WCDT, OHSU, ORGANOID, CTSP, HCMI)
      selectizeInput(inputId = "checkdataset", "Select cancer dataset", 
                     as.list(get(input$checkprogram)), options=list(maxOptions=length(get(input$checkprogram))), 
                     multiple=FALSE)
    })
    
    output$checkdataset_gene <- renderUI({
      TCGA <- c("TCGA-ACC","TCGA-BLCA","TCGA-BRCA (Breast Cancer)"="TCGA-BRCA","TCGA-CESC","TCGA-CHOL","TCGA-COAD","TCGA-DLBC","TCGA-ESCA","TCGA-GBM","TCGA-HNSC","TCGA-KICH","TCGA-KIRC","TCGA-KIRP","TCGA-LAML","TCGA-LGG","TCGA-LIHC","TCGA-LUAD","TCGA-LUSC","TCGA-MESO","TCGA-OV","TCGA-PAAD","TCGA-PCPG","TCGA-PRAD","TCGA-READ","TCGA-SARC","TCGA-SKCM","TCGA-STAD","TCGA-TGCT","TCGA-THCA","TCGA-THYM","TCGA-UCEC","TCGA-UCS","TCGA-UVM")
      TARGET <- c("TARGET-ALL-Phase 1 (B-ALL)"="TARGET-ALL-P1","TARGET-ALL-Phase 2"="TARGET-ALL-P2", "TARGET-ALL-Phase 2 (B-ALL)"="TARGET-ALL-P2-Bcell","TARGET-ALL-Phase 2 (T-ALL)"="TARGET-ALL-P2-Tcell", "TARGET-ALL-Phase 3 (ALAL)"="TARGET-ALL-P3","TARGET-AML","TARGET-CCSK","TARGET-NBL","TARGET-OS","TARGET-RT","TARGET-WT")
      MMRF <- c("MMRF-COMMPASS")
      CPTAC <- c("CPTAC-2","CPTAC-3")
      #BEATAML1.0 <- c("BEATAML1.0")
      NCICCR <- c("NCICCR-DLBCL")
      CGCI <- c("CGCI-BLGSP","CGCI-HTMCP-CC")
      #CMI <- c("CMI-MBC","CMI-ASC") ## recently made available Aug 2020; have to download
      WCDT <- c("WCDT-MCRPC")
      OHSU <- c("OHSU-CNL")
      ORGANOID <- c("ORGANOID-PANCREATIC")
      CTSP <- c("CTSP-DLBCL1")
      HCMI <- c("HCMI-CMDC")
      disease <- c(TCGA, TARGET, MMRF, CPTAC, NCICCR, CGCI, WCDT, OHSU, ORGANOID, CTSP, HCMI)
      selectizeInput(inputId = "checkdataset_gene", "Select cancer dataset", 
                     as.list(get(input$checkprogram)), options=list(maxOptions=length(get(input$checkprogram))), 
                     multiple=TRUE) 
      
    })
    
    output$checkdataset_ratio <- renderUI({
      TCGA <- c("TCGA-ACC","TCGA-BLCA","TCGA-BRCA (Breast Cancer)"="TCGA-BRCA","TCGA-CESC","TCGA-CHOL","TCGA-COAD","TCGA-DLBC","TCGA-ESCA","TCGA-GBM","TCGA-HNSC","TCGA-KICH","TCGA-KIRC","TCGA-KIRP","TCGA-LAML","TCGA-LGG","TCGA-LIHC","TCGA-LUAD","TCGA-LUSC","TCGA-MESO","TCGA-OV","TCGA-PAAD","TCGA-PCPG","TCGA-PRAD","TCGA-READ","TCGA-SARC","TCGA-SKCM","TCGA-STAD","TCGA-TGCT","TCGA-THCA","TCGA-THYM","TCGA-UCEC","TCGA-UCS","TCGA-UVM")
      TARGET <- c("TARGET-ALL-Phase 1 (B-ALL)"="TARGET-ALL-P1","TARGET-ALL-Phase 2"="TARGET-ALL-P2", "TARGET-ALL-Phase 2 (B-ALL)"="TARGET-ALL-P2-Bcell","TARGET-ALL-Phase 2 (T-ALL)"="TARGET-ALL-P2-Tcell", "TARGET-ALL-Phase 3 (ALAL)"="TARGET-ALL-P3","TARGET-AML","TARGET-CCSK","TARGET-NBL","TARGET-OS","TARGET-RT","TARGET-WT")
      MMRF <- c("MMRF-COMMPASS")
      CPTAC <- c("CPTAC-2","CPTAC-3")
      #BEATAML1.0 <- c("BEATAML1.0")
      NCICCR <- c("NCICCR-DLBCL")
      CGCI <- c("CGCI-BLGSP","CGCI-HTMCP-CC")
      #CMI <- c("CMI-MBC","CMI-ASC") ## recently made available Aug 2020; have to download
      WCDT <- c("WCDT-MCRPC")
      OHSU <- c("OHSU-CNL")
      ORGANOID <- c("ORGANOID-PANCREATIC")
      CTSP <- c("CTSP-DLBCL1")
      HCMI <- c("HCMI-CMDC")
      disease <- c(TCGA, TARGET, MMRF, CPTAC, NCICCR, CGCI, WCDT, OHSU, ORGANOID, CTSP, HCMI)
      selectizeInput(inputId = "checkdataset_ratio", "Select cancer dataset", 
                     as.list(get(input$checkprogram)), options=list(maxOptions=length(get(input$checkprogram))), 
                     multiple=TRUE) 
      
    })
    
    output$checkdataset_for_MAP <- renderUI({
      disease <- c("TCGA-BLCA", 
                   "TCGA-BRCA (Breast Cancer)"="TCGA-BRCA",
                   "TCGA-CESC", 
                   "TCGA-COAD",
                   "TCGA-LUAD",
                   "TCGA-LUSC",
                   "TCGA-PAAD", 
                   "TCGA-READ", 
                   "TCGA-SKCM", 
                   "TCGA-STAD", 
                   "TCGA-UCEC", 
                   "TCGA-UVM")
      selectizeInput(inputId = "checkdataset_for_MAP", "Select a dataset for TIL%:", as.list(disease), options=list(maxOptions=length(disease)), multiple=FALSE) 
    })
    
    output$cellSelector <- renderUI({
      if(input$checktils == "LM22"){
        celltypes <-  c("B.cells.naive",
                        "B.cells.memory",
                        "Plasma.cells",
                        "T.cells.CD8",
                        "T.cells.CD4.naive",
                        "T.cells.CD4.memory.resting",
                        "T.cells.CD4.memory.activated",
                        "T.cells.follicular.helper",
                        "T.cells.regulatory..Tregs.",
                        "T.cells.gamma.delta",
                        "NK.cells.resting",
                        "NK.cells.activated",
                        "Monocytes",
                        "Macrophages.M0",
                        "Macrophages.M1",
                        "Macrophages.M2",
                        "Dendritic.cells.resting",
                        "Dendritic.cells.activated",
                        "Mast.cells.resting",
                        "Mast.cells.activated",
                        "Eosinophils",
                        "Neutrophils")
      } else {
        celltypes <-  c("B.cells",
                        "CD8.T.cells",
                        "CD4.T.cells",
                        "NK.cells",
                        "Monocytes",
                        "Neutrophils")
      }
      selectizeInput(inputId = "cellSelector", 
                     label=tags$span(
                       "Choose a Cell Type"
                       ,style="color: black; font=size:100%;"),
                     as.list(celltypes), options=list(maxOptions=length(celltypes))) 
    })
    
    output$checksiglist <- renderUI({
      signature <- c("Bcell","CD4_naive","CD4_T","CD8_naive","CD8_T","Central_memory","Cytotoxic","DC","Effector_memory","Exhausted","Gamma_delta","iTreg","Macrophage","MAIT","Monocyte","Neutrophil","NK","NKT","nTreg","Tfh","Th1","Th2","Th17","Tr1",
                     "Glycolytic", "FattyAcidOxidation",
                     "Angiogenesis", 
                     "Cell_Cycle_Control", 
                     "Cell_Death_Regulation_Signaling", 
                     "DNA_Damage", "Down-regulated_by_androgen", 
                     "epithelial_ovarian_cancer_associated_oncogenes", 
                     "Folate_Transport", "Growth_Proliferation_Signaling", 
                     "Invasion_and_Metastasis", 
                     "Notch_Signaling", 
                     "p53_Signaling", 
                     "PI3K-AKT-mTOR_Signaling", 
                     "Ras-Raf-MEK-Erk-JNK_Signaling", 
                     "RB_Pathway",
                     "Regulation_of_ribosomal_protein_synthesis_cell_growth", 
                     "RTK-RAS-PI3K-AKT_Signaling", 
                     "RTK_Signaling_family", 
                     "Telomere_maintainence", 
                     "TGF-beta_Pathway", 
                     "TP53_Pathway", 
                     "Tumor-suppressor_genes_epithelial_ovarian_cancer",
                     "AR_and_steriod_synthesis_enzymes", 
                     "Prostate_Cancer-AR Signaling", 
                     "Steroid_inactivating_genes")
      selectizeInput(inputId = "checksiglist", 
                     label=tags$span(
                       "Pre-Defined Sets:"
                       ,style="color: black; font=size:100%;"),
                     as.list(signature), options=list(maxOptions=length(signature))) 
    })
    
    observeEvent(input$run_tils, {
      
      if(length(input$checkdataset) == 0){
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
        shinyalert('Please Select a Dataset!', type = "error")
        
      } else {
        
        update$run_tils <- input$run_tils
        update$checkprogram <- input$checkprogram
        update$checkdataset <- input$checkdataset
        update$checktumor <- input$checktumor
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        #update$cellSelector <- input$cellSelector
        #update$tils_choice <- input$tils_choice
        update$checkgroup <- input$checkgroup
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$checksurvival <- input$checksurvival
        
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_tils = NULL
            #session$sendCustomMessage(type = 'testmessage', message = 'Event-Free survival is available for TARGET data')
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
          }
        }
        
        if(update$checktumor == "primary"){
          if(update$checkdataset %in% getDisSummary()$primary | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tils = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "recurrent"){
          if(update$checkdataset %in% getDisSummary()$recurrent | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tils = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "metastatic"){
          if(update$checkdataset %in% getDisSummary()$metastatic | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tils = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        
      }
      
    })
    
    observeEvent(input$checksiglist, {
      
      output$showsiglist <- renderUI({
        readSIGFile <- paste0("data/SIG_SETS/", input$checksiglist, ".txt")
        sig_list <-  read.table(readSIGFile, header=F, sep="\t")
        req(sig_list)
        sig_genes <- as.character(sig_list[,1])
        
        selectizeInput(inputId = "showsiglist", 
                       label=tags$span(
                         "Genes in selected set"
                         ,style="color: black; font=size:100%;"),
                       as.list(sig_genes), options=list(maxOptions=length(sig_genes))) 
        
      })
    })
    
    observeEvent(input$check_user_geneSet, {
      
      req(input$user_geneSet)
      
      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      user_list <-  unlist(strsplit(x = input$user_geneSet,split = '[\r\n]' ))
      
      check_tag <- toupper(user_list) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Failed', paste0('Check ', user_list[!check_tag]), type = "error")
      } else {
        shinyalert('Success', 'All genes identified!', type = "success")
      }
      
    })

    observeEvent(input$check_user_gene, {
      
      req(input$user_gene)
      
      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      user_list <-  unlist(strsplit(x = input$user_gene,split = '[\r\n]' ))
      
      check_tag <- toupper(user_list) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Failed', 'Check format or try an alias name', type = "error")
      } else {
        shinyalert('Success', 'Gene identified!', type = "success")
      }
    })
    
    observeEvent(input$check_user_ratio, {
      
      req(input$user_geneA)
      req(input$user_geneB)
      
      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      user_listA <-  unlist(strsplit(x = input$user_geneA,split = '[\r\n]' ))
      user_listB <-  unlist(strsplit(x = input$user_geneB,split = '[\r\n]' ))
      user_list <- c(user_listA, user_listB)
      
      check_tag <- toupper(user_list) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Failed', 'Check format or try an alias name', type = "error")
      } else {
        shinyalert('Success', 'Gene identified!', type = "success")
      }
    })
    
    
    observeEvent(input$check_cluster_genes, {
      
      req(input$clsfile$datapath)
      dataFile <- input$clsfile$datapath
      if (file.exists( isolate({dataFile}) ) ){
      }
      else{
        shinyalert("Unable to open the file!", type="error")
        stop()
      }
      
      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
      req(sig_list)
      sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
      
      cluster_genes <- unique(as.character(sig_list$gene))
      check_tag <- toupper(cluster_genes) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Missing', paste0(length(cluster_genes[!check_tag]), " genes"), type = "error")
      } else {
        shinyalert('Success', 'All genes identified!', type = "success")
      }
      
    })
    
     observeEvent(input$check_url_geneSet, {
      
      req(input$file_url)

      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      sig_list <-  read.csv(input$file_url)
      req(sig_list)
      sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
      
      cluster_genes <- unique(as.character(sig_list$gene))
      check_tag <- toupper(cluster_genes) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Missing', paste0(length(cluster_genes[!check_tag]), " genes"), type = "error")
      } else {
        shinyalert('Success', 'All genes identified!', type = "success")
      }
      
    })
    
    observeEvent(input$check_gep_genes, {
      
      req(input$gepfile$datapath)
      dataFile <- input$gepfile$datapath
      if (file.exists( isolate({dataFile}) ) ){
      }
      else{
        shinyalert("Unable to open the file!", type="error")
        stop()
      }
      
      read_symbols <- fread(geneSymboldatabase, header=T, sep="\t")
      sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
      gep_genes <- unique(as.character(sig_list$gene))
      check_tag <- toupper(gep_genes) %in% toupper(read_symbols[[1]])
      
      if(FALSE %in% check_tag){
        shinyalert('Missing', paste0(length(gep_genes[!check_tag]), " genes"), type = "error")
      } else {
        shinyalert('Success', 'All genes identified!', type = "success")
      }
      
    })
    
    observeEvent(input$run_sigs, {
      
      if(length(input$checkdataset) == 0){
        shinyalert('Please select a dataset!', type = "error")
        #session$sendCustomMessage(type = 'testmessage', message = 'Please select a dataset!')
      } else {
        
        update$run_sigs <- input$run_sigs
        update$checkprogram <- input$checkprogram
        update$checkdataset <- input$checkdataset
        update$checktumor <- input$checktumor
        #update$checktils_for_sigs <- input$checktils_for_sigs
        #update$checkpvalue_for_sigs <- input$checkpvalue_for_sigs
        update$sigs_choice <- input$sigs_choice
        update$checksiglist <- input$checksiglist
        update$user_geneSet <- input$user_geneSet
        update$geneSet <- input$geneSet
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_sigs = NULL
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
          }
        }
        
        if(update$checktumor == "primary"){
          if(update$checkdataset %in% getDisSummary()$primary | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_sigs = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "recurrent"){
          if(update$checkdataset %in% getDisSummary()$recurrent | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_sigs = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "metastatic"){
          if(update$checkdataset %in% getDisSummary()$metastatic | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_sigs = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
      }
      
    })
    
    observeEvent(input$run_cls, {
      
      if(length(input$checkdataset) == 0){
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
        shinyalert('Please Select a Dataset!', type = "error")
        
      } else {
        
        update$run_cls <- input$run_cls
        update$checkdataset <- input$checkdataset
        update$checkprogram <- input$checkprogram
        update$checktumor <- input$checktumor
        update$clsfiletype <- input$clsfiletype
        update$clsfile$datapath <- input$clsfile$datapath
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        #update$clusterSelector <- input$clusterSelector
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_cls = NULL
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
          }
        }
        
        otag <- update$checkdataset %in% getDisSummary()$other_tag
        if(TRUE %in% otag){
          update$run_cls = NULL
          shinyalert('Not enough samples',paste0('remove ', update$checkdataset[otag]), type = "error")
        }
        
        if(update$checktumor == "primary"){
          ptag <- update$checkdataset %in% getDisSummary()$primary
          if(TRUE %in% ptag){
            update$run_cls = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset[ptag]), type = "error")
          }
        }
        if(update$checktumor == "recurrent"){
          rtag <- update$checkdataset %in% getDisSummary()$recurrent
          if(TRUE %in% rtag){
            update$run_cls = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset[rtag]), type = "error")
          }
        }
        if(update$checktumor == "metastatic"){
          mtag <- update$checkdataset %in% getDisSummary()$metastatic
          if(TRUE %in% mtag){
            update$run_cls = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset[mtag]), type = "error")
          }
        }
        
      }
      
    })
    
    observeEvent(input$run_gene, {
      
      if(length(input$checkdataset_gene) == 0){
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
        shinyalert('Please Select a Dataset!', type = "error")
        
      } else {
        
        update$run_gene <- input$run_gene
        update$checkprogram <- input$checkprogram
        update$checkdataset_gene <- input$checkdataset_gene
        update$checktumor <- input$checktumor
        update$user_gene <- input$user_gene
        update$diseaseSelector <- input$diseaseSelector
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_gene = NULL
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
          }
        }
        
        otag <- update$checkdataset_gene %in% getDisSummary()$other_tag
        if(TRUE %in% otag){
          update$run_gene = NULL
          shinyalert('Not enough samples',paste0('remove ', update$checkdataset_gene[otag]), type = "error")
        }
        
        if(update$checktumor == "primary"){
          ptag <- update$checkdataset_gene %in% getDisSummary()$primary
          if(TRUE %in% ptag){
            update$run_gene = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_gene[ptag]), type = "error")
          }
        }
        if(update$checktumor == "recurrent"){
          rtag <- update$checkdataset_gene %in% getDisSummary()$recurrent
          if(TRUE %in% rtag){
            update$run_gene = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_gene[rtag]), type = "error")
          }
        }
        if(update$checktumor == "metastatic"){
          mtag <- update$checkdataset_gene %in% getDisSummary()$metastatic
          if(TRUE %in% mtag){
            update$run_gene = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_gene[mtag]), type = "error")
          }
        }
        
      }
      
    })
    
    observeEvent(input$run_ratio, {
      
      if(length(input$checkdataset_ratio) == 0){
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
        shinyalert('Please Select a Dataset!', type = "error")
        
      } else {
        
        update$run_ratio <- input$run_ratio
        update$checkprogram <- input$checkprogram
        update$checkdataset_ratio <- input$checkdataset_ratio
        update$checktumor <- input$checktumor
        update$user_geneA <- input$user_geneA
        update$user_geneB <- input$user_geneB
        update$checkgroup <- input$checkgroup
        update$diseaseSelector_ratio <- input$diseaseSelector_ratio
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_ratio = NULL
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
          }
        }
        
        otag <- update$checkdataset_ratio %in% getDisSummary()$other_tag
        if(TRUE %in% otag){
          update$run_ratio = NULL
          shinyalert('Not enough samples',paste0('remove ', update$checkdataset_ratio[otag]), type = "error")
        }
        
        if(update$checktumor == "primary"){
          ptag <- update$checkdataset_ratio %in% getDisSummary()$primary
          if(TRUE %in% ptag){
            update$run_ratio = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_ratio[ptag]), type = "error")
          }
        }
        if(update$checktumor == "recurrent"){
          rtag <- update$checkdataset_ratio %in% getDisSummary()$recurrent
          if(TRUE %in% rtag){
            update$run_ratio = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_ratio[rtag]), type = "error")
          }
        }
        if(update$checktumor == "metastatic"){
          mtag <- update$checkdataset_ratio %in% getDisSummary()$metastatic
          if(TRUE %in% mtag){
            update$run_ratio = NULL
            shinyalert('Not enough samples',paste0('remove ', update$checkdataset_ratio[mtag]), type = "error")
          }
        }
        
      }
      
    })
    
    observeEvent(input$run_tmb, {
      
      if(length(input$checkdataset) == 0){
        shinyalert('Please Select a Dataset!', type = "error")
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
      } else {
        
        update$run_tmb <- input$run_tmb
        update$checkprogram <- input$checkprogram
        update$checkdataset <- input$checkdataset
        update$checktumor <- input$checktumor
        #input$tmb_choice <- input$tmb_choice
        update$mutation_type <- input$mutation_type
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_tmb = NULL
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
          }
        }
        
        if(update$checktumor == "primary"){
          if(update$checkdataset %in% getDisSummary()$primary | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tmb = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "recurrent"){
          if(update$checkdataset %in% getDisSummary()$recurrent | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tmb = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "metastatic"){
          if(update$checkdataset %in% getDisSummary()$metastatic | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_tmb = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        
      }
      
    })
    
    observeEvent(input$run_map, {
      
      if(length(input$checkdataset_for_MAP) == 0){
        shinyalert('Please Select a Dataset!', type = "error")
        #session$sendCustomMessage(type = 'testmessage', message = 'Please Select a Dataset!')
      } else {
        
        update$run_map <- input$run_map
        update$checkprogram_map <- input$checkprogram_map
        update$checkdataset_for_MAP <- input$checkdataset_for_MAP
        update$checktumor <- input$checktumor
        #input$tmb_choice <- input$tmb_choice
        update$mutation_type <- input$mutation_type
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram_map != "TARGET"){
            update$run_map = NULL
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
          }
        }
        
        if(update$checktumor == "primary"){
          if(update$checkdataset_for_MAP %in% getDisSummary()$primary | update$checkdataset_for_MAP %in% getDisSummary()$other_tag){
            update$run_map = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "recurrent"){
          if(update$checkdataset_for_MAP %in% getDisSummary()$recurrent | update$checkdataset_for_MAP %in% getDisSummary()$other_tag){
            update$run_map = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "metastatic"){
          if(update$checkdataset_for_MAP %in% getDisSummary()$metastatic | update$checkdataset_for_MAP %in% getDisSummary()$other_tag){
            update$run_map = NULL
            shinyalert('Not enough samples!, Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        
      }
      
    })
    
    observeEvent(input$run_gep, {
      
      if(length(input$checkdataset) == 0){
        shinyalert('Please select a dataset!', type = "error")
      } else {
        
        update$run_gep <- input$run_gep
        update$checkprogram <- input$checkprogram
        update$checkdataset <- input$checkdataset
        update$checktumor <- input$checktumor
        update$gepfiletype <- input$gepfiletype
        update$gepfile$datapath <- input$gepfile$datapath
        update$checkgroup <- input$checkgroup
        update$checkgroup_cutp <- input$checkgroup_cutp
        update$upper_per <- input$upper_per
        update$lower_per <- input$lower_per
        update$check_correlation <- input$check_correlation
        update$checktils <- input$checktils
        update$checkpvalue <- input$checkpvalue
        update$checksurvival <- input$checksurvival
        
        if(update$checksurvival == "EventFree" | update$checksurvival == "RelapseFree"){
          if(update$checkprogram != "TARGET"){
            update$run_gep = NULL
            shinyalert('Event-Free survival is only available for TARGET datasets', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'EFS/RFS is available for TARGET data')
          }
        }
        
        if(update$checktumor == "primary"){
          if(update$checkdataset %in% getDisSummary()$primary | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_gep = NULL
            shinyalert('Not enough samples! Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "recurrent"){
          if(update$checkdataset %in% getDisSummary()$recurrent | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_gep = NULL
            shinyalert('Not enough samples! Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
        if(update$checktumor == "metastatic"){
          if(update$checkdataset %in% getDisSummary()$metastatic | update$checkdataset %in% getDisSummary()$other_tag){
            update$run_gep = NULL
            shinyalert('Not enough samples! Select another data or tumor type', type = "error")
            #session$sendCustomMessage(type = 'testmessage', message = 'Not enough samples! Select another data or tumor type')
          }
        }
      }
      
    })
    
    
    
    update <- reactiveValues(
      run_tils = NULL,
      run_sigs = NULL,
      run_cls = NULL,
      run_tmb = NULL,
      run_map = NULL,
      run_gep = NULL,
      run_gene = NULL,
      run_ratio = NULL,
      checkprogram = NULL,
      checkprogram_map = NULL,
      checkdataset = NULL,
      checktumor = NULL,
      checktils = NULL,
      checkpvalue = NULL,
      #checktils_for_sigs = NULL,
      #checkpvalue_for_sigs = NULL,
      #checktils_for_tmb = NULL,
      #checkpvalue_for_tmb = NULL,
      #checktils_for_gep = NULL,
      #checkpvalue_for_gep = NULL,
      #cellselector = NULL,
      checkgroup = NULL,
      upper_per = NULL,
      lower_per = NULL,
      sigs_choice = NULL,
      checksiglist = NULL,
      geneSet = NULL,
      user_geneSet = NULL,
      user_gene = NULL,
      file=NULL,
      datapath=NULL,
      #datapath=NULL,
      diseaseSelector = NULL,
      diseaseSelector_ratio = NULL,
      clusterSelector = NULL,
      checkdataset_for_MAP = NULL,
      #datapath = NULL,
      #tils_choice = NULL,
      #tmb_choice = NULL,
      mutation_type = NULL,
      check_correlation = NULL,
      #check_correlation_tmb = NULL,
      #check_correlation_GEP = NULL,
      checksurvival = NULL
    )
    
    output$diseaseSelector <- renderUI({
      if(length(update$checkdataset_gene)>5){
        disease <- update$checkdataset_gene[1:5]
      } else {
        disease <- update$checkdataset_gene
      }
      
      selectizeInput(inputId = "diseaseSelector", 
                     label=tags$span(
                       "Your selected datasets",
                       style="color: black; font=size:100%;"),
                     as.list(disease), options=list(maxOptions=length(disease))) 
    })
    
    output$diseaseSelector_ratio <- renderUI({
      if(length(update$checkdataset_ratio)>5){
        disease <- update$checkdataset_ratio[1:5]
      } else {
        disease <- update$checkdataset_ratio
      }
      
      selectizeInput(inputId = "diseaseSelector_ratio", 
                     label=tags$span(
                       "Your selected datasets",
                       style="color: black; font=size:100%;"),
                     as.list(disease), options=list(maxOptions=length(disease))) 
    })
    
    
    output$clusterSelector <- renderUI({
      
      req(update$run_cls)
      
      if(input$clsfiletype=="LD"){
        dataFile <- update$clsfile$datapath
        if (file.exists(isolate({dataFile}) ) ){
        }
        else{
          shinyalert("Unable to open the file!", type="error")
          stop()
        }
        sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
      }
      if(input$clsfiletype=="URL"){
        dataFile <- input$file_url
        sig_list <-  read.csv(dataFile)
      } 
      if(input$clsfiletype=="UD"){
        dataFile <- paste0("data/geneSet_cluster/", "cluster-markers", ".txt")
        if (file.exists(isolate({dataFile}) ) ){
        }
        else{
          shinyalert("Unable to open the file!", type="error")
          stop()
        }
        sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
      }
      
      req(sig_list)
      sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
      cluster <- unique(as.character(sig_list$cluster))
      selectizeInput(inputId = "clusterSelector", 
                     label=tags$span(
                       "Select your cluster"
                       ,style="color: black; font=size:100%;"),
                     as.list(cluster), options=list(maxOptions=length(cluster))) 
    })
    
    
    getData <- eventReactive(update$run_tils, {
      
      if(update$checktils == "LM22"){
        n = 22
      } else {
        n = 6
      }
      
      iccFile <- paste0("cibersoft_results_", update$checkdataset, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No estimated cell proportions for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
      }
      
      subset_tils_cli <- merge(icc_sig, samples, by="sample", sort=FALSE)
      
      #return data
      return(data = list(cell_prop=icc_sig, input=subset_tils_cli, n=n))
      
    })
    
    getcutp <- eventReactive(update$run_tils, {
      
      req(getData()$input)
      req(getData()$n)
      
      inputData <- getData()$input
      if(update$checktils == "LM22"){
        #cutp_vec <- matrix(list(), nrow=getData()$n, ncol=2)
        cutp_vec <- rep("", getData()$n)
        names(cutp_vec) <- colnames(inputData)[2:23]
      } else {
        #cutp_vec <- matrix(list(), nrow=getData()$n, ncol=2)
        cutp_vec <- rep("", getData()$n)
        names(cutp_vec) <- colnames(inputData)[2:7]
      }
      if(update$checkgroup == "cutp"){
        
        if(update$checkgroup_cutp == "FALSE"){
          shinyalert("Please check Yes to proceed!", "", type="error")
          stop()
        } 
        for (i in 1:getData()$n){
          end <- i+1
          gx <- as.matrix(as.numeric(inputData[,end]))
          rownames(gx) <- inputData$sample
          label <- colnames(inputData[,end])
          
          f <- opt_cutp(inputData, end, type=update$checksurvival)
          low_cpi <- signif(as.numeric(f),3)
          high_cpi <- signif(as.numeric(f),3)
          cpi <- c(low_cpi, high_cpi)
          
          cutp_vec[i] <- cpi
        }
      } else {
        
        if(update$checkgroup == "percentile"){
          
          for (i in 1:getData()$n){
            end <- i+1
            gx <- as.matrix(as.numeric(inputData[,end]))
            rownames(gx) <- inputData$sample
            label <- colnames(inputData[,end])
            
            f <- std_cutp(gx, update$checkgroup, update$upper_per, update$lower_per)
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[2]]),3)
            cpi <- c(low_cpi, high_cpi)
            
            cutp_vec[i] <- cpi
          }
        } else {
          
          for (i in 1:getData()$n){
            end <- i+1
            gx <- as.matrix(as.numeric(inputData[,end]))
            rownames(gx) <- inputData$sample
            label <- colnames(inputData[,end])
            
            f <- std_cutp(gx, update$checkgroup, update$upper_per, update$lower_per)
            low_cpi <- signif(as.numeric(f),3)
            high_cpi <- signif(as.numeric(f),3)
            cpi <- c(low_cpi, high_cpi)
            
            cutp_vec[i] <- cpi
          }
          
        }
      }
      
      return(cutp_vec)
    })
    
    applycutpt <- eventReactive(update$run_tils, {
      
      req(getData()$input)
      req(getData()$n)
      req(getcutp())
      
      inputData <- getData()$input
      cp <- as.numeric(getcutp())
      end = getData()$n+1
      
      if(update$checksurvival =="Overall"){
        inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
        
      } else {
        inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
      }
      
      lrpval <- vector(length=getData()$n)
      names(lrpval) <- colnames(inputData)[2:end]
      
      cox.tbl = data.frame(matrix(vector(), getData()$n+2, 10,
                                  dimnames=list(c(), c("Cell_Types", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      #res <-c("", "HR, HR.confint.lower, HR.confint.upper, hrpval, nlow, nhigh)
      #cox.tbl[i,] <- res
      #rownames(cox.tbl)[i] <- as.character(colnames(inputData)[end])
      res <-c("Cell Types", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      for (i in 1:getData()$n){
        
        if(update$checkgroup == "percentile"){
          low_cpi <- signif(cp[i][[1]],4)
          high_cpi <- signif(cp[i][[2]],4)
        } else {
          low_cpi <- signif(cp[i][[1]],4)
          high_cpi <- signif(cp[i][[1]],4)
        }
        
        end= i+1
        gx <-inputData[,end]
        names(gx) <- inputData$sample
        
        #identify samples within each low and high cut-points
        los = names(gx)[gx <= low_cpi]
        his = names(gx)[gx > high_cpi]
        inputData$group[inputData$sample %in% los] = "Low"
        inputData$group[inputData$sample %in% his] = "High"
        
        #make low expression as the reference level
        inputData$group = factor(inputData$group, levels = c("Low", "High"))
        #stratify patients by group
        cox.os = coxph(os ~ group, data=inputData) 
        km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
        
        nlow <- km.os$n[1]
        nhigh <- km.os$n[2]
        
        lrpval[i] <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
        HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
        HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
        HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
        HR <- paste0(HR.ratio, " (", 
                     HR.confint.lower, "-", HR.confint.upper, ")")
        
        res <-c(as.character(colnames(inputData)[end]), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",",high_cpi), lr_pval)
        cox.tbl[2+i,] <- res
        rownames(cox.tbl)[2+i] <- as.character(colnames(inputData)[end])
        
      }
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(lrpval=lrpval, cox.tbl=cox.dt.df))
      
    })
    
    output$hrplot <- renderPlot({
      
      req(applycutpt()$cox.tbl)
      
      cox_tbl <- applycutpt()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=getData()$n
      
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      
      for (i in 1:n){
        #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
        if(pval[i] <= 0.05){
          b_clrs[i] = "red"
          l_clrs[i] = "black"
        } else {
          b_clrs[i] = "grey"
          l_clrs[i] = "grey"
        }
      }
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      #tabletext <- list(list(), list()) #Creating a list with "sublists" for each column
      #tabletext[[1]] <- rownames(cox_tbl)
      #tabletext[[2]][1] <- list(expression(paste(italic("r"), " = .42"))) #manual way using expression and paste
      #tabletext[[2]][2] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=HRQoL$Sweden[,2]))) #need substitute function to access variables
      #tabletext[[2]][3] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3])))) #The substitute functions allows addicitonal manipulation of strings to be put back into the expression
      #tabletext[[2]][4] <- list(substitute(expression(paste(italic("t")[df],"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3]), df="23"))) #One can also substitute multiple elements of expression, use subscripts etc. 
      #tabletext[[1]][4] <- list(expression(bold("Make single line bold"))) #Manipulate strings manually, using unicode
      
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = update$checkdataset,
                 is.summary=c(TRUE,TRUE,rep(FALSE,getData()$n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=FALSE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 #col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0, 0.2, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    
    gen_icc_plot <- eventReactive(update$run_tils,{
      req(getData()$input)
      inputData = getData()$input
      end <- getData()$n+1
      dm <- melt(inputData[,c(1:end)])
      par(las=2)
      par(mar=c(17.1,6.1,1,1))
      pirateplot(formula = value ~ variable, data=dm, 
                 theme=2, main="", sortx="mean", decreasing=TRUE, 
                 ylab=paste0("Estimated fraction of TILs (CIBERSOFT)", "\n"), xlab="", 
                 cex.axis = 1.5, cex.names = 1.3, cex.lab = 1.5,
                 jitter.val = .01,
                 point.o = 1 / 200
      )
      #text(10, 0.8, label=update$checkdataset, cex=2.0)
      #corners=par("usr")
      #par(xpd = TRUE) 
      #text(x=(corners[2]/2), y=corners[4]+0.06, update$checkdataset, cex=1.8, font=2)
    })
    
    output$icc_plot <- renderPlot({
      gen_icc_plot()
    })
    
    gen_icc_corr_plot <- eventReactive(update$run_tils,{
      req(getData()$cell_prop)
      req(getData()$n)
      inputData = getData()$cell_prop
      end <- getData()$n+1
      
      icc_sig <- as.matrix(inputData[,c(2:end)])
      cor.coef <- cor(icc_sig)
      p <- cor.test.p(icc_sig)
      
      cor.coef[is.na(cor.coef)] <- 0
      
      #par(mar=c(16,6,4,4))
      #corrplot(cor.coef, method="square")
      
      cp <- heatmaply_cor(
        cor.coef,
        node_type = "scatter",
        point_size_mat = -log10(p), 
        point_size_name = "-log10(p-value)",
        label_names = c("x", "y", "Correlation"),
        distfun="pearson", 
        hclust_method="ward.D"
        #margins=c(50,50,0,0)
      )
      cp
    })
    
    output$icc_corr_plot <- renderPlotly({
      gen_icc_corr_plot()
    })
    
    output$os_pval <- renderPlot({
      
      req(getData()$input)
      req(getData()$n)
      req(getcutp())
      req(input$cellSelector)
      
      gotData <- os_data_compute(inputData=getData()$input, cp=as.numeric(getcutp()), cp_method=update$checkgroup, ind=(getData()$n+1), selected=input$cellSelector)
      inputData <- gotData$inputData
      
      if(input$tils_choice == TRUE){
        
        los<-inputData$group[inputData$group=="Low"]
        his<-inputData$group[inputData$group=="High"]
        
        if(update$checksurvival =="Overall"){
          inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
          inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
          
        } else {
          inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
          inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
        }
        
        inputData$group = factor(inputData$group, levels = c("Low", "High"))
        cox.os = coxph(os ~ group, data=inputData) 
        km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
        
        low.col = rgb(0, 0, 0.5)
        high.col = rgb(0.5, 0, 0)
        #cols = c(low.col, high.col)
        cols = c("royalblue", "maroon")
        hist.col = rgb(0.5, 0.5, 0.5)
        
        
        par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
        plot(km.os, cex.lab = 2.0, cex.main = 1.5, cex.axis=1.5,
             main = paste0(update$checksurvival, " Survival", "\n", update$checkdataset),
             xlab = "Time in Months", 
             ylab = "Survival Probability", 
             mark.time = T, col = cols, lwd=2, lty=2)
        legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = input$cellSelector, lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
        legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
        #text(400,0,labels=paste0("n=", length(los)), adj=c(2,-1), col=low.col)
        #text(x.median,0,labels=paste0("n=", length(his)), adj=c(-1,-1), col=high.col)
        
      } else {
        ind <- gotData$iSel
        gx_data <- data.frame(inputData$sample, inputData$group, inputData[,ind])
        colnames(gx_data) <- c("sample", "group", "gx")
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(gx_data,input$cellSelector,label="CIBERSOFT Fraction")
      }
    })
    
    output$hr_scp <- renderPlotly({
      
      req(applycutpt()$cox.tbl)
      
      dfp <- applycutpt()$cox.tbl[-c(1:2),c(2,7,8)]
      dfp$cells <-applycutpt()$cox.tbl[-c(1:2),1]
      dfp_merge <- merge(dfp, getCellNames, by="cells", sort=FALSE)
      
      x_vec <- as.vector(dfp_merge$HR_Ratio)
      y_vec <- as.vector(dfp_merge$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp_merge$x_vec <- x_vec
      dfp_merge$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp_merge$sig <- "p<0.05"
      dfp_merge$sig[dfp_merge$y_vec<=0.05 & dfp_merge$x_vec>1.0]="Adv"
      dfp_merge$sig[dfp_merge$y_vec<=0.05 & dfp_merge$x_vec<=1.0]="Fav"
      dfp_merge$y_vec <- -log(dfp_merge$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp_merge$HR = paste0(dfp_merge$cells, "\n", dfp_merge$HR, "\n", "p=", dfp_merge$HR_Pvalue)
      
      bp <- ggplot(data=dfp_merge,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=abbr),hjust=0, vjust=0, size = 4, show.legend = FALSE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=update$checkdataset) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=3)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    getDataSIG  <- eventReactive(update$run_sigs, {
      
      if(update$sigs_choice == "LD"){ #if a signature gene set
        #subset gx data for genes in the signature set
        readSIGFile <- paste0("data/SIG_SETS/", update$checksiglist, ".txt")
        sig_list <-  read.table(readSIGFile, header=F, sep="\t")
        req(sig_list)
        sig_genes <- as.character(sig_list[,1])
        geneSet <- update$checksiglist
      }
      if(update$sigs_choice == "UD"){ #if a signature gene set
        
        if(length(update$user_geneSet) == 0){
          shinyalert("Please input a gene set!", type="error")
          stop()
        }
        req(update$user_geneSet)
        sig_genes <-  unlist(strsplit(x = update$user_geneSet,split = '[\r\n]' ))
        sig_genes <- toupper(sig_genes)
        req(sig_genes)
        geneSet = as.character(update$geneSet)
      }
      
      expFile <- paste0(update$checkdataset, "_", "FPKM", ".txt")
      readEXPFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", expFile)
      if(!file.exists( isolate({readEXPFile}))){
        shinyalert("No Data", "No genomics data for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      
      fpkm <- fread(readEXPFile, sep="\t", stringsAsFactors = FALSE)
      colnames(fpkm)[2] <- "symbol"
      req(fpkm)
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
          stop()
        } 
        
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
          stop()
        }
        
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      }
      fpkm$symbol <- toupper(fpkm$symbol)
      gene_fpkm <- fpkm[fpkm$symbol %in% sig_genes, ]
      dt <- data.table(gene_fpkm)
      #to remove duplicated ensemble gene ids
      dt$var <- rowVars(dt[,-c(1:2)])
      dtt <- dt[order(dt$symbol, -dt$var), ]
      dt <- dtt[ !duplicated(dtt$symbol), ]
      
      symbol_order <- dt$symbol
      subset_fpkm <- dt[ ,intersect(names(dt),samples$sample), with=FALSE]
      #subset_fpkm <- dt[, samples$sample, with=TRUE] # this will generate error when list is not found
      #subset_fpkm <- subset_fpkm[rowSums(subset_fpkm[])>0,]
      
      gx <- as.matrix(log2(subset_fpkm+1))
      rownames(gx) <- symbol_order
      
      if(sum(gx)==0 | nrow(gx)==0){
        shinyalert("Failed", "Gene not found or has no expression!", type="error")
        stop()
      }
      
 
      #if(length(unique(sig_genes))>=3){
      if(length(sig_genes)>=3){
        scores <- callGSVA(gx, symbol_order)
        colnames(scores) <- "gx"
      } else {
        zscores <- callZSCORE(gx)
        scores <- as.matrix(zscores[,-1])
        rownames(scores) <- zscores$sample
        colnames(scores) <- "gx"
      }
      
      ##Get the cut-point for each disease GSVA score
      if(update$checkgroup == "cutp"){
        if(update$checkgroup_cutp == "FALSE"){
          shinyalert("Please check Yes to proceed!", "", type="error")
          stop()
        } 
        
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, scores, by="row.names", sort=FALSE)
        end <- grep("gx", colnames(subset_tils_cli))
        f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
        
        low_cpi <- signif(as.numeric(f),3)
        high_cpi <- signif(as.numeric(f),3)
        
        #identify samples within each low and high cut-points
        los = rownames(scores)[scores <= low_cpi]
        his = rownames(scores)[scores > high_cpi]
        scores <- data.frame(rownames(scores),scores)
        colnames(scores) <- c("sample","gx")
        scores$group[scores$sample %in% los] = "Low"
        scores$group[scores$sample %in% his] = "High"
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
        
        cpi <- c(low_cpi, high_cpi)
      }
      else {
        
        if(update$checkgroup == "percentile"){
          f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[2]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          scores <- subset(scores, scores$group=="Low" | scores$group=="High")
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
        } 
        else {
          
          f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[1]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
        }
      }
      
      return(data=list(scores=scores, input=subset_tils_cli, cpi=cpi, geneSet=geneSet))
      
    })
    
    applycutptSIG <- eventReactive(update$run_sigs, {
      
      req(getDataSIG()$input)
      req(getDataSIG()$cpi)
      req(update$checkdataset)
      
      i=1
      
      low_cpi=as.numeric(getDataSIG()$cpi[[1]])
      high_cpi=as.numeric(getDataSIG()$cpi[[2]])
      
      lrpval <- vector(length=i)
      names(lrpval) <- update$checkdataset
      
      cox.tbl = data.frame(matrix(vector(), i, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      res <-c("Cell Types", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      inputData <- getDataSIG()$input
      
      if(update$checksurvival =="Overall"){
        inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
        
      } else {
        inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
      }
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      nlow <- km.os$n[1]
      nhigh <- km.os$n[2]
      
      lrpval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
      HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
      HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
      HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
      HR <- paste0(HR.ratio, " (", 
                   HR.confint.lower, "-", HR.confint.upper, ")")
      
      res <-c(update$checkdataset, HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",",high_cpi), lr_pval)
      cox.tbl[2+i,] <- res
      rownames(cox.tbl)[2+i] <- update$checkdataset
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(lrpval=lrpval, cox.tbl=cox.dt.df))
      
    })
    
    #################################
    getDataCORR <- eventReactive(update$run_sigs, {
      
      #if(update$check_correlation == TRUE){
      
      req(update$checkdataset)
      req(update$checktumor)
      req(update$checktils)
      
      iccFile <- paste0("cibersoft_results_", update$checkdataset, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No estimated cell proportions for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      gsva_scores <- getDataSIG()$scores
      gsva_scores <- gsva_scores[,-3]
      colnames(gsva_scores) <- c("sample", "ES")
      icc_sig_subset <- merge(icc_sig, gsva_scores, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_sig_subset[,c(ncol(icc_sig_subset))]) #GSVA 
      y <- as.matrix(icc_sig_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_sig_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_sig_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c("ES", "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      return(dm)
      #}
    })
    
    output$wt_plot <- renderPlot({
      req(getDataSIG()$input)
      req(getDataSIG()$geneSet)
      
      if(input$gsva_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(getDataSIG()$scores,getDataSIG()$geneSet,"ES")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(getDataSIG()$scores,getDataSIG()$geneSet,label="ES")
      }
    })
    
    output$os_pval_SIG <- renderPlot({
      req(getDataSIG()$input)
      req(getDataSIG()$geneSet)
      
      inputData <- getDataSIG()$input
      
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      if(update$checksurvival =="Overall"){
        inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
        
      } else {
        inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
      }
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5, 
           main = paste0(update$checksurvival, " Survival", "\n", update$checkdataset), 
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=3)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = getDataSIG()$geneSet, lwd = 6, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      #text(400,0,labels=paste0("n=", length(los)), adj=c(2,-1), col=low.col)
      #text(x.median,0,labels=paste0("n=", length(his)), adj=c(-1,-1), col=high.col)
    })
    
    output$hrplot_SIG <- renderPlot({
      req(applycutptSIG()$cox.tbl)
      
      cox_tbl <- applycutptSIG()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=1
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <-  vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      i=1
      
      #for (i in 1:getData()$n){
      #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
      if(pval[i] <= 0.05){
        b_clrs[i] = "red"
        l_clrs[i] = "black"
      } else {
        b_clrs[i] = "grey"
        l_clrs[i] = "grey"
      }
      #}
      
      fn <- local({
        i=0
        
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      #tabletext <- list(list(), list()) #Creating a list with "sublists" for each column
      #tabletext[[1]] <- rownames(cox_tbl)
      #tabletext[[2]][1] <- list(expression(paste(italic("r"), " = .42"))) #manual way using expression and paste
      #tabletext[[2]][2] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=HRQoL$Sweden[,2]))) #need substitute function to access variables
      #tabletext[[2]][3] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3])))) #The substitute functions allows addicitonal manipulation of strings to be put back into the expression
      #tabletext[[2]][4] <- list(substitute(expression(paste(italic("t")[df],"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3]), df="23"))) #One can also substitute multiple elements of expression, use subscripts etc. 
      #tabletext[[1]][4] <- list(expression(bold("Make single line bold"))) #Manipulate strings manually, using unicode
      
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = getDataSIG()$geneSet,
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=TRUE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0.1, 0.25, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_SIG <- renderPlotly({
      
      req(applycutptSIG()$cox.tbl)
      
      dfp <- applycutptSIG()$cox.tbl[-c(1:2),c(2,7,8)]
      dfp$Dataset <- applycutptSIG()$cox.tbl[-c(1:2),1]
      
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, show.legend = TRUE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=getDataSIG()$geneSet) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=4)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot <- renderPlot({
      req(getDataCORR())
      dm <- getDataCORR()
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        #arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=6, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab("Enrichment Scores") +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
    })
    
    
    getDataCLS <- eventReactive(update$run_cls, {
      
      req(update$checkdataset)
      req(update$checktumor)
      req(update$clsfiletype)
      
      if(update$clsfiletype == "ED"){ #if an example gene set
        dataFile <- paste0("data/geneSet_cluster/", "cluster-markers", ".txt")
        if (file.exists( isolate({dataFile}) ) ){
        }
        else{
          shinyalert("Unable to open the file!", type="error")
          stop()
        }
        sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
        req(sig_list)
        sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
        
        cluster <- unique(as.character(sig_list$cluster))
        n <- length(cluster)
        
        if(n == 0){
          shinyalert("Please provide one or more cluster!", type="error")
          stop()
        }
        
        
      }
      if(update$clsfiletype == "LD"){ #if user-uploaded
        
        req(update$clsfile$datapath)
        dataFile <- update$clsfile$datapath
        if (file.exists( isolate({dataFile}) ) ){
        }
        else{
          shinyalert("Unable to open the file!", type="error")
          stop()
        }
        
        sig_list <-  fread(dataFile, header=T, sep="\t", stringsAsFactors = T)
        req(sig_list)
        sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
        
        cluster <- unique(as.character(sig_list$cluster))
        n <- length(cluster)
        
        if(n == 0){
          shinyalert("Please provide one or more cluster!", type="error")
          stop()
        }
        
      }
      
      if(update$clsfiletype == "URL"){ #if user-url
        
        dataFile <- input$file_url
        if(hd <- httr::HEAD(dataFile)$status_code == 200){
        } 
        else{
                shinyalert("Unable to open the file!", type="error")
          		stop()
        }
        

        sig_list <-  read.csv(dataFile)
        req(sig_list)
        sig_list <- subset(sig_list, sig_list$p_val_adj<=0.05)
        
        cluster <- unique(as.character(sig_list$cluster))
        n <- length(cluster)
        
        if(n == 0){
          shinyalert("Please provide one or more cluster!", type="error")
          stop()
        }
        
      }
      
      expFile <- paste0(update$checkdataset, "_", "FPKM", ".txt")
      readEXPFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", expFile)
      if(!file.exists( isolate({readEXPFile}))){
        shinyalert("No Data", "No genomics data for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      
      fpkm <- fread(readEXPFile, sep="\t", stringsAsFactors = FALSE)
      colnames(fpkm)[2] <- "symbol"
      req(fpkm)
      fpkm$symbol <- toupper(fpkm$symbol)
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
          stop()
        }
        
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      }
      
      
      cutp_vec <- rep("", n)
      names(cutp_vec) <- cluster[1:n]
      
      lrpval <- vector(length=n)
      scores_df <- list()
      input_df <- list()
      names(lrpval) <- cluster[1:n]
      
      cox.tbl = data.frame(matrix(vector(), n+2, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      rownames(cox.tbl)[1:2] <- c("row1", "row2")
      
      for(i in 1:n){ ## allow upto five datasets
        
        sig_genes <- as.character(sig_list$gene[sig_list$cluster==cluster[i]])
        sig_genes <- toupper(sig_genes)
        
        gene_fpkm <- fpkm[fpkm$symbol %in% sig_genes, ]
        dt <- data.table(gene_fpkm)
        subset_fpkm <- dt[ ,intersect(names(dt),samples$sample), with=FALSE]
        #subset_fpkm <- dt[, samples$sample, with=TRUE] # this will generate error when list is not found
        symbol_order <- dt$symbol
        gx <- as.matrix(log2(subset_fpkm+1))
        rownames(gx) <- symbol_order
        
        if(sum(gx)==0){
          shinyalert("Failed", "Gene not found or has no expression!", type="error")
          stop()
        }
        
        if(length(unique(sig_genes))>=3){
          scores <- callGSVA(gx, symbol_order)
          colnames(scores) <- "gx"
        } else {
          zscores <- callZSCORE(gx)
          scores <- as.matrix(zscores[,-1])
          rownames(scores) <- zscores$sample
          colnames(scores) <- "gx"
        }
        
        ##Get the cut-point for each disease GSVA score
        if(update$checkgroup == "cutp"){
          if(update$checkgroup_cutp == "FALSE"){
            shinyalert("Please check Yes to proceed!", "", type="error")
            stop()
          } 
          
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="row.names", sort=FALSE)
          end <- grep("gx", colnames(subset_tils_cli))
          f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
          
          low_cpi <- signif(as.numeric(f),3)
          high_cpi <- signif(as.numeric(f),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        } else {
          
          if(update$checkgroup == "percentile"){
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[2]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            scores <- subset(scores, scores$group=="Low" | scores$group=="High")
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
            
          } 
          else {
            
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[1]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
            
          }
        }
        cutp_vec[i] <- cpi
        scores_df[[i]] <- scores
        
        if(update$checksurvival =="Overall"){
          subset_tils_cli$Overall.Survival.Time.in.Months = (subset_tils_cli$Overall.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Overall.Survival.Time.in.Months, event=subset_tils_cli$Vital.Status=="Dead")
          
        } else {
          subset_tils_cli$Event.Free.Survival.Time.in.Months = (subset_tils_cli$Event.Free.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Event.Free.Survival.Time.in.Months, event=subset_tils_cli$Event.Status=="Event")
        }
        
        #make low expression as the reference level
        subset_tils_cli$group = factor(subset_tils_cli$group, levels = c("Low", "High"))
        #stratify patients by group
        cox.os = coxph(os ~ group, data=subset_tils_cli) 
        km.os = survfit(os ~ group, data = subset_tils_cli, conf.type = "log-log")
        
        nlow <- km.os$n[1]
        nhigh <- km.os$n[2]
        
        lrpval[i] <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
        HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
        HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
        HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
        HR <- paste0(HR.ratio, " (", 
                     HR.confint.lower, "-", HR.confint.upper, ")")
        
        res <-c(as.character(cluster[i]), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste(low_cpi,",",high_cpi), lr_pval)
        cox.tbl[2+i,] <- res
        rownames(cox.tbl)[2+i] <- as.character(cluster[i])
        
        input_df[[i]] <- subset_tils_cli
      }
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(scores_df=scores_df, cpi=cutp_vec, input_df=input_df, lrpval=lrpval, cox.tbl=cox.dt.df, n=n, cluster=cluster))
      
      rm(scores_df, input_df, lrpval, cox.dt.df)
    })
    
    #################################
    getDataCORR_CLS <- eventReactive(update$run_cls, {
      
      #if(update$check_correlation == TRUE){
      
      req(input$checkdataset)
      req(update$checktumor)
      req(update$checktils)
      
      iccFile <- paste0("cibersoft_results_", input$checkdataset, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No cell proportions estimated for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      inputData <- getDataCLS()$scores_df[[1]]
      inputData <- inputData[,-3]
      colnames(inputData) <- c("sample", getDataCLS()$cluster[1])
      
      for(i in 2:getDataCLS()$n){ ## allow upto five datasets
        
        gx_scores <- getDataCLS()$scores_df[[i]]
        gx_scores <- gx_scores[,-3]
        colnames(gx_scores) <- c("sample", getDataCLS()$cluster[i])
        
        inputData <- merge(inputData, gx_scores, by="sample", sort=FALSE)
      }
      
      
      icc_sig_subset <- merge(icc_sig, inputData, by="sample", sort=FALSE)
      
      
      y <- as.matrix(icc_sig_subset[,c(2:end)]) #cell prop
      x_start <- end + 4
      x <- as.matrix(icc_sig_subset[,c(x_start:ncol(icc_sig_subset))]) #GX
      
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_sig_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_sig_subset)
      p.coef_p <- p.coef$p[1:getDataCLS()$n,-c(1:getDataCLS()$n)]
      p.coef_p[is.na(p.coef_p)] <- 1
      colnames(p.coef_p) <- colnames(cor.coef)
      rownames(p.coef_p) <- rownames(cor.coef)
      
      dm_coef <- melt(cor.coef)
      colnames(dm_coef)[3] <- "value"
      dm_coef_p <- melt(p.coef_p)
      colnames(dm_coef_p)[3] <- "pvalue"
      
      dm <- merge(dm_coef, dm_coef_p, by=c("Var1", "Var2"))
      colnames(dm)[1] <- "variable"
      colnames(dm)[2] <- "cell"
      
      return(dm)
      #}
    })
    
    output$CLS_plot <- renderPlot({
      req(getDataCLS()$scores_df)
      req(getDataCLS()$cluster)
      req(input$clusterSelector)
      
      index_to_plot <- which(input$clusterSelector == getDataCLS()$cluster)
      gx_data <- getDataCLS()$scores_df[[index_to_plot]]
      if(input$cls_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(gx_data,input$clusterSelector,"Enrichment Scores")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(gx_data,input$clusterSelector,label="Enrichment Scores")
      }
    })
    
    
    output$os_pval_CLS <- renderPlot({
      
      req(getDataCLS()$input_df)
      req(getDataCLS()$n)
      req(input$clusterSelector)
      
      index_to_plot <- which(input$clusterSelector == getDataCLS()$cluster)
      inputData <- getDataCLS()$input_df[[index_to_plot]]
      
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5,
           main = paste0(update$checksurvival, " Survival", "\n", input$clusterSelector),
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = update$user_gene, lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      
      #plot.data = fortify(km.os)
      #plot.data$info = paste0("n.risk:", plot.data$n.risk, "\n", "n.event:", plot.data$n.event, "\n", "n.censor:", plot.data$n.censor)
      #plot.data$grp_col = "white"
      #plot.data$grp_col[plot.data$strata=="Low"]="blue"
      #plot.data$grp_col[plot.data$strata=="High"]="red"
      
      #max_x <- max(plot.data$time)+10
      #min_x <- min(plot.data$time)+10
      
      #med_low <- summary(km.os)$table[, "median"][[1]] #low
      #med_high <- summary(km.os)$table[, "median"][[2]] #high
      #pval <- signif(summary(cox.os)$sctest["pvalue"], digits=3)
      #if(pval<=0.001){
      #  pval = "<0.001"
      #} else {
      #  pval <- signif(summary(cox.os)$sctest["pvalue"], digits=2)
      #}
      
      #pval_label <- paste0("p=", pval)
      
      #p1 <- ggplot(plot.data, aes(time, surv, group = strata, colour = grp_col, text=info))
      #p1 <- p1 + geom_point(size=4, shape=3, stroke=0.5, show.legend=FALSE)
      #p1 <- p1 + geom_step(size = 1.5, show.legend=TRUE)
      #p1 <- p1 + theme_classic()
      #p1 <- p1 + scale_color_manual(name=update$user_gene, labels = c('Low', 'High'), 
      #                              values = c('blue', 'red'), 
      #                              guide='legend')
      #p1 <- p1 + scale_x_continuous(breaks = seq(0, max_x, by = 50), limits = c(0, max_x))
      #p1 <- p1 + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))
      #p1 <- p1 + xlab("Time (in months)")
      #p1 <- p1 + ylab('Survival Probability')
      #p1 <- p1 + theme(axis.title.y = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(axis.title.x = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5))
      #p1 <- p1 + theme(axis.text=element_text(size=12,face="plain",colour="black"))
      #p1 <- p1 + guides(fill=guide_legend(title=update$user_gene))
      #p1 <- p1 + theme(legend.text = element_text(colour="black", size=14, face="bold"))
      #p1 <- p1 + theme(legend.title = element_text(colour="black", size=15, face="bold"))
      #p1 <- p1 + theme(legend.position = c(0.9, 0.9), legend.justification = c(1, 1))
      #p1 <- p1 + labs(fill ="", title=input$clusterSelector) 
      
      #p1 <- p1 + annotate("text", x=min_x, y=0.05, label=pval_label, color="black", size=8, fontface="bold")
      #for high
      #p1 <- p1 + geom_segment(x = med_high, xend = med_high, y = -Inf, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_high, y = 0.5, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #for low
      #p1 <- p1 + geom_segment(x = med_low, xend = med_low, y = -Inf, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_low, y = 0.5, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + theme(plot.margin = unit(c(2,2,2,2),"pt"))
      #p1   
      #ggplotly(p1, tooltip = c("info"))
    })
    
    
    output$hrplot_CLS <- renderPlot({
      
      req(getDataCLS()$cox.tbl)
      req(getDataCLS()$n)
      
      cox_tbl <- getDataCLS()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=getDataCLS()$n
      
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      
      for (i in 1:n){
        #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
        if(pval[i] <= 0.05){
          b_clrs[i] = "red"
          l_clrs[i] = "black"
        } else {
          b_clrs[i] = "grey"
          l_clrs[i] = "grey"
        }
      }
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = update$user_gene,
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=FALSE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 #col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0, 0.2, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_CLS <- renderPlotly({
      
      req(getDataCLS()$cox.tbl)
      
      dfp <- getDataCLS()$cox.tbl[-c(1:2),c(1,2,7,8)]
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, fontface="bold", show.legend = FALSE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=update$checkdataset_gene) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=3)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_CLS <- renderPlot({
      req(getDataCORR_CLS())
      req(getDataCLS()$n)
      
      dm <- getDataCORR_CLS()
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      cell <- unique(dm$cell)
      dm$cell <- factor(dm$cell, levels = cell)
      
      if(getDataCLS()$n<=7){
        margins = c(2,10,5,12)
        given_size=6
      }
      if(getDataCLS()$n>8){
        margins = c(2,10,5,5)
        given_size=4
      }
      if(getDataCLS()$n>20){
        margins = c(2,10,5,2)
        given_size=2
      }
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=given_size, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab("") +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_text(colour="black",face="bold",size=14.0, angle=90, hjust = 1, vjust=0)) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(margins),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
    })
    
    
    
    getDataTMB  <- eventReactive(update$run_tmb, {
      
      tmbFile <- paste0(update$checkdataset, "_", "MB", ".txt")
      readTMBFile <- paste0("data/GDC_MAF/", tmbFile)
      if(!file.exists( isolate({readTMBFile}))){
        shinyalert("No Data", "No TMB file for the chosen Dataset and Tumor Type", type="error")
        stop()
      } 
      tmb <- fread(readTMBFile, sep="\t", header=T)
      req(tmb)
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
        
      }
      
      get_col <- update$mutation_type
      tmb_estimate <- as.matrix(as.numeric(tmb[,get(get_col)]))
      rownames(tmb_estimate) <- tmb$sample
      colnames(tmb_estimate) <- "gx"
      
      ##Get the cut-point for each disease GSVA score
      if(update$checkgroup == "cutp"){
        if(update$checkgroup_cutp == "FALSE"){
          shinyalert("Please check Yes to proceed!", "", type="error")
          stop()
        } 
        
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, tmb_estimate, by="row.names", sort=FALSE)
        end <- grep("gx", colnames(subset_tils_cli))
        f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
        
        low_cpi <- signif(as.numeric(f),3)
        high_cpi <- signif(as.numeric(f),3)
        
        #identify samples within each low and high cut-points
        los = rownames(tmb_estimate)[tmb_estimate <= low_cpi]
        his = rownames(tmb_estimate)[tmb_estimate > high_cpi]
        tmb_estimate <- data.frame(rownames(tmb_estimate),tmb_estimate)
        colnames(tmb_estimate) <- c("sample","gx")
        tmb_estimate$group[tmb_estimate$sample %in% los] = "Low"
        tmb_estimate$group[tmb_estimate$sample %in% his] = "High"
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, tmb_estimate, by="sample", sort=FALSE)
        
        cpi <- c(low_cpi, high_cpi)
        
      } else {
        
        if(update$checkgroup == "percentile"){
          f <- std_cutp(tmb_estimate, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[2]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(tmb_estimate)[tmb_estimate <= low_cpi]
          his = rownames(tmb_estimate)[tmb_estimate > high_cpi]
          tmb_estimate <- data.frame(rownames(tmb_estimate),tmb_estimate)
          colnames(tmb_estimate) <- c("sample","gx")
          tmb_estimate$group[tmb_estimate$sample %in% los] = "Low"
          tmb_estimate$group[tmb_estimate$sample %in% his] = "High"
          tmb_estimate <- subset(tmb_estimate, tmb_estimate$group=="Low" | tmb_estimate$group=="High")
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, tmb_estimate, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        } 
        else {
          
          f <- std_cutp(tmb_estimate, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[1]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(tmb_estimate)[tmb_estimate <= low_cpi]
          his = rownames(tmb_estimate)[tmb_estimate > high_cpi]
          tmb_estimate <- data.frame(rownames(tmb_estimate),tmb_estimate)
          colnames(tmb_estimate) <- c("sample","gx")
          tmb_estimate$group[tmb_estimate$sample %in% los] = "Low"
          tmb_estimate$group[tmb_estimate$sample %in% his] = "High"
          tmb_estimate <- subset(tmb_estimate, tmb_estimate$group=="Low" | tmb_estimate$group=="High")
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, tmb_estimate, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        }
      }
      return(data=list(scores=tmb_estimate, input=subset_tils_cli, cpi=cpi))
      
    })
    
    applycutptTMB <- eventReactive(update$run_tmb, {
      
      req(getDataTMB()$input)
      req(getDataTMB()$cpi)
      
      i=1
      
      low_cpi=as.numeric(getDataTMB()$cpi[[1]])
      high_cpi=as.numeric(getDataTMB()$cpi[[2]])
      
      lrpval <- vector(length=i)
      names(lrpval) <- update$checkdataset
      
      cox.tbl = data.frame(matrix(vector(), i, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      inputData <- getDataTMB()$input
      inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
      inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      nlow <- km.os$n[1]
      nhigh <- km.os$n[2]
      
      lrpval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
      HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
      HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
      HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
      HR <- paste0(HR.ratio, " (", 
                   HR.confint.lower, "-", HR.confint.upper, ")")
      
      res <-c(as.character(update$checkdataset), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",",high_cpi), lr_pval)
      cox.tbl[2+i,] <- res
      rownames(cox.tbl)[2+i] <- as.character(update$checkdataset)
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(lrpval=lrpval, cox.tbl=cox.dt.df))
      
    })
    
    getDataCORR_TMB <- eventReactive(update$run_tmb, {
      
      #if(update$check_correlation == TRUE){
      
      req(update$checkdataset)
      req(update$checktumor)
      req(update$checktils)
      
      iccFile <- paste0("cibersoft_results_", update$checkdataset, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No estimated cell proportions for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      tmbFile <- paste0(update$checkdataset, "_", "MB", ".txt")
      readTMBFile <- paste0("data/GDC_MAF/", tmbFile)
      if(!file.exists( isolate({readTMBFile}))){
        shinyalert("No Data", "No TMB file for the chosen Dataset and Tumor Type", type="error")
        stop()
      } 
      tmb <- fread(readTMBFile, sep="\t", header=T)
      req(tmb)
      
      tmb_estimate <- tmb[,sample,get(update$mutation_type)]
      colnames(tmb_estimate) <- c(update$mutation_type, "sample")
      icc_tmb_subset <- merge(icc_sig, tmb_estimate, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_tmb_subset[,c(ncol(icc_tmb_subset))]) #TMB
      y <- as.matrix(icc_tmb_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_tmb_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_tmb_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c(update$mutation_type, "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      return(dm)
      #}
    })
    
    output$tmb_plot <- renderPlot({
      req(getDataTMB()$scores)
      
      if(input$tmb_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(getDataTMB()$scores,update$checkdataset,"Mutations/Mb")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(getDataTMB()$scores, update$checkdataset, "Mutations/Mb")
      }
    })
    
    output$os_pval_TMB <- renderPlot({
      req(getDataTMB()$input)
      
      inputData <- getDataTMB()$input
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
      inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
      
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5, 
           main = paste0(update$checksurvival, " Survival", "\n", update$checkdataset),
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = "TMB", lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      #text(400,0,labels=paste0("n=", length(los)), adj=c(2,-1), col=low.col)
      #text(x.median,0,labels=paste0("n=", length(his)), adj=c(-1,-1), col=high.col)
    })
    
    output$hrplot_TMB <- renderPlot({
      req(applycutptTMB()$cox.tbl)
      
      cox_tbl <- applycutptTMB()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=1
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      
      i=1
      #for (i in 1:getData()$n){
      #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
      if(pval[i] <=0.05){
        b_clrs[i] = "red"
        l_clrs[i] = "black"
      } else {
        b_clrs[i] = "grey"
        l_clrs[i] = "grey"
      }
      #}
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      #tabletext <- list(list(), list()) #Creating a list with "sublists" for each column
      #tabletext[[1]] <- rownames(cox_tbl)
      #tabletext[[2]][1] <- list(expression(paste(italic("r"), " = .42"))) #manual way using expression and paste
      #tabletext[[2]][2] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=HRQoL$Sweden[,2]))) #need substitute function to access variables
      #tabletext[[2]][3] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3])))) #The substitute functions allows addicitonal manipulation of strings to be put back into the expression
      #tabletext[[2]][4] <- list(substitute(expression(paste(italic("t")[df],"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3]), df="23"))) #One can also substitute multiple elements of expression, use subscripts etc. 
      #tabletext[[1]][4] <- list(expression(bold("Make single line bold"))) #Manipulate strings manually, using unicode
      
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title =paste0("TMB", "(# Mutations/Mb)"),
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=TRUE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0.1, 0.25, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_TMB <- renderPlotly({
      
      req(applycutptTMB()$cox.tbl)
      
      dfp <- data.frame(applycutptTMB()$cox.tbl[-c(1:2),c(1, 2,7,8)])
      #dfp$dataset <- rownames(dfp)
      
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, show.legend = TRUE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        guides(fill=guide_legend(title="")) + 
        #labs(fill ="", title=update$checkdataset) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=4)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_TMB <- renderPlot({
      req(getDataCORR_TMB())
      dm <- getDataCORR_TMB()
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=5, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab("Mutations/Mb") +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
    })
    
    
    getDataGEP  <- eventReactive(update$run_gep, {
      
      if(update$gepfiletype == "ED"){ #if an example gene set
        
        readGEPFile <- paste0("data/GEP/", "GEP_weights.txt")
        gep_list <-  read.table(readGEPFile, header=T, sep="\t")
        req(gep_list)
        rownames(gep_list) <- gep_list$gene
        gep_genes <- as.character(gep_list$gene)
        geneSet <- "GEP scores"
        
      }
      if(update$gepfiletype == "LD"){ #if user-uploaded
        
        req(update$gepfile$datapath)
        dataFile <- update$gepfile$datapath
        if (file.exists( isolate({dataFile}) ) ){
        }
        else{
          shinyalert("Unable to open the file!", type="error")
          stop()
        }
        
        gep_list  <-  read.table(dataFile, header=T, sep="\t")
        req(gep_list)
        rownames(gep_list) <- gep_list$gene
        gep_genes <- as.character(gep_list$gene)
        geneSet <- "GEP scores"
      }
      
      expFile <- paste0(update$checkdataset, "_", "FPKM", ".txt")
      readEXPFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", expFile)
      fpkm <- fread(readEXPFile, sep="\t", stringsAsFactors = FALSE)
      colnames(fpkm)[2] <- "symbol"
      req(fpkm)
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", update$checkdataset), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      }
      
      gene_fpkm <- fpkm[fpkm$symbol %in% gep_genes, ]
      dt <- data.table(gene_fpkm)
      subset_fpkm <- dt[ ,intersect(names(dt),samples$sample), with=FALSE]
      #subset_fpkm <- dt[, samples$sample, with=TRUE] # this will generate error when list is not found
      symbol_order <- dt$symbol
      gx <- as.matrix(log2(subset_fpkm+1))
      rownames(gx) <- symbol_order
      
      gx_wts <- merge(gep_list, gx, by="row.names", sort=FALSE)
      gx_wts[, -c(1:3)] <- (gx_wts[, -c(1:3)] * gx_wts[, 3])
      
      scores <- as.matrix(colSums(gx_wts[,-c(1:3)]))
      #rownames(scores) <- gsub("\\.", "-", rownames(scores))
      colnames(scores) <- "gx"
      
      ##Get the cut-point for each disease GSVA score
      if(update$checkgroup == "cutp"){
        if(update$checkgroup_cutp == "FALSE"){
          shinyalert("Please check Yes to proceed!", "", type="error")
          stop()
        } 
        
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, scores, by="row.names", sort=FALSE)
        end <- grep("gx", colnames(subset_tils_cli))
        f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
        
        low_cpi <- signif(as.numeric(f),3)
        high_cpi <- signif(as.numeric(f),3)
        
        #identify samples within each low and high cut-points
        los = rownames(scores)[scores <= low_cpi]
        his = rownames(scores)[scores > high_cpi]
        scores <- data.frame(rownames(scores),scores)
        colnames(scores) <- c("sample","gx")
        scores$group[scores$sample %in% los] = "Low"
        scores$group[scores$sample %in% his] = "High"
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
        
        cpi <- c(low_cpi, high_cpi)
        
      } else {
        
        if(update$checkgroup == "percentile"){
          f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[2]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          scores <- subset(scores, scores$group=="Low" | scores$group=="High")
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        } 
        else {
          
          f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[1]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
        }
      }
      return(data=list(scores=scores, input=subset_tils_cli, cpi=cpi, geneSet=geneSet))
      
    })
    
    applycutptGEP <- eventReactive(update$run_gep, {
      
      req(getDataGEP()$input)
      req(getDataGEP()$cpi)
      
      i=1
      
      low_cpi=as.numeric(getDataGEP()$cpi[[1]])
      high_cpi=as.numeric(getDataGEP()$cpi[[2]])
      
      lrpval <- vector(length=i)
      names(lrpval) <- update$checkdataset
      
      cox.tbl = data.frame(matrix(vector(), i, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      inputData <- getDataGEP()$input
      
      if(update$checksurvival =="Overall"){
        inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
        
      } else {
        inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
      }
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      nlow <- km.os$n[1]
      nhigh <- km.os$n[2]
      
      lrpval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
      HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
      HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
      HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
      HR <- paste0(HR.ratio, " (", 
                   HR.confint.lower, "-", HR.confint.upper, ")")
      
      res <-c(update$checkdataset, HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi, ",", high_cpi), lr_pval)
      cox.tbl[2+i,] <- res
      rownames(cox.tbl)[2+i] <- update$checkdataset
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(lrpval=lrpval, cox.tbl=cox.dt.df))
      
    })
    
    #################################
    getDataCORR_GEP <- eventReactive(update$run_gep, {
      
      #if(update$check_correlation == TRUE){
      
      req(update$checkdataset)
      req(update$checktumor)
      req(update$checktils)
      
      iccFile <- paste0("cibersoft_results_", update$checkdataset, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No estimated cell proportions for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      gsva_scores <- getDataGEP()$scores
      gsva_scores <- gsva_scores[,-3]
      colnames(gsva_scores) <- c("sample", "ES")
      icc_sig_subset <- merge(icc_sig, gsva_scores, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_sig_subset[,c(ncol(icc_sig_subset))]) #GSVA 
      y <- as.matrix(icc_sig_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_sig_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_sig_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c("ES", "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      return(dm)
      #}
    })
    
    output$gep_plot <- renderPlot({
      req(getDataGEP()$input)
      #req(getDataGEP()$geneSet)
      
      if(input$gep_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(getDataGEP()$scores,getDataGEP()$geneSet,"")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(getDataGEP()$scores,getDataGEP()$geneSet,label=update$checkdataset)
      }
    })
    
    output$os_pval_GEP <- renderPlot({
      req(getDataGEP()$input)
      #req(getDataGEP()$geneSet)
      
      inputData <- getDataGEP()$input
      
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      if(update$checksurvival =="Overall"){
        inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
        
      } else {
        inputData$Event.Free.Survival.Time.in.Months = (inputData$Event.Free.Survival.Time.in.Days)/30.42
        inputData$os = Surv(time=inputData$Event.Free.Survival.Time.in.Months, event=inputData$Event.Status=="Event")
      }
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5, 
           main = paste0(update$checksurvival, " Survival", "\n", update$checkdataset), 
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = getDataGEP()$geneSet, lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      #text(400,0,labels=paste0("n=", length(los)), adj=c(2,-1), col=low.col)
      #text(x.median,0,labels=paste0("n=", length(his)), adj=c(-1,-1), col=high.col)
    })
    
    output$hrplot_GEP <- renderPlot({
      req(applycutptGEP()$cox.tbl)
      
      cox_tbl <- applycutptGEP()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=1
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <-  vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      i=1
      
      #for (i in 1:getData()$n){
      #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
      if(pval[i] <= 0.05){
        b_clrs[i] = "red"
        l_clrs[i] = "black"
      } else {
        b_clrs[i] = "grey"
        l_clrs[i] = "grey"
      }
      #}
      
      fn <- local({
        i=0
        
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      #tabletext <- list(list(), list()) #Creating a list with "sublists" for each column
      #tabletext[[1]] <- rownames(cox_tbl)
      #tabletext[[2]][1] <- list(expression(paste(italic("r"), " = .42"))) #manual way using expression and paste
      #tabletext[[2]][2] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=HRQoL$Sweden[,2]))) #need substitute function to access variables
      #tabletext[[2]][3] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3])))) #The substitute functions allows addicitonal manipulation of strings to be put back into the expression
      #tabletext[[2]][4] <- list(substitute(expression(paste(italic("t")[df],"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3]), df="23"))) #One can also substitute multiple elements of expression, use subscripts etc. 
      #tabletext[[1]][4] <- list(expression(bold("Make single line bold"))) #Manipulate strings manually, using unicode
      
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = getDataGEP()$geneSet,
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=TRUE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0.1, 0.25, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_GEP <- renderPlotly({
      
      req(applycutptGEP()$cox.tbl)
      
      dfp <- applycutptGEP()$cox.tbl[-c(1:2),c(2,7,8)]
      dfp$Dataset <- applycutptGEP()$cox.tbl[-c(1:2),1]
      
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, show.legend = TRUE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=getDataGEP()$geneSet) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=4)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_GEP <- renderPlot({
      req(getDataCORR_GEP())
      dm <- getDataCORR_GEP()
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=6, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab("GEP scores") +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
    })
    
    
    getDataGENE <- eventReactive(update$run_gene, {
      
      req(update$user_gene)
      req(update$checkdataset_gene)
      sig_genes <-  unlist(strsplit(x = update$user_gene, split = '[\r\n]' ))
      sig_genes <- toupper(sig_genes)
      
      if(length(sig_genes) == 0){
        shinyalert("Please input a gene!", type="error")
        stop()
      }
      
      gene_datasets <- update$checkdataset_gene
      n = length(gene_datasets)
      
      if(n>5){
        shinyalert("Only first five dataset selections will be used", "", type="info")
        n=5
        gene_datasets <- gene_datasets[1:n]
      } 
      
      cutp_vec <- rep("", n)
      names(cutp_vec) <- gene_datasets[1:n]
      
      lrpval <- vector(length=n)
      scores_df <- list()
      input_df <- list()
      names(lrpval) <- gene_datasets[1:n]
      
      cox.tbl = data.frame(matrix(vector(), n+2, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
     
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      
      for(i in 1:n){ ## allow upto five datasets
        
        
        expFile <- paste0(gene_datasets[i], "_", "FPKM", ".txt")
        readEXPFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", expFile)
        if(!file.exists( isolate({readEXPFile}))){
          shinyalert("No Data", "No genomics data for the chosen Dataset/Tumor Type", type="error")
          stop()
        } 
        
        fpkm <- fread(readEXPFile, sep="\t", stringsAsFactors = FALSE)
        colnames(fpkm)[2] <- "symbol"
        req(fpkm)
        
        if(update$checksurvival == "Overall"){
          osFile <- paste0(gene_datasets[i], "_", "clinical_OS", ".txt")
          readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
          if(!file.exists( isolate({readOSFile}))){
            shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
            stop()
          } 
          outcome <-  read.table(readOSFile, header=T, sep="\t")
          req(outcome)
          colnames(outcome)[2] <- "sample"
          
          samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
          samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
          rownames(samples) <- samples$sample
          
          if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
            shinyalert(paste0("Remove ", gene_datasets[i]), "Not enough events for survival analysis", type="error")
            stop()
          } 
          
        } else {
          
          osFile <- paste0(gene_datasets[i], "_", "clinical_OS", ".txt")
          readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
          if(!file.exists( isolate({readOSFile}))){
            shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
            stop()
          }
          
          outcome <-  read.table(readOSFile, header=T, sep="\t")
          req(outcome)
          colnames(outcome)[2] <- "sample"
          
          samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
          samples$First.Event <- as.character(samples$First.Event)
          samples$Event.Status = "Event"
          samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
          samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
          rownames(samples) <- samples$sample
          
          if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
            shinyalert(paste0("Remove ", gene_datasets[i]), "Not enough events for survival analysis", type="error")
            stop()
          } 
          
        }

        fpkm$symbol <- toupper(fpkm$symbol)
        gene_fpkm <- fpkm[fpkm$symbol %in% sig_genes, ]
        dt <- data.table(gene_fpkm)
        subset_fpkm <- dt[ ,intersect(names(dt),samples$sample), with=FALSE]
        #subset_fpkm <- dt[, samples$sample, with=TRUE] # this will generate error when list is not found
        symbol_order <- dt$symbol
        gx <- as.matrix(log2(subset_fpkm+1))
        rownames(gx) <- symbol_order
        
        if(sum(gx)==0 | nrow(gx)==0){
          shinyalert("Failed", "Gene not found or has no expression!", type="error")
          stop()
        }
        
        if(nrow(gx)>1){
          max_index <- which.max(rowVars(gx))
          scores <- as.matrix(gx[max_index, ])
          colnames(scores) <- "gx"
        } else {
          scores <- t(gx)
          colnames(scores) <- "gx"
        }
        
        ##Get the cut-point for each disease GSVA score
        if(update$checkgroup == "cutp"){
          if(update$checkgroup_cutp == "FALSE"){
            shinyalert("Please check Yes to proceed!", "", type="error")
            stop()
          } 
          
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="row.names", sort=FALSE)
          end <- grep("gx", colnames(subset_tils_cli))
          f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
          
          low_cpi <- signif(as.numeric(f),3)
          high_cpi <- signif(as.numeric(f),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        } else {
          
          if(update$checkgroup == "percentile"){
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[2]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            scores <- subset(scores, scores$group=="Low" | scores$group=="High")
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
          } 
          else {
            
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[1]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
          }
        }
        
        cutp_vec[i] <- cpi
        scores_df[[i]] <- scores
        
        if(update$checksurvival =="Overall"){
          subset_tils_cli$Overall.Survival.Time.in.Months = (subset_tils_cli$Overall.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Overall.Survival.Time.in.Months, event=subset_tils_cli$Vital.Status=="Dead")
          
        } else {
          subset_tils_cli$Event.Free.Survival.Time.in.Months = (subset_tils_cli$Event.Free.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Event.Free.Survival.Time.in.Months, event=subset_tils_cli$Event.Status=="Event")
        }
        
        #make low expression as the reference level
        subset_tils_cli$group = factor(subset_tils_cli$group, levels = c("Low", "High"))
        #stratify patients by group
        cox.os = coxph(os ~ group, data=subset_tils_cli) 
        km.os = survfit(os ~ group, data = subset_tils_cli, conf.type = "log-log")
        
        nlow <- km.os$n[1]
        nhigh <- km.os$n[2]
        
        lrpval[i] <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
        HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
        HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
        HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
        HR <- paste0(HR.ratio, " (", 
                     HR.confint.lower, "-", HR.confint.upper, ")")
        
        res <-c(as.character(gene_datasets[i]), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",", high_cpi), lr_pval)
        cox.tbl[2+i,] <- res
        rownames(cox.tbl)[2+i] <- as.character(gene_datasets[i])
        
        input_df[[i]] <- subset_tils_cli
      }
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(scores_df=scores_df, cpi=cutp_vec, input_df=input_df, lrpval=lrpval, cox.tbl=cox.dt.df, n=n, gene_datasets=gene_datasets))
      
      rm(scores_df, input_df, lrpval, cox.dt.df)
    })
    
    output$GENE_plot <- renderPlot({
      req(getDataGENE()$scores_df)
      req(getDataGENE()$gene_datasets)
      
      index_to_plot <- which(input$diseaseSelector == getDataGENE()$gene_datasets)
      gx_data <- getDataGENE()$scores_df[[index_to_plot]]
      if(input$gene_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(gx_data,input$diseaseSelector,"Log2(FPKM+1) Expression")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(gx_data,input$diseaseSelector,label="Log2(FPKM+1) Expression")
      }
    })
    
    
    output$os_pval_GENE <- renderPlot({
      
      req(getDataGENE()$input_df)
      req(getDataGENE()$n)
      
      index_to_plot <- which(input$diseaseSelector == getDataGENE()$gene_datasets)
      inputData <- getDataGENE()$input_df[[index_to_plot]]
      
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5,
           main = paste0(update$checksurvival, " Survival", "\n", input$diseaseSelector),
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = update$user_gene, lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      
      #plot.data = fortify(km.os)
      #plot.data$info = paste0("n.risk:", plot.data$n.risk, "\n", "n.event:", plot.data$n.event, "\n", "n.censor:", plot.data$n.censor)
      #plot.data$grp_col = "white"
      #plot.data$grp_col[plot.data$strata=="Low"]="blue"
      #plot.data$grp_col[plot.data$strata=="High"]="red"
      
      #max_x <- max(plot.data$time)+10
      #min_x <- min(plot.data$time)+10
      
      #med_low <- summary(km.os)$table[, "median"][[1]] #low
      #med_high <- summary(km.os)$table[, "median"][[2]] #high
      #pval <- signif(summary(cox.os)$sctest["pvalue"], digits=3)
      #if(pval<=0.001){
      #  pval = "<0.001"
      #} else {
      #  pval <- signif(summary(cox.os)$sctest["pvalue"], digits=2)
      #}
      
      #pval_label <- paste0("p=", pval)
      
      #p1 <- ggplot(plot.data, aes(time, surv, group = strata, colour = grp_col, text=info))
      #p1 <- p1 + geom_point(size=4, shape=3, stroke=0.5, show.legend=FALSE)
      #p1 <- p1 + geom_step(size = 1.5, show.legend=TRUE)
      #p1 <- p1 + theme_classic()
      #p1 <- p1 + scale_color_manual(name=update$user_gene, labels = c('Low', 'High'), 
      #                              values = c('blue', 'red'), 
      #                              guide='legend')
      #p1 <- p1 + scale_x_continuous(breaks = seq(0, max_x, by = 50), limits = c(0, max_x))
      #p1 <- p1 + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))
      #p1 <- p1 + xlab("Time (in months)")
      #p1 <- p1 + ylab('Survival Probability')
      #p1 <- p1 + theme(axis.title.y = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(axis.title.x = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5))
      #p1 <- p1 + theme(axis.text=element_text(size=12,face="plain",colour="black"))
      #p1 <- p1 + guides(fill=guide_legend(title=update$user_gene))
      #p1 <- p1 + theme(legend.text = element_text(colour="black", size=14, face="bold"))
      #p1 <- p1 + theme(legend.title = element_text(colour="black", size=15, face="bold"))
      #p1 <- p1 + theme(legend.position = c(0.9, 0.9), legend.justification = c(1, 1))
      #p1 <- p1 + labs(fill ="", title=input$diseaseSelector) 
      
      #p1 <- p1 + annotate("text", x=min_x, y=0.05, label=pval_label, color="black", size=8, fontface="bold")
      #for high
      #p1 <- p1 + geom_segment(x = med_high, xend = med_high, y = -Inf, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_high, y = 0.5, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #for low
      #p1 <- p1 + geom_segment(x = med_low, xend = med_low, y = -Inf, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_low, y = 0.5, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + theme(plot.margin = unit(c(2,2,2,2),"pt"))
      #p1   
      #ggplotly(p1, tooltip = c("info"))
    })
    
    
    output$hrplot_GENE <- renderPlot({
      
      req(getDataGENE()$cox.tbl)
      req(getDataGENE()$n)
      
      cox_tbl <- getDataGENE()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=getDataGENE()$n
      
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      
      for (i in 1:n){
        #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
        if(pval[i] <= 0.05){
          b_clrs[i] = "red"
          l_clrs[i] = "black"
        } else {
          b_clrs[i] = "grey"
          l_clrs[i] = "grey"
        }
      }
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = update$user_gene,
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=FALSE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 #col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0, 0.2, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_GENE <- renderPlotly({
      
      req(getDataGENE()$cox.tbl)
      
      dfp <- getDataGENE()$cox.tbl[-c(1:2),c(1,2,7,8)]
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, fontface="bold", show.legend = FALSE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=update$checkdataset_gene) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=3)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_GENE <- renderPlot({
      
      req(update$checktumor)
      req(update$checktils)
      req(getDataGENE()$gene_datasets)
      req(getDataGENE()$scores_df)
      req(input$diseaseSelector)
      
      #if(update$check_correlation == TRUE){
      
      index_to_plot <- which(input$diseaseSelector == getDataGENE()$gene_datasets)
      inputData <- getDataGENE()$scores_df[[index_to_plot]]
      gx_scores <- inputData[,-3]
      colnames(gx_scores) <- c("sample", input$diseaseSelector)
      
      iccFile <- paste0("cibersoft_results_", input$diseaseSelector, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No cell proportions estimated for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      icc_sig_subset <- merge(icc_sig, gx_scores, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_sig_subset[,c(ncol(icc_sig_subset))]) #GX
      y <- as.matrix(icc_sig_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_sig_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_sig_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c(input$diseaseSelector, "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=6, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab(input$diseaseSelector) +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
      #}
    })
    
    
    getDataRATIO <- eventReactive(update$run_ratio, {
      
      req(update$user_geneA)
      req(update$user_geneB)
      req(update$checkdataset_ratio)
      gene_A <-  toupper( unlist(strsplit(x = update$user_geneA, split = '[\r\n]' )) )
      gene_B <-  toupper( unlist(strsplit(x = update$user_geneB, split = '[\r\n]' )) )
      
      if(length(gene_A) == 0 || length(gene_B) == 0){
        shinyalert("Please input a gene!", type="error")
        stop()
      }
      
      gene_datasets <- update$checkdataset_ratio
      n = length(gene_datasets)
      
      if(n>5){
        shinyalert("Only first five dataset selections will be used", "", type="info")
        n=5
        gene_datasets <- gene_datasets[1:n]
      } 
      
      cutp_vec <- rep("", n)
      names(cutp_vec) <- gene_datasets[1:n]
      
      lrpval <- vector(length=n)
      scores_df <- list()
      input_df <- list()
      names(lrpval) <- gene_datasets[1:n]
      
      cox.tbl = data.frame(matrix(vector(), n+2, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      
      for(i in 1:n){ ## allow upto five datasets
        
        
        expFile <- paste0(gene_datasets[i], "_", "FPKM", ".txt")
        readEXPFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", expFile)
        if(!file.exists( isolate({readEXPFile}))){
          shinyalert("No Data", "No genomics data for the chosen Dataset/Tumor Type", type="error")
          stop()
        } 
        
        fpkm <- fread(readEXPFile, sep="\t", stringsAsFactors = FALSE)
        colnames(fpkm)[2] <- "symbol"
        req(fpkm)
        
        if(update$checksurvival == "Overall"){
          osFile <- paste0(gene_datasets[i], "_", "clinical_OS", ".txt")
          readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
          if(!file.exists( isolate({readOSFile}))){
            shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
            stop()
          } 
          outcome <-  read.table(readOSFile, header=T, sep="\t")
          req(outcome)
          colnames(outcome)[2] <- "sample"
          
          samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
          samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
          rownames(samples) <- samples$sample
          
          if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
            shinyalert(paste0("Remove ", gene_datasets[i]), "Not enough events for survival analysis", type="error")
            stop()
          } 
          
        } else {
          
          osFile <- paste0(gene_datasets[i], "_", "clinical_OS", ".txt")
          readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
          if(!file.exists( isolate({readOSFile}))){
            shinyalert("No Data", "No clinical survival available for the chosen Dataset/Tumor Type", type="error")
            stop()
          }
          
          outcome <-  read.table(readOSFile, header=T, sep="\t")
          req(outcome)
          colnames(outcome)[2] <- "sample"
          
          samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
          samples$First.Event <- as.character(samples$First.Event)
          samples$Event.Status = "Event"
          samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
          samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
          rownames(samples) <- samples$sample
          
          if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
            shinyalert(paste0("Remove ", gene_datasets[i]), "Not enough events for survival analysis", type="error")
            stop()
          } 
          
        }
        
        fpkm$symbol <- toupper(fpkm$symbol)
        gene_fpkm_A <- fpkm[fpkm$symbol %in% gene_A, ]
        gene_fpkm_B <- fpkm[fpkm$symbol %in% gene_B, ]
        
        if(nrow(gene_fpkm_A)>1){
          max_index <- which.max(rowVars(gene_fpkm_A[,-c(1,2)]))
          gene_fpkm_A  <- gene_fpkm_A[max_index,]
        } 
        if(nrow(gene_fpkm_B)>1){
          max_index <- which.max(rowVars(gene_fpkm_B[,-c(1,2)]))
          gene_fpkm_B  <- gene_fpkm_B[max_index,]
        }
        
        #gene_fpkm <- (log(gene_fpkm_A[,-c(1,2)]+1, 2) - log(gene_fpkm_B[,-c(1,2)] + 1, 2) )
        gene_fpkm <- log( ( (gene_fpkm_A[,-c(1,2)]+0.0001)/(gene_fpkm_B[,-c(1,2)]+0.0001) ) + 1, 2)
        
        dt <- data.table(gene_fpkm)
        subset_fpkm <- dt[ ,intersect(names(dt),samples$sample), with=FALSE]
        gx <- as.matrix(subset_fpkm)
        rownames(gx) <- "genes ratio"
        
        if(sum(gx)==0 | nrow(gx)==0){
          shinyalert("Failed", "Gene not found or has no expression!", type="error")
          stop()
        }
        
        if(nrow(gx)>1){
          max_index <- which.max(rowVars(gx))
          scores <- as.matrix(gx[max_index, ])
          colnames(scores) <- "gx"
        } else {
          scores <- t(gx)
          colnames(scores) <- "gx"
        }
        
        ##Get the cut-point for each disease GSVA score
        if(update$checkgroup == "cutp"){
          if(update$checkgroup_cutp == "FALSE"){
            shinyalert("Please check Yes to proceed!", "", type="error")
            stop()
          } 
          
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="row.names", sort=FALSE)
          end <- grep("gx", colnames(subset_tils_cli))
          f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
          
          low_cpi <- signif(as.numeric(f),3)
          high_cpi <- signif(as.numeric(f),3)
          
          #identify samples within each low and high cut-points
          los = rownames(scores)[scores <= low_cpi]
          his = rownames(scores)[scores > high_cpi]
          scores <- data.frame(rownames(scores),scores)
          colnames(scores) <- c("sample","gx")
          scores$group[scores$sample %in% los] = "Low"
          scores$group[scores$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
          
        } else {
          
          if(update$checkgroup == "percentile"){
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[2]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            scores <- subset(scores, scores$group=="Low" | scores$group=="High")
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
            
          } 
          else {
            
            f <- std_cutp(scores, update$checkgroup, update$upper_per, update$lower_per)
            
            low_cpi <- signif(as.numeric(f[[1]]),3)
            high_cpi <- signif(as.numeric(f[[1]]),3)
            
            #identify samples within each low and high cut-points
            los = rownames(scores)[scores <= low_cpi]
            his = rownames(scores)[scores > high_cpi]
            scores <- data.frame(rownames(scores),scores)
            colnames(scores) <- c("sample","gx")
            scores$group[scores$sample %in% los] = "Low"
            scores$group[scores$sample %in% his] = "High"
            #samples common to both clinical and immmune cell proportion
            subset_tils_cli <- merge(samples, scores, by="sample", sort=FALSE)
            
            cpi <- c(low_cpi, high_cpi)
            
          }
        }
        cutp_vec[i] <- cpi
        scores_df[[i]] <- scores
        
        if(update$checksurvival =="Overall"){
          subset_tils_cli$Overall.Survival.Time.in.Months = (subset_tils_cli$Overall.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Overall.Survival.Time.in.Months, event=subset_tils_cli$Vital.Status=="Dead")
          
        } else {
          subset_tils_cli$Event.Free.Survival.Time.in.Months = (subset_tils_cli$Event.Free.Survival.Time.in.Days)/30.42
          subset_tils_cli$os = Surv(time=subset_tils_cli$Event.Free.Survival.Time.in.Months, event=subset_tils_cli$Event.Status=="Event")
        }
        
        #make low expression as the reference level
        subset_tils_cli$group = factor(subset_tils_cli$group, levels = c("Low", "High"))
        #stratify patients by group
        cox.os = coxph(os ~ group, data=subset_tils_cli) 
        km.os = survfit(os ~ group, data = subset_tils_cli, conf.type = "log-log")
        
        nlow <- km.os$n[1]
        nhigh <- km.os$n[2]
        
        lrpval[i] <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
        hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
        HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
        HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
        HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
        HR <- paste0(HR.ratio, " (", 
                     HR.confint.lower, "-", HR.confint.upper, ")")
        
        res <-c(as.character(gene_datasets[i]), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",",high_cpi), lr_pval)
        cox.tbl[2+i,] <- res
        rownames(cox.tbl)[2+i] <- as.character(gene_datasets[i])
        
        input_df[[i]] <- subset_tils_cli
      }
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(scores_df=scores_df, cpi=cutp_vec, input_df=input_df, lrpval=lrpval, cox.tbl=cox.dt.df, n=n, gene_datasets=gene_datasets))
      
      rm(scores_df, input_df, lrpval, cox.dt.df)
    })
    
    output$RATIO_plot <- renderPlot({
      req(getDataRATIO()$scores_df)
      req(getDataRATIO()$gene_datasets)
      
      index_to_plot <- which(input$diseaseSelector_ratio == getDataRATIO()$gene_datasets)
      gx_data <- getDataRATIO()$scores_df[[index_to_plot]]
      if(input$ratio_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(gx_data,input$diseaseSelector_ratio,"Log2 Genes Expression Ratio")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(gx_data,input$diseaseSelector_ratio,label="Log2 Genes Expression Ratio")
      }
    })
    
    
    output$os_pval_RATIO <- renderPlot({
      
      req(getDataRATIO()$input_df)
      req(getDataRATIO()$n)
      
      index_to_plot <- which(input$diseaseSelector_ratio == getDataRATIO()$gene_datasets)
      inputData <- getDataRATIO()$input_df[[index_to_plot]]
      
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5,
           main = paste0(update$checksurvival, " Survival", "\n", input$diseaseSelector_ratio),
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = paste0(update$user_geneA, "/", update$user_geneB, " ratio"), lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      
      #plot.data = fortify(km.os)
      #plot.data$info = paste0("n.risk:", plot.data$n.risk, "\n", "n.event:", plot.data$n.event, "\n", "n.censor:", plot.data$n.censor)
      #plot.data$grp_col = "white"
      #plot.data$grp_col[plot.data$strata=="Low"]="blue"
      #plot.data$grp_col[plot.data$strata=="High"]="red"
      
      #max_x <- max(plot.data$time)+10
      #min_x <- min(plot.data$time)+10
      
      #med_low <- summary(km.os)$table[, "median"][[1]] #low
      #med_high <- summary(km.os)$table[, "median"][[2]] #high
      #pval <- signif(summary(cox.os)$sctest["pvalue"], digits=3)
      #if(pval<=0.001){
      #  pval = "<0.001"
      #} else {
      #  pval <- signif(summary(cox.os)$sctest["pvalue"], digits=2)
      #}
      
      #pval_label <- paste0("p=", pval)
      
      #p1 <- ggplot(plot.data, aes(time, surv, group = strata, colour = grp_col, text=info))
      #p1 <- p1 + geom_point(size=4, shape=3, stroke=0.5, show.legend=FALSE)
      #p1 <- p1 + geom_step(size = 1.5, show.legend=TRUE)
      #p1 <- p1 + theme_classic()
      #p1 <- p1 + scale_color_manual(name=update$user_gene, labels = c('Low', 'High'), 
      #                              values = c('blue', 'red'), 
      #                              guide='legend')
      #p1 <- p1 + scale_x_continuous(breaks = seq(0, max_x, by = 50), limits = c(0, max_x))
      #p1 <- p1 + scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.2))
      #p1 <- p1 + xlab("Time (in months)")
      #p1 <- p1 + ylab('Survival Probability')
      #p1 <- p1 + theme(axis.title.y = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(axis.title.x = element_text(colour="black", face="plain", size=16.0))
      #p1 <- p1 + theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5))
      #p1 <- p1 + theme(axis.text=element_text(size=12,face="plain",colour="black"))
      #p1 <- p1 + guides(fill=guide_legend(title=update$user_gene))
      #p1 <- p1 + theme(legend.text = element_text(colour="black", size=14, face="bold"))
      #p1 <- p1 + theme(legend.title = element_text(colour="black", size=15, face="bold"))
      #p1 <- p1 + theme(legend.position = c(0.9, 0.9), legend.justification = c(1, 1))
      #p1 <- p1 + labs(fill ="", title=input$diseaseSelector_ratio) 
      
      #p1 <- p1 + annotate("text", x=min_x, y=0.05, label=pval_label, color="black", size=8, fontface="bold")
      #for high
      #p1 <- p1 + geom_segment(x = med_high, xend = med_high, y = -Inf, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_high, y = 0.5, yend = 0.5, colour = "red", size = 0.2, linetype="dashed")
      #for low
      #p1 <- p1 + geom_segment(x = med_low, xend = med_low, y = -Inf, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + geom_segment(x = -Inf, xend = med_low, y = 0.5, yend = 0.5, colour = "blue", size = 0.2, linetype="dashed")
      #p1 <- p1 + theme(plot.margin = unit(c(2,2,2,2),"pt"))
      #p1   
      #ggplotly(p1, tooltip = c("info"))
    })
    
    
    output$hrplot_RATIO <- renderPlot({
      
      req(getDataRATIO()$cox.tbl)
      req(getDataRATIO()$n)
      
      cox_tbl <- getDataRATIO()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=getDataRATIO()$n
      
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      pval[is.na(pval)] <- 1
      
      for (i in 1:n){
        #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
        if(pval[i] <= 0.05){
          b_clrs[i] = "red"
          l_clrs[i] = "black"
        } else {
          b_clrs[i] = "grey"
          l_clrs[i] = "grey"
        }
      }
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = paste0(update$user_geneA, "/", update$user_geneB, " ratio"),
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=FALSE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 #col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0, 0.2, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_RATIO <- renderPlotly({
      
      req(getDataRATIO()$cox.tbl)
      
      dfp <- getDataRATIO()$cox.tbl[-c(1:2),c(1,2,7,8)]
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, fontface="bold", show.legend = FALSE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        #labs(fill ="", title=update$checkdataset_ratio) + 
        guides(fill=guide_legend(title="")) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=3)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_RATIO <- renderPlot({
      
      req(update$checktumor)
      req(update$checktils)
      req(getDataRATIO()$gene_datasets)
      req(getDataRATIO()$scores_df)
      req(input$diseaseSelector_ratio)
      
      #if(update$check_correlation == TRUE){
      
      index_to_plot <- which(input$diseaseSelector_ratio == getDataRATIO()$gene_datasets)
      inputData <- getDataRATIO()$scores_df[[index_to_plot]]
      gx_scores <- inputData[,-3]
      colnames(gx_scores) <- c("sample", input$diseaseSelector_ratio)
      
      iccFile <- paste0("cibersoft_results_", input$diseaseSelector_ratio, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No cell proportions estimated for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      icc_sig_subset <- merge(icc_sig, gx_scores, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_sig_subset[,c(ncol(icc_sig_subset))]) #GX
      y <- as.matrix(icc_sig_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_sig_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_sig_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c(input$diseaseSelector_ratio, "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=6, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab(input$diseaseSelector_ratio) +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
      #}
    })
    
    
    getDataMAP  <- eventReactive(update$run_map, {
      
      mapFile <- paste0(update$checkdataset_for_MAP, "_", "clusterInfo", ".csv")
      readMAPFile <- paste0("data/TILs_MAP/", mapFile)
      if(!file.exists( isolate({readMAPFile}))){
        shinyalert("No Data", "No TIL index for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      map <- fread(readMAPFile, sep=",", header=T)
      req(map)
      dt_map <- data.table(map)
      map_uniq <- dt_map[,.SD[which.max(`number of clusters`)],by=list(sample)]
      rownames(map_uniq) <- map_uniq$sample
      
      if(update$checksurvival == "Overall"){
        osFile <- paste0(update$checkdataset_for_MAP, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/GDC_FPKM/", update$checktumor, "/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Overall.Survival.Time.in.Days) & !is.na(outcome$Vital.Status))
        samples = subset(samples, samples$Vital.Status == "Alive" | samples$Vital.Status == "Dead")
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Vital.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", checkdataset_for_MAP), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      } else {
        
        osFile <- paste0(update$checkdataset_for_MAP, "_", "clinical_OS", ".txt")
        readOSFile <- paste0("data/TARGET_EXCEL_CLINICAL/", osFile)
        if(!file.exists( isolate({readOSFile}))){
          shinyalert("No clinical survival available for the chosen Dataset and Tumor Type", type="error")
          stop()
        } 
        outcome <-  read.table(readOSFile, header=T, sep="\t")
        req(outcome)
        colnames(outcome)[2] <- "sample"
        
        samples = subset(outcome, !is.na(outcome$Event.Free.Survival.Time.in.Days) & !is.na(outcome$First.Event))
        samples$First.Event <- as.character(samples$First.Event)
        samples$Event.Status = "Event"
        samples$Event.Status[samples$First.Event=="None" & samples$Vital.Status=="Alive"] <- "NoEvent"
        samples$Event.Status[samples$First.Event=="Censored"  & samples$Vital.Status=="Alive"] <- "NoEvent"
        rownames(samples) <- samples$sample
        
        if(length(unique(factor(samples$Event.Status)))<2 | nrow(samples)<20){
          shinyalert(paste0("Remove ", checkdataset_for_MAP), "Not enough events for survival analysis", type="error")
          stop()
        } 
        
      }
      
      map_estimate <- as.matrix(as.numeric(map_uniq$til_percentage))
      rownames(map_estimate) <- map_uniq$sample
      colnames(map_estimate) <- "gx"
      
      ##Get the cut-point for each disease TIL %
      if(update$checkgroup == "cutp"){
        
        if(update$checkgroup_cutp == "FALSE"){
          shinyalert("Please check Yes to proceed!", "", type="error")
          stop()
        } 
        
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, map_estimate, by="row.names", sort=FALSE)
        end <- grep("gx", colnames(subset_tils_cli))
        f <- opt_cutp(subset_tils_cli, end, type=update$checksurvival)
        
        low_cpi <- signif(as.numeric(f),3)
        high_cpi <- signif(as.numeric(f),3)
        
        #identify samples within each low and high cut-points
        los = rownames(map_estimate)[map_estimate <= low_cpi]
        his = rownames(map_estimate)[map_estimate > high_cpi]
        map_estimate <- data.frame(rownames(map_estimate),map_estimate)
        colnames(map_estimate) <- c("sample","gx")
        map_estimate$group[map_estimate$sample %in% los] = "Low"
        map_estimate$group[map_estimate$sample %in% his] = "High"
        #samples common to both clinical and immmune cell proportion
        subset_tils_cli <- merge(samples, map_estimate, by="sample", sort=FALSE)
        
        cpi <- c(low_cpi, high_cpi)
        
      } else {
        
        if(update$checkgroup == "percentile"){
          f <- std_cutp(map_estimate, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[2]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(map_estimate)[map_estimate <= low_cpi]
          his = rownames(map_estimate)[map_estimate > high_cpi]
          map_estimate <- data.frame(rownames(map_estimate),map_estimate)
          colnames(map_estimate) <- c("sample","gx")
          map_estimate$group[map_estimate$sample %in% los] = "Low"
          map_estimate$group[map_estimate$sample %in% his] = "High"
          map_estimate <- subset(map_estimate, map_estimate$group=="Low" | map_estimate$group=="High")
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, map_estimate, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
        } 
        else {
          
          f <- std_cutp(map_estimate, update$checkgroup, update$upper_per, update$lower_per)
          
          low_cpi <- signif(as.numeric(f[[1]]),3)
          high_cpi <- signif(as.numeric(f[[1]]),3)
          
          #identify samples within each low and high cut-points
          los = rownames(map_estimate)[map_estimate <= low_cpi]
          his = rownames(map_estimate)[map_estimate > high_cpi]
          map_estimate <- data.frame(rownames(map_estimate),map_estimate)
          colnames(map_estimate) <- c("sample","gx")
          map_estimate$group[map_estimate$sample %in% los] = "Low"
          map_estimate$group[map_estimate$sample %in% his] = "High"
          #samples common to both clinical and immmune cell proportion
          subset_tils_cli <- merge(samples, map_estimate, by="sample", sort=FALSE)
          
          cpi <- c(low_cpi, high_cpi)
        }
      }
      return(data=list(scores=map_estimate, input=subset_tils_cli, cpi=cpi))
      
    })
    
    applycutptMAP <- eventReactive(update$run_map, {
      
      req(getDataMAP()$input)
      req(getDataMAP()$cpi)
      
      i=1
      
      low_cpi=as.numeric(getDataMAP()$cpi[[1]])
      high_cpi=as.numeric(getDataMAP()$cpi[[2]])
      
      lrpval <- vector(length=i)
      names(lrpval) <- update$checkdataset_for_MAP
      
      cox.tbl = data.frame(matrix(vector(), i, 10,
                                  dimnames=list(c(), c("Dataset", "HR_Ratio","HR_CI_low", "HR_CI_high", "nlow", "nhigh", "HR", "HR_Pvalue", "Cut_Point", "LR_Pvalue"))))
      
      res <-c("Dataset", "HR", "HR", "HR", "nLow", "nHigh", "HR", "HR", "Point", "LR")
      cox.tbl[1,] <- res
      res <-c("", "Ratio", "CI-lower", "CI-upper", "Cases", "Cases", "(95% CI)", "Pvalue", "Cut", "Pvalue")
      cox.tbl[2,] <- res
      
      inputData <- getDataMAP()$input
      inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
      inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      nlow <- km.os$n[1]
      nhigh <- km.os$n[2]
      
      lrpval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      lr_pval <-signif(summary(cox.os)$sctest["pvalue"], digits=3)
      hrpval <-signif(summary(cox.os)$wald["pvalue"], digits=3)
      HR.ratio <-signif(summary(cox.os)$coefficients[2], digits=2);
      HR.confint.lower <- signif(summary(cox.os)$conf.int[3], 2)
      HR.confint.upper <- signif(summary(cox.os)$conf.int[4],2)
      HR <- paste0(HR.ratio, " (", 
                   HR.confint.lower, "-", HR.confint.upper, ")")
      
      res <-c(as.character(update$checkdataset_for_MAP), HR.ratio, HR.confint.lower, HR.confint.upper, nlow, nhigh, HR, hrpval, paste0(low_cpi,",",high_cpi), lr_pval)
      cox.tbl[2+i,] <- res
      rownames(cox.tbl)[2+i] <- as.character(update$checkdataset_for_MAP)
      
      cox.dt <- data.table(cox.tbl)
      cox.dt <- cox.dt[order(-cox.dt$HR_Ratio, cox.dt$HR_Pvalue)]
      cox.dt.df <- data.frame(cox.dt)
      
      return(data=list(lrpval=lrpval, cox.tbl=cox.dt.df))
      
    })
    
    getDataCORR_MAP <- eventReactive(update$run_map, {
      
      #if(update$check_correlation == TRUE){
      
      req(update$checkdataset_for_MAP)
      req(update$checktumor)
      req(update$checktils)
      
      iccFile <- paste0("cibersoft_results_", update$checkdataset_for_MAP, "_", update$checktils, ".txt")
      readICCFile <- paste0("data/CIBERSOFT/", update$checktumor, "/", iccFile)
      if(!file.exists( isolate({readICCFile}))){
        shinyalert("No Data", "No estimated cell proportions for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      icc <- read.table(readICCFile, sep="\t", row.names=1)
      req(icc)
      icc <- data.frame(rownames(icc), icc)
      colnames(icc)[1] <- "sample"
      
      #subset samples by CIBERSOFT p-value <=0.05
      if(update$checkpvalue == TRUE){
        icc_sig <- subset(icc, icc$P.value<=0.05)
      } else {
        icc_sig <- icc
      }
      
      #if(nrow(icc_sig) < 20){
      #  shinyalert("Not enough samples to proceed", "Change selections and try again!", type="error")
      #  stop()
      #} 
      
      if(update$checktils == "LM22"){
        end = 22+1
      } else {
        end = 6+1
      }
      
      mapFile <- paste0(update$checkdataset_for_MAP, "_", "clusterInfo", ".csv")
      readMAPFile <- paste0("data/TILs_MAP/", mapFile)
      if(!file.exists( isolate({readMAPFile}))){
        shinyalert("No Data", "No TIL index for the chosen Dataset/Tumor Type", type="error")
        stop()
      } 
      map <- fread(readMAPFile, sep=",", header=T)
      req(map)
      
      map_estimate <- map[, sample, til_percentage]
      colnames(map_estimate) <- c("TIL_Percentage","sample")
      icc_map_subset <- merge(icc_sig, map_estimate, by="sample", sort=FALSE)
      
      x <- as.matrix(icc_map_subset[,c(ncol(icc_map_subset))]) #MAP
      y <- as.matrix(icc_map_subset[,c(2:end)]) #cell prop
      cor.coef <- cor(x, y)
      cor.coef[is.na(cor.coef)] <- 0
      
      icc_map_subset <- cbind(x, y) #combined matrix for p-value
      p.coef <- cor.mtest(icc_map_subset)
      p.coef_p <- p.coef$p[1,-1]
      p.coef_p[is.na(p.coef_p)] <- 1
      p <- rbind(cor.coef, p.coef_p)
      rownames(p) <- c("TIL_Percentage", "pvalue")
      tp <- data.frame(t(p))
      tp$cell <- rownames(tp)
      dm <- melt(tp, id.vars=c("cell", "pvalue"))
      return(dm)
      #}
    })
    
    output$map_plot <- renderPlot({
      req(getDataMAP()$scores)
      
      if(input$map_choice == TRUE){
        par(mar=c(4.1, 6.1, 4.1, 6.1))
        waterfallplot(getDataMAP()$scores,update$checkdataset_for_MAP,"TIL%")
      } else {
        par(mar=c(4.1, 6.1, 4.1, 5.1))
        generate_boxplot(getDataMAP()$scores, update$checkdataset_for_MAP, "TIL%")
      }
    })
    
    output$os_pval_MAP <- renderPlot({
      req(getDataMAP()$input)
      
      inputData <- getDataMAP()$input
      los<-inputData$group[inputData$group=="Low"]
      his<-inputData$group[inputData$group=="High"]
      
      inputData$Overall.Survival.Time.in.Months = (inputData$Overall.Survival.Time.in.Days)/30.42
      inputData$os = Surv(time=inputData$Overall.Survival.Time.in.Months, event=inputData$Vital.Status=="Dead")
      
      
      inputData$group = factor(inputData$group, levels = c("Low", "High"))
      cox.os = coxph(os ~ group, data=inputData) 
      km.os = survfit(os ~ group, data = inputData, conf.type = "log-log")
      
      low.col = rgb(0, 0, 0.5)
      high.col = rgb(0.5, 0, 0)
      #cols = c(low.col, high.col)
      cols = c("royalblue", "maroon")
      hist.col = rgb(0.5, 0.5, 0.5)
      
      par(mar=c(4.1, 6.1, 4.1, 3.1)) # adapt margins
      plot(km.os, cex.lab = 2.0, cex.main = 2.0, cex.axis=1.5, 
           main = paste0(update$checksurvival, " Survival", "\n", update$checkdataset_for_MAP),
           xlab = "Time in Months", 
           ylab = "Survival Probability", 
           mark.time = T, col = cols, lwd=3, lty=2)
      legend("topright", c(paste0("Low (n=", length(los), ")"), paste0("High (n=", length(his), ")")), title = "TIL%", lwd = 4, col = cols, bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.6)
      legend("bottomleft", paste0("Logrank p=", signif(summary(cox.os)$sctest["pvalue"], digits=2)),bty = "n", cex = 2.0, border = NA, seg.len = 0.5, x.intersp = 0.5)
      #text(400,0,labels=paste0("n=", length(los)), adj=c(2,-1), col=low.col)
      #text(x.median,0,labels=paste0("n=", length(his)), adj=c(-1,-1), col=high.col)
    })
    
    output$hrplot_MAP <- renderPlot({
      req(applycutptMAP()$cox.tbl)
      
      cox_tbl <- applycutptMAP()$cox.tbl
      mean <- as.numeric(cox_tbl[,2])
      lower <- as.numeric(cox_tbl[,3])
      upper <- as.numeric(cox_tbl[,4])
      metadata <- cbind(mean, lower, upper)
      colnames(metadata) <- c("mean", "lower", "upper")
      
      labeltext <- cox_tbl
      labeltext <- labeltext[,-c(2:4)]
      
      labeltext <- labeltext[,c(1,2,3,6,4,5,7)]
      
      n=1
      b_clrs <- vector(length=n)
      l_clrs <- vector(length=n)
      pval <- vector(length=n)
      pval <- as.numeric(cox_tbl[-c(1:2),8])
      
      i=1
      #for (i in 1:getData()$n){
      #if(pval[i]=="-Inf" | pval[i]=="Inf" | is.na(pval[i]) | pval[i]>0.05){
      if(pval[i] <=0.05){
        b_clrs[i] = "red"
        l_clrs[i] = "black"
      } else {
        b_clrs[i] = "grey"
        l_clrs[i] = "grey"
      }
      #}
      
      fn <- local({
        i=0
        function(..., clr.line, clr.marker){
          i <<- i + 1
          fpDrawNormalCI(..., clr.line = l_clrs[i], clr.marker = b_clrs[i])
        }
      })
      
      #tabletext <- list(list(), list()) #Creating a list with "sublists" for each column
      #tabletext[[1]] <- rownames(cox_tbl)
      #tabletext[[2]][1] <- list(expression(paste(italic("r"), " = .42"))) #manual way using expression and paste
      #tabletext[[2]][2] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=HRQoL$Sweden[,2]))) #need substitute function to access variables
      #tabletext[[2]][3] <- list(substitute(expression(paste(italic("r"),"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3])))) #The substitute functions allows addicitonal manipulation of strings to be put back into the expression
      #tabletext[[2]][4] <- list(substitute(expression(paste(italic("t")[df],"=",r_string)), list(r_string=sprintf("%.3f", HRQoL$Sweden[,3]), df="23"))) #One can also substitute multiple elements of expression, use subscripts etc. 
      #tabletext[[1]][4] <- list(expression(bold("Make single line bold"))) #Manipulate strings manually, using unicode
      
      
      forestplot(labeltext, metadata, new_page = TRUE,
                 fn.ci_norm = fn,
                 title = "TIL Percentage",
                 is.summary=c(TRUE,TRUE,rep(FALSE,n)),
                 graph.pos = 5,
                 txt_gp = fpTxtGp(label = list(gpar(fontfamily = "Times", fontface="plain", col="black", cex=1.5),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2),
                                               gpar(fontfamily = "", fontface="italic", col = "black", cex=1.2)
                 ),
                 title = gpar(fontfamily = "", col = "black", cex=2.0),
                 summary = gpar(fontfamily = "", col = "black", cex=1.3, fontface="bold"),
                 ticks = gpar(fontfamily = "", cex=1),
                 xlab  = gpar(fontfamily = "HersheySerif", cex = 1.3, fontface="bold.italic")),
                 
                 #txt_gp = fpTxtGp(label = lapply(high_label, 
                 #                                function(val)  gpar(fontface = val)), ticks=gpar(cex=0.8), title=gpar(fontface = "bold", cex=2), xlab=gpar(fontface = "bold", cex=1)),
                 hrzl_lines = list("3" = gpar(lty=2)),
                 clip=c(0.1,2.5), 
                 xlog=TRUE, 
                 xlab=paste0("\n","<--Better Survival ~~~ Poorer Survival-->"),
                 col=fpColors(box="black", lines="black", zero = "gray50"),
                 zero=1, cex=1.2, 
                 lineheight = "auto", 
                 boxsize=0.3, 
                 colgap=unit(8,"mm"), 
                 graphwidth = unit(10, "cm"),
                 lwd.ci=2, 
                 ci.vertices=TRUE,
                 ci.vertices.height = 0.4,
                 xticks=c(0.1, 0.25, 0.5, 1, 1.5, 2, 3))
      #col=fpColors(box="royalblue", line="darkblue", hrz_lines = "#444444")
      #, vertices = TRUE,
      #graphwidth = unit(6, "cm"))
    })
    
    output$hr_scp_MAP <- renderPlotly({
      
      req(applycutptMAP()$cox.tbl)
      
      dfp <- data.frame(applycutptMAP()$cox.tbl[-c(1:2),c(1,2,7,8)])
      #dfp$dataset <- rownames(dfp)
      
      x_vec <- as.vector(dfp$HR_Ratio)
      y_vec <- as.vector(dfp$HR_Pvalue)
      x_vec <- as.numeric(x_vec)
      y_vec <- as.numeric(y_vec)
      
      dfp$x_vec <- x_vec
      dfp$y_vec <- y_vec
      
      min_x <- 0
      max_x <- max(x_vec)+1.0
      dfp$sig <- "p<0.05"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec>1.0]="Adv"
      dfp$sig[dfp$y_vec<=0.05 & dfp$x_vec<=1.0]="Fav"
      dfp$y_vec <- -log(dfp$y_vec,10)
      
      pal <- c("red", "blue", "grey")
      pal <- setNames(pal, c("Adv", "Fav", "p<0.05"))
      
      dfp$HR = paste0(dfp$Dataset, "\n", dfp$HR, "\n", "p=", dfp$HR_Pvalue)
      
      bp <- ggplot(data=dfp,  aes(x=x_vec, y=y_vec, colour=factor(sig), text=HR))
      bp <- bp + geom_point(size=2)
      bp <- bp + scale_color_manual("", values=pal)
      bp <- bp + geom_text(aes(label=Dataset),hjust=0, vjust=0, size = 4, show.legend = TRUE)
      
      #bp <- bp + geom_label_repel(aes(label = abbr),
      #                            box.padding   = 0.35, 
      #                            point.padding = 0.5,
      #                            segment.color = 'grey50',
      #                            show.legend = FALSE)
      bp <- bp + theme_classic() + 
        theme(axis.title.y = element_text(colour="black", face="plain", size=16.0)) + 
        theme(axis.title.x = element_text(colour="black", face="plain", size=16.0)) + 
        theme(plot.title = element_text(colour="black", face="bold", size=15.0, hjust=0.5)) + 
        theme(panel.background = element_rect(fill="NA")) + 
        theme(panel.border = element_rect(colour = "black", fill="NA")) + 
        theme(panel.grid.major.y = element_line(colour="NA")) + 
        theme(panel.grid.minor = element_line(colour = "NA")) + 
        theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, colour="black", size=12)) + 
        theme(axis.text.y = element_text(hjust = 1.0, vjust = 0.5, colour="black", face="plain", size=12)) +
        theme(legend.position="none") + 
        #theme(legend.text =  element_text(hjust = 1.0, vjust = 0.5, colour="black", face="bold", size=16.0)) +
        ylab("-Log10(HR P-value)") + 
        xlab("HR Ratio") + 
        xlim(0, max_x) + 
        guides(fill=guide_legend(title="")) + 
        #labs(fill ="", title=update$checkdataset_for_MAP) + 
        geom_vline(xintercept = 1, color="grey40", lty=4) + 
        geom_hline(yintercept = 1.30103, color="grey40", lty=4)
      
      bp <- ggplotly(bp, tooltip=c("HR"))
      
    })
    
    output$CORR_plot_MAP <- renderPlot({
      req(getDataCORR_MAP())
      dm <- getDataCORR_MAP()
      
      dm$shape <- "1"
      dm$shape[dm$pvalue<=0.05] = "22"
      
      dm$size = 1
      dm$size[dm$pvalue<=0.05] = 1.4
      
      dm <- dm %>%
        #arrange(desc(value)) %>%
        arrange(desc(pvalue)) %>%
        mutate(text = paste0("Cell Type: ", cell, "\n",
                             "Correlation: ", value, "\n",
                             "P-value: ", pvalue))
      
      dm$cell <- factor(dm$cell, levels = dm$cell)
      
      cp <- ggplot(dm, aes(x=cell, y=variable, text=text, fill=value, color=value)) +
        geom_point(aes(shape=factor(shape)), size=5, show.legend = TRUE) +
        scale_colour_gradient2(name = paste0("Correlation-", "\n", "coefficient"),
                               low="blue", mid="grey", high="red",
                               midpoint = 0,
                               guide = 'legend') +
        guides(color = guide_colorbar(reverse=FALSE)) + 
        scale_fill_gradient2(name = "Correlation(r)",
                             low="blue", mid="grey", high="red",
                             midpoint = 0,
                             guide = 'legend') +
        guides(fill=FALSE) +  
        scale_shape_manual(name = 'P-value',
                           values = c(22, 1),
                           breaks = c("22","1"),
                           labels = c('<=0.05','>0.05'),
                           guide = 'legend') +
        ylab("TIL%") +
        xlab("") +
        theme_classic() +
        theme(axis.text.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.text.x = element_blank()) +
        theme(axis.title.y = element_text(colour="black",face="bold",size=14.0)) +
        theme(axis.title.x = element_text(colour="black",face="bold",size=14.0)) +
        theme(legend.title=element_text(size=12, face="bold"), 
              legend.text=element_text(size=12)) + 
        coord_flip() + 
        theme(plot.margin = unit(c(2,10,2,12),"pt"))
      cp
      #cp1 <- ggplotly(cp, tooltip=c("text"))
      #cp2 <- cp1 %>% style(cp1, showlegend = FALSE)
    })
    
    
  })


