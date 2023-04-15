#load packages
pacman::p_load(shiny, sf, tidyverse, tools, DT, shinythemes, plotly, shinyWidgets, shinyjs, tmap, 
               leaflet, ggcorrplot, olsrr, ggpubr, spdep, sfdep, GWmodel, gtsummary, spgwr, shinycssloaders,
               htmltools)

#load data files
subdistrict <- readRDS("data/proj_dataset.rds")

#remove geometry col
subdistrict_non_geo <- subdistrict %>% st_drop_geometry()

#dependent variables
y <- "vaccination_rate"

#independent variables
x <- names(subdistrict_non_geo)[sapply(subdistrict_non_geo, is.numeric)& names(subdistrict_non_geo) != "vaccination_rate"]

#spatial weight computation methods
sw_computation <- c("Fixed Distance"="fd",
                    "Adaptive Distance"="ad")

#bandwidth options
bw_options <- c("Fixed"=FALSE,
                "Adaptive"=TRUE)

#approach options
approach_options <- c("Cross-Validation" = "CV",
                      "Least Akaike Information Criterion" = "AIC")

#kernel options
kernel_options <- c("Gaussian" = "gaussian",
                    "Exponential" = "exponential",
                    "Bisquare" = "bisquare",
                    "Tricube" = "tricube",
                    "Boxcar" = "boxcar")

#distance options
distance_options <- c("Euclidean" =2,
                      "Manhattan" = 1)

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("journal"),
                #enable shinyjs
                useShinyjs(),
                
                #nav bar
                navbarPage("regress Rangers", fluid=TRUE, windowTitle="Simple Geospatial Analysis using R and Shiny", selected = "intro",
                           # Intro window
                           tabPanel(
                             "Introduction", value="intro", fluid=TRUE, icon=icon("hand"),
                             # Introduce the team and purpose of the app
                             tags$div(
                               style = "text-align:center;",
                               h3("Welcome to our Application!"),
                               "This app was made by the RegressRangers team to help you easily create geographically weighted regression models."
                             ),
                             
                             # Introduce the problem the app is trying to solve
                             tags$div(
                               style = "text-align:left; padding-top: 10px;",
                               h4("Problem Statement"),
                               "Many real-world phenomena exhibit significant spatial variability, and traditional regression models that assume spatial homogeneity fail to capture the underlying patterns in the data set collected. Geographically weighted regression models are an effective way to address this problem by allowing for local variation in the relationship between the independent variable and the dependent variables. However, creating accurate geographically weighted regression models can be a complex and time-consuming process that requires specialized knowledge and software. Additionally, evaluating the performance of these models can be challenging due to the local nature of the models."
                             ),
                             
                             # Introduce the objectives of the app
                             tags$div(
                               style = "text-align:left; padding-top: 10px;",
                               h4("Objectives"),
                               "Our project aims to create a web-based geospatial analytics tool that helps users create and evaluate geographical regression models easily by:",
                               tags$table(
                                 tags$li("identifying significant regressors in the model 🔍"),
                                 tags$li("creating customized Linear and Geographically Weighted Models 📈"),
                                 tags$li("observing results of the models based on a variety of statistical measures 🧪")
                               )
                             ),
                             
                             # Introduce the case study used in the demo
                             tags$div(
                               style = "text-align:left; padding-top: 10px;",
                               h4("Application Demo"),
                               "We will be regressing the COVID-19 vaccination rate in sub-districts within DKI Jakarta. With the availability of COVID-19 vaccination data, we can gain important insights into the progress of immunization campaigns and help identify variables that influence vaccine adoption across the different sub-dstricts in DKI Jakarta.",
                             ),
                             
                             # Contact us
                             tags$div(
                               style = "text-align: left; padding-top: 10px;",
                               h4("RegressRangers Team"),
                               tableOutput("contact")
                             )
                           ),
                           #Data Window
                           tabPanel("Data", value="data", fluid=TRUE, icon=icon("database"),
                                    titlePanel("View Data Table"),
                                    sidebarLayout(
                                      
                                      #sidebar panel
                                      sidebarPanel(width=3,
                                        
                                        selectInput("subdistrict_cols", "Select Columns:",
                                                                  choices = colnames(subdistrict_non_geo),
                                                                  selected = head(colnames(subdistrict_non_geo),4),
                                                                  multiple = TRUE),

                                                      checkboxInput("show_cols_3", "Show Column Filter"),

                                        fluidRow(
                                          column(6,
                                                 actionButton("show_subdistrict_table", "Refresh Table", icon = icon("refresh")),
                                          ),
                                          column(
                                            6,
                                            conditionalPanel(
                                              "input.show_subdistrict_table > 0",
                                              actionButton("info_button_d", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                            )
                                          )
                                        )
                                      ),
                                      
                                      #show contents of chosen file as a table output
                                      mainPanel(
                                        DT::dataTableOutput("sdtable"),
                                        br(),
                                        fluidRow(
                                          column(
                                            12,
                                            align="center",
                                            uiOutput("info_message_d"),
                                          ),
                                        ),
                                      )
                                    )
                          ),
                          #EDA Window
                          tabPanel("EDA", value="eda", fluid=TRUE, icon=icon("search"),
                                   titlePanel("Exploratory Data Analysis"),
                                   sidebarLayout(position="left", fluid=TRUE,
                                                 sidebarPanel(width=3, fluid=TRUE,
                                                              conditionalPanel(
                                                                condition = "input.EDA_Tabset == 'Bivariate Analysis'",
                                                                selectInput(
                                                                  "x",
                                                                  "Select Independent Variables:",
                                                                  choices = x,
                                                                  selected = head(x, 1),
                                                                  multiple = FALSE
                                                                ),
                                                                selectInput(
                                                                  "y",
                                                                  "Select Dependent Variable:",
                                                                  choices = y,
                                                                  multiple = FALSE),
                                                                # actionButton("eda_BA", "Update Plots", icon = icon("refresh"))
                                                                fluidRow(
                                                                  column(6,
                                                                         actionButton("eda_BA", "Update Plots", icon = icon("refresh")),
                                                                  ),
                                                                  column(
                                                                    6,
                                                                    conditionalPanel(
                                                                      "input.eda_BA > 0",
                                                                      actionButton("info_button_ba", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                                                    )
                                                                  )
                                                                )
                                                                ),
                                                              
                                                              conditionalPanel(
                                                                'input.EDA_Tabset == "Correlation Analysis"',
                                                                selectInput(
                                                                  "cp_measures",
                                                                  "Select Measures:",
                                                                  choices = x,
                                                                  selected= head(x, 5),
                                                                  multiple=TRUE
                                                                ),
                                                                # actionButton("eda_CA", "Update Plot", icon = icon("refresh"))
                                                                fluidRow(
                                                                  column(6,
                                                                         actionButton("eda_CA", "Update Plots", icon = icon("refresh")),
                                                                  ),
                                                                  column(
                                                                    6,
                                                                    conditionalPanel(
                                                                      "input.eda_CA > 0",
                                                                      actionButton("info_button_cp", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                                                    )
                                                                  )
                                                                )
                                                                ),
                                                 ),
                                                 mainPanel(width=9, fluid=TRUE,
                                                           tabsetPanel(
                                                             id = 'EDA_Tabset',
                                                             tabPanel("Bivariate Analysis",
                                                                      fluidRow(
                                                                        column(7,
                                                                               plotlyOutput(outputId="eda_sp", height="100%", width="100%"),
                                                                        ),
                                                                        column(5, 
                                                                               plotlyOutput(outputId = "eda_map", height="100%", width="100%")
                                                                        ),
                                                                      ),
                                                                      br(),
                                                                      fluidRow(
                                                                        column(
                                                                          12,
                                                                          align="center",
                                                                          uiOutput("info_message_ba"),
                                                                        ),
                                                                      ),
                                                            ),
                                                            tabPanel("Correlation Analysis",
                                                                     plotlyOutput("eda_cp", height="100%", width="100%"),
                                                                     br(),
                                                                     fluidRow(
                                                                       column(
                                                                         12,
                                                                         align="center",
                                                                         uiOutput("info_message_cp"),
                                                                       ),
                                                                     ),
                                                                     
                                                                     
                                                            )
                                                          )
                                                )
                                  )
                         ),
                         #Model Assumption Window
                         tabPanel("Model Assumption", value="esda", fluid=TRUE, icon=icon("globe"),
                                    titlePanel("Checking Model Assumptions"),
                                    sidebarLayout(position="left", fluid=TRUE,
                                                  sidebarPanel(width=3, fluid=TRUE,
                                                               conditionalPanel(
                                                                 'input.ESDA_Tabset == "Linearity & Normality Assumption"',
                                                                 selectInput("esda_ln_x",
                                                                             "Select Independent Variables:",
                                                                             choices=x,
                                                                             selected=head(x, 5),
                                                                             multiple=TRUE
                                                                 ),
                                                                 selectInput("esda_ln_y",
                                                                             "Select Dependent Variable:",
                                                                             choices=y,
                                                                             # selected=head(y, 1),
                                                                             multiple=FALSE
                                                                 ),
                                                                 numericInput(
                                                                   "crs",
                                                                   "Insert Coordinate Reference System:",
                                                                   value = 23845),
                                                                 fluidRow(
                                                                   column(6,
                                                                          actionButton("esda_LN", "Update Plots", icon = icon("refresh")),
                                                                   ),
                                                                   column(
                                                                     6,
                                                                     conditionalPanel(
                                                                       "input.esda_LN > 0",
                                                                       actionButton("info_button_ln_assumption", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                                                     )
                                                                   )
                                                                 )
                                                               ),
                                                               conditionalPanel(
                                                                 "input.ESDA_Tabset == 'Spatial Autocorrelation'",
                                                                 selectInput("gwr_x",
                                                                             "Select Independent Variables:",
                                                                             choices=x,
                                                                             selected=head(x, 5),
                                                                             multiple=TRUE
                                                                 ),
                                                                 selectInput("gwr_y",
                                                                             "Select Dependent Variable:",
                                                                             choices=y,
                                                                             selected=head(y, 1),
                                                                             multiple=FALSE
                                                                 ),
                                                                 numericInput(
                                                                   "crs_2",
                                                                   "Insert Coordinate Reference System:",
                                                                   value = 23845),
                                                                 selectInput(inputId="moran_I_analysis",
                                                                             label="Select Spatial Weight Method:",
                                                                             choices=sw_computation,
                                                                             selected=head(sw_computation, 1),
                                                                             multiple=FALSE
                                                                 ),
                                                                 conditionalPanel(
                                                                   "input.moran_I_analysis == 'ad'",
                                                                   {sliderInput("k",
                                                                                "Select Number of Neighbours:",
                                                                                min = 2,
                                                                                max = 50,
                                                                                value = 5)}
                                                                 ),
                                                                 fluidRow(
                                                                   column(6,
                                                                          actionButton("gwr_model", "Update Plots", icon = icon("refresh")),
                                                                   ),
                                                                   column(
                                                                     6,
                                                                     conditionalPanel(
                                                                       "input.gwr_model > 0",
                                                                       actionButton("info_button_spatial_autocorrelation", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                                                     )
                                                                   )
                                                                 )
                                                               )
                                                               
                                                  ),
                                                  mainPanel(width=9, fluid=TRUE,
                                                            tabsetPanel(
                                                              id = 'ESDA_Tabset',
                                                              tabPanel("Linearity & Normality Assumption",
                                                                       fluidRow(
                                                                         column(width = 12,
                                                                                plotlyOutput("linearity_assumption"))
                                                                       ),
                                                                       fluidRow(
                                                                         column(width = 7,
                                                                                plotlyOutput("normality_assumption")),
                                                                         br(),
                                                                         br(),
                                                                         column(width = 5,
                                                                                verbatimTextOutput("ols_results"))
                                                                       ),
                                                                       br(),
                                                                       fluidRow(
                                                                         column(
                                                                           12,
                                                                           align="center",
                                                                           uiOutput("info_message_ln_assumption"),
                                                                         ),
                                                                       ),
                                                              ),
                                                              tabPanel(
                                                                "Spatial Autocorrelation",
                                                                fluidRow(
                                                                  column(width=6,
                                                                         tmapOutput("ResidualPlot")
                                                                  ),
                                                                  column(width=6,
                                                                         verbatimTextOutput("sa_error"),
                                                                         verbatimTextOutput("moran_i"),
                                                                  )
                                                                ),
                                                                br(),
                                                                fluidRow(
                                                                  column(
                                                                    12,
                                                                    align="center",
                                                                    uiOutput("info_message_spatial_autocorrelation"),
                                                                  ),
                                                                ),
                                                              )
                                                            )
                                                  )
                                    )
                           ),
                           #GWR Window
                           tabPanel("GWR Results", value="gwr", fluid=TRUE, icon=icon("layer-group"),
                                    titlePanel("Results of Geographically Weighted Regression Model"),
                                    sidebarLayout(position="left", fluid=TRUE,
                                                  sidebarPanel(width=3, fluid=TRUE,
                                                               selectInput("gwr_x_output",
                                                                           "Select Independent Variables:",
                                                                           choices=x,
                                                                           multiple=TRUE
                                                               ),
                                                               selectInput("gwr_y_output",
                                                                           "Select Dependent Variable:",
                                                                           choices=y,
                                                                           selected=head(y, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               numericInput(
                                                                 "crs_3",
                                                                 "Insert Coordinate Reference System:",
                                                                 value = 23845),
                                                               selectInput("gwr_bw",
                                                                           "Select Type of Bandwidth:",
                                                                           choices=bw_options,
                                                                           selected=head(bw_options, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               selectInput("gwr_bw_approach",
                                                                           "Select Bandwidth Calibration Method:",
                                                                           choices=approach_options,
                                                                           selected=head(approach_options, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               selectInput("gwr_bw_distance",
                                                                           "Select Distance Metric:",
                                                                           choices=distance_options,
                                                                           selected=head(distance_options, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               selectInput("gwr_bw_kernel",
                                                                           "Select Computation Function:",
                                                                           choices=kernel_options,
                                                                           selected=head(kernel_options, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               selectInput("coeff_selected",
                                                                           "Select Coefficient Plot Variable:",
                                                                           choices=NULL,
                                                                           # selected=head(x, 1),
                                                                           multiple=FALSE
                                                               ),
                                                               fluidRow(
                                                                 column(6,
                                                                        actionButton("gwr_results", "Update Plots", icon = icon("refresh")),
                                                                ),
                                                                column(
                                                                  6,
                                                                  conditionalPanel(
                                                                    "input.gwr_results > 0",
                                                                    actionButton("info_button_gwr", "Show Info", style = "color: #FFF; background-color: #ee7964; border-color: #ee7964;")
                                                                  )
                                                                )
                                                               )
                                                               
                                                  ),
                                                  mainPanel(width=9, fluid=TRUE,
                                                            fluidRow(
                                                              column(
                                                                width=6,
                                                                tmapOutput("R2_plot")
                                                              ),
                                                              column(
                                                                width=6,
                                                                tmapOutput("coeff_plot")
                                                              )
                                                            ),
                                                            br(),
                                                              fluidRow(
                                                                column(
                                                                  12,
                                                                  align="center",
                                                                  uiOutput("info_message_gwr"),
                                                                ),
                                                              ),
                                                  )
                                    ),
                           )
                           
                           
                           
                )
)

# Define server logic
server <- function(input, output, session) {
  
  # Data Tab
  
  subdistrict_data <- eventReactive(input$show_subdistrict_table, {
    selected_cols <- c(input$subdistrict_cols)
    subdistrict_non_geo %>%
      select(all_of(selected_cols)) %>%
      mutate_if(is.numeric, round, digits = 3)
  })
  
  subdistrict_filter <- eventReactive(input$show_subdistrict_table, {
    do_filter <- ifelse(input$show_cols_3, "top", "none")
  })
  
  output$sdtable <- DT::renderDataTable({
    subdistrict_data()
  },
  filter = subdistrict_filter(), 
  options = list(scrollX = TRUE,
                 columnDefs = list(list(targets = "_all", searchable = TRUE))))
  
  #EDA Tab
  scatter_plot_data <- eventReactive(input$eda_BA, {
    data <- highlight_key(subdistrict, ~subdistrict)
  })
  
  output$eda_sp <- renderPlotly({
    scatter_plot <- ggplot(scatter_plot_data(), aes_string(isolate(input$x), isolate(input$y))) +
      geom_point(alpha=0.6,
                 color="#08519c") +
      geom_smooth(method=lm) +
      theme_classic()

    ggplotly(scatter_plot) %>%
      highlight(on="plotly_selected", off="plotly_deselect",
                opacityDim=0.4)
  })
  

  output$eda_map <- renderPlotly({
    plot2 <- ggplot(scatter_plot_data()) +
      geom_sf(alpha=0.6,
              fill="#bdd7e7",
              lwd=0.5,
              color="#08519c") +
      theme_classic()
    ggplotly(plot2) %>%
      highlight(on="plotly_click", off="plotly_doubleclick",
              opacityDim=0.4)
  })

  corr_plot_data <- eventReactive(input$eda_CA, {
    data <- subdistrict_non_geo %>% ungroup() %>% select(input$cp_measures)
    return (data)
  })

  output$eda_cp <- renderPlotly({
    corr_matrix = cor(corr_plot_data())
    p <- ggcorrplot(corr_matrix, hc.order = TRUE,
                    ggtheme=ggplot2::theme_minimal, outline.col="black")

    main_border <- annotate("rect", xmin = 0.5, xmax = ncol(corr_matrix) + 0.5,
                            ymin = 0.5, ymax = nrow(corr_matrix) + 0.5,
                            fill = NA, color = "black")
    cell_borders <- geom_rect(data = expand.grid(x = 1:nrow(corr_matrix), y = 1:ncol(corr_matrix)),
                              aes(xmin = x-0.5, xmax = x+0.5, ymin = y-0.5, ymax = y+0.5),
                              fill = NA, color = "black", inherit.aes = FALSE)
    p+main_border+cell_borders
  })
  
  #Model Assumption Tab
  observeEvent(input$cp_measures, {
    updateSelectInput(session, "esda_ln_x", choices = x, selected=input$cp_measures)
  })
  
  ln_model <- eventReactive(input$esda_LN, {
    data <- st_transform(subdistrict, input$crs)
    formula <- as.formula(paste(input$esda_ln_y, "~", paste(input$esda_ln_x, collapse = "+")))
    model <- lm(formula, data = data)

    return(model)
  })
  
  output$linearity_assumption <- renderPlotly({
    ols_plot_resid_fit(ln_model())
  })

  output$normality_assumption <- renderPlotly({
    ols_plot_resid_hist(ln_model())
  })

  output$ols_results <- renderPrint({
    ols_test_normality(ln_model())
  })
  
  observeEvent(input$esda_ln_x, {
    updateSelectInput(session, "gwr_x", choices = x, selected = input$esda_ln_x)
  })
  
  ln_model2 <- eventReactive(input$gwr_model, {
    data <- st_transform(subdistrict, input$crs_2)
    formula <- as.formula(paste(input$gwr_y, "~", paste(input$gwr_x, collapse = "+")))
    model <- lm(formula, data = data)
    
    return(model)
  })
  

  spatial_weight <- eventReactive(input$gwr_model,{
    sw_method <- input$moran_I_analysis
    data <- st_transform(subdistrict, input$crs_2)
    lat <- map_dbl(data$geometry, ~st_centroid(.x)[[2]])
    lng <- map_dbl(data$geometry, ~st_centroid(.x)[[1]])
    coords <- cbind(lng, lat)
    if (sw_method == "fd") {
      k1 <- knn2nb(knearneigh(coords))
      k1dists <- unlist(nbdists(k1, coords, longlat=TRUE))
      max_dist <- max(k1dists)
      weight_matrix <- dnearneigh(coords, 0, round(max_dist + 1), longlat=TRUE)
      nb_lw <- nb2listw(weight_matrix, style = 'W', zero.policy=TRUE)

    } else {
      # adaptive distance spatial weights
      weight_matrix <- knn2nb(knearneigh(coords,k=input$k))
      nb_lw <- nb2listw(weight_matrix, style = 'W', zero.policy=TRUE)
    }

    return(nb_lw)
  })
  

  output$ResidualPlot <- renderTmap({
    original_data <- st_transform(subdistrict, input$crs_2)
    plot_data <- cbind(original_data, ln_model2()$residuals)
    tm_shape(plot_data) +
      tm_fill(col = "ln_model2...residuals",
              alpha = 0.6,
              style="quantile",
              title = "Residuals") +
      tm_layout(title.position = "centertop") +
      tm_borders(alpha = 0.5) +
      tm_view(set.zoom.limits = c(10.5,14), view.legend.position = c("left", "bottom"))+
      tm_scale_bar() +
      tm_grid(alpha =0.2)
  })

  output$moran_i <- renderPrint({
    lm.morantest(ln_model2(), spatial_weight())
  })
  
  observeEvent(input$gwr_x, {
    updateSelectInput(session, "gwr_x_output", choices = x, selected=input$gwr_x)
  })

  observeEvent(input$gwr_x_output, {
    updateSelectInput(session, "coeff_selected", choices = input$gwr_x_output, selected=head(input$gwr_x_output, 1))
  })
  
  gwr_model_output <- eventReactive(input$gwr_results,{
    data <- subdistrict %>% st_transform(crs=input$crs_3) %>% as_Spatial()
    formula <- as.formula(paste(input$gwr_y_output, "~", paste(input$gwr_x_output, collapse = "+")))
    bw <- bw.gwr(formula = formula,
                 data = data,
                 adaptive = as.logical(input$gwr_bw),
                 approach = input$gwr_bw_approach,
                 p = as.numeric(input$gwr_bw_distance),
                 kernel = input$gwr_bw_kernel,
                 longlat = FALSE)
    
    model <- gwr.basic(formula = formula,
                       data = data,
                       bw = bw,
                       kernel = input$gwr_bw_kernel,
                       adaptive=as.logical(input$gwr_bw),
                       longlat = FALSE)
    
    return(model)
  })
  
  gwr_result_plot_data <- eventReactive(input$gwr_results,{
    main_data <- st_transform(subdistrict, input$crs_3)
    model_output <- as.data.frame(gwr_model_output()$SDF)
    data <- cbind(main_data, as.matrix(model_output))
    return(data)
  })
  
  coeff_var <- eventReactive(input$gwr_results,{
    return(input$coeff_selected)
  })

  output$R2_plot <- renderTmap({
    tm_shape(gwr_result_plot_data()) +
      tm_fill(col = "Local_R2",
              alpha = 0.6,
              title = "R2") +
      tm_layout(title.position = "centertop") +
      tm_borders(alpha = 0.5) +
      tm_view(set.zoom.limits = c(10.5,14), view.legend.position = c("left", "bottom"))+
      tm_scale_bar() +
      tm_grid(alpha =0.2)
  })
  
  output$coeff_plot <- renderTmap({
    tm_shape(gwr_result_plot_data()) +
      tm_fill(col = str_glue("{coeff_var()}_TV"),
              alpha = 0.6,
              title = str_glue("{coeff_var()}")) +
      tm_layout(title.position = "centertop") +
      tm_borders(alpha = 0.5) +
      tm_view(set.zoom.limits = c(10.5,14), view.legend.position = c("left", "bottom"))+
      tm_scale_bar() +
      tm_grid(alpha =0.2)
  })
  
  info_message <- function(tab_name) {
    if (tab_name == "GWR Results") {
      
      message <- paste("These two diagrams illustrate the results of the GWR model calibrated based on the selections made in the side panel. 
                       The diagram to the left shows us the R2 values of the different sub-districts in DKI Jakarta.
                       It indicates how well the local regression model fits observed y-values.
                       A low R2 value tells us that the local model is performing poorly.
                       If this is the case, it suggests that the model might need to be improved by providing more important x variables.
                       The digram to the right shows us the Coefficient t-value of the selected x variable.
                       It indicates the statistical significance of the x variable. 
                       This is used to identify which are most influential in explainaing the variation in the y-values across DKI Jakarta.")
    } else if (tab_name == "Linearity & Normality Assumption") {
      
      message <- paste("When performing multiple linear regression, it is imperative that the linearity and normality assumptions are tested.  
                       The scatterplot above seeks to reveal whether there is a linear relationship between the x and y-values. 
                       If the data points are scattered around the 0 line, we can safely conclude that the relationship is linear. 
                       The histogram below the scatterplot seeks to reveal the distribution of the residual of the multiple linear regression model. 
                       The report on its right helps to statistically prove the observations seen from the histogram. 
                       If the p-value of the four tests are smaller than the alpha value of 0.05, we can infer, with statistical evidence, that the residuals are not normally distributed.")
    } else if (tab_name == "Spatial Autocorrelation") {
      
      message <- paste("This tab seeks to clarify the existence of Spatial Autocorrelation. 
                       In this case, it informs whether the y-variable (i.e., vaccination_rate) varies across space. 
                       The Residual Plot on the left shows whether the residuals of the linear regression model differ across the different sub-districts of DKI Jakarta. 
                       To statistically prove the observations made, the Global Moran's I test is reported to the right of the Residual Plot. 
                       If the p-value is less than the alpha value of 0.05, we can reject the claim that the residuals are randomly distributed. 
                       If there is Spatial Autocorrelation, then it makes sense for us to perform Geographically Weighted Regression as shown in the next tab.")
    } else if (tab_name == "Bivariate Analysis") {
      
      message <- paste("The Bivariate Analysis is used to show the relationship between the chosen x and y-variables. 
                       The scatterplot and best best fit line is used to observe patterns between two variables and their average trend. 
                       By examining the plot on the left, we can determine whether there is a relationship between the two variables as well as the strength and direction of that relationship. 
                       If the points on the scatterplot form a tight cluster around the best fit line, then there is a strong relationship between the two variables. 
                       Additionally, if you highlight the data points on the scatter plot, it will show you which sub-districts in DKI Jakarta they are from on the map to the left. 
                       To reset the view, please double-click on any whitespace within the scatterplot.")
      
    } else if (tab_name == "Correlation Analysis") {
      
      message <- paste("The Correlation Plot is a graphical representation of the pairwise correlations between different variables in a dataset. 
                       By examining the correlation plot, we can identify strong positive or negative correlations between variables. 
                       The darker the red and blue colours of the grid, the stronger their correlation. 
                       This tab is used to identify which variables are strongly correlated. 
                       To build an accurate model, one of the variables of the pairwise correlation should be removed. 
                       Please note that selected x variables will be automatically selected in the upcoming tabs.")
      
    } else {
      
      message <- paste("This page allows us to view the data that will be used for our analysis. 
                       For our use case, this is a table of the census data at the sub-district level of DKI Jakarta. 
                       It also includes information related to number of COVID-19 cases and vaccination rates so that we can perform Geographically Weighted Regression.")
      
    }
    return(message)
  }
  
  output$info_message_gwr <- renderUI({
    if (input$info_button_gwr %% 2 == 1) {
      message <- info_message("GWR Results")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })

  observeEvent(input$info_button_gwr, {
    if(input$info_button_gwr %% 2 == 1) {
      updateActionButton(session, "info_button_gwr", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_gwr", label="Show Info")
    }
  })
  
  output$info_message_spatial_autocorrelation <- renderUI({
    if (input$info_button_spatial_autocorrelation %% 2 == 1) {
      message <- info_message("Spatial Autocorrelation")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })
  
  observeEvent(input$info_button_spatial_autocorrelation, {
    if(input$info_button_spatial_autocorrelation %% 2 == 1) {
      updateActionButton(session, "info_button_spatial_autocorrelation", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_spatial_autocorrelation", label="Show Info")
    }
  })
  
  output$info_message_ln_assumption <- renderUI({
    if (input$info_button_ln_assumption %% 2 == 1) {
      message <- info_message("Linearity & Normality Assumption")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })
  
  observeEvent(input$info_button_ln_assumption, {
    if(input$info_button_ln_assumption %% 2 == 1) {
      updateActionButton(session, "info_button_ln_assumption", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_ln_assumption", label="Show Info")
    }
  })
  
  output$info_message_ba <- renderUI({
    if (input$info_button_ba %% 2 == 1) {
      message <- info_message("Bivariate Analysis")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })
  
  observeEvent(input$info_button_ba, {
    if(input$info_button_ba %% 2 == 1) {
      updateActionButton(session, "info_button_ba", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_ba", label="Show Info")
    }
  })
  
  output$info_message_cp <- renderUI({
    if (input$info_button_cp %% 2 == 1) {
      message <- info_message("Correlation Analysis")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })

  observeEvent(input$info_button_cp, {
    if(input$info_button_cp %% 2 == 1) {
      updateActionButton(session, "info_button_cp", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_cp", label="Show Info")
    }
  })
  
  output$info_message_d <- renderUI({
    if (input$info_button_d %% 2 == 1) {
      message <- info_message("Data")
      tagList(p(message, style="text-align: left;"))
    } else {
      NULL
    }
  })
  
  observeEvent(input$info_button_d, {
    if(input$info_button_d %% 2 == 1) {
      updateActionButton(session, "info_button_d", label="Hide Info")
    } else {
      updateActionButton(session, "info_button_d", label="Show Info")
    }
  })
  
  output$contact <- renderTable({
    data.frame(
      Name = c("S Guganesh", "Ho Yong Quan", "Peng You Yun"),
      Email = c("guganeshs.2020@smu.edu.sg", "yongquan.ho.2020@smu.edu.sg", "youyun.peng.2021@smu.edu.sg")
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
