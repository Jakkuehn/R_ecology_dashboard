#Loading Libraries ----
#General functions such as reading tables and tidy data
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('openxlsx')) install.packages('openxlsx'); library('openxlsx')
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
#Shiny related packages
if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('shinythemes')) install.packages('shinythemes'); library('shinythemes')
if (!require('shinyWidgets')) install.packages('shinyWidgets'); library('shinyWidgets')
if (!require('shinydashboard')) install.packages('shinydashboard'); library('shinydashboard')
if (!require('colourpicker')) install.packages('colourpicker'); library('colourpicker')
#datatable related packages
if (!require('gt')) install.packages('gt'); library('gt')
if (!require('gtsummary')) install.packages('gtsummary'); library('gtsummary')
if (!require('DT')) install.packages('DT'); library('DT')
#machine learning functions
if (!require('caret')) install.packages('caret');library('caret')

#preloading 'iris' example dataset ----
data(iris)

# Define UI ----
ui <- fluidPage(
  
  theme = shinytheme("paper"),

  #AppTitle
  titlePanel("Fatty Acid Ecology Portfolio"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(position = "right",
                #Sidebar Panels
                sidebarPanel(
                  #Panel: Data Input and Overview----
                  conditionalPanel(
                    condition = "input.tabselected==1",
                    
                    #Input: Select a file
                    fileInput("data_file", 
                              h5("Choose a file of .csv, .xls or .xlsx type"),
                              multiple = T,
                              accept = c("text/csv",
                                         "text/comma-values,text/plain",
                                         ".csv",
                                         ".xls",
                                         ".xlsx")),
                    #Load 'iris' example data if no own data is present
                    h4("Have no data?"),
                    actionButton("iris_data","Load Example"),
                    
                    #Input: header checkbox
                    checkboxInput("header","Does datatable have header?", T),
                    checkboxInput("rownames","First column as row names?", F),
                    uiOutput("df_separator"), #Select Seperator (only in .csv)
                    uiOutput("df_quote"), #Select Quotation Type (only in .csv)
                    hr(),
                    # add Shiny and RStudio logos
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                           height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png",
                           height = "30px"),
                       ".")),
                  
                  #Panel: Statistical Analysis----
                  conditionalPanel(
                    condition = "input.tabselected==2",
                    
                    #Input: Choose Statistic 
                    selectInput("stattype", "Select a Statistics:",
                                choices = c("Descriptive" = "stats_desc",
                                            "ANOVA" = "stats_aov"#,
                                            #"MANOVA" = "stats_man", #currently WIP
                                            #"Correlation Analysis" = "stats_corr", #currently WIP
                                            #"Regression Analysis" = "stats_reg"), #currently WIP
                                ),
                                selected = "stats_desc"),
                    #downloadButton("stat_download", "Download Table"), #currently WIP
                    hr(),
                    # add Shiny and RStudio logos
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                           height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png",
                           height = "30px"),
                       ".")
                  ),
                  
                  #Panel: Plotting Data----
                  conditionalPanel(
                    condition = "input.tabselected==3",
                    
                    #Input: Choose Variables
                    selectInput("plottype", "Select a Plot Type:",
                                choices = c("Scatter Plot" = "plot_scatter",
                                            "Bar Plot" = "plot_bar",
                                            "Box Plot" = "plot_box",
                                            "Filled Bar Plot" = "plot_fillbar",
                                            "Histogramm" = "plot_hist",
                                            "non Metric Scaling Plot" = "plot_nmds",
                                            "Principal Component Analysis" = "plot_pca"
                                            ), 
                                selected = "plot_scatter"),
                    downloadButton("plot_download", "Download Plot"),
                    hr(),
                    # add Shiny and RStudio logos
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                           height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png",
                           height = "30px"),
                       ".")
                  ),
                  
                  #Panel: Modelling and Machine Learning----
                  conditionalPanel(
                    condition = "input.tabselected==4",
                    #Input: Choose Statistic 
                    selectInput("modeltype", "Select a Model:",
                                choices = c("Linear Regression" = "ml_linreg"#,
                                            #"Polynomial Regression" = "ml_polreg", #currently WIP
                                           #"Partial Least Squares Regression" = "ml_pls" #currently WIP
                                ),
                                selected = "ml_linreg"),
                    hr(),
                    # add Shiny and RStudio logos
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                           height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png",
                           height = "30px"),
                       ".")
                  ),
                  
                  #Panel: Utility Functions----
                  conditionalPanel(
                    condition = "input.tabselected==5",
                    
                    #Input: Choose Variables
                    h4("Data Table Transformations"),
                    selectInput("utility_funs","Select a Utility Function",
                                choices = c("Wide-to-Long Transformation" = "wide_to_long",
                                            "Response-to-Biomass Calculation" = "response_to_biomass")),
                    downloadButton("util_download", "Download Transformed Table"),
                    hr(), 
                    # add Shiny and RStudio logos
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                           height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png",
                           height = "30px"),
                       ".")
                  )
                ),

    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Multiple Tabs with Table, Summary, Plot and Utility Functions ----
      tabsetPanel( type = "tabs", id = "tabselected", selected = 1,
                   
                   #Tab: Data Overview----
                   tabPanel("Table", value = 1,
                            wellPanel(h4("This is your data", align = "center"),
                                      br(),
                                      dataTableOutput("data_table"))),
                   
                   #Tab: Statistical Analysis----
                   tabPanel("Statistics", value = 2,
                            
                            #Panel: Variable Selection, depending on statistic
                            wellPanel(h4("Data Curation", align = "center"),
                                      br(),
                                      uiOutput("choose_col_groupfactor"),
                                      uiOutput("choose_col_testvar"),
                                      uiOutput("choose_mult_testvars")),
                            
                            #Panel: Statistical Analysis as Table
                            wellPanel(h4("Data Exploration and Analysis", align = "center"),
                                      br(),
                                      tableOutput("data_summary"),
                                      tableOutput("data_aov"),
                                      tableOutput("data_man"))),

                   #Tab: Plot Output----
                   tabPanel("Plot", value = 3,
                            wellPanel(h4("Data Curation", align = "center"),
                                      hr(),
                                      uiOutput("choose_cols_x"),
                                      uiOutput("choose_cols_y"),
                                      uiOutput("choose_cols_g"),
                                      uiOutput("choose_cols_pick_x"),
                                      uiOutput("choose_cols_pick_y")),
                            wellPanel(" ",
                                      plotOutput(outputId = "main_plot", click = "plot_click")),
                            wellPanel(h4("Currently clicked data point:", align = "center"),
                                      tableOutput("click_data"))),
                   
                   #Tab: Modelling and Machine Learning----
                   tabPanel("Modelling", value = 4,
                            wellPanel(h4("Data Modelling", algin = "center"),
                                      br(),
                                      uiOutput("ML_test_train_split"),
                                      uiOutput("ML_choose_Y_data"),
                                      uiOutput("ML_choose_X_data"),
                                      hr()),
                            wellPanel(" ",
                                      plotOutput(outputId = "ML_plot"),
                                      verbatimTextOutput(outputId = "ML_model"))),
                   
                   
                   #Tab: Utility Functions----
                   tabPanel("Utility", value = 5,
                            conditionalPanel(condition = "input.utility_funs=='wide_to_long'",
                            wellPanel(h4("Data Transformation Tool"),
                                      uiOutput("tbl_to_long_transformer"),
                                      br(),
                                      dataTableOutput("longformat_tbl"))),
                            conditionalPanel(condition = "input.utility_funs=='response_to_biomass'",
                            wellPanel(h4("Response-to-Biomass Calculator"),
                                      uiOutput("rsps_upload"),
                                      uiOutput("hlpr_upload"),
                                      br(),
                                      uiOutput("rsps_to_biomass_button"))),
                            wellPanel(dataTableOutput("response_to_biomass"))),
                   
                   #Tab: Debugging----
                   # tabPanel("Debug", value = 6,
                   #          wellPanel(h4("Debugger"),
                   #                    tableOutput("debugger")))
                            
                  
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  #Table Tab ----
  #Input Render: Select separator for .csv files
  output$df_separator <- renderUI({
    req(tools::file_ext(input$data_file$name) == "csv")
    radioButtons("separat", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")
  })
  
  #Input Render: Select quote type for .csv files
  output$df_quote <- renderUI({
    req(tools::file_ext(input$data_file$name) == "csv")
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"')
  })
  
  #Load user input data file or load iris data if no user data is input
  df <- reactive({
    if(!is.null(input$data_file)){
      ext <- tools::file_ext(input$data_file$name)
      data <- switch(ext,
             csv = read.csv(input$data_file$datapath,
                            header = input$header,
                            sep = input$separat,
                            quote = input$quote),
             xls = read_excel(input$data_file$datapath,
                                     col_names = input$header),
             xlsx = read_excel(input$data_file$datapath,
                               col_names = input$header),
             validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
      )
    if(input$rownames == T){
      # data <- column_to_rownames(data, var = names(data[toString(input$df_rownames)][,1]))
      # rownames(data) <- data[,names(data[toString(input$df_rownames)][,1])] %>% suppressWarnings()
      # data <- data[,-c(names(data[toString(input$df_rownames)][,1]))]
      rownames(data) <- data[,1]
      data <- data[,-1]
    }
    else{
      return(data)
    }
    }
    else if(input$iris_data){
        iris
    }
  })
  
  #Render a Table of the uploaded Data or the iris data set
  output$data_table <- renderDataTable({
    df() %>% datatable(., options = list(pageLength = 10,
                                         scrollX = T,
                                         width = '256px'))
  })
  
  #Stats Tab ----
  
  #Stats Tab DropDown Menu: Choose grouping factor from data set
  output$choose_col_groupfactor <- renderUI({
    req(input$stattype == "stats_desc" |
          input$stattype == "stats_aov" |
          input$stattype == "stats_man" |
          input$stattype == "stats_reg")
    varSelectInput("statvar_factor", label = h6("Choose grouping factor"), df()%>% dplyr::select(where(negate(is.numeric))))
  })

  #Stats Tab DropDown Menu: Pick the "X" predictor variables for MANOVA
  output$choose_mult_testvars <- renderUI({
    req(input$stattype == "stats_aov" | 
        input$stattype == "stats_man")
    pickerInput("statpick_x", label = h6("Select Columns with test variables"), 
                choices = names(df() %>% dplyr::select(where(is.numeric))),
                options = list(
                  title = "Select a column...",
                  size = 5,
                  `selected-text-format` = "count > 1",
                  `actions-box` = TRUE), 
                multiple = TRUE
    )
    })

  
  #Generate Summary Data from uploaded data set
  tbl_sum <- reactive({
    df() %>% tbl_summary(
    by = !!input$statvar_factor,
    statistic = list(all_continuous() ~ "{mean} ± {sd}")) %>%
    add_stat_label(location = "column") %>% as_gt()
  })
  
  #Render the Table Output of summary data
  output$data_summary <- render_gt({
    req(input$stattype == "stats_desc")
    tbl_sum()
  })
  
  #ANOVA helper function to grab F and p values
  gt_aov <- function(data, variable, by, ...) {
    aov(data[[variable]] ~ data[[by]], data) %>%
      broom::tidy() %>%
      filter(term != "Residuals") %>% 
      select(statistic, p.value)
  }
  
  #Generate ANOVA data from uploaded data set
  tbl_aov <- reactive({
    df() %>% tbl_summary(by = !!input$statvar_factor, 
                       statistic = list(!!input$statpick_x ~ "{mean} ± {sd}"),
                       include = !!input$statpick_x) %>% 
      add_stat(fns = everything() ~ gt_aov) %>%
      modify_header(list(statistic ~ "**F statistic**",
                         p.value ~ "**p value**"))  %>%
      modify_fmt_fun(list(statistic ~ style_sigfig,
                          p.value ~ style_pvalue)) %>%
      modify_footnote(c(statistic, p.value) ~ "One Way ANOVA") %>%
    add_stat_label(location = "column") %>% as_gt()
  })

  #Render the Table Output of ANOVA data
  output$data_aov <- render_gt({
    req(input$stattype == "stats_aov")
    tbl_aov()
  })
  
  #Generate MANOVA data from uploaded data set !!!WIP!!!
  tbl_man <- reactive({
    df() %>% tbl_summary(by = !!input$statvar_factor, 
                         statistic = list(input$statpick_x ~ "{mean} ± {sd}"),
                         include = input$statpick_x) %>% add_p(test = input$statpick_x ~ "aov") %>%
      add_stat_label(location = "column") %>% as_gt()
  })
  
  #Render the Table Output of MANOVA data !!!WIP!!!
  output$data_man <- render_gt({
    req(input$stattype == "stats_man")
    tbl_aov()
  })
  
  #Download Statistic Table Functionality !!!WIP!!!
  output$plot_download <- downloadHandler(
    filename = function() { paste0(input$stattype, "png", sep=" ") },
    content = function(file) {
      gtsave(file, data = tbl_aov())
    }
  )

  #Plot Tab ----
  #Plot Tab Dropdown Menu, choosing x, y and group variables, depending on plot type
  
  #Column for X variable
  output$choose_cols_x <- renderUI({
    req(input$plottype == "plot_hist" | 
          input$plottype == "plot_scatter")
    varSelectInput("plotvariable_x", label = h6("Choose X axis variable"), df() %>% dplyr::select(where(is.numeric)))
  })
  
  #Column for Y variable
  output$choose_cols_y <- renderUI({
    req(input$plottype == "plot_scatter" |
        input$plottype == "plot_bar" |
        input$plottype == "plot_box")
    varSelectInput("plotvariable_y", label = h6("Choose Y axis variable"), df() %>% dplyr::select(where(is.numeric)))
  })
  
  #Column for group factor  
  output$choose_cols_g <- renderUI({
    req(input$plottype == "plot_scatter" |
        input$plottype == "plot_bar" |
        input$plottype == "plot_box" |
        input$plottype == "plot_fillbar" |
        input$plottype == "plot_nmds" |
        input$plottype == "plot_pca")
    varSelectInput("plotvariable_g", label = h6("Choose group variable"), df() %>% dplyr::select(where(negate(is.numeric))))
  })
  
  #Pick the "X" predictor variables for PCA et al.
  output$choose_cols_pick_x <- renderUI({
    req(input$plottype == "plot_nmds" |
        input$plottype == "plot_pca")
    pickerInput("plotpick_x", label = h6("Select Columns with response variables"), 
      choices = names(df()%>% dplyr::select(where(is.numeric))),
      options = list(
        title = "Select a column...",
        size = 5,
        `selected-text-format` = "count > 1",
        `actions-box` = TRUE), 
      multiple = TRUE
    )
    
    
  })
  
  #Pick the "Y" environmental variables for PCA et al.
  output$choose_cols_pick_y <- renderUI({
    req(input$plottype == "plot_pca")
    pickerInput("plotpick_y", label = h6("Select Columns with environmental variables"), 
                choices = names(df()%>% dplyr::select(where(is.numeric))),
                options = list(
                  title = "Select a column...",
                  size = 5,
                  `selected-text-format` = "count > 1",
                  `actions-box` = TRUE), 
                multiple = TRUE
    )
    
  })
  #Colourblind Palette
  cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  #Generating the plot, utilising the chosen variables
    shinyplot <- reactive({
      #Initializing the ggplot device and adding general theme
      p <- ggplot(df()) +
        scale_colour_manual(values = cb_palette) +
        scale_fill_manual(values = cb_palette) +
        theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
              axis.title.x = element_text(face= "bold", size = 15),
              axis.title.y = element_text(face = "bold", size = 15),
              axis.text = element_text(face = "bold", size = 12),
              legend.title = element_text(face = "bold", size = 15),
              legend.text = element_text(size = 12),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white",colour = "black"))
      
      #Scatterplot, Barplot and Box plot utilise a simple switch form
      if(input$plottype == "plot_hist" | 
         input$plottype == "plot_scatter" | 
         input$plottype == "plot_bar" | 
         input$plottype == "plot_box"){
        
        switch(input$plottype,
               plot_hist = p + 
                 geom_histogram(aes(x = !!input$plotvariable_x)),
               plot_scatter = p +
                 geom_point(aes(x = !!input$plotvariable_x,
                                y = !!input$plotvariable_y,
                                fill = !!input$plotvariable_g,
                                color = !!input$plotvariable_g), 
                            size = 2.5),
               plot_bar = p +
                 geom_bar(aes(x = !!input$plotvariable_g,
                              y = !!input$plotvariable_y,
                              group = !!input$plotvariable_g,
                              fill = !!input$plotvariable_g,
                              color = !!input$plotvariable_g), 
                          stat = "identity",
                          size = 2.5),
               plot_box = p + 
                 geom_boxplot(aes(x = !!input$plotvariable_g,
                                  y = !!input$plotvariable_y,
                                  group = !!input$plotvariable_g,
                                  fill = !!input$plotvariable_g)))
      }
      
      #Filled Barplot; requires summing row data to 1 and turning data to long format
      else if(input$plottype == "plot_fillbar"){
        
        #Calculating the normalised data, then turning it to long format
        df_bind <- bind_cols(df()[toString(input$plotvariable_g)], (df()[complete.cases(df()),] %>% dplyr::select(where(is.numeric)) * 0.01) / rowSums(df()[complete.cases(df()),] %>% dplyr::select(where(is.numeric)) * 0.01)) #Bindinf factors with normalized data
        df_fill <- df_bind %>% mutate_if(is.character, as.factor) # Turning character columns into factors
        df_cast <- df_fill %>% pivot_longer(.,cols = c(names(select_if(df_fill, is.numeric))),names_to = "Variable", values_to = "Proportion")
        
        #Plotting the filled bar plot
        ggplot(data = df_cast, aes( x = !!input$plotvariable_g, y = Proportion, fill = Variable)) +
          geom_bar(position = "fill", stat = "identity") +
          coord_flip() +
          scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Set3"))(length(unique(df_cast$Variable)))) +
          labs(title = "Fatty Acid Profile",
               x = "Collembola",
               y = "Proportion",
               fill = "Fatty Acid") +
          theme(plot.title = element_text(face ="bold", size = 15, hjust = 0.5),
                axis.title.x = element_text(face="bold", size = 15),
                axis.title.y = element_text(face ="bold", size = 15),
                axis.text=element_text(face = "bold", size = 12),
                legend.title=element_text(face ="bold", size = 15),
                legend.text = element_text(size = 12),
                legend.position="bottom",
                panel.grid = element_blank(),
                panel.background = element_rect(fill = "white", colour = "white"))
      }
      
      #NMDS plot, requires prior calculation of NMDS data      
      else if(input$plottype == "plot_nmds"){

        if(!require(vegan)){
          install.packages("vegan")
          library(vegan)
        }
        
        #Calculating NMDS data, as well as mean of group (for label)
        df_NMDS <- df()[,input$plotpick_x] %>% metaMDS
        nmds_data <- data.frame(groupvar = df()[toString(input$plotvariable_g)][,1], 
                                NMDS1 = df_NMDS$points[, 1], 
                                NMDS2 = df_NMDS$points[, 2])
        
        nmds_mean = aggregate(nmds_data[, c("NMDS1", "NMDS2")], list(group = nmds_data$groupvar), mean)
        
        #Plotting the NMDS
        ggplot(data = nmds_data) +
          geom_point(aes(x = NMDS1,
                         y = NMDS2,
                         colour = groupvar),
                     size = 4.5) +
          annotate("text",
                   x = nmds_mean$NMDS1,
                   y = nmds_mean$NMDS2,
                   label = nmds_mean$group,
                   fontface = "bold" , size = 5) +
          labs(x = paste("NMDS 1"),
               y = paste("NMDS 2")) +
          scale_fill_manual(values = cb_palette) + 
          scale_colour_manual(values = cb_palette) +
          theme(plot.title = element_text(face ="bold", size = 15, hjust = 0.5),
                axis.title.x = element_text(face="bold", size = 15),
                axis.title.y = element_text(face ="bold", size = 15),
                axis.text=element_text(face = "bold", size = 12),
                legend.title=element_text(face ="bold", size = 15),
                legend.text = element_text(size = 12), panel.grid = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black"))
      }
      
      #PCA Plot, requires prior calculation of principal components
      else if(input$plottype == "plot_pca"){
        if(!require(vegan)){
          install.packages("vegan")
          library(vegan)
        }
        
        #Calculating PCA data, as well as mean of group (for label) and eigenvectors (for explained variance)
        df_pca <- df()[, input$plotpick_x] %>% prcomp
        pca_data <- data.frame(groupvar = df()[toString(input$plotvariable_g)][,1], 
                                PC1 = df_pca$x[, 1], 
                                PC2 = df_pca$x[, 2])
        
        
        pca_mean = aggregate(pca_data[, c("PC1", "PC2")], list(group = pca_data$groupvar), mean)
        eigs <- df_pca$sdev^2
        eig_var <- (eigs / sum(eigs)) * 100
        
        #Adding Arrows using the environmental data
        vec_sp <- envfit(df_pca$x, df()[, input$plotpick_y], perm = 1000, na.rm = F)
        vec_sp_df <- as.data.frame(vec_sp$vectors$arrows * sqrt(vec_sp$vectors$r))
        vec_sp_df$group <- rownames(vec_sp_df)
        
        
        #Plotting the PCA (no biplot)
        ggplot(data = pca_data) +
          geom_point(aes(x = PC1,
                         y = PC2,
                         colour = groupvar),
                     size = 4.5) +
          # annotate("text",
          #          x = pca_mean$PC1,
          #          y = pca_mean$PC2,
          #          label = pca_mean$group,
          #          fontface = "bold" , size = 5) +
          labs(x = paste("Root 1 (", round(eig_var[1], digits = 2), "% explained variance)", sep = ""), 
               y = paste("Root 2 (", round(eig_var[2], digits = 2), "% explained variance)", sep = "")) +
          theme(plot.title = element_text(face ="bold", size = 15, hjust = 0.5),
                axis.title.x = element_text(face="bold", size = 15),
                axis.title.y = element_text(face ="bold", size = 15),
                axis.text=element_text(face = "bold", size = 12),
                legend.title=element_text(face ="bold", size = 15),
                legend.text = element_text(size = 12), panel.grid = element_blank(),
                panel.background = element_rect(fill = "white", colour = "black")) +
          geom_segment(data = vec_sp_df, 
                       aes(x = 0, xend = PC1, 
                           y = 0, yend = PC2), 
                       arrow = arrow(length = unit(0.5,  "cm")),
                       size = 1.0,
                       colour = "black") + 
          geom_text(data = vec_sp_df, 
                    aes(x = PC1, 
                        y = PC2, 
                        label = group), 
                    size = 5)
          
      }
      
      
    })
  
  #Render the Plot
  output$main_plot <- renderPlot({
   shinyplot()
  })
    
  #Functionality to click the plot to view neatest data point (only for scatter plot)
  output$click_data <- renderTable({
    req(input$plottype == "plot_scatter")
    nearPoints(df(), input$plot_click)
  })
    
  #Download the Plot as .png Functionality 
  output$plot_download <- downloadHandler(
    filename = function() { paste0(input$plottype, ".png", sep=" ") },
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = shinyplot(), device = device)
    }
  )
  #Modelling Tab ----
  
  #Modelling Tab UI: Slider that determines train/test split
  output$ML_test_train_split <- renderUI({
    req(input$modeltype == "ml_linreg" | 
          input$modeltype == "ml_polreg")
    sliderInput("test_train_split_slider", label = h6("What percentage to use as test data?"), min = 0, 
                max = 100, value = 25)
  })
  
  #Modelling Tab DropDown Menu: Choose Y data from data set
  output$ML_choose_Y_data <- renderUI({
    req(input$modeltype == "ml_linreg" | 
          input$modeltype == "ml_polreg")
    varSelectInput("mlvar_y", label = h6("Choose output variable to predict ('y' data)"), df())
  })
  
  #Modelling Tab DropDown Menu: Pick the "X" predictor variables from data set
  output$ML_choose_X_data <- renderUI({
    req(input$modeltype == "ml_linreg" | 
          input$modeltype == "ml_polreg")
    varSelectInput("mlvar_X", label = h6("Select Column with predictor variable ('X' data)"), df())
  }) 
  
  # output$ML_choose_X_data <- renderUI({
  #   req(input$modeltype == "ml_linreg" | 
  #         input$modeltype == "ml_polreg")
  #   pickerInput("mlvar_X", label = "Select Columns with predictor variables ('X' data)", 
  #               choices = names(df()),
  #               options = list(
  #                 title = "Select a column...",
  #                 size = 5,
  #                 `selected-text-format` = "count > 1",
  #                 `actions-box` = TRUE), 
  #               multiple = TRUE
  #   )
  # })
  
  #Create and Output a simple linear regression model
  shiny_model <- reactive({
    data <- df()
    prop <- input$test_train_split_slider / 100
    dep_var <-  df() %>% pull(input$mlvar_y)
    pred_var <- df() %>% pull(input$mlvar_X)
    train.index <- createDataPartition(df() %>% pull(input$mlvar_y), list = FALSE, p = 1 - prop)
    train <- data[train.index, ]
    test <- data[-train.index, ]
    model <- lm(dep_var ~ pred_var, data = train)
    names(model$coefficients) <- c("Intercept",input$mlvar_X)
    list <- list("Train_Data" = train, "Test_Data" = test, "Model" = model)
    train
  })

  #Generating the model plot, utilising the chosen variables
  shiny_model_plot <- reactive({
    
    #Scatterplot with added linear regression line
    ggplot(shiny_model()) +
      scale_colour_manual(values = cb_palette) +
      scale_fill_manual(values = cb_palette) +
      theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
            axis.title.x = element_text(face= "bold", size = 15),
            axis.title.y = element_text(face = "bold", size = 15),
            axis.text = element_text(face = "bold", size = 12),
            legend.title = element_text(face = "bold", size = 15),
            legend.text = element_text(size = 12),
            panel.grid = element_blank(),
            panel.background = element_rect(fill = "white",colour = "black")) +
      geom_point(aes(x = !!input$mlvar_X,
                              y = !!input$mlvar_y), 
                          size = 2.5,
                 color = "#1F77B4")  +
      geom_smooth(aes(x = !!input$mlvar_X,
                      y = !!input$mlvar_y), method = "lm", 
                  se = FALSE, 
                  color = "#D62728", 
                  formula = y ~ x)
  })
  
  #Render the Model Plot
  output$ML_plot <- renderPlot({
    shiny_model_plot()
  })
  
  output$ML_model <- renderPrint({
    summary(shiny_model()[3])
  })
  #

  #Utilities Tab----

  #Transform a wide format data table to long format:
  
  #Action Button for wide-to-long-transformation
  output$tbl_to_long_transformer <- renderUI({
    req(input$utility_funs == "wide_to_long")
    actionButton("tbl_to_long","Transform to Long Format")
  })

  #Reactive Function: Transform wide data table to long format
  df_tfd_wl <- reactive({
    if(input$tbl_to_long){
      df() %>% 
        pivot_longer(.,
                     cols = c(names(df() %>% select(where(is.numeric)))),
                     names_to = "Variable", 
                     values_to = "Proportion")
    }
  })
  
  #Render the Long Data Table
  output$longformat_tbl <- renderDataTable({
    req(input$utility_funs == "wide_to_long")
    df_tfd_wl() %>% datatable(., options = list(pageLength = 10,
                                                scrollX = T,
                                                width = '256px'))
  })

  #Transform Response Excel Sheets to Biomass using internal formula and 19:0 standard:
  
  #Action Button for response-to-biomass calculator
  output$rsps_to_biomass_button <- renderUI({
    req(input$utility_funs == "response_to_biomass")
    actionButton("tbl_to_biomass","Transform Response Master Table to Biomass")
  })
  
  #File Input Buttons for Response Table and Helper Table
  
  #Response Table Button
  output$rsps_upload <- renderUI({
    req(input$utility_funs == "response_to_biomass")
    fileInput("rsps_file", 
              h5("Choose a master table of .xls or .xlsx type"),
              multiple = T,
              accept = c("text/csv",
                         "text/comma-values,text/plain",
                         ".csv",
                         ".xls",
                         ".xlsx"))
  })
  
  #Helper Table Button
  output$hlpr_upload <- renderUI({
    req(input$utility_funs == "response_to_biomass")
    fileInput("hlpr_file", 
              h5("Choose a helper table of .xls or .xlsx type"),
              multiple = T,
              accept = c("text/csv",
                         "text/comma-values,text/plain",
                         ".csv",
                         ".xls",
                         ".xlsx"))
  })

  #Load the response value table ("Mastertabelle")
  df_rsps <- reactive({
    if(!is.null(input$rsps_file)){
      ext <- tools::file_ext(input$rsps_file$name)
      data <- switch(ext,
                     xls = read_excel(input$rsps_file$datapath,
                                      col_names = T),
                     xlsx = read_excel(input$rsps_file$datapath,
                                       col_names = T),
                     validate("Invalid file; Please upload a .xls or .xlsx file")
      )
      }
      else{
        return(data)
      }
  })
  #Load the helper table (should contain a "Weight", "Set" and "DW" column 
  #with values for each sample. "Weight" denotes the sample weight, 
  #"Set" denotes which extraction set the samples belong to, and "DW" denotes dry weight factor)
  df_hlpr <- reactive({
    if(!is.null(input$hlpr_file)){
      ext <- tools::file_ext(input$hlpr_file$name)
      data <- switch(ext,
                     xls = read_excel(input$hlpr_file$datapath,
                                      col_names = T),
                     xlsx = read_excel(input$hlpr_file$datapath,
                                       col_names = T),
                     validate("Invalid file; Please upload a .xls or .xlsx file")
      )
    }
    else{
      return(data)
    }
  })
  
  #Prelimninary for the Response to Biomass calculation
  mIS <- 6.924 #mass of internal standard (equivalent to the 30 µl added during MIDI)
  IS <- c("19:0") #Fatty Acid that is used as internal standard
  mw_ref <- read.csv("data/MolRef.csv", sep = ";", header = T) #loading a reference file of molecular weight
  
  #Function that calculates averages for every group of n samples 
  #Defaults to 2, as 2 blind values are commonly chosen
  Splitmeans<-function(df, n = 2){
    n_grp <- ncol(df) / n
    idx_grp <- split(seq(df), rep(seq(n_grp), each = n))
    res <- lapply(idx_grp, function(i) {
      tmp <- df[i]
      rowMeans(tmp, na.rm = TRUE)
    })
    R <- as.data.frame(res)
    names_frst <- names(df)[sapply(idx_grp, "[", 1)]
    names(R) <- names_frst                  
    return(R)
  }
  
  #Transforming the Response Table to a Biomass Table
  df_tfd_rb <- reactive({
    if(input$tbl_to_biomass){
      
      #Prepare Tables
      tbl_rsps <- df_rsps()
      tbl_hlpr <- df_hlpr()
      tbl_rsps[is.na(tbl_rsps)] <- 0
      
      #Separating Table into blind values and sample values
      tbl_blind <- cbind(tbl_rsps[,1], tbl_rsps %>% select(starts_with("BW")))
      tbl_sample <- tbl_rsps %>% select(!starts_with("BW"))
      
      #Conforming the first column to the "FA" column in the helper table
      names(tbl_blind)[1] <- "FA"
      names(tbl_sample)[1] <- "FA"
      
      #subsetting the molecular weight reference set to the data table,
      #and then the data table to the reference set. This excludes both
      #Fatty Acids not encountered in the samples as well as non-fatty acids
      #that are picked up by the FID
      mw_set <- plyr::match_df(mw_ref, tbl_blind, on = "FA")
      mw_set <- mw_set[order(mw_set$FA),]
      tbl_blind_set <- plyr::match_df(tbl_blind, mw_set, on = "FA")
      tbl_sample_set <- plyr::match_df(tbl_sample, mw_set, on = "FA")
      
      #Calculate Blind Values: BW = (ResponseFS*mIS*1000)/(ResponseIS*mwFA)
      blind_calc_top <- tbl_blind_set %>% select(where(is.numeric)) *mIS * 1000 #(ResponseFS*mIS*1000)
      blind_calc_bot <- tbl_blind_set[tbl_blind_set$FA %in% IS,-1] #(ResponseIS)
      
      blind_rslt <- mapply('/', blind_calc_top, blind_calc_bot) %>% as.data.frame()
      blind_conc <- blind_rslt  / cbind(blind_rslt, mw_set[,2])[,length(cbind(blind_rslt, mw_set[,2]))]
      
      blind_conc_sets <- Splitmeans(blind_conc)
      names(blind_conc_sets) <- paste0("Set", c(1:length(blind_conc_sets))) #Rename to 'Set1' etc.
      
      #Calculate Sample Values: SW = ((ResponseFS*mIS*1000)/(ResponseIS*mwFA)-cBW)*(1/dry weight * sample weight)
      sample_calc_top <- tbl_sample_set %>% select(where(is.numeric)) *mIS * 1000 #(ResponseFS*mIS*1000)
      sample_calc_bot <- tbl_sample_set[tbl_sample_set$FA %in% IS,-1] #(ResponseIS)
      
      sample_rslt <- mapply('/', sample_calc_top, sample_calc_bot) %>% as.data.frame()
      sample_conc <- sample_rslt  / cbind(sample_rslt, mw_set[,2])[,length(cbind(sample_rslt, mw_set[,2]))]
      
      sets <- unique(tbl_hlpr["Set"])[,1] %>% t %>% sort #Indexes number of sets
      
      #Substracts blind values from sample values, removing contaminations
      sample_part <- vector("list",length(sets))
      for (i in seq_along(sets)){
        sample_part[[i]] <- sample_conc %>% select_if(tbl_hlpr["Set"] == sets[[i]]) - blind_conc_sets[[i]]
      }
      sample_cleaned <- do.call(cbind, sample_part)
      
      DW_x_mass <- tbl_hlpr["DW"] * tbl_hlpr["Weight"]
      
      #(1/dry weight * sample weight)
      sample_biomass <- mapply('/',sample_cleaned, t(DW_x_mass)) %>% as.data.frame()
      
      #Rebinding the data set with the fatty acid names
      tbl_biomass <- cbind(tbl_sample_set["FA"], sample_biomass)
      
      #Removing internal standard 19:0
      tbl_biomass %>% filter(tbl_biomass["FA"] != "19:0")
    }
  })
  
  #Output Biomass Table
  output$response_to_biomass <- renderDataTable({
    req(input$utility_funs == "response_to_biomass")
          df_tfd_rb() %>% datatable(., options = list(pageLength = 10,
                                                      scrollX = T,
                                                      width = '256px'))
  })

  #Download Transformed Data preparation: chooses correct data table
  df_tf_dl <- reactive({
    if(input$utility_funs == "response_to_biomass"){
      df_tfd_rb()
    }
    else if(input$utility_funs == "wide_to_long"){
      df_tfd_wl()
    }
  })
  
  #Download Transformed Data Functionality
  output$util_download <- downloadHandler(
    filename = function() { paste0(input$utility_funs, ".xlsx", sep = " ")},
    content = function(file) {write.xlsx(df_tf_dl(),file)})
  
  #Debug Tab----
  #Debugging Function
  # output$debugger <- renderTable({
  #   shiny_model()[1] %>% class
  # })

}

#Call the App----
shinyApp(ui = ui, server = server)
