library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggstatsplot)
library(plotly)
library(readr)
library(leaflet)
library(haven)
library(funModeling)
library(crosstalk)
library(data.table)
library(ggplot2)
library(tidytext)
library(ggfittext)
library(tidymodels)
library(ggcorrplot)
library(vip)
library(Boruta)
library(reshape2)
library(shinycssloaders)
library(ranger)
library(skimr)
library(shinythemes)
library(rgdal)
library(sf)
library(tmap)

#########
var_remove <- c("minimum_minimum_nights", "maximum_minimum_nights",
                "minimum_maximum_nights", "maximum_maximum_nights",
                "minimum_nights_avg_ntm", "maximum_nights_avg_ntm")

final_listings <- read_csv("./data/listing_processed.csv") %>%
    select(-all_of(var_remove)) %>%
    mutate(across(where(is.character), as.factor)) %>%
    mutate(across(where(is.logical), as.factor)) %>%
    mutate(price_per_pax = price/accommodates) %>%
    drop_na(host_is_superhost)

subzone <- readOGR(dsn = "data/spatial", layer="MP14_SUBZONE_WEB_PL")
airbnb <- select(final_listings, host_is_superhost, review_scores_rating, 
                 neighbourhood_cleansed, room_type, price_per_pax, latitude, longitude)
airbnb <- subset(airbnb, !is.na(host_is_superhost))
airbnb_sf <- st_as_sf(airbnb, coords = c("longitude", "latitude"), crs = 4326)

mpsz2 <- st_read(dsn = 'data/spatial',
                 layer = 'MP14_SUBZONE_WEB_PL', 
                 quiet = TRUE) %>%
    group_by(PLN_AREA_N) %>%
    summarise(geometry = sf::st_union(geometry))

airbnb_map <- right_join(mpsz2,listing_summary, c("PLN_AREA_N" = 'neighbourhood_cleansed'))

listing_summary <- final_listings %>% 
    group_by(neighbourhood_cleansed) %>%
    summarise(avg_price_pp = round(mean(price_per_pax),0),
              min_price_pp = round(min(price_per_pax),0),
              max_price_pp = round(max(price_per_pax),0)) %>%
    mutate_at(.vars = vars(neighbourhood_cleansed), .funs= funs(toupper))


cat_var <-final_listings %>% select(where(is.factor))
num_var <- final_listings %>% select(where(is.numeric))


facet_var <- final_listings %>% select(host_response_time, host_is_superhost, host_identity_verified, neighbourhood_group_cleansed,
                                       room_type,instant_bookable,bathroom_type)

themes <- list('Gray' = theme_gray(),
               'Black & white' = theme_bw(),
               'Linedraw' = theme_linedraw(),
               'Light' = theme_light(),
               'Dark' = theme_dark(),
               'Minimal' = theme_minimal(),
               'Classic' = theme_classic(),
               'Void' = theme_void(),
               'Test' = theme_test())

########


ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(title = "Airbnb ShinyPET"),         
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Intro", icon = icon('airbnb')),
            menuItem("EDA module", tabName = "EDA", icon = icon('chart-bar')),
            menuItem("Text module", tabName = "TA", icon = icon('text-height')),
            menuItem("Predictive module", tabName = "PA", icon = icon('chart-line'))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem("Intro",
                    fluidPage(theme = shinytheme('journal')) #https://rstudio.github.io/shinythemes/
                    #for intro page
            ),
            tabItem("EDA",
                    tabsetPanel(
                        tabPanel("Observe variables",
                                 observeUI('observe')
                        ),
                        tabPanel("Explore variables",
                                 fluidPage(
                                     titlePanel("Exploring Singapore Airbnbs"),
                                     sidebarLayout(
                                         sidebarPanel(
                                             selectInput(inputId = 'chart_type',
                                                         label = 'Select chart type',
                                                         choices = sort(c('Distribution',
                                                                          "Boxplot",
                                                                          "Mosaic",
                                                                          "Scatter")),
                                                         selected = "Distribution"),
                                             uiOutput('x_ui'),
                                             uiOutput('y_ui'),
                                             uiOutput('colour'),
                                             uiOutput('flip'),
                                             uiOutput('facet'),
                                             uiOutput('bins'),
                                             selectInput(inputId = 'theme',
                                                         label = 'Change theme',
                                                         choices = names(themes),
                                                         selected = 'Gray'),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Distribution'",
                                                 uiOutput('input_ttest')
                                             ),
                                             uiOutput('conf_lev'),
                                             width = 2
                                         ),
                                         mainPanel(
                                             
                                             h3("Statistical test result"),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Distribution'",
                                                 textOutput('test_method'),
                                                 h5("Hypothesis:"),
                                                 textOutput('null_hypo_ttest'),
                                                 textOutput('alt_hypo_ttest'),
                                                 h5('Confident interval:'),
                                                 textOutput('ci'),
                                                 h5('p-value:'),
                                                 textOutput('pvalue'),
                                                 p("If p-value is smaller than alpha (1 - confident interval), then there is enough statistical evidence to reject the null hypothesis.")
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Boxplot'",
                                                 uiOutput('test_method_2ttest'),
                                                 uiOutput('test_method_anova'),
                                                 h5("Hypothesis:"),
                                                 textOutput('null_hypo_2ttest'),
                                                 textOutput('alt_hypo_2ttest'),
                                                 h5('p-value:'),
                                                 uiOutput('pvalue_2ttest'),
                                                 uiOutput('pvalue_anova'),
                                                 p("If p-value <0.05, then there is enough statistical evidence to reject the null hypothesis.")
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Mosaic'",
                                                 textOutput('test_method_chisq'),
                                                 h5("Hypothesis:"),
                                                 textOutput('null_hypo_chisq'),
                                                 textOutput('alt_hypo_chisq'),
                                                 h5('p-value:'),
                                                 textOutput('pvalue_chisq'),
                                                 p("If p-value <0.05, then there is enough statistical evidence to reject the null hypothesis.")
                                                 
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Scatter'",
                                                 textOutput('test_method_cortest'),
                                                 h5("Hypothesis:"),
                                                 textOutput('null_hypo_cortest'),
                                                 textOutput('alt_hypo_cortest'),
                                                 h5('p-value:'),
                                                 textOutput('pvalue_cortest'),
                                                 p("If p-value is smaller than alpha (1 - confident interval), then there is enough statistical evidence to reject the null hypothesis.")
                                                 
                                                 
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Distribution'",
                                                 plotlyOutput('dist')
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Boxplot'",
                                                 plotlyOutput('bbox')
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Mosaic'",
                                                 plotlyOutput('mosaic')
                                             ),
                                             conditionalPanel(
                                                 condition = "input.chart_type == 'Scatter'",
                                                 plotlyOutput('scatter')
                                             ),
                                             
                                             
                                         )
                                     )
                                     
                                     
                                 )),
                        tabPanel("Map",
                                 fluidPage(
                                     titlePanel("Mapping Airbnb Singapore"),
                                     sidebarLayout(
                                         sidebarPanel(
                                             
                                             selectInput(inputId = 'map_type',
                                                         label = 'Select map type',
                                                         choices = c('Point Symbol map',
                                                                     'Choropleth map'),
                                                         selected = 'Point Symbol map'),
                                             uiOutput('point_view'),
                                             uiOutput('show_data'),
                                             width = 2
                                         ),
                                         mainPanel(
                                             leafletOutput('leaf_map', height = 800),
                                             conditionalPanel(
                                                 condition = "input.map_type == 'Choropleth map'",
                                                 DT::dataTableOutput('szTable')
                                             )
                                             
                                         )
                                     )
                                 )
                        )
                    ),
            ),
            tabItem("TA",
                    tabsetPanel(
                        tabPanel("Token frequency",
                                 #put ui here
                        ),
                        tabPanel("Sentiment analysis",
                                 #put ui here
                        ),
                        tabPanel("Topic modeling",
                                 #put ui here
                        ),
                        tabPanel("Network analysis",
                                 #put ui here
                        )
                    )
            ),
            tabItem("PA",
                    tabsetPanel(
                        tabPanel("Data splitting",
                                 data_splittingUI("ds")
                        ),
                        tabPanel("Feature selection",
                                 feat_selectUI("fs")
                        ),
                        tabPanel("Data transformation",
                                 data_transformUI("dtf")
                        ),
                        tabPanel("Model training",
                        ),
                        tabPanel("Model evaluation")
                    )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    ###############
    # observe tab
    ##############
    observeServer('observe', final_listings)
    
    ############
    # explore tab
    ###########
    
    plot_type <-reactive({
        if(input$chart_type == 'Distribution'){-1}
        else if(input$chart_type == 'Mosaic'){0}
        else if(input$chart_type == 'Boxplot'){1}
        else if(input$chart_type == 'Scatter'){2}
    })
    
    Rselected_varx <- reactiveVal(NULL)
    observeEvent(input$select_x,{
        Rselected_varx(input$select_x)
    })
    
    Rselected_vary0 <- reactiveVal(NULL)
    observeEvent(input$select_y0,{
        Rselected_vary0(input$select_y0)
    })
    
    Rselected_vary1 <- reactiveVal(NULL)
    observeEvent(input$select_y1,{
        Rselected_vary1(input$select_y1)
    })
    
    
    output$x_ui <- renderUI({
        
        if (plot_type() == -1){selectInput('select_x', 
                                           paste("Select x variable: numeric"),
                                           choices = sort(colnames(num_var)),
                                           selected = Rselected_varx())}
        else if(plot_type() == 0){selectInput('select_x', 
                                              paste("Select x variable: ", class(Rselected_varx())),
                                              choices = sort(colnames(cat_var)),
                                              selected = Rselected_varx())}
        else if(plot_type() == 1){selectInput('select_x', 
                                              paste("Select x variable:", class(Rselected_varx())),
                                              choices = sort(colnames(cat_var)),
                                              selected = Rselected_varx())}
        else if(plot_type() == 2){selectInput('select_x', 
                                              paste("Select x variable: numeric"),
                                              choices = sort(colnames(num_var)),
                                              selected = Rselected_varx())}
        
    })
    
    output$y_ui <- renderUI({
        
        if(plot_type() == 0){selectInput('select_y0', 
                                         paste("Select y variable: character"),
                                         choices = sort(colnames(cat_var)),
                                         selected = Rselected_vary0())}
        else if(plot_type() == 1){selectInput('select_y0', 
                                              paste("Select y variable: numeric"),
                                              choices = sort(colnames(num_var)),
                                              selected = Rselected_vary0())}
        else if(plot_type() == 2){selectInput('select_y0', 
                                              paste("Select y variable: numeric"),
                                              choices = sort(colnames(num_var)),
                                              selected = Rselected_vary0())}
        
    })
    
    
    output$colour <- renderUI({
        
        if(plot_type() %in% c(1,2)){
            selectInput(inputId = 'colour',
                        label = 'Colour by:',
                        choices = c('None',sort(colnames(cat_var))),
                        selected = 'None')}
    })  
    
    output$flip <- renderUI({
        
        if(plot_type() %in% c(1,2)){
            checkboxInput(inputId = 'flip',
                          label = paste('Flip axis'),
                          value = FALSE)}
    })  
    
    output$facet <- renderUI({
        
        if(plot_type() %in% c(0,1,2)){
            selectInput(inputId = 'facet',
                        label = 'Facet by:',
                        choices = c('None',sort(colnames(facet_var))),
                        selected = 'None')}
    })  
    
    
    output$bins <- renderUI({
        x <- unlist(final_listings[,input$select_x])
        if(plot_type() == -1 & class(x) == 'numeric'){
            sliderInput(inputId = "bin", 
                        label = "Number of bins", 
                        min = 5, 
                        max = 20, 
                        value = 10)
        }
        
    })
    
    plot_theme <- reactive({themes[[input$theme]]})
    
    ###distribution####
    
    output$dist <- renderPlotly({
        x <- unlist(final_listings[,input$select_x])
        
        ###histogram for numeric##
        if (class(x) == 'numeric'){
            hist <- ggplot(final_listings, aes(x = x)) + 
                geom_histogram(bins = input$bin,
                               color = '#767676',
                               fill = '#FF5A5F',
                               alpha=0.7) +
                geom_vline(aes(xintercept=mean(x,na.rm=T)),
                           color="#00A699", 
                           linetype="dashed", 
                           size=1)+ 
                geom_vline(aes(xintercept=median(x,na.rm=T)),
                           color="#484848", 
                           linetype="dashed", 
                           size=1) +
                plot_theme() +
                labs(title = paste("Distribution of",input$select_x),
                     x = paste(input$select_x),
                     y = "Count") + 
                theme(axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.text.x = element_text(angle = 45))
            
            ggplotly(hist, height = 600, width = 1500)
        }
        
        ###barplot for categorical##
        else {
            bar <- ggplot(final_listings, aes(x = x)) + 
                plot_theme() + 
                geom_histogram(stat = 'count',
                               color = '#767676',
                               fill = '#FF5A5F',
                               alpha = 0.7) +
                labs(title = paste("Distribution of",input$select_x),
                     x = paste(input$select_x)) +
                theme(axis.text.y=element_blank(),
                      axis.ticks.y=element_blank(),
                      axis.text.x = element_text(angle = 45))
            
            ggplotly(bar, height = 600, width = 1500)
            
        }
        
    })
    
    ####boxplot###
    
    output$bbox <- renderPlotly({
        x_var <- unlist(final_listings[,input$select_x])
        
        y_var <- unlist(final_listings[,input$select_y0])
        
        ccat <- if(input$colour == 'None'){'None'} 
        else {unlist(final_listings[,input$colour])}
        
        text_theme <- theme(axis.text.y=element_blank(),
                            axis.ticks.y=element_blank(),
                            axis.text.x = element_text(angle = 45))
        
        add_box <- ggplot(final_listings, aes(x = x_var, y = y_var)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0),
                x = paste(input$select_x),
                y = paste(input$select_y0),
                colour = paste(input$colour)) +
            plot_theme()+
            geom_boxplot(aes(fill = '#FF5A5F'),outlier.shape = NA) + 
            stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="blue") + 
            text_theme
        
        add_box_facet <- ggplot(final_listings, aes(x = x_var, y = y_var)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0, "facet wrap by", input$facet),
                x = paste(input$select_x),
                y = paste(input$select_y0),
                colour = paste(input$colour)) +
            plot_theme() +
            geom_boxplot(aes(fill = '#FF5A5F'),outlier.shape = NA) + 
            facet_wrap(~get(input$facet)) +
            text_theme
        
        add_c <- ggplot(final_listings, aes(x = x_var, y = y_var)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0),
                x = paste(input$select_x),
                y = paste(input$select_y0),
                colour = paste(input$colour)) +
            plot_theme() +
            geom_boxplot(aes(fill = ccat), outlier.shape = NA) +
            text_theme
        
        add_c_facet <- ggplot(final_listings, aes(x = x_var, y = y_var)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0, "facet wrap by", input$facet),
                x = paste(input$select_x),
                y = paste(input$select_y0),
                colour = paste(input$colour)) +
            plot_theme() + 
            geom_boxplot(aes(fill = ccat), outlier.shape = NA) + 
            facet_wrap(~get(input$facet)) + 
            text_theme
        
        
        bbox <- if (input$colour != 'None' & input$facet == "None"){add_c}
        else if(input$colour != 'None' & input$facet != "None"){add_c_facet}
        else if(input$colour == 'None' & input$facet == "None"){add_box}
        else if(input$colour == 'None' & input$facet != "None"){add_box_facet}
        
        
        flip_chart <- if(input$flip){
            bbox + coord_flip()
        } else {
            bbox
        }  
        
        boxly <- ggplotly(flip_chart, height = 600, width = 1500) %>% layout(boxmode = "group")
        
        boxly
    })
    
    ### mosaic ###
    
    output$mosaic <- renderPlotly({
        x_var <- unlist(final_listings[,input$select_x])
        
        y_var <- unlist(final_listings[,input$select_y0])
        
        m <- ggplot(final_listings, aes(x_var, fill = y_var)) +
            geom_bar(position = 'fill', width = 0.98) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0),
                x = paste(input$select_x),
                y = paste(input$select_y0)) +
            plot_theme() + 
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle = 45))
        
        m_facet <- ggplot(final_listings, aes(x_var, fill = y_var)) +
            geom_bar(position = 'fill', width = 0.98) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0, "facet wrap by", input$facet),
                x = paste(input$select_x),
                y = paste(input$select_y0)) +
            plot_theme() +
            facet_wrap(~get(input$facet)) +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle = 45))
        
        plt_m <- if(input$facet == "None"){m}
        else if (input$facet != "None"){m_facet}
        
        ggplotly(plt_m, height = 600, width = 1500)
        
    })
    
    
    ## scatter ##
    
    output$scatter <- renderPlotly({
        x_var <- unlist(final_listings[,input$select_x])
        
        y_var <- unlist(final_listings[,input$select_y0])
        
        ccat <- if(input$colour == 'None'){'None'} 
        else {unlist(final_listings[,input$colour])}
        
        scatter_colour <- ggplot(final_listings, aes(x = x_var, y= y_var)) +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle = 45)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0),
                x = paste(input$select_x),
                y = paste(input$select_y0)) +
            plot_theme() +
            geom_point(aes(colour = ccat)) + 
            geom_smooth(method = 'lm', se = FALSE) 
        
        scatter_no_colour <- ggplot(final_listings, aes(x = x_var, y= y_var)) +
            theme(axis.text.y=element_blank(),
                  axis.ticks.y=element_blank(),
                  axis.text.x = element_text(angle = 45)) + 
            labs(
                title = paste(input$chart_type, 'plot of ', input$select_x, ' and ', input$select_y0),
                x = paste(input$select_x),
                y = paste(input$select_y0)) +
            plot_theme() +
            geom_point() + 
            geom_smooth(method = 'lm', se = FALSE) 
        
        scatter_facet <- facet_wrap(~get(input$facet))
        
        
        plt_scatter <- if (input$colour == 'None' & input$facet == "None"){scatter_no_colour}
        else if(input$colour != 'None' & input$facet != "None"){scatter_colour + scatter_facet}
        else if(input$colour != 'None' & input$facet == "None"){scatter_colour}
        else if(input$colour == 'None' & input$facet != "None"){scatter_no_colour + scatter_facet}
        
        flip_chart <- if(input$flip){
            plt_scatter + coord_flip()
        } else {
            plt_scatter
        }  
        
        ggplotly(flip_chart, height = 600, width = 1500)
        
        
    })
    
    ## conf level
    
    output$conf_lev <- renderUI({
        if(plot_type() %in% c(-1,2)){
            sliderInput(inputId = 'conf',
                        label = 'Select confidence interval',
                        min = 0,
                        max = 1,
                        value = 0.95)}
    })
    
    ## t-test
    
    output$input_ttest <- renderUI({
        x <- unlist(final_listings[,input$select_x])
        if(plot_type() == -1 & class(x) == 'numeric'){
            numericInput(inputId = "test_mean", 
                         label = "Test mean", 
                         min = min(x), 
                         max = max(x), 
                         value = round(mean(x),2))
        }
    })
    
    ttestout <- reactive({
        x <- unlist(final_listings[,input$select_x])
        y <- unlist(final_listings[,input$select_y0])
        conf <- input$conf
        t1 <- t.test(x, mu = input$test_mean, conf.level = conf)
        
        if(plot_type() == -1 & class(x) == 'numeric'){t1}
        else if(plot_type() == -1 & class(x) != 'numeric'){NULL}
        
    })
    
    
    output$null_hypo_ttest <- renderText({
        
        vals <- ttestout()
        var_name <- vals$data.name
        
        paste("Null hypothesis: mean of", input$select_x, 'equals to', input$test_mean)
    })
    
    output$alt_hypo_ttest <- renderText({
        
        paste("Alternative hypothesis: mean of", input$select_x, 'not equals to', input$test_mean)
        
    })
    
    
    output$test_method <- renderText({
        vals <- ttestout()
        vals$method
    })
    
    output$pvalue <- renderText({
        vals <- ttestout()
        round(vals$p.value,5)
    })
    
    
    output$ci <- renderText({
        vals <- ttestout()
        lower <- vals$conf.int[1]
        upper <- vals$conf.int[2]
        paste('lower bound', round(lower,2),  'upper bound', round(upper,2))
    })
    
    ## mosaic test
    
    chisq_out <- reactive({
        x <- unlist(final_listings[,input$select_x])
        y <- unlist(final_listings[,input$select_y0])
        chisq_m <- chisq.test(x = x, y= y, correct = FALSE)
        chisq_m
    })
    
    
    output$null_hypo_chisq <- renderText({
        paste("Null hypothesis: There is no association between", input$select_x, 'and', input$select_y0)
    })
    
    output$alt_hypo_chisq <- renderText({
        paste("Alternative hypothesis: There is an association between", input$select_x, 'and', input$select_y0)
    })
    
    
    output$test_method_chisq <- renderText({
        vals <- chisq_out()
        vals$method
    })
    
    
    output$pvalue_chisq <- renderText({
        vals <- chisq_out()
        vals$p.value
    })
    
    ## t test - boxplot
    
    boxttest <- reactive({
        x <- unlist(final_listings[,input$select_x])
        y <- unlist(final_listings[,input$select_y0])
        if(Rselected_varx() %in% c('host_is_superhost','host_identity_verified','instant_bookable')){
            t.test(y ~ x, final_listings, var.equal = FALSE)
        }
        
    })
    
    anovatest <- reactive({
        x <- unlist(final_listings[,input$select_x])
        y <- unlist(final_listings[,input$select_y0])
        if(Rselected_varx() %in% c('host_response_time','neighbourhood_cleansed','neighbourhood_group_cleansed',
                                   'property_type','room_type','bathroom_type')){
            aov(y ~ x, data = final_listings)
        }
        
    })
    
    output$null_hypo_2ttest <- renderText({
        paste("Null hypothesis: There is no difference between the average", input$select_x, 'for the levels within', input$select_y0)
    })
    
    output$alt_hypo_2ttest <- renderText({
        paste("Alternative hypothesis: There is a difference between the average", input$select_x, 'for the levels within', input$select_y0)
    })
    
    output$test_method_2ttest <- renderUI({
        vals <- boxttest()
        twolevels <- paste(vals$method)
        
        if(Rselected_varx() == 'host_is_superhost'){twolevels}
        else if(Rselected_varx() == 'host_identity_verified'){twolevels}
        else if( Rselected_varx() == 'instant_bookable'){twolevels}
        
    })
    
    output$test_method_anova <- renderUI({
        more_two <- "Anova test"
        if(Rselected_varx() %in% c('host_response_time','neighbourhood_cleansed','neighbourhood_group_cleansed',
                                   'property_type','room_type','bathroom_type')){more_two}
    })
    
    output$pvalue_2ttest <- renderUI({
        x <- unlist(final_listings[,input$select_x])
        vals <- boxttest()
        twolevels <- paste(vals$p.value)
        if(Rselected_varx() %in% c('host_is_superhost','host_identity_verified','instant_bookable')){
            twolevels} 
    })      
    
    output$pvalue_anova <- renderUI({
        x <- unlist(final_listings[,input$select_x])
        aov <- anovatest()
        sum_aov <- unlist(summary(aov))
        if(Rselected_varx() %in% c('host_response_time','neighbourhood_cleansed','neighbourhood_group_cleansed',
                                   'property_type','room_type','bathroom_type')){paste(sum_aov[["Pr(>F)1"]])}
    })      
    ## correlation test - scatter
    
    cor_out <- reactive({
        x <- unlist(final_listings[,input$select_x])
        y <- unlist(final_listings[,input$select_y0])
        cor_test <- cor.test(x = x, y= y, conf.level = input$conf)
        cor_test
    })
    
    
    output$null_hypo_cortest <- renderText({
        paste("Null hypothesis: There is no correlation between", input$select_x, 'and', input$select_y0)
    })
    
    output$alt_hypo_cortest <- renderText({
        paste("Alternative hypothesis: There is correlation between", input$select_x, 'and', input$select_y0)
    })
    
    
    output$test_method_cortest <- renderText({
        vals <- cor_out()
        vals$method
    })
    
    
    output$pvalue_cortest <- renderText({
        vals <- cor_out()
        vals$p.value
    })
    
    ############
    # map tab
    ###########
    output$point_view <- renderUI({
        if (input$map_type == "Point Symbol map") {
            selectInput(inputId = 'filter_point',
                        label = 'View by',
                        choices = c('Superhost' = 'superhost',
                                    'Room type' = 'room_type'),
                        selected = 'Host status')} })
    
    output$show_data <- renderUI({
        if(input$map_type == "Choropleth map"){
            checkboxInput(inputId = 'show_data',
                          label = "Show data table",
                          value = FALSE)
        }
    })
    
    output$leaf_map <- renderLeaflet({
        
        superhost <- leaflet(airbnb_sf) %>%
            addTiles() %>%
            addProviderTiles('OneMapSG.Original', group = 'Original') %>%
            addProviderTiles('OneMapSG.Grey', group = 'Grey') %>%
            addProviderTiles('OneMapSG.Night', group = 'Night') %>%
            addCircleMarkers(
                data = airbnb[airbnb$host_is_superhost == 'TRUE',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#FF5A5F',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Superhost'
            ) %>%
            addCircleMarkers(
                data = airbnb[airbnb$host_is_superhost == 'FALSE',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#00A699',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Regular_host'
            ) %>%
            addLayersControl(baseGroups = c('Original', 'Grey', 'Night'),
                             overlayGroups = c('Superhost','Regular_host'),
                             options = layersControlOptions(collapsed = FALSE))
        
        rooms <- leaflet(airbnb_sf) %>%
            addTiles() %>%
            addProviderTiles('OneMapSG.Original', group = 'Original') %>%
            addProviderTiles('OneMapSG.Grey', group = 'Grey') %>%
            addProviderTiles('OneMapSG.Night', group = 'Night') %>%
            addCircleMarkers(
                data = airbnb[airbnb$room_type == 'Entire home/apt',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#FF5A5F',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Entire home'
            ) %>%
            addCircleMarkers(
                data = airbnb[airbnb$room_type == 'Hotel room',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#00A699',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Hotel room'
            ) %>%
            addCircleMarkers(
                data = airbnb[airbnb$room_type == 'Private room',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#3182bd',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Private room'
            ) %>%
            addCircleMarkers(
                data = airbnb[airbnb$room_type == 'Shared room',],
                lng = ~longitude,
                lat = ~latitude,
                color = '#756bb1',
                radius = 2,
                stroke = FALSE,
                fillOpacity = 0.5,
                group = 'Shared room'
            ) %>%  
            addLayersControl(baseGroups = c('Original', 'Grey', 'Night'),
                             overlayGroups = c('Entire home','Hotel room', 'Private room','Shared room'),
                             options = layersControlOptions(collapsed = FALSE))
        
        
        
        avg_price <- tm_shape(mpsz2)+
            tm_polygons() +
            tm_shape(airbnb_map) +
            tm_fill('avg_price_pp',
                    n = 6,
                    style = 'quantile',
                    palette = 'Blues')+
            tm_borders(lwd = 0.5, alpha = 1)
        
        
        cho_map <- tmap_leaflet(avg_price) %>%
            addTiles() %>%
            addProviderTiles('OneMapSG.Original', group = 'Original') %>%
            addProviderTiles('OneMapSG.Grey', group = 'Grey') %>%
            addProviderTiles('OneMapSG.Night', group = 'Night') %>%
            addLayersControl(baseGroups = c('Original', 'Grey', 'Night'),
                             options = layersControlOptions(collapsed = FALSE))
        
        if(input$map_type == 'Choropleth map'){cho_map}
        else if(input$filter_point == 'superhost'){ superhost}
        else if (input$filter_point == 'room_type') {rooms}
        
    })
    
    output$szTable <- DT::renderDataTable({
        if(input$show_data){
            DT::datatable(data = listing_summary)
        }
    })
    
    
    
    
    
    
    
    ########### predictive server file ###########
    return_val1 <- data_splittingServer("ds", final_listings)
    feat_selectServer("fs", final_listings, return_val1)
    data_transformServer("dtf", final_listings, return_val1)
    ############################################
    
}

# Run the application 
shinyApp(ui = ui, server = server)
