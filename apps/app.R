library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(readr)
library(leaflet)
library(haven)
library(funModeling)
library(crosstalk)
library(data.table)
library(ggplot2)
library(ggfittext)
library(tidymodels)
library(glmnet)
library(ranger)
library(xgboost)
library(rpart)
library(visNetwork)
library(sparkline)
library(ggcorrplot)
library(vip)
library(Boruta)
library(reshape2)
library(shinycssloaders)
library(shinyalert)
library(ranger)
library(skimr)
library(shinythemes)
library(rgdal)
library(sf)
library(tmap)
library(wordcloud2)
library(NLP)
library(tm) # text mining
library(stringr)
library(SnowballC) # text stemming
library(RColorBrewer) # Color Palettes
library(topicmodels)
library(tidytext)
library(slam)
library(tidyr)
library(igraph)
library(ggraph)
library(highcharter)
library(wordcloud)
library(widyr)

#########
var_remove <- c("minimum_minimum_nights", "maximum_minimum_nights",
                "minimum_maximum_nights", "maximum_maximum_nights",
                "minimum_nights_avg_ntm", "maximum_nights_avg_ntm")

final_listings <- read_csv("data/listing_processed.csv") %>%
  select(-all_of(var_remove)) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.logical), as.factor)) %>%
  mutate(price_per_pax = price/accommodates) %>%
  drop_na(host_is_superhost)

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


reviews <- read_csv("data/reviews.csv")%>% 
  dplyr::select(listing_id,comments)
listings <- read_csv("data/listings.csv")  %>% 
  rename(listing_id=id) %>% 
  dplyr::select(-c(listing_url, scrape_id, last_scraped, name, picture_url,host_url, host_about,host_thumbnail_url, host_picture_url, host_listings_count, host_verifications,calendar_updated,first_review,last_review,license,neighborhood_overview,description,host_total_listings_count,host_has_profile_pic,availability_30,availability_60,availability_90,availability_365,calculated_host_listings_count,calculated_host_listings_count_entire_homes,calculated_host_listings_count_private_rooms,calculated_host_listings_count_shared_rooms,reviews_per_month,minimum_nights,maximum_nights,minimum_minimum_nights,maximum_minimum_nights,minimum_maximum_nights,maximum_maximum_nights,number_of_reviews_ltm,number_of_reviews_l30d,minimum_nights_avg_ntm,maximum_nights_avg_ntm,calendar_last_scraped,has_availability,instant_bookable))
data <- right_join(reviews,listings,by="listing_id")
data$comments <- gsub("^[alpha:][:space:]'\"]", " ",data$comments) %>% 
  tolower()
data$comments <- gsub("[^a-zA-Z]", " ",data$comments)
data$comments <- iconv(data$comments,to="UTF-8")
data$price <- str_remove(string=data$price,pattern='\\$') %>% 
  as.numeric()
data$host_response_rate <- gsub("%","",data$host_response_rate) 
data$amenities <- gsub('\"', "", data$amenities, fixed = TRUE)
data <- subset(data,host_location=="Singapore" | host_location=="Singapore, Singapore" | host_location=="SG")
new_stopwords <- c("NA","michelle's","elizabeth","yuan","felix","anita","susan","eddie","eddie's","edwin","belinda","besan","nargis","antonio","sharm","tim","kathleen","stteven","jerome","freddy","eunice","eunice's","vivian","jerome's","mi's","freddy's","joey","tay","michelle","noor","anthony","tay's","carrie","jauhara","susan","karen","jenny","lena","leonard","kingsley","freda","jialin","matthew","fran","na","joey")
all_stopwords <- c(new_stopwords,stop_words)
data_comments <- data %>% 
  dplyr::select(listing_id,comments,review_scores_rating,neighbourhood_cleansed,neighbourhood_group_cleansed)%>%
  unnest_tokens(word,comments) %>% 
  group_by(listing_id) %>% 
  ungroup() %>% 
  anti_join(stop_words)
data_count <- data_comments %>% 
  group_by(word) %>% 
  summarise(frequency=n()) %>% 
  arrange(desc(frequency))
bigram_data_count <- data %>% 
  dplyr::select(listing_id,comments,review_scores_rating,neighbourhood_cleansed,neighbourhood_group_cleansed)%>%
  unnest_tokens(word,comments,token="ngrams",n=2) %>% 
  separate(word,c("word1","word2"),sep=" ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  #ungroup() %>% 
  count(word,sort=TRUE) %>% 
  dplyr::slice(-c(1))


afinn <- get_sentiments("afinn") 
afinn_sentiments <- data_comments %>% 
  inner_join(afinn) %>% 
  count(word,value,sort=TRUE) %>% 
  acast(word~value,value.var="n",fill=0) 
#%>% 
#comparison.cloud()

afinn_count <- data_comments %>% 
  #group_by(listing_id) %>% 
  inner_join(afinn) %>% 
  count(word,value) %>% 
  filter(n>500) %>% 
  #mutate(n=ifelse(sentiment=="negative",-n,n)) %>% 
  mutate(word=reorder(word,n)) %>% 
  arrange(desc(n))
#%>% 
#ggplot(aes(word,(n)))+
#geom_col()+
#coord_flip()

bing <- get_sentiments("bing")
bing_sentiments <- data_comments %>% 
  inner_join(bing) %>% 
  count(word,sentiment,sort=TRUE) %>% 
  acast(word~sentiment,value.var="n",fill=0) 
#%>% 
#comparison.cloud(colors = c("#FF5A5F", "#00A699"))

bing_count <- data_comments %>% 
  inner_join(bing) %>% 
  count(word,sentiment) %>% 
  filter(n>500) %>% 
  mutate(n=ifelse(sentiment=="negative",-n,n)) %>% 
  mutate(word=reorder(word,n))%>% 
  arrange(desc(n))
#%>% 
#ggplot(aes(word,n,fill=sentiment))+
#geom_col()+
#coord_flip()
bing_count

nrc <- get_sentiments("nrc")
nrc_sentiments <- data_comments %>% 
  inner_join(nrc) %>% 
  count(word,sentiment,sort=TRUE) %>% 
  acast(word~sentiment,value.var="n",fill=0) 
#%>% 
#comparison.cloud()

nrc_count <- data_comments %>% 
  inner_join(nrc) %>% 
  count(word,sentiment,sort=TRUE) %>% 
  filter(n>1500) %>% 
  mutate(n=ifelse(sentiment=="negative",-n,n))%>% 
  mutate(word=reorder(word,n)) %>% 
  arrange(desc(n))
#%>% 
#ggplot(aes(word,n))+
#facet_grid(~sentiment)+
#geom_col()+
#coord_flip()
nrc_count

region_data <-data_comments %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  inner_join(afinn) %>% 
  count(word,value) %>% 
  mutate(score=value*n) %>%
  group_by(neighbourhood_group_cleansed) %>% 
  summarise(mean_score=mean(score))

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


listing_summary <- final_listings %>% 
  group_by(neighbourhood_cleansed) %>%
  summarise(avg_price_pp = round(mean(price_per_pax),0),
            min_price_pp = round(min(price_per_pax),0),
            max_price_pp = round(max(price_per_pax),0)) %>%
  mutate_at(.vars = vars(neighbourhood_cleansed), .funs= funs(toupper))

subregion_data <-data_comments %>% 
  group_by(neighbourhood_cleansed) %>% 
  inner_join(afinn) %>% 
  count(word,value) %>% 
  mutate(score=value*n) %>%
  group_by(neighbourhood_cleansed) %>% 
  summarise(avg_sentiment_score=round(mean(score),0)) %>%
  mutate_at(.vars = vars(neighbourhood_cleansed), .funs= funs(toupper))

listing_summary <- right_join(subregion_data,listing_summary, c("neighbourhood_cleansed" = "neighbourhood_cleansed" ))

airbnb_map <- right_join(mpsz2,listing_summary, c("PLN_AREA_N" = 'neighbourhood_cleansed'))

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
                  fluidPage(theme = shinytheme('journal'), #https://rstudio.github.io/shinythemes/
                            #for intro page
                            titlePanel("Shiny PET: A Predictive, Exploratory and Text Application"),
                            fluidRow(
                              column(width = 6,
                                     h4('About'),
                                     p("Shiny PET is a user-friendly application that will enable users to make data-driven decisions without the need to understand programming languages or have extensive statistical knowledge."),
                                     p('We have used Airbnb data as our baseline for this project - data generated is rich in information, which consists of structured, unstructured (textual), and location data.'),
                                     h4('Application feature'),
                                     p("As seen in the figure on the right, this application has 3 modules – exploratory, text and predictive"),
                                     p('The Exploratory module allows users to perform exploratory and confirmatory analysis on selected variables to identify interesting patterns.'),
                                     p('The Text module allows users to analyse textual data from reviews to generate more quantitative insights.'),
                                     p('The predictive module enables users to prepare and build a variety of prediction models.'),
                                     tags$div(
                                       'For more information on this project, please visit our',
                                       tags$a(href="https://ourshinypet.netlify.app/", 
                                              "website")
                                     )
                              ),
                              column(width = 6,
                                     tags$img(src = 'nav.png',width = 500, height = 400))
                            )
                  )
          ),
            tabItem("EDA",
                    tabsetPanel(
                        tabPanel("Observe",
                                 observeUI('observe')
                        ),
                        tabPanel("Explore and confirm",
                                 fluidPage(
                                   titlePanel("Explore variables"),
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
                                       width = 3
                                     ),
                                     mainPanel(
                                       
                                       h4("Statistical test result (x and y variable only)"),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Distribution'",
                                         withSpinner(textOutput('test_method'),type =7, color = "#FF5A5F", size = 1),
                                         h5("Hypothesis:"),
                                         withSpinner(textOutput('null_hypo_ttest'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner( textOutput('alt_hypo_ttest'),type =7, color = "#FF5A5F", size = 1),
                                         h5('Confident interval:'),
                                         withSpinner(textOutput('ci'),type =7, color = "#FF5A5F", size = 1),
                                         h5('p-value:'),
                                         withSpinner(textOutput('pvalue'),type =7, color = "#FF5A5F", size = 1),
                                         p("If p-value is smaller than alpha (1 - confident interval), then there is enough statistical evidence to reject the null hypothesis.")
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Boxplot'",
                                         withSpinner(uiOutput('test_method_2ttest'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner(uiOutput('test_method_anova'),type =7, color = "#FF5A5F", size = 1),
                                         h5("Hypothesis:"),
                                         withSpinner(textOutput('null_hypo_2ttest'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner(textOutput('alt_hypo_2ttest'),type =7, color = "#FF5A5F", size = 1),
                                         h5('p-value:'),
                                         withSpinner(uiOutput('pvalue_2ttest'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner(uiOutput('pvalue_anova'),type =7, color = "#FF5A5F", size = 1),
                                         p("If p-value <0.05, then there is enough statistical evidence to reject the null hypothesis.")
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Mosaic'",
                                         withSpinner(textOutput('test_method_chisq'),type =7, color = "#FF5A5F", size = 1),
                                         h5("Hypothesis:"),
                                         withSpinner(textOutput('null_hypo_chisq'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner(textOutput('alt_hypo_chisq'),type =7, color = "#FF5A5F", size = 1),
                                         h5('p-value:'),
                                         withSpinner(textOutput('pvalue_chisq'),type =7, color = "#FF5A5F", size = 1),
                                         p("If p-value <0.05, then there is enough statistical evidence to reject the null hypothesis.")
                                         
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Scatter'",
                                         withSpinner(textOutput('test_method_cortest'),type =7, color = "#FF5A5F", size = 1),
                                         h5("Hypothesis:"),
                                         withSpinner(textOutput('null_hypo_cortest'),type =7, color = "#FF5A5F", size = 1),
                                         withSpinner(textOutput('alt_hypo_cortest'),type =7, color = "#FF5A5F", size = 1),
                                         h5('p-value:'),
                                         withSpinner(textOutput('pvalue_cortest'),type =7, color = "#FF5A5F", size = 1),
                                         p("If p-value is smaller than alpha (1 - confident interval), then there is enough statistical evidence to reject the null hypothesis.")
                                         
                                         
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Distribution'",
                                         withSpinner(plotlyOutput('dist'),type =7, color = "#FF5A5F", size = 1)
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Boxplot'",
                                         withSpinner(plotlyOutput('bbox'),type =7, color = "#FF5A5F", size = 1)
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Mosaic'",
                                         withSpinner(plotlyOutput('mosaic'),type =7, color = "#FF5A5F", size = 1)
                                       ),
                                       conditionalPanel(
                                         condition = "input.chart_type == 'Scatter'",
                                         withSpinner(plotlyOutput('scatter'),type =7, color = "#FF5A5F", size = 1)
                                       ),
                                       
                                       
                                     )
                                   )
                                   
                                   
                                 )),
                        tabPanel("Map",
                                 fluidPage(
                                   titlePanel("Mapping Airbnbs"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput(inputId = 'map_type',
                                                   label = 'Select map type',
                                                   choices = c('Point Symbol map',
                                                               'Choropleth map'),
                                                   selected = 'Point Symbol map'),
                                       uiOutput('point_view'),
                                       uiOutput('show_data'),
                                       width = 3
                                     ),
                                     mainPanel(
                                       withSpinner(leafletOutput('leaf_map', height = 450),type = 6, color = "#FF5A5F", size = 2),
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
                                 fluidPage(
                                     fluidRow(
                                         column(7, id = "col_word_cloud",
                                                box(width=12, height=550, solidHeader = F, title = strong("The Word Cloud"),
                                                    radioButtons("word_cloud_gram",NULL, c("Uni-gram","Bi-gram"), selected = "Uni-gram", inline = T),
                                                    #plotOutput("word_cloud_plot",height = "300px")
                                                    wordcloud2Output("word_cloud_plot",height = "470px"))
                                         ),
                                         column(5, id = "col_freq",
                                                box(width=12, height=550, solidHeader = F, title = strong("Word Frequency"),
                                                highchartOutput("word_freq_plot", height=500)
                                                )
                                                
                                         )
                                     )
                                 )
                                 
                        ),
                        tabPanel("Sentiment analysis",
                                 fluidPage(
                                     fluidRow(
                                         column(7, id = "col_polarity_cloud",
                                                box(width=12, height=550, solidHeader = F, title = strong("Polarity"),
                                                    radioButtons("word_polarity_plot",NULL, c("AFINN","BING","NRC"), selected = "BING", inline = T),
                                                    #plotOutput("word_cloud_plot",height = "300px")
                                                    wordcloud2Output("polarity_cloud_plot",height = "470px"))
                                         ),
                                         column(5, id = "col_polarity_freq",
                                                box(width=12, height=550, solidHeader = F, title = strong("Word Frequency"),
                                                    highchartOutput("polarity_freq_plot", height=500)
                                                )
                                                )
                                         )
                                     )
                        ),
                        tabPanel("Topic modeling",
                                 #put ui here
                        ),
                        tabPanel("Network analysis",
                                 fluidRow(
                                     box(width=12, height=500, solidHeader = F,
                                         tabsetPanel(type = 'pills',
                                                     id = 'network_panel',
                                                     tabPanel("Bi Directional Network", 
                                                              #sliderInput("bi_freq", "Min Frequency of Bi-grams:",min = 10, max = 200,value = 100, width = '20%'),
                                                              plotOutput("network_plot1")),
                                                     tabPanel("Correlation Network", plotOutput("network_plot2"))
                                         )
                                     )
                                 )
                        )
                        )),
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
                                 model_trainUI("mdlt")
                        ),
                        tabPanel("Model evaluation",
                                 model_evalUI("mdle")
                        )
            )
        )
    )
))

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
      
      ggplotly(hist,  height = 450, width = 800)
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
      
      ggplotly(bar,  height = 450, width = 800)
      
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
    
    boxly <- ggplotly(flip_chart, height = 450, width = 800) %>% layout(boxmode = "group")
    
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
    
    ggplotly(plt_m,  height = 450, width = 800)
    
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
    
    ggplotly(flip_chart,  height = 450, width = 800)
    
    
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
    vals$p.value
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
                  selected = 'Host status')} 
    
    else if (input$map_type == "Choropleth map") {
      selectInput(inputId = 'filter_point',
                  label = 'View map by',
                  choices = c('Average price' = 'avg_price',
                              'Average Sentiment score' = 'avg_sentiment_score'),
                  selected = 'Average price')}
    
  })
  
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
              palette = 'Reds')+
      tm_borders(lwd = 0.5, alpha = 1)
    
    
    cho_map <- tmap_leaflet(avg_price) %>%
      addTiles() %>%
      addProviderTiles('OneMapSG.Original', group = 'Original') %>%
      addProviderTiles('OneMapSG.Grey', group = 'Grey') %>%
      addProviderTiles('OneMapSG.Night', group = 'Night') %>%
      addLayersControl(baseGroups = c('Original', 'Grey', 'Night'),
                       options = layersControlOptions(collapsed = FALSE))
    
    sent_map <- tm_shape(mpsz2)+
      tm_polygons() +
      tm_shape(airbnb_map) +
      tm_fill('avg_sentiment_score',
              n = 6,
              style = 'pretty',
              palette = 'Reds')+
      tm_borders(lwd = 0.5, alpha = 1) 
    
    sent_cho_map <- tmap_leaflet(sent_map) %>%
      addTiles() %>%
      addProviderTiles('OneMapSG.Original', group = 'Original') %>%
      addProviderTiles('OneMapSG.Grey', group = 'Grey') %>%
      addProviderTiles('OneMapSG.Night', group = 'Night') %>%
      addLayersControl(baseGroups = c('Original', 'Grey', 'Night'),
                       options = layersControlOptions(collapsed = FALSE))
    
    
    if(input$filter_point == 'avg_price'){cho_map}
    else if(input$filter_point == 'avg_sentiment_score'){sent_cho_map}
    else if(input$filter_point == 'superhost'){superhost}
    else if (input$filter_point == 'room_type') {rooms}
    
  })
  
  output$szTable <- DT::renderDataTable({
    if(input$show_data){
      DT::datatable(data = listing_summary)
    }
  })
    #### Word Freq plot ####

    output$word_freq_plot  <- renderHighchart(
        hc <- highchart() %>%
            #hc_title(text = "Incremental Revenue and Total Cost by Offer Group") %>%
            hc_chart(type = "bar") %>%
            #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
            hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                       formatter = JS(paste0("function() {
                                       //console.log(this);
                                       //console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.x.name+'</span>:<b> '
                                       +Math.round(this.point.y.toFixed(0)/100)/10 + 'K' + '</b>';
                                       return result;
      }"))) %>%
            hc_xAxis(categories = data_count[1:100,]$word,
                     #labels = list(rotation = 0, step=1), title =list(text="Brand")
                     labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)
            )    %>%
            hc_add_series(name="Word", data = data_count[1:100,]$frequency, type ="column",
                          #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                          color = "#FF5A5F", showInLegend= F)
        #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
    )
    
    
    #### Wordcloud plot ####
    output$word_cloud_plot <- renderWordcloud2({
        
        if(input$word_cloud_gram == "Uni-gram"){
            
            set.seed(1234)
            # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
            #           max.words=100, random.order=FALSE, rot.per=0.35, 
            #           colors=brewer.pal(8, "Dark2"))
            d1 <- (data_count %>% filter(frequency>1) %>% arrange(desc(frequency)))[1:100,]
            wordcloud2(data = data_count, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                       ellipticity = 0.65)
            
        } else if(input$word_cloud_gram == "Bi-gram"){
            
            set.seed(1234)
            # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
            #           max.words=100, random.order=FALSE, rot.per=0.35, 
            #           colors=brewer.pal(8, "Dark2"))
            d2 <- (bigram_data_count %>% filter(n>1) %>% arrange(desc(n)))[1:100,]
            wordcloud2(data = bigram_data_count, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                       ellipticity = 0.65)
            
            
        }
        
    })
    
    
    #### POLARITY Sentiment Analysis ####
    
    output$polarity_freq_plot <- renderHighchart(
            hc <- highchart() %>%
                hc_chart(type = "bar") %>%
                #hc_plotOptions(bar = list(getExtremesFromAll = T)) %>% 
                hc_tooltip(crosshairs = TRUE, shared = FALSE,useHTML=TRUE,
                           formatter = JS(paste0("function() {
                                       //console.log(this);
                                       //console.log(this.point.y);
                                       var result='';
                                       result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.x.name+'</span>:<b> '
                                       +Math.round(this.point.y.toFixed(0)/100)/10 + 'K' + '</b>';
                                       return result;
      }"))) %>%
                hc_xAxis(categories = afinn_count[1:100,]$word,
                         #labels = list(rotation = 0, step=1), title =list(text="Brand")
                         labels = list(style = list(fontSize= '11px')), max=20, scrollbar = list(enabled = T)
                )    %>%
                hc_add_series(name="Word", data = afinn_count[1:100,]$n, type ="column",
                              #max=max(d()$freq), tickInterval = max(d()$freq)/4, alignTicks = F,
                              color = "#FF5A5F", showInLegend= F) 
            #hc_legend(layout = "vertical", align = "right", verticalAlign = "top", width=120, itemStyle = list(fontSize= '10px'))
            )
    
    #### POLARITY Wordcloud plot ####
    output$polarity_cloud_plot <- renderPlot({
        
        if(input$polarity_cloud_plot == "AFINN"){
            
            set.seed(1234)
            # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
            #           max.words=100, random.order=FALSE, rot.per=0.35, 
            #           colors=brewer.pal(8, "Dark2"))
            c1 <- (afinn_count %>% filter(frequency>1) %>% arrange(desc(n)))[1:100,]
            comparison.cloud(data = afinn_sentiments, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                             ellipticity = 0.65)
            
        } else if (input$polarity_cloud_plot == "BING"){
            
            set.seed(1234)
            # wordcloud(words = d()$word, freq = d()$freq, scale = c(3,0.5), min.freq = 3,
            #           max.words=100, random.order=FALSE, rot.per=0.35, 
            #           colors=brewer.pal(8, "Dark2"))
            c2 <- (bing_count %>% filter(n>1) %>% arrange(desc(n)))[1:100,]
            comparison.cloud(data = bing_sentiments, size=0.8, minSize = 0.0, fontWeight = 'bold', 
                       ellipticity = 0.65)
            
            
            
        }
        
        
    })
    
    
####  bigram_graph ####

    output$network_plot1 <- renderPlot({
        progress <- shiny::Progress$new()
        progress$set(message = "Bi Directional Graph", value = 0.2)
        on.exit(progress$close())
        progress$set(detail = "Creating Bitokens..", value = 0.6)
        
        progress$set(detail = "Plotting..", value = 0.8)
        bigram_graph <- head(bigram_data_count %>% arrange(desc(n)),150) %>% graph_from_data_frame()
        
        
        #bigram_graph
        #library(ggraph)
        set.seed(2020)
        # ggraph(bigram_graph, layout = "fr") +
        #   geom_edge_link() +
        #   geom_node_point() +
        #   geom_node_text(aes(label = name), vjust = 1, hjust = 1)
        
        a <- grid::arrow(type = "closed", length = unit(.08, "inches"))
        ggraph(bigram_graph, layout = "fr") +
            geom_edge_link() +
            geom_node_point(color = "#FF5A5F", size = 1) +
            geom_node_text(aes(label = name), vjust = 1, hjust = 1, size=2) +
            theme_void()
        
        
    })
    
    output$network_plot2 <- renderPlot({
        progress <- shiny::Progress$new()
        progress$set(message = "Correlation Plot", value = 0.2)
        on.exit(progress$close())
        progress$set(message = "Creating Pairwise words", value = 0.4)
        
        #x <- data.frame(text = sapply(docs(), as.character), stringsAsFactors = FALSE)
        #x$tweet_nbr <- 1:nrow(x)
        #tweet_word <- x %>% unnest_tokens(word, text)
        
        word_corr <- data_count %>% 
            filter(frequency >= 300) %>% 
            pairwise_cor(word, frequency, sort = TRUE)
        
        #progress$set(detail = "Plotting..", value = 0.8)
        set.seed(1234)
        word_corr %>%
            filter(correlation > .5) %>%
            graph_from_data_frame() %>%
            ggraph(layout = "fr") +
            geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "#FF5A5F") +
            geom_node_point(size = 2) +
            geom_node_text(aes(label = name), repel = TRUE,
                           point.padding = unit(0.2, "lines")) +
            theme_void()
        
        
    })
    
    #### SPATIAL PLOT ####
    
    output$chart <- renderHighchart({
        highchart(type = "map") %>% 
                                        hc_add_series_map(data=mpsz_nosea, df=region_data(),value = "PLN_AREA_N", joinBy = "mean_score") %>% 
                                        hc_colorAxis(stops = color_stops()) %>% 
                                        hc_tooltip(useHTML=TRUE,headerFormat='',pointFormat = paste0(subregion_data$mean_score,'  {point.neighbourhood_cleansed} Sales : {point.mean_score} ')) %>% 
                                        #hc_title(text = 'Global Sales') %>% 
                                        #hc_subtitle(text = paste0('Year: ',input$yearid)) %>% 
                                        hc_exporting(enabled = TRUE,filename = 'custom')
    })   
    
    
    
    
    
    
    
    
    
    ########### predictive server file ###########
    return_val1 <- data_splittingServer("ds", final_listings)
    feat_selectServer("fs", final_listings, return_val1,
                      return_val1$triggerReset)
    return_trf <- data_transformServer("dtf", final_listings, return_val1,
                                       return_val1$triggerReset)
    return_train <- model_trainServer("mdlt", return_trf, return_val1$targetvar,
                                      return_val1$triggerReset)
    model_evalServer("mdle", return_train, return_trf$selected_var,
                     return_val1$targetvar, return_val1$triggerReset)
    ############################################
    
}

# Run the application 
shinyApp(ui = ui, server = server)