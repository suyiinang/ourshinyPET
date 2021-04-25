#' Target variable selection,
#' Data splitting,
#' Distribution plot
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 

data_splittingUI <- function(id){
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        circleButton(inputId = NS(id, "Iquestion"), icon = icon("question"),
                     status = "info", size = "xs"),
        pickerInput(
          inputId = NS(id, "Itarget_var"),
          label = "Choose response variable:",
          choices = c("price", "review_scores_rating", "price_per_pax"),
          selected = "price"),
        sliderInput(NS(id, "Itrain_prop"), "Training set (%)",
                    value = 80, min = 50, max = 100, step = 5),
        actionButton(NS(id, "btn_split_data"), "Split data")
      , width = 3),
      mainPanel(
        (div(style='height: 500px; overflow-x: scroll',
                       plotlyOutput(NS(id, "Oplot_traintest_dense"))%>%
               withSpinner(color="#FF5A5F", type=6)
                       )),
        
        plotlyOutput(NS(id, "Oplot_traintest_bar")) %>% withSpinner(color="#FF5A5F", type=6)
      , width = 9)
    )
  )
}

data_splittingServer <- function(id, final_listings){
  moduleServer(id, function(input, output, session){
    set.seed(1234)
    
    output$Oplot_traintest_dense <- renderPlotly(NULL)
    output$Oplot_traintest_bar <- renderPlotly(NULL)
    
    Rlisting_split <- reactiveVal(NULL)
    Rtarget_var <- reactiveVal(NULL)
    Rtrigger <- reactiveVal(0)
    Rvar_list <- reactiveVal(NULL)
    
    observeEvent(input$btn_split_data, {
      shinyalert("This step will reset all the previously built model and their results.
                 Are you sure?",
                 showCancelButton = TRUE, type = "warning", closeOnEsc = FALSE,
                 closeOnClickOutside = FALSE, inputId = "saReset")
    })  
    observeEvent(input$saReset, {
      req(input$saReset)
      
        if (is.null(Rtarget_var())){
          Rtarget_var(input$Itarget_var)
        } else {
          Rtarget_var(input$Itarget_var)
          Rtrigger(Rtrigger()+1)
        } 
        
        listing_split <- final_listings %>%
          drop_na(input$Itarget_var) %>% #remove rows where target variable is missing
          mutate(id = row_number()) %>%
          initial_split(prop = input$Itrain_prop/100, strata = input$Itarget_var)
        
        Rlisting_split(listing_split)
        
        listing_train <- training(listing_split)
        listing_test <- testing(listing_split)
        
        var_list <- listing_train %>%
          select(-all_of(input$Itarget_var), -id) %>%
          names() %>%
          str_sort()
        Rvar_list(var_list)
        
        train_grp <- listing_train %>%
          mutate(split = "training")
        
        test_grp <- listing_test %>%
          mutate(split = "test")
        
        trainTest_grp <- rbind(train_grp, test_grp)
        
        output$Oplot_traintest_dense <- renderPlotly({
          trainTest_p <- trainTest_grp %>%
            select(where(is.numeric) | split) %>%
            select(-id) %>%
            gather(key, value, -split) %>%
            ggplot(aes(x = value, fill = split)) +
            facet_wrap(~ key, scales = "free", nrow = 2) +
            geom_density(alpha=0.5) +
            ggtitle("Numerical variables") +
            theme(panel.spacing.y = unit(3, "lines"),
                  plot.margin = margin(1, 5, 0, 5))
          ggplotly(trainTest_p,
                   width = 6000, height = 470) %>%
            layout(legend = list(orientation = "h", x=0.03, y=1.15))
          })
        
        output$Oplot_traintest_bar <- renderPlotly({
          trainTest_p2 <- trainTest_grp %>%
            select(where(negate(is.numeric)) | split) %>%
            gather(key, value, -split) %>%
            filter(!is.na(value)) %>%
            ggplot(aes(x = value, fill = split)) +
            facet_wrap(~ key, scales = "free", ncol = 3) +
            geom_bar() +
            ggtitle("Categorical variables") +
            theme(axis.text.x = element_blank())
          ggplotly(trainTest_p2)
          })
      
      })
    
    observeEvent(input$Iquestion,{
      text_item <- paste0("Select response variable from the dropdown list and
                 choose proportion of training set using the slider.", "\n",
                 "Once done, click on \"Split data\" button to see the distribution ",
                 "of data between training and test set.")
      shinyalert(text=text_item,
                 type="info")
    })
    
    
    return(list(
      datasplit = Rlisting_split,
      targetvar = Rtarget_var,
      Iprop = reactive({input$Itrain_prop}),
      var_list = Rvar_list,
      triggerReset = Rtrigger
      )
    )
    })
}
