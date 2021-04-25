#' Data transformation
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 

data_transformUI <- function(id){
  tabsetPanel(id = NS(id, "tabset_dtf"),
    tabPanel("Variables and Recipe",
             fluidPage(
               circleButton(inputId = NS(id, "Iquestion"), icon = icon("question"),
                            status = "info", size = "xs"),
                multiInput(
                  inputId = NS(id, "Inum_vars_sel"),
                  label = "Variable selection :",
                  choices = character(0),
                  width = "900px"),
                fluidRow(
                  column(10,
                         actionButton(NS(id, "btn_all_num"), "Select all"),
                         actionButton(NS(id, "btn_des_all_num"), "Deselect all")
                         ),
                  column(2,
                         actionButton(NS(id, "btn_rcpTransform"), "Prepare recipe")
                         ),
                ),
                br(),
                fluidRow(
                  column(6,
                         textOutput(NS(id, "Otransform_recipe_title")),
                         verbatimTextOutput(NS(id, "Otransform_recipe"))
                  ),
                  column(6,
                         textOutput(NS(id, "Otransform_recipe2_title")),
                         verbatimTextOutput(NS(id, "Otransform_recipe2"))
                  ),
                ),
                uiOutput(NS(id,"button2"))
                )),
    tabPanel("Transformed variable",
             uiOutput(NS(id, "Oui_trfVar"))
             )
  )
}

data_transformServer <- function(id, final_listings, return_val1, trigger_reset){
  moduleServer(id, function(input, output, session){
    
    Rreset <- reactiveVal(0)
    observeEvent(trigger_reset(),{
      if (trigger_reset() != Rreset()){
        Rreset(trigger_reset())
        output$Otransformed_plot <- renderPlotly(NULL)
        output$Oplot_traintest_dense <- renderPlotly(NULL)
        output$Oplot_traintest_bar <- renderPlotly(NULL)
        output$Oplot_traintest_dense <- renderPlotly(NULL)
        output$Oplot_traintest_bar <- renderPlotly(NULL)
        output$Otransformed_plot <- renderPlotly(NULL)
        RtrainTProcessed(NULL)
        RrcpTransform(NULL)
        RrcpTransform2(NULL)
        Rdatasplit(NULL)
        Rformula(NULL)
        output$Otransform_recipe_title <- NULL
        output$Otransform_recipe <- NULL
        output$Otransform_recipe2_title <- NULL
        output$Otransform_recipe2 <- NULL
        output$button2 <- renderUI(NULL)
        output$Oui_trfVar <- renderUI(NULL)
        updateTabsetPanel(session,
                          inputId = "tabset_dtf",
                          selected = "Variables and Recipe")
        updateMultiInput(
          session = session,
          inputId = "Inum_vars_sel",
          selected = character(0)
        )
      }
    })
    
    output$Otransformed_plot <- renderPlotly(NULL)
    output$Oplot_traintest_dense <- renderPlotly(NULL)
    output$Oplot_traintest_bar <- renderPlotly(NULL)
    
    observe({
      var_list <- return_val1$var_list()
      
      updateMultiInput(
        session = session,
        inputId = "Inum_vars_sel",
        choices = var_list)
    })

    observeEvent(input$btn_all_num, {
      updateMultiInput(
        session = session,
        inputId = "Inum_vars_sel",
        selected = return_val1$var_list()
      )
    })

    observeEvent(input$btn_des_all_num, {
      updateMultiInput(
        session = session,
        inputId = "Inum_vars_sel",
        selected = character(0)
      )
    })
 
    ################

    RtrainTProcessed <- reactiveVal(NULL)
    RrcpTransform <- reactiveVal(NULL)
    RrcpTransform2 <- reactiveVal(NULL)
    Rdatasplit <- reactiveVal(NULL)
    Rformula <- reactiveVal(NULL)
    
    observeEvent(input$btn_rcpTransform,{
      validate(
        need(length(input$Inum_vars_sel) > 0,
             'Please select predictor variables')
      )
      #split again based on user selection
      set.seed(1234)
      listing_split <- final_listings %>%
        drop_na(isolate(return_val1$targetvar())) %>% #remove rows where target variable is missing
        select(isolate(input$Inum_vars_sel), isolate(return_val1$targetvar())) %>%
        na.omit() %>% #remove rows where there is missing value
        mutate(id = row_number()) %>%
        initial_split(prop = return_val1$Iprop()/100, strata = isolate(return_val1$targetvar()))
      
      Rdatasplit(listing_split)
      
      f <- as.formula(paste(isolate(return_val1$targetvar()), "~",
                            paste(c(isolate(input$Inum_vars_sel), "id"), collapse="+")))
      
      Rformula(f)
      
      listing_train <- training(Rdatasplit())
      
      rcpTransform <- recipe(f, data = listing_train) %>%
        update_role(id, new_role = "id variable") %>%
        # step_naomit(all_predictors(), skip = TRUE) %>% #remove rows with NA
        step_corr(all_numeric(), -all_outcomes(), threshold = 0.7,
                  method = "pearson") %>% #remove correlated variables at 0.7 threshold
        step_normalize(all_numeric(), -id, -all_outcomes()) %>%
        step_other(all_predictors(), -all_numeric(),
                   threshold = 0.05, other = "Others") %>%
        step_dummy(all_nominal(), -all_outcomes())

      rcpTransform2 <- recipe(f, data = listing_train) %>%
        update_role(id, new_role = "id variable") %>%
        step_other(all_predictors(), -all_numeric(),
                   threshold = 0.05, other = "Others")
      
      output$Otransform_recipe_title <- renderText({paste("Recipe for linear regression and boosted tree")})
      output$Otransform_recipe <- renderPrint(rcpTransform)
      output$Otransform_recipe2_title <- renderText({paste("Recipe for decision tree and random forest")})
      output$Otransform_recipe2 <- renderPrint(rcpTransform2)
      
      output$button2 <- renderUI({
        actionButton(NS(id, "btn_rcpApply"), "Transform variables")
      })
      
      RrcpTransform(rcpTransform)
      RrcpTransform2(rcpTransform2)
      })
    
    observeEvent(input$btn_rcpApply,{
      validate(
        need(RrcpTransform(), 'Please prepare recipe')
      )
      #see the transformed data
      listingTrain_T <- RrcpTransform() %>% prep() %>% juice()

      listingTrain_T %>%
        gather() %>%
        ggplot(aes(x = value)) +
        facet_wrap(~ key, scales = "free", ncol = 4) +
        geom_bar()

      listing_train_var <- training(Rdatasplit()) %>%
        keep(is.numeric) %>%
        names()

      listingTrain_T_var <- listingTrain_T %>%
        keep(is.numeric) %>%
        names()

      intersect_vars <- intersect(listing_train_var, listingTrain_T_var)
      intersect_vars <- intersect_vars[intersect_vars != "id"]

      train_proc <- training(Rdatasplit()) %>%
        select(all_of(intersect_vars)) %>%
        mutate(processed = "original")

      train_T_proc <- listingTrain_T %>%
        select(all_of(intersect_vars)) %>%
        mutate(processed = "processed")

      RtrainTProcessed(rbind(train_proc, train_T_proc))
      
      listing_train <- training(Rdatasplit())
      listing_test <- testing(Rdatasplit())
      
      train_grp <- listing_train %>%
        mutate(split = "training")
      
      test_grp <- listing_test %>%
        mutate(split = "test")
      
      trainTest_grp <- rbind(train_grp, test_grp)
      
      output$Oui_trfVar <- renderUI({
        fluidPage(
          circleButton(inputId = NS(id, "Iquestion2"), icon = icon("question"),
                       status = "info", size = "xs"),
          h4("Train-test split"),
          (div(style='height: 500px; overflow-x: scroll',
               plotlyOutput(NS(id, "Oplot_traintest_dense"), width = "auto") %>%
                 withSpinner(color="#FF5A5F", type=6)
          )),
          plotlyOutput(NS(id, "Oplot_traintest_bar"), width = 'auto') %>%
            withSpinner(color="#FF5A5F", type=6),
          br(),
          pickerInput(
            inputId = NS(id, "Iproc_var"),
            label = "Check transformed variables:",
            choices = intersect_vars,
            options = list(size = 10)),
          plotlyOutput(NS(id, "Otransformed_plot"), width = 'auto') %>%
            withSpinner(color="#FF5A5F", type=6)
        )
      })
      
      output$Oplot_traintest_dense <- renderPlotly({
        width_dynamic <- 2000*length(isolate(input$Inum_vars_sel))/(2*10)
        
        trainTest_p <- trainTest_grp %>%
          select(where(is.numeric) | split) %>%
          select(-id) %>%
          gather(key, value, -split) %>%
          ggplot(aes(x = value, fill = split)) +
          facet_wrap(~ key, scales = "free", nrow = 2) +
          geom_density(alpha=0.5) +
          ggtitle("Numerical variables") +
          theme(panel.spacing.y = unit(3, "lines"),
                legend.title = element_blank(),
                plot.margin = margin(1, 5, 0, 5))
        ggplotly(trainTest_p,
                 width = width_dynamic, height = 470) %>%
          layout(legend = list(orientation = "h", x=0.02, y=0.55))
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
      
      updateTabsetPanel(session,
                        inputId = "tabset_dtf",
                        selected = "Transformed variable")
    })
    

    observeEvent(input$Iproc_var, {
      output$Otransformed_plot <- renderPlotly(
      ggplotly(
        RtrainTProcessed() %>%
          select(all_of(input$Iproc_var) | processed) %>%
          gather(key, value, -processed) %>%
          ggplot(aes(x = value, fill = processed)) +
          facet_wrap(~ processed, scales = "free_x") +
          geom_histogram(alpha=0.5))
      )
    })
    ######################
    observeEvent(input$Iquestion,{
      text_item <- paste0("Select variables from the list as the final selection for ",
                          "developing predictive model. Click on \"Prepare recipe\"",
                          " to view the predefined data transformation steps using ",
                          "recipe framework from tidymodels. Apply the steps ",
                          "by clicking \"Transform variables\" button.")
      shinyalert(text=text_item,
                 type="info")
    })
    
    observeEvent(input$Iquestion2,{
      text_item <- paste0("The following plots show the distribution of variables between the ",
                          "training and test set (after final variable selection). ",
                          "Some of the numerical variables have been transformed ",
                          "based on the recipe in the previous page. The value, ",
                          "before and after transformation, are also shown below.")
      shinyalert(text=text_item,
                 type="info")
    })
    
    return(list(
      rcp1 = RrcpTransform,
      rcp2 = RrcpTransform2,
      Rformula = Rformula,
      datasplit = Rdatasplit,
      selected_var = isolate(reactive({input$Inum_vars_sel}))
    ))
  })
}
