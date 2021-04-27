model_evalUI <- function(id){
  navlistPanel(id = NS(id, "navpanel_eval"), "Evaluation", widths = c(2,10),
               tabPanel("Info",
                        uiOutput(NS(id,"Oui_eval_info")),
                        actionButton(NS(id, "btn_allMetric"),
                                     "Collect model performance")
               ),
               tabPanel("Performance",
                        uiOutput(NS(id, "Oui_performance"))
                        ),
               tabPanel("Predict",
                          uiOutput(NS(id, "Oui_predict"))
                        )
  )
}

get_prediction <- function(final_model, datasplit, newinput){
  listing_pred <- predict(final_model, new_data = newinput)
  pred_val <- listing_pred$.pred
  return(pred_val)
}

model_evalServer <- function(id, return_train, selected_var, targetvar, trigger_reset){
  moduleServer(id, function(input, output, session){
    
    Rreset <- reactiveVal(0)
    
    observeEvent(trigger_reset(),{
      if (trigger_reset() != Rreset()){
        Rreset(trigger_reset())
        output$Oplot_metric <- renderPlotly(NULL)
        output$Opred_val <- renderText(NULL)
        output$Otext_model <- renderText(NULL)
        output$Oui_performance <- renderUI(NULL)
        output$Oui_predict <- renderUI(NULL)
        output$Oboxes <- renderUI(NULL)
        updateNavlistPanel(session,
                           inputId = "navpanel_eval",
                           selected = "Info")
      }
    })
    
    output$Oplot_metric <- renderPlotly(NULL)
    output$Opred_val <- renderText(NULL)
    
    observeEvent(input$btn_allMetric, {
      if(
        (is.null(return_train$fm_lm()) & is.null(return_train$fm_glm()) &
         is.null(return_train$fm_tree()) & is.null(return_train$fm_randomf()) &
         is.null(return_train$fm_xgb()))
      ){
        shinyalert(text="Please train and validate at least 1 model",
                   type="error")
      }
      validate(
        need((!is.null(return_train$fm_lm()) | !is.null(return_train$fm_glm()) |
                !is.null(return_train$fm_tree()) | !is.null(return_train$fm_randomf()) |
                !is.null(return_train$fm_xgb())),
             "at least 1 trained model"),
        need(length(return_train$datasplit())==4, "Please go through data transformation process")
      )
      
      metric_p <- NULL
      available_model <- NULL
      
      if (!is.null(return_train$fm_lm())){
        metric_p <- metric_p %>%
          bind_rows(return_train$fm_lm() %>% mutate(model = "LM"))
        available_model <- c(available_model, "LM")
      }
      if (!is.null(return_train$fm_glm())){
        metric_p <- metric_p %>%
          bind_rows(return_train$fm_glm() %>% mutate(model = "GLM"))
        available_model <- c(available_model, "GLM")
      }
      if (!is.null(return_train$fm_tree())){
        metric_p <- metric_p %>%
          bind_rows(return_train$fm_tree() %>% mutate(model = "DTree"))
        available_model <- c(available_model, "DTree")
      }
      if (!is.null(return_train$fm_randomf())){
        metric_p <- metric_p %>%
          bind_rows(return_train$fm_randomf() %>% mutate(model = "RdmForest"))
        available_model <- c(available_model, "RdmForest")
      }
      if (!is.null(return_train$fm_xgb())){
        metric_p <- metric_p %>%
          bind_rows(return_train$fm_xgb() %>% mutate(model = "XGBoost"))
        available_model <- c(available_model, "XGBoost")
      }
      
      updateNavlistPanel(session,
                         inputId = "navpanel_eval",
                         selected = "Performance")
      
      output$Oplot_metric <- renderPlotly({
        metric_p <- metric_p %>%
        ggplot(aes(reorder_within(model, -.estimate, .metric), .estimate,
                   text = paste0("Model: ", model, "\n",
                                 .metric, ": ", round(.estimate, 3)))) +
          geom_bar(stat = 'identity') +
          scale_x_reordered() +
          facet_wrap(~ .metric, scales = "free") +
          ylab("Estimate") +
          theme(axis.title.x = element_blank(),
                panel.spacing.y = unit(2, "lines"))
        ggplotly(metric_p, tooltip = c("text"))
      })
      
      output$Oui_performance <- renderUI({
        fluidPage(
          h4("Best model performance comparison"),
          plotlyOutput(NS(id, "Oplot_metric")) %>%
            withSpinner(color="#FF5A5F", type=6),
          br(),
          fluidRow(
            column(4,
                   pickerInput(
                     inputId = NS(id, "Imodel"),
                     label = "Choose model:",
                     choices = available_model)
            ),
            column(8,
                   br(),
                   actionButton(NS(id, "btn_finalmodel"), "Select final model")
            )
          )
        )
      })
    })
    
    observeEvent(input$btn_finalmodel, {
      updateNavlistPanel(session,
                         inputId = "navpanel_eval",
                         selected = "Predict")
      
      listing_train <- training(return_train$datasplit())
      
      list_UI <- list()
      list_UI_num <- list()
      list_UI_fct <- list()
      
      for (i in 1:length(selected_var())){
        data_type <- listing_train %>%
          select(selected_var()[i]) %>%
          sapply(.,class)
        
        if ((data_type=="numeric") | (data_type=="integer")){
          data_min <- listing_train %>%
            select(selected_var()[i]) %>%
            min()
          data_max <- listing_train %>%
            select(selected_var()[i]) %>%
            max()
          
          box_item <- box(width = 3, #background = "teal",
                          title = p(selected_var()[i],
                                     style = "display:inline; color:#484848"),
                          sliderInput(NS(id, selected_var()[i]),
                                      label = NULL,
                                      min = data_min,
                                      max = data_max,
                                      value = data_min)
                          )
          list_UI_num[[selected_var()[i]]] <- box_item
          
        } else if (data_type=="factor"){
          fct_level <- listing_train %>%
            select(selected_var()[i]) %>%
            lapply(., levels)
          box_item <- box(width = 3, #background = "teal",
                          title = p(selected_var()[i],
                                     style = "display:inline; color:#484848"),
                          pickerInput(NS(id, selected_var()[i]),
                                      label = NULL,
                                      choices = fct_level)
                          )
          list_UI_fct[[selected_var()[i]]] <- box_item
        }
      }
      
      list_UI <- c(list_UI_num, list_UI_fct)
      
      output$Oui_predict <- renderUI({
        fluidPage(
          fluidRow(
            circleButton(inputId = NS(id, "IquestionPredict"),
                         icon = icon("question"),
                         status = "info", size = "xs"),
            h4("Input variables")
          ),
          fluidRow(
            tabPanel("Ibox_var", uiOutput(NS(id, "Oboxes")))
            ),
          fluidRow(
            column(4,
                   textOutput(NS(id, "Otext_model")),
                   actionButton(NS(id, "btn_predict"), "Predict")
                   ),
            column(8,
                   textOutput(NS(id, "Opred_val"))
                   )
          )
        )
        })
      
      output$Oboxes <- renderUI(list_UI)
      
      output$Otext_model <- renderText({
          paste0("Prediction using best ", input$Imodel, " model.")
      })
    })
    
    observeEvent(input$btn_predict, {
      output$Opred_val <- renderText({
        paste0("Please wait...")
      })
        df_toPred <- NULL
        for (i in 1:length(selected_var())){
          ui_idname <- selected_var()[i]
          df_toPred[[ui_idname]] <- input[[ui_idname]]
        }
        df_toPred[["id"]] <- 1
        df_toPred <- as.data.frame(df_toPred)
        
        newinput <- df_toPred
        datasplit <- return_train$datasplit()
        
        if (input$Imodel=="LM"){
          listing_pred <- predict(return_train$fit_lm(), new_data = newinput)
          pred_val <- listing_pred$.pred
          
        }else if (input$Imodel=="GLM"){
          tune_result <- return_train$glm_result()
          model_wf <- return_train$glm_wf()
          pred_val <- get_prediction(return_train$final_glm(), datasplit, newinput)
          
        }else if (input$Imodel=="DTree"){
          tune_result <- return_train$tree_result()
          model_wf <- return_train$tree_wf()
          pred_val <- get_prediction(return_train$final_tree(), datasplit, newinput)
          
        }else if (input$Imodel=="RdmForest"){
          tune_result <- return_train$randomf_result()
          model_wf <- return_train$randomf_wf()
          pred_val <- get_prediction(return_train$final_randomf(), datasplit, newinput)
          
        }else if (input$Imodel=="XGBoost"){
          tune_result <- return_train$xgb_result()
          model_wf <- return_train$xgb_wf()
          pred_val <- get_prediction(return_train$final_xgboost(), datasplit, newinput)
          
        }
        
        output$Opred_val <- renderText({
          paste0("Predicted ", targetvar(), ": ", round(pred_val,2))
        })

    })
    
    output$Oui_eval_info <- renderUI({
      fluidPage(
        h3("Model evaluation"),
        p("In this section, we will bring the best models from each model type
          and compare their performance metric. Based on final model selection,
          new values of input variables can be entered to predict the target variable.")
      )
    })
    
    observeEvent(input$IquestionPredict,{
      text_item <- paste0("This section allows user to provide new input value ",
                          "for the selected variables and use the selected best ",
                          "model to predict the target variable.")
      shinyalert(text=text_item,
                 type="info")
    })
    })
}
