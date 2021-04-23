#' Data transformation
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' 

model_trainUI <- function(id){
  tabsetPanel(
    tabPanel("Linear Model",
             navlistPanel(id = NS(id, "navpanel_LM"), "Linear model", widths = c(2,10),
                          tabPanel("Info",
                                   uiOutput(NS(id, "Oui_LM_info")),
                                   br(),
                                   actionButton(NS(id, "btn_trainLM"),
                                                "Train linear regeression model")
                                   ),
                          tabPanel("Training",
                                   uiOutput(NS(id, "Oui_LMtrain"))
                                   ),
                          tabPanel("Validation",
                                   uiOutput(NS(id, "Oui_LMvalidate"))
                                   ),
                          tabPanel("Prediction error",
                                   uiOutput(NS(id, "Oui_LMerror"))
                                   )
                          )
             ),
    tabPanel("Cross Validation",
             uiOutput(NS(id, "Oui_CV_info")),
             br(),
             pickerInput(
               inputId = NS(id, "Ikfold"),
               label = "K-fold CV:",
               choices = c(3,5,10),
               selected = 3),
             useShinyalert(),
             actionButton(NS(id, "btn_crossval"), "Prepare CV training set")
             ),
    tabPanel("GLM",
             navlistPanel(id = NS(id, "navpanel_GLM"), "GLM", widths = c(2,10),
                          tabPanel("Info",
                                   uiOutput(NS(id, "Oui_GLM_info")),
                                   br(),
                                   actionButton(NS(id, "btn_trainGLM"), "Train GLM model")
                                   ),
                          tabPanel("Training",
                                   uiOutput(NS(id, "Oui_GLMtrain"))
                                   ),
                          tabPanel("Validation",
                                   br(),
                                   plotlyOutput(NS(id, "Oplot_GLMpred")) %>%
                                     withSpinner(color="#FF5A5F"),
                                   br(),
                                   tableOutput(NS(id, "Otbl_finalGLM"))
                          )
                          )
             ),
    tabPanel("Decision Tree",
             navlistPanel(id = NS(id, "navpanel_DT"), "Decision Tree", widths = c(2,10),
                          tabPanel("Info",
                                   uiOutput(NS(id, "Oui_DT_info")),
                                   br(),
                                   actionButton(NS(id, "btn_trainDT"), "Train DT model")
                          ),
                          tabPanel("Training",
                                   uiOutput(NS(id, "Oui_DTtrain"))
                          ),
                          tabPanel("Validation",
                                   fluidPage(
                                     br(),
                                     plotlyOutput(NS(id, "Oplot_DTpred")) %>%
                                       withSpinner(color="#FF5A5F"),
                                     br(),
                                     tableOutput(NS(id, "Otbl_finalDT"))
                                   ) 
                          )
             )
             ),
    tabPanel("Random Forest",
             navlistPanel(id = NS(id, "navpanel_RF"), "Random Forest", widths = c(2,10),
                          tabPanel("Info",
                                   uiOutput(NS(id, "Oui_RF_info")),
                                   br(),
                                   actionButton(NS(id, "btn_trainRF"), "Train RF model")
                          ),
                          tabPanel("Training",
                                   uiOutput(NS(id, "Oui_RFtrain"))
                          ),
                          tabPanel("Validation",
                                   fluidPage(
                                     br(),
                                     plotlyOutput(NS(id, "Oplot_RFpred")) %>%
                                       withSpinner(color="#FF5A5F"),
                                     br(),
                                     tableOutput(NS(id, "Otbl_finalRF"))
                                   ) 
                          )
             )
    ),
    tabPanel("Boosted Tree",
             navlistPanel(id = NS(id, "navpanel_BT"), "Boosted Tree", widths = c(2,10),
                          tabPanel("Info",
                                   uiOutput(NS(id, "Oui_BT_info")),
                                   br(),
                                   actionButton(NS(id, "btn_trainBT"), "Train BT model")
                          ),
                          tabPanel("Training",
                                   uiOutput(NS(id, "Oui_BTtrain"))
                          ),
                          tabPanel("Validation",
                                   fluidPage(
                                     br(),
                                     plotlyOutput(NS(id, "Oplot_BTpred")) %>%
                                       withSpinner(color="#FF5A5F"),
                                     br(),
                                     tableOutput(NS(id, "Otbl_finalBT"))
                                   ) 
                          )
             )
    )
  )
}

model_trainServer <- function(id, return_trf, target_var, trigger_reset){
  moduleServer(id, function(input, output, session){
    
    RfitResult <- reactiveVal(NULL)
    Rlisting_fit <- reactiveVal(NULL)
    Rlisting_pred <- reactiveVal(NULL)
    Rtop_err_tbl <- reactiveVal(NULL)
    Rlisting_kfolds <- reactiveVal(NULL)
    Rfinal_metric_lm <- reactiveVal(NULL)
    Rreset <- reactiveVal(0)
    
    observeEvent(trigger_reset(),{
      if (trigger_reset() != Rreset()){
        Rreset(trigger_reset())
        RfitResult(NULL)
        Rlisting_fit(NULL)
        Rlisting_pred(NULL)
        Rtop_err_tbl(NULL)
        Rlisting_kfolds(NULL)
        Rfinal_metric_lm(NULL)
        output$Ocoeff_est <- NULL
        output$Otraining_fit <- NULL
        output$OtestRsq <- NULL
        output$Opred_error <- NULL
        output$Ovalidation_lm <- NULL
        output$Oui_LMtrain <- renderUI(NULL)
        output$Oui_LMvalidate <- renderUI(NULL) 
        output$Oui_LMerror <- renderUI(NULL)
        output$Oui_GLMtrain <- renderUI(NULL)
        output$Oui_GLMbest <- renderUI(NULL)
        output$Oui_DTtrain <- renderUI(NULL)
        output$Oui_DTbest <- renderUI(NULL)
        output$Oplot_DT_visN <- NULL
        output$Oui_RFtrain <- renderUI(NULL)
        output$Oui_RFbest <- renderUI(NULL)
        output$Oui_BTtrain <- renderUI(NULL)
        output$Oui_BTbest <- renderUI(NULL)
        Rglm_result(NULL)
        Rglm_wf(NULL)
        Rfinal_glm_wf(NULL)
        Rfinal_glm(NULL)
        Rfinal_metric_glm(NULL)
        output$OplotGLMtrain <- renderPlotly(NULL)
        output$Oplot_GLMcoeff <- renderPlotly(NULL)
        output$Oplot_GLMpred <- renderPlotly(NULL)
        output$Otbl_finalGLM <- renderTable(NULL)
        Rtree_result(NULL)
        Rtree_wf(NULL)
        Rfinal_tree_wf(NULL)
        Rfinal_tree(NULL)
        Rfinal_metric_tree(NULL)
        output$OplotDTtrain <- renderPlotly(NULL)
        output$Oplot_DTvip <- renderPlot(NULL)
        output$Oplot_DTpred <- renderPlotly(NULL)
        output$Otbl_finalDT <- renderTable(NULL)
        Rrandomf_result(NULL)
        Rrandomf_wf(NULL)
        Rfinal_randomf_wf(NULL)
        Rfinal_randomf(NULL)
        Rfinal_metric_randomf(NULL)
        output$OplotRFtrain <- renderPlotly(NULL)
        output$Oplot_RFvip <- renderPlot(NULL)
        output$Oplot_RFpred <- renderPlotly(NULL)
        output$Otbl_finalRF <- renderTable(NULL)
        Rxgboost_result(NULL)
        Rxgboost_wf(NULL)
        Rfinal_xgboost_wf(NULL)
        Rfinal_xgboost(NULL)
        Rfinal_metric_xgboost(NULL)
        output$OplotBTtrain <- renderPlotly(NULL)
        output$Oplot_BTvip <- renderPlot(NULL)
        output$Oplot_BTpred <- renderPlotly(NULL)
        output$Otbl_finalBT <- renderTable(NULL)
        
        updateNavlistPanel(session,
                           inputId = "navpanel_BT",
                           selected = "Info")
        updateNavlistPanel(session,
                           inputId = "navpanel_RF",
                           selected = "Info")
        updateNavlistPanel(session,
                           inputId = "navpanel_DT",
                           selected = "Info")
        updateNavlistPanel(session,
                           inputId = "navpanel_GLM",
                           selected = "Info")
        updateNavlistPanel(session,
                           inputId = "navpanel_LM",
                           selected = "Info")
      }
    })
    
    ################## LM start ######################
    output$Oui_LM_info <- renderUI({
      fluidPage(
        h3("Linear regression model (LM)"),
        p("A linear approach in modelling the relationship between a response
          variable and one or more explanatory/independent variables.
          The relationships are modeled using linear predictor functions
          whose unknown model parameters are estimated from the data.
          Linear regression focuses on the conditional probability
          distribution of the response given the values of the predictors
          rather than on the joint probability distribution of all of these
          variables, which is the domain of multivariate analysis.
          The following regression model is fitted using the least squares approach
          using", em("lm"), "function.",
          tags$a(href="https://en.wikipedia.org/wiki/Linear_regression",
                 em("Wikipedia")))
      )
    })
    
    output$Ocoeff_est <- NULL
    output$Otraining_fit <- NULL
    output$OtestRsq <- NULL
    output$Opred_error <- NULL
    output$Ovalidation_lm <- NULL
    
    observeEvent(input$btn_trainLM,{
      if(is.null(return_trf$rcp1())){
        shinyalert(text="Please perform data transformation step first",
                   type="error")
      }
      validate(
        need(return_trf$rcp1(),
             'Please go through data transformation process'),
        need(length(return_trf$datasplit())==4,
             'Please go through data transformation process')
      )
      
      updateNavlistPanel(session,
                         inputId = "navpanel_LM",
                         selected = "Training")
      
      rcpTransform1 <- return_trf$rcp1()
      listing_train <- training(return_trf$datasplit())
      
      #Linear regression
      lm_mod <- linear_reg() %>%
        set_engine("lm") %>%
        set_mode("regression")
      
      listing_wflow <-  workflow() %>%
        add_model(lm_mod) %>%
        add_recipe(rcpTransform1)
      
      listing_fit <- listing_wflow %>%
        fit(data = listing_train)
      
      output$Otraining_fit <- renderTable({
        glance(listing_fit)
        })
      
      fitResult <- listing_fit %>%
        pull_workflow_fit() %>%
        tidy()
      
      RfitResult(fitResult)
      Rlisting_fit(listing_fit)
      
      output$Oui_LMtrain <- renderUI({
        fluidPage(
          h4("Model fit result"),
          (div(style='height:140px; overflow-x: scroll',
               tableOutput(NS(id, "Otraining_fit")) %>%
                 withSpinner(color="#FF5A5F"))),
          br(),
          h4("Coefficient estimate result"),
          fluidRow(
            column(6,
                   pickerInput(
                     inputId = NS(id, "Isig_lvl"),
                     label = "p-value:",
                     choices = c("0.01" = "0.01",
                                 "0.05" = "0.05",
                                 "0.1" = "0.1",
                                 "All" = "1"),
                     selected = "1")
            ),
            column(6,
                   pickerInput(
                     inputId = NS(id, "Isort_by"),
                     label = "Sort variables by:",
                     choices = c("p-value", "estimate"),
                     selected = "estimate")
            )
          ),
          plotlyOutput(NS(id, "Ocoeff_est")) %>%
            withSpinner(color="#FF5A5F"),
          br(),
          actionButton(NS(id, "btn_testLM"), "Validate model")
        )
      })
    })
    
    observeEvent(RfitResult(),{
      output$Ocoeff_est <- renderPlotly({
      validate(
        need(RfitResult(), "Please train the model first"))
      #select p-value threshold
      pval <- input$Isig_lvl
  
      #choose sorting by p.value or estimate
      sort_by <- if_else(input$Isort_by == "p-value", "p.value_neg", input$Isort_by)
  
      #get significant features
      sig_lm_predictors <- RfitResult() %>%
        filter(p.value <= pval) %>%
        mutate(p.value_neg = -p.value)
      
        #plot
      lm_p <- sig_lm_predictors %>%
        ggplot(aes(x = estimate, y = reorder(term, !!as.symbol(sort_by)),
                   text = paste0(term, "\nestimate: ", round(estimate,3),
                                 "\n95% CI: ", round(estimate-std.error,2),
                                 "-", round(estimate+std.error,2),
                                 "\np-value: ", round(p.value,5)))) +
        geom_pointrange(aes(xmin = estimate-std.error, xmax = estimate+std.error)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "#484848") +
        theme(axis.title.y = element_blank())
        
      ggplotly(lm_p, tooltip = c('text'))
    })
    })
    
  observeEvent(input$btn_testLM, {
    if(is.null(return_trf$rcp1())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    }
    validate(
      need(return_trf$rcp1(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process')
    )
    updateNavlistPanel(session,
                       inputId = "navpanel_LM",
                       selected = "Validation")
    
    rcpTransform1 <- return_trf$rcp1()
    listing_test <- testing(return_trf$datasplit())
    listing_fit <- Rlisting_fit()
      
    listing_test_T <- rcpTransform1 %>% prep() %>% bake(listing_test)
    
    listing_pred <- predict(listing_fit, new_data = listing_test) %>%
      bind_cols(listing_test_T)
    
    multi_metric <- metric_set(mae, mape, rmse, rsq)
    
    final_metric_lm <- listing_pred %>%
      multi_metric(truth = !!target_var(), estimate = .pred)
    Rfinal_metric_lm(final_metric_lm)
    
    output$Ovalidation_lm <- renderTable({
      final_metric_lm
    })
    
    output$OtestRsq <- renderPlotly({
      pred_p <- ggplot(listing_pred, aes(x = !!as.symbol(target_var()), y = .pred,
                                                text = paste0("Actual: ", !!as.symbol(target_var()),
                                                              "\n", "Predicted: ", round(.pred, 2)))) +
        geom_point(color = "#FF5A5F") +
        geom_abline(color = "#484848") +
        coord_obs_pred() +
        labs(title = "LM Prediction Result",
             x = paste0("Actual ", target_var()),
             y = paste0("Predicted ", target_var()))
      ggplotly(pred_p, tooltip = 'text')
    })
    
    Rlisting_pred(listing_pred)
    
    output$Oui_LMvalidate <- renderUI({
      fluidPage(
        plotlyOutput(NS(id, "OtestRsq")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        h4("Model validation result"),
        tableOutput(NS(id, "Ovalidation_lm")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        
        actionButton(NS(id, "btn_checkError"), "Check prediction error")
      )
    })
  })
  
  observeEvent(input$btn_checkError,{
    if(is.null(return_trf$rcp1())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    }
    validate(
      need(return_trf$rcp1(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(Rlisting_pred(),
           'Please validate the model first')
    )
    updateNavlistPanel(session,
                       inputId = "navpanel_LM",
                       selected = "Prediction error")
    
    output$Opred_error <- renderPlotly({
      pval <- input$Isig_lvl
      
      #get significant features
      sig_lm_predictors <- RfitResult() %>%
        filter(p.value <= pval)
      
      top_n_predictor <- input$Itop_predictor
      topn_val <- input$Itop_error
      n_column <- if_else(top_n_predictor == 3, as.integer(2), as.integer(3))
      
      slp <- sig_lm_predictors %>%
        filter(term != "(Intercept)") %>%
        arrange(p.value) %>%
        top_n(top_n_predictor) %>%
        pull(term)
  
      rcpTransform1 <- return_trf$rcp1()
      listing_train <- training(return_trf$datasplit())
      listing_pred <- Rlisting_pred()
  
      listingTrain_T <- rcpTransform1 %>% prep() %>% juice()
  
      slp_train <- listingTrain_T %>%
        select(all_of(slp), !!as.symbol(target_var())) %>%
        gather()
  
      top_err_id <- listing_pred %>%
        mutate(pred_error = abs(!!as.symbol(target_var())-.pred)) %>%
        arrange(-pred_error) %>%
        top_n(topn_val) %>%
        pull(id)
      
      top_err_tbl <- listing_pred %>%
        filter(id %in% top_err_id) %>%
        mutate(pred_error = abs(!!as.symbol(target_var())-.pred)) %>%
        select(id, pred_error, !!as.symbol(target_var()), all_of(slp))
      
      Rtop_err_tbl(top_err_tbl)
      
      top_err <- listing_pred %>%
        filter(id %in% top_err_id) %>%
        mutate(pred_error = abs(!!as.symbol(target_var())-.pred)) %>%
        select(all_of(slp), id, pred_error, !!as.symbol(target_var())) %>%
        gather(key, value, -id, -pred_error) %>%
        mutate(id = as.factor(id))
  
      color_vec <- rep(c("#FF5A5F"), times = length(unique(top_err$id)))
      
      ggplotly(ggplot(NULL, aes(x = value)) +
                 geom_histogram(data = slp_train, color = "grey70",
                                fill = "grey60", alpha = 0.5) +
                 geom_jitter(data = top_err, aes(y = 0, color = id,
                                                 text = paste0("id: ", id,
                                                               "\n", key, ":", round(value,2),
                                                               "\nerror: ", round(pred_error,2))),
                             alpha = 0.7, width = 0.01, height = 50) +
                 scale_color_manual(values = color_vec) +
                 facet_wrap(~ key, scales = "free_x", ncol = n_column) +
                 theme(panel.spacing.y = unit(1, "lines"),
                       panel.spacing.x = unit(1, "lines")),
               tooltip = c('text'), height = 250*ceiling(top_n_predictor/3))
      })
    
    output$Oerror_tbl <- renderDataTable(
      Rtop_err_tbl(), options = list(scrollX = TRUE,
                                     paging = FALSE, searching = FALSE, info = FALSE)
    )
    
    output$Oui_LMerror <- renderUI({
      pval <- input$Isig_lvl
      sig_lm_predictors <- RfitResult() %>%
        filter(p.value <= pval)
      
      fluidPage(
        circleButton(inputId = NS(id, "IquestionLMerror"), icon = icon("question"),
                     status = "info", size = "xs"),
        fluidRow(
          column(4,
                 sliderInput(NS(id, "Itop_error"), "Select top N prediction error:",
                             value = 10, min = 1, max = 30, step = 1)
          ),
          column(4,
                 sliderInput(NS(id, "Itop_predictor"), "Select top N predictor:",
                             value = 1, min = 1, max = length(sig_lm_predictors$term), step = 1)
          ),
          column(4,
                 pickerInput(
                   inputId = NS(id, "Isig_lvl"),
                   label = "p-value:",
                   choices = c("0.01", "0.05", "0.1"),
                   selected = "0.05")
          )
        ),
        plotlyOutput(NS(id, "Opred_error"), height = 'auto') %>%
          withSpinner(color="#FF5A5F"),
        dataTableOutput(NS(id, "Oerror_tbl"))
      )
    })
    
  })
  ###################### LM end ######################
  
  ###################### Cross validation start ######################
  output$Oui_CV_info <- renderUI({
    fluidPage(
      h3("K-fold cross validation"),
      p("Cross-validation is a resampling procedure used to 
      evaluate machine learning models on a limited data sample.
      The procedure has a single parameter called k that refers to the 
      number of groups that a given data sample is to be split into.
      It is a popular method because it is simple to understand and because
      it generally results in a less biased or less optimistic estimate of the
      model skill than other methods, such as a simple train/test split.",
      tags$a(href="https://machinelearningmastery.com/k-fold-cross-validation/",
             em("Source"))),
      p("The commonly used k value is 5/10 fold.
      For demo purpose, we can use 3-fold to speed up the process.")
      )
  })
  
  observeEvent(input$btn_crossval,{
    if((length(return_trf$datasplit())!=4) | (is.null(return_trf$Rformula()))){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    }
    validate(
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(return_trf$Rformula(),
           'Please go through data transformation process')
    )
    
    set.seed(1234)
    kfold <- as.numeric(input$Ikfold)
    listing_train <- training(return_trf$datasplit())
    f <-  return_trf$Rformula()
    
    rcpKFold <- recipe(f, data = listing_train) %>%
      step_naomit(all_predictors(), -all_nominal(), skip = TRUE) #remove rows with NA
    
    listing_kfolds <- rcpKFold %>%
      prep() %>%
      juice() %>%
      vfold_cv(v = kfold, strata = target_var())
    Rlisting_kfolds(listing_kfolds)
    shinyalert(text="CV training set created! \n Proceed to the next step",
               type="success")
  })
  
  ###################### Cross validation end ######################
  
  ############## GLM start ##################
  
    output$Oui_GLM_info <- renderUI({
    fluidPage(
      h3("Generalised linear model (GLM)"),
      p("A flexible generalization of ordinary linear regression that allows for
      response variables that have error distribution models other than a normal
      distribution. The GLM generalizes linear regression by allowing the linear
      model to be related to the response variable via a link function and by
      allowing the magnitude of the variance of each measurement to be a function
      of its predicted value.",
        tags$a(href="https://en.wikipedia.org/wiki/Generalized_linear_model#General_linear_models",
               em("Wikipedia"))),
      p("In this model, we use glmnet as the engine that fits generalized linear
        regression model. Two hyperparameters are being tuned: ", em("penalty"),
        "which refers to the amount of regularisation or ", em("lambda"),
        "in regularised linear model, and", em("mixture"),
        ", which is the amount of different types of regularisation where 0
        refers to ridge regression and 1 refers to lasso regression.",
        tags$a(href="https://parsnip.tidymodels.org/reference/linear_reg.html",
               em("Tidymodels")))
    )
  })
  
  Rglm_result <- reactiveVal(NULL)
  Rglm_wf <- reactiveVal(NULL)
  Rfinal_glm_wf <- reactiveVal(NULL)
  Rfinal_glm <- reactiveVal(NULL)
  Rfinal_metric_glm <- reactiveVal(NULL)
  output$OplotGLMtrain <- renderPlotly(NULL)
  output$Oplot_GLMcoeff <- renderPlotly(NULL)
  output$Oplot_GLMpred <- renderPlotly(NULL)
  
  observeEvent(input$btn_trainGLM, {
    if(is.null(return_trf$rcp1())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    } else if(is.null(Rlisting_kfolds())){
      shinyalert(text="Please prepare cross validation data set first",
                 type="error")
    }
    validate(
      need(return_trf$rcp1(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(Rlisting_kfolds(),
           'Please go through CV process')
    )
    
    shinyalert(text="GLM training in progress, please wait...",
               type="info", showConfirmButton = FALSE, closeOnEsc = FALSE)
    rcpTransform1 <- return_trf$rcp1()
    set.seed(1234)
    glmnet_model <- linear_reg(mode = "regression",
                               penalty = tune(),
                               mixture = tune()) %>%
      set_engine("glmnet")
    glmnet_params <- parameters(penalty(), mixture())
    glmnet_grid <- grid_max_entropy(glmnet_params, size = 20)
    glm_wf <- workflow() %>%
      add_model(glmnet_model) %>%
      add_recipe(rcpTransform1)
    Rglm_wf(glm_wf)
    glm_result <- glm_wf %>%
      tune_grid(resamples = Rlisting_kfolds(),
                grid = glmnet_grid,
                metrics = metric_set(mae, mape, rmse, rsq))
    Rglm_result(glm_result)
    closeAlert()
    
    updateNavlistPanel(session,
                       inputId = "navpanel_GLM",
                       selected = "Training")
    
    output$OplotGLMtrain <- renderPlotly({
      glm_p <- Rglm_result() %>%
        collect_metrics() %>%
        ggplot(aes(penalty, mean)) +
        # geom_errorbar(aes(ymin = mean - std_err,
        #                   ymax = mean + std_err),
        #               alpha = 0.5) +
        geom_point(color = "#FF5A5F") +
        geom_line(color = "#484848") +
        facet_wrap(~.metric, scales = "free", nrow = 2) +
        scale_x_log10() +
        theme(legend.position = "none")
      ggplotly(glm_p)
    })

    output$Otbl_GLMresult <- renderDataTable({
      Rglm_result() %>%
        show_best(metric = input$ImetricGLM)
    }, options = list(paging = FALSE, searching = FALSE, info = FALSE))
    
    output$Oui_GLMtrain <- renderUI({
      fluidPage(
        h4("Hyper-parameter tuning result"),
        plotlyOutput(NS(id, "OplotGLMtrain"), width = 'auto') %>%
          withSpinner(color="#FF5A5F"),
        br(),
        fluidRow(
          column(6,
                 pickerInput(
                   inputId = NS(id, "ImetricGLM"),
                   label = "Choose metric to select best model:",
                   choices = c("rmse"= "rmse", "mae"= "mae",
                               "mape"= "mape", "rsquare"= "rsq"),
                   selected = "rmse")
                 ),
          column(6,
                 br(),
                 actionButton(NS(id, "btn_GLMbest"), "Choose best model")
                 )
        ),
        dataTableOutput(NS(id, "Otbl_GLMresult")),
        br(),
        uiOutput(NS(id, "Oui_GLMbest"))
      )
    })
    
    output$Oui_GLMbest <- renderUI(NULL)  #reset UI
  })
  
  observeEvent(input$btn_GLMbest,{
    validate(
      need(Rglm_result(),
           'Please go through GLM training'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process')
    )
    
    listing_train <- training(return_trf$datasplit())
    
    best_glm_model <- Rglm_result() %>%
      select_best(metric = input$ImetricGLM)
    final_glm_wf <- Rglm_wf() %>%
      finalize_workflow(best_glm_model)
    Rfinal_glm_wf(final_glm_wf)
    final_glm <- final_glm_wf %>%
      fit(data = listing_train)
    Rfinal_glm(final_glm)
    glm_p <- final_glm %>%
      pull_workflow_fit() %>%
      tidy() %>%
      filter(estimate!=0) %>%
      arrange(estimate) %>%
      ggplot(aes(x = estimate, y = reorder(term, estimate),
                 text = paste0(term, "\nestimate: ", round(estimate,3)))) +
      geom_point(color = "#FF5A5F") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "#484848") +
      theme(axis.title.y = element_blank()) +
      ggtitle("Coefficient estimate for GLM model")
    
    output$Oplot_GLMcoeff <- renderPlotly({
      ggplotly(glm_p, tooltip = c('text'))
    })
    
    output$Oui_GLMbest <- renderUI({
      fluidPage(
        plotlyOutput(NS(id, "Oplot_GLMcoeff")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        actionButton(NS(id, "btn_testGLM"), "Validate model")
      )
    })
  })
  
  observeEvent(input$btn_testGLM, {
    updateNavlistPanel(session,
                       inputId = "navpanel_GLM",
                       selected = "Validation")
    
    final_fit_glm <- last_fit(Rfinal_glm_wf(),
                              split = return_trf$datasplit(),
                              metrics = metric_set(mae, mape, rmse, rsq))
    
    output$Oplot_GLMpred <- renderPlotly({
      pred_glm_p <- final_fit_glm %>%
      collect_predictions() %>%
      ggplot(aes(x=!!as.symbol(target_var()), y=.pred)) +
      geom_point(color = "#FF5A5F") +
      geom_abline(color = "#484848") +
      coord_obs_pred() +
      labs(title = "GLM Prediction Result",
           x = paste0("Actual ", target_var()),
           y = paste0("Predicted ", target_var()))
      ggplotly(pred_glm_p)  
    })
    
    final_metric_glm <- collect_metrics(final_fit_glm)
    Rfinal_metric_glm(final_metric_glm)
    
    output$Otbl_finalGLM <- renderTable({
      final_metric_glm %>%
        select(-as.symbol(".config"))
      })
    
  })
  ############## GLM end ##################
  
  ############## DT start ##################
  
  output$Oui_DT_info <- renderUI({
    fluidPage(
      h3("Decision tree (DT) model"),
      p("This model uses a decision tree (as a predictive model) to go from
        observations about a variable (represented in the branches) to conclusions
        about the target variable (represented in the leaves). Tree models where
        the target variable can take a discrete set of values are called
        classification trees, while those with continuous target variable are
        called regression trees. Decision trees are among the most popular machine
        learning algorithms given their intelligibility and simplicity.",
        tags$a(href="https://en.wikipedia.org/wiki/Decision_tree_learning",
               em("Wikipedia"))),
      p("In this model, we are using", em("rpart"), "package with ",
        em("cost_complexity, tree_depth (maximum tree depth), and min_n
        (minimum number of data points in a node that are required for the 
        node to be split further)"), "as hyper-parameters to be tuned.",
        em("visTree"), "function from ", em("visNetwork"), "package is used for
        interactive decision tree visualisation.",
        tags$a(href="https://parsnip.tidymodels.org/reference/decision_tree.html",
               em("Tidymodels")))
    )
  })
  
  Rtree_result <- reactiveVal(NULL)
  Rtree_wf <- reactiveVal(NULL)
  Rfinal_tree_wf <- reactiveVal(NULL)
  Rfinal_tree <- reactiveVal(NULL)
  Rfinal_metric_tree <- reactiveVal(NULL)
  output$OplotDTtrain <- renderPlotly(NULL)
  output$Oplot_DTvip <- renderPlot(NULL)
  output$Oplot_DTpred <- renderPlotly(NULL)
  
  observeEvent(input$btn_trainDT, {
    if(is.null(return_trf$rcp2())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    } else if(is.null(Rlisting_kfolds())){
      shinyalert(text="Please prepare cross validation data set first",
                 type="error")
    }
    validate(
      need(return_trf$rcp2(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(Rlisting_kfolds(),
           'Please go through CV process')
    )
    
    shinyalert(text="DT training in progress, please wait...",
               type="info", showConfirmButton = FALSE, closeOnEsc = FALSE)
    rcpTransform2 <- return_trf$rcp2()
  
    set.seed(1234)
    tree_model <- decision_tree(cost_complexity = tune(),
                                tree_depth = tune(),
                                min_n = tune()) %>%
      set_engine("rpart") %>%
      set_mode("regression")
    
    tree_grid <- grid_regular(cost_complexity(),
                              tree_depth(range = c(3,7)),
                              min_n(),
                              levels = 4)
    
    tree_wf <- workflow() %>%
      add_model(tree_model) %>%
      add_recipe(rcpTransform2)
    Rtree_wf(tree_wf)
    tree_result <- tree_wf %>%
      tune_grid(resamples = Rlisting_kfolds(),
                grid = tree_grid,
                metrics = metric_set(mae, mape, rmse, rsq),
                control = control_resamples(save_pred = TRUE))
    Rtree_result(tree_result)
    closeAlert()
    
    updateNavlistPanel(session,
                       inputId = "navpanel_DT",
                       selected = "Training")
    
    output$OplotDTtrain <- renderPlotly({
      tree_p <- Rtree_result() %>%
        collect_metrics() %>%
        mutate(tree_depth = as.factor(tree_depth)) %>%
        ggplot(aes(cost_complexity, mean, color = tree_depth)) +
        geom_point() +
        geom_line() +
        # geom_errorbar(aes(ymin = mean - std_err,
        #                   ymax = mean + std_err),
        #               alpha = 0.5) +
        facet_grid(.metric ~ min_n, scales = "free",
                   labeller = labeller(min_n = label_both)) +
        scale_x_log10()
      ggplotly(tree_p) %>%
        layout(legend = list(orientation = "v", x = 1.1, y = 1))
    })
    
    output$Otbl_DTresult <- renderDataTable({
      Rtree_result() %>%
        show_best(metric = input$ImetricDT)
    }, options = list(paging = FALSE, searching = FALSE, info = FALSE))
    
    output$Oui_DTtrain <- renderUI({
      fluidPage(
        h4("Hyper-parameter tuning result"),
        plotlyOutput(NS(id, "OplotDTtrain")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        fluidRow(
          column(6,
                 pickerInput(
                   inputId = NS(id, "ImetricDT"),
                   label = "Choose metric to select best model:",
                   choices = c("rmse"= "rmse", "mae"= "mae",
                               "mape"= "mape", "rsquare"= "rsq"),
                   selected = "rmse")
          ),
          column(6,
                 br(),
                 actionButton(NS(id, "btn_DTbest"), "Choose best model")
          )
        ),
        dataTableOutput(NS(id, "Otbl_DTresult")),
        br(), 
        uiOutput(NS(id, "Oui_DTbest"))
      )
    })
    
    output$Oui_DTbest <- renderUI(NULL) #reset UI
  })
  
  observeEvent(input$btn_DTbest,{
    validate(
      need(Rtree_result(),
           'Please go through DT training'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process')
    )
    
    listing_train <- training(return_trf$datasplit())
    
    best_tree_model <- Rtree_result() %>%
      select_best(metric = input$ImetricDT)
    final_tree_wf <- Rtree_wf() %>%
      finalize_workflow(best_tree_model)
    Rfinal_tree_wf(final_tree_wf)
    final_tree <- final_tree_wf %>%
      fit(data = listing_train)
    Rfinal_tree(final_tree)
    
    output$Oplot_DTvip <- renderPlot({
      final_tree %>%
      pull_workflow_fit() %>%
      vip()
    })
    
    final_tree_fit <- final_tree %>%
      pull_workflow_fit()
    
    output$Oui_DTbest <- renderUI({
      fluidPage(
        h4("Variable importance"),
        plotOutput(NS(id, "Oplot_DTvip")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        h4("Decision Tree visualisation"),
        visNetworkOutput(NS(id, "Oplot_DT_visN")),
        br(),
        actionButton(NS(id, "btn_testDT"), "Validate model")
      )
    })
    
    output$Oplot_DT_visN <- renderVisNetwork({
      visNetwork::visTree(final_tree_fit$fit)
    })
    
  })
  
  observeEvent(input$btn_testDT, {
    updateNavlistPanel(session,
                       inputId = "navpanel_DT",
                       selected = "Validation")
    
    final_fit_tree <- last_fit(Rfinal_tree_wf(),
                              split = return_trf$datasplit(),
                              metrics = metric_set(mae, mape, rmse, rsq))
    
    pred_dt_p <- final_fit_tree %>%
      collect_predictions() %>%
      ggplot(aes(x=!!as.symbol(target_var()), y=.pred)) +
      geom_point(color = "#FF5A5F") +
      geom_abline(color = "#484848") +
      coord_obs_pred() +
      labs(title = "DT Prediction Result",
           x = paste0("Actual ", target_var()),
           y = paste0("Predicted ", target_var()))
    
    output$Oplot_DTpred <- renderPlotly({
      ggplotly(pred_dt_p)  
    })
    
    final_metric_tree <- collect_metrics(final_fit_tree)
    Rfinal_metric_tree(final_metric_tree)
    
    output$Otbl_finalDT <- renderTable({
      final_metric_tree %>%
        select(-as.symbol(".config"))
      })
  })
  
  ############## DT end ##################
  
  ############## RF start ##################
  
  output$Oui_RF_info <- renderUI({
    fluidPage(
      h3("Random forest (RF) model"),
      p("An ensemble learning method for classification, regression and other
        tasks that operates by constructing a multitude of decision trees at 
        training time and outputting the class that is the mode of the classes
        (classification) or mean/average prediction (regression) of the 
        individual trees. Random decision forests correct for decision trees'
        habit of overfitting to their training set. Random forests generally
        outperform decision trees, but their accuracy is lower than gradient
        boosted trees.",
        tags$a(href="https://en.wikipedia.org/wiki/Random_forest",
               em("Wikipedia"))),
      p("In this model, we are using", em("ranger"), "package with ",
        em("mtry (number of variables randomly sampled as candidates at each split)
        and min_n (minimum number of data points in a node that are required for the 
        node to be split further)"), "as hyper-parameters to be tuned, while
        fixing the number of trees at 200 to speed up the process.",
        tags$a(href="https://parsnip.tidymodels.org/reference/rand_forest.html",
               em("Tidymodels")))
    )
  })
  
  Rrandomf_result <- reactiveVal(NULL)
  Rrandomf_wf <- reactiveVal(NULL)
  Rfinal_randomf_wf <- reactiveVal(NULL)
  Rfinal_randomf <- reactiveVal(NULL)
  Rfinal_metric_randomf <- reactiveVal(NULL)
  output$OplotRFtrain <- renderPlotly(NULL)
  output$Oplot_RFvip <- renderPlot(NULL)
  output$Oplot_RFpred <- renderPlotly(NULL)
  
  observeEvent(input$btn_trainRF, {
    if(is.null(return_trf$rcp2())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    } else if(is.null(Rlisting_kfolds())){
      shinyalert(text="Please prepare cross validation data set first",
                 type="error")
    }
    validate(
      need(return_trf$rcp2(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(Rlisting_kfolds(),
           'Please go through CV process')
    )
    
    shinyalert(text="RF training in progress, please wait...",
               type="info", showConfirmButton = FALSE, closeOnEsc = FALSE)
    rcpTransform2 <- return_trf$rcp2()
    
    set.seed(1234)
    randomf_model <-rand_forest(trees = 200,
                                mtry = tune(),
                                min_n = tune()) %>%
      set_engine("ranger",
                 importance = "permutation") %>%
      set_mode("regression")
    
    randomf_wf <- workflow() %>%
      add_model(randomf_model) %>%
      add_recipe(rcpTransform2)
    Rrandomf_wf(randomf_wf)
    randomf_grid <- grid_regular(mtry(range = c(10, 20)),
                                 min_n(),
                                 levels = 3)
    randomf_result <- randomf_wf %>%
      tune_grid(resamples = Rlisting_kfolds(),
                grid = randomf_grid,
                control = control_resamples(save_pred = TRUE),
                metrics = metric_set(mae, mape, rmse, rsq))
    Rrandomf_result(randomf_result)
    closeAlert()
    
    updateNavlistPanel(session,
                       inputId = "navpanel_RF",
                       selected = "Training")
    
    output$OplotRFtrain <- renderPlotly({
      randomf_p <- Rrandomf_result() %>%
        collect_metrics() %>%
        mutate(min_n = as.factor(min_n)) %>%
        ggplot(aes(mtry, mean, color = min_n)) +
        geom_point() +
        geom_line() +
        # geom_errorbar(aes(ymin = mean - std_err,
        #                   ymax = mean + std_err),
        #               alpha = 0.5) +
        facet_wrap(~.metric, scales = "free", nrow = 2) +
        # facet_grid(.metric ~ min_n, scales = "free", labeller=label_both) +
        scale_x_log10()
      ggplotly(randomf_p) %>%
        layout(legend = list(orientation = "v", x = 1.1, y = 1))
    })
    
    output$Otbl_RFresult <- renderDataTable({
      Rrandomf_result() %>%
        show_best(metric = input$ImetricRF)
    }, options = list(paging = FALSE, searching = FALSE, info = FALSE))
    
    output$Oui_RFtrain <- renderUI({
      fluidPage(
        h4("Hyper-parameter tuning result"),
        plotlyOutput(NS(id, "OplotRFtrain")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        fluidRow(
          column(6,
                 pickerInput(
                   inputId = NS(id, "ImetricRF"),
                   label = "Choose metric to select best model:",
                   choices = c("rmse"= "rmse", "mae"= "mae",
                               "mape"= "mape", "rsquare"= "rsq"),
                   selected = "rmse")
                 ),
          column(6,
                 br(),
                 actionButton(NS(id, "btn_RFbest"), "Choose best model"),
                 )
        ),
        dataTableOutput(NS(id, "Otbl_RFresult")),
        br(),
        uiOutput(NS(id, "Oui_RFbest"))
      )
    })
    
    output$Oui_RFbest <- renderUI(NULL) #reset UI
  })
  
  observeEvent(input$btn_RFbest,{
    validate(
      need(Rrandomf_result(),
           'Please go through RF training'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process')
    )
    
    listing_train <- training(return_trf$datasplit())
    
    best_randomf_model <- Rrandomf_result() %>%
      select_best(metric = input$ImetricRF)
    final_randomf_wf <- Rrandomf_wf() %>%
      finalize_workflow(best_randomf_model)
    Rfinal_randomf_wf(final_randomf_wf)
    final_randomf <- final_randomf_wf %>%
      fit(data = listing_train)
    Rfinal_randomf(final_randomf)
    
    output$Oplot_RFvip <- renderPlot({
      final_randomf %>%
        pull_workflow_fit() %>%
        vip()
    })
    
    final_randomf_fit <- final_randomf %>%
      pull_workflow_fit()
    
    output$Oui_RFbest <- renderUI({
      fluidPage(
        h4("Variable importance"),
        plotOutput(NS(id, "Oplot_RFvip")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        actionButton(NS(id, "btn_testRF"), "Validate model")
      )
    })
    
    # output$validate_btn_RF <- renderUI({
    #   actionButton(NS(id, "btn_testRF"), "Validate model")
    # })
  })
  
  observeEvent(input$btn_testRF, {
    updateNavlistPanel(session,
                       inputId = "navpanel_RF",
                       selected = "Validation")
    
    final_fit_randomf <- last_fit(Rfinal_randomf_wf(),
                               split = return_trf$datasplit(),
                               metrics = metric_set(mae, mape, rmse, rsq))
    
    pred_randomf_p <- final_fit_randomf %>%
      collect_predictions() %>%
      ggplot(aes(x=!!as.symbol(target_var()), y=.pred)) +
      geom_point(color = "#FF5A5F") +
      geom_abline(color = "#484848") +
      coord_obs_pred() +
      labs(title = "RF Prediction Result",
           x = paste0("Actual ", target_var()),
           y = paste0("Predicted ", target_var()))
    
    output$Oplot_RFpred <- renderPlotly({
      ggplotly(pred_randomf_p)  
    })
    
    final_metric_randomf <- collect_metrics(final_fit_randomf)
    Rfinal_metric_randomf(final_metric_randomf)
    
    output$Otbl_finalRF <- renderTable({
      final_metric_randomf %>%
        select(-as.symbol(".config"))
      })
  })
  
  ############## RF end ##################
  
  ############## BT start ##################
  output$Oui_BT_info <- renderUI({
    fluidPage(
      h3("Boosted tree (BT) model"),
      p("Similar model to random forest which uses decision trees ensembles.
        However, the difference arises from how we train them. Models are fit
        using any arbitrary differentiable loss function and gradient descent
        optimization algorithm.",
        tags$a(href="https://machinelearningmastery.com/xgboost-for-regression",
               em("Source"))),
      p("In this model, we are using", em("xgboost"), "package with ",
        em("min_n (minimum number of data points in a node that
        are required for the node to be split further), tree depth, and
        learning rate"), "as hyper-parameters to be tuned, while
        fixing the number of trees at 200 to speed up the process.",
        tags$a(href="https://parsnip.tidymodels.org/reference/boost_tree.html",
               em("Tidymodels")))
    )
  })
  
  Rxgboost_result <- reactiveVal(NULL)
  Rxgboost_wf <- reactiveVal(NULL)
  Rfinal_xgboost_wf <- reactiveVal(NULL)
  Rfinal_xgboost <- reactiveVal(NULL)
  Rfinal_metric_xgboost <- reactiveVal(NULL)
  output$OplotBTtrain <- renderPlotly(NULL)
  output$Oplot_BTvip <- renderPlot(NULL)
  output$Oplot_BTpred <- renderPlotly(NULL)
  
  observeEvent(input$btn_trainBT, {
    if(is.null(return_trf$rcp2())){
      shinyalert(text="Please perform data transformation step first",
                 type="error")
    } else if(is.null(Rlisting_kfolds())){
      shinyalert(text="Please prepare cross validation data set first",
                 type="error")
    }
    validate(
      need(return_trf$rcp2(),
           'Please go through data transformation process'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process'),
      need(Rlisting_kfolds(),
           'Please go through CV process')
    )
    
    shinyalert(text="BT training in progress, please wait...",
               type="info", showConfirmButton = FALSE, closeOnEsc = FALSE)
    rcpTransform1 <- return_trf$rcp1()
    
    set.seed(1234)
    xgboost_model <- boost_tree(mode = "regression",
                                trees = 200,
                                min_n = tune(),
                                tree_depth = tune(),
                                learn_rate = tune()) %>%
      set_engine("xgboost", objective = "reg:squarederror")
    
    xgboost_wf <- workflow() %>%
      add_model(xgboost_model) %>%
      add_recipe(rcpTransform1)
    Rxgboost_wf(xgboost_wf)
    xgboost_params <- parameters(min_n(), tree_depth(), learn_rate())
    xgboost_grid <- grid_max_entropy(xgboost_params, size = 20)
    xgboost_result <- xgboost_wf %>%
      tune_grid(resamples = Rlisting_kfolds(),
                grid = xgboost_grid,
                metrics = metric_set(mae, mape, rmse, rsq))
    
    Rxgboost_result(xgboost_result)
    closeAlert()
    
    updateNavlistPanel(session,
                       inputId = "navpanel_BT",
                       selected = "Training")
    
    output$OplotBTtrain <- renderPlotly({
      xgboost_p <- Rxgboost_result() %>%
        collect_metrics() %>%
        mutate(tree_depth = as.factor(tree_depth)) %>%
        ggplot(aes(learn_rate, mean, color = tree_depth)) +
        geom_point() +
        geom_line() +
        # geom_errorbar(aes(ymin = mean - std_err,
        #                   ymax = mean + std_err),
        #               alpha = 0.5) +
        facet_wrap(~ .metric, scales = "free", nrow = 2) +
        scale_x_log10()
      ggplotly(xgboost_p) %>%
        layout(legend = list(orientation = "v", x = 1.1, y = 1))
    })
    
    output$Otbl_BTresult <- renderDataTable({
      Rxgboost_result() %>%
        show_best(metric = input$ImetricBT)
    }, options = list(paging = FALSE, searching = FALSE, info = FALSE))
    
    output$Oui_BTtrain <- renderUI({
      fluidPage(
        h4("Hyper-parameter tuning result"),
        plotlyOutput(NS(id, "OplotBTtrain")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        fluidRow(
          column(6,
                 pickerInput(
                   inputId = NS(id, "ImetricBT"),
                   label = "Choose metric to select best model:",
                   choices = c("rmse"= "rmse", "mae"= "mae",
                               "mape"= "mape", "rsquare"= "rsq"),
                   selected = "rmse")
                 ),
          column(6,
                 br(),
                 actionButton(NS(id, "btn_BTbest"), "Choose best model"),
                 )
        ),
        dataTableOutput(NS(id, "Otbl_BTresult")),
        br(),
        uiOutput(NS(id, "Oui_BTbest"))
      )
    })
    
    output$Oui_BTbest <- renderUI(NULL) # reset UI
  })
  
  observeEvent(input$btn_BTbest,{
    validate(
      need(Rxgboost_result(),
           'Please go through BT training'),
      need(length(return_trf$datasplit())==4,
           'Please go through data transformation process')
    )
    
    listing_train <- training(return_trf$datasplit())
    
    best_xgboost_model <- Rxgboost_result() %>%
      select_best(metric = input$ImetricBT)
    final_xgboost_wf <- Rxgboost_wf() %>%
      finalize_workflow(best_xgboost_model)
    Rfinal_xgboost_wf(final_xgboost_wf)
    final_xgboost <- final_xgboost_wf %>%
      fit(data = listing_train)
    Rfinal_xgboost(final_xgboost)
    
    output$Oplot_BTvip <- renderPlot({
      final_xgboost %>%
        pull_workflow_fit() %>%
        vip()
    })
    
    final_xgboost_fit <- final_xgboost %>%
      pull_workflow_fit()
    
    output$Oui_BTbest <- renderUI({
      fluidPage(
        h4("Variable importance"),
        plotOutput(NS(id, "Oplot_BTvip")) %>%
          withSpinner(color="#FF5A5F"),
        br(),
        actionButton(NS(id, "btn_testBT"), "Validate model")
      )
    })
    
    # output$validate_btn_BT <- renderUI({
    #   actionButton(NS(id, "btn_testBT"), "Validate model")
    # })
  })
  
  observeEvent(input$btn_testBT, {
    updateNavlistPanel(session,
                       inputId = "navpanel_BT",
                       selected = "Validation")
    
    final_fit_xgboost <- last_fit(Rfinal_xgboost_wf(),
                                  split = return_trf$datasplit(),
                                  metrics = metric_set(mae, mape, rmse, rsq))
    
    pred_xgboost_p <- final_fit_xgboost %>%
      collect_predictions() %>%
      ggplot(aes(x=!!as.symbol(target_var()), y=.pred)) +
      geom_point(color = "#FF5A5F") +
      geom_abline(color = "#484848") +
      coord_obs_pred() +
      labs(title = "BT Prediction Result",
           x = paste0("Actual ", target_var()),
           y = paste0("Predicted ", target_var()))
    
    output$Oplot_BTpred <- renderPlotly({
      ggplotly(pred_xgboost_p)  
    })
    
    final_metric_xgboost <- collect_metrics(final_fit_xgboost)
    Rfinal_metric_xgboost(final_metric_xgboost)
    
    output$Otbl_finalBT <- renderTable({
      final_metric_xgboost %>%
        select(-as.symbol(".config"))
      })
  })
  
  ############## BT end ##################
  
  observeEvent(input$IquestionLMerror,{
    text_item <- paste0("This section provides tool to explore cases where ",
                        "prediction error are high. Select how many cases and ", 
                        "how many top predictors satisfying the p-value threshold ",
                        "using the slider and dropdown menu. Each case is assigned ",
                        "a unique id which can be singled out by double clicking the ",
                        "id from the legend.")
    shinyalert(text=text_item,
               type="info")
  })
  
  return(list(
    fm_lm = Rfinal_metric_lm,
    fit_lm = Rlisting_fit,
    
    fm_glm = Rfinal_metric_glm,
    glm_result = Rglm_result,
    glm_wf = Rglm_wf,
    final_glm = Rfinal_glm,
    
    fm_tree = Rfinal_metric_tree,
    tree_result = Rtree_result,
    tree_wf = Rtree_wf,
    final_tree = Rfinal_tree,
    
    fm_randomf = Rfinal_metric_randomf,
    randomf_result = Rrandomf_result,
    randomf_wf = Rrandomf_wf,
    final_randomf = Rfinal_randomf,
    
    fm_xgb = Rfinal_metric_xgboost,
    xgb_result = Rxgboost_result,
    xgb_wf = Rxgboost_wf,
    final_xgb = Rfinal_xgboost,
    datasplit = return_trf$datasplit
  ))
  })
}
