######################################## SERVER STARTS HERE #########################################
server <- function(input, output) {
  ##########################
  ## Code for DATA PANEL  ##
  ##########################
  output$data <- DT::renderDT(Arrhythmia)
  output$summary <- renderPrint(summary(Arrhythmia))
  output$str <- renderPrint(report(Arrhythmia))
  
  ############################
  ## Code for VISUAL PANEL  ##
  ############################
  x <- reactive(input$var1)
  y <- reactive(input$var2)
  dist <- reactive(input$dist)
  output$plot <- renderPlotly({
    if (is.factor(Arr[, x()]) & is.factor(Arr[, y()])) {
      g <- ggplot(data = Arr, aes(x = Arr[, x()], fill = Arr[, y()])) +
        geom_bar(position = "fill") +
        ylab("Proportion") + xlab(x()) + labs(fill = y()) +
        ggtitle(paste("Barplot of", x(), "vs", y())) +
        theme_classic()
    } else if (is.factor(Arr[, x()])) {
      g <-
        ggplot(data = Arr, aes(x = Arr[, x()],
                               y = Arr[, y()])) +
        geom_boxplot() + xlab(x()) + ylab(y()) +
        ggtitle(paste("Boxplot of", x(), "vs", y())) +
        theme_classic()
    } else if (is.factor(Arr[, y()])) {
      g <-
        ggplot(data = Arr, aes(
          x = Arr[, x()],
          y = Arr[, y()],
          fill = Arr[, y()]
        )) +
        geom_boxplot() + xlab(x()) + ylab(y()) + labs(fill = y()) +
        ggtitle(paste("Boxplot of", x(), "vs", y()))
    } else {
      g <- ggplot(data = Arr, aes(x = Arr[, x()], y = Arr[, y()])) +
        geom_point() + geom_smooth(method = "lm") + xlab(x()) + ylab(y()) +
        ggtitle(paste("Scatter plot of", x(), "vs", y())) +
        theme_classic()
    }
    ggplotly(g)
  })
  output$corrplot <- renderPlot({
    forhtmap <- as.matrix(Arrhythmia2)
    corr <- round(cor(Arrhythmia2), 2)
    ggcorrplot(
      corr,
      hc.order = TRUE,
      type = "lower",
      lab = TRUE,
      title = "Correlation Heatmap For Continuous Variables"
    )
  })
  output$hist <- renderPlotly({
    if (is.factor(Arr[, dist()])) {
      ggplot(data = Arr, aes(x = Arr[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist())) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = Arr, aes(x = Arr[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist())) +
        theme_classic()
    }
  })
  output$hist_normal <- renderPlotly({
    if (is.factor(Arr_normal[, dist()])) {
      ggplot(data = Arr_normal, aes(x = Arr_normal[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Normal group")) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = Arr_normal, aes(x = Arr_normal[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Normal group")) +
        theme_classic()
    }
  })
  output$hist_arr <- renderPlotly({
    if (is.factor(Arr_arr[, dist()])) {
      ggplot(data = Arr_arr, aes(x = Arr_arr[, dist()])) +
        geom_histogram(
          stat = "count",
          colour = "black",
          fill = "white",
          size = 1
        ) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Arrhythmia group")) +
        theme_classic() + theme(axis.text.x = element_text(
          angle = 20,
          vjust = 0.5,
          hjust = 1
        ))
    } else {
      ggplot(data = Arr_arr, aes(x = Arr_arr[, dist()])) +
        geom_histogram(
          aes(y = ..density..),
          colour = 1,
          fill = "white",
          size = 1
        ) +
        geom_density(color = 2) +
        ylab("Frequency") + xlab(dist()) +
        ggtitle(paste("Distribution of", dist(), "for Arrhythmia group")) +
        theme_classic()
    }
  })
  ###########################
  ## Code for MODEL PANEL  ##
  ###########################
  formula <- reactive({
    req(input$mvars)
    if (input$interaction) {
      paste("label ~ ", paste0("(", paste(input$mvars, collapse = " + "), ")^2"))
    } else {
      paste("label ~ ", paste(input$mvars, collapse = " + "))
    }
  })
  model_text <- reactive({
    req(input$mvars)
    paste("Selected Model:")
  })
  output$model_text <- renderText(model_text())
  model <- reactive({
    glm(formula(), family = binomial, data = Arrhythmia)
  })
  output$model <- renderPrint({
    req(input$mvars)
    summary(model())
  })
  model1 <- reactive({
    input$mod1
    isolate(model())
  })
  model2 <- reactive({
    input$mod2
    isolate(model())
  })
  output$model1 <- renderText({
    input$mod1
    isolate({
      formula()
    })
  })
  output$model2 <- renderText({
    input$mod2
    isolate({
      formula()
    })
  })
  output$anova <- renderPrint({
    anova(model1(), model2(), test = "LRT")
  })
  vif_text <- reactive({
    req(input$vif)
    paste("VIF values:")
  })
  output$vif_text <- renderText(vif_text())
  output$vif <- renderPrint({
    if (input$vif) {
      car::vif(model())
    }
  })
  stepmodel_text <- reactive({
    req(input$stepgo)
    input$stepgo
    isolate(paste(input$step, "Stepwise Model Selection:"))
  })
  stepmodel <- reactive({
    req(input$stepgo)
    input$stepgo
    isolate({
      nullmodel <- glm(label ~ 1, family = binomial, data = Arrhythmia)
      complexmodel <- as.formula(model())
      if (input$step == "Forward") {
        step(
          nullmodel,
          direction = "forward",
          scope = complexmodel,
          trace = FALSE
        )
      } else if (input$step == "Backward") {
        step(object = model(),
             direction = "backward",
             trace = FALSE)
      } else {
        step(
          nullmodel,
          direction = "both",
          scope = complexmodel,
          trace = FALSE
        )
      }
    })
  })
  output$step_text <- renderText(stepmodel_text())
  output$step <- renderPrint({
    req(input$stepgo)
    summary(stepmodel())
  })
  contingency_text <- reactive({
    req(input$contingency)
    paste("Contingency Table:")
  })
  output$contingency_text <- renderText(contingency_text())
  contab <- reactive({
    req(input$contingency)
    predicted <-
      ifelse(fitted.values(model()) < 0.5, "Normal", "Arrhythmia")
    table("predicted" = predicted, "observed" = Arr$label)
  })
  output$contingency <- renderPrint(contab())
  output$accuracy <- renderText({
    paste0("Accuracy: ", round((contab()[1, 1] + contab()[2, 2]) / sum(contab()), 2) *
             100, "%")
  })
  output$sensitivity <- renderText({
    paste0("Sensitivity (True Positive Rate): ",
           round(contab()[1, 1] / (contab()[1, 1] + contab()[2, 1]), 2) *
             100,
           "%")
  })
  output$specificity <- renderText({
    paste0("Specificity (True Negative Rate): ",
           round(contab()[2, 2] / (contab()[2, 2] + contab()[1, 2]), 2) *
             100,
           "%")
  })
  odds <- reactive(input$odds)
  odds_text <- reactive({
    req(input$odds)
    paste("Odds Interval:")
  })
  output$odds_text <- renderText(odds_text())
  output$odds <- renderPrint({
    if (odds()) {
      exp(cbind("Estimate" = coef(model()), confint(model())))
    }
  })
  
  mcomp <- reactive({
    req(input$comp_button)
    input$comp_button
    isolate({
      if (input$mcomp == "BMIGroups") {
        BMIGroupsModel <-
          glm(label ~ BMIGroups,
              data = Arrhythmia,
              family = binomial(link = "logit"))
        glht(BMIGroupsModel, linfct = mcp(BMIGroups = "Tukey"))
      } else {
        AgeGroupsModel <-
          glm(label ~ AgeGroups,
              data = Arrhythmia,
              family = binomial(link = "logit"))
        glht(AgeGroupsModel, linfct = mcp(AgeGroups = "Tukey"))
      }
    })
  })
  output$mcomps <- renderPrint(summary(mcomp()))
  
  ###################
  ## PREDICT PANEL ##
  ###################
  pred1 <- reactive({
    input$predict1
    isolate({
      p <- predict.glm(
        model_prediction,
        newdata = data.frame(
          "Sex" = input$sex1,
          "Age" = (input$age1 - mean(Arrhythmia$Age)) / sd(Arrhythmia$Age),
          "Heart.rate" = (input$heartrate1 - mean(Arrhythmia$Heart.rate)) /
            sd(Arrhythmia$Heart.rate),
          "Weight" = (input$weight1 - mean(Arrhythmia$Weight)) / sd(Arrhythmia$Weight),
          "QRSDuration" = input$qrs1,
          "T.interval" = input$t_interval1,
          "P.interval" = input$p_interval1
        ),
        type = "response"
      )
    })
  })
  output$person1 <- renderText({
    req(input$predict1)
    paste("Odds of having arrhythmia for person 1 is" ,
          round(exp(pred1()), 2),
          ".")
  })
  pred2 <- reactive({
    input$predict2
    isolate({
      p <- predict.glm(
        model_prediction,
        newdata = data.frame(
          "Sex" = input$sex2,
          "Age" = (input$age2 - mean(Arrhythmia$Age)) / sd(Arrhythmia$Age),
          "Heart.rate" = (input$heartrate2 - mean(Arrhythmia$Heart.rate)) /
            sd(Arrhythmia$Heart.rate),
          "Weight" = (input$weight2 - mean(Arrhythmia$Weight)) / sd(Arrhythmia$Weight),
          "QRSDuration" = input$qrs2,
          "T.interval" = input$t_interval2,
          "P.interval" = input$p_interval2
        ),
        type = "response"
      )
    })
  })
  output$person2 <- renderText({
    req(input$predict2)
    paste("Odds of having arrhythmia for person 2 is" ,
          round(exp(pred2()), 2),
          ".")
  })
  output$logit_ci <- renderPrint({
    cbind("Estimate" = coef(model_prediction),
          confint(model_prediction))
  })
  output$odds_ci <- renderPrint({
    exp(cbind(
      "Estimate" = coef(model_prediction),
      confint(model_prediction)
    ))
  })
}