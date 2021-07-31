# Created on 7-17-21
# Julianna Bernardi



library(shiny)
library(dplyr)
library(ggplot2)
library(aod)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

slump_dat <- read.csv("slump_test.csv") %>%
    rename(
        "fly_ash" = "Fly.ash",
        "course_agg" = "Coarse.Aggr.",
        "fine_agg" = "Fine.Aggr.",
        "slump" = "SLUMP.cm.",
        "flow" = "FLOW.cm.",
        "compstrength" = "Compressive.Strength..28.day..Mpa."
    ) %>% select(-No)
colnames(slump_dat) <- tolower(colnames(slump_dat))
target_vars <- c("slump", "flow", "compstrength")

cols_to_pretty <- c(
    "cement" = "Cement (kg)",
    "slag" = "Slag (kg)",
    "fly_ash" = "Fly Ash (kg)",
    "water" = "Water (kg)",
    "sp" = "Superplasticizer (kg)",
    "course_agg" = "Course Aggregate (kg)",
    "fine_agg" = "Fine Aggregate (kg)",
    "slump" = "Slump (cm)",
    "flow" = "Flow (cm)",
    "compstrength" = "28 Day Compressive Strength (Mpa)"
    
)
pretty_to_cols <- setNames(names(cols_to_pretty), cols_to_pretty)
clean_dat <- slump_dat %>%
    rename(pretty_to_cols)


# Define UI for application that draws a histogram
ui <- navbarPage(
    "Concrete Slump Test Linear Model",
    
    # Model tab ----
    tabPanel(
        title = "Model Explore",
        sidebarLayout(
            sidebarPanel(
                selectInput(
                    "target_var",
                    "Choose the variable you want to predict",
                    choices = setNames(target_vars, cols_to_pretty[target_vars])
                ),
                selectizeInput(
                    "covs",
                    "Choose the input variables for your model",
                    choices = pretty_to_cols[-match(target_vars, pretty_to_cols)],
                    selected = pretty_to_cols[-match(target_vars, pretty_to_cols)][1],
                    multiple = T
                )
            ),
            mainPanel(
                fluidRow(
                    column(
                        6,
                        verbatimTextOutput("mod_summary")
                    ),
                    column(
                        6,
                        plotOutput("resid_plot")
                    )
                ),
                fluidRow(
                    column(
                        6,
                        plotOutput("pred_outcome")
                    ),
                    column(
                        6,
                        plotOutput("fitted_scatter")
                    )
                )
            ))
    ),
    tabPanel(
        # Linear Regression ----
        title = "Linear Regression",
        fluidRow(
            includeScript(rmarkdown::render("linear_regression_overview.Rmd"))
        )
    ),
    tabPanel(
        # EDA tab ----
        title = "Exploratory Data Analysis",
        fluidRow(
            includeScript(rmarkdown::render("cement_EDA.Rmd"))
            # tags$script(src = "cement_EDA.Rmd")
        )
        
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    # Model tab ----
    # Build model
    model <- reactive({
        form <- paste0(
            paste0(input$target_var, " ~ "), paste(input$covs, collapse = "+")
        )
        
        lm(data = slump_dat, formula(form))
    })
    
    # Model summary
    output$mod_summary <- renderPrint({
        summary(model())
    })
    
    # Residual plot
    output$resid_plot <- renderPlot({
        resids <- data.frame(residuals = model()$residuals,
                             fitted = model()$fitted.values)
        
        ggplot(resids)+
            geom_point(aes(x = fitted, y = residuals), alpha = .6)+
            geom_hline(yintercept = 0.0, lty = 2)+
            ggtitle("Residual Plot")
    })
    
    # Fitted versus actuals
    output$fitted_scatter <- renderPlot({
        temp <- data.frame(
            fitted = model()$fitted,
            outcome = slump_dat[[input$target_var]]
        )
        
        ggplot(temp)+
            geom_point(aes(x = fitted, y = outcome), alpha = .6)+
            xlab("Fitted Values")+
            ylab(input$target_var)+
            ggtitle("Target Versus Fitted Values")
        geom_abline(lty = 2, col = "blue")
    })
    
    # Predictors versus outcome
    output$pred_outcome <- renderPlot({
        s_plots <- list()
        vars <- cols_to_pretty[names(slump_dat)[-match(target_vars, names(slump_dat))]]
        for (i in 1:length(vars)){
            xvar <- vars[i]
            yvar <- cols_to_pretty[input$target_var]
            
            
            
            if (pretty_to_cols[xvar] %in% input$covs){
                g <- ggplot(clean_dat)+
                    geom_point(aes_(x = as.name(xvar), y = as.name(yvar)), alpha = .6, col = "#0066ff")
            }else{
                g <- ggplot(clean_dat)+
                    geom_point(aes_(x = as.name(xvar), y = as.name(yvar)), alpha = .6)
            }
            
            if (i %% 3 - 1 != 0){
                g <- g + ylab("")
            }
            
            s_plots[[paste0('g', i)]] <- g
        }
        do.call("grid.arrange", c(s_plots, ncol = 3))
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
