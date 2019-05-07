# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

## Code written by Grant McDermott.
## Based on the dynamic model of open access described in Conrad (2010, Chpt. 3.5).

library(shiny)
library(shinythemes)
library(tidyverse)
library(hrbrthemes)
theme_set(hrbrthemes::theme_ipsum_rc(axis_title_size = 12, base_size = 15))

#############################
#############################
### Define UI for the app ###
#############################
#############################

ui <- 
  fluidPage(
    theme = shinytheme("flatly"),
    
    ## Add MathJax support for LaTeX: Here Denoted by $ (inline) or $$ (displayed separately)
    ## See: http://docs.mathjax.org/en/latest/start.html
    tags$head(
      tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
      tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});", type='text/x-mathjax-config')
      ),
    
    ## Application title
    titlePanel("Open-access fishery dynamics"),
    h5("Author: ", a("@grant_mcdermott", href="https://twitter.com/grant_mcdermott", target="_blank")),
    h5("Source code: ", a("https://github.com/grantmcdermott/open-access-fishery", href="https://github.com/grantmcdermott/open-access-fishery", target="_blank")),
    

     ## Sidebar with sliders for our input variables
     sidebarLayout(
        sidebarPanel(
          "As per", a("Conrad (2010, Chpt. 3.5)", href="https://www.amazon.com/Resource-Economics-Jon-M-Conrad-ebook/dp/B00FF76RAK", target="_blank"),"this fishery is governed by the difference equations:", 
          "$$ X_{t+1} = [1+r - rX_t/K - qEt]X_t $$",
          "$$ E_{t+1} = [1+\\eta(pq_tX_t-c)]E_t $$",
          "Adjust the individual parameters below to see how it affects the system dynamics.",
          tags$br(),
          tags$br(),
          ## Intrinsic growth rate
          sliderInput(
            inputId = "r", label = "Intrinsic growth rate $(r)$:",
            min = 0.05, max = 0.3, value = 0.1
            ),
          ## Environmental carrying capacity
          sliderInput(
            inputId = "K", label = "Carrying capacity $(K)$:",
            min = 0.1, max = 1, value = 1
            ),
          ## Catchability coefficient
          sliderInput(
            inputId = "q", label = "Catchability coefficient $(q)$:",
            min = 0.01, max = 0.1, value = 0.01
            ),
          ## Adjustment (stiffness paramter)
          sliderInput(
            inputId = "eta", label = HTML("Adjustment parameter $(\\eta)$:"),
            min = 0.1, max = 1, value = 0.3
            ),
          ## Per-unit price
          sliderInput(
            inputId = "p", label = "Per-unit price $(p)$:",
            min = 10, max = 500, value = 200
            ),
          ## Unit cost of effort
          sliderInput(
            inputId = "C", label = "Unit cost of effort ($c$):",
            min = 1, max = 10, value = 1
            ),
          ### Initial conditions
          ## Initial stock
          sliderInput(
            inputId = "x0", label = HTML(paste0("Initial stock level ($X_0$)")),
            min = 0, max = 1, value =0.5
            ),
          ## Initial effort
          sliderInput(
            inputId = "e0", label = HTML(paste0("Initial effort level $(E_0)$")),
            min = 0, max = 1, value = 1
            ),
          ## Total time periods
          sliderInput(
            inputId = "T", label = "Total time periods $(T)$",
            min = 0, max = 100, value = 100
            )
           ),
        ## Show the generated plot(s)
        mainPanel(
          ## Combined plot option 1: Use patchwork package (Doesn't work with R 3.6.0 yet...)
          # plotOutput("plot_combined")
          ##  Combined plot option 2: Use in-built shiny fluidRow layout functionality 
          tabPanel("Plot",
                   fluidRow(
                     fluidRow(
                       splitLayout(
                         cellWidths = c("50%", "50%"),
                         plotOutput("p1"), plotOutput("p2")
                         )
                     ),
                     plotOutput("p3")
                   )) ## End of tabPanel
          ) ## End of mainPanel
        ) ## End of sidebarLayout
     ) ## End of fluidPage


###########################################################################
###########################################################################
### Define server logic required execute the underlying simulation code ###
###########################################################################
###########################################################################

server <- 
  function(input, output) {
    
    ## First, create the (reactive) data frame, which I'll creatively call "fish"
    fish <- 
      reactive({
        ## Variables
        r <- input$r
        K <- input$K
        q <- input$q
        eta <- input$eta
        p <- input$p
        C <- input$C
        ## Initial conditions and total time periods
        x0 <- input$x0
        e0 <- input$e0
        T <- input$T
        
        ## Create data frame
        Time <- 1
        Stock <- x0
        Effort <- e0
        
        sapply(1:T, function(t){
          Time[t+1] <<- t+1
          Stock[t+1] <<- (1 + r - r*Stock[t]/K - q*Effort[t]) * Stock[t]
          if (Stock[t+1] < 0) Stock[t+1] <<- 0 ## Stock can't go negative
          Effort[t+1] <<- (1 + eta * (p*q*Stock[t] - C)) * Effort[t]
          })
      
        fish <- 
          tibble(Time, Stock, Effort) %>%
          # mutate(Catch = q*K*Effort*(1-q/r*Effort))
          mutate(Catch = q*Effort*Stock)
        
        })
    
    
    ## Next, draw the plots and arrange in a grid

    ## Option 1: Render as one, using patchwork to arrange (Simple but doesn't work with R 3.6.0 yet...)
    # output$plot_combined <-
    #   renderPlot({
    #     ## Note "fish()" rather than conventional "fish"
    #     p1 <-
    #       fish() %>%
    #       geom_path(lwd=0.75) +
    #       labs(title = "Catch vs. Effort") +
    #       scale_colour_viridis_c()
    #     p2 <-
    #       fish() %>%
    #       ggplot(aes(x=Stock, y=Effort, col=Time)) +
    #       geom_path(lwd=0.75) +
    #      labs(title = "Stock vs. Effort") +
    #       scale_colour_viridis_c()
    #     p3 <-
    #       fish() %>%
    #       gather(Key, Value, -Time) %>%
    #       ## Optional: Group catch and stock together to reflect relative catch
    #       mutate(norm_key = ifelse(Key %in% c("Catch", "Stock"), 1, 0)) %>%
    #       group_by(norm_key) %>%
    #       mutate(Value_norm = Value/max(Value)) %>%
    #       ggplot(aes(x=Time, y=Value_norm, col=Key)) +
    #       geom_line(lwd=0.75) +
    #       labs(
    #         title = "Time series (normalised)",
    #         y = "Normalised values"
    #         ) +
    #       scale_colour_brewer(palette = "Set1")
    #     p1 + p2 - p3 + plot_layout(ncol = 1)
    #     })
    ## Option 2: Render each plot separately. Will arrange above using Shiny's in-built fluidRow layout functionality.
    output$p1 <-
      renderPlot({
        ## Note "fish()" rather than conventional "fish"
        fish() %>%
          ggplot(aes(x=Catch, y=Effort, col=Time)) +
          geom_path(lwd=0.75) +
          labs(title = "Catch vs. Effort") +
          scale_colour_viridis_c()
        })
    output$p2 <-
      renderPlot({
        ## Note "fish()" rather than conventional "fish"
        fish() %>%
          ggplot(aes(x=Stock, y=Effort, col=Time)) +
          geom_path(lwd=0.75) +
          labs(title = "Stock vs. Effort") +
          scale_colour_viridis_c()
        })
    output$p3 <-
      renderPlot({
        ## Note "fish()" rather than conventional "fish"
        fish() %>%
          gather(Key, Value, -Time) %>%
          ## Optional: Group catch and stock together to reflect relative catch
          mutate(norm_key = ifelse(Key %in% c("Catch", "Stock"), 1, 0)) %>%
          group_by(norm_key) %>%
          mutate(Value_norm = Value/max(Value)) %>%
          ggplot(aes(x=Time, y=Value_norm, col=Key)) +
          geom_line(lwd=0.75) +
          labs(
            title = "Time series (normalised)",
            y = "Normalised values"
            ) +
          scale_colour_brewer(palette = "Set1")
        })
}

## Run the app 
shinyApp(ui = ui, server = server)
