#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    shinythemes::shinytheme("cerulean"),
    
    # Application title
    titlePanel("Due distribution calculator"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateInput("date",
                      "Enter the due date:"),
            numericInput("my_sd",
                         "Enter the standard deviation for the due date (defaults to 9.5):",
                         9.5)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            p("Default standard deviation based on this resource here: https://spacefem.com/pregnant/charts/duedate0.php")
        )
    )
)

create_plot <- function(due_date, my_sd) {
    
    # lower_bound <- qnorm(.025) * my_sd
    # upper_bound <- qnorm(.975) * my_sd
    
    date_today <- Sys.Date()
    due_date <- anytime::anydate(due_date)
    diff <- date_today - due_date
    
    pr <- pnorm(diff / my_sd)
    
    library(ggplot2)
    
    p <- ggplot(aes(x), data = data.frame(x = c(-35, 35))) +
        stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = my_sd)) +
        scale_x_continuous() +
        xlab("Days before or after due date") +
        geom_vline(xintercept = diff, color = "red") +
        geom_text(aes(x = diff, 
                      label = paste0("Pr(Baby born by today's date) = ", 
                                     round(pr, 3)), 
                      y = .005, 
                      color = "red",
                      size = 18)) +
        theme_bw() +
        theme(legend.position = "none", text = element_text(size = 16)) +
        ylab("Pr(Baby born)")
    
    p
    
}

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        create_plot(input$date, input$my_sd)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

