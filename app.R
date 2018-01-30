library(shiny)
library(stringr)
library(lubridate)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
    
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
            p(),
            p("Default standard deviation based on this resource here: https://spacefem.com/pregnant/charts/duedate0.php"),
            p("Source code here: https://github.com/jrosen48/due-distribution"),
            p("Created by Joshua Rosenberg and Alex Lishinski")
        )
    )
)

create_plot <- function(due_date, my_sd) {
    
    due_dist <- read.csv("preg due.csv", stringsAsFactors = F)
    due_dist$Progress <- str_c(str_trim(due_dist$Progress), str_trim(due_dist$p2), sep = "")
    
    cprob <- function(x, y, xy){
        (xy * y) / x
    }
    
    pcts <- as.numeric(str_sub(due_dist$On.this.date.overall, 1, -3)) * .01
    pcts_cum <- as.numeric(str_sub(due_dist$By.this.date.overall, 1, -3)) * .01
    due_dist$pcts_day <- mapply(FUN = cprob, x = 1 - pcts_cum, y = pcts, xy = 1.00)
    
    due_date <- anytime::anydate(due_date)
    date_today <- Sys.Date()
    diff <- date_today - due_date
    n_days <- days(diff)
    
    #pr <- pnorm(n_days@day / my_sd)
    pr <- round(due_dist[which(due_dist$Progress == "40W0D") + n_days@day, "pcts_day"], digits = 3)
    
    data = data.frame(x = c(-35, 35))
    
    p <- ggplot(data, aes(x)) +
        stat_function(fun = dnorm, n = 1000, args = list(mean = 0, sd = my_sd)) +
        scale_x_continuous() +
        xlab("Days before or after due date") +
        geom_vline(xintercept = diff, color = "red") +
        geom_text(aes(label = paste0("Pr(Baby born on today's date) = ",
                                     round(pr, 3)),
                      y = .005,
                      x = 1,
                      color = "red",
                      size = 24)) +
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