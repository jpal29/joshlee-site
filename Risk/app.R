library(shiny)
library(scales)

one_shake_function <- function(att_troop_per_shake, def_troop_per_shake){
  
  dice <- 1:6
  
  att_shake <- c(sort(sample(x = dice, size = att_troop_per_shake, replace = TRUE), decreasing = TRUE))
  
  def_shake <- c(sort(sample(x = dice, size = def_troop_per_shake, replace = TRUE), decreasing = TRUE))
  
  dice_in_play <- min(att_troop_per_shake, def_troop_per_shake)
  
  att_shake_in_play <- att_shake[1:dice_in_play]
  
  def_shake_in_play <- def_shake[1:dice_in_play]
  
  result <- att_shake_in_play > def_shake_in_play
  
  att_troops_lost <- sum(result == FALSE)
  
  def_troops_lost <- sum(result == TRUE)
  
  return(c(att_troops_lost, def_troops_lost))
  
}

attack_function <- function(attack_no, def_no){
  
  att_troops_left <- attack_no
  
  def_troops_left <- def_no
  
  while (att_troops_left > 0 & def_troops_left > 0) {
    
    att_troops_available <- min(3, att_troops_left)
    
    def_troops_available <- min(2, def_troops_left)
    
    troops_lost <- one_shake_function(att_troop_per_shake = att_troops_available,
                                      
                                      def_troop_per_shake = def_troops_available)
    
    att_troops_left <- att_troops_left - troops_lost[1]
    
    def_troops_left <- def_troops_left - troops_lost[2]
    
  }
  
  return(c(att_troops_left, def_troops_left))
  
}

sim_function <- function(n = 10000, attack_number, def_number){
  showModal(modalDialog("Simulating Outcomes"))
  win_outcomes <- list()
  
  att_troops_lost_outcome <- numeric()
  
  def_troops_lost_outcome <- numeric()
  
  for (i in 1:n){
    
    result <- attack_function(attack_no = attack_number,
                              
                              def_no = def_number)
    
    att_troops_left <- result[1]
    
    def_troops_left <- result[2]
    
    att_troops_lost <- attack_number - att_troops_left
    
    def_troops_lost <- def_number - def_troops_left
    
    att_victory <- as.logical(att_troops_left > def_troops_left)
    
    win_outcomes <- append(win_outcomes, att_victory)
    
    att_troops_lost_outcome <- append(att_troops_lost_outcome, att_troops_lost)
    
    def_troops_lost_outcome <- append(def_troops_lost_outcome, def_troops_lost)
    
  }
  
  perc_att_victory <- sum(win_outcomes == TRUE)/n
  avg_att_troops_lost <- mean(att_troops_lost_outcome)
  avg_def_troops_lost <- mean(def_troops_lost_outcome)
  
  outcome <- list("att_troops_lost_outcome" = att_troops_lost_outcome,
                  "avg_att_troops_lost" = avg_att_troops_lost,
                  "def_troops_lost_outcome" = def_troops_lost_outcome,
                  "avg_def_troops_lost" = avg_def_troops_lost,
                  "perc_att_victory" = perc_att_victory)
  removeModal()
  return(outcome)
  
}

#Define UI for app that draws a histogram

ui <- fluidPage(
  
  # App title ----
  titlePanel("Risk Attacks Outcome"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      numericInput(inputId = "att_troop_num",
                   label = "Number of attacking troops",
                   min = 1,
                   max = 100,
                   value = ""),
      numericInput(inputId = "def_troop_num",
                   label = "Number of defending troops",
                   min = 1,
                   max = 100,
                   value = "")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      textOutput(outputId="att_win_pct"),
      textOutput(outputId="avg_att_lost"),
      textOutput(outputId="avg_def_lost"),
      #Output: Histogram ----
      plotOutput(outputId = "attTroopPlot"),
      plotOutput(outputId = "defTroopPlot")
      
    )
  )
    
)

server <- function(input, output) {

    outcome <- reactive({
      sim_function(n=10000, attack_number = input$att_troop_num, def_number = input$def_troop_num)
    })
  
   output$attTroopPlot <- renderPlot({
     
     validate(
       need(input$att_troop_num != '', 'Select attacking troop count'),
       need(input$def_troop_num != '', '')
     )
     
     
     outcome_list <- outcome()
     x <- outcome_list$att_troops_lost_outcome
  
     bins <- seq(min(x), max(x), length.out = input$att_troop_num + 1)
  
     hist(x, breaks=bins, col = "#75AADB", border = "white",
          xlab = "Attacking Troops Lost",
          main = "Distribution of Attacking Troops Lost")
     
     output$att_win_pct <- renderText({paste("Probability of Attack Win: ", percent(outcome_list$perc_att_victory))})
     output$avg_att_lost <- renderText({paste("Expected Attacking Troops Lost: ",outcome_list$avg_att_troops_lost )})
     output$avg_def_lost <- renderText({paste("Expected Defending Troops Lost: ", outcome_list$avg_def_troops_lost)})
   })

   output$defTroopPlot <- renderPlot({
     
     validate(
       need(input$def_troop_num != '', 'Select defending troop count'),
       need(input$att_troop_num != '', '')
     )
     
     outcome_list <- outcome()
     
     x <- outcome_list$def_troops_lost_outcome
     bins <- seq(min(x), max(x), length.out = input$def_troop_num + 1)
  
     hist(x, breaks=bins, col = "#75AADB", border = "white",
          xlab = "Defending Troops Lost",
          main = "Distribution of Defending Troops Lost")
     
     output$att_win_pct <- renderText({paste("Probability of Attack Win: ", percent(outcome_list$perc_att_victory))})
     output$avg_att_lost <- renderText({paste("Expected Attacking Troops Lost: ",outcome_list$avg_att_troops_lost )})
     output$avg_def_lost <- renderText({paste("Expected Defending Troops Lost: ", outcome_list$avg_def_troops_lost)})
   })
  
}

shinyApp(ui = ui, server = server)