library(shiny)
library(tidyverse)


bday_p <- function(k) {
  if (k <= 1) return(0)
  if (k > 365) return(1)
  q <- 1 - (0:(k - 1)) / 365
  1 - prod(q)
}

k <- 60

p <- numeric(k)

for(i in 1:k){
  q <- 1 - (0:(i-1)) / 365
  p[i] <- 1 - prod(q)
}

p_df <- data.frame(
  people = 1:k,
  p = p*100
)

ui <- fluidPage(
  titlePanel(
    div(
      h1("The Birthday Paradox"),
      h3("Explanation, Visualization and Random Generator",
         style = "color: grey; margin-top: -10px;")
    )
  ),
  
  withMathJax(
    div(
      h4("What is the Birthday Paradox?", style = "margin-top: 30px;"),
      HTML("
      
      <p>The Birthday Paradox (also referred to as the Birthday Problem) is an excellent example of how our intuition regarding probabilities can mislead us.
      It is attributed to the Austrian mathematician Richard von Mises—the younger brother of the renowned Austrian economist
      Ludwig von Mises—who first formulated it in 1939. Richard von Mises posed the following question:
      </p>
      
      <p>
      <b>How many people need to be in a room for the chance that at least two of them share a birthday to exceed 50%?</b>
      </p>
      
      <p>
      The surprising answer is: 23. Conversely, one could ask: 'What is the probability that, 
      among 23 people, at least two of them share the same birthday?' Most people would answer 
      that the probability is very low. After all, how often do you meet someone who shares your own birthday? 
      Exactly herein lies the misconseption. We tend to compare ourselves to others, 
      disregarding the fact that there are 22 other people in the room who can all be compared 
      with one another in various combinations to form a pair. In other words:
      we underestimate how many different pairs can be formed from 23 people.
      </p>
      
      <p>
      There are <b>253</b> possible ways to form a pair from 23 people. 
  This is derived from the binomial coefficient, which tells us in how many 
  different ways one can choose a subset of \\(k\\) objects from a 
  given set of \\(n\\) distinct objects:
      </p>
      \\[
      \\binom{n}{k} = \\frac{n!}{k! \\cdot (n - k)!}
      \\]
      Applied to our question:
      <p>
      <p>
      \\[
      \\binom{23}{2} = \\frac{23!}{2! \\cdot (23-2)!} = 253
      \\]
      </p>
      </p>
      
      <p>
      The probability \\(P\\) that at least two people share the same birthday is 
      calculated using the complement probability \\( \\overline{P} \\) 
      that all birthdays are different from one another:
      \\[
      \\ P = 1 - \\overline{P}
      \\]
      </p>
      
      <p>
      The complement probability \\(\\overline{P} \\) is calculated as the 
  product of the probabilities that each additional person has a 
  different birthday than all previously considered individuals. 
  For \\(n \\) people, this means:
      </p>
      \\[
      \\overline{P} = \\frac{365}{365} \\cdot \\frac{364}{365} \\cdot \\frac{363}{365}
      \\cdot (...) \\cdot \\frac{365 - (n - 1)}{365} = \\frac{365!}{(365 - n)! \\cdot 365^{n}}
      \\]

      <p>
      \\[
      \\overline{P} = \\prod_{k=0}^{n-1} \\frac{365-k}{365}
      \\]
      </p>
      <p>
      Of course, this calculation is not exact. In reality, we deal with leap years, 
  twins, triplets, esoterically minded parents, and other cultural, biological, 
  geographical, and climatic influences that make an exact calculation practically 
  impossible. Under the assumptions that leap years are ignored, individuals are independent 
  of one another, and the probability for all birthdays is uniformly distributed, 
  we can say with certainty: in a group of 23 people, it is more likely than not 
  to find two or more individuals who share the same birthday.
      <p>
      Below is a visualization of the probabilities relative to the number of people. 
      To calculate the exact probability for any given number of people, 
      a calculator has been included below, which generates random data 
      and highlights any discovered pairs in red.
      </p>
        "
      )
    )
  ),
 
  
  plotOutput("probPlot"),
  
  hr(),
  
  h4("Random Generator"),
  numericInput("k_input", "Number of people:", value = 23, min = 1, max = 365),
  actionButton("compute", "Calculate Probability and Simulate Birthdays",
               class = "btn-default"),
  verbatimTextOutput("calcResult"),
  br(),
  h4("Simulated Birthdays"),
  uiOutput("birthdayBoxes")
)


server <- function(input, output, session) {
  output$probPlot <- renderPlot({
    p_df |> 
      ggplot(aes(x = people, y = p)) +
      geom_point(size = 0.5) +
      geom_vline(aes(xintercept = 23), color = "red", linetype = "dotted") +
      geom_vline(aes(xintercept = 41), color = "red", linetype = "dotted") +
      annotate(
        "text",
        x = 23,
        y = 45,
        label = "
        For n = 23, the probability is already greater than 50 %",
        size = 3.5,
        hjust = -0.1
        ) +
      annotate(
        "text",
        x = 41,
        y = 85,
        label = "
        For n = 41, the probability is already greater than 90 %",
        size = 3.5,
        hjust = -0.1
      ) +
      scale_x_continuous(breaks = seq(0, 75, by = 5)) +
      scale_y_continuous(breaks = seq(0, 100, by = 10)) +
      labs(
        x = "Number of People in a Room",
        y = "Probability (%)"
      )
  })
  
  results <- eventReactive(input$compute, {
    k_val <- input$k_input
    prob <- bday_p(k_val) * 100
    birthdays <- sample(1:365, k_val, replace = TRUE)
    duplicates <- duplicated(birthdays) | duplicated(birthdays,
                                                     fromLast = TRUE)
    
    list(prob = prob, birthdays = birthdays, duplicates = duplicates,
         k_val = k_val)
  })
  
  output$calcResult <- renderText({
    if(input$compute == 0) return("")
    
    data <- results()
    
    k_val <- data$k_val
    
    if(k_val >= 366){
      return(paste0("For ", k_val, " people, the probability is 100 %."))
    }
    
    if(data$prob >= 99.99){
      return(paste0("For ", k_val, " people, the probability is ≈ 100 %."))
    }
    
    paste0("For ", k_val, " people, the probability is  ",
           sprintf("%.2f", data$prob), " %.")
    
  })
  
  output$birthdayBoxes <- renderUI({
    if(input$compute == 0) return(NULL)
    
    data <- results()
    
    boxes <- map(1:length(data$birthdays), function(i) {
      style <- if(data$duplicates[i]) {
        "display: inline-block; margin: 3px; padding: 8px; background-color: #FF474C;"
      } else {
        "display: inline-block; margin: 3px; padding: 8px;"
      }
      
      div(style = style, data$birthdays[i])
    })
    
    div(boxes)
  })
}

shinyApp(ui = ui, server = server)
