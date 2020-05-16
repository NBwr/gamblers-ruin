library(shiny)

ui <- fluidPage(
  #icon("github", lib = "font-awesome", "fa-2x"),
  tags$style(".fa-github {color:black}"),
  headerPanel(tagList("Gambler's Ruin", a(icon("github", lib = "font-awesome", "fa-1x"), href="https://github.com/NBwr"))),
  #navbarPage(icon("calendar")),
  sidebarPanel(
    numericInput('probs', 'Probability of winning single bets', round(9/19,3),
                 min = 0, max = 0.49999999999999999999999999999999999, step = 0.001),
    numericInput('start_n', 'Start capital', 100,
                 min = 1, max = 1000, step = 10),
    numericInput('upper_b', 'Upper boundary', 200,
                 min = 1, max = 100000, step = 10),
    numericInput('time_steps', 'No. of time steps', 2000,
                 min = 1, max = 100000, step = 100),
    numericInput('number_lines', 'No. of plotted lines', 40,
                 min = 1, max = 1000, step = 1),
    numericInput('seeed', 'Seed', 123,
                 min = 1, max = 100000)
    
    
  ),
  mainPanel(
    plotOutput('plot2'),
    textOutput("stats_info")
  )
)

server <- function(input, output) {
  
  variables = reactive({
    p = input$probs
    n = input$start_n
    m = input$upper_b
    N = input$time_steps
    M = input$number_lines
    
    set.seed(input$seeed)
    
    x=matrix(sample(c(-1,1),N*M,p=c(1-p,p),replace = TRUE),ncol=M)
    y1=n+apply(x,2,cumsum)
    
    list(p=p, n=n, m=m, N=N, M=M, x=x, y1=y1)
  })
  
  
  output$plot2 = renderPlot({
  matplot(variables()$y1,type="l",col=rgb(0,0,1,0.5),lty=1,ylim=c(0,variables()$m),ylab="Returns", xlab="Number of time steps");grid()
  title(paste0("p=",variables()$p))
  abline(h=0,col=2);abline(h=variables()$m,col="darkgreen")
  })
  
  output$stats_info = renderText({
    paste0("You have simulated ", variables()$M, " bets. At the end of the simulation, ", 
           sum(tail(variables()$y1,1)>variables()$n), " bets are larger than the initial start capital of ", variables()$n,
           " Euro per bet, while ", sum(tail(variables()$y1,1)<variables()$n), " bets are smaller. 
           Running these bets and odds in a casino, you would have made ", sum(tail(variables()$y1,1))-variables()$M*variables()$n, " Euro.") 
  })
  #dev.off()
  #png("figures/RandomWalks1.png",width=800,height = 400)
  #matplot(y2,type="l",col=rgb(0,0,1,0.5),lty=1,ylim=c(0,n+m));grid()
  #title("p=0.5")
  #abline(h=0,col=2);abline(h=n+m,col="darkgreen")
  #dev.off()
  
  

}

shinyApp(ui = ui, server = server)
