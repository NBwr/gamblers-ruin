library(shiny)

ui <- fluidPage(
  tags$style(".fa-github {color:black}"),
  headerPanel(tagList("Gambler's Ruin", a(icon("github", lib = "font-awesome", "fa-1x"), href="https://github.com/NBwr/gamblers-ruin"))),
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
    textOutput("stats_info"),
    plotOutput('plot3')
  )
)

server <- function(input, output) {
  
  Pn = function(p,n=10,m=10,verbose=0) {#Probability to win
    k=length(p)
    pn = rep(NA,k)
    io = (1-p)/p #inverse odds
    if (verbose >1) return(c(io^n-1, io^(n+m)-1))
    pn[p< 0.5] = ((io^n-1)/(io^(n+m)-1))[p< 0.5]
    pn[p == 0.5] = n/(n+m)
    return(pn)
  }  
  
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
  
  output$plot3 = renderPlot({
    p = seq(variables()$p, 0.5, length=100)
    plot(p, Pn(p, variables()$n, variables()$m-variables()$n), type = 'l', xlab = 'p',
         ylab = 'Probability to win', xlim = c(variables()$p,0.5),
         lwd=2,lty=1, col = "darkblue");grid()
  })
  
  
  output$stats_info = renderText({
    paste0("The odds of winning ", variables()$m-variables()$n, " before losing ", 
           variables()$n, " are: ", formatC(Pn(variables()$p, variables()$n, variables()$m-variables()$n)*100, format="e", digits=4), "%")
  })
}

shinyApp(ui = ui, server = server)
