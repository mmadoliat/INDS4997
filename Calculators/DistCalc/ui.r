library(shiny);

options(shiny.sanitize.errors = FALSE)

shinyUI(fluidPage(tags$head(tags$style(HTML("body { max-width: 1250px !important; }"))),
        headerPanel("Distribution Calculator"),
        sidebarPanel(width=3, tags$head(tags$style(type="text/css", ".well { max-width: 250px; }")),
          selectInput("type","Type: ", choices = c("Continuous" = "cont", "Discrete" = "disc")),
          uiOutput("dist"),
          uiOutput("p1"),
          uiOutput("p2"),
          uiOutput("p3"),
          selectInput("fun", "Function: ", choices = c("Probability function" = "prob", "Quantile function" = "quantile")),
          uiOutput("prob"),
          uiOutput("a"),
          uiOutput("q")
       ),
       mainPanel(width=9, tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; },"),#".nav-tabs {font-size: 10px}"),
          uiOutput("g.choice"),
          uiOutput("area", align = "center", style="font-size:170%;"),
          plotOutput("plot")
       )
  ))
