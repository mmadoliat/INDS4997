library(shiny);

options(shiny.sanitize.errors = FALSE)

shinyUI(fluidPage(tags$head(tags$style(HTML("body { max-width: 1250px !important; }"))),
                  titlePanel("Regression-Illustration"),
                  sidebarLayout(
                    sidebarPanel(width=3, tags$head(tags$style(type="text/css", ".well { max-width: 300px; }")),
                                 selectInput("model","Model: ", choices = c("Linear"="lin", "Polynomial"="poly", "Orthogonal Design"="orth", "Trig."="trig"), selected="lin", width="250px"), 
                                 numericInput("n", HTML("Sample Size:"), min = 3, max = 1000, value = 50, step = 1, width="250px"),
                                 sliderInput("p", HTML("# of Parameters"), min = 2, max = 5, value = 2, step = 1, width="210px"),
                                 column(6,sliderInput("sigma", HTML("Sigma.e"), min = 0, max = 10, value = 1, step = .1, width="100px")),
                                 column(6,sliderInput("sig.x", HTML("Sigma.x"), min = 0, max = 10, value = 1, step = .1, width="200px")),
                                 checkboxInput("asp", "Same x-y Scale", value = FALSE),
                                 tags$hr(style="border-color: red;", width="150px"), 
                                 sliderInput("b0", HTML("b0"), min = -10, max = 10, value = 1, step = .1, width="210px"),
                                 sliderInput("b1", HTML("b1"), min = -10, max = 10, value = 1, step = .1, width="210px"),
                                 uiOutput("b2"),uiOutput("b3"),uiOutput("b4"),
                                 column(6,uiOutput("simul"))),
                    mainPanel(width=9, tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; },"),#".nav-tabs {font-size: 10px}"),
                              tabsetPanel(id="Panel", type = "tabs", 
                                          tabPanel(title="Data", value="Data",
                                                   column(12, uiOutput("ts.selected", align = "center"), style="color:red;"),
                                                   fluidRow(column(4,radioButtons('f.choice', 'Choose from:', c("Simulate" = "sim", "Server" = "server", "Upload" = "upload"), selected= "sim", inline = TRUE, width="250px")), 
                                                            column(4,uiOutput("s.choice", width="250px"))),
                                                   fluidRow(column(4,uiOutput("file")), column(8,uiOutput("sep"),uiOutput("header"))),
                                                   column(8,plotOutput("data.plot", height = 600, width = 600)), column(4,tableOutput('data'))),
                                          tabPanel("Basic Linear Regression",
                                                   column(8,plotOutput("regr.desc", height = 600, width = 600), verbatimTextOutput("RegRes")), 
                                                   column(4,uiOutput("resp", width="300px"), uiOutput("pred", width="300px"),
                                                          checkboxGroupInput('glm.int', 'Misc:', choices=c("Linear Regression"="linm", "Logistic Regression"="logm", "Include Interaction"="in.int"), width="150px"),
                                                          radioButtons("scaling", "Scaling", c("None" = "none", "Unit Normal" = "unN", "Unit Length" = "unL"), selected="none", inline = TRUE),
                                                          selectInput("reg.plot","Plot: ", choices = c("Regression Line"="reg.lin", "Residuals Plots"="reg.res", "Partial Regression"="par.reg", "BoxCox Transformation"="boxcox", "Fitted vs Residual"="res.fit"), selected="reg.lin", width="210px"),
                                                          selectInput("reg.text","Output: ", choices = c(" "="NULL", "Summary"="summary", "ANOVA"="anova", "Design Matrix Meas."="xtxi", "Influence Measures"="infM", "Residuals"="res"), selected="summary", width="210px"), 
                                                          column(6, checkboxGroupInput('sh.int', 'Intervals:', choices=c("Confidence"="conf", "Prediction"="pred"), width="150px")), 
                                                          column(6, sliderInput("level", HTML("a-level"), min = 0.5, max = 1, value = .95, step = .01, width="150px")),
                                                          uiOutput("x0"), verbatimTextOutput("regRes")))))
                  )
))

