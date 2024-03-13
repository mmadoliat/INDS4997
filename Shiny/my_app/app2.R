library(shiny)

ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      h1("First level title"),
      h2("Second level title"),
      h3("Third level title"),
      h4("Fourth level title"),
      h5("Fifth level title"),
      h6("Sixth level title")
    )
  )
)

# ui <- fluidPage(
#   titlePanel("My Star Wars App"),
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       h6("Episode IV", align = "center"),
#       h6("A NEW HOPE", align = "center"),
#       h5("It is a period of civil war.", align = "center"),
#       h4("Rebel spaceships, striking", align = "center"),
#       h3("from a hidden base, have won", align = "center"),
#       h2("their first victory against the", align = "center"),
#       h1("evil Galactic Empire.")
#     )
#   )
# )
# 
# ui <- fluidPage(
#   titlePanel("My Shiny App"),
#   sidebarLayout(
#     sidebarPanel(),
#     mainPanel(
#       p("p creates a paragraph of text."),
#       p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph.", style = "font-family: 'times'; font-si16pt"),
#       strong("strong() makes bold text."),
#       em("em() creates italicized (i.e, emphasized) text."),
#       br(),
#       code("code displays your text similar to computer code"),
#       div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
#       br(),
#       p("span does the same thing as div, but it works with",
#         span("groups of words", style = "color:blue"),
#         "that appear inside a paragraph.")
#     )
#   )
# )

shinyApp(ui = ui, server = server)
