library(shiny)
runExample("01_hello")

runApp("Shiny/my_app", display.mode = "showcase")

runApp("Shiny/my_app/app2.R", display.mode = "showcase")

runApp("Shiny/my_app/app3.R", display.mode = "showcase")

runApp("Shiny/census-app", display.mode = "showcase")


counties <- readRDS("Shiny/census-app/data/counties.rds")
head(counties)

library(maps)
library(mapproj)
source("Shiny/census-app/helpers.R")
counties <- readRDS("Shiny/census-app/data/counties.rds")
percent_map(counties$white, "darkgreen", "% White")

runApp("Shiny/census-app/app.R", display.mode = "showcase")

runApp("Shiny/census-app/app1.R", display.mode = "showcase")

runApp("Shiny/stockVis", display.mode = "showcase")


library(shiny)
runUrl("https://raw.githubusercontent.com/mmadoliat/INDS4997/main/Shiny/stockVis/stockVis.zip")
runGitHub("INDS4997", username = "mmadoliat", subdir = "Shiny/stockVis")
runGist("eb3470beb1c0252bd0289cbc89bcf36f")
