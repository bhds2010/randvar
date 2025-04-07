# RandVar is a simple shiny app to facilitate the mastery of random variables as a ScM student in BHDS at Brown University.

#Run the application 
#install.packages()
#renv::status()
#renv::snapshot()
#gh repo create bhds2010/test --public
# curl -u Alphaprime7 https://api.github.com/orgs/bhds2010/repos -d '{"name":"NAME_OF_REPO", "description":"SOME_DESCRIPTION", "private": true, "has_issues": true, "has_projects": true, "has_wiki":false }'
source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)

#publish
#library(rsconnect)
