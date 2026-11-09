# Setup the git and github prerequisites (based on https://happygitwithr.com/ssh-keys.html)

install.packages("gert")
install.packages("credentials")

library(gert)
library(credentials)

usethis::create_github_token()

# Add this computer as a trusted agent
credentials::ssh_agent_add()

# Find the SSH file on local machine
credentials::ssh_home()

# Get the SSH key info
credentials::ssh_key_info()

# Check if ssh-agent is enabled in terminal (if enabled will return an Agent pid)
  ## on Mac
    ###eval "$(ssh-agent -s)"
  ## on Linux
    ###eval "$(ssh-agent -s)"
  ## on Windows
    ###eval $(ssh-agent -s)

# Setup
credentials::ssh_setup_github()

# To check that ssh-authentication works, try to run
# ssh -T git@github.com

# Do the followings in terminal 
#1 pull all files from the GitHub repo (typically just readme, license, gitignore)
# git remote add origin git@github.com:asalavaty/color_distance.git

#2 set up GitHub repo to track changes on local machine
# git pull origin master #master is the name of the parent branch on github. Check first as sometimes it's called "main"

#3 Cement the tracking relationship between your GitHub repository and the local repo by pushing and setting the “upstream” remote:
# git push -u origin master

###*********************************************************###

install.packages(c(
  "gapminder", "ggforce", "globals", "reactable", "openintro", "RSQLite", "shiny", "shinyEffects",
  "shinycssloaders", "shinyFeedback", "shinythemes", "testthat", "profvis", "argonR", "argonDash", "bs4Dash",
  "thematic", "tidyverse", "vroom", "waiter", "xml2", "zeallot" , "shinydashboard", "shinydashboardPlus"
))

library(gapminder)
library(shinydashboard)
library(shinydashboardPlus)
library(argonR)
library(argonDash)
library(bs4Dash)
library(shinyEffects)
library(echarts4r)
library(profvis)
library(ggforce)
library(globals)
library(reactable)
library(openintro)
library(RSQLite)
library(shiny)
library(shinycssloaders)
library(shinyFeedback)
library(shinythemes)
library(testthat)
library(thematic)
library(tidyverse)
library(vroom)
library(waiter)
library(xml2)
library(zeallot)
library(promises)
library(future)
library(magrittr)
library(emayili)
library(ipc)

#Important
# Remember to use plan(multicore) for deploying on to the server (which usually is a Unix system)
# and use NON-async for putting the app inside the package (so that all users can use it on any machine)
# Remember to change the location of Text of Navbarpage and add the "Home" action link.
# Remember to add required UI and Server codes to the local/package version of the app for generation of Windows app
# Add the "shiny_bookmarks" folder to the main folder of the app

###*********************************************************###

# Selecet the desired template (based on https://github.com/RinteRface/RinteRfaceVerse)

devtools::install_github("RinteRface/RinteRfaceVerse")
library(RinteRfaceVerse)

previewTemplate(lib = "shinydashboardPlus")
previewTemplate(lib = "bs4Dash")
previewTemplate(lib = "argonDash")
previewTemplate(lib = "tablerDash")

useTemplate(path = getwd(), lib = "shinydashboardPlus")
