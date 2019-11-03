#Comments were added for section unserstanding puerposes by Andres Torres.

#Libraries required, pre-loads them
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

# Sign in screen
signinpage <- div(id = "signinpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("SIGN IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("frstName", placeholder="First Name", label = "First Name"),
                     textInput("lstName1", placeholder="Last Name 1", label = "Last Name 1"),
                     textInput("lstName", placeholder="Last Name 2", label = "Last Name 2"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "LOG IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         actionLink("signin", "SIGN IN", style = "color: #3c8dbc;;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         tags$code("Username: myuser  Password: mypass"),
                         br(),
                         tags$code("Username: myuser1  Password: mypass1")
                     ))
)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "LOG IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         actionLink("signin", "SIGN IN", style = "color: #3c8dbc;;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center"))),
                         br(),
                         br(),
                         tags$code("Username: myuser  Password: mypass"),
                         br(),
                         tags$code("Username: myuser1  Password: mypass1")
                     ))
)

#Data frame that contains the users of this page
credentials = data.frame(
    username_id = c("myuser", "myuser1"),
    passod   = sapply(c("mypass", "mypass1"),password_store),
    permission  = c("basic", "advanced"), 
    stringsAsFactors = F
)

#Contains dashboard header items that will be shown in UI
header <- dashboardHeader( title = "Outcome Project", uiOutput("logoutbtn"))

#Contains dshboard sidebar items that will be shown in UI
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))

#Contains dashboard body items that will be shown in UI
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))

#Contains the dashboard page implementing everything inside
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    
    #Server variables section
    login = FALSE
    USER <- reactiveValues(login = login)
    
    #The function that determines when an event has occured
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    
                    #Displays error message if one of the credentials is wrong
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } 
                        else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } 
                    
                    else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }    
    })
    
    #This section will display the UI if there is a login session active.
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout",
                #This JS will reload the page and will set login to FALSE again.
                #NOTE: Refreshing page will cause the login session to be lost. (LOG IN WILL BE REQUIRED)
                href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    #This section will display the UI if there is a login session active.
    #It will also determine permission levels for users.
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                sidebarMenu(
                    menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
                    menuItem("About Page", tabName = "About", icon = icon("th"))
                )
            }
            
            #If not set to advanced displays basic permision layout in UI
            else{
                sidebarMenu(
                    menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard"))
                )
                
            }
        }
    })
    
    #This section will display the UI if there is a login session active.
    #It will also determine permission levels for users.
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            
            #Checks if the permission level for the user is set to advanced and displays UI
            if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
                tabItems(
                    tabItem(
                        tabName ="dashboard", class = "active",
                        fluidRow(
                            box(width = 12, dataTableOutput('results'))
                        ))
                    ,
                    tabItem(
                        tabName ="About",
                        h2("This is second tab")
                    )
                )
            }
            
            #If not set to advanced displays basic permision layout in UI
            else {
                tabItem(
                    tabName ="dashboard", class = "active",
                    fluidRow(
                        box(width = 12, dataTableOutput('results'))
                    ))
                
            }
            
        }
        else {
            loginpage
        }
    })
    
    output$results <-  DT::renderDataTable({
        datatable(iris, options = list(autoWidth = TRUE,
                                       searching = FALSE))
    })
    
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
