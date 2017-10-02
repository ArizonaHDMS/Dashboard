ui_login <- function(){
  tagList(
    div(id = "login",
        wellPanel(titlePanel('HDMS Dashboard'),
                  textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in"))),
    tags$style(type="text/css", 
               "#login {font-size:12px;   
               text-align: left;
               position:absolute;top: 40%;
               left: 50%;margin-top: -100px;
               margin-left: -150px;}"
    )
  )}