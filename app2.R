library(shiny)
library(shinydashboard)

# Running Dog
img <- 'www/tm_epl.gif'

# Image Size
imgsize <- "auto"

# User Interface
ui <-
  dashboardPage(skin = "black",
                dashboardHeader(title = "Loading Screen"),
                dashboardSidebar(),
                dashboardBody(
                  
                  # Javasript Code
                  singleton(tags$head(HTML("
<script type='text/javascript'>

/* When recalculating starts, show loading screen */
$(document).on('shiny:recalculating', function(event) {
$('div#divLoading').addClass('show');
});

/* When new value or error comes in, hide loading screen */
$(document).on('shiny:value shiny:error', function(event) {
$('div#divLoading').removeClass('show');
});

</script>"))),
                  
                  # CSS Code
                  singleton(tags$head(HTML(paste0("
<style type='text/css'>
#divLoading
{
  display : none;
  }
  #divLoading.show
  {
  display : block;
  position : fixed;
  z-index: 100;
  background-image : url('",img,"');
  background-size:", imgsize, ";
  background-repeat : no-repeat;
  background-position : center;
  left : 0;
  bottom : 0;
  right : 0;
  top : 0;
  }
  #loadinggif.show
  {
  left : 50%;
  top : 50%;
  position : absolute;
  z-index : 101;
  -webkit-transform: translateY(-50%);
  transform: translateY(-50%);
  width: 100%;
  margin-left : -16px;
  margin-top : -16px;
  }
  div.content {
  width : 1000px;
  height : 1000px;
  }
  
</style>")))),
                  
                  # HTML Code   
                  
                  box(tags$body(HTML("<div id='divLoading'> </div>")),
                      plotOutput('plot', width = "800px", height = "450px"),
                      actionButton('goPlot', 'Generate Plot', icon("paper-plane"), 
                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      width = 12, height = 500)
                  
                ))

server <- function(input, output) {
  output$plot <- renderPlot({
    input$goPlot
    Sys.sleep(3)
    plot(iris$Sepal.Length, iris$Petal.Length)
  })
}

runApp(list(ui = ui, server = server), launch.browser =T)