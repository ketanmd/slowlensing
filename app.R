# 
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#

library(shiny)
library(tibble)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
    ##h1('ah1'), 'abc', 'aliq', br(), 'asto', strong('apr'),
    ## Application title
    titlePanel("Play with lensing parameters"),
    ##titlePanel("Play with lensing parameters"),
    ##em('foo'),
 
    ## Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput('Lens_RA', 'RA of lens [mas]', value = 10),
            numericInput('Lens_Dec', 'Dec of lens [mas]', value = 10),
            sliderInput('Lens_Mass',
                        'Mass of lens [Solar mass units]',
                        min = 0.001, max = 1, value = 0.001),
            sliderInput('Lens_Distance',
                        'Distance to lens [parsec]',
                        min = 0.001, max = 10, value = 0.001),
            sliderInput('Source_Distance',
                        'Distance to source [parsec]',
                        min = 1, max = 1000, value = 5),
            sliderInput('Lens_Offset',
                        'Offset of lens to source [mas]',
                        min = 0.001, max = 100, value = c(0.001,100))
        ),
        
        ## Show a plot of the generated distribution
        mainPanel(
            textOutput("ering"),
            ##observe({print('Einstein ring = ',
            ##r_e())})),
            plotOutput("lensparplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    spyr <- 3e7 # seconds per year
    masprad <- 206265e3 # milliarcseconds per radian
    arcsprad <- 206265 # arcseconds per radian
    pcpyr   <- 0.307 # light speed [parsecs per year]
    mppc <- 3.086e16 # meters per parsec
    cee <- 3e8 # speed of light [m/s]
    Gee <- 6.67e-11 # grav constant
    m_S <- 2e30 # mass of sun [kg]

    r_S <- 2 * Gee * m_S / cee / cee
    
    RMax = 3600 * (180/pi) * sqrt(2 * r_S / mppc) # [arcsec]

    r_e <- reactive({RMax * sqrt(input$Lens_Mass *
                                   ((1 / input$Lens_Distance) 
                                    - (1/ input$Source_Distance)))})
    output$lensparplot <- renderPlot({
      Jval = 10^seq(log10(input$Lens_Offset[1]), 
                    log10(input$Lens_Offset[2]), length.out = 100)
      indata <- tibble(J = Jval,
                       imagesep = -Jval - (r_e()*r_e()/Jval))
      ggplot(indata) + geom_point(aes(x = J, y = imagesep))
    })
    output$ering <- renderText(paste0("Ering = ",
                                               r_e()))
}

# run the shiny servers
shinyApp(ui, server)

