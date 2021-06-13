

library(shiny)
library(miniUI)

ui <- miniPage(
    div(style='background-color:blue;
               display: flex;',
        div(style='background-color:green;
                   width:150px;
            ',
            'left'
        ),
        div(style='background-color:red;
                   flex: 1;
                   white-space: nowrap;
                   overflow: hidden;
                   text-overflow: ellipsis;',
            'middle'
        ),
        div(style='background-color:green;
                   width:150px;',
            'right'
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
