#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Ying Bao (April 2023): This application will produce an interactive 2D perceptual map.
# You can click on the map to move around a product.  The product's predicted share will be shown on the bottom
# right corner of the plot.  There are two drop-down menus.  The first allows you to change a product, and the
# second allows you to select a choice rule.

library(shiny)

d = read.csv("perceptual_data.csv",header=TRUE,row.names=1,fileEncoding="UTF-8-BOM")

r = read.csv("ratings_data.csv",header=TRUE,row.names=1,fileEncoding="UTF-8-BOM")

r.t = t(r)

source("joint_space_map.r")

ui <- fluidPage(

    # Application title
    titlePanel("Interactive Perceptual Map"),

    # Drop down menus
    selectInput("product", "Product to Reposition:",
                rownames(d)),
    selectInput("choicerule", "Choice Rule:",
                c("First Choice"="fc","Share of Preference"="sp")),

    plotOutput("plot1", click = "plot_click", height = "100%")

)

server <- function(input, output) {

    click_saved <- reactiveValues(singleclick = NULL)
    
    observeEvent(eventExpr = input$plot_click, handlerExpr = { click_saved$singleclick <- input$plot_click })
    
    rv=reactiveValues(m=data.frame(x=0,y=0))
    
    output$plot1 <- renderPlot({
       
        out <- Joint_Space_Map(d,r.t,cex=1)
        points(x=click_saved$singleclick$x, y=click_saved$singleclick$y, pch=19, col="red")
        
        # compute predicted shares
        
        brdnames = rownames(d)
        
        nresp = nrow(r.t)
        
        pbrand.share = 0
        
        p.ind = which(brdnames == input$product)
        
        p.util = out$p.util
        
        if(!is.null(click_saved$singleclick)) {
            arrows(x0=out$coords[p.ind,1], y0=out$coords[p.ind,2], 
                   x1=click_saved$singleclick$x, y1=click_saved$singleclick$y, length = 0.1)
            text(x=click_saved$singleclick$x, y=click_saved$singleclick$y, 
                 labels=paste("Repositioned",brdnames[p.ind]),col="black", cex= 0.75, font=2, pos=4)
        }
        
        for(i in 1:nresp) {
            
            if(!is.null(click_saved$singleclick)) {
                p.util[i,p.ind] = out$p.coefs[i,1] + out$p.coefs[i,2]*click_saved$singleclick$x + 
                    out$p.coefs[i,3]*click_saved$singleclick$y
            }
            
            if(input$choicerule == "fc") {
                pbrand.share = pbrand.share + as.numeric(which.max(p.util[i,])==p.ind)
            } else {
                pbrand.share = pbrand.share + exp(p.util[i,p.ind])/sum(exp(p.util[i,]))
            }
        }
        
        pbrand.share = pbrand.share/nresp
        
        text(x=0.75,y=-1.25,labels = paste("Predicted Market Share: ",round(pbrand.share*100,2),"%",sep=""))
        
    }, height = 600)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
