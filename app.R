library(shiny)
source("bitcoind.R")

### qrencoder is optional pkg for plotting qr codes - not tested on windows
## install using drat # install.packages("drat")
# drat::addRepo("hrbrmstr")
# install.packages("qrencoder")
## install using devtools # install.packages("devtools")
# devtools::install_github("hrbrmstr/qrencoder")

# bitcoin node ------------------------------------------------------------

options(rpchost = "192.168.56.103",
        rpcuser = "bitcoinduser",
        rpcpassword = "userpassbitcoind")

# ui ----------------------------------------------------------------------

ui <- fluidPage(
    fluidRow(column(12L, align="left",
                    titlePanel("shinyPay"),
                    br())),
    fluidRow(column(3L, align="left",
                    actionButton("getnewaddress", label = "get payment address", icon = icon("btc")),br(),
                    br(),
                    strong("payment address"),
                    verbatimTextOutput("pay_address"),
                    br(),
                    numericInput("amount", "amount in BTC", min = 0, max = 100, value = 0.1, step = 0.01),br(),
                    br(),
                    strong("actual rates"),
                    textOutput("ratesrefresh"),
                    tableOutput("rates")),
             column(9L, align="center",
                    plotOutput("pay_qr"),br(),
                    br(),
                    htmlOutput("pay_uri"),br(),
                    br())),
    fluidRow(column(3L, align="left",
                    strong("amount received"),
                    textOutput("confrefresh"),
                    tableOutput("conf")))
)

# server ------------------------------------------------------------------

server <- function(input, output, session){
    
    values <- reactiveValues()
    observe({
        # use only one payment address per session
        if(input$getnewaddress==1L) values$address <- getnewaddress()
    })
    
    address <- reactive({
        validate(need(input$getnewaddress > 0L && !is.null(values$address) && is.character(values$address) && nchar(values$address) > 0L, message = ""))
        isolate(values$address)
    })
    amount <- reactive({
        validate(need(is.numeric(input$amount) && round(input$amount, digits = 8) > 0, message = "Invalid amount"))
        round(input$amount, digits = 8)
    })
    paymenturi <- reactive(makepaymenturi(address(), amount()))
    
    # render payment details
    
    output$pay_address <- renderText(address())
    output$pay_uri <- renderUI(a(paymenturi(), href = paymenturi(), target="_blank"))
    output$pay_qr <- renderPlot({
        validate(need(length(paymenturi()) > 0L, message = ""))
        plotQR(paymenturi())
    })
    
    # monitor received and confirmations (from 0 to 2) - every 10 seconds
    
    refreshreceivedbyaddress <- reactiveTimer(10000, session)
    output$confrefresh <- renderText({
        refreshreceivedbyaddress()
        validate(need(length(address()) > 0L, message = ""))
        as.character(Sys.time())
    })
    output$conf <- renderTable({
        refreshreceivedbyaddress()
        validate(need(length(address()) > 0L, message = ""))
        received <- numeric()
        for(conf in 0:2){
            received <- c(received, getreceivedbyaddress(address(), conf))
            if(received[length(received)]==0) break # do not check for more confirmations if previous didn't arrive yet
        }
        data.frame(confirmations = seq_along(received)-1L, received = received)
    }, digits=c(0,0,8), include.rownames=FALSE)
    
    # use kraken ticker rates to calc fiat value - every 60 seconds
    
    refreshkrakenrates <- reactiveTimer(60000, session)
    rates <- reactive({
        refreshkrakenrates()
        r <- jsonlite::fromJSON("https://api.kraken.com/0/public/Ticker?pair=XXBTZUSD,XXBTZEUR,XXBTZGBP")
        validate(need(length(r$error)==0L && length(r$result) > 0L, message = "Fetching exchcange rates failed"))
        lapply(r$result, function(pair) (as.numeric(pair$a[1L]) + as.numeric(pair$b[1L])) / 2)
    })
    output$ratesrefresh <- renderText({
        refreshkrakenrates()
        as.character(Sys.time())
    })
    output$rates <- renderTable({
        df <- data.frame(currency = substr(names(rates()),6,8), rate = unname(unlist(rates())), value = round(amount() * unname(unlist(rates())),2L))
        df[with(df, order(currency)),]
    }, include.rownames=FALSE)
    
}

# shinyApp ----------------------------------------------------------------

if(!length(getinfo()) > 0L) stop("could not connect to bitcoind rpc")
shinyApp(ui, server)
