library(shiny)
library(shinyjs)
library(ggplot2)
library(plotly)
library(gridExtra)
library(kableExtra)
library(DT)
library(subscanr)

# Helper function to concat
`%+%` <- function(a, b) paste0(a, b)

bgcolor <- '#E0EEE0'
overview <- get_polkaholic_chains()

# names(daily)



# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    # Application title
    titlePanel("Parachain Health"),

    # overview$chainName
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        div( id ="Sidebar",sidebarPanel(
            checkboxGroupInput("chains", "Select Parachains",
                               overview$chainName, 
                               selected = c("Acala","Astar","Clover","Moonbeam","Parallel")),
        )),

        # Show a plot of the generated distribution
        mainPanel(
          actionButton("toggleSidebar", "Toggle Parachain List"),
          tabsetPanel(type = "tabs",
                      tabPanel("Overview", 
                               h4("Entire history"),
                               tableOutput("overview1"),
                               h4("Most recent 7 days"),
                               tableOutput("overview7"),
                               h4("Most recent 30 days"),
                               tableOutput("overview30")
                      ),
                      tabPanel("Charts", 
                               plotlyOutput("numAddresses"),
                               plotlyOutput("numAccountsActive"),
                               plotlyOutput("numExtrinsics"),
                               plotlyOutput("Events"),
                               plotlyOutput("valueTransfersUSD"),
                               plotlyOutput("transfers"),
                               plotlyOutput("fees"),
                               plotlyOutput("TransactionsEVM"),
                               plotlyOutput("valXCMTransferIncomingUSD"),
                               plotlyOutput("valXCMTransferOutgoingUSD")
                      ),
                      tabPanel("Rawdata", dataTableOutput("rawdata"))
          )
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observeEvent(input$toggleSidebar, {
    shinyjs::toggle(id = "Sidebar")
  })
  
  daily <- reactive({
    
    chains <- unlist(overview[chainName %in% input$chains, id])
    if (length(chains) == 0) chains <- c("kusama","polkadot")
    
    d <- list()
    for (tag in chains) {
      tmp <- get_polkaholic_chainlog(tag)
      tmp$Parachain = tools::toTitleCase(tag)
      tmp[, Date := as.Date(logDT)]
      d[[tag]] <- tmp
    }
    daily = rbindlist(d)
    daily <- daily[, c("Parachain", "Date", "numExtrinsics", "numEvents", "numTransfers", "numSignedExtrinsics",
                       "valueTransfersUSD", "numTransactionsEVM", "numAccountsActive", "numAddresses", "fees",
                       "numXCMTransfersIn", "numXCMMessagesIn", "numXCMTransfersOut", "numXCMMessagesOut",
                       "valXCMTransferIncomingUSD", "valXCMTransferOutgoingUSD")]
    daily
    
  })
  
    myFormat <- function(x, prefix = "", suffix = "") prefix %+% (x %>% as.numeric %>% round(2) %>% format(nsmall=1, big.mark=",")) %+% suffix
    
    newNames <- c("Parachain","Icon","Active Accts","Transfers","Transfers USD","Extrinsics","Incoming XCM USD","Outgoing XCM USD")
    prefix <- c("","","","","","")
    
    output$overview1 <- function() {
      chains <- unlist(overview[chainName %in% input$chains, id])
      if (length(chains) == 0) chains <- c("kusama","polkadot")
      
      overview1 <- overview[id %in% chains, .(chainName, "", numAccountsActive, numTransfers, valueTransfersUSD, numExtrinsics,valXCMTransferIncomingUSD,valXCMTransferOutgoingUSD)]
      setnames(overview1, newNames)
      
      
      
      for (i in 1:6) {
        tag <- names(overview1)[i + 2]
        v <- overview1[[tag]]
        m <- median(v)
        overview1[[tag]] <- cell_spec(myFormat(v, prefix[i]), color = ifelse(v > m, "green", ifelse(v < m, "red", "black")))
      }
      
      kbl(overview1, booktabs = TRUE, format.args = list(big.mark = ","), escape = FALSE, align = c('l','c',rep('r', 6))) %>%
        kable_styling(latex_options = "striped", full_width = TRUE) %>%
        column_spec(1, link = overview[id %in% chains, parachainsURL]) %>%
        column_spec(2, image = spec_image(path = overview[id %in% chains, iconUrl], width = 50, height = 50))
      
    }
    
    output$overview7 <- function() {
      chains <- unlist(overview[chainName %in% input$chains, id])
      if (length(chains) == 0) chains <- c("kusama","polkadot")
      
      overview7 <- overview[id %in% chains, .(chainName, "", numAccountsActive7d, numTransfers7d, valueTransfersUSD7d, numExtrinsics7d, numXCMTransferIncoming7d, numXCMTransferOutgoing7d)]
      setnames(overview7, newNames)
      
      for (i in 1:6) {
        tag <- names(overview7)[i + 2]
        v <- overview7[[tag]]
        m <- median(v)
        overview7[[tag]] <- cell_spec(myFormat(v, prefix[i]), color = ifelse(v > m, "green", ifelse(v < m, "red", "black")))
      }
      
      kbl(overview7, booktabs = TRUE, format.args = list(big.mark = ","), escape = FALSE, align = c('l','c',rep('r', 6))) %>%
        kable_styling(latex_options = "striped", full_width = TRUE) %>%
        column_spec(1, link = overview[id %in% chains, parachainsURL]) %>%
        column_spec(2, image = spec_image(path = overview[id %in% chains, iconUrl], width = 50, height = 50))
    }


    output$overview30 <- function() {
      chains <- unlist(overview[chainName %in% input$chains, id])
      if (length(chains) == 0) chains <- c("kusama","polkadot")
    
      overview30 <- overview[id %in% chains, .(chainName, "", numAccountsActive30d, numTransfers30d, valueTransfersUSD30d, numExtrinsics30d, numXCMTransferIncoming30d, numXCMTransferOutgoing30d)]
        setnames(overview30, newNames)

      for (i in 1:6) {
        tag <- names(overview30)[i + 2]
        v <- overview30[[tag]]
        m <- median(v)
        overview30[[tag]] <- cell_spec(myFormat(v, prefix[i]), color = ifelse(v > m, "green", ifelse(v < m, "red", "black")))
      }
      
      kbl(overview30, booktabs = TRUE, format.args = list(big.mark = ","), escape = FALSE, align = c('l','c',rep('r', 6))) %>%
        kable_styling(latex_options = "striped", full_width = TRUE) %>%
        column_spec(1, link = overview[id %in% chains, parachainsURL]) %>%
        column_spec(2, image = spec_image(path = overview[id %in% chains, iconUrl], width = 50, height = 50))
    }
    
    output$numAddresses <- renderPlotly({
      daily <- daily()
      addr <- daily[!is.na(numAddresses), .(Date, Parachain, numAddresses)]
      addr[, numAddresses := numAddresses / 1e3]
      p <- ggplot(addr, aes(x=Date, y=numAddresses, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Total Addresses over Time", x = "", y = "Addresses (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })
    
    output$numAccountsActive <- renderPlotly({
      daily <- daily()
      active <- daily[!is.na(numAccountsActive), .(Date, Parachain, numAccountsActive)]
      active[, numAccountsActive := numAccountsActive / 1e3]
      p <- ggplot(active, aes(x=Date, y=numAccountsActive, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Daily Active Accounts", x = "", y = "Active Accounts (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })
    
    output$transfers <- renderPlotly({
      daily <- daily()
      transfers <- daily[!is.na(numTransfers), .(Date, Parachain, numTransfers)]
      transfers[, numTransfers := numTransfers / 1e3]
      p <- ggplot(transfers, aes(x=Date, y=numTransfers, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Number of Transfers over Time", x = "", y = "Transfers (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$valueTransfersUSD <- renderPlotly({
      daily <- daily()
      valueTransfersUSD <- daily[!is.na(valueTransfersUSD), .(Date, Parachain, valueTransfersUSD)]
      valueTransfersUSD[, valueTransfersUSD := valueTransfersUSD / 1e3]
      p <- ggplot(valueTransfersUSD, aes(x=Date, y=valueTransfersUSD, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Value of Transfers in USD over Time", x = "", y = "Value of Transfers in USD (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$Events <- renderPlotly({
      daily <- daily()
      Events <- daily[!is.na(numEvents), .(Date, Parachain, numEvents)]
      Events[, numEvents := numEvents / 1e3]
      p <- ggplot(Events, aes(x=Date, y=numEvents, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Number of Events over Time", x = "", y = "Events (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$numExtrinsics <- renderPlotly({
      daily <- daily()
      numExtrinsics <- daily[!is.na(numExtrinsics), .(Date, Parachain, numExtrinsics)]
      numExtrinsics[, numExtrinsics := numExtrinsics / 1e3]
      p <- ggplot(numExtrinsics, aes(x=Date, y=numExtrinsics, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Number of Extrinsics over Time", x = "", y = "Extrinsics (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$TransactionsEVM <- renderPlotly({
      daily <- daily()
      TransactionsEVM <- daily[!is.na(numTransactionsEVM), .(Date, Parachain, numTransactionsEVM)]
      TransactionsEVM[, numTransactionsEVM := numTransactionsEVM / 1e3]
      p <- ggplot(TransactionsEVM, aes(x=Date, y=numTransactionsEVM, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Number of EVM Transactions over Time", x = "", y = "EVM Transactions (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })
      
    output$fees <- renderPlotly({
      daily <- daily()
      fees <- daily[!is.na(fees), .(Date, Parachain, fees)]
      fees[, fees := as.numeric(fees)]
      p <- ggplot(fees, aes(x=Date, y=fees, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Total Fees over Time", x = "", y = "Fees (in native tokens)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$valXCMTransferIncomingUSD <- renderPlotly({
      daily <- daily()
      valXCMTransferIncomingUSD <- daily[!is.na(valXCMTransferIncomingUSD), .(Date, Parachain, valXCMTransferIncomingUSD)]
      valXCMTransferIncomingUSD[, valXCMTransferIncomingUSD := valXCMTransferIncomingUSD / 1e3]
      p <- ggplot(valXCMTransferIncomingUSD, aes(x=Date, y=valXCMTransferIncomingUSD, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Value Incoming XCM Transfer USD over Time", x = "", y = "Value Incoming XCM Transfer USD (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p)
    })

    output$valXCMTransferOutgoingUSD <- renderPlotly({
      daily <- daily()
      valXCMTransferOutgoingUSD <- daily[!is.na(valXCMTransferOutgoingUSD), .(Date, Parachain, valXCMTransferOutgoingUSD)]
      valXCMTransferOutgoingUSD[, valXCMTransferOutgoingUSD := valXCMTransferOutgoingUSD / 1e3]
      p2 <- ggplot(valXCMTransferOutgoingUSD, aes(x=Date, y=valXCMTransferOutgoingUSD, col=Parachain)) +
        geom_line() + 
        theme(strip.background = element_blank(), strip.placement = "outside") +
        labs(title="Value Outgoing XCM Transfer USD over Time", x = "", y = "Value Outgoing XCM Transfer USD (in thousands)") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(panel.background = element_rect(fill=bgcolor),
              panel.border = element_blank())
      ggplotly(p2)
    })

    output$rawdata <- renderDataTable(daily(), options = list(pageLength = 10))

}

# Run the application 
shinyApp(ui = ui, server = server)
