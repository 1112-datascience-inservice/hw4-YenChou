library(shiny)
library(ggbiplot)
library(FactoMineR)
library(ggcorrplot)
pca_vars <- c("PCA1", "PCA2", "PCA3", "PCA4")
ui <- fluidPage(
  titlePanel("HW4 111971003 周正晏"),
  sidebarPanel(
    sliderInput(inputId = "num",
                label = "Choose how many input to do PCA",
                min = 6, max = nrow(iris), value = nrow(iris)),
    selectInput('xcol', 'X Variable', pca_vars),
    selectInput('ycol', 'Y Variable', pca_vars, selected = pca_vars[[2]])
    
  ),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "PCA Plot",
        h3("PCA plot"),
        plotOutput("pca"),
        h3("Summary"),
        verbatimTextOutput("pcasummary"),
      ),
      tabPanel(
        title = "CA",
        h3("CA plot"),
        plotOutput("ca"),
        h3("Summary"),
        verbatimTextOutput("casummary")
      ),
      tabPanel(
        title = "Dataset",
        h3("Correlation"),
        plotOutput("correlation"),
        h3("Summary of row data"),
        verbatimTextOutput("summary"),
        h3("Dataset"),
        DT::dataTableOutput("iris")
      ),
    )
  ),
)

server <- function(input, output) {
  # 處理iris資料
  set.seed(123)
  # A reactive subset of iris
  my_iris <- reactive({iris[sample(nrow(iris), input$num),]})
  ir.pca <- reactive({prcomp(log(iris[sample(nrow(iris), input$num),][, 1:4]),center = TRUE, scale. = TRUE)})
  iris_ca <- reactive({
    CA(iris[sample(nrow(iris), input$num),][, 1:4], graph = FALSE)
  })
  # Summary of data
  output$summary <- renderPrint({
    summary(my_iris())
  })
  # Data set
  output$iris <- DT::renderDataTable({
    DT::datatable(my_iris(), rownames = FALSE, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  })
  output$correlation <- renderPlot({
    ir.species <- my_iris()[,1:4]
    corr_matrix <- round(cor(iris_num), 2)
    p.mat <- cor_pmat(corr_matrix)
    ggcorrplot(corr_matrix, hc.order = TRUE, lab = TRUE, p.mat = p.mat, colors = c("#6D9EC1", "white", "#E46726"),method = "square")
  })
  # PCA
  output$pca <- renderPlot({
    log.ir <- log(my_iris()[, 1:4])
    ir.species <- my_iris()[, 5]
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    xcol <- 1
    ycol <- 2
    if (input$xcol == "PCA1") {
      xcol = 1
    } else if (input$xcol == "PCA2") {
      xcol = 2
    } else if (input$xcol == "PCA3") {
      xcol = 3
    } else if (input$xcol == "PCA4") {
      xcol = 4
    }
    if (input$ycol == "PCA1") {
      ycol = 1
    } else if (input$ycol == "PCA2") {
      ycol = 2
    } else if (input$ycol == "PCA3") {
      ycol = 3
    } else if (input$ycol == "PCA4") {
      ycol = 4
    }
    ggbiplot(ir.pca, choices = c(xcol, ycol), obs.scale = 1, var.scale = 1, groups = ir.species, circle = TRUE) + scale_color_discrete(name = '') + theme(legend.direction = 'horizontal', legend.position = 'top')
  })
  output$pcasummary <- renderPrint({
    summary(ir.pca())
  })
  output$pcascreeplot <- renderPlot({
    plot(ir.pca(),
         type="line",
         main="Scree Plot for iris")
    abline(h=1, col="blue") 
  })
  # CA
  output$ca <- renderPlot({
    iris_cat <- my_iris()[,5]
    iris_num <- my_iris()[,1:4]
    ca_res <- CA(iris_num, graph = TRUE)
  })
  output$casummary <- renderPrint({
    summary(iris_ca())
  })
  
}

shinyApp(ui = ui, server = server)
