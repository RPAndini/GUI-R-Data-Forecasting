install.packages("shiny")
install.packages("DT")
install.packages("shinydashboard")
install.packages("shinythemes")

library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)

ui <- fluidPage(
  theme=shinytheme("cosmo"),
  dashboardPage(
    skin = "purple",
  dashboardHeader(title = "My Forecast"),
      
  dashboardSidebar(
    sidebarMenu(
      menuItem("About Exponential Smoothing", tabName = "about", icon = icon("dashboard")),
      menuItem("Input Data", tabName = "input", icon = icon("th")),
      menuItem("Grafik Holt-Winters Filtering", tabName = "expo", icon = icon("bar-chart-o")),
      menuItem("Forecast", tabName = "prediksi", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "about",
              h2("Exponential Smoothing",style=" 
    font-family: 'Sans-Serif';
    color: Black;
    text-align:center
    "),
              h6("Metode Exponential Smoothing merupakan prosedur perbaikan terus-menerus pada peramalan terhadap objek pengamatan terbaru. Metode peramalan ini menitikberatkan pada penurunan prioritas secara eksponensial pada objek pengamatan yang lebih tua. Dalam exponential smoothing terdapat satu atau lebih parameter smoothing yang ditentukan secara eksplisit, dan hasil ini menentukan bobot yang dikenakan pada nilai observasi. Dengan kata lain, observasi terbaru akan diberikan prioritas lebih tinggi bagi peramalan daripada observasi yang lebih lama. Metode exponential smoothing dibagi lagi berdasarkan menjadi beberapa metode. (Makridakis, 1999)", style="
                 font-family: 'cursive';
                 color: Black;
                 text-align:justify;
                 width: 400;
                 "),
              
              h5("Singel Exponential Smoothing", style="
                 font-family: 'cursive';
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
              
              h6("Juga dikenal sebagai simple exponential smoothing yang digunakan pada peramalan jangka pendek, biasanya hanya 1 bulan ke depan. Model mengasumsikan bahwa data berfluktuasi di sekitar nilai mean yang tetap, tanpa trend atau pola pertumbuhan konsisten", style="
                 font-family: 'cursive';
                 color: Black;
                 text-align:justify;
                 width: 400;
                 "),
              
              h5("Double Exponential Smoothing", style="
                 font-family: 'cursive';
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
              
              h6("Metode ini digunakan ketika data menunjukkan adanya trend. exponential smoothing dengan adanya trend seperti pemulusan sederhana kecuali bahwa dua komponen harus diupdate setiap periode level dan trendnya. Level adalah estimasi yang dimuluskan dari nilai data pada akhir masingmasing periode. Trend adalah estimasi yang dihaluskan dari pertumbuhan rata-rata pada akhir masing-masing periode.", style="
                 font-family: 'cursive';
                 color: Black;
                 text-align:justify;
                 width: 400;
                 "),
              
              h5("Triple Exponential Smoothing", style="
                 font-family: 'cursive';
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
              
              h6("Makridakis, (1999) menerangkan bahwa metode ini digunakan ketika data menunjukan adanya trend dan perilaku musiman Untuk menangani musiman, telah dikembangkan parameter persamaan ketiga yang disebut metode Holtn Winters sesuai dengan nama penemuya. Terdapat dua model Holt-Winters tergantung pada tipe musimannya yaitu Multiplicative seasonal model dan Additive seasonal model. Metode exponential smoothing yang telah dibahas sebelumnya dapat digunakan untuk hampir segala jenis data stasioner atau non stasioner sepanjang data tersebut tidak mengandung faktor musiman. Tetapi bilamana terdapat musiman, metode ini dijadikan cara untuk meramalkan data yang mengandung faktor musiman.", style="
                 font-family: 'cursive';
                 color: Black;
                 text-align:justify;
                 width: 400;
                 "),
              br(), br(),
              
              h6("Reference: (Makridakis, Spyros dan Wheelwright, Steven C., 1999, Metode dan Aplikasi Peramalan. Binarupa Aksara, Jakarta)", style="
                 font-family: 'cursive';
                 color: Black;
                 text-align:left;
                 width: 400;
                 ")
      ),
      
      # Second tab content
      tabItem(tabName = "input",
              fluidPage(
              sidebarLayout(
                sidebarPanel(
                  fileInput("file", "Input File CSV",
                            accept = c(
                              "text/csv/xlsx",
                              "text/comma-separated-values,text/plain",
                              ".csv","xlsx")
                  ),
                  radioButtons("pemisah", "Separator",
                               choices = c(Comma = ",",
                                           Semicolon = ";",
                                           Tab = "\t"),
                               selected = ",", inline = TRUE),
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("TABEL",
                             DTOutput("tabel")),
                    tabPanel("PLOT", plotOutput("plot")),
                    tabPanel("SUMMARY",
                             verbatimTextOutput("summary")))),
      ))),
      # Third tab content
      tabItem(tabName = "expo",                
              fluidPage(
                
                    tabsetPanel(
                      tabPanel("SINGEL EXPONENTIAL SMOOTHING",
                               h4("Nilai Fitted Value", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               DTOutput("fitted1"),
                               br(), br(),
                               h4("Grafik Holt-Winters Filtering", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               plotOutput("forecast1")

                      ),
                      
                      tabPanel("DOUBLE EXPONENTIAL SMOOTHING",
                               h4("Nilai Fitted Value", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               DTOutput("fitted2"),
                               br(), br(),
                               h4("Grafik Holt-Winters Filtering", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               plotOutput("forecast2")
     
                      )
                      
                      
                      )
                    )
                  ),
      # Fourth tab content
      tabItem(tabName = "prediksi",                
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    textInput("periode","Periode Forecasting",value="10")
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("SINGEL EXPONENTIAL SMOOTHING",
                               verbatimTextOutput("holt1"),
                               
                               br(),
                               
                               h4("Nilai SSE", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               verbatimTextOutput("SSE1"),
                               
                               br(),
                               
                               h4("Nilai Prediksi", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               verbatimTextOutput("pred1"),
                               
                               br(),
                      
                               h4("Grafik Forecasting", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               plotOutput("chart1")),
                    
                      tabPanel("DOUBLE EXPONENTIAL SMOOTHING",
                               verbatimTextOutput("holt2"),
                               
                               br(),
                               
                               h4("Nilai SSE", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               verbatimTextOutput("SSE2"),
                               
                               br(),
                               
                               h4("Nilai Prediksi", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               verbatimTextOutput("pred2"),
                    
                               br(), br(),
                    
                               h4("Grafik Forecasting", style="
                 font-family: 'Lucida Handwriting',cursive;
                 color: Purple;
                 text-align:justify;
                 width: 400;
                 "),
                               plotOutput("chart2"))
 
                    )
                  )
                )
              )
      )
      
 
      
      
                )
              )
      )
)


server <- function(input, output) {
    
    output$tabel <- renderDT({
      
      data_komstat <- input$file
      
      if (is.null(data_komstat))
        return(NULL)
      
      read.csv(data_komstat$datapath, sep = input$pemisah)
    })
    
    output$plot <-renderPlot({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      ts.plot(Data.ts)
    })
    
    output$summary<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      summary(Data)
    })
    
    output$fitted1<-renderDT({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast1 = HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      forecast1$fitted
      round(forecast1$fitted, 2)
    })
    
      output$fitted2<-renderDT({
        data_komstat <- input$file
        Data=read.csv(data_komstat$datapath, sep = input$pemisah)
        Data.ts=ts(Data)
        forecast2 = HoltWinters(Data.ts, gamma=FALSE)
        forecast2$fitted
        round(forecast2$fitted, 2)
      
    })
    output$forecast1<-renderPlot({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast1 = HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      forecast1$SSE
      plot(forecast1, type="o")
     
    })
    
    output$forecast2<-renderPlot({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast2 = HoltWinters(Data.ts, gamma=FALSE)
      forecast2$SSE
      plot(forecast2, type="o")
      
    })
    
    output$holt1<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      
    })
    
    output$holt2<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      HoltWinters(Data.ts, gamma=FALSE)
      
    })
    
    output$holt3<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      HoltWinters(Data.ts, alpha=NULL, beta=NULL, gamma=NULL)
      
    })
    
    
    output$pred1<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast1 = HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      predict(forecast1, n.ahead=input$periode)
      
    })
    
    output$pred2<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast2 = HoltWinters(Data.ts, gamma=FALSE)
      predict(forecast2, n.ahead=input$periode)
      
    })
    
    
    output$chart1<-renderPlot({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast1 = HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      pred1=predict(forecast1, n.ahead=input$periode)
      plot(Data.ts,main="Grafik Peramalan",lwd=2, col="blue", type="o", pch=1)
      lines(forecast1$fitted[,1],lwd=2, col="red",type="o", pch=12)
      lines(pred1, col="green", type="o", pch=10)
      legend("bottomright", legend=c("Data Aktual","Fitted Value","Peramalan"), col=c("blue","red", "green"), lty=1, cex=0.6, inset=0.02)
    })
      
    
    output$chart2<-renderPlot({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast2 = HoltWinters(Data.ts, gamma=FALSE)
      pred2=predict(forecast2, n.ahead=10)
      plot(Data.ts,main="Grafik Peramalan",lwd=2, col="blue", type="o", pch=15)
      lines(forecast2$fitted[,1],lwd=2, col="red",type="o", pch=12)
      lines(pred2, col="green", type="o", pch=10)
      legend("bottomright", legend=c("Data Aktual","Fitted Value","Peramalan"), col=c("blue","red", "green"), lty=1, cex=0.6, inset=0.02)
    
      
    })
    
    output$SSE1<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast1 = HoltWinters(Data.ts, beta=FALSE, gamma=FALSE)
      SSE1=forecast1$SSE
      SSE1
    })
    
    output$SSE2<-renderPrint({
      data_komstat <- input$file
      Data=read.csv(data_komstat$datapath, sep = input$pemisah)
      Data.ts=ts(Data)
      forecast2 = HoltWinters(Data.ts, gamma=FALSE)
      SSE2=forecast2$SSE
      SSE2
      
    })
    
    
}

shinyApp(ui=ui, server=server)