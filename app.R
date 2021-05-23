library(shiny)
library(vcd)
library(ggplot2)

data("PlantGrowth")
plant <- split(PlantGrowth, PlantGrowth$group)
ctrlplant<- plant[1]
trt1plant<-plant[2]
trt2plant<-plant[3]
plant.data<- data.frame(ctrlplant, trt1plant, trt2plant)
df = subset(plant.data, select = -c(ctrl.group,trt1.group,trt2.group) )
plantctrlvstrt1 = subset(df, select = -c(trt2.weight) )
plantctrlvstrt2 = subset(df, select = -c(trt1.weight) )
planttrt1vstrt2 =  subset(df, select = -c(ctrl.weight) )
data("iris")
iris_filtered<- subset(iris, iris$Species!="virginica")
iris_filtered2<- subset(iris, iris$Species!="setosa")
iris_filtered3<- subset(iris, iris$Species!="versicolor")
iris_filtered$Species= iris_filtered$Species[,drop=TRUE]
iris_filtered2$Species= iris_filtered2$Species[,drop=TRUE]
iris_filtered3$Species= iris_filtered3$Species[,drop=TRUE]
ToothGrowth$size <- ifelse(ToothGrowth$len < median(ToothGrowth$len),"small", "big" )
dosevssize=table(ToothGrowth$dose, ToothGrowth$size)
dosevssupp=table(ToothGrowth$dose, ToothGrowth$supp)
suppvssize=table(ToothGrowth$supp, ToothGrowth$size)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("T-test",
             sidebarLayout(
               sidebarPanel(
                 HTML(("<h2>T test</h2>")),
                 "T-test is a hypothesis test used in inferential statistics to find out if there is a difference between the means of the 2 groups.
         Here, the in-built dataset PlantGrowth has been used to compare the yield, measured by dried plant weight between different groups: Control, Treatment 1, and Treatment 2.",
                 hr(),
                 HTML(("<h3>Input</h3>")),
                 radioButtons("ttestchoice", "Choose the data for t-test",
                              choices = c("Control vs Treatment 1" = "ctvst1",
                                          "Control vs Treatment 2" = "ctvst2",
                                          "Treatment 1 vs Treatment 2" = "t1vst2" 
                              )),
                 HTML(("<h3>Summary of the 2 Different Groups</h>")),
                 verbatimTextOutput("tblsummary"),
                 HTML(("<h3>Result of T-test</h3>")),
                 verbatimTextOutput("Result")
                 
               ),
               mainPanel(
                 
                 
                 plotOutput("boxplt"),
                 wellPanel(
                   HTML(("<h4>Histogram for Comparision</h4>")),
                   plotOutput("histm"),
                 ))
               
             )
             ),
    tabPanel("F-Test",
             sidebarLayout(
               sidebarPanel(
                 HTML(("<h2>F test</h2>")),
                 "F-test is a hypothesis test used in inferential statistics to find out if the ratio between the variances of two populations is 1.
         Here, the in-built dataset iris has been used. For this test, only Sepal Width has been taken into consideration since its distribution is roughly normal.",
                 hr(),
                 HTML(("<h3>Summary of the 2 Different Groups</h>")),
                 verbatimTextOutput("irisSumm"),
                 HTML(("<h3>Input</h3>")),
                 
                 radioButtons("fchoice", "Choose the data for F-test",
                              choices = c("Setosa vs Versicolor" = "sevsve",
                                          "Versicolor vs Virginica" = "vevsvi",
                                          "Setosa vs Virginica" = "sevsvi" 
                              )),
                 
                 
                 HTML(("<h3>Result of F-test</h3>")),
                 verbatimTextOutput("irisResult")
               ),
               mainPanel(
                 
                 HTML(("<h3>Comparision of the 2 Species</h3>")),  
                 plotOutput("bxxpltt"),
                 wellPanel(
                   HTML(("<h3>Side-by-side Histograms</h3>")), 
                   plotOutput("phist1"))
                 
               ))
             ),
    tabPanel("Chi-Square Test of Independance",
             sidebarLayout(
               sidebarPanel(
                 HTML(("<h2>Chi-Square test of Independance</h2>")),
                 "The Chi-Square test of Independance is a hypothesis test used in inferential statistics to find out if there is any relationship between different parameters.
         Here, the in-built dataset ToothGrowth has been used. For this test, the growth, i.e, size increase as well as other parameters have been tabulated to see their relationship. In this test, Null Hypothesis is the hypothesis which assumes independance.",
                 hr(),
                 
                 HTML(("<h3>Input</h3>")),
                 radioButtons("chichoice", "Choose the data for Chi-Square Test of Independance",
                              choices = c("Dose vs Size" = "dovssi",
                                          "Dose vs Supplement" = "dovssu",
                                          "Supplement vs Size" = "suvssi" 
                              )),
                 
                 HTML(("<h3>Mosaic Plot</h3>"),
                      HTML("<i> The Mosaic plot has been used to visualize data present in a Contingency table, and its height represents proportion value</i>")),
                 plotOutput("mosaicplt")
                 
               ),
               mainPanel(
                 
                 HTML(("<h3>Bar Plot")), 
                 plotOutput("barplt"),
                 wellPanel(
                   HTML(("<h3>Result of Chi-Square test</h3>")),
                   verbatimTextOutput("chiresult"))
                 
                 
               )
             )
             )
  )
)

server <- function(input, output) {
  output$bxxpltt<-renderPlot({
    if(input$fchoice== "sevsve")
    {
      boxplot(iris_filtered$Sepal.Width~iris_filtered$Species,data=iris_filtered, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered$Sepal.Width~iris_filtered$Species,method="jitter",jitter=.05,vertical=T,add=T) 
    }
    else if(input$fchoice== "vevsvi")
    {
      boxplot(iris_filtered2$Sepal.Width~iris_filtered2$Species,data=iris_filtered2, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered2$Sepal.Width~iris_filtered2$Species,method="jitter",jitter=.05,vertical=T,add=T) 
    }
    else
    {
      boxplot(iris_filtered3$Sepal.Width~iris_filtered3$Species,data=iris_filtered3, main="Sepal Width Data",
              xlab="Species Name", ylab="Width")
      stripchart(iris_filtered3$Sepal.Width~iris_filtered3$Species,method="jitter",jitter=.05,vertical=T,add=T)
    }
    
  })
  output$phist1<-renderPlot({
    if(input$fchoice== "sevsve")
    {
      par(mfrow=c(1,2))
      hist(iris_filtered$Sepal.Width[iris_filtered$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
      hist(iris_filtered$Sepal.Width[iris_filtered$Species=="versicolor"],main = "Sepal Width Data",xlab="Versicolor", ylab = "Width" )
    }
    else if(input$fchoice== "vevsvi")
    {
      par(mfrow=c(1,2))
      hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="versicolor"], main = "Sepal Width Data", xlab = "Versicolor", ylab = "Width" )
      hist(iris_filtered2$Sepal.Width[iris_filtered2$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )
      
    }
    else
    {
      par(mfrow=c(1,2))
      hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="setosa"], main = "Sepal Width Data", xlab = "Setosa", ylab = "Width" )
      hist(iris_filtered3$Sepal.Width[iris_filtered3$Species=="virginica"],main = "Sepal Width Data",xlab="Virginica", ylab = "Width" )
    }
    
  })
  fchoice <- reactive({switch(input$fchoice,
                              sevsve = iris_filtered,
                              vevsvi = iris_filtered2,
                              sevsvi = iris_filtered3,
                              iris_filtered)})
  output$irisSumm <- renderPrint({str(fchoice())})
  output$irisResult<- renderPrint({
    if(input$fchoice== "sevsve")
    {
      var.test(iris_filtered$Sepal.Width ~ iris_filtered$Species, data = iris_filtered)
    }
    else if(input$fchoice== "vevsvi")
    {
      var.test(iris_filtered2$Sepal.Width ~ iris_filtered2$Species, data = iris_filtered2)
    }
    else
    {
      var.test(iris_filtered3$Sepal.Width ~ iris_filtered3$Species, data = iris_filtered3)
    }
  })
  ttestchoice <- reactive({switch(input$ttestchoice,
                                  ctvst1 = plantctrlvstrt1,
                                  t1vst2 = planttrt1vstrt2,
                                  ctvst2 = plantctrlvstrt2,
                                  plantctrlvstrt1)})
  output$boxplt <- renderPlot({ 
    
    
    
    boxplot(ttestchoice(),main="Comparision Boxplot",ylab="Weight of Dried Plant")
    stripchart(ttestchoice(),method="jitter",jitter=.05,vertical=T,add=T) 
  })
  
  output$histm <- renderPlot({ 
    if(input$ttestchoice == "ctvst1")
    {
      par(mfrow=c(1,2))
      hist(plantctrlvstrt1$ctrl.weight, main="Histogram of Control Weight", xlab="Control Weight")
      hist(plantctrlvstrt1$trt1.weight, main="Histogram of Treatment 1 Weight", xlab="Treatment 1 Weight")
    }
    else if(input$ttestchoice == "ctvst2")
    {
      par(mfrow=c(1,2))
      hist(plantctrlvstrt2$ctrl.weight, main="Histogram of Control Weight", xlab="Control Weight")
      hist(plantctrlvstrt2$trt2.weight, main="Histogram of Treatment 2 Weight", xlab="Treatment 2 Weight") 
    }
    else
    {
      par(mfrow=c(1,2))
      hist(planttrt1vstrt2$trt1.weight, main="Histogram of Treatment 1 Weight", xlab="Treatment 1 Weight")
      hist(planttrt1vstrt2$trt2.weight, main="Histogram of Treatment2 Weight", xlab="Treatment 2 Weight")
    }
  })
  output$tblsummary <- renderPrint({summary(ttestchoice())})
  output$Result <- renderPrint({
    if(input$ttestchoice == "ctvst1")
    {
      with(plantctrlvstrt1, t.test(plantctrlvstrt1$ctrl.weight, plantctrlvstrt1$trt1.weight,equal.var=TRUE) )
    }
    else if(input$ttestchoice == "ctvst2")
    {
      with(plantctrlvstrt2, t.test(plantctrlvstrt2$ctrl.weight, plantctrlvstrt2$trt2.weight,equal.var=TRUE) ) 
    }
    else
    {
      with(planttrt1vstrt2, t.test(planttrt1vstrt2$trt1.weight, planttrt1vstrt2$trt2.weight,equal.var=TRUE) ) 
    }
  })
  output$barplt<-renderPlot({
    if(input$chichoice== "dovssi")
    {
      ggplot(ToothGrowth) +
        aes(x = dose, fill = size) +
        geom_bar()
    }
    else if(input$chichoice== "dovvsu")
    {
      ggplot(ToothGrowth) +
        aes(x = dose, fill = supp) +
        geom_bar()
    }
    else
    {
      ggplot(ToothGrowth) +
        aes(x = supp, fill = size) +
        geom_bar()
    }
    
  })
  output$mosaicplt<-renderPlot({
    if(input$chichoice== "dovssi")
    {
      mosaic(~ dose + size,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
    }
    else if(input$chichoice== "dovssu")
    {
      mosaic(~ dose + supp,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
      
    }
    else
    {
      mosaic(~ supp + size,
             direction = c("v", "h"),
             data = ToothGrowth,
             shade = TRUE
      )
    }
    
  })
  output$chiresult<- renderPrint({
    if(input$chichoice== "dovvsi")
    {
      print(chisq.test(dosevssize))
      "We reject the null hypothesis at 0.05 significance level "
      
    }
    else if(input$chichoice== "dovssu")
    {
      print(chisq.test(dosevssupp))
      "We accept the null hypothesis at 0.05 significance level "
    }
    else
    {
      print(chisq.test(suppvssize))
      "We reject the null hypothesis at 0.05 significance level "
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)