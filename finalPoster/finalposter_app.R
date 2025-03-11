#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plm)
library(gplots)
library(tidyverse)
library(dbscan)
library(fpc)
library(plotly)
library(stargazer)


#data handling
filtered_df = read.csv(file="app_data.csv")

filtered_df = filtered_df%>%
  mutate(Date=as.Date(Date), County.Code = as.factor(County.Code))

cluster_df = scale(filtered_df[,sapply(filtered_df, is.numeric)])


countyLookup = filtered_df%>%
  group_by(County.Code)%>%
  summarise(name = unique(county.Name))

initPlot_choices = c("Traffic Volume" = "Current.Traffic",
                     "Change in Traffic Volume" = "Delta.Traffic",
                    "Truck Volume" = "Current.Truck",
                    "Change in Truck Volume" = "Delta.Truck")
meanPlot_choices = c("Air Quality Index" = "AQI",
                     "Traffic Volume" = "Current.Traffic",
                     "Change in Traffic Volume" = "Delta.Traffic",
                     "Truck Volume" = "Current.Truck",
                     "Change in Truck Volume" = "Delta.Truck")

ui <- fluidPage(

    # Application title, reference: https://stackoverflow.com/questions/65587869/r-shiny-how-to-box-a-simple-text-on-a-shiny-page
    titlePanel(
      tags$div("Air Quality in Pennsylvania, 2018 - 2023",
               style = "font-family: 'Times'; font-weight: bold; font-size: 28px; text-align: center;")),
    
    img(src='car-air-pollution-traffic.jpg', align='center', height="25%"),
    
    # Main panel, reference: ChatGPT
    fluidRow(
      column(12,
             h1('I. Introduction',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold;"),
             p(HTML("Air quality is a critical factor influencing public health and 
           environmental sustainability, particularly in regions with significant 
           traffic congestion (National Institute of Environmental Health Sciences, 2025). 
           <span style='background-color: yellow;'>This project investigates the relationship between air quality and 
        traffic volume across counties in Pennsylvania from 2018 to 2023 
        using panel regression among other techniques.</span>"), 
               style = "font-family: 'Times'; font-size: 18px;"),
             
             p('Air quality data was collected from the US EPA and defined as the Air Quality Index or AQI
           (US EPA, 2024). Traffic volume data was collected from PennDOT\'s Open Data portal
           (PennDOT, 2025).', 
               style = "font-family: 'Times'; font-size: 18px;"),
             p('The findings of this study can inform policymakers and urban planners 
           in designing targeted interventions to mitigate air pollution, improve 
           public health, and enhance transportation policies in Pennsylvania.', 
               style = "font-family: 'Times'; font-size: 18px;"),
             h1('II. Research Questions',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold;"),
             tags$ol(
               tags$li('What is the relationship between air quality and traffic volume in 
                       Pennsylvania counties from 2018 to 2023?'),
               tags$li('How does this relationship change if we consider innate differences
                       between counties and dates?'),
               tags$li('Are there any interesting, latent trends in the air quality-traffic volume dataset?'),
               style = "font-family: 'Times'; font-size: 18px;"
             ),
             h1('III. Methodology',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold;"),
             h2('a. Ordinary Least Square:',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold; font-style: italic;"),
             p('An ordinary least square model will show a significant relationship between traffic  volume and change in traffic volume 
              (difference between current and historical vehicle count on a street) with AQI. However, the coefficients are positive for the former and 
              negative for the latter, leading to confusing interpretation. The scatter plot also shows intense outliers for both traffic and air quality variables.',
               style = "font-family: 'Times'; font-size: 18px;")
      )
    ),
    
    sidebarLayout(
        sidebarPanel(
          selectInput("var", "Select a variable:",
                      choices = initPlot_choices,
                      selected = "Current.Traffic"),
          htmlOutput(outputId = "olsModel"),
          tags$head(tags$style("#olsModel{font-size:75%;}"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("initPlot"),
          p(textOutput(" ")),
          plotOutput("histPlot")        
          )
    ),
    
    fluidRow(
      column(12,
             h2('b. Panel Effects in the Data:',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold; font-style: italic;"),
             p('The plot demonstrates differences in means across counties over time, for which the previous model does not control. A 
              random-effects panel regression model found no relationship
               between traffic volume and AQI, which is not necessary a bad finding! Because this analysis is constrained within specific 
               Pennsylvania counties on specific dates in time, it is not as robust in capturing the relationship between traffic volume and 
               air quality. It might also speak to the effectiveness of air quality monitoring efforts and car emission controls in 
               Pennsylvania.',
               style = "font-family: 'Times'; font-size: 18px;")
      )
    ),
    
    
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "Select a county:",
                    choices = countyLookup$name),
        selectInput("varMeans", "Select a variable:",
                    choices = meanPlot_choices,
                    selected = "AQI"),
        htmlOutput(outputId = "randomModel"),
        tags$head(tags$style("#randomModel{font-size:85%;}"))
        
      ),
      
      mainPanel(
        plotOutput("meansPlot"),
        # plotOutput("perCounty")
      )
    ),

    fluidRow(
      column(12,
         h2('c. DBSCAN Clustering:',
            style = "font-family: 'Times'; font-size: 18px; font-weight: bold; font-style: italic;"),
         p('DBSCAN stands for density-based spatial clustering of applications with noise, which not 
           only identifies outliers in hyper-dimensional planes but also does not assume that the data clusters by 
           spherical shapes (Yenigun, 2024). We first plot a k-NN distance plot to determine the best epsilon 
           hyperparameter (the elbow of the plot) for a chosen k nearest neighbors. Despite hypertuning,
           DBSCAN insists on two clusters with cluster 0 being outliers in hyper-dimensional planes. Among cluster
           0, we typically see two trends: high AQI, low traffic volume and low AQI, (mostly) high traffic volume. This 
           inverse relationship contradicts common understand of the relationship between air quality and car exhaust 
           fumes; thus, it is reasonable for them to be "outliers" compared to other points.',
           style = "font-family: 'Times'; font-size: 18px;")
      )
    ),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        sliderInput("minPts","Choose the minimum number of points in a cluster:", min=20, max=30, value=24, step=1),
        sliderInput("eps","Choose an epsilon:", min=0, max=7, value=3.4, step=0.1),
        plotOutput("distPlot")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tags$style(type = "text/css", "#dbCluster {height: 90vh !important;}"),
        plotlyOutput("dbCluster", height="90vh")
      )
    ),
    
    tags$head(
      tags$style(HTML("
      .hanging-indent {
        padding-left: 40px;
        text-indent: -40px;
      }
    "))
    ),
    
    fluidRow(
      column(12,
             h1('IV. Conclusion:',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold;"),
             p('There is simply not enough evidence to affirm a relationship between traffic volume and air quality in specific 
               Pennsylvania counties from April 2018 to December 2023. This is not to refuse robust, scientific research that links 
               air pollution to car exhaust fumes (Buckeridge et al., 2002). However, it is supportive evidence that Pennsylvania\'s 
               environmental policies, air quality monitoring efforts, and stringent exhaust controls might have played a role in providing 
               clean air for residents (Benthem et al., 2022).',
               style = "font-family: 'Times'; font-size: 18px;"),
             p('This analysis can be improved through better traffic volume data, specifically county-level traffic volume aggregated from all streets in a county. 
             I also believe there are gaps in the data between April 2018 and December 2023. Not all dates were represented, which might have impacted my coefficients.', 
               style = "font-family: 'Times'; font-size: 18px;"),
             h1('V. References',
                style = "font-family: 'Times'; font-size: 18px; font-weight: bold;"),
             
             p(HTML("Benthem, A. van, Jacobsen, M., Sallee, J., & Shapiro, J. (2022, December 7). How Effective Are Vehicle Exhaust Standards? 
             <em>Kleinman Center for Energy Policy</em>. Retrieved February 21, 2025, from 
             <a href='https://kleinmanenergy.upenn.edu/research/publications/how-effective-are-vehicle-exhaust-standards/' target='_blank'>
                    https://kleinmanenergy.upenn.edu/research/publications/how-effective-are-vehicle-exhaust-standards/</a>"), 
             style = "font-family: 'Times'; font-size: 18px; margin-left: 40px; text-indent: -40px;"),
             
             p(HTML("Buckeridge, D. L., Glazier, R., Harvey, B. J., Escobar, M., Amrhein, C., & Frank, J. (2002). 
             Effect of motor vehicle emissions on respiratory health in an urban area. <em>Environmental Health 
                    Perspectives, 110</em>(3), 293<span>&#8211;</span>300."), 
             style = "font-family: 'Times'; font-size: 18px; margin-left: 40px; text-indent: -40px;"),
             
             p(HTML("National Institute of Environmental Health Sciences. (2025, February 26). 
               <em>Air Pollution and Your Health.</em> National Institute of Environmental Health Sciences. 
               <a href='https://www.niehs.nih.gov/health/topics/agents/air-pollution' target='_blank'>
                    https://www.niehs.nih.gov/health/topics/agents/air-pollution </a>" ),
                style = "font-family: 'Times'; font-size: 18px; padding-left: 40px; text-indent:-40px;"),
             
             p(HTML("plherrera. (2009, January 19). <em>Cars at Rush Hour Driving Through Thick Smog</em> [Photograph]. Getty Images. Retrieved February 26, 2025, 
             from <a href='https://www.gettyimages.com/detail/photo/cars-at-rush-hour-driving-through-thick-smog-royalty-free-image/174655376?adppopup=true' 
                    target='_blank'> https://www.gettyimages.com/detail/photo/cars-at-rush-hour-driving-through-thick-smog-royalty-free-image/174655376?adppopup=true </a>"), 
               style = "font-family: 'Times'; font-size: 18px; margin-left: 40px; text-indent: -40px;"),
             
             p(HTML("PennDOT. (2025, January 21). <em>RMSTRAFFIC (Traffic Volumes)</em> [Data & Tools]. PennShare. 
               <a href='https://data-pennshare.opendata.arcgis.com/datasets/a17c20bf71dd40fea24363bb9f0ae0e4_0/about' target='_blank'>
               https://data-pennshare.opendata.arcgis.com/datasets/a17c20bf71dd40fea24363bb9f0ae0e4_0/about</a>"), 
             style = "font-family: 'Times'; font-size: 18px; margin-left: 40px; text-indent: -40px"),
             
             p(HTML("US EPA. (2024, November 19). <em>Pre-Generated Data Files</em> [Data & Tools]. 
               Air Data. <a href='https://aqs.epa.gov/aqsweb/airdata/download_files.html#AQI' target= '_blank'>
                    https://aqs.epa.gov/aqsweb/airdata/download_files.html#AQI </a>"),
               style = "font-family: 'Times'; font-size: 18px; padding-left: 40px; text-indent:-40px;"),
             
             p(HTML("Yenigun, O. (2024, March 11). <em>DBSCAN Clustering Algorithm Demystified</em>. Built In. Retrieved February 21, 2025, from
             <a href='https://builtin.com/articles/dbscan' target='_blank'> https://builtin.com/articles/dbscan</a>"), 
             style = "font-family: 'Times'; font-size: 18px; margin-left: 40px; text-indent: -40px;")
             
      )
    )

)

server <- function(input, output) {
  output$initPlot = renderPlot ({
    ggplot(data=filtered_df, 
           mapping=aes_string(x=input$var, y="AQI"))+
      geom_point()+
      geom_smooth()+
      labs(x=names(initPlot_choices)[initPlot_choices==input$var], 
           title = paste0("Scatter Plot between AQI and ", names(initPlot_choices)[initPlot_choices==input$var]))+
      theme(plot.title = element_text(face = "bold")) 
  })
  
  output$olsModel = renderUI ({
    ols_revised <- lm(AQI ~ daily.Avg.precipitation + daily.Avg.temp + 
                        Current.Traffic + Delta.Traffic + factor(Category) + 
                        factor(Defining.Parameter) +factor(county.Name), data=filtered_df)
    
    HTML(stargazer(ols_revised, type="html", header=FALSE, single.row=TRUE, title="Ordinary Least Square"))
  })
  
  output$histPlot = renderPlot({
    outlier_cutoff = quantile(filtered_df[[input$var]],
                              0.75) + 1.5 * IQR(filtered_df[[input$var]])
    
    ggplot(data=filtered_df[-which(filtered_df[[input$var]]>outlier_cutoff),],
           mapping=aes_string(x=input$var, color="Category", fill="Category"))+
      geom_histogram(bins=20, aes(alpha=0.7))+
      scale_fill_manual(values = c("#007f00", "#66b266", "#ffff66", "#ff9933", "#cc0000")) +  # Green to Red
      scale_color_manual(values = c("#007f00", "#66b266", "#ffff66", "#ff9933", "#cc0000")) +
      labs(title= paste0("Histogram of ", names(initPlot_choices[initPlot_choices==input$var])), 
           subtitle="By Air Quality Categories, Outliers Removed",
           x= names(initPlot_choices[initPlot_choices==input$var]), y="Absolute Frequency", fill = "Air Quality", color = "Air Quality")+
      theme(
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        plot.title = element_text(face="bold")
        )
  })
  
  output$meansPlot = renderPlot({
    plotmeans(as.formula(paste(input$varMeans, "~ County.Code")), main="Heterogeineity across Counties", data=filtered_df,
              xlab="County Code", ylab = names(meanPlot_choices[meanPlot_choices == input$varMeans]), n.label=FALSE)
    abline(v = which(levels(filtered_df$County.Code) == countyLookup$County.Code[countyLookup$name == input$county]), col = "#cc0000")
  })
  
  # output$perCounty = renderPlot ({
  #   ggplot(data=subset(filtered_df, county.Name==input$county), mapping = aes_string(y=input$varMeans, x="Date"))+
  #     geom_point()+
  #     geom_smooth()+
  #     labs(y=names(meanPlot_choices)[meanPlot_choices==input$varMeans], 
  #          title = paste0(names(meanPlot_choices)[meanPlot_choices==input$varMeans], " over Time"),
  #          subtitle = paste0("In ", input$county, "County"))+
  #     theme(plot.title = element_text(face = "bold")) 
  # })
  
  output$randomModel = renderUI ({
    random <- plm(AQI ~ Current.Traffic+Current.Truck+Delta.Traffic+Delta.Truck,
                  data=filtered_df, index=c("Date","County.Code"), model="random")
    HTML(stargazer(random, header=FALSE, type="html", single.row=TRUE, title="Random-effects Model"))
  })
  
  output$distPlot = renderPlot({
    dbscan::kNNdistplot(cluster_df, k = input$minPts)
    abline(h = input$eps, lty = 2)
  })
  
  output$dbCluster = renderPlotly({
    set.seed(123)
    db <- fpc::dbscan(cluster_df, eps = input$eps, MinPts = input$minPts)
    # Add cluster assignments to the data frame
    temp = data.frame(filtered_df)
    temp$cluster <- factor(db$cluster)
    
    plot_ly(
      temp, x = ~Current.Traffic, y=~AQI, z=~Current.Truck,
      color = ~cluster, type = "scatter3d", mode = "markers",
      colors = c("#FFCE00", "#002A86")
    ) %>% layout(scene = list(
      xaxis = list(title = "Traffic Volume"),
      yaxis = list(title = "Air Quality"),
      zaxis = list(title = "Truck Volume")
    ), legend = list(title = list(text = "Cluster")))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
