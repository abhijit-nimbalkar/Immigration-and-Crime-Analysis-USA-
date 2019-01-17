
set.seed(100)
server <- function(input,output, session){
  
  getDataSet1<-reactive({
    input.str<-str_split(input$parameter,'_')
    input.str1<-input.str[[1]][1]
    input.str2<-input.str[[1]][2]
    dataSet1<-immigrant_data[immigrant_data$X1==input$year & immigrant_data$variable==paste0(input.str1,"_state_",input.str2),]

    joinedDataset1<-districts
    
    joinedDataset1@data$DISTRICT<-as.integer(joinedDataset1@data$DISTRICT)
    joinedDataset1@data <- suppressWarnings(left_join(joinedDataset1@data, dataSet1, by="DISTRICT"))
    
    joinedDataset1
  })
  
  getDataSet2<-reactive({
    
    input.str<-str_split(input$parameter,'_')
    input.str1<-input.str[[1]][1]
    input.str2<-input.str[[1]][2]
    dataSet2<-immigrant_data[immigrant_data$X1==input$year & immigrant_data$variable==paste0(input.str1,"_abroad_",input.str2),]
    
    joinedDataset2<-districts
    joinedDataset2@data$DISTRICT<-as.integer(joinedDataset2@data$DISTRICT)
    joinedDataset2@data <- suppressWarnings(left_join(joinedDataset2@data, dataSet2, by="DISTRICT"))
    joinedDataset2
  })
  
  data_for_scatter<-reactive({
    input.str<-str_split(input$parameter,'_')
    input.str1<-input.str[[1]][1]
    input.str2<-input.str[[1]][2]
    data_for_scatter<-immigrant_data[immigrant_data$X1==input$year,]
    yaxis<-data_for_scatter %>% filter(variable=='freq')
    xaxis1<-data_for_scatter %>% filter(variable==paste0(input.str1,"_abroad_",input.str2))
    xaxis2<-data_for_scatter %>% filter(variable==paste0(input.str1,"_state_",input.str2))
    total_xaxis<-left_join(xaxis1,xaxis2,by=c("X1","DISTRICT","name")) %>% mutate(total=value.x+value.y) 
    df<-left_join(yaxis,total_xaxis,by=c("X1","DISTRICT","name"))
    df
  })
  
  output$scatterImmigrantCrime <- renderPlotly({
    data_to_plot<-data_for_scatter()
    plot_ly(data = data_to_plot, x = ~total, y = ~value, color = ~value, colors = c("#CCB7F1","#4E2623")) %>% layout(title = 'Crime and Immigrant Scatter Plot',
                                                                                                                     xaxis = list(title='Immigrant',showgrid = TRUE ),
                                                                                                                     yaxis = list(title='Crime',showgrid = TRUE),
                                                                                                                     plot_bgcolor="#0B0B0B",
                                                                                                                     font=list(color="#ffffff",size=10),
                                                                                                                     paper_bgcolor="#0B0B0B",
                                                                                                                     showlegend = FALSE)
    
  })
  
  
   output$mymap_state<-renderLeaflet({
     leaflet() %>% 
       addTiles() %>% 
       
       
       setView(-118.387990,34.145223,zoom=10)   
     
 })
   
  output$mymap_abroad<-renderLeaflet({
   
    leaflet() %>%
      addTiles() %>%
      
      setView(-118.387990,34.145223,zoom=10)     
 })
 
  
  
  observe({
    theData1<-getDataSet1() 
    theData2<-getDataSet2()
    
    pal <- colorBin(c("#E57960","#CD5D48","#B24334","#952C23","#761714","#550606"),theData1$value) 
    
    
    data_popup_state <- paste0("<strong>Representative Name: </strong>", 
                            theData1$name, 
                            "<br>Total Immigrant(in <strong>",
                            input$parameter," 
                            )</strong>: ", 
                            formatC(theData1$value, format="d", big.mark=',')
    )
    
    data_popup_abroad <- paste0("<strong>Representative Name: </strong>", 
                                  theData2$name, 
                                  "<br>Total Immigrant(in <strong>",
                                  input$inputradio," 
                                  )</strong>: ", 
                                  formatC(theData2$value, format="d", big.mark=',')
    )
    
    labels1 <- sprintf(
      "<strong>District Number %s: </strong>%g Immigrant",
      theData1$DISTRICT, theData1$value
    ) %>% lapply(htmltools::HTML)
    
    labels2 <- sprintf(
      "<strong>District Number %s: </strong>%g Immigrant",
      theData2$DISTRICT, theData2$value
    ) %>% lapply(htmltools::HTML)
    
    m1<-leafletProxy("mymap_state", data = theData1) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        clearShapes() %>%
        addPolygons(data = theData1,
                  fillColor = pal(theData1$value), 
                  fillOpacity = 1, 
                  color = "white", 
                  weight = 2,
                  dashArray = 3,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE), popup = data_popup_state,
                    label = labels1,
                    labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    m1 <-m1 %>% clearControls() %>% addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                                        position = "bottomright")

    m2<-leafletProxy("mymap_abroad", data = theData2) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
        clearShapes() %>%
        addPolygons(data = theData2,
                  fillColor = pal(theData2$value), 
                  fillOpacity = 1, 
                  color = "white", 
                  weight = 2,
                  dashArray = 3,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "black",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE), popup = data_popup_abroad,
                    label = labels2,
                    labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))
    
    m2 %>% clearControls() %>% addLegend(pal = pal, values = ~value, opacity = 0.7, title = NULL,
                                         position = "bottomright")
    
    if(input$crimeoverlay=='crime'){
      data_for_circle<-get(paste0("crime",input$year))
      m1 <- m1 %>% addCircles(data=data_for_circle,lat = ~lat,lng = ~lng, radius = 0.5, color = c("#57531F","#C6DA64")) %>% withSpinner()
      m2 %>% addCircles(data=data_for_circle,lat = ~lat,lng = ~lng, radius = 0.5, color = c("#57531F","#C6DA64")) %>% withSpinner()
    }
       
  })
  
  output$immigrantTable <- renderDataTable(datatable({
    dataset1<-getDataSet1()
    dataset2<-getDataSet2()
  
    dataset1<-dataset1@data[,c(4,11,12,14)]
    names(dataset1)<-c("District_ID","Year","Reprensetative_Name",paste0(input$parameter,"_From Different STATE"))
  
    dataset2<-dataset2@data[,c(4,11,12,14)]
    names(dataset2)<-c("District_ID","Year","Reprensetative_Name",paste0(input$parameter,"_From ABROAD"))
    
    final_df<-left_join(dataset1,dataset2,by=c("Year","District_ID","Reprensetative_Name"))
    
    final_df
  },
  options = list(lengthMenu = c(5, 10, 33), pageLength = 5))
  )
  
}