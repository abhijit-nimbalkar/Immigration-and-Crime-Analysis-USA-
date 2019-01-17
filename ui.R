ui <- navbarPage("Immigration and Crime Analysis - City of Los Angeles", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          includeCSS("styles.css")
                        ),
                        
                        fluidRow(
                          fluidRow( class="informationPanal",
                            column(3,
                                   sliderTextInput("year", "Select Year", choices = c("2016",
                                                                                      "2015",
                                                                                      "2014",
                                                                                      "2013",
                                                                                      "2012",
                                                                                      "2011",
                                                                                      "2010"),animate = TRUE, grid = TRUE
                                   )
                                   ),
                            column(3,
                                   selectInput("parameter", "Select Parameter to filter", c("Race:Hispanic and Latino"="race_hispanic",
                                                                                            "Race:White"="race_white",
                                                                                            "Race:Asian"="race_asian",
                                                                                            "Race:Black and Africa"="race_black",
                                                                                            "Education:High School"="education_high",
                                                                                            "Education:College"="education_college",
                                                                                            "Education:Bachelor"="education_bachelor",
                                                                                            "Education:Graduate"="education_graduate",
                                                                                            "Poverty:Below 100"="poverty_below100",
                                                                                            "Poverty:From 100 to 149"="poverty_100to149",
                                                                                            "Poverty:Above 150"="poverty_above150"
                                                                                            ), 
                                               selected = "race_hispanic")
                                   
                                   
                                   ),
                            column(3,
                                   radioButtons("crimeoverlay",label = "Crime Data Overlay",choices=list("Show Crime Data"="crime",
                                                                                 "Remove Crime Data"="nocrime"),selected ="nocrime" )
                                   
                            ),
                            column(3,
                                   plotlyOutput("scatterImmigrantCrime", height = 170)
                                   
                                   )
                          ),
                        fluidRow( 
                          column(6, tags$p(class="map-title","Immigration from other States to Los Angeles"),
                                 withSpinner(leafletOutput("mymap_state",height =  330 , width='100%'))
                          ),
                          column(6,tags$p(class="map-title","Immigration from Abroad to Los Angeles"),
                                 withSpinner(leafletOutput("mymap_abroad",height = 330 ,width='100%'))
                          )
                          
                          )
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    div(class="nonouter",
                        
                        tags$head(
                          includeCSS("styles.css")
                        ),
                    
                    dataTableOutput("immigrantTable")
           ),
           
           conditionalPanel("false", icon("crosshair")))
)