library(shiny)
library(shinydashboard)

# Define UI
header <- dashboardHeader(
    title = h3("InsightBnB: Analyzing Descriptions of Airbnb Listings in the United States"),
    titleWidth = 800
)

sidebar <- dashboardSidebar(
    div(style = "padding: 20px; padding-top: 20px; padding-bottom: 0px;",
        img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/69/Airbnb_Logo_B%C3%A9lo.svg/2560px-Airbnb_Logo_B%C3%A9lo.svg.png", height = "50px")
    ),
    sidebarMenu(
        menuItem("Property Types", tabName = "propertytype", icon = icon("home")),
        selectInput("propertyType", "Property Type:", 
                    choices = c("All", "Apartment", "House", "Condominium")),
        menuItem("Room Types", tabName = "roomtype", icon = icon("bed")),
        selectInput("roomType", "Room Type:", 
                    choices = c("All", "Entire home/apt", "Private room", "Shared room")),
        menuItem("Created by Suraj Udasi", icon = icon("github"),
                 href = "https://github.com/udasii")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "propertytype",
                h3("Data Categorized by Property Type"),
                fluidRow(
                    box(
                        title = "Correlogram for Property Types",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("corrplot_pt")
                    ),
                    box(
                        title = "Most Unique and High-Value Terms",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("propertytype_plot")
                    )
                ),
                fluidRow(
                    valueBoxOutput("listings_box_pt", width = 4),
                    valueBoxOutput("average_price_box_pt", width = 4),
                    valueBoxOutput("average_pax_box_pt", width = 4)
                ),
                fluidRow(
                    box(
                        title = "TF-IDF Bigram Network",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("tf_idf_bigram_plot_pt")
                    ),
                    box(
                        title = "Comparing AFINN, Bing, and NRC Sentiment Scores",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("sentiment_analysis_pt")
                    )
                )
        ),
        tabItem(tabName = "roomtype",
                h3("Data Categorized by Room Type"),
                fluidRow(
                    box(
                        title = "Correlogram for Room Types",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("corrplot_rt")
                    ),
                    box(
                        title = "Most Unique and High-Value Terms",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("roomtype_plot")
                    )
                ),
                fluidRow(
                    valueBoxOutput("listings_box_rt", width = 4),
                    valueBoxOutput("average_price_box_rt", width = 4),
                    valueBoxOutput("average_pax_box_rt", width = 4)
                ),
                fluidRow(
                    box(
                        title = "TF-IDF Bigram Network",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("tf_idf_bigram_plot_rt")
                    ),
                    box(
                        title = "Comparing AFINN, Bing, and NRC Sentiment Scores",
                        status = "primary",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        plotOutput("sentiment_analysis_rt")
                    )
                )
        )
    )
)

ui <- dashboardPage(
    header,
    sidebar,
    body,
    skin = "red", #"blue”, “black”, “purple”, “green”, “red”, “yellow”
    title = "InsightBnB Dashboard"
)

