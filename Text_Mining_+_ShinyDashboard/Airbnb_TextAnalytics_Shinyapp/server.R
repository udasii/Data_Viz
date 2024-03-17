library(shiny)
library(shinydashboard)

# Your ggplot code wrapped in a reactive function
corr_plot_pt <- reactive({
    ggplot(frequency_pt, aes(x=proportion, y=`Apartment`, 
                             color = abs(`Apartment`- proportion)))+
        geom_abline(color="grey40", lty=2)+
        geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
        geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
        scale_x_log10(labels = percent_format())+
        scale_y_log10(labels= percent_format())+
        scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray")+
        facet_wrap(~property_type, ncol=2)+
        theme(legend.position = "none")+
        labs(y= "Apartment", x=NULL)
})

corr_plot_rt <- reactive({
    ggplot(frequency_rt, aes(x=proportion, y=`Entire home/apt`, 
                             color = abs(`Entire home/apt`- proportion)))+
        geom_abline(color="grey40", lty=2)+
        geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
        geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
        scale_x_log10(labels = percent_format())+
        scale_y_log10(labels= percent_format())+
        scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
        facet_wrap(~room_type, ncol=2)+
        theme(legend.position = "none")+
        labs(y= "Entire home/apt", x=NULL)    
})


# Define server logic
shinyServer(function(input, output) {
    
    # Reactive expression for filtering the airbnb dataset based on property type
    airbnb_filtered_pt <- reactive({
        if (input$propertyType == "All") {
            return(airbnb)
        } else {
            return(airbnb %>% filter(property_type == input$propertyType))
        }
    })
    
    airbnb_filtered_rt <- reactive({
        if (input$roomType == "All") {
            return(airbnb)
        } else {
            return(airbnb %>% filter(room_type == input$roomType))
        }
    })
    
    output$listings_box_pt <- renderValueBox({
        valueBox(nrow(airbnb_filtered_pt()), "Number of Listings", icon = icon("home"), color = 'aqua')
    })
    
    output$average_price_box_pt <- renderValueBox({
        valueBox(round(mean(airbnb_filtered_pt()$price, na.rm = TRUE)), "Average Price", icon = icon("dollar"), color = 'light-blue')
    })
    
    output$average_pax_box_pt <- renderValueBox({
        valueBox(round(mean(airbnb_filtered_pt()$accommodates, na.rm = TRUE)), "Average PAX", icon = icon("users"), color = 'teal')
    })
    
    output$listings_box_rt <- renderValueBox({
        valueBox(nrow(airbnb_filtered_rt()), "Number of Listings", icon = icon("home"), color = 'aqua')
    })
    
    output$average_price_box_rt <- renderValueBox({
        valueBox(round(mean(airbnb_filtered_rt()$price, na.rm = TRUE)), "Average Price", icon = icon("dollar"), color = 'light-blue')
    })
    
    output$average_pax_box_rt <- renderValueBox({
        valueBox(round(mean(airbnb_filtered_rt()$accommodates, na.rm = TRUE)), "Average PAX", icon = icon("users"), color = 'teal')
    })
    
    output$corrplot_pt <- renderPlot({
        corr_plot_pt()
    })
    
    output$corrplot_rt <- renderPlot({
        corr_plot_rt()
    })
    
    output$propertytype_plot <- renderPlot({
        tf_idf_plot_pt
    })
    
    output$roomtype_plot <- renderPlot({
        tf_idf_plot_rt
    })
    
    output$tf_idf_bigram_plot_pt <- renderPlot({
        if (input$propertyType == "All") {
            pt_bigram_graph_plot
        } else if (input$propertyType == "House") {
            house_bigram_graph_plot
        } else if (input$propertyType == "Condominium") {
            condo_bigram_graph_plot
        } else {
            apt_bigram_graph_plot
        }
    })
    
    output$tf_idf_bigram_plot_rt <- renderPlot({
        if (input$roomType == "All") {
            rt_bigram_graph_plot
        } else if (input$roomType == "Entire home/apt") {
            eh_bigram_graph_plot
        } else if (input$roomType == "Private room") {
            pr_bigram_graph_plot
        } else {
            sr_bigram_graph_plot
        }
    })
    
    output$sentiment_analysis_pt <- renderPlot({
        if (input$propertyType == "All") {
            airbnb_sent
        } else if (input$propertyType == "House") {
            house_sent
        } else if (input$propertyType == "Condominium") {
            condo_sent
        } else {
            apt_sent
        }
    })
    
    output$sentiment_analysis_rt <- renderPlot({
        if (input$roomType == "All") {
            airbnb_sent
        } else if (input$roomType == "Entire home/apt") {
            eh_sent
        } else if (input$roomType == "Private room") {
            pr_sent
        } else {
            sr_sent
        }
    })
    
})
