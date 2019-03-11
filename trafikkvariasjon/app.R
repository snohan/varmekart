#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ghql)
library(tidyverse)
library(magrittr)
library(lubridate)
library(jsonlite)
library(httr)
library(ggExtra)
library(ggthemes)
library(DT)
library(mgcv)


ui <- fluidPage(

  verticalLayout(

    titlePanel(h4("Variasjon i timetrafikk")),

    wellPanel(
      # TODO: Velge fra punktliste. Kan den filtreres på fylke og veg?
      # Evt. i første versjon, lim inn trpid.
      # selectInput(inputId = "fil",
      #             label = "Velg fil:",
      #             choices = list.files(path = "timetrafikk")
      #             ),
      textInput("trpid",
                label = h3("Lim inn punkt-ID"),
                value = "73168V705238"),
      radioButtons(inputId = "year",
                   label = "Velg år:",
                   choices = c("2014", "2015", "2016", "2017", "2018", "2019"),
                   inline = TRUE)
      )
    ),
    plotOutput("varmekart")
  )

server <- function(input, output) {

  output$varmekart <- renderPlot({

    cli <- GraphqlClient$new(
      url = "https://www.vegvesen.no/trafikkdata/api/?query="
    )

    # Funksjon for å hente timetrafikk fra trafikkdata-API
    # TODO: hent per kjørefelt
    getHourlytraffic <- function(trpID = "73168V705238", from, to) {
      # Default values
      hasNextPage <- TRUE
      cursor <- ""
      hourlyTraffic <- data.frame()

      build_query <- function() {
        query_hourlyTraffic <- paste0(
          '{
          trafficData(trafficRegistrationPointId: "',
          trpID,
          '"){
          trafficRegistrationPoint {
          id
          name
          }
          volume {
          byHour(
          from: "',
          from,
          '",
          to: "',
          to,
          '",
          after: "',
          cursor,
          '"
          ) {
          edges {
          node {
          from
          total {
          volume
          }
          }
          }
          pageInfo {
          hasNextPage
          endCursor
          }
          }
          }
          }
          }
          ')
        }

    while(hasNextPage == TRUE){

      myqueries <- Query$new()
      myqueries$query("hourlyTraffic", build_query())

      trafficData <- cli$exec(myqueries$queries$hourlyTraffic) %>%
        fromJSON(simplifyDataFrame = T, flatten = T)

      if(length(trafficData$data$trafficData$volume$byHour$edges) == 0)
        break;

      trafficData %<>% as.data.frame()

      cursor <-
        trafficData$data.trafficData.volume.byHour.pageInfo.endCursor[1] %>%
        as.character()
      hasNextPage <-
        trafficData$data.trafficData.volume.byHour.pageInfo.hasNextPage[1]

      trafficData %<>% select(1:4)

      hourlyTraffic <- bind_rows(hourlyTraffic, trafficData)
    }

    if(nrow(hourlyTraffic) == 0) {
      hourlyTraffic <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                c("point_id", "point_name", "hour_from",
                                  "total_volume"))
    }else{
      colnames(hourlyTraffic) <- c("point_id", "point_name", "hour_from",
                                   "total_volume")
      hourlyTraffic %<>% mutate(hour_from = with_tz(ymd_hms(hour_from), "CET"))
    }

    return(hourlyTraffic)
    }

    # Funksjon for varmekart
    lag_heatmap <- function(lanevolumes, feltnr) {
      msnr <- lanevolumes$msnr[1]
      lanenumber <- feltnr
      feltplott <- lanevolumes %>%
        filter(felt == lanenumber) %>%
        ggplot(aes(dag, time, fill = volum)) +
        theme(plot.margin = unit(c(.2,.2,.2,.2), units = "lines")) +
        geom_tile(color = "white", size = 0.1) +
        coord_equal() +
        scale_fill_viridis_c(name = "Trafikkmengde", option = "magma",
                             direction = -1) +
        theme_tufte(base_size = 6) +
        facet_wrap( ~ maaned, ncol = 4) +
        scale_y_continuous(trans = "reverse", breaks = c(0, 8, 16, 23)) +
        scale_x_continuous(breaks = c(1, 15, 31)) +
        labs(title = paste0("Trafikkmengde per time - stasjonsnr. ",
                            msnr, ", felt", lanenumber),
             x = "Dag", y = "Timestart") +
        theme(legend.position = "right") +
        theme(plot.title = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 6)) +
        theme(strip.background = element_rect(color = "white")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.ticks = element_blank()) +
        theme(axis.text = element_text(size = 6)) +
        theme(panel.spacing.x = unit(0.01, "cm")) +
        theme(panel.spacing.y = unit(0.05, "cm")) +
        theme(legend.title = element_text(size = 6)) +
        theme(legend.text = element_text(size = 6)) +
        removeGrid()

      return(feltplott)
    }

    lag_varmekart_total <- function(lanevolumes) {

      feltplott <- lanevolumes %>%
        mutate(dag = day(hour_from),
               time = hour(hour_from),
               maaned = month(hour_from)) %>%
        ggplot(aes(dag, time, fill = total_volume)) +
        theme(plot.margin = unit(c(.2,.2,.2,.2), units = "lines")) +
        geom_tile(color = "white", size = 0.1) +
        coord_equal() +
        scale_fill_viridis_c(name = "Trafikkmengde", option = "magma",
                             direction = -1) +
        theme_tufte(base_size = 6) +
        facet_wrap( ~ maaned, ncol = 4) +
        scale_y_continuous(trans = "reverse", breaks = c(0, 8, 16, 23)) +
        scale_x_continuous(breaks = c(1, 15, 31)) +
        labs(title = paste0("Trafikkmengde per time - stasjonsnr. "),
             x = "Dag", y = "Timestart") +
        theme(legend.position = "right") +
        theme(plot.title = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 6)) +
        theme(strip.background = element_rect(color = "white")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(axis.ticks = element_blank()) +
        theme(axis.text = element_text(size = 6)) +
        theme(panel.spacing.x = unit(0.01, "cm")) +
        theme(panel.spacing.y = unit(0.05, "cm")) +
        theme(legend.title = element_text(size = 6)) +
        theme(legend.text = element_text(size = 6)) +
        removeGrid()

      return(feltplott)
    }


    # Kaller funksjonene
    yearfrom <- #input$year #1.1
    "2018-01-01T00:00:00+01:00"
    yearto <- #input$year + 1 #1.1
      "2019-01-01T00:00:00+01:00"
    lanevolumes <- getHourlytraffic("73168V705238",
      #input$trpid,
      yearfrom, yearto)
    # Hent ut alle felt som er med, og lag et plott for hver.
    lag_varmekart_total(lanevolumes)

  })
}

# Run the application
shinyApp(ui = ui, server = server)

