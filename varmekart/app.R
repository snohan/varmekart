#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
#library(viridis)
library(ggExtra)
library(ggthemes)
library(DT)



# fil <- "./timetrafikk/100001-volumes-2017.csv"
# lanevolumes <- les_inn_timeverdier_uten_summering(fil)
# lag_heatmap(lanevolumes, 1)


ui <- fluidPage(

  verticalLayout(

    titlePanel(h4("Varmekart for timetrafikk")),

    wellPanel(

      selectInput(inputId = "fil",
                  label = "Velg fil:",
                  choices = list.files(path = "timetrafikk")
      )
    ),
    plotOutput("varmekart")
  )
)

server <- function(input, output) {

  #filer <- list.files(path = "timetrafikk")

  output$varmekart <- renderPlot({

    # Funksjon for les inn
    les_inn_timeverdier_uten_summering <- function(fil) {

      timeverdier <- read_delim(fil, delim = ";") %>%
        select(1, 4:7) %>%
        rename(msnr = measure_point_number,
               felt = lane,
               volum = '[..,..)') %>%
        mutate(interval_start = str_sub(interval_start, 1, 22)) %>%
        mutate(interval_start = str_replace(interval_start, ":00", ":00:00"))

      timeverdier$interval_start <-
        with_tz(ymd_hms(timeverdier$interval_start), "CET")

      timeverdier2 <- timeverdier %>%
        mutate(aar = year(interval_start),
               maaned = month(interval_start, label = T),
               dag = day(interval_start),
               time = hour(interval_start),
               aardag = yday(interval_start))

      return(timeverdier2)
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

    fila <- paste0("timetrafikk/", input$fil)

    lanevolumes <- les_inn_timeverdier_uten_summering(fila)
    lag_heatmap(lanevolumes, 1)

  })
}

# Run the application
shinyApp(ui = ui, server = server)

