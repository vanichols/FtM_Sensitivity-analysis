library(tidyverse)
library(shiny)
library(plotly)
library(scales)
library(reshape2)

# load data
load("app_data/d01_sa_output.rda")
load("app_data/d02_pct_impact.rda")

metric_lookup <- read_csv("app_data/metric_lookup.csv")

######################################
# Define functions
######################################
# custom theme -----------------------
theme_cust_1 <- function () {
  theme_light() %+replace% 
    theme(legend.position = "none")
}

theme_cust_2 <- function () {
  theme_light() %+replace% 
    theme(legend.position = "none",
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.x = element_text(margin = margin(2,2,2,2), color = "grey50"),
          strip.background = element_rect(color = "grey90", fill = "grey90"))
}

# plot pct impact --------------------
filter_1 <- function(m, c, irr_stat, reg) {
  
  # m <- "Energy Use Fieldprint"
  # c <- "Corn grain"
  # irr_stat <- "Irrigated"
  # reg <- "Central"
  
  dat <- d02_pct_impact
  
  if ("All Regions" %in% reg) {
    dat_2 <- dat %>% filter(metric_label == m, crop == c, irrigation_status == irr_stat)
  } else {
    dat_2 <- dat %>% filter(metric_label == m, crop == c, irrigation_status == irr_stat, 
                            #region_label %in% reg) 
                            region_group %in% reg) 
  }
  
  # sort by median val of each scenario
  sorter <- dat_2 %>%
    group_by(grouping, change_name_label) %>%
    summarise(med_pct_change = median(pct_change),
              med_range = median(grp_range)) %>%
    arrange(med_range, med_pct_change)
  
  dat_2$change_name_label <- factor(dat_2$change_name_label, levels = sorter$change_name_label)
  
  dat_2
  
}

plt_1 <- function(m, c, irr_stat, reg) {
  
  # filter data
  dat <- filter_1(m, c, irr_stat, reg)

  # # get five colors to use for region groupings
  # gg_color_hue <- function(n) {
  #   hues = seq(15, 375, length = n + 1)
  #   hcl(h = hues, l = 65, c = 100)[1:n]
  # }
  # 
  # color_mapping <- data.frame(
  #   region_group = c("Central",
  #                    "NE/Appalachia",
  #                    "Southeast",
  #                    "Southwest/California",
  #                    "Mountains/PNW"),
  #   col = gg_color_hue(5),
  #   stringsAsFactors = FALSE
  #   )

  # color_scale <- dat %>%
  #   distinct(region_group) %>%
  #   left_join(color_mapping, by = "region_group")

  #--isn't working 7/22
  # gg <- dat %>%
  #   highlight_key(~region_label) %>%
  #   ggplot(aes(x = pct_change, y = change_name_label,
  #              text = paste0("Region: ", region_label,
  #                            "<br>Pct Change: ", percent(pct_change, accuracy = 1)))) +
  #   geom_vline(aes(xintercept = 0), color = "grey70") +
  #   geom_point(aes(color = region_group), alpha = 0.8, size = 2) +
  #   scale_x_continuous(labels = percent_format(accuracy = 1)) +
  #   scale_color_manual(breaks = color_scale$region_group, values = color_scale$col) +
  #   #theme_cust_1() +
  #   theme(legend.position = "none") +
  #   labs(title = paste0(m, ": ", c, ", ", irr_stat),
  #        color = "Region") +
  #   ylab("") +
  #   xlab("Percent Change")

  #--try to change groupings of regions 7/22
  gg <- 
    dat %>%
    #highlight_key(~region_label) %>%
    ggplot(aes(x = change_name_label,
               y = pct_change, 
               text = paste0("Region: ", region_label,
                             "<br>Pct Change: ", percent(pct_change, accuracy = 1)))) +
    geom_hline(aes(yintercept = 0), color = "grey70") +
    geom_point(aes(color = region_label), alpha = 1, size = 2) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    #theme_cust_1() +
    theme(legend.position = "top") +
    labs(title = paste0(m, ": ", reg, ", ", c, ", ", irr_stat),
         color = "Specific Region",
         y = "Percent Change",
         x = NULL) + 
      coord_flip()
  
  ggplotly(gg, tooltip = "text", height = max(length(unique(dat$change_name_label))*20, 200)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick") %>% 
    layout(height = 800, width = 1200)
  
}

# plot impact on soil outcomes -------

filter_2 <- function(m, c, irr_stat) {
  
   # m <- "Soil Carbon Fieldprint"
   # c <- "Corn grain"
  #  irr_stat <- "Irrigated"
  
  dat <- 
    d01_sa_output %>% 
    mutate_if(is.factor, as.character)
  
  # get col name and unit that corresponds to metric label
  m_col <- metric_lookup$metric[metric_lookup$metric_label == m]
  m_unit <- metric_lookup$metric_unit[metric_lookup$metric_label == m]
  
  # sort by region code (generally corresponds to east-west)
  sorter <- 
    dat %>% 
    select(region, region_label) %>% 
    distinct() %>% 
    arrange(region)
  
  # select columns and edit factor levels
  dat_2 <- 
    dat %>% 
    filter(
      grouping == "base", 
      crop == c, 
      irrigation_status == irr_stat) %>% 
    select(crop, irrigation_status, region, region_group, region_label, base_scenario, cover_crop, tillage_type, m_col) %>% 
    mutate(cover_crop = ifelse(cover_crop == TRUE, "Yes", "No"),
           tillage_type = ifelse(tillage_type == "No till", "No Till", tillage_type)) %>% 
    mutate(region_label = factor(region_label, levels = sorter$region_label, ordered = TRUE),
           #cover_crop = factor(cover_crop, levels = c("No", "Yes")),
           tillage_type = factor(tillage_type, levels = c("Conventional Tillage", "Reduced Tillage", "No Till")))
  
  dat_2
  
}

plt_2 <- function(m, c, irr_stat) {
  
  # filter data 
  dat <- filter_2(m, c, irr_stat)
  
  # get col name and unit that corresponds to metric label
  m_col <- metric_lookup$metric[metric_lookup$metric_label == m]
  m_unit <- metric_lookup$metric_unit[metric_lookup$metric_label == m]
  
  # old - has weird layout and cover crop distinction not showing up 7/22 
  # gg <- dat %>%
  #   highlight_key(~base_scenario) %>%
  #   ggplot(aes(x = get(m_col), y = cover_crop, text = paste0(m, ": ", round(get(m_col), digits = 1)))) +
  #   geom_vline(aes(xintercept = 0), color = "grey70") +
  #   geom_point(aes(color = tillage_type, shape = cover_crop), alpha = 0.8, size = 2) +
  #   facet_wrap(~region_label, ncol = 1)+
  #   scale_shape_manual(breaks = c("Yes", "No"), values = c(1,16)) +
  #   #theme_cust_2() +
  #   theme(legend.position = "none",
  #         axis.text.y = element_blank(),
  #         axis.ticks.y = element_blank(),
  #         strip.text.x = element_text(margin = margin(2,2,2,2))) +
  #   labs(title = paste0(m, ": ", c, ", ", irr_stat),
  #        color = "Tillage Type", shape = "Cover Crop") +
  #   ylab("") +
  #   xlab(m_unit)
  
  #--try to change layout 7/22
  gg <-
    dat %>%
    highlight_key(~base_scenario) %>%
    ggplot(aes(x = region_label,
               y = get(m_col),
               #y = cover_crop,
               text = paste0(m, ": ", round(get(m_col), digits = 1)))) +
    #geom_vline(aes(xintercept = 0), color = "grey70") +
    geom_point(aes(color = cover_crop, shape = cover_crop), size = 2) +
    geom_hline(yintercept = 0, color = "grey70") +
    scale_color_manual(values = c("Yes" = "green4", "No" = "gold4")) +
    scale_shape_manual(values = c("Yes" = 17, "No" = 19)) +
      scale_y_continuous(labels = label_comma()) +
  #theme_cust_2() +
      facet_grid(region_group ~ tillage_type, scales = "free", 
                 labeller = label_wrap_gen(15),
                 switch = "y") +
      theme(legend.position = "top", 
          legend.direction = "horizontal",
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.y.left = element_text(angle = 0),
          strip.placement = "outside") +
    labs(title = paste0(m, ": ", c, ", ", irr_stat),
         color = "Cover Crop",
         shape = "Cover Crop",
         x = NULL,
         y = m_unit) +
  
    coord_flip()
  
  #plot(gg)
  ggplotly(gg, tooltip = "text", height = max(length(unique(dat$region_label))*80, 200)) %>%
    highlight(on = "plotly_click", off = "plotly_doubleclick") %>% 
    layout(height = 800, width = 1200)
  
}

# regions - changed 7/22
regions_list <- 
  d02_pct_impact %>% 
  select(region_group) %>% 
  arrange(region_group) %>% 
  distinct() 

regions_list <- 
  #regions_list$region_label %>%
  regions_list$region_group %>% 
  as.list()

regions_list <- c("All Regions", regions_list)

######################################
# Define UI
######################################
ui <- navbarPage("Sensitivity Analysis Results",
  
  tabPanel("Percent Impact",
    
    sidebarLayout(
      sidebarPanel(width = 2,
                   selectInput("metric_1",
                               label = "Select Metric",
                               choices = list("Biodiversity Fieldprint",
                                              "Energy Use Fieldprint",
                                              "Greenhouse Gas Fieldprint",
                                              "Soil Carbon Fieldprint",
                                              "Water Erosion Fieldprint",
                                              "Wind Erosion Fieldprint"
                                              #"Water Quality Fieldprint"
                                              ),
                               selected = "Energy Use Fieldprint"),
                   selectInput("crop_1",
                               label = "Select Crop",
                               choices = list("Alfalfa",
                                              "Barley",
                                              "Corn grain",
                                              "Corn silage",
                                              "Cotton",
                                              "Peanuts",
                                              "Potatoes",
                                              "Rice",
                                              "Sorghum",
                                              "Soybeans",
                                              "Sugar beets",
                                              "Wheat"),
                               selected = "Corn grain"),
                   selectInput("irrigation_1",
                               label = "Select Irrigation Status",
                               choices = list("Irrigated",
                                              "Rainfed"),
                               selected = "Rainfed"),
                   selectizeInput("regions_1",
                               label = "Select Region(s)",
                               choices = regions_list,
                               selected = "All Regions",
                               multiple = F),
                   # strong("Legend"),
                   #img(src="legend_1.PNG"),
                   h2(""),
                   downloadButton("downloadData_1", "Download Data")
      ),
      
      mainPanel(
        plotlyOutput("plt_1")#, width = "100%", height = "auto")
      )
    )
    
  ),
  
  tabPanel("Absolute Impact",
           
           sidebarLayout(
             sidebarPanel(width = 2,
                          selectInput("metric_2",
                                      label = "Select Metric",
                                      choices = list("Biodiversity Fieldprint",
                                                     "Energy Use Fieldprint",
                                                     "Greenhouse Gas Fieldprint",
                                                     "Soil Carbon Fieldprint",
                                                     "Water Erosion Fieldprint",
                                                     "Wind Erosion Fieldprint"
                                                     #"Water Quality Fieldprint"
                                      ),
                                      selected = "Soil Carbon Fieldprint"),
                          selectInput("crop_2",
                                      label = "Select Crop",
                                      choices = list("Alfalfa",
                                                     "Barley",
                                                     "Corn grain",
                                                     "Corn silage",
                                                     "Cotton",
                                                     "Peanuts",
                                                     "Potatoes",
                                                     "Rice",
                                                     "Sorghum",
                                                     "Soybeans",
                                                     "Sugar beets",
                                                     "Wheat"),
                                      selected = "Corn grain"),
                          selectInput("irrigation_2",
                                      label = "Select Irrigation Status",
                                      choices = list("Irrigated",
                                                     "Rainfed"),
                                      selected = "Rainfed"),
                          # strong("Legend"),
                          #img(src="legend_2.PNG"),
                          h2(""),
                          downloadButton("downloadData_2", "Download Data")
             ),
             
             mainPanel(
               plotlyOutput("plt_2")#, width = "100%", height = "auto")
               #plotOutput("plt_2")#, width = "100%", height = "90%")
             )
           )
           
  )

)

# Define server logic ----
server <- function(input, output) {
  
  output$plt_1 <- renderPlotly({
    plt_1(input$metric_1, input$crop_1, input$irrigation_1, input$regions_1)
  })
  
  output$plt_2 <- renderPlotly({
  #output$plt_2 <- renderPlot({
    plt_2(input$metric_2, input$crop_2, input$irrigation_2)
  })
  
  output$downloadData_1 <- downloadHandler(
    filename = function() {
      paste0(paste(input$metric_1, input$crop_1, input$irrigation_1, sep=" - "), ".csv")
    },
    content = function(file) {
      
      data <- filter_1(input$metric_1, input$crop_1, input$irrigation_1, input$regions_1) %>% 
        arrange(change_name_label, region) %>% 
        select(metric_label, metric_unit, crop, irrigation_status, 
               region_label, region_group, 
               change_name_label, change_val, base_val, pct_change) %>% 
        rename(metric = metric_label, 
               region = region_label,
               scenario_name = change_name_label,
               scenario_fieldprint = change_val,
               baseline_fieldprint = base_val)
      
      write_csv(data, file)
    }
  )
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste0(paste(input$metric_2, input$crop_2, input$irrigation_2, sep=" - "), ".csv")
    },
    content = function(file) {
      
      data <- filter_2(input$metric_2, input$crop_2, input$irrigation_2) %>%
        arrange(region, tillage_type, cover_crop) %>% 
        select(-region, -base_scenario) %>% 
        select(crop, irrigation_status, region_label,
               tillage_type, cover_crop, everything()) %>% 
        rename(region = region_label)
      
      write_csv(data, file)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)