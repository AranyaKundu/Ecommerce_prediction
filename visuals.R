# library(readxl)
# 
# raw_data <- read_excel("D:/Coursework/Mod-4/Project/Global Superstore Orders.xlsx")
# data <- raw_data[, -1]
# colnames(data) <- gsub(" ", "_", names(data))
# save(data, file = "data.rda")

#load necessary libraries
library(ggplot2)
library(dplyr)
library(zoo)

# load the data
load("data.rda")

# choose the axes options for the visualizations in shiny
x_axes_options <- colnames(data)[c(4, 7, 12, 13, 15)]
y_axes_options <- colnames(data)[c(18, 19, 21)]

# create basic visual
plot_fn <- function(x_axis, y_axis){
  plot_data <- data %>% select(x_axis, y_axis) # filter the data for the plot
  plot_data <- plot_data %>% group_by(get(x_axis)) %>%
    summarise(y_axis = sum(get(y_axis))) %>% as.data.frame # group data for bar chart
  colnames(plot_data) <- c(x_axis, y_axis)
  plot_1 <- ggplot(plot_data,
                   aes(x = plot_data[, 1], y = plot_data[, 2], fill = plot_data[, 1])) +
    geom_col() + # Plot bar chart
    ggtitle(glue::glue("Total {colnames(plot_data)[2]} per {colnames(plot_data)[1]} Graph")) +
    scale_y_continuous(labels = scales::comma) + # change scales to continuous from scientific
    labs(x = colnames(plot_data)[1], y = colnames(plot_data)[2]) +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text = element_text(size = 16),
          plot.title = element_text(size = 20),
          axis.title = element_text(size = 18))
  return(plot_1)
}

plot_facet_fn <- function(x_axis, y_axis, facet_wrap){
  plot_data <- data %>% select(x_axis, y_axis, facet_wrap) # select data for the plot
  plot_data <- plot_data %>% group_by(get(x_axis), get(facet_wrap)) %>% 
    summarise(sum(get(y_axis))) %>% as.data.frame() # group by for bar chart with facet wrap
  colnames(plot_data) <- c(x_axis, facet_wrap, y_axis)
  plot_2 <- ggplot(plot_data, 
                   aes(x = plot_data[, 1], y = plot_data[, 3], fill = plot_data[, 1])) +
    geom_col() + 
    facet_wrap(~ plot_data[, 2]) + # facet wrap here
    ggtitle(glue::glue("Total {colnames(plot_data[3])} per {colnames(plot_data)[1]} per 
                       {colnames(plot_data[2])}")) + 
    scale_y_continuous(labels = scales::comma) +
    labs(x = colnames(plot_data)[1], y = colnames(plot_data)[3]) + 
    theme(panel.grid = element_blank(), # set custom theme
          legend.position = "none",
          axis.text = element_text(angle = 90, hjust = 1, size = 16),
          plot.title = element_text(size = 20),
          axis.title = element_text(size = 18))
  return (plot_2)
}

# table to check the original data
data_dist_table <- function(select_data){
  table_data <- data %>% select(select_data) %>% table() %>% as.data.frame()
  return (table_data)
}

time_series_viz <- function(selectx, mov_avg){
  # select necessary data
  monthly_sales <- data %>% group_by(year_month = format(Order_Date, "%Y-%m")) %>% 
    summarise(Total_sales = sum(Sales),
              Total_Profit = sum(Profit))
  
  if (selectx == "Total Sales"){
    # convert to time series object
    plot_data <- ts(monthly_sales[, 2], frequency = 12, start = c(2012, 01), 
                    end = c(2015, 12))
  }
  else{
    # convert to time series object
    plot_data <- ts(monthly_sales[, 3], frequency = 12, start = c(2012, 01), 
                    end = c(2015, 12))
  }
  if (mov_avg == "Yes"){
    # add centered and trailing moving average
    ma.trailing = rollmean(plot_data, k = 12, align = "right")
    ma.centered = ma(plot_data, order = 12)
  plot_3 <- plot(plot_data, lwd = 2, col = 'blue', 
                 xlab = selectx, ylab = 'Time', 
                 main = glue::glue("{selectx} change over Time"))
  # Add moving average lines to the main time series plot
  lines(ma.centered, col = "red", lwd = 2)
  lines(ma.trailing, col = "purple", lwd = 2)
  legend("topleft", col = c("blue", "red", "purple"), lwd = c(1, 2, 2), 
         legend = c(selectx, "Centered MA", "Trailing MA"), lty = c(1, 1, 2))
  }else{
    plot_3 <- plot(plot_data, lwd = 2, col = 'blue', 
                   xlab = selectx, ylab = 'Time', 
                   main = glue::glue("{selectx} change over Time"))
  }
  return (plot_3)
}
