# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# ITERATION WITH PURRR ----

library(readxl)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)



# 1.0 PRIMER ON PURRR ----
# Programmatically getting Excel files into R
#   __dir_info() => return informations of all files in the directory 

excel_paths_tbl <- fs::dir_info("../00_data/bike_sales/data_raw/")

#   __pipe in character
paths_chr <- excel_paths_tbl %>% 
    pull(path) 
# What Not To Do: Don't use for loops


# What to Do: Use map() => automate Excel reading file

# OPTION_1 ---

#   __map(.x, .Func) =>  apply a function to each element of a list ----
#   __map() ==> always return a List ----

excel_list <- paths_chr %>% map(read_excel)  %>% set_names(paths_chr)


# OPTION_2 : Anonymous Function  ----
 paths_chr %>% map(~ read_excel(.))

# OPTIOn_3 : function specified with function  ----
paths_chr %>% map(function(x) read_excel(path = x))


# 2.0 MAPPING DATA FRAMES ----

# 2.1 Column-wise Map ----

bike_orderlines_tbl %>% map(~ class(.)) 

# 2.2 Map Variants ----

#   __map_chr() => display the columns names | output is a character ----
bike_orderlines_tbl %>%
    map_chr(~ class(.)[1]) 

QQ <- bike_orderlines_tbl %>% 
    map_dfc(~ class(.)[1])  

bike_orderlines_tbl %>% map_dfr(~ sum(is.na(.)) / length(.)) 

# 2.3 Row-wise Map ----

    #__READ a format in a repertory ----

excel_tbl <- excel_paths_tbl %>%
    select(path) %>%
    mutate(data = path %>% map(read_excel))

    #__the difference between a List (have contains inside) and a tibble (data.frame) ----

#excel_list => List ----

#excel_tbl => Data frame ----
    
# 3.0 NESTED DATA ----

# Unnest

excel_tbl$data

excel_tbl$data[[1]]

#   __unnest() => 

excel_tbl_unnested <- excel_tbl %>% unnest_legacy(data, .id = "ID") %>% view()
    


#   __Nest() => apply (unnested_data %>% group_by + nest()) ----

excel_tbl_nested <- excel_tbl_unnested %>% group_by(ID, path) %>% nest()

# ............................................... ----
# Mapping Nested List Columns ----

x <- rep(NA, 5)

!is.na(x) %>% all()

#................................................ -----
# 1.0 Apply a command on one list of the nested list ----

#   Eliminates all the NA element inside the 1st tibble of the nested data ----
#   __all() => returns TRUE if all elements are TRUE

excel_tbl_nested$data[[1]] %>%
    select_if(~ !is.na(.) ) %>% all() 

#................................................ -----
# 2.0 Apply a command to the ensemble of tiblle nestled ----

# Method 1 : Creating a function outside of purr::map()

# Step_1 ==> make the function for one element 

select_non_na_columns <- function(data) {
    
    data %>% select_if(~ !is.na(.) %>% all())
}

# Step_2 ==> extract and check if the function work for an element 
excel_tbl_nested$data[[1]] %>% select_non_na_columns()


# step_3 ==> Use mutate() + map(function created)
excel_tbl_nested_fixed <- excel_tbl_nested %>% mutate(data_fixed = data %>% map(select_non_na_columns))

excel_tbl_nested_fixed$data_fixed[[1]]


# 4.0 MODELING WITH PURRR ----

# 4.1 Time Series Plot ----
#  - What if we wanted to approximate the 3 month rolling average with a line?
#  - We can use a smoother

# Code comes from 04_functions_iteration/01_functional_programming
rolling_avg_3_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date(order_date, unit = "month") - period(1, unit = "days")) %>%
    
    group_by(category_1, category_2, month_end) %>%
    summarise(
        total_price = sum(total_price)
    ) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price)) 

rolling_avg_3_tbl %>%
    
    ggplot(aes(month_end, total_price, color = category_2)) +
    
    # Geometries
    geom_point() +
    geom_line(aes(y = rolling_avg_3), color = "blue", linetype = 1) +
    facet_wrap(~ category_2, scales = "free_y") +
    
    # Add Loess Smoother
    geom_smooth(method = "loess", se = FALSE, span = 0.2, color = "black") +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K"))




# 4.2 Modeling Primer ----

# Data Preparation

sales_by_m_cross_country_tbl <- rolling_avg_3_tbl %>%
    filter(category_2 == "Cross Country Race") %>%
    select(month_end, total_price) %>%
    mutate(month_end_num = as.numeric(month_end))
    
sales_by_m_cross_country_tbl %>% 
    
    ggplot(aes(month_end_num, total_price)) + 
    geom_point() + 
    geom_smooth(method = "loess", span = 0.2, se = FALSE)

# Making a loess model

# __loess() => isnt a tidy function thats why we need to pipe sales_by_m_cross_country in the argument (data = .) ----

fit_loess_data1 <- sales_by_m_cross_country_tbl %>% loess(total_price ~ month_end_num, data = ., span=.3)


# Working With Broom

#   __broom() => is a package that allow to deal with output from statistical model ----
#   __broom()::augment  => convert statistical object into tidy tibbles ----

fit_loess_data1 %>% broom::augment() %>%

# Visualizing results

ggplot(aes(month_end_num, total_price)) + 
    geom_point()+
    geom_line(aes(y = .fitted), color = "blue")
    


# 4.3 Function To Return Fitted Results ----

# Step_1

#   __group_nest() => for nesting group columns ----

nested_data <- rolling_avg_3_tbl %>% group_by(category_1, category_2) %>% group_nest()

nested_data_month_tbl <- nested_data$data[[3]] %>%
    select(month_end, total_price) %>%
    mutate(month_end_num = as.numeric(month_end)) 


fit_loess <- nested_data_month_tbl %>% loess(total_price ~ month_end_num, data = ., span=.2)

fit_result <- fit_loess %>% broom::augment()  

data <- nested_data$data[[1]]

# Step_2 => make a general function starting from a testable data (test easily your function)

loess_regr <- function(data) {
    

    nested_data_formatted <- data %>%
        select(month_end, total_price) %>%
        mutate(month_end_num = as.numeric(month_end))
    
    fit_loess <- loess(total_price ~ month_end_num, data = nested_data_formatted, span=.2)
    
    fit_result_tbl <- fit_loess %>% broom::augment() %>% select(.fitted)
    
    return(fit_result_tbl)
    
}

# step_3 ==> test the function on one extracted element (change the number to go through all the tibble)
nested_data$data[[4]] %>% loess_regr()

# step_4 ==> Use mutate() + map(function created)

result <- nested_data %>% mutate(fitted = data %>% map(loess_regr))

result$fitted[[1]]

result %>% unnest() %>% view()


# Visualize Results
t
result %>% unnest() %>%
    ggplot(aes(month_end, total_price, color = category_2)) + 
    
    # Geometries 
    geom_point() + 
    geom_line(aes(y = .fitted), color = "blue") + 
   # geom_smooth(method = "loess" , span = 0.2) + 
    facet_wrap(~ category_2, scales = "free_y")
    
    
    
    



