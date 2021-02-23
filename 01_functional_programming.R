# DS4B 101-R: R FOR BUSINESS ANALYSIS ---- 
# FUNCTIONAL PROGRAMMING ----


install.packages("ggrepel") # ggrepel needed for text and label repel in plots

library(tidyverse)
library(lubridate)
library(tidyquant)
library(ggrepel)
library(fs)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)





# 1.0 ANATOMY OF A FUNCTION ----

# 1.1 Examining the mean() function ----
x <- c(0:10, 50, NA_real_)
x



# 1.2 Customizing a mean function ----

# Name                     # Arguments (... = allow to add additional argument dans le calcul de la fonction) ----
mean_remove_na <- function(x, na.rm = TRUE, ...) {
    
    # Body
    avg <- mean(x, na.rm = na.rm, ...)
    
    # Return
    return(avg)
}

mean_remove_na(x)
mean_remove_na(x, na.rm = FALSE)
#   Ex = mean_remove(x, trim = 0.1 (ajout non initialement présent dans la définition de la fonction) ----
mean_remove_na(x, trim = 0.1)



# 2.0 THE TWO STYLES OF FUNCTIONS: VECTOR FUNCTIONS & DATA FUNCTIONS ----

# Calculating a 3 month rolling average  for category_1 & category_2 
# with dates aligned at last day of the month
#   __rollback() => 

rolling_avg_3_tbl <-  bike_orderlines_tbl %>%
    
    select(order_date, category_1, category_2, total_price) %>%
    mutate(order_date = ymd(order_date)) %>%
    mutate(month_end = ceiling_date (order_date, unit = "month")) %>% 
    mutate(month_end = rollback(month_end))  %>%
    
    # Grouped calc's = Moyenne mobile sur 3 mois consécutifs ----
    group_by(category_1, category_2, month_end) %>%
    summarise(total_price = sum(total_price)) %>%
    mutate(rolling_avg_3 = rollmean(total_price, k = 3, na.pad = TRUE, align = "right")) %>%
    ungroup() %>%
    
    mutate(category_2 = as_factor(category_2) %>% fct_reorder2(month_end, total_price))



rolling_avg_3_tbl %>%
    ggplot(aes(x = month_end, y = total_price, color = category_2)) + 
    
    # Geometries
    geom_point() + 
    geom_line(aes(y = rolling_avg_3), color = "blue", size = 1) + 
    facet_wrap(~ category_2, scales = "free_y") + 
    
    # FOrmatting 
    theme_tq() + 
    scale_color_tq()
    
bikes_tbl <- bike_orderlines_tbl %>%
    distinct(model, category_1, price)

# Visualise Box plot (tibble) ----
bikes_tbl %>% 
    ggplot(aes(x = category_1, y = price)) +
    geom_boxplot()


# 2.1 Vector Functions ----


# 2.2 Data Functions ----


# 3.0 CONTROLLING FLOW: IF STATEMENTS, MESSAGES, WARNINGS, STOP ----

#   IF-STATEMENT : Controlling Flow => Great for checking User Input to Functions ----

#   __ message()  => print a message to the user ----
#   __warning()   => allows function to continue ----
#   __stop()      => Errors stop - Does not allow function to continue (if the criteria is not verified) ----

class_detect <- function(x) {
    
    if (is.numeric(x)) {
        message("Value is numeric")
        print(x)
    } else if (!is.character(x)) {
        warning("In class_detect(): value is character! should be numeric, but can be accepted", call. = FALSE)
        print(x)
        else if (is.logical(x)) {
            stop("In class_detect(): value is logical ! shoulb be numeric. Definitely cannot be accepted", call. = FALSE)
        print(x)
        else {
            message ("Unknow Class")
            print(x)
        }
        }
    }
}

formula(y ~ x) %>% class_detect()

# 4.0 VECTORIZED REMOVE OUTLIERS FUNCTION ----
#  - Box Plot Diagram to Identify Outliers
#  - Goal: Use box plot approach to identify outliers

# Make bikes_tbl ----

bikes_tbl <- bike_orderlines_tbl %>%
    distinct(model, category_1, price)

# Visualize Box plot (tibble) ----

bikes_tbl %>% 
    ggplot(aes(x = category_1, y = price)) +
    geom_boxplot()

# Create detect_outliers()

x <- c(0:10, 50, NA_real_)
x

# FUNCTION THAT TAKES A VECTOR in ARGUMENT ----

detect_outliers <- function(x)
    
{
    if (missing (x)) stop("The argument x needs a vector.")
    if (!is.numeric(x)) stop("the argument x must be numeric.")
    
    #   __quantile(data, probs = quantile 25, 70, 50%, na.rm = TRUE (handle NA data))
    
    data_tbl <- tibble(data = x)
    
    limits_tbl <- data_tbl %>%
        summarise(
            quantile_lo = quantile(data, probs = 0.25, na.rm = TRUE), 
            quantile_hi = quantile(data, probs = 0.75, na.rm = TRUE), 
            iqr = IQR(data, na.rm = TRUE), 
            limit_lo = quantile_lo - 1.5 * iqr, 
            limit_hi = quantile_hi + 1.5 * iqr
        )
    output_tbl <- data_tbl %>%
        mutate(outlier = case_when(
            data < limits_tbl$limit_lo ~ TRUE, 
            data > limits_tbl$limit_hi ~ TRUE, 
            TRUE ~ FALSE
        ))
    
    #   __output_tbl$outlier => result is a vector($)
    return(output_tbl$outlier)
}

tibble(x = x)

x %>% detect_outliers()

# Apply detect_outliers() to bikes_tbl ----

bike_outliers_tbl <- bikes_tbl %>%
    group_by(category_1) %>%
    mutate(outlier = detect_outliers(price)) %>%
    ungroup()

# Visualize with detect_outliers() 
bike_outliers_tbl %>% 
    
    ggplot(aes(category_1, price)) + 
    geom_boxplot() + 
    
#   __ggrepel::geom_label_repel() => display text ----
    ggrepel::geom_label_repel(aes(label = model), 
                              color = "red", 
                              size = 3, 
                              data = .%>%
                                  filter(outlier))


# Create remove_outliers()



# Apply remove_outliers() to bikes_tbl



# Visualize with remove_outlers()





# 5.0 DATA FUNCTION: FEATURE ENGINEERING ----
#  - Goal: Want to simplify the text feature engineering steps to convert model name to features


# Pipeline Comes From 02_data_wrangling/04_text.R
bikes_tbl %>%
    
    select(model) %>%
    
    # Fix typo
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
        model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
        TRUE ~ model
    )) %>%
    
    # separate using spaces
    separate(col     = model, 
             into    = str_c("model_", 1:7), 
             sep     = " ", 
             remove  = FALSE, 
             fill    = "right") %>%
    
    # creating a "base" feature
    mutate(model_base = case_when(
        
        # Fix Supersix Evo
        str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Fat CAAD bikes
        str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Beast of the East
        str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
        
        # Fix Bad Habit
        str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Scalpel 29
        str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
        
        # catch all
        TRUE ~ model_1)
    ) %>%
    
    # Get "tier" feature
    mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
    
    # Remove unnecessary columns
    select(-matches("[0-9]")) %>%
    
    # Create Flags
    mutate(
        black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
        team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
        ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
        disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    )

#   __create a function for a tibble dataset ----

data <- bikes_tbl

separate_bike_function <- function(data, keep_model_column = TRUE, append = TRUE) {
    
    # Append 
    if (!append) {
        data <- data %>% select()
    }
    
    # Pipeline 
    
    output_tbl <- data %>% 
        
    #select(model) %>% --> cannot be active since it's in append section 
        
        # Fix typo
        mutate(model = case_when(
            model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
            model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
            model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
            TRUE ~ model
        )) %>%
        
        # separate using spaces
        separate(col     = model, 
                 into    = str_c("model_", 1:7), 
                 sep     = " ", 
                 remove  = FALSE, 
                 fill    = "right") %>%
        
        # creating a "base" feature
        mutate(model_base = case_when(
            
            # Fix Supersix Evo
            str_detect(str_to_lower(model_1), "supersix") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Fat CAAD bikes
            str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Beast of the East
            str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
            
            # Fix Bad Habit
            str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
            
            # Fix Scalpel 29
            str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
            
            # catch all
            TRUE ~ model_1)
        ) %>%
        
        # Get "tier" feature
        mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
        
        # Remove unnecessary columns
        select(-matches("[0-9]")) %>%
        
        # Create Flags
        mutate(
            black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
            hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
            team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
            red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
            ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
            dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
            disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
        )
    if(!keep_model_column) output_tbl <- output_tbl %>% select(-model)
    
    return(output_tbl)
    
}

bikes_tbl %>% separate_bike_function(keep_model_column = TRUE, append = TRUE)



# 6.0 SAVING AND SOURCING FUNCTIONS ----

# 6.1 Create folder and file ----
fs::dir_create("00_scripts/")

path <- "../00_scripts/separate_bikes_and_outlier_detection.R"

fs::file_create(path)

# 6.2 Build and add header ----
file_header_text <- str_glue("
                             # SEPARATE BIKE MODELS AND DETECT OUTLIERS
                             # Library(tidyverse)"
)

#   __write_lines() => write inside a file ----

write_lines(file_header_text, path = path)

#   __file.edit() => modify a file ----

file.edit("../00_scripts/separate_bikes_and_outlier_detection.R")

# 6.3 Add functions with dump() ----

#   __dump() => write functions to a file ----

c("class_detect", "detect_outliers") %>%
    dump(file = "../00_scripts/separate_bikes_and_outlier_detection.R", 
    append = TRUE)


# 6.4 Source function ----

#   __remove a function (rm)
#   __source a function ==> call the function (from a directory) you want to use ----

rm("class_detect")
rm("detect_outliers")
source("../00_scripts/separate_bikes_and_outlier_detection.R")

# 
