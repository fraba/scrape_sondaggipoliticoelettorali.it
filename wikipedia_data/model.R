formatDate <- function(x, year) {
  
  x <- gsub("31 Nov", "30 Nov", x)
  
  if (grepl("\\d{5}", x)) {
    return(as.character(as.Date("1900-01-01") + as.numeric(x) - 2)) # This is how Excel stores numbers
  } else if (grepl("[A-Za-z]{3}(-|–)\\d{1,2}", x)) {
    return(as.character(as.Date(paste0(gsub("^(.*)(-|–)", "", x), " ", year), "%d %b %Y")))
  } else {
    return(as.character(as.Date(paste0(gsub("^\\d+(-|–)", "", x), " ", year), "%d %b %Y")))
  }
}

excel_file <- "public_git/scrape_sondaggipoliticoelettorali.it/wikipedia_data/wikipedia_data.xlsx"

library(readxl)
sheets <- excel_sheets(excel_file)

wikipedia_data <- data.frame()

library(reshape2)
library(stringr)
for (s in sheets) {
  this_dat <- read_excel(excel_file, sheet = s)
  this_dat <- this_dat[,-c(1:2)]
  this_dat <- reshape2::melt(this_dat, id.vars = c("Date.3", "Polling firm"))
  this_dat$sheet <- s
  this_dat$date.class <- as.Date(sapply(this_dat$Date.3, formatDate, year = str_extract(s, "\\d{4}")))
  wikipedia_data <- rbind(wikipedia_data, this_dat)
}

wikipedia_data <- subset(wikipedia_data, !variable %in% c("Lead", "Date text", "Others", "Oth."))
wikipedia_data$value <- as.numeric(wikipedia_data$value)
wikipedia_data <- subset(wikipedia_data, !is.na(value))
wikipedia_data$variable <- droplevels(wikipedia_data$variable)

wikipedia_data_elections <- 
  subset(wikipedia_data, `Polling firm` %in% c('General Election','EP Election'))
wikipedia_data <-
  subset(wikipedia_data, !`Polling firm` %in% c('General Election','EP Election'))

color_palette <- 
  c("M5S"    = '#FFEB3B', 
    "PD"     = '#F0001C',
    "FI"     = '#0087DC',
    "PdL/FI" = '#0087DC',
    "PdL"    = '#0087DC',
    "NCD"    = '#5C9EDB',
    "AP"     = '#5C9EDB',
    "LN"     = '#008800',
    "SI"     = '#C80815',
    "SEL/SI" = '#C80815',
    "SEL"    = '#C80815',
    "RC"     = '#FF6600',
    "FdI"    = '#00005A',
    "UdC"    = '#87CEFA',
    "MDP"    = '#D13241',          
    "CP"     = '#F87431', 
    "SC"     = '#1560BD',
    "IdV"    = '#FF8000',
    "FLI"    = '#1C39BB',
    "VTR"    = '#1560BD',
    "CD"     = '#FF9900'
    )

wikipedia_data$party_color <- color_palette[match(wikipedia_data$variable, names(color_palette))]

# Moving average
library(zoo)

seq_days <- seq(from = min(wikipedia_data$date.class), 
                to = max(wikipedia_data$date.class),
                by = '1 day')
variables <- unique(wikipedia_data$variable)
polling_firms <- unique(wikipedia_data$`Polling firm`)


empty_df <- expand.grid(seq_days, variables, polling_firms)
names(empty_df) <- c('date.class', 'variable', 'Polling firm')
empty_df$party_color <- color_palette[match(empty_df$variable, names(color_palette))]

wikipedia_data_ts <- wikipedia_data[,c('party_color', 'date.class', 'variable', 'value', 'Polling firm')]

library(dplyr)
library(zoo)
wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color) %>%
  dplyr::arrange(date.class) %>%
  dplyr::mutate(value_loess = loess(value ~ as.numeric(date.class))$fitted)

wikipedia_data_ts <- 
  merge(wikipedia_data_ts,
        empty_df,
        by = c('variable', 'date.class', 'party_color', 'Polling firm'), all = T)


wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, `Polling firm`) %>%
  dplyr::arrange(date.class) %>%
  dplyr::mutate(value_interpolated = na.approx(value, na.rm = F))

wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, `Polling firm`, date.class) %>%
  dplyr::mutate(value_interpolated = mean(value_interpolated, na.rm = T))

wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, `Polling firm`) %>%
  dplyr::arrange(date.class) %>%
  dplyr::mutate(value_firm_ma_30d = rollapply(value_interpolated, 30, mean, na.rm = T, fill = NA))

wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, `Polling firm`, date.class) %>%
  dplyr::mutate(value_firm_ma_30d = mean(value_firm_ma_30d, na.rm = T))
  
wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, date.class) %>%
  dplyr::mutate(value_ma_30d = mean(value_firm_ma_30d, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(party_color) %>%
  dplyr::arrange(date.class) %>%
  dplyr::mutate(value_ma_30d = rollapply(value_ma_30d, 30, mean, na.rm = T, fill = NA))

wikipedia_data_ts <- 
  wikipedia_data_ts %>%
  dplyr::group_by(party_color, date.class) %>%
  dplyr::mutate(value_ma_60d = mean(value_firm_ma_30d, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(party_color) %>%
  dplyr::arrange(date.class) %>%
  dplyr::mutate(value_ma_60d = rollapply(value_ma_60d, 60, mean, na.rm = T, fill = NA))


save(wikipedia_data_elections, wikipedia_data, wikipedia_data_ts, color_palette,
     file = "public_git/scrape_sondaggipoliticoelettorali.it/wikipedia_data/environment.RData")

