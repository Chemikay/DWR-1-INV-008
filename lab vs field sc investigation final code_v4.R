library(writexl)
library(tidyverse)
library(data.table)
library(readxl)
library(janitor)
library(here)

# Pull in all original WDL data sets into one dataframe
# Unzip folder containing WDL data to a temporary directory
unzip(zipfile = here("data/raw/Raw files from WDL_restored copy.zip"), exdir = tempdir())

filelist <- list.files(
  path = file.path(tempdir(), "Raw files from WDL - restored copy"),    
  pattern = "*.xlsx$",
  full.names = FALSE,
  recursive = TRUE
) 

gfg_data <- 
  list.files(
    file.path(tempdir(), "Raw files from WDL - restored copy"),    
    pattern = "*.xlsx$",
    full.names = TRUE,
    recursive = TRUE
  ) %>%
  lapply(read_excel) 

attr(gfg_data, "names") <- filelist

gfg_data2 <- rbindlist(gfg_data, idcol= "id", fill = TRUE)

gfg_data2 <- clean_names(gfg_data2)

View(gfg_data2)

names(gfg_data2)

# now read in the data from Kelley (data from her internal WDL for time period
  # not captured, some overlap)
kelley <- read_excel(here("data/raw/SC Field and Lab_Dec2022-Feb2023_SomeProjects.xlsx"))

kelley <- clean_names(kelley)

names(kelley)

# need to recreate the 'unique_field' and 'unique_lab' data for this new dataset
kelley <- mutate(kelley, id = paste(data_owner_id, data_owner))

names(gfg_data2)
names(kelley)
unique(kelley$lab_method_name)
unique(kelley$lab_method_title)
str(kelley)

# standardize the names, reformat date field and lab result field
kelley <- mutate(kelley, lab_result = as.numeric(lab_result),
                field_analyte_name = paste(field_fraction_name, field_measure_name),
                collection_date2 = as.character(format(collection_date, format = "%m/%d/%Y %H:%M")))

unique(kelley$field_analyte_name)

unique(gfg_data2$analyte)
names(gfg_data2)

# handle O&M bottom conductivity which is comparable to lab value per Daniel
  # Wisheropp
gfg_data2a <- gfg_data2 %>%
  mutate(
    analyte1a = case_when(
      data_owner =="2040" & analyte == "Field (Bottom) Specific Conductance" ~ "Field Specific Conductance", 
      TRUE ~ analyte
    )
  )

unique(gfg_data2a$analyte1a)

# filter out all other methods not related to conductivity, move data under
  # Field EC into Field SC because directly comparable
unique(gfg_data2a$method)
gfg_data3 <- filter(gfg_data2a, method == "Std Method 2510-B [1]*" | method == "EPA 120.1 (Field) [1]*" | method == "Std Method 2510-B (Field) [1]*" | method == "Std Method 2510 B (Filtered) [1]*") %>%
  mutate(analyte2 = case_when(analyte1a == "Field Electrical Conductance" ~ "Field Specific Conductance",
                              TRUE ~ analyte1a))

str(gfg_data3)

#end addition

unique(gfg_data3$analyte2)

# filter out bottom field SC for per Morgan Battey for DEMP
# as field SC for DEMP is done at same depth as discrete sample, bottom SC
  # collected as well at different depth for permit compliance
gfg_data4 <- filter(gfg_data3, analyte2 == "Field Specific Conductance" | analyte == "Specific Conductance")

unique(gfg_data4$analyte2)


# need to separate out field SC from lab SC, manage case when values for both
  # field bottom SC and field SC for single sample code, per Morgan Battey
# use field SC as this is done at same depth as discrete sample, bottom SC
  # collected as well at different depth for permit compliance
gfg_data4_wide <- gfg_data4 %>% 
  pivot_wider(id_cols = c(long_station_name, data_owner, sample_code, collection_date),
              names_from = analyte2, values_from = result,
              values_fill = NA, values_fn = first) 


gfg_data4_wide <- clean_names(gfg_data4_wide)

# turn less than into the number it's less than
gfg_data5 <- mutate(gfg_data4_wide, field_specific_conductance = as.numeric(str_remove(field_specific_conductance, "<")))

gfg_data6 <- mutate(gfg_data5, specific_conductance = as.numeric(str_remove(specific_conductance, "<")))


# filter out bottom SC per Morgan Battey, 
# as field SC is done at same depth as discrete sample, bottom SC collected as
  # well at different depth for permit compliance
kelley1 <- filter(kelley, field_analyte_name == "Specific Conductance" | field_analyte_name == "Electrical Conductance")

# move data under Field EC into Field SC because directly comparable
kelley2 <- mutate(kelley1, field_analyte_name_2 = case_when(field_analyte_name == "Electrical Conductance" ~ "Specific Conductance",
                                                           TRUE ~ field_analyte_name))  


# select column names to keep for kelley code when binding to gfg_data6 code
kelley3 <- select(kelley2, data_owner_id, dwr_sample_code, collection_date2, field_result, lab_result)  

# rename columns in gfg_data6 to match kelley3
gfg_data6 <-  gfg_data6 %>% 
  rename(
    'data_owner_id' = 'data_owner',
    'dwr_sample_code' = 'sample_code',
    'collection_date2' = 'collection_date',
    'field_result' = 'field_specific_conductance',
    'lab_result' = 'specific_conductance'
  )

# bind data for gfg_data6 and kelley3
data_bound <- bind_rows(gfg_data6, kelley3)

# Look for NA values in the field and lab results
data_bound %>% summarize(across(everything(), ~ sum(is.na(.x))))

# Both the field and lab results have NA values. Rows with NA values in either
  # of the result columns need to be removed
data_bound2 <- data_bound %>% drop_na(lab_result, field_result)

# Look for duplicates using data owner, sample code, and collection date as
  # unique identifiers
data_bound2 %>% 
  count(data_owner_id, dwr_sample_code, collection_date2) %>% 
  filter(n > 1)

# There are 190 duplicates, these are either from lab duplicates or duplicates
  # from crossover on the original data sheets
# We'll remove these by selecting the first entry
data_bound3 <- data_bound2 %>% distinct(data_owner_id, dwr_sample_code, collection_date2, .keep_all = TRUE)

# Check if there are any duplicates using just sample code as unique identifier
  # since this should be enough
data_bound3 %>% count(dwr_sample_code) %>% filter(n > 1)

# There are 6 sample codes that still have duplicate records
# We'll take a closer look to see why that is
data_bound3 %>% 
  count(dwr_sample_code) %>% 
  filter(n > 1) %>%
  select(-n) %>% 
  left_join(data_bound3, by = join_by(dwr_sample_code))

# 5 of these are from different data owners, but are otherwise identical
# 1 of these is from different collection times, but are also otherwise identical
# We can safely clean up these duplicates by using the sample code as the unique identifier
data_bound4 <- data_bound3 %>% distinct(dwr_sample_code, .keep_all = TRUE)

# All duplicates should be remove now, but let's check
data_bound4 %>% 
  count(dwr_sample_code) %>% 
  filter(n > 1)

# All duplicate records are removed
#perform RPD calculation in data_bound4
data_bound_rpd <- data_bound4 %>% 
  mutate(rpd =  100*((field_result-lab_result)/(field_result + lab_result))/2)

#export R file 
write.csv(data_bound_rpd,here("data/processed/RPD data from full data set_v5.csv"))

           