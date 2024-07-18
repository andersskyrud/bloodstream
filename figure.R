# Figure for the letter to the editor
# Use of bacteraemia vs bloodstream infection
# Written by Anders Skyrud Danielsen
# 1 July 2024

# Load required libraries
pacman::p_load(tidyverse,
               viridis)

# Import the CSV file
df <- read_delim("C:/Users/ANSD/OneDrive - Folkehelseinstituttet/Forskning og analyse/ResCan/Samarbeid/Bacteraemia vs BSI/PubMed_Timeline_Results_by_Year.csv", 
               col_names = TRUE, delim = ";")

# Create a function to categorize the years into intervals
categorize_years <- function(year) {
  case_when(
    year >= 1945 & year <= 1947 ~ "1945-1947",
    year >= 1948 & year <= 1950 ~ "1948-1950",
    year >= 1951 & year <= 1953 ~ "1951-1953",
    year >= 1954 & year <= 1956 ~ "1954-1956",
    year >= 1957 & year <= 1959 ~ "1957-1959",
    year >= 1960 & year <= 1962 ~ "1960-1962",
    year >= 1963 & year <= 1965 ~ "1963-1965",
    year >= 1966 & year <= 1968 ~ "1966-1968",
    year >= 1969 & year <= 1971 ~ "1969-1971",
    year >= 1972 & year <= 1974 ~ "1972-1974",
    year >= 1975 & year <= 1977 ~ "1975-1977",
    year >= 1978 & year <= 1980 ~ "1978-1980",
    year >= 1981 & year <= 1983 ~ "1981-1983",
    year >= 1984 & year <= 1986 ~ "1984-1986",
    year >= 1987 & year <= 1989 ~ "1987-1989",
    year >= 1990 & year <= 1992 ~ "1990-1992",
    year >= 1993 & year <= 1995 ~ "1993-1995",
    year >= 1996 & year <= 1998 ~ "1996-1998",
    year >= 1999 & year <= 2001 ~ "1999-2001",
    year >= 2002 & year <= 2004 ~ "2002-2004",
    year >= 2005 & year <= 2007 ~ "2005-2007",
    year >= 2008 & year <= 2010 ~ "2008-2010",
    year >= 2011 & year <= 2013 ~ "2011-2013",
    year >= 2014 & year <= 2016 ~ "2014-2016",
    year >= 2017 & year <= 2019 ~ "2017-2019",
    year >= 2020 & year <= 2022 ~ "2020-2022"
  )
}

# Rename the 'Year' column to lowercase to match the mutate function
df <- df %>%
  rename(year = Year)

# Apply the categorization function to the years
df <- df %>%
  mutate(interval = categorize_years(year))

# Sum the number of publications by interval
df_interval <- df %>%
  group_by(interval) %>%
  summarize(total_publications = sum(Count, na.rm = TRUE))

# Numerator data from the figure
numerator_data <- tibble(
  interval = c("1945-1947", "1948-1950", "1951-1953", "1954-1956", "1957-1959", "1960-1962", 
               "1963-1965", "1966-1968", "1969-1971", "1972-1974", "1975-1977", "1978-1980", 
               "1981-1983", "1984-1986", "1987-1989", "1990-1992", "1993-1995", "1996-1998", 
               "1999-2001", "2002-2004", "2005-2007", "2008-2010", "2011-2013", "2014-2016", 
               "2017-2019", "2020-2022"),
  bloodstream_infections = c(0, 0, 0, 2, 0, 0, 1, 1, 1, 1, 8, 7, 9, 14, 30, 31, 99, 164, 
                             323, 551, 818, 1196, 1813, 2457, 2921, 3972)
)

# Merge the numerator data with the interval data and remove rows with NA in the interval column
merged_df <- df_interval %>%
  left_join(numerator_data, by = "interval") %>%
  filter(!is.na(interval)) %>%
  mutate(percentage = (bloodstream_infections / total_publications) * 100)

# Plot the data
figure1 <- ggplot(merged_df, aes(x = interval, y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = 'Years', y = 'Percentage of all publications') +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Export the plot to the same folder as the source data
ggsave("C:/Users/ANSD/OneDrive - Folkehelseinstituttet/Forskning og analyse/ResCan/Samarbeid/Bacteraemia vs BSI/figure1.png", 
       plot = figure1, width = 10, height = 6, dpi = 300)