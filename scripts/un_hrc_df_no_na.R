# Loading packages
pacman::p_load(tidyverse, performance, readr)

# Loading the df
original_df <- read.csv("did_hrc_un.csv")
gdp_per_capita_ppp <- read.csv("imf_gdp_per_capita_ppp.csv")
democracy <- read.csv("democracy_index.csv")
hr_index <- read.csv("hr_index.csv")
interstate_conflict <- read.csv("interstate_conflicts.csv")
intrastate_conflict <- read.csv("intrastate_conflicts.csv")

# Correcting president of HRC column
main_df <- original_df %>%
  mutate(president_council = ifelse(is.na(president_council), 0,
                                    president_council))

# Adding the continent column to add the regional trend column
## Vector for countries and their continents
country_to_continent <- c(
  # Africa
  "Algeria" = "Africa", "Angola" = "Africa", "Benin" = "Africa", "Botswana" = "Africa",
  "Burkina Faso" = "Africa", "Burundi" = "Africa", "Cameroon" = "Africa",
  "Cape Verde" = "Africa", "Central African Republic" = "Africa", "Chad" = "Africa",
  "Comoros" = "Africa", "Congo" = "Africa", "Cote d'Ivoire" = "Africa", "Democratic Republic of Congo" = "Africa",
  "Djibouti" = "Africa", "Egypt" = "Africa", "Equatorial Guinea" = "Africa",
  "Eritrea" = "Africa", "Eswatini" = "Africa", "Ethiopia" = "Africa", "Gabon" = "Africa",
  "Gambia" = "Africa", "Ghana" = "Africa", "Guinea" = "Africa", "Guinea-Bissau" = "Africa",
  "Kenya" = "Africa", "Lesotho" = "Africa", "Liberia" = "Africa", "Libya" = "Africa",
  "Madagascar" = "Africa", "Malawi" = "Africa", "Mali" = "Africa", "Mauritania" = "Africa",
  "Mauritius" = "Africa", "Morocco" = "Africa", "Mozambique" = "Africa", "Namibia" = "Africa",
  "Niger" = "Africa", "Nigeria" = "Africa", "Rwanda" = "Africa", "Sao Tome and Principe" = "Africa",
  "Senegal" = "Africa", "Seychelles" = "Africa", "Sierra Leone" = "Africa", "Somalia" = "Africa",
  "Somaliland" = "Africa", "South Africa" = "Africa", "South Sudan" = "Africa",
  "Sudan" = "Africa", "Tanzania" = "Africa", "Togo" = "Africa", "Tunisia" = "Africa",
  "Uganda" = "Africa", "Zambia" = "Africa", "Zanzibar" = "Africa", "Zimbabwe" = "Africa",
  
  # Asia
  "Afghanistan" = "Asia", "Armenia" = "Asia", "Azerbaijan" = "Asia", "Bahrain" = "Asia",
  "Bangladesh" = "Asia", "Bhutan" = "Asia", "Cambodia" = "Asia", "China" = "Asia",
  "Cyprus" = "Asia", "East Timor" = "Asia", "Georgia" = "Asia", "India" = "Asia",
  "Indonesia" = "Asia", "Iran" = "Asia", "Iraq" = "Asia", "Israel" = "Asia",
  "Japan" = "Asia", "Jordan" = "Asia", "Kazakhstan" = "Asia", "Kuwait" = "Asia",
  "Kyrgyzstan" = "Asia", "Laos" = "Asia", "Lebanon" = "Asia", "Malaysia" = "Asia",
  "Maldives" = "Asia", "Mongolia" = "Asia", "Myanmar" = "Asia", "Nepal" = "Asia",
  "North Korea" = "Asia", "Oman" = "Asia", "Pakistan" = "Asia", "Philippines" = "Asia",
  "Qatar" = "Asia", "Saudi Arabia" = "Asia", "Singapore" = "Asia", "South Korea" = "Asia",
  "Sri Lanka" = "Asia", "Syria" = "Asia", "Tajikistan" = "Asia", "Thailand" = "Asia",
  "Turkey" = "Asia", "Turkmenistan" = "Asia", "United Arab Emirates" = "Asia",
  "Uzbekistan" = "Asia", "Vietnam" = "Asia", "Yemen" = "Asia",
  
  # Europe
  "Albania" = "Europe", "Austria" = "Europe", "Belarus" = "Europe", "Belgium" = "Europe",
  "Bosnia and Herzegovina" = "Europe", "Bulgaria" = "Europe", "Croatia" = "Europe",
  "Czechia" = "Europe", "Denmark" = "Europe", "Estonia" = "Europe", "Finland" = "Europe",
  "France" = "Europe", "Germany" = "Europe", "Greece" = "Europe", "Hungary" = "Europe",
  "Iceland" = "Europe", "Ireland" = "Europe", "Italy" = "Europe", "Latvia" = "Europe",
  "Lithuania" = "Europe", "Luxembourg" = "Europe", "Malta" = "Europe", "Moldova" = "Europe",
  "Montenegro" = "Europe", "Netherlands" = "Europe", "North Macedonia" = "Europe",
  "Norway" = "Europe", "Poland" = "Europe", "Portugal" = "Europe", "Romania" = "Europe",
  "Russia" = "Europe", "Serbia" = "Europe", "Slovakia" = "Europe", "Slovenia" = "Europe",
  "Spain" = "Europe", "Sweden" = "Europe", "Switzerland" = "Europe", "Ukraine" = "Europe",
  "United Kingdom" = "Europe",
  
  # Oceania
  "Australia" = "Oceania", "Fiji" = "Oceania", "New Zealand" = "Oceania",
  "Papua New Guinea" = "Oceania", "Solomon Islands" = "Oceania", "Vanuatu" = "Oceania",
  
  # North America
  "Barbados" = "North America", "Canada" = "North America", "Costa Rica" = "North America",
  "Cuba" = "North America", "Dominican Republic" = "North America", "El Salvador" = "North America",
  "Guatemala" = "North America", "Haiti" = "North America", "Honduras" = "North America",
  "Jamaica" = "North America", "Mexico" = "North America", "Nicaragua" = "North America",
  "Panama" = "North America", "Trinidad and Tobago" = "North America",
  "United States" = "North America",
  
  # South America
  "Argentina" = "South America", "Bolivia" = "South America", "Brazil" = "South America",
  "Chile" = "South America", "Colombia" = "South America", "Ecuador" = "South America",
  "Guyana" = "South America", "Paraguay" = "South America", "Peru" = "South America",
  "Suriname" = "South America", "Uruguay" = "South America", "Venezuela" = "South America"
)

## Adding the continent column
main_df <- main_df %>%
  mutate(continent = country_to_continent[countries])

## Filtering for the regional trend
continents_key <- c("Africa", "Asia", "Europe", "Oceania", "North America", 
                    "South America")

regional_trend <- hr_index %>%
  select(continent, year, regional_civil_liberties_index) %>%
  filter(year >= 2006 & continent %in% continents_key)

## Merging regional_trend and main_df 
main_df <- main_df %>%
  left_join(regional_trend, by = c("continent", "year"))

# Adding the gdp per capita ppp column
## Pivoting the table
gdp_per_capita_ppp <- gdp_per_capita_ppp %>%
  pivot_longer(
    cols = starts_with("X"),  
    names_to = "year",        
    values_to = "gdp_per_capita_ppp"     
  )

## Removing the x before the observations in the year column 
gdp_per_capita_ppp$year <- as.numeric(gsub("^X", "", gdp_per_capita_ppp$year))

## Renaming to allow merger
gdp_per_capita_ppp <- gdp_per_capita_ppp %>%
  rename(countries = GDP.per.capita..current.prices..Purchasing.power.parity..international.dollars.per.capita.)

# Renaming countries to make names compatible
gdp_per_capita_ppp <- gdp_per_capita_ppp %>%
  mutate(countries = recode(countries,
                              "Congo, Dem. Rep. of the" = "Democratic Republic of Congo",
                              "Congo, Republic of " = "Congo",
                              "Cabo Verde" = "Cape Verde",
                              "China, People's Republic of" = "China",
                              "Côte d'Ivoire" = "Cote d'Ivoire",
                              "Czech Republic" = "Czechia",
                              "Timor-Leste" = "East Timor",
                              "Gambia, The" = "Gambia",
                              "Kyrgyz Republic" = "Kyrgyzstan",
                              "Lao P.D.R." = "Laos",
                              "Russian Federation" = "Russia",
                              "São Tomé and Príncipe" = "Sao Tome and Principe",
                              "Slovak Republic" = "Slovakia",
                              "Korea, Republic of" = "South Korea",
                              "South Sudan, Republic of" = "South Sudan",
                              "Türkiye, Republic of" = "Turkey",
                              "North Macedonia " = "North Macedonia"))


## Merging gdp_per_capita_ppp and main_df 
main_df <- main_df %>%
  left_join(gdp_per_capita_ppp, by = c("countries", "year"))

## Changing the "no data" to NA
main_df <- main_df %>% 
  mutate(gdp_per_capita_ppp = ifelse(gdp_per_capita_ppp == "no data", NA, gdp_per_capita_ppp))

# Adding the armed conflict column
## Merging both dfs about conflict
armed_conflict <- interstate_conflict %>%
  inner_join(intrastate_conflict, by = c("Entity", "Year", "Code"))

## Adding a total conflict column
armed_conflict$total_conflict <- armed_conflict$Interstate + armed_conflict$Intrastate

## Selecting columns
armed_conflict <- armed_conflict %>%
  select(Entity, Year, total_conflict)

## Renaming columns 
armed_conflict <- armed_conflict %>%
  rename(countries = Entity, year = Year)

## Merging the armed_conflict and the main_df
main_df <- main_df %>%
  inner_join(armed_conflict, by = c("countries", "year"))

# Adding the Democracy column
## Selecting columns
democracy <- democracy %>%
  select(Entity, Year, Democracy.score)

## Renaming columns 
democracy <- democracy %>%
  rename(countries = Entity, year = Year, democracy_index = Democracy.score)

## Merging the mass_mob df and the main_df
main_df <- main_df %>%
  left_join(democracy, by = c("countries", "year"))

# Making the gdp_per_capita_ppp numeric
main_df$gdp_per_capita_ppp <- main_df$gdp_per_capita_ppp %>%
  as.numeric()

# Correcting the target of special rapporteur column
main_df <- main_df %>%
  mutate(target_special_rapporteur = ifelse(is.na(target_special_rapporteur), 0,
                                            target_special_rapporteur))

# Running a simple lr
model <- lm(civil_liberties_index ~ membership + world_civil_liberties_index +
              post_membership + president_council +
              target_special_rapporteur + n_rat_hr_treaties + n_accept_ind_complains +
              regional_civil_liberties_index + gdp_per_capita_ppp + total_conflict +
              democracy_index, data = main_df)
summary(model)

check_collinearity(model)

# Removing problematic variables
refined_df <- main_df %>%
  select(-c(post_membership, n_accept_ind_complains))

# Removing NA rows
refined_df <- na.omit(refined_df)

# Exporting the data frame to a CSV file
write_csv(refined_df, "refined_df.csv")
