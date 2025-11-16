# Cyclistic Bike-Share Analysis

## Objective
Analyze 2024 bike-share data to understand how annual members and casual riders use Cyclistic differently.

## Tools
R, tidyverse, lubridate, ggplot2, Tableau Public

## Process
- Combined 12 months of trip data
- Cleaned data and computed ride_duration
- Compared behavior by user type, weekday, and bike type

## Key Insights
- Members ride more often during weekdays
- Casuals ride longer on weekends
- Electric bikes are more popular with casual riders

## Deliverables
- [R Script](scripts/analysis.R)
- [Cleaned Data (Drive Link)](https://bit.ly/cyclistic_cleaned_2024)
- [Dashboard (Tableau)](https://public.tableau.com/app/profile/onyedikachi.ikuru)


## Generating the cleaned dataset

The cleaned Cyclistic dataset is too large to include in this repository. 
To generate it locally:

1. Download the raw CSV files into `data/raw/`.
2. Open the `data_cyclistic.R` script.
3. Run the script in R.
   - The cleaned dataset will be saved to `data/cleaned/cyclistic_2024_cleaned.csv`.
4. To get the up to date classification of the top 20 locations, run the address_classifiction.py script, with a valid gemini pro api key specified in your .env file. make sure to run this command to get the required python dependencies ``` pip install python-dotenv requests ```

---
*Google Data Analytics Capstone Project*