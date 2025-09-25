library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(lubridate)
library(purrr)
library(ggplot2)
library(broom)

# Define UDAAP keywords and regex
keywords <- c(
  "threat", "false statement", "do not owe", "debt", "frequent call", "unexpected call",
  "mislead", "unauthorized", "harass", "abuse", "unfair", "deceptive", "lie", "scam",
  "fees", "not informed", "without reason", "without notice", "fake offer",
  "wrong information", "without consent", "family called", "refused to investigate"
)
pattern <- paste0("\\b(", paste(str_replace_all(keywords, " ", "\\\\s+"), collapse = "|"), ")(s|ed|ing|ly)?\\b")

# Date-based chunking
start_date <- as.Date("2016-01-01")
end_date <- Sys.Date()
interval <- months(1)

# Initialize final data frame
final_df <- data.frame()

# Loop through monthly chunks
current_date <- start_date
while (current_date < end_date) {
  from <- current_date
  to <- min(current_date + interval - 1, end_date)
  cat("📅 Fetching complaints from", from, "to", to, "\n")
  
  res <- GET("https://www.consumerfinance.gov/data-research/consumer-complaints/search/api/v1/",
             query = list(
               has_narrative = "true",
               date_received_min = as.character(from),
               date_received_max = as.character(to),
               size = 10000,
               sort = "created_date_desc"
             ))
  
  if (status_code(res) != 200) {
    cat("❌ Error fetching from", from, "to", to, "- skipping.\n")
    current_date <- current_date + interval
    next
  }
  
  json_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  hits <- json_data$hits
  
  if (is.null(hits) || length(hits) == 0) {
    cat("🛑 No complaints found for", from, "to", to, "\n")
    current_date <- current_date + interval
    next
  }
  
  df_raw <- map_df(hits, ~ as.data.frame(.x$`_source`, stringsAsFactors = FALSE))
  cat("📦 Total complaints fetched:", nrow(df_raw), "\n")
  
  df_filtered <- df_raw %>%
    filter(!is.na(complaint_what_happened)) %>%
    mutate(narrative_clean = str_replace_all(tolower(complaint_what_happened), "[^a-z\\s]", " ")) %>%
    filter(str_detect(narrative_clean, pattern)) %>%
    select(where(~ !is.list(.)))  # remove list-cols that break binding
  
  cat("✅ UDAAP complaints matched:", nrow(df_filtered), "\n")
  
  final_df <- bind_rows(final_df, df_filtered)
  
  current_date <- current_date + interval
  Sys.sleep(0.5)
}


final_df <- final_df %>% select(-zip_code, -tags)
final_df <- final_df %>% select(-complaint_id)
final_df <- final_df %>%
  mutate(date_sent_to_company = lubridate::ymd_hms(date_sent_to_company))
final_df <- final_df %>%
  mutate(date_sent_to_company = as.Date(date_sent_to_company))

final_df <- final_df %>% select(-complaint_what_happened)
final_df <- final_df %>% select(-has_narrative)


final_df <- final_df %>%
  mutate(date_received = as.Date(date_received))


### Question 1

# Group by month and count
monthly_trend <- final_df %>%
  mutate(month = floor_date(date_received, "month")) %>%
  count(month) %>%
  arrange(month)

monthly_trend_filtered <- monthly_trend %>%
  filter(month != "2025-03-01")


ggplot(monthly_trend_filtered, aes(x = month, y = n)) +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  labs(
    title = "UDAAP Complaints Over Time",
    x = "Month",
    y = "Number of Complaints"
  ) +
  theme_minimal()

last_2_years <- monthly_trend_filtered %>%
  filter(month >= Sys.Date() %m-% months(24))

ggplot(last_2_years, aes(x = month, y = n)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "UDAAP Complaints Trend - Last 2 Years",
    x = "Month",
    y = "Complaints"
  ) +
  theme_minimal()


# Convert month to numeric index for regression
last_2_years <- last_2_years %>%
  mutate(month_num = as.numeric(month - min(month)))

# Linear trend model
trend_model <- lm(n ~ month_num, data = last_2_years)
summary(trend_model)


## To understand whether UDAAP (Unfair, Deceptive, or Abusive Acts or Practices)
## complaints have been increasing in recent years, we analyzed complaint data from
## the past two years. First, we grouped the data by month and counted the number 
## of complaints each month. Then, to identify any trends, we created a simple 
## linear regression model with the number of complaints (n) as the response 
## variable and the time in months (month_num) as the predictor. The model showed 
## that the number of complaints increases by about 1 complaint per month on 
## average. The p-value (0.0487) indicates that this upward trend is statistically 
## significant at the 5% level, meaning it's unlikely to have occurred by chance. 
## So, although the monthly increase is small, it’s a consistent and measurable 
## rise in complaints related to unfair or deceptive financial practices.



### Question 2

# 1. Create year-month column
final_df <- final_df %>%
  mutate(month = floor_date(date_received, "month"))

# 2. Count complaints per company per month
monthly_counts <- final_df %>%
  group_by(company, month) %>%
  summarise(count = n(), .groups = "drop")

# 3. Filter companies appearing in at least 24 months (to avoid noise)
valid_companies <- monthly_counts %>%
  group_by(company) %>%
  summarise(n_months = n()) %>%
  filter(n_months >= 24) %>%
  pull(company)

monthly_counts_filtered <- monthly_counts %>%
  filter(company %in% valid_companies)

# 4. Assign month number (to fit linear trend)
monthly_counts_filtered <- monthly_counts_filtered %>%
  group_by(company) %>%
  arrange(month) %>%
  mutate(month_num = as.integer(month - min(month)) + 1) %>%
  ungroup()

# 5. Fit linear model per company and extract growth slope
growth_models <- monthly_counts_filtered %>%
  group_by(company) %>%
  do(tidy(lm(count ~ month_num, data = .))) %>%
  filter(term == "month_num") %>%
  arrange(desc(estimate))  # highest positive growth on top

## Filter significant growth models (p-value < 0.05)
significant_models <- growth_models %>%
  filter(p.value < 0.05)

# Select top 10 companies
top_5_companies <- significant_models[1:3, ] 


# Filter monthly counts only for top 10 companies
top_5_names <- top_5_companies$company

top_5_data <- monthly_counts_filtered %>%
  filter(company %in% top_5_names)

# Plot
ggplot(top_5_data, aes(x = month, y = count, color = company)) +
  geom_line(size = 1) +
  labs(
    title = "Complaint Trends Over Time for Top 3 Companies",
    x = "Month",
    y = "Number of Complaints",
    color = "Company"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


### Question 4

product <- table(final_df$product)
View(product)
# Sort in decreasing order and get top 5
top_5_products <- sort(product, decreasing = TRUE)[1:5]
# View result
top_5_products

get_top_5 <- function(data, column_name) {
  col_sym <- rlang::sym(column_name)
  
  data %>%
    dplyr::filter(!is.na(!!col_sym)) %>%      # Remove NAs
    dplyr::count(!!col_sym, sort = TRUE) %>%  # Count & sort
    head(5)
}


# Top 5 issues
get_top_5(final_df, "issue")

# Top 5 sub-products
get_top_5(final_df, "sub_product")

# Top 5 sub-issues
get_top_5(final_df, "sub_issue")

#####################################

# Function to get top 5 for a single column
get_top_5_df <- function(df, col_name) {
  df %>%
    filter(!is.na(.data[[col_name]])) %>%
    count(!!sym(col_name), sort = TRUE) %>%
    slice_head(n = 5) %>%
    rename(Value = 1, Count = 2) %>%
    mutate(Column = col_name)
}

# Combine top 5 for all required columns
top5_summary <- bind_rows(
  get_top_5_df(final_df, "product"),
  get_top_5_df(final_df, "issue"),
  get_top_5_df(final_df, "sub_product"),
  get_top_5_df(final_df, "sub_issue")
) %>%
  select(Column, Value, Count)

# View the result
View(top5_summary)

### The top themes in UDAAP-related complaints revolve around debt collection
### and credit reporting. Consumers most frequently report issues such as incorrect
### information, debt not owed, and ineffective investigations. Sub-themes include 
### complaints that information belongs to someone else,
### and that investigations failed to fix errors.


# Group by company and issue, and count complaints
udaap_themes_per_bank <- final_df %>%
  group_by(company, issue) %>%
  summarise(complaint_count = n(), .groups = "drop") %>%
  arrange(company, desc(complaint_count))

top_issues_per_bank <- udaap_themes_per_bank %>%
  group_by(company) %>%
  slice_max(order_by = complaint_count, n = 3) %>%
  ungroup()


### Question 3

# Monthly unemployment rate: Feb 2023 – Mar 2025
monthly_unemp <- data.frame(
  month = seq.Date(as.Date("2023-02-01"), as.Date("2025-02-01"), by = "month"),
  unemployment_rate = c(
    3.6, 3.5, 3.4, 3.7, 3.6, 3.5, 3.8, 3.8, 3.8, 3.7, 3.7, 3.7,
    3.9, 3.8, 3.9, 4.0, 4.1, 4.3, 4.2, 4.1, 4.1, 4.2, 4.1, 4.0,
    4.1
  )
)

# Filter complaints and aggregate monthly
monthly_complaints <- final_df %>%
  filter(date_received >= as.Date("2023-02-01") & date_received <= as.Date("2025-03-01")) %>%
  mutate(month = floor_date(date_received, "month")) %>%
  count(month) %>%
  rename(complaint_count = n)

# Merge with unemployment
monthly_combined <- left_join(monthly_complaints, monthly_unemp, by = "month")

# Fit regression model
monthly_model <- lm(complaint_count ~ unemployment_rate, data = monthly_combined)
summary(monthly_model)



# Filter complaints for 2016–2024
annual_complaints <- final_df %>%
  filter(year(date_received) >= 2016 & year(date_received) <= 2024) %>%
  mutate(year = year(date_received)) %>%
  count(year) %>%
  rename(complaint_count = n)

# Actual annual unemployment rate for 2016–2024
annual_unemployment <- data.frame(
  year = 2016:2024,
  unemployment_rate = c(4.9, 4.4, 3.9, 3.7, 8.1, 5.3, 3.6, 3.6, 4.0)
)

# Merge
annual_combined <- left_join(annual_complaints, annual_unemployment, by = "year")

# Fit regression model
annual_model <- lm(complaint_count ~ unemployment_rate, data = annual_combined)
summary(annual_model)


monthly_complaints <- final_df %>%
  filter(date_received >= as.Date("2016-01-01"), date_received <= as.Date("2025-03-31")) %>%
  mutate(month = floor_date(date_received, "month")) %>%
  count(month) %>%
  rename(complaint_count = n)

# Step 2: Monthly inflation data
inflation_values <- c(
  1.4, 1, 0.9, 1.1, 1, 1, 0.8, 1.1, 1.5, 1.6, 1.7, 2.1, 2.5, 2.7, 2.4, 2.2, 1.9, 1.6,
  1.7, 1.9, 2.2, 2, 2.2, 2.1, 2.1, 2.2, 2.4, 2.5, 2.8, 2.9, 2.9, 2.7, 2.3, 2.5, 2.2,
  1.9, 1.6, 1.5, 1.9, 2, 1.8, 1.6, 1.8, 1.7, 1.7, 1.8, 2.1, 2.3, 2.5, 2.3, 1.5, 0.3,
  0.1, 0.6, 1, 1.3, 1.4, 1.2, 1.2, 1.4, 1.4, 1.7, 2.6, 4.2, 5, 5.4, 5.4, 5.3, 5.4,
  6.2, 6.8, 7, 7.5, 7.9, 8.5, 8.3, 8.6, 9.1, 8.5, 8.3, 8.3, 7.7, 7.1, 6.5, 6.4, 6,
  5, 4.9, 4.1, 3, 3.2, 3.7, 3.7, 3.2, 3.1, 3.4, 3.1, 3.2, 3.5, 3.4, 3.3, 3, 2.9,
  2.5, 2.4, 2.6, 2.7, 2.9, 3, 2.8, 2.4
)

months_seq <- seq.Date(from = as.Date("2016-01-01"), by = "month", length.out = length(inflation_values))

inflation_df <- data.frame(
  month = months_seq,
  inflation_rate = inflation_values
)

# Step 3: Join complaints with inflation data
monthly_combined <- left_join(monthly_complaints, inflation_df, by = "month")

# Step 4: Linear regression model
inflation_model <- lm(complaint_count ~ inflation_rate, data = monthly_combined)

# Step 5: Model summary
summary(inflation_model)



### Question 5

# Filter for leading banks
top_banks <- c("JPMORGAN CHASE & CO.", "DISCOVER BANK", "AMERICAN EXPRESS COMPANY", "CAPITAL ONE FINANCIAL CORPORATION")

# Filter data for top banks and clean 'timely' variable
bank_complaints <- final_df %>%
  filter(company %in% top_banks, !is.na(timely)) %>%
  mutate(timely_flag = ifelse(timely == "Yes", "Timely", "Delayed"))

# Summarize timely vs delayed complaints per bank
timely_summary <- bank_complaints %>%
  group_by(company, timely_flag) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(company) %>%
  mutate(percent = count / sum(count) * 100)

# View summary
print(timely_summary)


# Check top complaint issues for delayed responses
delayed_issues <- bank_complaints %>%
  filter(timely_flag == "Delayed") %>%
  count(company, issue, sort = TRUE) %>%
  group_by(company) %>%
  slice_max(n, n = 5)

# View top 5 delayed issues per bank
print(delayed_issues)



# Check for variations of 'American Express' and 'Capital One'
amex_matches <- unique(final_df$company[grepl("american express", final_df$company, ignore.case = TRUE)])
capitalone_matches <- unique(final_df$company[grepl("capital one", final_df$company, ignore.case = TRUE)])

# View the matches
cat("American Express matches:\n")
print(amex_matches)

cat("\nCapital One matches:\n")
print(capitalone_matches)



timely_no_df <- subset(final_df, timely == "No")
### Misleading information was given



### Question 6

library(tidytext)

# Load stopwords and Bing sentiment
data("stop_words")
bing <- get_sentiments("bing")

# Step 1: Prepare data (no filtering for specific companies)
sentiment_df <- final_df %>%
  filter(!is.na(narrative_clean), !is.na(timely)) %>%
  mutate(row_id = row_number())

# Step 1: Stratified balancing — downsample "Yes" to match "No"
yes_df <- sentiment_df %>% filter(timely == "Yes")
no_df  <- sentiment_df %>% filter(timely == "No")

set.seed(123)  # for reproducibility
yes_sample <- yes_df %>% sample_n(nrow(no_df))

balanced_df <- bind_rows(yes_sample, no_df) %>%
  mutate(row_id = row_number())


negative_sentiment <- balanced_df %>%
  unnest_tokens(word, narrative_clean) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(bing %>% filter(sentiment == "negative"), by = "word") %>%
  count(row_id, name = "neg_count") %>%
  filter(neg_count > 0)

# Step 3: Tag complaints as Negative or Not Negative
balanced_df <- balanced_df %>%
  left_join(negative_sentiment, by = "row_id") %>%
  mutate(sentiment = ifelse(!is.na(neg_count), "Negative", "Not Negative"))

# Step 4: Plot the proportion of timely responses by sentiment
ggplot(balanced_df, aes(x = sentiment, fill = timely)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Proportion of Timely Responses by Complaint Sentiment (Balanced Data)",
    x = "Complaint Sentiment",
    y = "Proportion",
    fill = "Timely Response"
  ) +
  scale_fill_manual(values = c("Yes" = "steelblue", "No" = "firebrick")) +
  theme_minimal()


# Create a contingency table
sentiment_table <- table(balanced_df$sentiment, balanced_df$timely)

# View the table
print(sentiment_table)

# Perform Chi-Square Test of Independence
chisq_test <- chisq.test(sentiment_table)

# Print test results
print(chisq_test)
