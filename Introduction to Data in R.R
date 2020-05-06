# Install packages
install.packages('openintro')

# Load packages
library(openintro)
library(dplyr)

# Load ggplot2
library(ggplot2)

# Load data
data("hsb2")
data("email50")

# View the structure of the data
str(email50)

# Glimpse email50
glimpse(email50)

# Subset of emails with big numbers: email50_big
email50_big <- email50 %>%
  filter(number == 'big')

# Glimpse the subset
glimpse(email50_big)

# Table of the number variable
table(email50_big$number)

# Drop levels
# Dropping the levels of the number variable gets rid of the levels with counts of zero
email50_big$number_dropped <- droplevels(email50_big$number)

# Table of the number_dropped variable
table(email50_big$number_dropped)

# Calculate median number of characters: med_num_char
med_num_char <- median(email50$num_char)

# Create num_char_cat variable in email50
email50_fortified <- email50 %>%
  mutate(num_char_cat = ifelse(num_char < med_num_char, "below median", "at or above median"))

# Count emails in each category
email50_fortified %>%
  count(num_char_cat)


# Combining levels of a different factor
# Create number_yn column in email50
email50_fortified <- email50 %>%
  mutate(
    number_yn = case_when(
      # if number is "none", make number_yn "no"
      number == "none" ~ "no", 
      # if number is not "none", make number_yn "yes"
      number != "none" ~ "yes"  
    )
  )

# Visualize the distribution of number_yn
ggplot(email50_fortified, aes(x = number_yn)) +
  geom_bar()

# Scatterplot of exclaim_mess vs. num_char
ggplot(email50, aes(x = num_char, y = exclaim_mess, color = factor(spam))) +
  geom_point()

# Load data
data("UCBAdmissions")

ucb_admit <- as.data.frame(UCBAdmissions)
ucb_admission_counts <- ucb_admit %>% count(Gender,Admit)

# Count number of male and female applicants admitted
ucb_admit %>%
  count(Gender, Admit)

# Calculate the proportion of males admitted overall
ucb_admission_counts %>%
  # Group by gender
  group_by(Gender) %>%
  # Create new variable
  mutate(prop = n / sum(n)) %>%
  # Filter for admitted
  filter(Admit == "Admitted")

# Proportion of males admitted for each department
ucb_admission_counts <- ucb_admit %>%
  # Counts by department, then gender, then admission status
  count(Dept, Gender, Admit)

# See the result
ucb_admission_counts

ucb_admission_counts  %>%
  # Group by department, then gender
  group_by(Dept, Gender) %>%
  # Create new variable
  mutate(prop = n / sum(n)) %>%
  # Filter for male and admitted
  filter(Gender == 'Male', Admit == 'Admitted')