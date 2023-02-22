
str(healthcare.dataset.stroke.data)


table(healthcare.dataset.stroke.data)
# Delete the "bmi" column
healthcare.dataset.stroke.data$bmi <- NULL
str(healthcare.dataset.stroke.data)

library(ggplot2)

# Create a frequency table of stroke occurrence
stroke_freq <- table(healthcare.dataset.stroke.data$stroke)

# Convert the table to a data frame
stroke_df <- as.data.frame(stroke_freq)

# Rename the columns
names(stroke_df) <- c("stroke", "count")

# Create a bar plot of stroke occurrence
library(ggplot2)



# Create a bar plot of stroke occurrence with title, subtitle, and caption
ggplot(stroke_df, aes(x = stroke, y = count, fill = factor(stroke))) +
  geom_bar(stat = "identity") +
  xlab("Stroke Occurrence") +
  ylab("Count") +
  ggtitle("Distribution of Stroke Occurrence in the Dataset") +
  labs(subtitle = "Data from the healthcare dataset") +
  labs(caption = "Source: https://www.kaggle.com/fedesoriano/stroke-prediction-dataset") +
  scale_fill_manual(values = c("#FF9999", "#66CCFF")) +
  guides(fill = FALSE)
  

##_________##_-------------------------------###

# Create a scatter plot of age and avg_glucose_level
ggplot(healthcare.dataset.stroke.data, aes(x = age, y = avg_glucose_level)) +
  geom_point(aes(color = factor(stroke))) +
  labs(title = "Age and Glucose Level vs. Stroke Occurrence",
       subtitle = "Dataset of 5,110 patients",
       x = "Age",
       y = "Average Glucose Level",
       color = "Stroke Occurrence") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) 


##_________##_-------------------------------###



library(dplyr)
library(ggplot2)

# Filter the data for patients who never smoked and those who formerly smoked or currently smoke
smoking_stroke_data <- healthcare.dataset.stroke.data %>%
  filter(smoking_status %in% c("never smoked", "formerly smoked", "smokes"))

# Calculate the percentage of stroke occurrences for each smoking status group
smoking_stroke_percentages <- smoking_stroke_data %>%
  group_by(smoking_status, stroke) %>%
  summarize(n = n()) %>%
  mutate(stroke_percentage = n/sum(n)*100)

# Create a stacked bar chart
ggplot(smoking_stroke_percentages, aes(x = smoking_status, y = stroke_percentage, fill = factor(stroke))) +
  geom_bar(stat = "identity") +
  labs(title = "Stroke Occurrence by Smoking Status",
       subtitle = "Dataset of 4,272 patients",
       x = "Smoking Status",
       y = "Percentage of Stroke Occurrences",
       fill = "Stroke Occurrence") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#3366CC", "#DC3912"), name = NULL)  +
  guides(fill = guide_legend(reverse = TRUE))


#----------------------##----------------------------------##_---------

#plotting stroke occurrence by ever married status:


# Load the ggplot2 library
library(ggplot2)

# Calculate the stroke occurrence by marriage status
marriage_percentages <- healthcare.dataset.stroke.data %>%
  group_by(ever_married, stroke) %>%
  summarise(count = n()) %>%
  mutate(stroke_percentage = count / sum(count) * 100)

# Filter the data to include only stroke occurrences
stroke_percentages <- marriage_percentages %>%
  filter(stroke == 1)

# Create a bar plot of stroke occurrence by marriage status
ggplot(stroke_percentages, aes(x = ever_married, y = stroke_percentage, fill = ever_married)) +
  geom_bar(stat = "identity") +
  labs(title = "Stroke Occurrence by Marriage Status",
       subtitle = "Dataset of 5,110 patients",
       x = "Marriage Status",
       y = "Percentage of Patients with Stroke",
       fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#3366CC", "#DC3912")) +
  guides(fill=FALSE)


###__________##_________________________________________##_____

library(ggplot2)

# Create a data frame with age and stroke occurrence
age_stroke <- data.frame(age = healthcare.dataset.stroke.data$age,
                         stroke = ifelse(healthcare.dataset.stroke.data$stroke == 1, "Yes", "No"))

# Create a histogram with age and stroke occurrence
ggplot(age_stroke, aes(x = age, fill = stroke)) +
  geom_histogram(binwidth = 5) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), name = "Stroke") +
  labs(title = "Age and Stroke Occurrence",
       subtitle = "Dataset of 5,110 patients",
       x = "Age",
       y = "Count") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) 




##_______________________________####__________________####_____________________


#relationship between rural/urban location and stroke occurrence


library(dplyr)

stroke_percentages <- healthcare.dataset.stroke.data %>%
  group_by(Residence_type) %>%
  summarize(stroke_percentage = mean(stroke) * 100)

library(ggplot2)

ggplot(stroke_percentages, aes(x = Residence_type, y = stroke_percentage, fill = Residence_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Stroke Occurrence by Residence Type",
       subtitle = "Dataset of 5,110 patients",
       x = "Residence Type",
       y = "Stroke Occurrence Percentage",
       fill = "Residence Type") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#3366CC", "#DC3912")) +
  guides(fill=FALSE) 


##________________________###_____________________________####


#plot male and female stroke data

ggplot(healthcare.dataset.stroke.data, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Stroke Occurrence by Gender",
       subtitle = "Dataset of 5,110 patients",
       x = "Gender",
       y = "Number of Strokes",
       fill = "Stroke Occurrence") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) 

####------------------##############_--------------#####


library(ggplot2)

# Create a data frame with counts of strokes by work type and gender
work_type_stroke_counts <- healthcare.dataset.stroke.data %>% 
  group_by(work_type, gender, stroke) %>%
  summarise(count = n()) %>%
  ungroup()

# Create a stacked bar chart of work type vs stroke counts, with fill color representing gender
ggplot(work_type_stroke_counts, aes(x = work_type, y = count, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Stroke Occurrence by Work Type",
       subtitle = "Dataset of 4,245 patients",
       x = "Work Type",
       y = "Count of Strokes",
       fill = "Gender") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) 


####--------------#############-------------############------------


