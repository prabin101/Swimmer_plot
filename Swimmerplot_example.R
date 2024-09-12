
####################### Swimmer plot  #######################
library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)

df <-  structure(list(subject = c("Subject-001", "Subject-002", "Subject-003", 
              "Subject-003", "Subject-004", "Subject-004", "Subject-005", "Subject-005", 
              "Subject-005", "Subject-006", "Subject-006", "Subject-007", "Subject-008", 
              "Subject-009", "Subject-010"), STAGE = structure(c("Stage 1", 
              "Stage 2", "Stage 3", "Stage 3", "Stage 4", "Stage 4", "Stage 1", 
              "Stage 1", "Stage 1", "Stage 2", "Stage 2", "Stage 3", "Stage 1", 
              "Stage 4", "Stage 2"), label = "stage"), LOW = structure(c(0, 
              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), label = "low"), HIGH = structure(c(18.5, 
              17, 14, 14, 13.5, 13.5, 12.5, 12.5, 12.5, 12.6, 12.6, 11.5, 9.5, 
              8.3, 4.2), label = "high"), HIGHCAP = c("FilledArrow", "", "FilledArrow", 
              "FilledArrow", "FilledArrow", "FilledArrow", "FilledArrow", "FilledArrow", 
              "FilledArrow", "FilledArrow", "FilledArrow", "", "", "", "FilledArrow"
              ), STATUS = structure(c("Complete response", "Complete response", 
              "Partial response", "Partial response", "Partial response", "Partial response", 
              "Complete response", "Complete response", "Partial response", 
              "Partial response", "Partial response", "Complete response", 
              "Complete response", "Partial response", "Complete response"), label = "status"), 
                  START = structure(c(6.5, 10.5, 2.5, 6, 7, 11.5, 3.5, 6.5, 
                  10.5, 2.5, 9.5, 4.5, 1, 6, 1.2), label = "start"), END = structure(c(13.5, 
                  17, 3.5, NA, 11, NA, 4.5, 8.5, NA, 7, NA, 11.5, 9.5, NA, 
                  NA), label = "end"), DURABLE = structure(c(-0.25, -0.25, 
                  -0.25, NA, NA, NA, -0.25, NA, NA, NA, NA, -0.25, -0.25, NA, 
                  NA), label = "durable")), row.names = c(NA, -15L), class = c("tbl_df", 
              "tbl", "data.frame"))

# Prepare your data similar to your example
df.shapes <- df %>%
  select(subject, STATUS, START) %>%
  reshape2::melt(id.vars = c("subject", "STATUS"), value.name = "time") %>%
  filter(!is.na(time)) %>%
  select(-variable) %>%
  mutate(STATUS = paste(STATUS, "start", sep = " "))

df.shapes <- df.shapes %>%
  bind_rows(
    df %>%
      select(subject, END, HIGHCAP) %>%
      mutate(
        # Ensure both are numeric
        END = as.numeric(END),
        HIGHCAP = as.numeric(HIGHCAP),
        END = if_else(is.na(END), HIGHCAP, END)  # Replace NA in END with HIGHCAP
      ) %>%
      filter(!is.na(END)) %>%  # Remove rows where END is still NA
      mutate(STATUS = "Response end") %>%
      rename(time = END)
  )



# Append Durable column (if it exists in your dataset)
if ("DURABLE" %in% colnames(df)) {
  df.shapes <- df.shapes %>%
    bind_rows(df %>%
                select(subject, DURABLE) %>%
                filter(!is.na(DURABLE)) %>%
                mutate(STATUS = "Durable") %>%
                rename(time = DURABLE))
}

# Add on the arrow sets for continued treatments
df.shapes <- df.shapes %>%
  mutate(HIGHCAP = as.character(HIGHCAP))

df <- df %>%
  mutate(HIGHCAP = as.character(HIGHCAP))

# Proceed with binding rows
df.shapes <- df.shapes %>%
  bind_rows(
    df %>%
      select(subject, HIGH, HIGHCAP) %>%
      filter(HIGHCAP == "FilledArrow") %>%
      mutate(
        HIGH = HIGH + 0.25,
        STATUS = "Continued Treatment"
      ) %>%
      rename(time = HIGH)
  )

responseLevels = c("Complete response start", "Partial response start", "Response end", "Durable", "Continued Treatment")

df.shapes <- df.shapes %>%
  mutate(STATUS = factor(STATUS, levels = responseLevels)) %>%
  arrange(desc(STATUS))

unicode <- list(triangle = sprintf('\u25B2'),
                circle = sprintf('\u25CF'),
                square = sprintf('\u25A0'),
                arrow = sprintf('\u2794'))

# Create the swimmer plot
plot_1 <- df %>%
  select(subject, HIGH, STAGE) %>%
  distinct() %>%
  mutate(subject = forcats::fct_reorder(subject, as.numeric(gsub("Subject-", "", subject)), .desc = TRUE)) %>%
  ggplot(aes(subject, HIGH)) +
  geom_bar(stat = "identity", aes(fill = factor(STAGE))) +  # Bar plot colored by stage
  geom_point(data = df.shapes, aes(subject, time, colour = STATUS, shape = STATUS), size = 5) +
  coord_flip() +
  scale_colour_manual(values = c(RColorBrewer::brewer.pal(3, "Set1")[1:2], "black", "black", "black")) +
  scale_shape_manual(values = c(rep(unicode[["triangle"]], 2), unicode[["circle"]], unicode[["square"]], unicode[["arrow"]])) +
  scale_y_continuous(limits = c(-0.5, 20), breaks = 0:20) +
  labs(fill = "Disease Stage", colour = "Symbol Key", shape = "Symbol Key",
       x = "Subject ID", y = "Months since diagnosis",
       title = "Swimmer Plot",
       caption = "Durable defined as subject with six months or more of confirmed response") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size = 7, hjust = 0))

# Print the plot
plot_1

################## Next Example ##################
library(ggplot2)  
library(dplyr)  
library(tidyr)  

# Sample dataset  
set.seed(123)  
final <- data.frame(  
  SUBJID = paste("Subject_",factor(1:20)),  
  LOW0 = sample(0:0, 20, replace = TRUE),  
  LOW1 = sample(0:5, 20, replace = TRUE),  
  LOW2 = sample(0:5, 20, replace = TRUE),  
  LOW3 = sample(0:5, 20, replace = TRUE),  
  total_duration = sample(6:30, 20, replace = TRUE),  
  PR = sample(6:30, 20, replace = TRUE),  
  CR = sample(6:30, 20, replace = TRUE),  
  PD = sample(6:30, 20, replace = TRUE),  
  SD = sample(6:30, 20, replace = TRUE),  
  DEATH = sample(6:30, 20, replace = TRUE)  
)  

# Ensure that the event times are within the total duration for each patient  
final <- final %>%  
  mutate(  
    PR = ifelse(PR > total_duration, NA, PR),  
    CR = ifelse(CR > total_duration, NA, CR),  
    PD = ifelse(PD > total_duration, NA, PD),  
    SD = ifelse(SD > total_duration, NA, SD),  
    DEATH = ifelse(DEATH > total_duration, NA, DEATH)  
  ) %>% 
  arrange(total_duration) %>%  
  mutate(SUBJID = factor(SUBJID, levels = SUBJID))


p <- ggplot(final, aes(y = SUBJID)) +  
  # Text for lines  
  geom_text(aes(x = -3, label = SUBJID), family = "Arial", size = 2.5, color = "black", fontface = "plain") +
  # HIGHLOW bars with arrows  
  geom_segment(aes(yend = SUBJID, x = LOW0, xend = total_duration + 1), color = "grey", size = 2, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +  
  geom_segment(aes(yend = SUBJID, x = LOW1, xend = total_duration + 1), color = "#ADD8E6", size = 2, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +  
  geom_segment(aes(yend = SUBJID, x = LOW2, xend = total_duration + 1), color = "#b6ed72", size = 2, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +  
  geom_segment(aes(yend = SUBJID, x = LOW3, xend = total_duration + 1), color = "#278712", size = 2, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +  
  
  # HIGHLOW bars without arrows  
  geom_segment(aes(yend = SUBJID, x = LOW0, xend = total_duration), color = "grey", size = 2) +  
  geom_segment(aes(yend = SUBJID, x = LOW1, xend = total_duration), color = "#ADD8E6", size = 2) +  
  geom_segment(aes(yend = SUBJID, x = LOW2, xend = total_duration), color = "#b6ed72", size = 2) +  
  geom_segment(aes(yend = SUBJID, x = LOW3, xend = total_duration), color = "#278712", size = 2) +  
  
  # Event points  
  geom_point(aes(x = PR, shape = "PARTIAL RESPONSE", color = "PARTIAL RESPONSE"), size = 3) +  
  geom_point(aes(x = CR, shape = "COMPLETE RESPONSE", color = "COMPLETE RESPONSE"), size = 3) +  
  geom_point(aes(x = PD, shape = "DISEASE PROGRESSION", color = "DISEASE PROGRESSION"), size = 3) +  
  geom_point(aes(x = SD, shape = "STABLE RESPONSE", color = "STABLE RESPONSE"), size = 3) +  
  geom_point(aes(x = DEATH, shape = "DEATH", color = "DEATH"), size = 3) +  
  
  # Legends  
  scale_shape_manual(values = c("PARTIAL RESPONSE" = 16, "COMPLETE RESPONSE" = 16, "DISEASE PROGRESSION" = 15, "STABLE RESPONSE" = 16, "DEATH" = 15)) +  
  scale_color_manual(values = c("PARTIAL RESPONSE" = "#b6ed72", "COMPLETE RESPONSE" = "#278712", "DISEASE PROGRESSION" = "#FFD580", "STABLE RESPONSE" = "#ADD8E6", "DEATH" = "red")) +  
  guides(shape = guide_legend(title = NULL, override.aes = list(size = 3), order = 2),  
         color = guide_legend(title = NULL, override.aes = list(size = 3), order = 2)) +  
  
  # Axis labels and scales  
  xlab("Duration in Months") +  
  ylab("") +  
  scale_x_continuous(breaks = seq(0, 30, by = 3), labels = seq(0, 30, by = 3)) +  
  
  # Theme  
  theme_minimal() +  
  theme(  
    panel.grid = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.line.x = element_line(color = "black"),  
    axis.title = element_text(size = 10),  
    axis.text.x = element_text(size = 8),  
    legend.position = c(0.9, 0.1),  
    legend.text = element_text(size = 8)  
  )

print(p)  

