library(ggplot2)

data <- data.frame(x = c("MICE", "J2R", "LOCF", "LWD"),
                         y = c(6, 4, 10, 7),
                         lower = c(4, 1, 9, 1),
                         upper = c(8, 7, 11, 14))

data$x <- factor(data$x, levels = c("Observed", "MICE", "J2R", "LOCF", "LWD"))

ggplot(data, aes(x, y)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper)) + 
  geom_hline(yintercept = 4, linetype = "dashed", size = 1, color = "#46e7fd") + 
  geom_hline(yintercept = 5, size = 2, color = "#e18b22") + 
  geom_hline(yintercept = 6, linetype = "dashed", size = 1, color = "#4739a2") + 
  labs(title = "Estimating treatment effect on sedentary behaviour", 
       subtitle = "Bias and variance of missing data treatment",
       caption = "Data source: Simulation study") + 
  xlab("Method for managing dropout") + 
  ylab("Parameter estimate") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic"))


data <- data.frame(time_point = c("t0", "t1", "t2", "t3", "t4"),
                   y = c(0, 20, 40, 60, 70))

ggplot(data=data, aes(x=time_point, y=y, fill = time_point)) +
  geom_bar(stat="identity") + 
  coord_flip() + 
  scale_fill_manual(values = c("t0" = "#FFFFFF", 
                               "t1" = "#46e7fd", 
                               "t2" = "#4739a2", 
                               "t3" = "#e18b22", 
                               "t4" = "black"), 
                    guide = "none") + 
  labs(title = "Missing data", 
       subtitle = "Increases in drop out over time",
       caption = "Data source: Hypothetical case") + 
  xlab("Measurement time point") + 
  ylab("Percentage of drop out") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic"))




data1 <- data.frame(id = c(1:40),
                   sedentary_behaviour = abs(rnorm(40, 50, 20)), 
                   walking = abs(rnorm(40, 10, 5)), 
                   moderate = abs(rnorm(40, 5, 3)),
                   vigorous = abs(rnorm(40, 2, 1)), 
                   time = "t0")

data2 <- data.frame(id = c(1:40),
                    sedentary_behaviour = abs(rnorm(40, 40, 30)), 
                    walking = abs(rnorm(40, 20, 5)), 
                    moderate = abs(rnorm(40, 10, 3)),
                    vigorous = abs(rnorm(40, 3, 1)), 
                    time = "t1")

data2$sedentary_behaviour <- 
  ifelse(data2$id %in% c(1:8), NA, data2$sedentary_behaviour)

data2$walking <- 
  ifelse(data2$id %in% c(2:8), NA, data2$walking)

data2$moderate <- 
  ifelse(data2$id %in% c(3:8), NA, data2$moderate)

data2$vigorous <- 
  ifelse(data2$id %in% c(5:8), NA, data2$vigorous)

data3 <- data.frame(id = c(1:40),
                    sedentary_behaviour = abs(rnorm(40, 20, 10)), 
                    walking = abs(rnorm(40, 30, 5)), 
                    moderate = abs(rnorm(40, 20, 3)),
                    vigorous = abs(rnorm(40, 5, 1)), 
                    time = "t2")

data3$sedentary_behaviour <- 
  ifelse(data3$id %in% c(1:16), NA, data3$sedentary_behaviour)

data3$walking <- 
  ifelse(data3$id %in% c(10:8), NA, data3$walking)

data3$moderate <- 
  ifelse(data3$id %in% c(12:8), NA, data3$moderate)

data3$vigorous <- 
  ifelse(data3$id %in% c(13:8), NA, data3$vigorous)

data4 <- data.frame(id = c(1:40),
                    sedentary_behaviour = abs(rnorm(40, 15, 10)), 
                    walking = abs(rnorm(40, 32, 5)), 
                    moderate = abs(rnorm(40, 22, 3)),
                    vigorous = abs(rnorm(40, 7, 1)), 
                    time = "t3")

data4$sedentary_behaviour <- 
  ifelse(data4$id %in% c(1:10), NA, data4$sedentary_behaviour)

data4$sedentary_behaviour <- 
  ifelse(data4$id %in% c(20:33), NA, data4$sedentary_behaviour)

data4$walking <- 
  ifelse(data4$id %in% c(35:40), NA, data4$walking)

data4$moderate <- 
  ifelse(data4$id %in% c(10:30), NA, data4$moderate)

data4$vigorous <- 
  ifelse(data4$id %in% c(1:5), NA, data4$vigorous)

data4$vigorous <- 
  ifelse(data4$id %in% c(35:40), NA, data4$vigorous)

setDT(data4)

data4 <- melt(data4, id.vars = c("id", "time"), 
              variable.name = "variable")


data <- rbind(data1, data2, data3, data4)


data4 %>%
  ggplot(aes(x=variable, y=id, fill=value))+
  geom_tile()+
  scale_fill_continuous(low = '#46e7fd', 
                        high = '#4739a2', 
                        na.value = '#e18b22', 
                        guide_legend(title = "Hours spent")) + 
  labs(title = "Missing data", 
       subtitle = "Missing data across variables at final measurement point",
       caption = "Data source: Hypothetical case") + 
  scale_x_discrete(labels = c("sedentary_behaviour" = "Sedentary", 
                              "walking" = "Walking", 
                              "moderate" = "Moderate", 
                              "vigorous" = "vigorous"))
  xlab("Activity") + 
  ylab("Cases with missingness") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        plot.title = element_text(color = "#2F2E41", size = 12, face = "bold"),
        plot.subtitle = element_text(color = "#454543", face = "bold"),
        plot.caption = element_text(color = "#454543", face = "italic"))

library(tidyverse)
