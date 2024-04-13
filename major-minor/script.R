library(tidyverse)
library(taylor)
library(scales)
library(janitor)
library(showtext)

font_add_google("Manrope", "man")
showtext_auto()

# Example data structure (you'll have your actual data)
songs_data <- taylor_album_songs |> 
  select(key_name, mode_name)

# Assuming you have a data frame called `song_data` with columns 'key_name' and 'track_name'.
# First, calculate the percentage of songs for each key.
percentages <- songs_data %>%
  count(key_name) %>%
  mutate(percent = n / sum(n))

# Merge this with your white_keys and black_keys data frames. This code may change depending on your data's structure.
 white_keys <- data.frame(
  x_min = 0:6,
  x_max = 1:7,
  y_min = rep(0, 7),
  y_max = rep(1, 7)
)

# Define positions for the black keys where they exist
black_keys <- data.frame(
  x_min = c(0.7, 1.7, 3.7, 4.7, 5.7),
  x_max = c(1.3, 2.3, 4.3, 5.3, 6.3),
  y_min = rep(0.6, 5),
  y_max = rep(1, 5)
)

# Define positions for the white keys
white_keys <- white_keys %>%
  mutate(key_name = c('C', 'D', 'E', 'F', 'G', 'A', 'B'))

black_keys <- black_keys %>%
  mutate(key_name = c('C#', 'D#', 'F#', 'G#', 'A#'))

# Then, merge the percentage data with the keys.
white_keys <- merge(white_keys, percentages, by = "key_name", all.x = TRUE)
black_keys <- merge(black_keys, percentages, by = "key_name", all.x = TRUE)

# Now create the ggplot with annotations.
p <- ggplot() +
  geom_rect(data = white_keys, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max), fill = "white", color = "black") +
  geom_rect(data = black_keys, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max), fill = "black", color = "black") +
  geom_point(data = white_keys, aes(x = (x_min + x_max) / 2, y = 0.3, color = percent), size = 25) +
  geom_point(data = black_keys, aes(x = (x_min + x_max) / 2, y = 0.7, color = percent), size = 25) +
  geom_text(data = white_keys, aes(x = (x_min + x_max) / 2, y = 0.3, label = key_name), color = "white", size = 20,family = "man") +
  geom_text(data = black_keys, aes(x = (x_min + x_max) / 2, y = 0.7, label = key_name), color = "black", size = 20,family = "man") +
  scale_color_gradient(low = "#e0aaff", high = "#3c096c", labels = percent,name = "% of Songs") +
  guides(fill = guide_colorsteps(labels = scales::percent)) +
  #guides(color = guide_legend(title = "% of songs")) +
  theme_void() +
  theme(legend.position = "top",
        legend.margin = margin(t = 37, unit = "pt"),
        legend.text = element_text(size = 30, color = "#030303",family = "man"),
        legend.title =  element_text(size = 30),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  theme(legend.key.width = unit(3.5, 'cm')) +
  theme(plot.title  = element_text(color="black", size=140, hjust = 0.5, family = "man")) +
  theme(plot.caption = element_text(color="black", size=50, hjust = 0.5, family = 'man')) +
  coord_fixed(ratio = 4) +
  labs(title = "Taylor Swift Songs by Key",
       caption = "Data: Taylor R Package   Graphic: Deepali Kank")

# Display the plot
p
