load("public_git/scrape_sondaggipoliticoelettorali.it/wikipedia_data/environment.RData")

# Plot
library(ggplot2)

col_scale <- scale_colour_manual(name = "party_color", values = color_palette)

svg("public_git/scrape_sondaggipoliticoelettorali.it/wikipedia_data/ita_mean_polls_2013_2018.svg")
ggplot(wikipedia_data_ts, 
       aes(x = date.class, y = value, colour = variable)) + 
  geom_point(alpha = .3, size = .4) +
  geom_line(aes(y = value_ma_60d), size = 1.1) +
  geom_point(data = wikipedia_data_elections, shape = 13, size = 7, alpha = .7) +
  geom_vline(xintercept = unique(wikipedia_data_elections$date.class), colour = 'white', linetype = 'dotdash') +
  col_scale +
  guides(colour=FALSE) +
  theme(panel.background = element_rect(fill = 'grey')) + labs(x=NULL, y="%") +
  scale_x_date(limits = c(as.Date('2013-01-01'), max(wikipedia_data_ts$date.class)))
dev.off()

