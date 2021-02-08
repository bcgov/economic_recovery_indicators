bcstats_chart_theme <-
  theme_bw() +
  theme(
    panel.border = element_rect(colour="white"),
    plot.title = element_text(face="bold"),
    plot.caption = element_text(hjust=0),
    legend.position = c(1,0),
    legend.justification = c(1,0),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11),
    axis.line = element_line(colour="black"),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    axis.text.x = element_text(angle=90)
  )


line_colors <- c("#325A80", "#5091CD", "#00B0F0", "#B7DEE8")
# dark blue   #325A80: 50-90-128
# blue        #5091CD: 80-145-205
# bright blue #00B0F0: 0-176-240
# pale blue   #B7DEE8: 183-222-232