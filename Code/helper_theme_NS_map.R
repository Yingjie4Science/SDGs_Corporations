

### plot settings
unit_ns    <- 'mm'
width_1col <- 88         ## 1-column
width_2col <- 180        ## 2-column
font       <- 'sans'     ## "TT Arial"
font_size  <- 7 + 1      ##  Nature Sustainability: max = 7; min = 5
panel_labels = 'auto'    ## Nature
# panel_labels = 'AUTO'  ## Science

theme_ns <- 
  theme_bw(base_size = font_size)+
  theme(
    # axis.title =element_blank(),
    # axis.text  =element_blank(),
    # axis.ticks =element_blank(),
    # panel.background = element_rect(fill = NA),
    panel.grid.major.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_line(colour = "red", size = 1),
    text = element_text(size=font_size),
    axis.text   = element_text(size = font_size),
    strip.text  = element_text(size = font_size),
    legend.title= element_text(size =(font_size-1), face = 'bold'),
    legend.text = element_text(size =(font_size-1)),
    legend.background = element_rect(fill="transparent"),
    legend.key.size = unit(0.15,"cm")
  )


theme_map <- ggpubr::theme_transparent()+
  theme(axis.title = element_blank(),
        # axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        text = element_text(size=font_size),
        axis.text   = element_text(size = font_size),
        strip.text  = element_text(size = font_size),
        legend.title= element_text(size =(font_size-1), face = 'bold'),
        legend.text = element_text(size =(font_size-1)),
        legend.key.size = unit(0.2, "cm"),
        legend.key = element_rect(fill = NA, colour = NA, size = 0.25),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.box.background = element_rect(fill = "transparent", colour = NA))

theme_map_legend1 <- theme_map + 
  theme(legend.position = c(0.09, 0.38))


theme_map_legend2 <- theme_map + 
  theme(legend.position = c(0.7, 0.11))