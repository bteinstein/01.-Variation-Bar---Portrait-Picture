library(ggplot2)
library(dplyr)
library(magick)

# Read in image and convert to grayscale
img_name <- "bt01"

img <- image_read(glue::glue(here::here("image"), "/",img_name, ".jpg")) %>%
  image_convert(colorspace = "gray")


# Get dimensions
img_w <- image_info(img)$width
img_h <- image_info(img)$height
img_ratio <- img_w / img_h

# Resize the longest dimension to 120 pixels
if (img_w >= img_h) {
  img <- image_resize(img, "120")
} else {
  img <- image_resize(img, ("x120"))
}

# Create array and number rows and columns
img_array <- drop(as.integer(img[[1]]))
rownames(img_array) <- 1:nrow(img_array)
colnames(img_array) <- 1:ncol(img_array)

# Create data frame from array and rename columns
img_df <- as.data.frame.table(img_array) %>% 
  `colnames<-`(c("y", "x", "b")) %>% 
  mutate(
    across(everything(), as.numeric),
    # map b (0-255) to bf (1-0), so that "brighter" values become smaller numbers
    bf = 1 - b / 255,
  ) %>%
  arrange(y, x)

## Make random subsets
ngroups = 50 
nr = nrow(img_df)
indices = sample(1:ngroups, size = nr, replace = T)
sets = split(x = 1:nr, f = indices) 

color_vec = c(green = "green", spot ="#FBEAFF", generic="#FF6F91", yellow = "#F9F871")
for (set in 1:ngroups) {
  indx = unlist(sets[1:set]) # marge sets 
  ggplot(img_df) +
    geom_tile(data = img_df[indx,],aes(x = x, y = y, width = bf, height = 1)) + # switch 1 and bf for vertical/horizontal bars
    scale_y_reverse() +
    coord_fixed(expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = color_vec[4], color = NA)) +
    # ggsave(glue::glue(here::here("plots/pixelated"),"/pix_s",set,".png"), width = 3.5, height = 3.5 / img_ratio)
    ggsave(glue::glue(here::here("plots","pixelated","/"),img_name,"_",set,".png"), width = 3.5, height = 3.5 / img_ratio)
}


## Animation
imglayers_mc <- sapply(1:50, function(i) {
  image_read(glue::glue(here::here("plots","pixelated","/"),img_name,"_",i,".png"))
})

anim_mc = image_animate(image_join(imglayers_mc), fps = 5,dispose = "background",optimize = T,loop = 1)

image_write(anim_mc, path = glue::glue(here::here('gif'),"/pixelated_",img_name,".gif"))



