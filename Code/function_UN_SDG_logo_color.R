

## This script provides 
##  * SDG colors
##  * SDG logo
## Reference: 
## UN Guidelines for the use of the SDG logo, including the colour wheel and 17 icons
## Link: https://www.un.org/sustainabledevelopment/wp-content/uploads/2019/01/SDG_Guidelines_AUG_2019_Final.pdf (updated in May 2020)



## SDG name ----------------------------------------------------------------------------------------
sdg_name <- paste0("SDG", seq(1, 17, 1))

## SDG color ---------------------------------------------------------------------------------------

### RGB
color_rgb <-data.frame(R=c(229,221,76, 197,255,38, 252,162,253,221,253,191,63, 10, 86, 0,  25),
                       G=c(36, 166,159,25, 58, 189,195,25, 105,19, 157,139,126,141,192,104,72),
                       B=c(59, 58, 56, 45, 33, 226,11, 66, 37, 103,36, 46, 68, 217,43, 157,106))

### HEX
color_hex <-rgb(color_rgb, max=255)
names(color_hex) <- sdg_name


### PMS


### CMYK


## SDG logo  ---------------------------------------------------------------------------------------