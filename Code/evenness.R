

library(vegan)
library(dplyr)


### data ----------------------------------------------------------------------------
df <- data.frame(
  sp = c('a', 'b', 'c'),
  num= c(10, 20, 100)
)

# df <- data.frame(
#   sp = c('a', 'b', 'c'),
#   num= c(1, 11111, 102)
# )


### manual calculation --------------------------------------------------------------
richness = length(unique(df$sp)); richness
log(richness)
total <- sum(df$num); total

df <- df %>%
  dplyr::mutate(
    p = num/total,
    p_lnp = p*log(p))
H = -sum(df$p_lnp); H
evn <- H/log(richness); evn


### package calculation ------------------------------------------------------------
### change data foramt 
dt <- df %>%
  t() %>% as.data.frame()
### species name as column names
names(dt) <- df[,1] 
### remove the first row, which contains species names 
dt <- dt[-1, ] 
### all values as numeric
dt[] <- lapply(dt, function(x) as.numeric(as.character(x)))
str(dt)

### calculation 
#### Species richness (S) and Pielou's evenness (J):
H <- diversity(dt); H
S <- specnumber(dt); S ## rowSums(BCI > 0) does the same
J <- H/log(S); J
