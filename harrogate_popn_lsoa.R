# 2017 MYE from Nomis - at LSOA level for Harrogate 
# for all ages by single year
# (could easily do a single number for all ages combined instead)

library(jsonlite)
library(tidyverse)
library(ggplot2)
library(wesanderson)
library(extrafont)


# List of FIDs of all the Harrogate LSOAs
lsoa_fids <- paste0(
  "1249929546,1249929547,1249929565,1249929566,1249929592...1249929595,1249929523,1249929568,1249929616,1249929617,1249929585...",
  "1249929591,1249929524,1249929525,1249929527,1249929567,1249929561,1249929573,1249929581,1249929582,1249929522,1249929526,",
  "1249929554,1249929555,1249929521,1249929544,1249929545,1249929602,1249929548,1249929549,1249929553,1249929556,1249929518...",
  "1249929520,1249929570,1249929618,1249929619,1249929621,1249929569,1249929571,1249929600,1249929601,1249929603,1249929550...",
  "1249929552,1249929584,1249929529,1249929531,1249929537,1249929538,1249929572,1249929620,1249929528,1249929530,1249929606...",
  "1249929609,1249929532,1249929535,1249929539,1249929557,1249929558,1249929560,1249929563,1249929564,1249929575,1249929576,1249929583,1249929540...",
  "1249929543,1249929577,1249929613,1249929562,1249929574,1249929614,1249929615,1249929536,1249929559,1249929610...",
  "1249929612,1249929533,1249929534,1249929578,1249929596...1249929599,1249929579,1249929580,1249929604,1249929605"
)

# URL to get the JSON data from Nomis, broken down for clarity/editability (?)
json_source <- paste0(
  "https://www.nomisweb.co.uk/api/v01/dataset/",
  "NM_2010_1.jsonstat.json",
  "?geography=",
  lsoa_fids, # see above
  "&date=latest",
  "&gender=0", # all
  "&c_age=200,201,203,209,101...191", # 4 age groups followed by 91 single year categories (prob didn't need all of these)
  "&measures=20100")

# csv alternative to JSON:
# https://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249929546,1249929547,1249929565,1249929566,1249929592...1249929595,1249929523,1249929568,1249929616,1249929617,1249929585...1249929591,1249929524,1249929525,1249929527,1249929567,1249929561,1249929573,1249929581,1249929582,1249929522,1249929526,1249929554,1249929555,1249929521,1249929544,1249929545,1249929602,1249929548,1249929549,1249929553,1249929556,1249929518...1249929520,1249929570,1249929618,1249929619,1249929621,1249929569,1249929571,1249929600,1249929601,1249929603,1249929550...1249929552,1249929584,1249929529,1249929531,1249929537,1249929538,1249929572,1249929620,1249929528,1249929530,1249929606...1249929609,1249929532,1249929535,1249929539,1249929557,1249929558,1249929560,1249929563,1249929564,1249929575,1249929576,1249929583,1249929540...1249929543,1249929577,1249929613,1249929562,1249929574,1249929614,1249929615,1249929536,1249929559,1249929610...1249929612,1249929533,1249929534,1249929578,1249929596...1249929599,1249929579,1249929580,1249929604,1249929605&date=latest&gender=0&c_age=200&measures=20100
# http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1249929546,1249929547,1249929565,1249929566,1249929592...1249929595,1249929523,1249929568,1249929616,1249929617,1249929585...1249929591,1249929524,1249929525,1249929527,1249929567,1249929561,1249929573,1249929581,1249929582,1249929522,1249929526,1249929554,1249929555,1249929521,1249929544,1249929545,1249929602,1249929548,1249929549,1249929553,1249929556,1249929518...1249929520,1249929570,1249929618,1249929619,1249929621,1249929569,1249929571,1249929600,1249929601,1249929603,1249929550...1249929552,1249929584,1249929529,1249929531,1249929537,1249929538,1249929572,1249929620,1249929528,1249929530,1249929606...1249929609,1249929532,1249929535,1249929539,1249929557,1249929558,1249929560,1249929563,1249929564,1249929575,1249929576,1249929583,1249929540...1249929543,1249929577,1249929613,1249929562,1249929574,1249929614,1249929615,1249929536,1249929559,1249929610...1249929612,1249929533,1249929534,1249929578,1249929596...1249929599,1249929579,1249929580,1249929604,1249929605&date=latest&gender=0&c_age=101...191&measures=20100&select=geography_name,geography_code,c_age,c_age_name,obs_value


# read JSON into an R data object
lsoa_data <- jsonlite::fromJSON(json_source)

# create a REALLY long list (9880) by extracting the label for each LSOA and then repeating each one 95 times;
# NB *not* getting the list and repeating the list 95 times - the order matters
lsoa_label <- lapply(X = lsoa_data[["dimension"]][["geography"]][["category"]][["label"]],
                FUN = rep, each = 95) %>% 
                  unlist()
population <- lsoa_data[["value"]] %>% unlist()
# something wrong here - there was a variable `age_cat` for the number of age categories in the data request (95)
# but I have lost track of how I made this. Replacing with simple number 95 for now.
# num_lsoas <- length(population)/length(age_cat)
num_lsoas <- length(population)/95

# create a list of all the 95 age group labels multiplied by `num_lsoas` (number of LSOAs)
# This list should now be the same length as `lsoa_label` and `population`
# Note that the parameter for `rep` here is different from when we made `lsoa_label`...
# (`times` not `each`) ... we're repeating the list in order 104 times
age_group <- rep(lsoa_data[["dimension"]][["c_age"]][["category"]][["label"]],
                 times = num_lsoas) %>%
                    unlist()

# Now we have three lists to be combined into a tibble
popn_data <- tibble(
  lsoa_label,
  age_group,
  population)

# get a list of all the LSOAs total populations and sort them from most to least
# we need this step in order to order the next stage `age_brackets` properly by total pop
age_order <- popn_data %>% 
    filter(age_group == "All Ages") %>% 
      arrange(desc(population))

# get the data we actually want to work with: by LSOA and by age bracket
age_brackets <- popn_data %>% 
  filter(age_group %in% c("Aged 0 to 15", "Aged 16 to 64", "Aged 65+")) %>% 
    # fct_reorder(lsoa_label, population, .desc = TRUE)
      left_join(y = age_order, by = "lsoa_label") %>% 
        select(-age_group.y) %>% # remove unneeded column
          rename(age_group = age_group.x, total_popn = population.y, bracket_popn = population.x) %>% 
            # `desc = TRUE` matters here because it puts the highest pop LSOA at the left of the chart
            # we're factorising the list here by label and reordering by popn
            mutate(lsoa_label = fct_reorder(lsoa_label, total_popn, .desc = TRUE)) %>% 
              arrange(desc(total_popn))

# age_brackets$age_group.x <- as_factor(age_brackets$age_group.x)
# pivot_wider(id_cols = lsoa_label, names_from = age_group, values_from = population) %>% 

# set up the variables for the plot below
quartiles <- round(quantile(age_order$population, probs = seq(1, 0, length = 5), names = TRUE))
labels <- c("max:", "Q3:", "median:", "Q1:", "min:")
names(quartiles) <- labels
x_pos <- c(13, 33, 53, 73, 93)
# Wes Anderson palette FTW
wes_pal <- wes_palette("FantasticFox1", 3, type = "discrete")

# for each quartile this function will add a line and a label to the chart
add_lines <- function(x, y, z) {
  list(
    geom_hline(yintercept = y, linetype = "dashed", colour = "grey25"),
    geom_label(aes(x = z, y = y),
               fill = "grey25",
               colour = "white",
               label = paste(x, y),
               label.r = unit(0, "mm"),
               label.padding = unit(0.25, "lines"),
               label.size = 0,
               size = 5)
  )
}

# set up the chart
popn_age_plot <- age_brackets %>%
  ggplot(aes(
    x = lsoa_label,
    y = bracket_popn,
    fill = age_group)) +
  geom_col() +
    scale_fill_manual(values = wes_pal) +
  labs(
    x = "Harrogate Lower Super Output Areas (LSOAs) (n=104)",
    y = "Population",
    title = "Residents by LSOA, Harrogate, showing quartiles",
    caption = "Source: ONS (Nomis), 2017",
    fill = "Age bracket") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = "grey90"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    text = element_text(family = c("Source Sans Pro", "sans")),
    axis.title = element_text(size = 16, face = "bold"),
    title = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
  )

popn_age_plot + mapply(add_lines, labels, quartiles, x_pos)




