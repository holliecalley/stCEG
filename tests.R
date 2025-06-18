#stCEG tests
library(readr)
homicides <- read_csv("Documents/GitHub/stCEG Github/homicides.csv")
#homicides <- homicides[c(1:5)]
homicides <- homicides[c(9,3,2,4,5)]
homicides <- homicides %>%
  filter(Method_of_Killing != "Not Known/Not Recorded") %>%
  filter(Method_of_Killing != "Other Methods of Killing")
homicides$Method_of_Killing <- gsub("Knife or Sharp Implement", "Knife or\nSharp Implement", homicides$Method_of_Killing)
homicides$Method_of_Killing <- gsub("Physical Assault, no weapon", "Physical Assault,\nno weapon", homicides$Method_of_Killing)
homicides$Domestic_Abuse <- gsub("Domestic Abuse", "Domestic\nAbuse", homicides$Domestic_Abuse)
homicides$Domestic_Abuse <- gsub("Not Domestic Abuse", " Not Domestic\nAbuse", homicides$Domestic_Abuse)

library(stCEG)
homicides <- read_csv("data-raw/homicides.csv")
head(homicides)
homicides_ET <- create_event_tree(dataset = homicides, columns = c(2:4), level_separation = 1300, node_distance = 300, label_type = "both")
homicides_ET

homicides_FemDA <- delete_nodes(homicides_ET,level_separation = 1300, node_distance = 300, c("s15", "s16", "s19", "s20", "s23", "s24", "s27", "s28"))
homicides_FemDA

homicides_AHC <- ahc_colouring(homicides_ET, level_separation = 1300, node_distance = 300)
homicides_AHC
homicides_AHC <- update_node_colours(homicides_AHC, node_groups = list(c("s6", "s8", "s10", "s12")), colours = c("#92dce5") , level_separation = 1300, node_distance = 300)


group_judgements <- list(c("s13", "s21"), c("s5", "s9"), c("s17"), c("s25"), c("s6", "s8", "s10"), c("s12"), c("s2"), c("s4"))
colour_palette <- c("#92dce5","#C5D86D", "#f2dc5d", "#388697", "#fe5f55", "#ffaa00", "#A9E5BB", "#E79C9C")
homicides_ET_Colour <- update_node_colours(homicides_ET, node_groups = group_judgements, colours = colour_palette , level_separation = 1300, node_distance = 300)
homicides_ET_Colour

homicides_ET_AHC <- ahc_colouring(homicides_ET_Colour, level_separation = 1300, node_distance = 300)
homicides_ET_AHC

priors2 <- specify_priors(homicides_AHC, "Uniform")

homicides_ST_AHC <- staged_tree_prior(homicides_AHC, priors2, level_separation = 1300, node_distance = 300)
homicides_ST_AHC

homicides_CEG_AHC <- create_ceg(homicides_ST_AHC, view_table = TRUE, label = "posterior_mean", level_separation = 1500)

homicides_Delete <- delete_nodes(homicides_ET, c("s9", "s21", "s22"))
homicides_Delete

run_stceg()

consistent_colouring <- list(c("s13", "s21"), c("s5", "s9"), c("s17"), c("s25"), c("s6", "s8", "s10"), c("s12"), c("s2"), c("s4"), "s1", "s3", "s7", "s11", c("s14", "s15", "s23"), c("s16", "s18", "s20", "s22", "s24", "s26"), "s19", c("s27", "s28"))
consistent_colour_palette <- c("#92dce5","#C5D86D", "#f2dc5d", "#388697", "#fe5f55", "#ffaa00", "#A9E5BB", "#E79C9C", "#7987d7", "#d6e3ae", "#d6a56f", "#d7adcd", "#79d391", "#ccd3d2", "#8f52e0", "#d884d2")

homicides_ET_AHC <- update_node_colours(homicides_ET, node_groups = consistent_colouring, colours = consistent_colour_palette , level_separation = 1300, node_distance = 300)
homicides_ET_AHC

priors <- specify_priors(homicides_ET_AHC, "Custom")

homicides_ST <- staged_tree_prior(homicides_ET_AHC, priors, level_separation = 1300, node_distance = 300)
homicides_ST

homicides_CEG <- create_ceg(homicides_ST, view_table = TRUE, label = "posterior_mean", level_separation = 1500)


priors_consistent <- priors

#Takes a while to run as the model is large - please persist
homicides_spatial <- read_csv("Documents/GitHub/stCEG Github/homicides.csv")
homicides_spatial <- homicides_spatial[c(6,3,2,4,5)]
homicides_spatial <- homicides_spatial %>%
  filter(Method_of_Killing != "Not Known/Not Recorded") %>%
  filter(Method_of_Killing != "Other Methods of Killing")

#Takes a while to run as the model is large - please persist
homicides_spatial <- read_csv("Documents/GitHub/stCEG Github/homicides_filtered.csv")

homicides_ET_spatial <- create_event_tree(dataset = homicides_spatial, level_separation = 1300, node_distance = 300, label_type = "both")

homicides_AHC_spatial <- ahc_colouring(homicides_ET_spatial, level_separation = 1300, node_distance = 300)

priors <- specify_priors(homicides_AHC_spatial, "Phantom")

homicides_ST_spatial <- staged_tree_prior(homicides_AHC_spatial, priors, level_separation = 1300, node_distance = 300)

homicides_CEG_spatial <- create_ceg(homicides_ST_spatial, view_table = TRUE, label = "posterior_mean", level_separation = 1500)

london_boroughs <- st_read("/Users/holliecalley/Documents/GitHub/stCEG Github/Boroughs Shapefile")

generate_CEG_map(london_boroughs, homicides_CEG_spatial, colour_by = "Solved", color_palette = "viridis", conditionals = c("Knife or Sharp Implement", "Female"))

create_reduced_CEG(homicides_CEG_spatial, "Hillingdon") #change colours on these
create_reduced_CEG(homicides_CEG_spatial, "Hounslow") #change colours on these

summary(homicides_FemDA)

homicides_CEG_summary <- summary(homicides_CEG)

homicides_CEG_AHC_summary <- summary(homicides_CEG_AHC)

compare_ceg_models(homicides_CEG_summary,homicides_CEG_AHC_summary)

# Set seed for reproducibility
set.seed(123)

# Create a dataframe with 5 variables
df <- data.frame(
  Sentence_Length = sample(c("12 months or less", "12 months-3 years", "3 years+"), 100, replace = TRUE),
  Offence_Type = sample(c("Acquisitive", "Drug", "Vehicle"), 100, replace = TRUE),
  Sentence_No = sample(c("First Sentence", "Not First Sentence"), 100, replace = TRUE),
  Licence = sample(c("Release on licence", "Release not on licence"), 100, replace = TRUE),
  Reoffend = sample(c("Reoffend", "Don't reoffend"), 100, replace = TRUE)  # Random education levels
)

# Set seed for reproducibility
set.seed(123)

# Create a dataframe with 5 variables
df2 <- data.frame(
  Sentence_Length = sample(c("12 months or less", "12 months-3 years", "3 years+"), 2000, replace = TRUE),
  Offence_Type = sample(c("Acquisitive", "Drug", "Vehicle"), 2000, replace = TRUE),
  Sentence_No = sample(c("First Sentence", "Not First Sentence"), 2000, replace = TRUE),
  Boroughs = sample(c("Camden", "Greenwich", "Hackney", "Hammersmith and Fulham",
                      "Islington", "Kensington and Chelsea", "Lambeth", "Lewisham",
                      "Southwark", "Tower Hamlets", "Wandsworth", "Westminster"), 2000, replace = TRUE),
  Reoffend = sample(c("Reoffend", "Don't reoffend"), 2000, replace = TRUE)# Random education levels
)

write.csv(df2, "reoffending_london.csv", row.names = FALSE)


# View the first few rows
head(df)

reoffend_ET <- create_event_tree(df, c(1:3,5), level_separation = 1400, node_distance = 300, label_type = "both")
reoffend_ET

reoffend_ET_ahc2 <- ahc_colouring(reoffend_ET, level_separation = 1400, node_distance = 400)
reoffend_ET_ahc
reoffend_ET_ahc2


judgements <- list(c("s3"), c("s4", "s6", "s7", "s8", "s11"), c("s5", "s9", "s10", "s12"), c("s16", "s20", "s22", "s23", "s24", "s25","s26"), c("s14", "s30"))
colours <- c("#92dce5", "#B5E066" ,"#D8D4C7", "#82DABA", "#D87FC4")
reoffend_ET_judgements <- update_node_colours(reoffend_ET_ahc, node_groups = judgements, colours, level_separation = 1400, node_distance = 220)
reoffend_ET_judgements



priors <- specify_priors(reoffend_ET_judgements, "Uniform")

reoffend_ST <- staged_tree_prior(reoffend_ET_judgements, priors, level_separation = 1400, node_distance = 300, label_type = "priormeans")
reoffend_ST

homicides_CEG <- create_ceg(reoffend_ST, view_table = TRUE, label = "posterior_mean", level_separation = 1500)
