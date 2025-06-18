crime_data <- expand.grid(
  Evidence = c("Direct\n Evidence", "Circumstantial\n Evidence", "No Evidence"),
  Suspect = c("Suspect", "No Suspect"),
  Charged = c("Charged", "Not Charged")
)

homicides_data <- expand.grid(
  Evidence = c("Blunt Implement", "Knife/\nSharp Implement", "Physical Assault"),
  Suspect = c("Female", "Male"),
  Charged = c("Solved", "Unsolved")
)

#write.csv(crime_data, "crime_data.csv", row.names = FALSE)

event_tree_asymmetric <- create_event_tree(crime_data, 1:3, label_type = "names", level_separation = 1000)
delete_nodes(event_tree_asymmetric, c("s8", "s9", "s18", "s19", "s20", "s21"))

event_tree_symmetric <- create_event_tree(homicides_data, 1:3, label_type = "both", level_separation = 1000)

node_groups <- list(c("s1", "s2"), c("s3"), c("s4", "s6"), c("s5", "s7"), c("s8"), c("s9"))
homicides_Colours <- update_node_colours(event_tree_symmetric, node_groups = node_groups, colours =  c("#92dce5", "#CEBBC9", "#008FCC", "#fe5f55", "#6a0", "#FFB400"), level_separation = 1000)
homicides_Colours

priors <- specify_priors(homicides_Colours, "Uniform")
homicides_ST <- staged_tree_prior(homicides_Colours, priors, label_type = "priors", level_separation = 1000, node_distance = 350)
homicides_ST

homicides_CEG <- create_ceg(homicides_ST, view_table = TRUE, label = "posterior_mean", level_separation = 1400)
homicides_CEG

london_boroughs <- st_read(shapefile_path)

