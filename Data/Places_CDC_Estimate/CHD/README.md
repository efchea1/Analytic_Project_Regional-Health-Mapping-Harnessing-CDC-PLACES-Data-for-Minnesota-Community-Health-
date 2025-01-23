Data was downloaded locally and then filtered in R from each data source using the following script.

dplyr::filter(MeasureId == "CHD") |> 
  # Step 2: Export filtered data to a CSV file
  write.csv("Path to save file")
