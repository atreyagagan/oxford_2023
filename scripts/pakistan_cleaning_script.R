


# Specify the columns for which you want to calculate the SD
columns_to_include <- c(11:39, 41:66)

dspak <- as.data.frame(dspak)

# Calculate the SD for each row across specified columns
dspak$SD_across_items <- apply(dspak[, columns_to_include], 1, sd, na.rm = TRUE)

# Filter and arrange the rows where SD is below 1
rows_below_1_sd_Pakistan <- dspak %>%
    filter(SD_across_items < 1) %>%
    arrange(SD_across_items)

dspak <- dspak[, !]


PAK493
PAK309
PAK270
PAK370
PAK170
