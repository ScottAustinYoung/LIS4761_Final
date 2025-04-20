
# ========================
# Medicare Association Rule Mining – Tampa Bay Region
# Christopher Reddish – Final Project
# ========================

# ---- Setup ----
# Install packages if not already installed
if (!requireNamespace("arules")) install.packages("arules")
if (!requireNamespace("readr")) install.packages("readr")
if (!requireNamespace("dplyr")) install.packages("dplyr")

library(readr)
library(dplyr)
library(arules)

# ---- Step 1: Load Dataset and Filter by Tampa Bay ZIP Codes ----

df <- read_csv("Florida_Medicare_2022.csv")


# ---- Step 2: Filter by Tampa Bay ZIP Codes ----

tampa_bay_zips <- as.numeric(c(
  "34423","34428","34429","34430","34431","34432","34433","34434","34436","34442","34445",
  "34446","34447","34448","34449","34450","34451","34452","34453","34461","34465","34601","34602","34604","34605",
  "34606","34607","34608","34609","34613","34614","33510","33511","33527","33534","33547","33548","33549","33556",
  "33558","33559","33563","33565","33566","33567","33569","33570","33572","33573","33578","33579","33584","33586",
  "33592","33594","33596","33602","33603","33604","33605","33606","33607","33609","33610","33611","33612","33613",
  "33614","33615","33616","33617","33618","33619","33620","33621","33624","33625","33626","33629","33634","33635",
  "33637","33647","34201","34202","34203","34205","34207","34208","34209","34210","34211","34212","34215","34216",
  "34217","34219","34221","34222","33523","33524","33525","33537","33539","33540","33541","33542","33543","33544",
  "33545","33576","34610","34637","34638","34639","34652","34653","34654","34655","34667","34668","34669","34673",
  "34674","34679","34680","34690","34691","33701","33702","33703","33704","33705","33706","33707","33708","33709",
  "33710","33711","33712","33713","33714","33715","33716","33755","33756","33759","33760","33761","33762","33763",
  "33764","33765","33767","33770","33771","33772","33773","33774","33776","33777","33778","33781","33782","33785",
  "34677","34681","34683","34684","34685","34688","34689","33801","33803","33805","33809","33810","33811","33812",
  "33813","33815","33823","33825","33830","33834","33837","33838","33839","33841","33843","33844","33847","33849",
  "33850","33851","33853","33859","33860","33867","33868","33870","33873","33875","33876","33877","33880","33881",
  "33882","33884","33896","33898","34231","34232","34233","34234","34235","34236","34237","34238","34239","34240",
  "34241","34242","34243","34251","34275","34276","34277","34285","34286","34287","34288","34289","34292","34293",
  "34295"
))

tampa_df <- filtered_data %>%
  filter(Provider_Zip %in% tampa_bay_zips)

# ---- Step 3: Clean for Rule Mining ----

# Further filtering for rule mining:
# - Ensure required fields are not missing
# - Drop records with trivial payment values (< $10)
# - Convert NPI and HCPCS to character
# - Convert Provider_Type to factor for grouping
clean_df <- tampa_df %>%
  filter(
    !is.na(Provider_Type),
    !is.na(HCPCS_Code),
    !is.na(NPI),
    as.numeric(Avg_Medicare_Payment) >= 10
  ) %>%
  mutate(
    NPI = as.character(NPI),
    HCPCS_Code = as.character(HCPCS_Code),
    Provider_Type = as.factor(Provider_Type)
  )

# ---- Step 4: Association Rules by Specialty ----

# Generate association rules within each specialty group
# This avoids mixing procedures from unrelated specialties
specialty_groups <- clean_df %>%
  group_by(Provider_Type) %>%
  group_split()

detailed_summaries <- list()

for (spec_df in specialty_groups) {
  spec_name <- as.character(unique(spec_df$Provider_Type))
  if (nrow(spec_df) < 2) next
  
  # Group each provider's procedure codes
  provider_tx <- spec_df %>%
    group_by(NPI) %>%
    summarise(items = list(unique(HCPCS_Code)), .groups = "drop")
  
  tx_list <- split(provider_tx$items, provider_tx$NPI)
  tx_list <- lapply(tx_list, unlist)
  if (length(tx_list) < 2) next
  
  # Convert list into arules transactions
  trans <- as(tx_list, "transactions")
  
  # Apply Apriori algorithm
  # - support: at least 1% of providers use the combination
  # - confidence: rule holds in at least 50% of relevant transactions
  # - maxlen = 2 for pair rules only
  rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.5, maxlen = 2))
  if (length(rules) == 0) next
  
  # Filter to meaningful rules: lift >= 2
  # Lift = observed support / expected support
  # A lift of 2 means the items appear together twice as often as expected by chance
  rules_df <- as(rules, "data.frame") %>%
    filter(lift >= 2)
  if (nrow(rules_df) == 0) next
  
  # Format rule labels for clarity
  rules_df$lhs <- labels(lhs(rules[as.integer(rownames(rules_df))]))
  rules_df$rhs <- labels(rhs(rules[as.integer(rownames(rules_df))]))
  rules_df$rule_label <- paste(rules_df$lhs, "=>", rules_df$rhs)
  
  # Map rules to NPIs
  provider_rule_map <- list()
  for (i in seq_len(nrow(rules_df))) {
    lhs_items <- unlist(strsplit(gsub("[{}]", "", rules_df$lhs[i]), ","))
    rhs_items <- unlist(strsplit(gsub("[{}]", "", rules_df$rhs[i]), ","))
    all_items <- unique(trimws(c(lhs_items, rhs_items)))
    for (npi in names(tx_list)) {
      if (all(all_items %in% tx_list[[npi]])) {
        provider_rule_map[[npi]] <- c(provider_rule_map[[npi]], rules_df$rule_label[i])
      }
    }
  }
  
  # Summarize rules per provider
  provider_summary <- data.frame(
    Specialty = spec_name,
    NPI = names(provider_rule_map),
    Total_Rules = sapply(provider_rule_map, function(x) length(unique(x))),
    stringsAsFactors = FALSE
  )
  
  # Add count of unique codes used by each provider
  hcpcs_diversity <- spec_df %>%
    group_by(NPI) %>%
    summarise(Unique_Codes = n_distinct(HCPCS_Code), .groups = "drop")
  
  provider_summary <- provider_summary %>%
    left_join(hcpcs_diversity, by = "NPI") %>%
    mutate(
      Rules_Per_Code = round(Total_Rules / Unique_Codes, 2),
      Unique_Codes_Per_Rule = round(Unique_Codes / Total_Rules, 4)
    )
  
  detailed_summaries[[spec_name]] <- provider_summary
}

# ---- Step 5: Final Filtering ----

# Combine all specialty rule summaries
all_providers <- bind_rows(detailed_summaries)

# Justification for filters:
# - Total_Rules >= 40: Based on Q3 + IQR threshold (75th percentile ~28 + IQR ~12)
# - Unique_Codes <= 60: Removes overly broad coding patterns
final_provider_results <- all_providers %>%
  filter(Total_Rules >= 40, Unique_Codes <= 60)

write_csv(final_provider_results, "provider_rules_summary.csv")

# ---- Step 6: Enrich Claims ----

# Merge summary statistics back into the original filtered dataset
full_df <- clean_df %>% mutate(NPI = as.character(NPI))
provider_summary <- final_provider_results %>% mutate(NPI = as.character(NPI))

merged_df <- full_df %>%
  left_join(provider_summary, by = c("NPI", "Provider_Type" = "Specialty"))

# Save enriched dataset
write_csv(merged_df, "TampaDataArules.csv")
