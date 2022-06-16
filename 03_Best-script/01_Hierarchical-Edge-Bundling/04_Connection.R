# This script gets connections and detect overlapping connections
# Note: This script consider only the complete dataset
rm(list=ls())

directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '[^litreview_visualization]+') %>% file.path("litreview_visualization")

{
  data_path = file.path(directory,'00_Data xlsx/')
  funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
  preprocessing_path = file.path(directory,'03_Best-script/00_Data-Preprocessing/')
  script_path = file.path(directory,'03_Best-script/01_Hierarchical-Edge-Bundling/')
  result_path = file.path(directory,'04_Results/')
  descriptives_path = file.path(directory,'05_Descriptives/')
}

# Step 0. Load necessary functions ====
source(file.path(funcon_path, "PN_GetConnection.R"))
source(file.path(funcon_path, "PN_GetConnection_OverlapOrUniqueLabel.R"))

# Get dataset (must be binarized)
df <- read_excel(file.path(data_path,"Lit_Review_binarized.xlsx"))

identifier_var <- c("Entry_ID", "Authors","Title", "Year")


# Step 5. Create connections ====

# get connections
connect <- PN_GetConnection(data=df, identifier_var = identifier_var)
# look for overlapping edges
connect_OverlapOrUnique <- PN_GetConnection_OverlapOrUniqueLabel(connect)



{
  write_xlsx(connect, file.path(data_path, "Lit_Review_Connect.xlsx")) 
  message('Create ', file.path(data_path, "Lit_Review_Connect.xlsx"))
  
  write_xlsx(connect, file.path(descriptives_path, "Lit_Review_Connect.xlsx")) 
  message('Create ', file.path(descriptives_path, "Lit_Review_Connect.xlsx"))
  
  write_xlsx(connect_OverlapOrUnique, file.path(data_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  message('Create ', file.path(data_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  
  write_xlsx(connect_OverlapOrUnique, file.path(descriptives_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  message('Create ', file.path(descriptives_path, "Lit_Review_connect_OverlapOrUnique.xlsx"))
  
  

}

