#' Write Weibull Shape and Scale Parameters to Workbook
#'
#' This function appends new Weibull scale and shape parameter estimates 
#' to an existing Excel workbook. It modifies the workbook in memory 
#' and returns the updated workbook object (without saving yet).
#'
#' @param wb An `openxlsx` workbook object loaded into R memory.
#' @param params_results A data frame containing parameter estimates, 
#' including `state_transition`, `scale`, `shape`, `scale_lci`, `scale_uci`, 
#' `shape_lci`, and `shape_uci` columns from the `fit_weibull_by_state` function
#' @param tumour A string indicating the tumour type (e.g., "lung", "breast").
#' @param treatment A string indicating the treatment name (e.g., "cisplatin").
#'
#' @return The modified workbook (`wb`) with new entries appended to the appropriate sheets.
#'
#' @details
#' - Appends `scale` parameters to the "1.7_Weibull_Scale_SoC" sheet.
#' - Appends `shape` parameters to the "1.6_Weibull_Shape_SoC" sheet.
#' - Adds a horizontal border (line) after each new block of inserted rows.
#' 
#' Note: You must save the workbook separately after all modifications using `saveWorkbook()`.
#'
write_shape_scale <- function(wb, params_results, tumour = "lung", treatment = "cisplatin") {
  
  scale_dat <- params_results %>%
    dplyr::mutate(tumour = tumour,
                  treatment = treatment,
                  dist = "weibull") %>%
    dplyr::select(tumour, treatment, state = state_transition, value = scale, dist, par1 = scale_lci, par2 = scale_uci)
  
  shape_dat <- params_results %>%
    dplyr::mutate(tumour = tumour,
                  treatment = treatment,
                  dist = "weibull") %>%
    dplyr::select(tumour, treatment, state = state_transition, value = shape, dist, par1 = shape_lci, par2 = shape_uci)
  
  wb <- write_shape_scale_helper(wb, param_dat = scale_dat, sheet_name = "1.7_Weibull_Scale_SoC")
  
  wb <- write_shape_scale_helper(wb, param_dat = shape_dat, sheet_name = "1.6_Weibull_Shape_SoC")
  
  return(wb)
}


#' Helper function to append parameters to a specific workbook sheet
#'
#' This helper function finds the next empty row in a specified Excel sheet, 
#' writes a block of data, adds a thin horizontal border line below the block, 
#' and returns the modified workbook.
#'
#' @param wb An `openxlsx` workbook object.
#' @param param_dat A data frame containing the block of parameters to append.
#' @param sheet_name A string specifying which sheet to write to.
#'
#' @return The modified workbook (`wb`) after writing and styling.
#'
#' @details
#' - Automatically detects the next available row based on existing data.
#' - Does not overwrite previous entries.
#' - Adds a visual separator line after each data block.
#'
#' @export
write_shape_scale_helper <- function(wb, param_dat, sheet_name = "1.6_Weibull_Shape_SoC") {
  # Read the existing sheet content
  existing_dat <- openxlsx::readWorkbook(wb, sheet = sheet_name)
  
  start_row <- nrow(existing_dat) + 2
  
  # Write the new block of parameters
  openxlsx::writeData(wb, sheet = sheet_name, x = param_dat, startRow = start_row, colNames = FALSE)
  
  # Draw a horizontal border line after the block
  openxlsx::addStyle(wb, 
                     sheet = sheet_name, 
                     style = openxlsx::createStyle(border = "top", borderStyle = "thin"), 
                     rows = start_row + nrow(param_dat), 
                     cols = 1:7, 
                     gridExpand = TRUE)
  
  return(wb)
}




##################################### EXAMPLE RUN #######################################################

# Load existing workbook (change name here)
wb <- openxlsx::loadWorkbook(glue("H://PACER//PREDiCT//PREDiCT_e//CEA//Model inputs examples//01_public_parameters_updated_NTLB2.xlsx"))

# Run function once for ech tumour (this can be looped over)
workbook <- write_shape_scale(wb = wb, params_results = params_results, tumour = "lung", treatment = "cisplatin")

workbook <- write_shape_scale(wb = wb, params_results = params_results, tumour = "breast", treatment = "levo")

# Sve workbook 
openxlsx::saveWorkbook(workbook, glue("H://PACER//PREDiCT//PREDiCT_e//CEA//Model inputs examples//01_public_parameters_updated_NTLB2.xlsx"), overwrite = TRUE)

#################################### LOOPED RUN ###############################################################
param_list <- tibbe::tibble(
  tumour = c("lung", "breast"),
  treatment = c("cisplatin", "levo"),
  # Ordered list of dataframes of the output of `fit_weibull_by_state` for each tumour type
  params_results = list(params_lung, params_breast))
  
# Load workbook (change name)
wb <- openxlsx::loadWorkbook(glue("H://PACER//PREDiCT//PREDiCT_e//CEA//Model inputs examples//01_public_parameters_updated_NTLB2.xlsx"))
  
# Loop over rows of the param_list
  for (i in seq_len(nrow(param_list))) {
    wb <- write_shape_scale(
      wb = wb,
      params_results = param_list$params_results[[i]],  #
      tumour = param_list$tumour[i],
      treatment = param_list$treatment[i]
    )
  }
  
  # Save workbook once
openxlsx::saveWorkbook(workbook, glue("H://PACER//PREDiCT//PREDiCT_e//CEA//Model inputs examples//01_public_parameters_updated_NTLB2.xlsx"), overwrite = TRUE)