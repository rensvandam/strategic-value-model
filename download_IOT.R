library(httr)
library(here)

# Function to download OECD IOT
download_oecd_iot <- function(output_folder = "data/oecd_iot") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    cat("Created directory:", output_folder, "\n")
  }
  
  # OECD ICIO direct download URL
  oecd_url <- "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=84b22f1a-20f5-4e55-9486-b06a24f32c22"
  
  # Output file path
  output_file <- file.path(output_folder, "OECD_ICIO_2021.zip")
  
  cat("Downloading OECD Inter-Country Input-Output tables...\n")
  cat("URL:", oecd_url, "\n")
  cat("Destination:", output_file, "\n\n")
  
  # Download the file
  tryCatch({
    response <- GET(
      url = oecd_url,
      write_disk(output_file, overwrite = TRUE),
      progress()
    )
    
    # Check if download was successful
    if (status_code(response) == 200) {
      cat("\n✓ Download successful!\n")
      cat("File saved to:", output_file, "\n")
      cat("File size:", file.size(output_file) / 1024^2, "MB\n\n")
      
      # Ask if user wants to unzip
      cat("Attempting to unzip files...\n")
      unzip_result <- tryCatch({
        unzip(output_file, exdir = output_folder)
        TRUE
      }, error = function(e) {
        cat("Note: Could not automatically unzip. You may need to unzip manually.\n")
        cat("Error:", e$message, "\n")
        FALSE
      })
      
      if (unzip_result) {
        cat("✓ Files extracted to:", output_folder, "\n")
        
        # List extracted files
        extracted_files <- list.files(output_folder, recursive = FALSE)
        cat("\nExtracted files:\n")
        print(extracted_files)
      }
      
    } else {
      cat("✗ Download failed with status code:", status_code(response), "\n")
      cat("This might be because:\n")
      cat("  1. The URL structure has changed\n")
      cat("  2. The file requires authentication\n")
      cat("  3. There's a temporary server issue\n\n")
      cat("Try visiting the URL directly in your browser:\n")
      cat(oecd_url, "\n")
    }
    
  }, error = function(e) {
    cat("✗ Error during download:\n")
    cat(e$message, "\n\n")
    cat("Alternative: Visit the OECD website directly:\n")
    cat("https://www.oecd.org/sti/ind/inter-country-input-output-tables.htm\n")
  })
  
  return(invisible(output_file))
}

# Example usage:
# Simply run the function with default folder
# download_oecd_iot()

# Or specify a custom folder
# download_oecd_iot(output_folder = "my_custom_folder/oecd_data")

# Run with default settings
cat("========================================\n")
cat("OECD IOT Download Script\n")
cat("========================================\n\n")

download_oecd_iot(output_folder = "./data/IOT/")

cat("\n========================================\n")
cat("Download process complete!\n")
cat("========================================\n")