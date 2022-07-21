
# go to this link: https://data.humdata.org/dataset/niger-healthsites
# click on the download tab next to the text Niger-healthsites-geojson
# copy that site link and paste to the object "link" below 

link <- "https://s3.us-east-1.amazonaws.com/hdx-production-filestore/resources/554cc503-8704-4672-b14f-774bc9e3e07d/niger.geojson?AWSAccessKeyId=AKIAXYC32WNARK756OUG&Signature=KrqvUSDcNPrGaQ%2FEGLqqF9Es5%2BU%3D&Expires=1658429879"

btc           <- jsonlite::fromJSON(txt = link)
df_btc        <- btc$features$properties
df_btc_coords <- btc$features$geometry
df_btc        <- dplyr::rename_with(df_btc, .cols = dplyr::everything(), .fn = ~stringr::str_replace_all(string = .x, pattern = "properties$", replacement = ""))
df_btc        <- cbind(df_btc, df_btc_coords)

saveRDS(object = df_btc, file = paste0(fp_data, "/niger_healthsites_multiple_source.rds"))