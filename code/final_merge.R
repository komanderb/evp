#### Final DF //

## even though there might be changes // 
evp_final = bind_rows(batch_4, batch_1, batch_2, batch_3, batch_5)
evp_final = evp_final[-c(126, 127)]
setwd("C:/Users/lenovo/Documents/GitHub/evp/final_data")
write.csv(evp_final, "evp_final.csv", row.names = F)
