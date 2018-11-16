library(tidyverse)
tab <- read_csv2("vsechny_uradovny_export.csv", locale = locale(encoding = "windows-1250")) # načte soubor s kódováním
uh <- tab$UREDNI_HODINY # výjme sloupec jako (textový) vektor pro další zpracování
po <- as.data.frame(str_extract_all(uh, "Pondělí:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                      simplify = T)) # najde regulární výraz (den a otevírací doba a výsledky uloží jako tabulku s několika sloupci)
po <- po %>% 
  unite("po", sep = " ") %>% # sloučí sloupce
  transmute(PONDELI = str_replace_all(po, "Pondělí:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2")) # smaže nadbytečný text (den) a vratí otevírací dobu jako rozsah ("-"), více údajů oddělí mezerou
ut <- as.data.frame(str_extract_all(uh, "Úterý:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                                    simplify = T))
ut <- ut %>% 
  unite("ut", sep = " ") %>% 
  transmute(UTERY = str_replace_all(ut, "Úterý:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2"))
st <- as.data.frame(str_extract_all(uh, "Středa:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                                    simplify = T))
st <- st %>% 
  unite("st", sep = " ") %>% 
  transmute(STREDA = str_replace_all(st, "Středa:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2"))
ct <- as.data.frame(str_extract_all(uh, "Čtvrtek:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                                    simplify = T))
ct <- ct %>% 
  unite("ct", sep = " ") %>% 
  transmute(CTVRTEK = str_replace_all(ct, "Čtvrtek:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2"))
pa <- as.data.frame(str_extract_all(uh, "Pátek:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                                    simplify = T))
pa <- pa %>% 
  unite("pa", sep = " ") %>% 
  transmute(PATEK = str_replace_all(pa, "Pátek:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2"))
dny <- cbind(po, ut, st, ct, pa)
for(nm in names(dny)) {
  dny[[nm]] <- map_chr(dny[[nm]], str_squish) # smaže nadbytečné mezery ve výsledku
  dny[[nm]] <- str_remove(dny[[nm]], "NA") # smaže hodnotu "NA" ve výsledku
}
# lze i dny <- map_df(dny, ~map_chr(.x, str_squish))
tab.opr <- cbind.data.frame(tab, dny) %>% # sloučí pův. a výslednou tabulku
  select(-c("UREDNI_HODINY", "X13")) # smaže nepotřebné sloupce
write.csv2(tab.opr, "vsechny_uradovny_opr2.csv", fileEncoding = "windows-1250", row.names = F, na = "") # uloží do CSV,
  # NA jako prázdné buňky

# DODĚLAT
# kontrola struktury otevíracích hodin (vždy Den: hh:hh ?)

## HOTOVO
# kontrola pořadí otevíracích hodin - ok