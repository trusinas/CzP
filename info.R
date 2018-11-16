library(tidyverse)
# path <- "C:\\Users\\TrusinaS\\Downloads\\vsechny_uradovny_export.csv"
# tab <- read_csv2(path, locale = locale(encoding = "windows-1250"))
tab <- read_csv2("vsechny_uradovny_export.csv", locale = locale(encoding = "windows-1250"))
uh <- tab %>% 
  select(UREDNI_HODINY) %>% 
  head()
uh <- uh %>% 
  mutate(PONDELI = str_extract_all(UREDNI_HODINY, "Pondělí:\\s+[0-9]{2}:[0-9]{2}\\s*[0-9]{2}:[0-9]{2}",
                                   simplify = F)) # PONDELI je list!!
uh <- uh %>% 
  mutate(PONDELI = str_replace_all(PONDELI, "Pondělí:\\s+([0-9]{2}:[0-9]{2})\\s*([0-9]{2}:[0-9]{2})", "\\1-\\2"))
uh$PONDELI[1]
eval(parse(text = uh$PONDELI[1])) # odstraní vektor
paste(uh$PONDELI[1], collapse = " ")
stringi::stri_paste(uh$PONDELI[1], collapse='')
write_csv(uh, "C:\\Users\\TrusinaS\\Downloads\\vsechny_uradovny_export_test.csv")

