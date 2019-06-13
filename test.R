library(hammond)
hdb_login(host = "192.168.0.64", password = "peace123")
ppi = hdb_get(c("Political Democracy Index",
                "Number of visitors",
                "The extent of regional integration",
                "Hostility to foreigners/private property",
                "Gross enrolment ratio, secondary (% of secondary school-age population)",
                "Gender Inequality Index (GII)",
                "Inequality-adjusted life expectancy index",
                "Youth Development Index",
                "C2: Factionalized Elites",
                "C3: Group Grievance",
                "P3: Human Rights",
                "Perceptions of Corruption",
                "FOP Total Score"))
ppi = ppi %>% select(geocode, variablename, year, value)
ppi = hinterpolate(ppi)
ppi$value[is.na(ppi$value)] = ppi$yhat[is.na(ppi$value)]
ppi = ppi %>% select(-yhat)

