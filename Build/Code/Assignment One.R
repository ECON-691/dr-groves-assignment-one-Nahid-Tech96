#Assignment One
rm(list=ls())

library()
library()
library(ipumsr)
library(stargazer)

#Getting data from IPUMS API
  #set_ipums_api_key("59cba10d8a5da536fc06b59d70620914b2404ea0b2f16f79c0c60484", save = TRUE) #Sets your personal key and saves it

  tst <- get_metadata_nhgis("time_series_tables") #This gives you a list of the time series tables that you can get

#Insert Answer to Question 2 Here####
tst$time_series[[1]]$geo_levels
#data_names <- c("A00","A57","B57","B18","CL6","B69") #These are the tables we are using
  data_ext<- define_extract_nhgis(
    description = "ECON 691",
    time_series_tables =  list(
      tst_spec("A00", "state"),
      tst_spec("A57", "state"),
      tst_spec("B57", "state"),
      tst_spec("B18", "state"),
      tst_spec("CL6", "state"),
      tst_spec("BX3", "state")
    )
  )
  
  ts<-submit_extract(data_ext)
  wait_for_extract(ts)
  filepath <- download_extract(ts)
  
  dat <- read_nhgis(filepath)

#Obtain Census Map Data

  state <- [Insert Code Here]
  
  cen.stat <- get_acs(geography = "state", 
                      survey = "acs5",
                      variables = "B01003_001E", 
                      year = 2020,
                      geometry = TRUE)


  cen.map <- cen.stat %>%
    select(GEOID, NAME, geometry)  %>%
    mutate(STATEFP = GEOID) 

 #Basic Clan of data####

       dat2 <- dat %>%
    select(STATEFP, ends_with(c("1970", "1980", "1990", "2000", "2010", "105", "2020", "205"))) %>%
      

      filter(!is.na(STATEFP)) %>%
      pivot_longer(!STATEFP, names_to = "series", values_to = "estimate") %>%
      mutate(series = str_replace(series, "105", "2010"),
             series = str_replace(series, "205", "2020"),
             year = substr(series, 6, nchar(series)),
             series = substr(series, 1, 5)) %>%
      distinct(STATEFP, series, year, .keep_all = TRUE) %>%
      filter(!is.na(estimate)) %>%
      pivot_wider(id_cols = c(STATEFP, year), names_from = series, values_from = estimate) %>%
      select(-B18AE)  %>%
      mutate(und18 = rowSums(across(B57AA:B57AD)) / A00AA,
             over65 = rowSums(across(B57AP:B57AR)) / A00AA,
             white = [Insert Code Here], #White Population
             black = [Insert Code Here], #Black Population
             asian = [Insert Code Here], #Asian Population
             other = [Insert Code Here], #Something other than the above including multi-race
             lessHS = (BX3AA + BX3AB + BX3AC + BX3AG + BX3AH + BX3AI) / A00AA,
             hscol =  [Insert Code Here], #12th Grade and some college
             ungrd =  [Insert Code Here], #4 years of college or Bach Degree
             advdg =  [Insert Code Here], #More than 4 years or advanced degree
             pov =  [Insert Code Here], #Share of population under Poverty Line
             ln_pop = [Insert Code Here]) %>%  #Natural Log of Population
      select(STATEFP, year, und18:ln_pop) 

    ufo.us <- ufo %>%
      filter(country == "us") %>%
      select(-comments) %>%
      mutate(date = as.Date(str_split_i(datetime," ", 1), "%m/%d/%Y"),
             year = year(date),
             decade = year - year %% 10) %>%
      filter(decade > 1959) %>%
      count(state, decade) %>%
      mutate(Abbr = toupper(state),
             year = as.numeric(decade)) %>%
      full_join(., st_abb, by = "Abbr") %>%
      filter(!is.na(n)) %>%
      rename("GEOID" = "Code") %>%
      mutate(GEOID = str_pad(as.character(GEOID), width = 2, side = "left", pad="0"),
             ln_n = log(n))

#Join the data and Map it

  core <- cen.map %>%
    [Insert Code Here]  %>%
    mutate(decade = as.numeric(year)) %>%  
    [Insert Code Here] 
    [Insert Code Here]

#Non-Race Variable Graphic Visualization#####

    ggplot(core) +
      geom_sf(aes(fill = [Insert Code Here])) +
      scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                           name = "[Insert Code Here]",
                           limits = c(0, .5)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(title = "FIgure One: Percentage of Population [Insert Code Here] Across the Decades") +
      facet_wrap(~ decade) 
    
    ggsave("./Analysis/Output/Figure1.png", dpi = 600)

#Race Variable Graphic Visualization

    ggplot(core) +
      geom_sf(aes(fill = [Insert Code Here])) +
      scale_fill_gradient2(low = "white", high = "blue", na.value = NA, 
                           name = "[Insert Code Here]",
                           limits = c(0, 1)) +
      theme_bw()+
      theme(axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.position = "bottom") +
      labs(title = "Figure Two: Percentage of Population [Insert Code Here] Across the Decades") +
      facet_wrap(~ decade)
    
    ggsave("./Analysis/Output/Figure2.png", dpi = 600)

#Summary Statistics

var1 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")

stargazer(as.data.frame(core), type = "html", out = "./Analysis/Output/Table1.html",
          title = "Table One - Summary Statistics",
          covariate.labels [Insert Code Here]

#Regression Analysis

mod1 <- lm([Insert Code Here]
           data=core)

mod2 <- lm([Insert Code Here]
           data=core)

mod3 <- lm([Insert Code Here]
           data=core)

var2 <- c("Percent Under 18", "Percent Over 65", "Percent White", "Percent Black",
          "Percent Asian", "Percent Other Race", "Percent with Less than HS", "Highschool Only",
          "Undergraduate Degree", "Advanced Degree", "Percent below 2X Poverty Line", 
          "LN of Population","Decade","Number of Signthings", "LN of Sightings")


stargazer(mod1, mod2, mod3,
          omit = ".State.",
          type = "html",
          title = "Table Two - Regression Results",
          out = "./Analysis/Output/Table2.html",
          add.lines=list(c("State F.E." [Insert Code Here] )),
          dep.var.labels = "LN(Sightings)",
          covariate.labels=var2)

