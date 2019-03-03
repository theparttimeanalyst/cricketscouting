library(tidyverse)
set.seed(123)

iplballs <- read_csv("ipldat2.csv")

iplballs <- read_csv("ipldat2.csv")
iplmatches <- read_csv("iplmatches.csv")



iplmat <- select(iplmatches, id, season)

colnames(iplmat)[1] <- "match_id"

str(ipldat)



alldat1 <- left_join(iplballs, iplmat, by = "match_id")

str(alldat1)


creatdatayears <- function(x, y){
  
  dismiss <- alldat1 %>% filter(season > x) %>%
    filter(season < y) %>%
    select(player_dismissed) %>%
    group_by(player_dismissed) %>%
    summarise(totw = n())
  
  colnames(dismiss)[1] <- "batsman"
  
  totrun <- alldat1 %>% filter(season > x) %>%
    filter(season < y) %>%
    group_by(batsman) %>%
    summarise(tot = sum(batsman_runs))
  
  ave_kpi <- totrun %>% left_join(dismiss, by = "batsman") %>%
    mutate(average = tot / totw)
  
  
  totstrik <- alldat1 %>% filter(season > x) %>%
    filter(season < y) %>%
    group_by(batsman) %>%
    summarise(tot = sum(batsman_runs), n = n())
  
  
  strik_kpi <- totstrik %>% left_join(dismiss, by = "batsman" ) %>%
    mutate(strikerate= (tot / n)* 100 , ballpinn = n / totw)  
  
  
  boun <- alldat1 %>% mutate(four = if_else(batsman_runs == 4, 1, 0), six = if_else(batsman_runs == 6, 1, 0)) %>%
    filter(season > x) %>%
    filter(season < y) %>%
    group_by(batsman) %>%
    summarise(n = n(), tot4 = sum(four), tot6 = sum(six)) %>%
    mutate(frate = (tot4/n)*100, sxrate = (tot6/n)*100)
  
  
  boun2 <- alldat1 %>% mutate(boundrun = if_else(batsman_runs == 4, 4, if_else(batsman_runs == 6, 6, 0)),
                              bounno = if_else(batsman_runs == 4, 1, if_else(batsman_runs == 6, 1, 0))) %>%
    filter(season > x) %>%
    filter(season < y) %>%
    group_by(batsman) %>%
    summarise(totrun = sum(batsman_runs), bounrun = sum(boundrun), n = n(), bounno2 = sum(bounno)) %>%
    mutate(bounper = (bounrun /totrun)*100, nonbounsr = ((totrun - bounrun) / (n - bounno2))*100 )
  
  
  alldat2 <- alldat1 %>% filter(season > x) %>%
    filter(season < y) %>%
    group_by(batsman, match_id) %>%
    mutate(ball2 = 1:n()) %>%
    filter(batsman_runs %in% c(4,6)) %>%
    mutate(rank = rank(ball2)) %>%
    ungroup() %>%
    group_by(batsman) %>%
    filter(rank == 1) %>%
    summarise(avefb = mean(ball2, na.rm =T))
  
  dismis2 <- alldat1 %>% filter(season > x) %>%
    filter(season < y) %>%
    select(player_dismissed, dismissal_kind) %>%
    mutate(dismissal = if_else(dismissal_kind == "caught", "caught", if_else(dismissal_kind == "bowled", "bowled", 
                                                                             if_else(dismissal_kind == "caught and bowled", "caught", "other")))) %>%
    
    group_by(player_dismissed, dismissal) %>%
    summarise(tot = n()) %>%
    spread(dismissal, tot) %>%
    mutate_all(funs(replace(., is.na(.), 0))) %>%
    mutate(bowlper = bowled / (bowled + caught + other) * 100, caughtper = caught/ (bowled + caught + other) * 100, 
           otherper = other/(bowled + caught + other) * 100) %>%
    select(player_dismissed, bowlper, caughtper, otherper)
  
  
  colnames(dismis2)[1] <- "batsman"
  
  
  fulldat <- ave_kpi %>% left_join(strik_kpi, by = "batsman" ) %>%
    left_join(boun, by = "batsman") %>%
    left_join(boun2, by = "batsman") %>%
    left_join(alldat2, by = "batsman") %>%
    left_join(dismis2, by = "batsman") %>%
    select(batsman, average, strikerate, ballpinn, frate, sxrate, bounper, nonbounsr, avefb,  bowlper, caughtper, otherper) %>%
    gather("kpi", "value", -batsman)
  
  return(fulldat)
  
}

allyears <- creatdatayears(2007, 2019)

balls <- alldat1 %>% group_by(batsman) %>%
  summarise(balls = n())

allyears3 <- allyears %>% spread(kpi, -batsman) %>%
                                    left_join(balls, by = "batsman") %>% 
                                       filter(balls > 40) %>%
                                        filter(batsman != "PP Ojha") 
                                          
  
  
allyears2 <- allyears %>% group_by(kpi) %>%
                            mutate(scaled = scale(value)) %>%
                              ungroup() %>%
                                select(batsman, kpi, scaled) %>%
                                  spread(kpi, -batsman)


allyears2




balls <- alldat1 %>% group_by(batsman) %>%
                      summarise(balls = n())

fulldat <- allyears2 %>% left_join(balls, by = "batsman") %>%
                            filter(balls > 40) %>%
                              select(-balls) %>% 
                                filter(batsman != "PP Ojha") 


fulldat1 <- fulldat %>% select(-batsman)


tot_withinss <- map_dbl(1:30,  function(k){
  model <- kmeans(x = fulldat1, centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(k = 1:30, tot = tot_withinss)


print(elbow_df)

ggplot(elbow_df, aes(x = k, y = tot)) + geom_line(col = "steelblue", size = 2) +
                                  geom_curve(x=18, xend=23, y=900, yend= 405, arrow = arrow(length = unit(0.2,"cm")), size =1, colour = "green") +
                                  geom_text(label = "k = 23\nselected level", x=18, y= 930, colour = "green") +
                                      labs(title = "Elbow Plot", y = "") +
                                      theme(panel.background = element_blank())

model <- kmeans(fulldat1, centers= 23)

clust20 <- model$cluster

fulldat3 <- mutate(allyears3, cluster = clust20)

fulldat4 <- fulldat1 %>% mutate(cluster = clust20) %>%
                            filter(cluster == 23)

ggplot(fulldat4, aes(x = average, y = nonbounsr, col = as.factor(cluster))) + geom_point()


filter(fulldat3, batsman == "CH Gayle")

filter(fulldat3, cluster == 23)