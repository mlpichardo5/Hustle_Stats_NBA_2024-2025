hustle <- read.csv("pergamehustle2024-25.csv")

## Per Game Leaders

### Deflections


hustle_by_def <- hustle[order(hustle$Deflections, decreasing = TRUE), ]
par(mar = c(7, 5, 4, 4))
def_dp <- barplot(height = hustle_by_def$Deflections[1:10], 
                  names.arg = hustle_by_def$Player[1:10], cex.names = 0.6, las = 2, 
                  ylab = "Deflections", border = "skyblue", 
                  col = adjustcolor("skyblue", alpha.f = 0.7))
text(def_dp, 0, hustle_by_def$Deflections[1:10], cex = 0.7, pos = 3)

### Loose Balls Recovered


hustle_by_lb <- hustle[order(hustle$Loose.Balls.Recovered, decreasing = TRUE),]
lb_bp <- barplot(height = hustle_by_lb$Loose.Balls.Recovered[1:10], 
                 names.arg = hustle_by_lb$Player[1:10], cex.names = 0.4, las = 2, 
                 ylab = "Loose Balls Recovered", border = "skyblue", 
                 col = adjustcolor("skyblue", alpha.f = 0.7))
text(lb_bp, 0, hustle_by_lb$Loose.Balls.Recovered[1:10], cex = 0.7, pos = 3)


### Screen Assists 


hustle_by_sa <- hustle[order(hustle$Screen.Assists, decreasing = TRUE), ]
par(mar = c(7, 5, 4, 4))
sa_bp <- barplot(height = hustle_by_sa$Screen.Assists[1:10], 
                 names.arg = hustle_by_sa$Player[1:10], cex.names = 0.6, las = 2, 
                 ylab = "Screen Assists", border = "skyblue", 
                 col = adjustcolor("skyblue", alpha.f = 0.7))
text(sa_bp, 0, hustle_by_sa$Screen.Assists[1:10], cex = 0.7, pos = 3)


### Contested Shots


hustle_by_cs <- hustle[order(hustle$Contested.Shots, decreasing = TRUE), ]
par(mar = c(7, 5, 4, 4))
cs_bp <- barplot(height = hustle_by_cs$Contested.Shots[1:10], 
                 names.arg = hustle_by_cs$Player[1:10], cex.names = 0.6, las = 2, 
                 ylab = "Contested Shots", border = "skyblue", 
                 col = adjustcolor("skyblue", alpha.f = 0.7))
text(cs_bp, 0, hustle_by_cs$Contested.Shots[1:10], cex = 0.7, pos = 3)


### Charges Drawn


hustle_by_cd <- hustle[order(hustle$Charges.Drawn, decreasing = TRUE), ]
par(mar = c(7, 5, 4, 4))
cd_bp <- barplot(height = hustle_by_cd$Charges.Drawn[1:10], 
                 names.arg = hustle_by_cd$Player[1:10], cex.names = 0.6, las = 2, 
                 ylab = "Charges Drawn", border = "skyblue", 
                 col = adjustcolor("skyblue", alpha.f = 0.7))
text(cd_bp, 0, hustle_by_cd$Charges.Drawn[1:10], cex = 0.7, pos = 3)


### Box Outs


hustle_by_bo <- hustle[order(hustle$Box.Outs, decreasing = TRUE), ]
par(mar = c(7, 5, 4, 4))
bo_bp <- barplot(height = hustle_by_bo$Box.Outs[1:10], 
                 names.arg = hustle_by_bo$Player[1:10], cex.names = 0.6, las = 2, 
                 ylab = "Box Outs", border = "skyblue", 
                 col = adjustcolor("skyblue", alpha.f = 0.7))
text(cd_bp, 0, hustle_by_bo$Box.Outs[1:10], cex = 0.7, pos = 3)


## Hustle Metric


library(gt)
library(dplyr)

hustle_metric <- function(deflections, loose, screens, contests, charges, boxouts){
  def <- deflections/max(deflections)
  lbr <- loose/max(loose)
  sa <- screens/max(screens)
  cs <- contests/max(contests)
  cd <- charges/max(charges)
  bo <- boxouts/max(boxouts)
  metric <- (lbr*0.3) + (def*0.3) + (cd*0.25) + (bo*0.05) + (cs*0.05) + (sa*0.05)
  metric * 100
}

player_metric <- hustle_metric(hustle$Deflections,
                               hustle$Loose.Balls.Recovered,
                               hustle$Screen.Assists,
                               hustle$Contested.Shots, hustle$Charges.Drawn,
                               hustle$Box.Outs)

hustle$Hustle.Rating <- player_metric

hustle_by_rating <- hustle[order(hustle$Hustle.Rating, decreasing = TRUE), ]
colnames(hustle_by_rating)[6:11] <- c("Screen Assists", "Deflections", "Loose Balls Recovered", "Charges Drawn", "Contested Shots", "Box Outs")
top_hustlers <- data.frame(hustle_by_rating$Player[1:10], round(hustle_by_rating$Hustle.Rating[1:10], digits = 2))
colnames(top_hustlers) <- c("Player", "Hustle Rating")
gt(top_hustlers) %>%
  tab_header(title = md("**Top Hustlers**"), subtitle = md("2024-2025"))


## Hustle Player of the Year? 

### Add Percentiles

hustle$def_percentiles <- ecdf(hustle$Deflections)(hustle$Deflections) * 100
hustle$loose_percentiles <-
  ecdf(hustle$Loose.Balls.Recovered)(hustle$Loose.Balls.Recovered) * 100
hustle$charges_percentiles <- 
  ecdf(hustle$Charges.Drawn)(hustle$Charges.Drawn) * 100
hustle$contests_percentiles <-
  ecdf(hustle$Contested.Shots)(hustle$Contested.Shots) * 100
hustle$screen_percentiles <-
  ecdf(hustle$Screen.Assists)(hustle$Screen.Assists) * 100
hustle$box_percentiles <- ecdf(hustle$Box.Outs)(hustle$Box.Outs) * 100


indices <- c(which(hustle$Player == "Toumani Camara"), 
             which(hustle$Player == "Dyson Daniels"), 
             which(hustle$Player == "Paul George"), 
             which(hustle$Player == "Victor Wembanyama"), 
             which(hustle$Player == "Draymond Green"), 
             which(hustle$Player == "Alperen Sengun"), 
             which(hustle$Player == "Josh Hart"), 
             which(hustle$Player == "Alex Caruso"), 
             which(hustle$Player == "Cason Wallace"), 
             which(hustle$Player == "Jalen Suggs"))

percentile_data <- cbind(hustle$def_percentiles[indices], hustle$loose_percentiles[indices], hustle$screen_percentiles[indices], hustle$charges_percentiles[indices], hustle$contests_percentiles[indices], hustle$box_percentiles[indices])


library(fmsb)

radar_data <- as.data.frame(rbind(rep(100, 6), rep(20, 6), percentile_data))
colnames(radar_data) <- c("Deflections", "Loose Balls", "Screen Assists", "Charges", "Contests", "Box Outs")

radar_data2 <- as.data.frame(rbind(c(max(hustle$Screen.Assists), max(hustle$Deflections), max(hustle$Loose.Balls.Recovered), max(hustle$Charges.Drawn), max(hustle$Contested.Shots), max(hustle$Box.Outs)), c(min(hustle$Screen.Assists), min(hustle$Deflections), min(hustle$Loose.Balls.Recovered), min(hustle$Charges.Drawn), min(hustle$Contested.Shots), min(hustle$Box.Outs)), hustle_by_rating[1:10, 6:11]))


radarchart(radar_data[1:3, ], pcol = "red", pfcol = adjustcolor("red", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Toumani Camara")
radarchart(radar_data[c(1, 2, 4), ], pcol = "darkred", pfcol = adjustcolor("darkred", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Dyson Daniels")
radarchart(radar_data[c(1, 2, 5), ], pcol = "darkblue", pfcol = adjustcolor("darkblue", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Paul George")
radarchart(radar_data[c(1, 2, 6), ], pcol = "black", pfcol = adjustcolor("black", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Victor Wembanyama")
radarchart(radar_data[c(1, 2, 7), ], pcol = "gold", pfcol = adjustcolor("gold", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Draymond Green")
radarchart(radar_data[c(1, 2, 8), ], pcol = "red", pfcol = adjustcolor("red", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Alperen Sengun")
radarchart(radar_data[c(1, 2, 9), ], pcol = "blue", pfcol = adjustcolor("blue", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Josh Hart")
radarchart(radar_data[c(1, 2, 10), ], pcol = "darkorange", pfcol = adjustcolor("darkorange", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Alex Caruso")
radarchart(radar_data[c(1, 2, 11), ], pcol = "darkorange", pfcol = adjustcolor("darkorange", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Cason Wallace")
radarchart(radar_data[c(1, 2, 12), ], pcol = "navy", pfcol = adjustcolor("navy", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Jalen Suggs")

hustle[which(hustle$Player == "Devin Booker"), ]
## Actual stats instead of percentiles

radarchart(radar_data2[1:3, ], pcol = "red", pfcol = adjustcolor("red", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Toumani Camara")
radarchart(radar_data2[c(1, 2, 4), ], pcol = "darkred", pfcol = adjustcolor("darkred", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Dyson Daniels")
radarchart(radar_data2[c(1, 2, 5), ], pcol = "darkblue", pfcol = adjustcolor("darkblue", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Paul George")
radarchart(radar_data2[c(1, 2, 6), ], pcol = "black", pfcol = adjustcolor("black", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Victor Wembanyama")
radarchart(radar_data2[c(1, 2, 7), ], pcol = "gold", pfcol = adjustcolor("gold", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Draymond Green")
radarchart(radar_data2[c(1, 2, 8), ], pcol = "red", pfcol = adjustcolor("red", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Alperen Sengun")
radarchart(radar_data2[c(1, 2, 9), ], pcol = "blue", pfcol = adjustcolor("blue", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Josh Hart")
radarchart(radar_data2[c(1, 2, 10), ], pcol = "darkorange", pfcol = adjustcolor("darkorange", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Alex Caruso")
radarchart(radar_data2[c(1, 2, 11), ], pcol = "darkorange", pfcol = adjustcolor("darkorange", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Cason Wallace")
radarchart(radar_data2[c(1, 2, 12), ], pcol = "navy", pfcol = adjustcolor("navy", alpha.f = 0.5), plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Jalen Suggs")


### Deflections v Loose Balls Recovered


plot(x = c(hustle$Deflections[indices], 3.7), 
     y = c(hustle$Loose.Balls.Recovered[indices], 1.0), pch = 16, 
     col = c("red", "red", rep("black", times = 8), "blue"), 
     ylim = c(0.4, 1.3), xlim = c(1.5, 6.6),
     xlab = "Deflections (per game)", 
     ylab = "Loose Balls Recovered (per game)")
text(x = c(hustle$Deflections[indices], 3.7), 
     y = c(hustle$Loose.Balls.Recovered[indices], 1.0) + 0.03, 
     labels = c("Camara", "Daniels", "George", "Wembanyama", "Green", "Sengun",
                "Hart", "Caruso", "Wallace/Caruso-24", "Suggs", ""), 
     cex = 0.6)

### Charges Drawn v Box Outs


plot(x = c(hustle$Charges.Drawn[indices], 0.17), 
     y = c(hustle$Box.Outs[indices], 0.4), pch = 16, 
     col = c("red", "red", rep("black", times = 8), "blue"), 
     ylim = c(0.1, 2.7),
     xlab = "Charges Drawn (per game)", 
     ylab = "Box Outs (per game)")
text(x = c(hustle$Charges.Drawn[indices] + c(0, 0.022, 0.021, 0.04, 0, 0, 0.014, 0.022, 0.024, 0.018), 0.17 + 0.03), 
     y = c(hustle$Box.Outs[indices] + c(0.12, 0, 0, 0, 0.12, -0.12, 0, 0, 0.01, -0.02), 0.4), 
     labels = c("Camara", "Daniels", "George", "Wembanyama", "Green", "Sengun",
                "Hart", "Caruso", "Wallace", "Suggs", "Caruso-24"), 
     cex = 0.6)


### Contested Shots v Screen Assists


plot(x = c(hustle$Contested.Shots[indices], 5.0), 
     y = c(hustle$Screen.Assists[indices], 0.5), pch = 16, 
     col = c("red", "red", rep("black", times = 8), "blue"), ylim = c(0, 3.7),
     xlab = "Contested Shots (per game)", 
     ylab = "Screen Asists (per game)")
text(x = c(hustle$Contested.Shots[indices], 5.0), 
     y = c(hustle$Screen.Assists[indices], 0.5) + 0.13, 
     labels = c("Camara", "Daniels", "George", "Wembanyama", "Green", "Sengun",
                "Hart", "Caruso", "Wallace", "Suggs", "Caruso-24"), 
     cex = 0.6)


tophustlers_table <- gt(hustle_by_rating[1:10, c(1, 2, 6:11)]) %>%
  tab_header(title = md("**Top Hustlers**"), subtitle = "Per Game Statistics")
tophustlers_table <- tophustlers_table %>%
  tab_style(style = cell_text(size = px(8)), locations = cells_body())
tophustlers_table %>%
  tab_style(style = cell_text(size = px(8)), locations = cells_column_labels())


## Looking at Teams

### Team Hustle Metric


teamhustle <- read.csv("hustletotals2024-25.csv")

team_def <- teamhustle$Deflections
team_lb <- teamhustle$Loose.Balls.Recovered
team_sa <- teamhustle$Screen.Assists
team_cd <- teamhustle$Charges.Drawn
team_cs <- teamhustle$Contested.Shots
team_bo <- teamhustle$Box.Outs

team_metric <- hustle_metric(teamhustle$Deflections, teamhustle$Loose.Balls.Recovered, teamhustle$Screen.Assists, teamhustle$Contested.Shots, teamhustle$Charges.Drawn, teamhustle$Box.Outs)

barplot(team_metric, names.arg = teamhustle$TEAM, cex.names = 0.5, las = 2, 
        ylab = "Team Hustle Ratings", main = "Hustle Ratings (2024-2025)",
        col = adjustcolor("skyblue", alpha.f = 0.5), border = "skyblue")

### Wins v Hustle Rating


team_wins <- c("ATL" = 26, "BKN" = 19, "BOS" = 38, "CHA" = 13, "CHI" = 22, 
               "CLE" = 44, "DAL" = 28, "DEN" = 35, "DET" = 28, "GSW" = 27, 
               "HOU" = 33, "IND" = 29, "LAC" = 29, "LAL" = 32, "MEM" = 36, 
               "MIA" = 25, "MIL" = 28, "MIN" = 30, "NOP" = 12, "NYK" = 35,
               "OKC" = 43, "ORL" = 26, "PHI" = 20, "PHX" = 26, "POR" = 23,
               "SAC" = 27, "SAS" = 23, "TOR" = 17, "UTA" = 12, "WAS" = 9)

wins_v_hustle <- lm(team_wins ~ team_metric)
summary(wins_v_hustle)

plot(x = team_metric, y = team_wins, main = "Does Hustle Impact Wins? (2024-2025)",
     ylab = "Wins", xlab = "Team Hustle Rating", pch = 16)
abline(wins_v_hustle, col = "red")
legend(x = 82, y = 17, legend = "R-Squared = 0.1413   ", cex = 0.8)

Based on the linear model, there seems to be little to no correlation between a team's hustle rating and the amount of games they've won.

### Wins v Deflections


wins_v_deflections <- lm(team_wins ~ team_def)
summary(wins_v_deflections)

plot(x = team_def, y = team_wins, main = "Do Deflections Impact Wins?",
     ylab = "Wins", xlab = "Deflections", pch = 16)
abline(wins_v_deflections, col = "red")

### Wins v Loose Ball Recoveries


wins_v_loose <- lm(team_wins ~ team_lb)
summary(wins_v_loose)

plot(x = team_lb, y = team_wins, 
     main = "Do Loose Ball Recoveries Impact Wins?", ylab = "Wins", 
     xlab = "Loose Balls Recovered", pch = 16)
abline(wins_v_loose, col = "red")

### Wins v Charges Drawn

wins_v_charges <- lm(team_wins ~ team_cd)
summary(wins_v_charges)

plot(x = team_cd, y = team_wins, main = "Do Charges Drawn Impact Wins?", 
     ylab = "Wins", xlab = "Charges Drawn", pch = 16)
abline(wins_v_charges, col = "red")

### Wins v Contested Shots

wins_v_contests <- lm(team_wins ~ team_cs)
summary(wins_v_contests)

plot(x = team_cs, y = team_wins, main = "Do Contested Shots Impact Wins?", 
     ylab = "Wins", xlab = "Contested Shots", pch = 16)
abline(wins_v_contests, col = "red")

### Wins v Screen Assits

wins_v_screens <- lm(team_wins ~ team_sa)
summary(wins_v_screens)

plot(x = team_sa, y = team_wins, main = "Do Screen Assists Impact Wins?", 
     ylab = "Wins", xlab = "Screen Assists", pch = 16)
abline(wins_v_screens, col = "red")

### Wins v Box Outs

wins_v_boxouts <- lm(team_wins ~ team_bo)
summary(wins_v_boxouts)

plot(x = team_bo, y = team_wins, main = "Do Box Outs Impact Wins?", 
     ylab = "Wins", xlab = "Box Outs", pch = 16)
abline(wins_v_boxouts, col = "red")

## Do the Top Hustlers Impact Wins?

top2 <- function(x){
  x <- sort(x, decreasing = TRUE)
  sum(x[1:2])
}
team_top3 <- tapply(X = hustle$Hustle.Rating, INDEX = hustle$Team, top3)
team_top3

### Wins v Top Hustlers

wins_v_top <- lm(team_wins ~ team_top3)
summary(wins_v_top)

plot(x = team_top3, y = team_wins, main = "Do Top Hustlers Impact Wins?", 
     ylab = "Wins", 
     xlab = "Sum of Hustle Rating of top 3 hustlers per team", 
     pch = 16)
abline(wins_v_top, col = "red")
legend(x = 82, y = 43, legend = "R-Squared = 0.1764    ", cex = 0.8)


playoffs_hustle <- read.csv("teamhustleplayoffs.csv")

playoffs_hustle$Hustle.Rating <- hustle_metric(playoffs_hustle$Deflections, playoffs_hustle$Loose.Balls.Recovered, playoffs_hustle$Screen.Assists, playoffs_hustle$Contested.Shots, playoffs_hustle$Charges.Drawn, playoffs_hustle$Box.Outs)

barplot(playoffs_hustle$Hustle.Rating, names.arg = playoffs_hustle$TEAM, las = 2, 
        col = c("darkgreen", "skyblue", "lightgreen", "skyblue", "blue", "gray", "gray", 
                "gray", "gray", "blue", "gray", "skyblue", "skyblue", "gray", "gray",
                "gray"),
        ylab = "Playoff Hustle Rating")
legend(x = 3, y = 92, legend = c("Champion", "Runner-Up", "Conference Runner-Up", "Second Round Exit", "First Round Exit"), col = c("darkgreen", "lightgreen", "blue", "skyblue", "gray"), pch = 18, cex = 0.5, pt.cex = 0.8)

  ## Comparisons Over the Years
  
hustle24 <- read.csv("pergamehustle2023-24.csv")

hustle24$Hustle.Rating <- hustle_metric(hustle24$Deflections, hustle24$Loose.Balls.Recovered, hustle24$Screen.Assists, hustle24$Contested.Shots, hustle24$Charges.Drawn, hustle24$Box.Outs)

hustle24_by_rating <- hustle24[order(hustle24$Hustle.Rating, decreasing = TRUE), ]
top_hustlers24 <- data.frame(hustle24_by_rating$Player[1:10], round(hustle24_by_rating$Hustle.Rating[1:10], digits = 2))
colnames(top_hustlers24) <- c("Player", "Hustle Rating")
gt(top_hustlers24) %>%
  tab_header(title = md("**Top Hustlers**"), subtitle = md("2023-2024"))

## Add Percentiles

hustle24$def_percentiles <- 
  ecdf(hustle24$Deflections)(hustle24$Deflections) * 100
hustle24$loose_percentiles <-
  ecdf(hustle24$Loose.Balls.Recovered)(hustle24$Loose.Balls.Recovered) * 100
hustle24$charges_percentiles <- 
  ecdf(hustle24$Charges.Drawn)(hustle24$Charges.Drawn) * 100
hustle24$contests_percentiles <-
  ecdf(hustle24$Contested.Shots)(hustle24$Contested.Shots) * 100
hustle24$screen_percentiles <-
  ecdf(hustle24$Screen.Assists)(hustle24$Screen.Assists) * 100
hustle24$box_percentiles <- ecdf(hustle24$Box.Outs)(hustle24$Box.Outs) * 100

## Compare Daniels and Camara to 2023-2024 Caruso

caruso_data <- cbind(hustle24$def_percentiles[c(7, 96)], hustle24$loose_percentiles[c(7, 96)], hustle24$screen_percentiles[c(7, 96)], hustle24$charges_percentiles[c(7, 96)], hustle24$contests_percentiles[c(7, 96)], hustle24$box_percentiles[c(7, 96)])

library(fmsb)

radar_data_24 <- as.data.frame(rbind(rep(100, 6), rep(0, 6), caruso_data, percentile_data))
colnames(radar_data_24) <- c("Deflections", "Loose Balls", "Screen Assists", "Charges", "Contests", "Box Outs")
rownames(radar_data_24) <- c(1, 2, "Caruso (23-24)", "Green (23-24)", "Camara", "Daniels", "George", "Wembanyama", "Green", "Sengun", "Hart", "Caruso", "Wallace", "Suggs")

radarchart(radar_data_24[c(1, 2, 3, 5), ], pcol = c("darkorange", "black"), pfcol = c(adjustcolor("darkorange", alpha.f = 0.25), adjustcolor("black", alpha.f = 0.25)), plty = 1, plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Camara vs Caruso (24)")
radarchart(radar_data_24[c(1, 2, 3, 6), ], pcol = c("darkorange", "darkred"), pfcol = c(adjustcolor("darkorange", alpha.f = 0.25), adjustcolor("darkred", alpha.f = 0.25)), plty = 1, plwd = 2, cglcol = "gray", cglty = 1, vlcex = 0.5, title = "Daniels vs Caruso (24)")
gt(round(radar_data_24[3:12, ]), rownames_to_stub = TRUE) %>%
  tab_header(title = md("**Percentile Data**"),
             subtitle = md("Represents how each player ranks compared to the league in their respective year"))

## Does Hustling Help Improve Minutes?

hustle3624 <- read.csv("per36huslte2023-2024.csv")
hustle3624$Min.G <- round(hustle3624$Min.G, digits = 1)
head(hustle3624)
hustle3624$Hustle.Rating <- hustle_metric(hustle3624$Deflections, hustle3624$Loose.Balls.Recovered, hustle3624$Screen.Assists, hustle3624$Contested.Shots, hustle3624$Charges.Drawn, hustle3624$Box.Outs)
median(hustle3624$Min.G)
role_players24 <- hustle3624[hustle3624$Min.G < 26, ]
role_players24 <- role_players24[role_players24$Min.G > 15, ]

role_by_rating24 <- role_players24[order(role_players24$Hustle.Rating, decreasing = TRUE), ]
top_role_hustlers24 <- data.frame(role_by_rating24$Player[1:75], role_by_rating24$Min.G[1:75], role_by_rating24$Hustle.Rating[1:75])
colnames(top_role_hustlers24) <- c("Player", "Minutes", "Hustle Rating")
top_role_hustlers24 <- top_role_hustlers24[order(top_role_hustlers24$Player), ]
top_role_hustlers24


indices2 <- which(hustle$Player %in% top_role_hustlers24$Player)
hustlers24_minutes25 <- hustle[indices2, c(1, 5, 12)]
hustlers24_minutes25

indices3 <- which(top_role_hustlers24$Player %in% hustlers24_minutes25$Player)
minutes_change <- hustlers24_minutes25$Min - top_role_hustlers24$Minutes[indices3]
minutes_change

hustle_v_min <- lm(minutes_change ~ top_role_hustlers24$`Hustle Rating`[indices3])
summary(hustle_v_min)

plot(x = top_role_hustlers24$`Hustle Rating`[indices3], y = minutes_change, 
     main = "Does Hustling Improve Minutes?", 
     xlab = "Per 36 Minutes Hustle Rating 2023-2024", ylab = "Minutes Gained in 2024-2025", 
     pch = 16, cex = 0.7)
abline(hustle_v_min, col = "red")
legend(x = 50, y = 10, legend = "R-Squared = 0.02737    ", cex = 0.6)

prole_players24 <- hustle24[hustle24$Min < 26, ]
prole_by_rating24 <- prole_players24[order(prole_players24$Hustle.Rating, decreasing = TRUE), ]
ptop_role_hustlers24 <- data.frame(prole_by_rating24$Player[1:75], prole_by_rating24$Min[1:75], prole_by_rating24$Hustle.Rating[1:75])
colnames(ptop_role_hustlers24) <- c("Player", "Minutes", "Hustle Rating")
ptop_role_hustlers24 <- ptop_role_hustlers24[order(ptop_role_hustlers24$Player), ]
ptop_role_hustlers24
indices4 <- which(hustle$Player %in% ptop_role_hustlers24$Player)
phustlers24_minutes25 <- hustle[indices4, c(1, 5, 12)]
phustlers24_minutes25

indices5 <- which(ptop_role_hustlers24$Player %in% phustlers24_minutes25$Player)
pminutes_change <- phustlers24_minutes25$Min - ptop_role_hustlers24$Minutes[indices5]
pminutes_change

phustle_v_min <- lm(pminutes_change ~ ptop_role_hustlers24$`Hustle Rating`[indices5])
summary(phustle_v_min)

plot(x = ptop_role_hustlers24$`Hustle Rating`[indices5], y = pminutes_change, 
     main = "Does Hustling Improve Minutes?", 
     xlab = "Hustle Rating 2023-2024", ylab = "Minutes Gained in 2024-2025", 
     pch = 16, cex = 0.7)
abline(phustle_v_min, col = "red")


## Hustle Rating this year vs Minutes Change


hustle25_v_min <- lm(minutes_change ~ hustlers24_minutes25$Hustle.Rating)
summary(hustle25_v_min)

plot(x = hustlers24_minutes25$Hustle.Rating, y = minutes_change, 
     main = "Does Hustling Improve Minutes?", 
     xlab = "Hustle Rating 2024-2025", ylab = "Minutes Gained in 2024-2025", 
     pch = 16, cex = 0.7)
abline(hustle25_v_min, col = "red")


## Combining Hustle Rating vs Minutes Change


summed_ratings_role <- hustlers24_minutes25$Hustle.Rating + top_role_hustlers24$`Hustle Rating`[indices3]
sum_hustle_v_min <- lm(minutes_change ~ summed_ratings_role)
summary(sum_hustle_v_min)

plot(x = summed_ratings_role, y = minutes_change, 
     main = "Does Hustling Improve Minutes?", 
     xlab = "Combined Hustle Rating 2023-2025", 
     ylab = "Minutes Gained in 2024-2025", 
     pch = 16, cex = 0.7)
abline(sum_hustle_v_min, col = "red")


## 2023-2024


teamhustle24 <- read.csv("hustletotals2023-2024.csv")
teamhustle24$Hustle.Rating <- hustle_metric(teamhustle24$Deflections, teamhustle24$Loose.Balls.Recovered, teamhustle24$Screen.Assists, teamhustle24$Contested.Shots, teamhustle24$Charges.Drawn, teamhustle24$Box.Outs)

hustle24_v_wins <- lm(teamhustle24$Wins ~ teamhustle24$Hustle.Rating)
summary(hustle24_v_wins)

plot(x = teamhustle24$Hustle.Rating, y = teamhustle24$Wins, 
     main = "Does Hustle Impact Wins? (2023-2024)",
     ylab = "Wins", xlab = "Team Hustle Rating", pch = 16)
abline(hustle24_v_wins, col = "red")
legend(x = 85, y = 25, legend = "R-Squared = 0.03955    ", cex = 0.6)



barplot(teamhustle24$Hustle.Rating, names.arg = teamhustle24$TEAM, cex.names = 0.5, 
        las = 2, ylab = "Team Hustle Ratings", main = "Hustle Ratings (2023-2024)",
        col = adjustcolor("skyblue", alpha.f = 0.5), border = "skyblue")


## Team Hustle vs. Team Defensive Rating


advanced <- read.csv("advanced2024-205.csv")

hustle_vs_drating <- lm(advanced$DefRtg ~ team_metric)
summary(hustle_vs_drating)

plot(x = team_metric, y = advanced$DefRtg, main = "Does Hustle Impact Defensive Rating?",
     ylab = "Defensive Rating", xlab = "Team Hustle Rating", pch = 16)
abline(hustle_vs_drating, col = "red")
legend(x = 82, y = 117, legend = "R-Squared = 0.2776    ", cex = 0.8)


## More Comparisons


player_advanced <- read.csv("player_advanced24.csv")

player_advanced <- player_advanced[which(player_advanced$PLAYER %in% hustle$Player), ]
hustle_new <- hustle[which(hustle$Player %in% player_advanced$PLAYER), ]

player_advanced <- player_advanced[order(player_advanced$PLAYER), ]
hustle_new <- hustle_new[order(hustle_new$Player), ]
all.equal(player_advanced$PLAYER, hustle_new$Player)

hustle_v_PIE <- lm(player_advanced$PIE ~ hustle_new$Hustle.Rating)
summary(hustle_v_PIE)

plot(x = hustle_new$Hustle.Rating, y = player_advanced$PIE, 
     main = "Does Hustle Affect Player Impact Estimate (PIE)?", 
     ylab = "PIE", xlab = "Hustle Rating", pch = 16, cex = 0.8)
abline(hustle_v_PIE, col = "red")
legend(x = 45, y = 5, legend = "R-squared = 0.1688   ", cex = 0.6)


hustle_v_drtg <- lm(player_advanced$DEFRTG ~ hustle_new$Hustle.Rating)
summary(hustle_v_drtg)

plot(x = hustle_new$Hustle.Rating, y = player_advanced$DEFRTG, 
     main = "Is there a correlation between hustle and defense?", 
     ylab = "Defensive Rating", xlab = "Hustle Rating", pch = 16, cex = 0.8)
abline(hustle_v_drtg, col = "red")


hustle_v_nrtg <- lm(player_advanced$NETRTG ~ hustle_new$Hustle.Rating)
summary(hustle_v_nrtg)

plot(x = hustle_new$Hustle.Rating, y = player_advanced$NETRTG, 
     main = "Is there a correlation between hustle and net rating?", 
     ylab = "Net Rating", xlab = "Hustle Rating", pch = 16, cex = 0.8)
abline(hustle_v_nrtg, col = "red")


hustle_new$DeflectionsPerMin <- hustle_new$Deflections/hustle_new$Min
hustle_new$LoosePerMin <- hustle_new$Loose.Balls.Recovered/hustle_new$Min
hustle_new$ScreensPerMin <- hustle_new$Screen.Assists/hustle_new$Min
hustle_new$ChargesPerMin <- hustle_new$Charges.Drawn/hustle_new$Min
hustle_new$ContestsPerMin <- hustle_new$Contested.Shots/hustle_new$Min
hustle_new$BoxOutsPerMin <- hustle_new$Box.Outs/hustle_new$Min

hustle_new$HustleRatingPerMin <- hustle_metric(hustle_new$DeflectionsPerMin, hustle_new$LoosePerMin, hustle_new$ScreensPerMin, hustle_new$ContestsPerMin, hustle_new$ChargesPerMin, hustle_new$BoxOutsPerMin)


hustle_by_perminrating <- hustle_new[order(hustle_new$HustleRatingPerMin, decreasing = TRUE), ]
colnames(hustle_by_perminrating)[6:11] <- c("Screen Assists", "Deflections", "Loose Balls Recovered", "Charges Drawn", "Contested Shots", "Box Outs")
top_permin_hustlers <- data.frame(hustle_by_perminrating$Player[1:10], round(hustle_by_perminrating$HustleRatingPerMin[1:10], digits = 2))
colnames(top_permin_hustlers) <- c("Player", "Hustle Rating")
gt(top_permin_hustlers) %>%
  tab_header(title = md("**Top Hustlers**"), subtitle = md("Using Per Minute Stats (2024-2025)"))



perminhustle_v_PIE <- lm(player_advanced$PIE ~ hustle_new$HustleRatingPerMin)
summary(perminhustle_v_PIE)

plot(x = hustle_new$HustleRatingPerMin, y = player_advanced$PIE, 
     main = "Does Hustle Impact PIE?",
     ylab = "PIE", xlab = "Hustle Rating (Per Min)", pch = 16)
abline(perminhustle_v_PIE, col = "red")
legend(x = 55, y = 5, legend = "R-Squared = 0.01517   ", cex = 0.6)



phustle_v_drtg <- lm(player_advanced$DEFRTG ~ hustle_new$Hustle.Rating)
summary(phustle_v_drtg)

plot(x = hustle_new$Hustle.Rating, y = player_advanced$DEFRTG, 
     main = "Does Hustle Impact Player Defensive Rating?",
     ylab = "Defensive Rating", xlab = "Hustle Rating", pch = 16)
abline(phustle_v_drtg, col = "red")
legend(x = , y = , legend = "R-Squared =    ", cex = 0.6)


perminhustle_v_drtg <- lm(player_advanced$DEFRTG ~ hustle_new$HustleRatingPerMin)
summary(perminhustle_v_drtg)

plot(x = hustle_new$HustleRatingPerMin, y = player_advanced$DEFRTG, 
     main = "Does Hustle Impact Player Defensive Rating?",
     ylab = "Defensive Rating", xlab = "Hustle Rating (Per Min)", pch = 16)
abline(perminhustle_v_drtg, col = "red")
legend(x = 55, y = 120, legend = "R-Squared = 0.1264    ", cex = 0.7)



equal_hustle_metric <- function(deflections, loose, screens, contests, charges, boxouts){
  def <- deflections/max(deflections)
  lbr <- loose/max(loose)
  sa <- screens/max(screens)
  cs <- contests/max(contests)
  cd <- charges/max(charges)
  bo <- boxouts/max(boxouts)
  metric <- (lbr*(1/6)) + (def*(1/6)) + (cd*(1/6)) + (bo*(1/6)) + (cs*(1/6)) + (sa*(1/6))
  metric * 100
}

hustle$EqualHustleRating <- equal_hustle_metric(hustle$Deflections, hustle$Loose.Balls.Recovered, hustle$Screen.Assists, hustle$Contested.Shots, hustle$Charges.Drawn, hustle$Box.Outs)

hustle_by_equal <- hustle[order(hustle$EqualHustleRating, decreasing = TRUE), ]
top_equal <- data.frame(hustle_by_equal$Player[1:10], round(hustle_by_equal$EqualHustleRating[1:10], digits = 2))
colnames(top_equal) <- c("Player", "Hustle Rating")
gt(top_equal) %>%
  tab_header(title = md("**Top Hustlers**"), subtitle = md("Each Stat Valued Equally (2024-2025)"))
