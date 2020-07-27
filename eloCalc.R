weeklyFrame = read.csv('./weekly_elo.csv', row.names = 1)



elo_calc <- function(helo, hpts, aelo, apts, k = 60) {
  share_h = 10^(helo/400)
  share_a = 10^(aelo/400)
  total = share_h + share_a
  expected_h = share_h/total
  expected_a = share_a/total
  return(cbind(helo + k * (hpts/9 - expected_h), aelo + k * (apts/9 - expected_a)))
}

elo_gen <- function(weekFrame, prevWeek) {
  playNum = length(prevWeek)
  blanks = rep(0, playNum)
  home_elos = prevWeek[weekFrame[, 1]]
  away_elos = prevWeek[weekFrame[, 3]]
  fudge = 9 - (weekFrame[, 2] + weekFrame[, 4])
  home_scores = weekFrame[, 2] + fudge
  away_scores = weekFrame[, 4] + fudge
  new_elos = elo_calc(home_elos, home_scores, away_elos, away_scores)
  blanks[weekFrame[, 1]] = new_elos[, 1]
  blanks[weekFrame[, 3]] = new_elos[, 2]
  return(blanks)
}

print(row.names(weeklyFrame))

week18mat = cbind(c(1, 2, 7, 8, 3, 5)
                  , c(7, 4, 3, 7, 2, 7)
                  , c(11, 4, 10, 9, 12, 6)
                  , c(2, 5, 6, 2, 7, 2))

weeklyFrame$week18 <- week18 <- elo_gen(week18mat, weeklyFrame$week17)
write.csv(weeklyFrame, 'weekly_elo.csv')
