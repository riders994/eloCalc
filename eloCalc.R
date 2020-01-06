weeklyFrame = read.csv('./weekly_elo.csv', row.names = 1)

print(row.names(weeklyFrame))

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

week10mat = cbind(c(1, 7, 8, 3, 4, 9)
                  , c(7, 2, 3, 5, 6, 4)
                  , c(2, 12, 11, 6, 5, 10)
                  , c(2, 7, 6, 3, 2, 5))

weeklyFrame$week10 <- week10 <- elo_gen(week10mat, weeklyFrame$week9)
write.csv(weeklyFrame, 'weekly_elo.csv')
