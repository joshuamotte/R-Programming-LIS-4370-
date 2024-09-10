Name <- c("Jeb", "Donald", "Ted", "Marco", "Carly", "Hillary", "Bernie")
ABC <- c(4, 62, 51, 21, 2, 14, 15)
CBS <- c(12, 75, 43, 19, 1, 21, 19)

election_results <- data.frame(
  Name,
  ABC,
  CBS
)

election_results


mean_results <- rowMeans(election_results[ ,2:3])
cbind(election_results, mean_results)