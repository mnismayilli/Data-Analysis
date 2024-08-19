beads <- rep(c("red", "blue"), 
              times=c(2,3))

beads

#Choose 1 bead from the beads
sample(beads, 1)

#Simple monte carlo simulation

B <- 10000

events <- replicate(B, sample(beads, 1))
table(events)

prop.table(table(events))

events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

mean(beads == "blue")

#Introducing Paste and expand grid
number <- "Three"
suit <- "Hearts"
paste(number, suit)


# joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))

#Creating a card deck in R
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", 
             "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number, deck$suit)

# probability of drawing a king
kings <- paste("King", suits)
mean(deck %in% kings)

library(gtools)
permutations(5,2)    # ways to choose 2 numbers in order from 1:5
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index,]

permutations(3,2)    # order matters
combinations(3,2)    # order does not matter

aces<- paste("Ace", suits)
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v=deck) # all possible hands
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

# probability of a natural 21 checking for both ace first and ace second
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|(hands[,2] %in% aces & hands[,1] %in% facecard))
hands


# checking for duplicated bdays in one 50 person group
n <- 50
bdays <- sample(1:365, n, replace = TRUE)    # generate n random birthdays
any(duplicated(bdays))    # check if any birthdays are duplicated

# Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)    # calculates proportion of groups with duplicated bdays


# function to calculate probability of shared bdays across n people
compute_prob <- function(n, B = 10000) {
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n <- seq(1, 60)

prob <- sapply(n, compute_prob)
plot(n, prob)

# function for computing exact probability of shared birthdays for any n
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365   # vector of fractions for mult. rule
  1 - prod(prob_unique)    # calculate prob of no shared birthdays and subtract from 1
}

# applying function element-wise to vector of n values
eprob <- sapply(n, exact_prob)

# plotting Monte Carlo results and exact probabilities on same graph
plot(n, prob)    # plot Monte Carlo results
lines(n, eprob, col = "red")    # add line for exact prob


# Exercise 1
#Two teams, say the Cavs and the Warriors, are playing a seven game championship 
#series. The first to win four games wins the series. The teams are equally good, 
#so they each have a 50-50 chance of winning each game.

#If the Cavs lose the first game, what is the probability that they win the series?

n <- 6
outcomes <- c(0,1)
l <- rep(list(outcomes), n)
possibilities <- expand.grid(l)
results <- rowSums(possibilities)>=4
mean(results)

#Exercise 2
#Confirm the results of the previous question with a Monte Carlo simulation 
#to estimate the probability of the Cavs winning the series after losing the 
#first game.

B<-10000
set.seed(1)
results <- replicate(
  B, {
    cavs_wins <- sample(c(0,1), 6, replace = TRUE)
    sum(cavs_wins)>=4
  }
)
mean(results)

#Exercise 3
#Two teams, and , are playing a seven series game series. 
#Team is better than team and has a chance of winning each game.

p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}

Pr <- sapply(p, prob_win)
plot(p, Pr)


#Exercise 4
N <- seq(1:25)

prob_win <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

Pr <- sapply(N, prob_win)
plot(N, Pr)


#Section 1 Assessment
library(gtools)
library(tidyverse)

P8 <- length(permutations(8,3))/3
P8

jamaica <- factorial(3)
jamaica

Pr <- jamaica/P8 
Pr 

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", 
             "Netherlands", "France", "South Africa")
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica"& winners[2] %in% "Jamaica"& winners[3] %in% "Jamaica")
})

mean(results)


#Exercise 2
length(combinations(6,2))/2*6*3

20*6*3

f <- function(entree){
  print(3*15*entree)
}

options <- seq(1:12)
sapply(options, f)

ff <- function(sides){
  3*6*nrow(combinations(sides,2))
}

options <- 2:12
sapply(options, ff)

plot(options, sapply(options, ff))

data(esoph)
head(esoph)

all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

esoph %>% filter(alcgp == "120+") %>%
  +   summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

# Given that a person is a case, what is the probability that they smoke 10g or
#  more a day?
esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
122/200

# Given that a person is a control, what is the probability that they smoke 10g or
#  more a day?
esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
450/975

esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))
45/all_cases

# For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases))
31/all_cases

esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
10/all_cases

# For cases, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
66/all_cases

####### QUESTION 6a/b/c/d/e/f #######
# For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

# For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

#Continous probability 
library(tidyverse)
library(dslabs)
data(heights)

F <- function(a) mean(x<=a)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# probability of male taller than 70 inches
1-F(70)

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


x <- seq(-4, 4, length = 100)
data.frame(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

x <- heights %>% filter(sex=="Male") %>% pull(height)

# generate simulated height data using normal distribution - both datasets should have n observations
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, avg, s)

# plot distribution of simulated_heights
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg, s)    # generate 800 normally distributed random heights
  max(simulated_data)    # determine the tallest height
})
mean(tallest >= 7*12) 

set.seed(16)

act_scores <- rnorm(10000, mean=20.9, sd=5.7)
mean(act_scores)
sd(act_scores)

sum(act_scores>=36)

mean(act_scores>=30)

mean(act_scores<=10)

x <- seq(1:36)

f_x <- dnorm(x, 20.9, 5.7)

plot(x,f_x)

z_act_scores <- (act_scores-mean(act_scores))/sd(act_scores)

mean(z_act_scores>2)

2*sd(act_scores)+mean(act_scores)

qnorm(0.975, mean(act_scores), sd(act_scores))

qnorm(0.95, mean(act_scores), sd(act_scores))

qnorm(0.95, mean=20.9, sd=5.7)

p <- seq(0.01, 0.99, 0.01)

sample_quantiles <-quantile(act_scores, p)

theoretical_quantiles <- qnorm(p, 20.9, 5.7)
qqplot(theoretical_quantiles, sample_quantiles)
