#----------------------------------------------------------
# Triangle Peg Solitaire (Cracker Barrel Table Game)
# Basic Reinforcement Learning Demo
#
# Created on: 12-20-2017
# Nick Holt, Ph.D.
#----------------------------------------------------------

library(tidyverse)
library(stringr)

#---------------
# The game
#---------------

# The triangle peg solitaire game is comprised of an equilateral-
# triangle-shaped game board with each side spanning 5 slots, meaning
# that there are 15 slots in total. There are 14 pegs, meaning that one
# slot is left empty at the beginning of the game. The goal of the game
# is to "jump" pegs to remove them from the board, leaving as few pegs
# as possible at the end of the game. The game ends when there are no
# adjacent peg sets, meaning that the set of possible "jumps" or actions 
# becomes zero.

# In the canonical Cracker Barrel version of the game, the following
# scoring system is listed:

# - - - - - - - - - - - - - - - - - - - - - - -
# Pegs remaining | Score Outcome
# - - - - - - - - - - - - - - - - - - - - - - - 
# 1 peg          | "You're a genius
# 2 pegs         | "You're pretty smart"
# 3 pegs         | "Just plain dumb"
# 4+ pegs        | "Just plain eg-no-ra-moose"
# - - - - - - - - - - - - - - - - - - - - - - -

# For now, we will ignore the issues inherant in using a simple strategy puzzle as
# a measure of a person's intelligence. However, we will use the simplistic scoring
# system as a guide for the reinforcement learning algorithm's reward, and hopefully by
# the end of the exercise, I will have created an agent that is not "just plain dumb."

#------------------------------------------
# Prequel: Create list of all possible game states
#------------------------------------------

# Since the game board can be represented as 15 positions, each with
# two possible states (0 or 1), we can create a list of all possible states
# by generating all possible 15-bit binary sequences (excluding the cases where
# all positions equal 0 or 1, because there are only 14 pegs, and the game ense
# when there is one peg remaining).

# Theoretically, then, there are 2^15 possible game states. Of course, some
# other game states may be impossible, but there's no need to
# simulate thousands of games to find out. 

# calculate number of states
2^15 - 2 # number of states

# 0 and 2^15 are invalid game states
# range is 1:32766

# generate all 15-digit binary combinations
states <- NULL
for(i in 1:32766){
        x <- intToBits(i)[1:15]
        states <- rbind(states, x)
}
states <- data.frame(states)
states <- map(states, as.numeric)
states <- data.frame(states)
# write_csv(states, "game_states.csv")

# create game states represented as strings
states <- read_csv("game_states.csv")

state_list <- NULL
for(i in seq_along(1:length(states$X1))){
        pattern <- str_c(as.character(states[i,]), collapse = "")
        state_list <- rbind(state_list, pattern)
}
state_list <- data.frame(state_list)

#---------------------------
# Step 1: Initialize Game
#---------------------------

# Need to create representation of initial game state
        
        # 0 = empty slot
        # 1 = peg

# initialize game board representation as a data frame of coordinates
initialize_game <- function(){
        board <- NULL
        board$x <- c(rep(1,5), rep(2,4), rep(3,3), rep(4,2), 5)
        board$y <- c(1,2,3,4,5,1,2,3,4,1,2,3,1,2,1)
        board$peg <- c(1,1,1,1,1,1,0,1,1,1,1,1,1,1,1)
        board <- data.frame(board) %>%
                mutate(id = paste0(x, y)) %>%
                mutate(id = as.numeric(id))
        return(board)
}

# function to create list of all valid moves for a given set of coordinates
legal_moves <- function(x, y){
        end_x <- c(x, # y - 2
                     x, # y + 2
                     x + 2, # y
                     x + 2, # y + 2
                     x + 2, # y - 2
                     x - 2, # y
                     x - 2, # y + 2
                     x - 2) # y - 2
        end_y <- c(y - 2, 
                     y + 2, 
                     y, 
                     y + 2, 
                     y - 2, 
                     y, 
                     y + 2, 
                     y - 2)
        moves <- data.frame(cbind(end_x, end_y)) %>%
                filter(end_x > 0 & end_y > 0) %>%
                filter(end_x + end_y <= 6 & end_x + end_y >= 2)
        return(moves)
}

# Create list of all possible legal moves based on game board
list_of_legal_moves <- NULL
board <- initialize_game()
for(i in seq_along(board$x)){
        x <- board$x[i]
        y <- board$y[i]
        possible <- data.frame(legal_moves(x, y))
        possible$begin_x <- x
        possible$begin_y <- y
        list_of_legal_moves <- rbind(list_of_legal_moves, possible %>% select(begin_x, begin_y, end_x, end_y))
}

        # there are 38 legal actions, depending on the state of the game board

# Complete action dictionary with action_id for each unique action for reinforcement learning
action_dictionary <- list_of_legal_moves %>%
        mutate(begin_id = str_c(begin_x, begin_y),
               end_id = str_c(end_x, end_y),
               action_id = str_c(begin_x, begin_y, end_x, end_y)
               ) %>%
        select(begin_id, end_id, action_id)

# function that checks game state, and provides list of currently available moves (actions)
check_valid_moves <- function(game_board){
        empty_slots <- which(game_board$peg == 0)
        begin_slots <- NULL
        for(i in seq_along(empty_slots)){
                x <- game_board$x[empty_slots[i]]
                y <- game_board$y[empty_slots[i]]
                begin_slots <- rbind(begin_slots, legal_moves(x, y))
        }
        # list of all moves from begin slots
        actions <- NULL
        for(i in seq_along(begin_slots[,1])){
                x <- begin_slots[,1][i]
                y <- begin_slots[,2][i]
                possible <- data.frame(legal_moves(x, y))
                possible$begin_x <- x
                possible$begin_y <- y
                actions <- rbind(actions, possible %>% select(begin_x, begin_y, end_x, end_y))
        }
        # filter actions based on which end slots are empty
        empty_coords <- NULL
        for(i in seq_along(empty_slots)){
                x <- game_board$x[empty_slots[i]]
                y <- game_board$y[empty_slots[i]]
                empty_coords <- rbind(empty_coords, paste0(x,y))
        }
        empty_coords <- as.numeric(empty_coords)
        actions <- actions %>%
                mutate(end_id = paste0(end_x, end_y)) %>%
                mutate(end_id = as.numeric(end_id)) %>%
                filter(end_id %in% empty_coords) %>%
                distinct()
        # filter actions based on whether there is a peg between begin and end coords
        peg_between <- NULL
        for(i in seq_along(actions[,1])){
               peg_between <- rbind(peg_between, 
                                    paste0(actions[i,3] + ((actions[i,1] - actions[i,3])/2), 
                                      actions[i,4] + ((actions[i,2] - actions[i,4])/2)
                                      )
               )
        }
        peg_between <- data.frame(peg_between) %>% 
                mutate(action_index = 1:length(peg_between)) %>%
                filter(peg_between %in% game_board$id[game_board$peg == 1]) %>%
                distinct()
        actions <- actions[peg_between$action_index,] %>% 
                mutate(begin_id = paste0(begin_x, begin_y)) %>%
                mutate(begin_id = as.numeric(begin_id)) %>%
                filter(begin_id %in% game_board$id[game_board$peg == 1]) %>%
                mutate(action_id = str_c(begin_id, end_id))
        # return actions
        return(actions)
}

# selected_action <- sample_n(check_valid_moves(board), 1)

# function that updates board based on a move that is selected
update_state <- function(game_board, action){
        # action
        action <- action %>% 
                mutate(begin_id = paste0(begin_x, begin_y)) %>%
                mutate(begin_id = as.numeric(begin_id))
        
        action$peg_between <- paste0(action[1,3] + ((action[1,1] - action[1,3])/2),
                              action[1,4] + ((action[1,2] - action[1,4])/2)
                              )
        game_board$peg[game_board$id == action$begin_id] <- 0
        game_board$peg[game_board$id == action$end_id] <- 1
        game_board$peg[game_board$id == action$peg_between] <- 0
        return(game_board)
}
        
# board <- update_state(board, selected_action)

# simulate peg game and return pegs remaining
simulate_peg_game <- function(){
        board <- initialize_game()
        remaining_pegs <- NULL
        while(nrow(check_valid_moves(board)) != 0){
                selected_action <- sample_n(check_valid_moves(board), 1)
                board <- update_state(board, selected_action)
                remaining_pegs <- sum(board$peg)
        }
        return(remaining_pegs)
}

#--------------------------------------
# Simulate and Analyze Game Properties
#--------------------------------------

# simulate 100 games
set.seed(1288)
peg_data <- rep(NA, 5000)
reps <- 1:5000
for(i in seq_along(reps)){
        peg_data[i] <- simulate_peg_game()
}

outcomes <- data.frame(peg_data)

# plot outcomes
ggplot(outcomes, aes(peg_data)) +
        geom_histogram(binwidth = 1, aes(fill = ..count..)) +
        scale_fill_gradient("Count", low = "skyblue4", high = "skyblue3") + 
        xlab("\nNumber of Pegs Remaining") +
        ylab("Count\n") +
        theme(legend.position="none") +
        scale_x_continuous(breaks = 1:10)

# write.csv(outcomes, "simulated_outcomes_n=5000.csv")

# I'm unaware of the history of the game's development (so this could 
        # be intentional), but it looks like the scoring system listed on the 
        # game board is surprisingly insightful. 

        # outcome summary table
        outcomes.t <- outcomes
        colnames(outcomes.t) <- "Result"
        outcome_sum <- outcomes.t %>%
                group_by(Result) %>%
                summarize(Count = n(),
                          Probability = Count/5000)
        
        # Here are the probabilities of the outcomes based on a random 
        # guessing strategy
        
        # - - - - - - - - - - - - - - -
        # Pegs Remaining | Probability
        # - - - - - - - - - - - - - - - 
        # 1              | .001
        # 2              | .036
        # 3              | .195
        # 4              | .489
        # 5              | .211
        # 6              | .032
        # 7              | .011
        # 8              | 0
        # 9              | 0
        # 10             | .024
        # - - - - - - - - - - - - - - -
        
        # We can see from the table that the worst score (4+ remaining) will occur
        # ~49% of the time under a random strategy, and the best score (1 remaining) 
        # is exceptionally rare (0.1% occurence) when guessing.
        
        # This simulation provides evidence that the game is not 
        # trivially easy to beat. The goal in the remainder of this project is
        # to train a machine learning system to beat the triangle peg solitaire game,
        # which will reveal the optimal strategy that a human player should adopt.
        
        # **This data is based on the open position being the slot directly below the
        # slot at the top of the triangle. It would be interesting to examine if/how 
        # changing the open slot at the beginning of the game affects the outcomes.
        
#---------------------------
# Profiling Sim Function
#---------------------------

# Profiling the simulation function to look for bottlenecks
Rprof("sim.out")
simulate_peg_game()
Rprof(NULL)

prof_sum <- summaryRprof("sim.out")
        
        # definitely room for improvement at a later time


#--------------------------------------------------------
# Step 2: Format Virtual Game Data for Reinforcement Learning
#--------------------------------------------------------

# install and load the reinforcement learning package
devtools::install_github("nproellochs/ReinforcementLearning")

        # From the CRAN Description:

        # "The ReinforcementLearning package uses experience replay to learn 
        # an optimal policy based on past experience in the form of sample sequences 
        # consisting of states, actions and rewards. Here each training example consists 
        # of a state transition tuple (s,a,r,s_new), as described below.

                # s The current environment state.
                # a The selected action in the current state.
                # r The immediate reward received after transitioning from the current state to the next state.
                # s_new The next environment state.

        # Note:
                # The input data must be a dataframe in which each 
                # row represents a state transition tuple (s,a,r,s_new)."

# Reward engineering

        # A key question I have in designing the reinforcement learning system
        # is how to decide what reward structure to use. It seems that an arbitrary
        # reward structure is not the best approach.

        # Since reinforcement learning optimizes for sparse time-delayed rewards,
        # it may be best to only reward the agent with the final score for the round

                # Possible Reward Structure:
                
                # 1 peg remaining: 8 points
                # 2 pegs remaining: 4 points
                # 3 pegs remaining: 2 points
                # 4 pegs remaining: 1 point
                # 5+ pegs remaining: 0 points


                # Alternative Reward Structure:
                
                # 1 peg remaining: 1 points
                # 2+ pegs remaining: -1 points


# simulate game and record log of game states with actions and rewards
simlog_peg_game <- function(){
        board <- initialize_game()
        game_log <- NULL
        move <- 0
        while(nrow(check_valid_moves(board)) != 0){
                move <- move + 1
                # record begin state
                begin_state <- str_c(board[,3], collapse = "")
                # find valid moves
                valid_moves <- check_valid_moves(board)
                # randomly select an action from the list of available actions
                selected_action <- sample_n(valid_moves, 1)
                # record the action_id of the selected action
                action_id <- selected_action$action_id
                # update the board based on the selected action
                board <- update_state(board, selected_action)
                # record end state after action
                end_state <- str_c(board[,3], collapse = "")
                # calculate remaining pegs
                remaining_pegs <- sum(board$peg)
                # update game log
                game_log <- rbind(game_log, cbind(move, begin_state, action_id, end_state, remaining_pegs))
        }
        # calculate reward
        game_log <- data.frame(game_log)
        game_log$reward <- 0
        game_log$reward[length(game_log$reward)] <- ifelse(game_log$remaining_pegs[length(game_log$remaining_pegs)] == 1, 1, -1)
        return(game_log)
}


#-------------------------------
# simulate lots of games and record current state (s), action selected (a), reward received (r), and next state (s_new)
#-------------------------------
library(progress)
set.seed(1918)
pegsolitaire <- NULL
reps <- 1:7000
pb <- progress_bar$new(format = " simulating [:bar] :percent eta: :eta",
                       total = length(reps),
                       clear = FALSE, 
                       width= 100)
system.time(for(i in seq_along(reps)){
        pegsolitaire <- rbind(pegsolitaire, simlog_peg_game())
        pb$tick()
})

# simlog 2000 games
# user   system  elapsed 
# 14360.05     2.69 14370.83

# check to see how many examples of winning games exist in sim data
pegsolitaire %>% filter(remaining_pegs == 1)

# clean up df %>% to tibble
pegsolitaire <- as_tibble(pegsolitaire)
glimpse(pegsolitaire)

# states and actions must be of type character, and rewards must be numeric
train <- pegsolitaire %>%
        mutate(State = as.character(begin_state),
               Action = as.character(action_id),
               NextState = as.character(end_state),
               Reward = as.numeric(reward)
               ) %>%
        select(State, Action, NextState, Reward)

# write to file
write_csv(pegsolitaire, "simulated_games_n=7000.csv")

# Step 3: Train Agent to Play Game
library(ReinforcementLearning)

# Define reinforcement learning parameters
control <- list(alpha = 0.1, gamma = 0.5, epsilon = 0.1)

# Perform reinforcement learning
model <- ReinforcementLearning(train, s = "State", a = "Action", r = "Reward", 
                               s_new = "NextState", control = control)

# Print result
print(model)


# Step 4: Evaluate Success
