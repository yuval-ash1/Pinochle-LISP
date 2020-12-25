#||
     ************************************************************
     * Name:  Yuval Ashkenazi                                   *
     * Project:  Project #2 LISP Pinochle                       *
     * Class:  CMPS366- Organization of Programming Languages   *
     * Date:  10/20/2020                                        *
     ************************************************************
||#

(load 'getters.lsp)
(load 'listModificationFuncs.lsp)
(load 'printingFunctions.lsp)
(load 'meldFunctions.lsp)
(load 'cardFindersForStrategy.lsp)

(setf *random-state* (make-random-state t))

#|| *********************************************************************
Function Name: tossCoin
Purpose: Determine the who starts in case of a tie
		as well as on the first turn of the first round
Parameters:
            num, an integer that represents the user's choice,
				which could be heads or tails
Return Value: The player that is going start
Local Variables:
            none
Algorithm:
            1)validate number
			2)generate a random number
			3)if random = num human starts. otherwise computer starts
Assistance Received: none
********************************************************************* ||#
(defun tossCoin (num)
	(cond 
		((or (= num 1) (= num 0))
			(princ "Coin shows: ")
			(cond
				((= num (princ (random 2)))
					(terpri) (princ "Human Starts") (terpri)
					'human
				)
				( t 
					(terpri) (princ "Computer Starts") (terpri)
					'computer
				)
			)
		)
		;making sure user inputs a valid number
		(t
			(princ "Number has to be 0 or 1, try again: ")
			(tossCoin(read))
		)
	)
)


#|| *********************************************************************
Function Name: setDeck
Purpose: Initialize the deck of cards for the game
Parameters:
            none
Return Value: The deck of the game which contains 48 cards.
Local Variables:
            none
Algorithm:
            1)create a list that contains all the deck
				cards for the game
Assistance Received: none.
********************************************************************* ||#
(defun setDeck()
	(list '9S '9C '9D '9H '9S '9C '9D '9H
		  'XS 'XC 'XD 'XH 'XS 'XC 'XD 'XH
		  'JS 'JC 'JD 'JH 'JS 'JC 'JD 'JH	
		  'QS 'QC 'QD 'QH 'QS 'QC 'QD 'QH
		  'KS 'KC 'KD 'KH 'KS 'KC 'KD 'KH
		  'AS 'AC 'AD 'AH 'AS 'AC 'AD 'AH
	)
)



#|| *********************************************************************
Function Name: shuffleDeck
Purpose: To shuffle the deck for a new round
Parameters:
            sequence- a list in which I am trying to shuffle (deck)
			count- the length of the sequence (deck)
Return Value: A shuffled deck
Local Variables:
            none
Algorithm:
            1)if count is greater than/equal to 2
				-switch 2 elements with one another
				-call function again (recursively) with a decremented count
			2)else-return the sequence
				
Assistance Received: 
		I used a function found on this webside as a reference:
		https://stackoverflow.com/questions/49490551/how-to-shuffle-list-in-lisp
		and modified it to be recursive instead of the use of loops
********************************************************************* ||#
(defun shuffleDeck(sequence count)
	(cond 
		((>= count 2)
			(rotatef (elt sequence (random count))
						(elt sequence (1- count)))
			(shuffleDeck sequence (- count 1))
		)
		( t
			sequence
		)
	)
)				

#|| *********************************************************************
Function Name: createMainList
Purpose: Initialize the game list
Parameters:
            none
Return Value: The initialized list of a new game
Local Variables:
            none
Algorithm:
            1)create the initialized list
			2)return the list
Assistance Received: none.
********************************************************************* ||#
(defun createMainList ()
	(list 1 0 0 () () () 0 0 () () () () () ())
)

(defun additionalRoundList (roundNum CGameScore HGameScore nextPlayer)
	(list roundNum CGameScore '(0) '() '() '() HGameScore '(0) '() '() '() '() '() nextPlayer)
)

#|| *********************************************************************
Function Name: validateCMenu
Purpose: validate the user's input for the menu displayed
		before the computer's turn
Parameters:
            none
Return Value: The number which represents the user's choice
Local Variables:
            choice- a variable containing the user's input
Algorithm:
            1)read input from the user (user's choice)
			2)if input is valid- return the input as a number
			3)else- ask for a valid input (recursively)
Assistance Received: none.
********************************************************************* ||#
(defun validateCMenu ()
	(princ '-->)
	(let ((choice (read)))
		(cond 
			((eq choice 1)
				1
			)
			((eq choice 2)
				2
			)
			((eq choice 3)
				3
			)
			( t
				(princ "Invalid input, possible inputs are 1, 2, and 3. Please try again")(terpri)
				(validateCMenu)
			)
		)
	)
)

#|| *********************************************************************
Function Name: validateHMenu
Purpose: validate the user's input for the menu displayed
		before the human's turn
Parameters:
            none
Return Value: The number which represents the user's choice
Local Variables:
            choice- a variable containing the user's input
Algorithm:
            1)read input from the user (user's choice)
			2)if input is valid- return the input as a number
			3)else- ask for a valid input (recursively)
Assistance Received: none.
********************************************************************* ||#
(defun validateHMenu ()
	(princ '-->)
	(let ((choice (read)))
		(cond 
			((eq choice 1)
				1
			)
			((eq choice 2)
				2
			)
			((eq choice 3)
				3
			)
			((eq choice 4)
				4
			)
			( t
				(princ "Invalid input, possible inputs are 1, 2, 3, and 4. Please try again")(terpri)
				(validateHMenu)
			)
		)
	)
)

#|| *********************************************************************
Function Name: dealCards
Purpose: This function assigns the cards at the beginning of a round
			  4 cards a player at a time (x3)
			  It also sets the trump card to be the 25th card in the stock
			  and deletes the first 25 cards from the deck
Parameters:
            prevoiusList- the previous game list that holds the information
						of previous rounds
			deck- a shuffled deck
			beginner- the name of the player that begins this round (human/computer)
Return Value: the updated game list, containing: initialized values for the game
Local Variables:
            none
Algorithm:
            1)create a game list containing the information from previous list
			2)players hands get 12 cards, 4 at a time. 4 cards are dealt to beginner
				,then 4 cards are dealt to the other player (this happen 3 times)
			3) trump gets 25th card from the beck
			4) deck gets same deck minus the first 25 cards
Assistance Received: none.
********************************************************************* ||#
(defun dealCards (previousList deck beginner)
	
	(list 
		;Round Number- doesn't change
		(first previousList)
		;Computer Game Score- doesn't change
		(first (rest previousList))
		;Computer Round Score- doesn't change
		(first (rest (rest previousList)))
		;Computer Hand
		(cond
			( (eq beginner 'computer)
				(list
					(first deck)
					(first (rest deck))
					(first (rest (rest deck)))
					(first (rest (rest (rest deck))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))
				)
			)
			( t
				(list
					(first (rest (rest (rest (rest deck)))))
					(first (rest (rest (rest (rest (rest deck))))))
					(first (rest (rest (rest (rest (rest (rest deck)))))))
					(first (rest (rest (rest (rest (rest (rest (rest deck))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))))))
				)
			)
		)
		
		;Computer Capture- doesn't change
		(first (rest (rest (rest (rest previousList)))))
		;Computer Meld- doesn't change
		(first (rest (rest (rest (rest (rest previousList))))))
		;Human Game Score- doesn't change
		(first (rest (rest (rest (rest (rest (rest previousList)))))))
		;Human Round Score- doesn't change
		(first (rest (rest (rest (rest (rest (rest (rest previousList))))))))
		;Human Hand
		(cond
			( (eq beginner 'human)
				(list
					(first deck)
					(first (rest deck))
					(first (rest (rest deck)))
					(first (rest (rest (rest deck))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))
				)
			)
			( t
				(list
					(first (rest (rest (rest (rest deck)))))
					(first (rest (rest (rest (rest (rest deck))))))
					(first (rest (rest (rest (rest (rest (rest deck)))))))
					(first (rest (rest (rest (rest (rest (rest (rest deck))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))
					
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))))
					(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck))))))))))))))))))))))))
				)
			)
		)
		;Human Capture- doesn't change
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest previousList))))))))))
		;Human Meld- doesn't change
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest previousList)))))))))))
		;Trump Card- gets the 25th card from the stock
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))))))
		;Stock Pile- is now smaller by 24 cards (because of the cards that were dealt)
		(rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest deck)))))))))))))))))))))))))
		;Next Player- doesn't change
		beginner
	)

)

#|| *********************************************************************
Function Name: vali
Purpose: validate the user's in many occurances 
		(using this function to validate morethan 1 scenario)
Parameters:
            input- contains the user's input
Return Value: the user's input after validation
Local Variables:
            none.
Algorithm:
            1)check: if input is valid- return the input as a number
			2)else- ask for a valid input and call function with the
				new input(recursively)
Assistance Received: none.
********************************************************************* ||#
(defun vali (input)
	(terpri)
	(cond
		( (eq input 1)
			'1
		)
		( (eq input 2)
			'2
		)
		( t
			(princ "Invalid input, try again")
			(terpri)
			(princ "Note: possible inputs are 1 or 2")
			(terpri)
			(princ '-->)
			(vali (read))
		)
	)
)

#|| *********************************************************************
Function Name: game
Purpose: initialize and start the game.
Parameters:
            none
Return Value: none
Local Variables:
            gameList- an initialized game list (after cards are dealt and trump declared)
			endGameList- the list after the game
			humanTotalScore- contains the human's game score + round score
			computerTotalScore- contains the computer's game score + round score
Algorithm:
            1)prompt user to choose a coin side
			2)set gameList to be an initialized game list 
			3)set endGameList to be the updated game list after calling round
			4)calculate human's and computer's total score
			5)declate the winner of the game base on the total scored calculated above
Assistance Received: none.
********************************************************************* ||#
(defun game ()
	(princ "Since this is the first round, we will toss a coin to decide who starts") (terpri)
	(princ "Player 1, please enter 0 for heads or 1 for tails: ") (terpri)
	(princ '-->)
	(let* ((gameList (dealCards (createMainList) (shuffleDeck (setDeck) (length (setDeck))) (tossCoin(read))))
		(endGameList (round gameList))
		(humanTotalScore (+ (getHGameScore endGameList) (getHRoundScore endGameList)))
		(computerTotalScore (+ (getCGameScore endGameList) (getCRoundScore endGameList))))
		
		(princ "The game ended!") (terpri) (terpri)
		(princ "Human Score: ") (princ humanTotalScore) (terpri)
		(princ "Computer Score: ") (princ computerTotalScore) (terpri)
		(cond
			((> humanTotalScore computerTotalScore)
				(princ "Human won this game!") (terpri)
			)
			((> computerTotalScore humanTotalScore)
				(princ "Computer won this game!") (terpri)
			)
			( t
				(princ "It's a tied game!") (terpri)
			)
		)
		(princ "Thank you for playing Pinochle :)") (terpri)
	)
)

#|| *********************************************************************
Function Name: loadedGame
Purpose: load a and start the loaded game
Parameters:
            gameList- the list of the loaded game (contains all the game's info)
Return Value: none
Local Variables:
			endGameList- the list after the game
			humanTotalScore- contains the human's game score + round score
			computerTotalScore- contains the computer's game score + round score
Algorithm:
			1)set endGameList to be the update game list after calling round
			2)calculate human's and computer's total score
			3)declate the winner of the game base on the total scored calculated above
Assistance Received: none.
********************************************************************* ||#
(defun loadedGame (gameList)
	(let* ((endGameList (round gameList))
		   (humanTotalScore (+ (getHGameScore endGameList) (getHRoundScore endGameList)))
		   (computerTotalScore (+ (getCGameScore endGameList) (getCRoundScore endGameList))))
		
		(princ "The game ended!") (terpri) (terpri)
		(princ "Human Score: ") (princ humanTotalScore) (terpri)
		(princ "Computer Score: ") (princ computerTotalScore) (terpri)
		(cond
			((> humanTotalScore computerTotalScore)
				(princ "Human won this game!") (terpri)
			)
			((> computerTotalScore humanTotalScore)
				(princ "Computer won this game!") (terpri)
			)
			( t
				(princ "It's a tied game!") (terpri)
			)
		)
		(princ "Thank you for playing Pinochle :)") (terpri)
	)
)

#|| *********************************************************************
Function Name: round
Purpose: start a new round (ends when both players' hands are empty)
Parameters:
            gameList- the list of the game (contains all the game's info)
Return Value: the updated game list at the end of the round
Local Variables:
			endRoundList- the updated game list at the end of the round
			newRoundList- the updated game list after calling a turn
Algorithm:
			1)print game list
			2)call turn
			3)if human hand and computer hand are not empty
				-call round again
			4)displayout both player's scores
			5)determine round winner
			6)ask user if they want to play another round
				-if yes: call another round with the local updated game list
				-if no: return the updated list
Assistance Received: none.
********************************************************************* ||#
(defun round (gameList)
	(PrintBeforeTurn gameList)
	(let* ((endRoundList (turn gameList)))
		(cond
			((and (not (eq (getHHand gameList) nil))
			  (not (eq (getCHand gameList) nil)) )
				(round endRoundList)
			)
			( t
				;Determining who won the round
				(terpri) (princ "Round number: ") (princ (getRoundNum endRoundList)) (princ " ended") (terpri)
				(princ "Human score: ") (princ (getHRoundScore endRoundList)) (terpri)
				(princ "Computer score: ") (princ (getCRoundScore endRoundList)) (terpri)
				(cond
					((> (getHRoundScore endRoundList) (getCRoundScore endRoundList))
						(princ "Human Won!") (terpri)
						(princ "To play another round press 1, otherwise press 2:") (terpri)
						(princ '-->)
						(cond 
							((= (vali(read)) 1)
								(let ((newRoundList (setListForNewRound 
																endRoundList
																(dealCards (createMainList) (shuffleDeck (setDeck) (length (setDeck))) 'human))))
									
									(round newRoundList)
								)
							)
							( t
								endRoundList
							)
						)
					)
					((< (getHRoundScore endRoundList) (getCRoundScore endRoundList))
						(princ "Computer Won!") (terpri)
						(princ "To play another round press 1, otherwise press 2:") (terpri)
						(princ '-->)
						(cond 
							((= (vali(read)) 1)
								(let ((newRoundList (setListForNewRound 
																endRoundList
																(dealCards (createMainList) (shuffleDeck (setDeck) (length (setDeck))) 'computer))))
									
									(round newRoundList)
								)
							)
							( t
								endRoundList
							)
						)
					)
					( t
						(princ "It's a tie!") (terpri)
						(princ "To play another round press 1, otherwise press 2:") (terpri)
						(princ '-->)
						(cond 
							((= (vali(read)) 1)
							(princ "Since it's a tie, we will toss a coin to decide who starts") (terpri)
							(princ "Player 1, please enter 0 for heads or 1 for tails: ") (terpri)
								(let ((newRoundList (setListForNewRound 
																endRoundList
																(dealCards (createMainList) (shuffleDeck (setDeck) (length (setDeck))) (tossCoin (read))))))
									
									(round newRoundList)
								)
							)
							( t
								endRoundList
							)
						)
					)
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: turn
Purpose: play a turn (where both players play)
Parameters:
            roundList- the list of the round (contains all the game's info)
Return Value: the updated game list at the end of the turn
Local Variables:
			leadCard- the card player by the lead player
			chaseCard- the card player by the chase player
			leadPoints- points to be recieved for the lead card
			chasePoints- points to be recieved for the chase card
			turnPoints- total points for lead and chase (leadPoints+chasePoints)
			newRoundList- an round list which contains the updated hands of the players
							after playing the cards
			turWinner- the name of the winner of the turn (computer/human)
			meldNameCode-the number associated with the meld from the menu displayed to players
			meldList- the list of cards of a meld that has been played by the 
						winner (nil if no meld played)
			meldPoints- points for the med that has been player (0 if no meld played)
			endTurnList- the updated list after the turn, containing updated scores, meld lists, 
						capture piles, deck, etc.
Algorithm:
			1)if human hand and computer hand are not empty
				-if computer is the next player-computer plays first
				-else human plays first
			2)each play a card
			3)winner is determined
			4)hands are updated
			5)winner is prompted to perform a meld
			6)update all neccessary values in round list
			7)return round list
Assistance Received: none.
********************************************************************* ||#
(defun turn (roundList)
	(cond
		;If both players hands are not empty: play and call turn again
		((and (not (eq (getHHand roundList) nil))
			  (not (eq (getCHand roundList) nil)) )
			(cond
				;if next player is computer, computer plays first
				( (eq (getNextP roundList) 'computer)
					(let* (
							(leadCard (computerPlay roundList nil))
							(chaseCard (humanPlay roundList leadCard))
							(leadPoints (cardPoints leadCard))
							(chasePoints (cardPoints chaseCard))
							(turnPoints (+ leadPoints chasePoints))
							(newRoundList (updateHands leadCard chaseCard roundList))
							(turnWinner (winnerName (getNextP roundList) (winner leadCard chaseCard (getTrump roundList))))
							(meldNameCode (meldName newRoundList turnWinner))
							(meldList (wouldYouMeld newRoundList meldNameCode turnWinner))
							(meldPoints (pointsForMeld meldList meldNameCode))
							(endTurnList (dealTurnCards newRoundList turnWinner turnPoints leadCard chaseCard meldList meldPoints))
							)
							endTurnList
					)
				)
				;Otherwise- human plays first
				( t
					(let* (
							(leadCard (humanPlay roundList nil))
							(chaseCard (computerPlay roundList leadCard))
							(leadPoints (cardPoints leadCard))
							(chasePoints (cardPoints chaseCard))
							(turnPoints (+ leadPoints chasePoints))
							(newRoundList (updateHands chaseCard leadCard roundList))
							(turnWinner (winnerName (getNextP roundList) (winner leadCard chaseCard (getTrump roundList))))
							(meldNameCode (meldName newRoundList turnWinner))
							(meldList (wouldYouMeld newRoundList meldNameCode turnWinner))
							(meldPoints (pointsForMeld meldList meldNameCode))
							(endTurnList (dealTurnCards newRoundList turnWinner turnPoints leadCard chaseCard  meldList meldPoints))
							)
							endTurnList
					)
				)
			)
		)
		( t
			;Both hands are empty-return the round list
			roundList
		)
	)
)

#|| *********************************************************************
Function Name: winnerName
Purpose: determine the fist to play in the next turn
Parameters:
            nextP- the beginner of the previous turn
			cardWinner- the card that won the trn (lead/chase)
Return Value: the name of the beginner of the next turn
Local Variables:
			none
Algorithm:
			1)if nextP = comptuer
				-if card winner = lead
					-return computer
				-else
					-return human
			2)else
				-if card winner = lead
					-return human
				else
					return comptuer
Assistance Received: none.
********************************************************************* ||#
(defun winnerName (nextP cardWinner)
	(cond
		((eq nextP 'computer)
			(cond 
				((eq cardWinner 'lead)
					(terpri) (princ "Computer won this turn") (terpri)
					'computer
				)
				( t
					(terpri) (princ "Human won this turn") (terpri)
					'human
				)
			)
		)
		( t
			(cond 
				((eq cardWinner 'lead)
					(terpri) (princ "Human Won this turn") (terpri)
					'human
				)
				( t
					(terpri) (princ "Computer won this turn") (terpri)
					'computer
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: cardPoints
Purpose: To determine how many points a card is worth
Parameters:
            card1- a card played, in which we want to get the point value of
Return Value: the number of points the card is worth
Local Variables:
			none
Algorithm:
			1)if card is of type 9
				-return 0
			2)if card is of type j
				-return 2
			3)if card is of type q
				-return 3
			4)if card is of type k
				-return 4
			5)if card is of type x
				-return 10
			1)if card is of type a
				-return 11
Assistance Received: none.
********************************************************************* ||#
(defun cardPoints (card1)
	;this function should return the number of points that should be added to the winner of the round
	;for the lead and chase of this turn
		(cond
			( (eq (getType card1) '#\9 )
				0)
			( (or (eq (getType card1) '#\j) (eq (getType card1) '#\J))
				2)
			( (or (eq (getType card1) '#\q) (eq (getType card1) '#\Q))
				3)
			( (or (eq (getType card1) '#\k) (eq (getType card1) '#\K))
				4)
			( (or (eq (getType card1) '#\x) (eq (getType card1) '#\X))
				10)
			( (or (eq (getType card1) '#\a) (eq (getType card1) '#\A))
				11)	
		)
)

#|| *********************************************************************
Function Name: getACard
Purpose: deals one card to a player
Parameters:
            stock- the stock of the current round
			trump- the trump card of the current round
Return Value: the card the user is getting (adealt card)
Local Variables:
			none
Algorithm:
			1)stock is not empty
				-return the first card in the stock
			2)else if trump suit does not equal trump type (happens after trump is dealt)
				-return trump
			3)else
				-return nil (no cards left to be dealt)
Assistance Received: none.
********************************************************************* ||#
(defun getACard (stock trump)
	;(princ "trump suit ") (princ (getSuit trump)) (terpri)
	(cond
		((not (eq stock nil))
			(first stock)
		)
		( (not (eq (getType trump) (getSuit trump)))
			(princ "returning trump card")(terpri) 
			trump	
		)
		(t
			nil
		)
	)
)

#|| *********************************************************************
Function Name: dealTurnCards
Purpose: This function deals a card to each player's hand, and updates the
		list with all the new inpormation at the end of a turn
Parameters:
            gameList- the list of the game
			winnerName- the name of the winner
			winnerPoints- the number of points winner gets for lead and chase
			leadCard-the card played by the first player
			chaseCard- the card played by the second player
			meldList- a list containing the meld card that has been performed by the
						winner of the turn
			meldPoints- the number of point the winner of the turn got for a
						meld (if performed)
Return Value: the updated list at the end of a turn
Local Variables:
			firstCard- the first card from the deck to be given to the turn winner
			secondCard- the second card from the deck to be given to the other player
Algorithm:
			1)define firstCard and secondCard
			2)create a list containing all the updated values using the parameters' values 
Assistance Received: none.
********************************************************************* ||#
(defun dealTurnCards (gameList winnerName winnerPoints leadCard chaseCard meldList meldPoints)
	(let* ((firstCard (getACard (getStock gameList) (getTrump gameList)))
			(secondCard (getACard (rest (getStock gameList)) (getTrump gameList))))
		
		(cond
			((eq winnerName 'computer)
				(list
					;Round number Stays the same
					(getRoundNum gameList)
					;game Score stays the same
					(getCGameScore gameList)
					;Round score gets previous score + pts for captured cards + pts for meld
					(+ winnerPoints (getCRoundScore gameList) meldPoints)
					;adding a card to the player only if the stock is not empty (if empty- hand doesn't change)
					(cond 
						((not (or (eq firstCard nil) ))
							(cons
							firstCard (getCHand gameList))
						)
						(t
							(getCHand gameList)
						)
					)
					;adding turn cards to capture pile
					(cons leadCard (cons chaseCard (getCCapture gameList)))
					;adding the meld to previous melds
					;if player performed a successful meld- add it to the previous melds list
					(cond
						((not (eq meldList nil))
							(cons meldList (getCMeld gameList))
						)
						( t
							(getCMeld gameList)
						)
					)
					(getHGameScore gameList)
					(getHRoundScore gameList)
					
					(cond 
						((not (eq secondCard nil))
							(cons
							secondCard (getHHand gameList))
						)
						(t
							(getHHand gameList)
						)
					)
					(getHCapture gameList)
					(getHMeld gameList)
					(cond
						((and (eq (getType secondCard) (getType (getTrump gameList)))
							  (eq (getSuit secondCard) (getSuit (getTrump gameList)))
							  (eq (length (getStock gameList)) 1))
							
							(getSuit (getTrump gameList))
						)
						( t
							(getTrump gameList)
						)
					)
					(rest (rest (getStock gameList)))
					winnerName
				)
			)
			( t
				(list
					(getRoundNum gameList)
					(getCGameScore gameList)
					(getCRoundScore gameList)
					(cond 
						((not (eq secondCard nil))
							(cons
							secondCard (getCHand gameList))
						)
						(t
							(getCHand gameList)
						)
					)
					(getCCapture gameList)
					(getCMeld gameList)
					(getHGameScore gameList)
					(+ winnerPoints (getHRoundScore gameList) meldPoints)
					(cond 
						((not (eq firstCard nil))
							(cons
							firstCard (getHHand gameList))
						)
						(t
							(getHHand gameList)
						)
					)
					(cons leadCard (cons chaseCard (getHCapture gameList)))
					;if player performed a successful meld- add it to the previous melds list
					(cond
						((not (eq meldList nil))
							(cons meldList (getHMeld gameList))
						)
						( t
							(getHMeld gameList)
						)
					)
					(cond
						((and (eq (getType secondCard) (getType (getTrump gameList)))
							  (eq (getSuit secondCard) (getSuit (getTrump gameList)))
							  (eq (length (getStock gameList)) 1))
							
							(getSuit (getTrump gameList))
						)
						( t
							(getTrump gameList)
						)
					)
					(rest (rest (getStock gameList)))
					winnerName
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: updateHands
Purpose: This function returns the game list, with upadted hands for both players
			the lead and chase cards dropped from each hand respectively 
Parameters:
            computerCard- the card played by the computer
			humanCard- the card played by the human
			gameList- the list of the game containing all the game's info
Return Value: the updated list with the computer card dropped from the computer's hand
				and the humanCard dropped from the human's hand`
Local Variables:
			newCHand- computer's hand after card played by comptuer was dropped
			newHHand- human's hand after the card played by human was dropped
Algorithm:
			1)set newChand to be computer's hand after card played by comptuer was dropped
			2)set newHhand to be human's hand after card played by human was dropped
			3)create a new list with the updated hands
Assistance Received: none.
********************************************************************* ||#
(defun updateHands (computerCard humanCard gameList)
	(let* ((newChand (dropCard (getCHand gameList) computerCard))
			(newHHand (dropCard (getHHand gameList) humanCard))
			(updatedList (list
						(getRoundNum gameList)
						(getCGameScore gameList)
						(getCRoundScore gameList)
						newChand
						(getCCapture gameList)
						(getCMeld gameList)
						(getHGameScore gameList)
						(getHRoundScore gameList)
						newHHand
						(getHCapture gameList)
						(getHMeld gameList)
						(getTrump gameList)
						(getStock gameList)
						(getNextP gameList)
						)
			))
			updatedList
	)
)

#|| *********************************************************************
Function Name: leadStrategy
Purpose: This function determines the best card to play as a lead player
Parameters:
            gameList- the list of the game
			hand- the hand of the player who is using the strategy
Return Value: a the best card to play from hand following the lead strategy
Local Variables:
			highestNonTrump- will hold a card: the highest card in hand that is not of trump suit
			lowestTrump- will hold a card: the lowest trump card in hand
Algorithm:
			1)if find the highest non trump in hand
			2)if exists:
				-return highest non trump
			3)if doesn't exist:
				-return lowest trump in hand
Assistance Received: none.
********************************************************************* ||#
(defun leadStrategy (gameList hand)
	(let ((highestNonTrump (getHighestNonT hand (getTrump gameList) nil))
		  (lowestTrump (getLowestTrump hand (getTrump gameList) nil)))
		(cond
			((not(eq highestNonTrump nil))
				(princ highestNonTrump) (terpri)
				(princ "	Strategy- playing the highest non trump card from hand") (terpri)
				highestNonTrump
			)
			( t
				(princ lowestTrump) (terpri)
				(princ "	Strategy- no non-trump cards in hand, playing lowest trump in hand") (terpri)
				lowestTrump
			)
		)
	)

)

#|| *********************************************************************
Function Name: chaseStrategy
Purpose: This function determines the best card to play as a chase player
Parameters:
            gameList- the list of the game
			hand- the hand of the player who is using the strategy
			lead- the lead card played by the first player
Return Value: a the best card to play from hand following the chase strategy
Local Variables:
			lowestLeadType- a card that will contain the lowest lead suited card that is higher
						than lead type
			lowestNonTrump-lowest card in hand that is not of trump suit
			lowestTrump-lowest trump card in hand that
Algorithm:
			1)if lead is trump suit	
				-find a higher trump in hand and return
				-if not found- find lowest typed card that is also not trrump suit and return
						(can't win- play least aluable card)
			2)else
				-play lowest lead suit from hand that is also higher than lead
				-if not found- play lowest trump in hand
				if not found- play lowest non trump from hand (can't win round, play least
					valuable card)
Assistance Received: none.
********************************************************************* ||#
(defun chaseStrategy (gameList lead hand)
	(cond
		((eq (getSuit lead) (getSuit (getTrump gameList)))
			(let ((lowestLeadType (getLowestLeadGTLead hand nil lead))
				  (lowestNonTrump (getLowestNonTrump hand (getTrump gameList) nil))
				  (lowestTrump (getLowestTrump hand (getTrump gameList) nil)))
								  
				(cond
					((not (eq lowestLeadType nil))
						(princ lowestLeadType) (terpri)
						(princ "	Strategy- playing the lowest trump possible to win the turn") (terpri)
						lowestLeadType
					)
					((not (eq lowestNonTrump nil))
						(princ lowestNonTrump) (terpri)
						(princ "	Strategy- can't win the turn, playing the least valuable non trump") (terpri)
						lowestNonTrump
					)
					( t
						(princ lowestTrump) (terpri)
						(princ "	Strategy- can't win the turn, but don't have any non trumps,") (terpri)
						(princ "	playing the least valuable trump") (terpri)
						lowestTrump
					)
				)
			)
		)
		;this means lead is not trump:
		( t
			(let ((lowestLeadType (getLowestLeadGTLead hand nil lead))
				  (lowestNonTrump (getLowestNonTrump hand (getTrump gameList) nil))
				  (lowestTrump (getLowestTrump hand (getTrump gameList) nil)))
								
				(cond
					((not (eq lowestLeadType nil))
						(princ lowestLeadType) (terpri)
						(princ "	Strategy- playing the lowest lead type possible to win the turn") (terpri)
						lowestLeadType
					)
					((not (eq lowestTrump nil))
						(princ lowestTrump) (terpri)
						(princ "	Strategy- don't have any non trumps that can win the turn,") (terpri)
						(princ "	playing lowest trump in hand to win the turn") (terpri)
						lowestTrump
					)
					( t
						(princ lowestNonTrump) (terpri)
						(princ "	Strategy- can't win the turn, playing least valuable non trump card") (terpri)
						lowestNonTrump
					)
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: computerPlay
Purpose: This function is responsible for the computer playing a card on its turn
Parameters:
            gameList- the list of the game
			lead- the lead card played by the first player (nil if computer is lead)
Return Value: the best card from the computer's hand using the computer strategy functions.
Local Variables:
			choice- a number representing the user's choice (for the menu before each player's turn)
Algorithm:
			1)display menu and read& validate user's input
			2)if user chose 1
				-readig a file name and saving the game to a file
			3)if user chose 2
				-computer is performing its turn
			4)if user chose 3
				-exiting the program
					
Assistance Received: none.
********************************************************************* ||#
(defun computerPlay (gameList lead)
	(princ "1. Save the game")(terpri)
	(princ "2. Make a move")(terpri)
	(princ "3. Quit the game")(terpri)
	(let ((choice (validateCMenu)))
		(cond 
			((eq choice 1)
				(princ "please enter a file name: ")
				(saveToFile (read) gameList)
				(princ "file saved!") (terpri)
				(exit)
			)
			((eq choice 2)
				(princ "Computer please play a card: ")
				(cond
					;Computer's strategy as a lead:
					((eq (getNextP gameList) 'computer)
						(let ((bestCard (leadStrategy gameList (getCHand gameList))))
							bestCard
						)
					)
					;Computer's strategy as a chase:
					( t 
						(let ((bestCard (chaseStrategy gameList lead (getCHand gameList))))
							bestCard
						)
					)
				)
			)
			( t
				(princ "Thank you for playing, exiting game")(terpri)
				(exit)
			)
		)
	)
)

#|| *********************************************************************
Function Name: huamnPlay
Purpose: This function is responsible for the human playing a card on its turn
Parameters:
            gameList- the list of the game
			lead- the lead card played by the first player (nil if human is lead)
Return Value: the user's card choice (in case user chose to play)
Local Variables:
			choice- a number representing the user's choice (for the menu before each player's turn)
Algorithm:
			1)display menu and read& validate user's input
			2)if user chose 1
				-readig a file name and saving the game to a file
			3)if user chose 2
				-prompting user to play the turn
			4)if user chose 3
				-exiting the program
			5)if user chose 4
				-calling computer's appropriate strategy function
				-prompting user to play the turn
					
Assistance Received: none.
********************************************************************* ||#
(defun humanPlay (gameList lead)

	(princ "1. Save the game")(terpri)
	(princ "2. Make a move")(terpri)
	(princ "3. Quit the game")(terpri)
	(princ "4. Ask for help")(terpri)
	(let ((choice (validateHMenu)))
		(cond
			((eq choice 1)
				(princ "please enter a file name: ")
				(saveToFile (read) gameList)
				(princ "file saved!") (terpri)
				(exit)
			)
			((eq choice 2)
				;This is play a card option, no need to do anything here, if uses chooses that option
				;it will play a card after cond.
			)
			((eq choice 3)
				(princ "Thank you for playing, exiting game")(terpri)
				(exit)
			)
			;This is the ask for help option (no validation needed here, already done)
			( t
				(cond
					;Computer's strategy as a lead:
					((eq (getNextP gameList) 'human)
						(princ "Computer recommends: ")
						(let ((bestCard (leadStrategy gameList (getHHand gameList))))
						)
					)
					;Computer's strategy as a chase:
					( t 
						(princ "Computer recommends: ")
						(let ((bestCard (chaseStrategy gameList lead (getHHand gameList))))
						)
					)
				)
			)
		)
		(princ "Human please play a card: ")
		;(princ (getHHand gameList)) (terpri)
		(let ((humanCard (validateCard (read) (getHHand gameList))))
			humanCard
		)
	)
)

#|| *********************************************************************
Function Name: validateCard
Purpose: This function is responsible validating the card the user's inputed
			exists in the human's hand
Parameters:
            card1-the card that the user inputted
			playerHand- the hand of the player (where the card should exist)
Return Value: the user's card if it exists in the user's hand
Local Variables:
			none
Algorithm:
			1)if card1 exists in hand: return card1
			2)else- call prompt user for a valid card and call validateCard with the new card (recursively)
Assistance Received: none.
********************************************************************* ||#
(defun validateCard (card1 playerHand)
	(cond
		( (eq (isCardValid playerHand card1) t)
			card1
		)
		( t
			(princ "card is not valid, please try again: ")
			(validateCard (read) playerHand)
		)
	)
)

#|| *********************************************************************
Function Name: isCardValid
Purpose: This function is responsible going over the player's hand recursively and determine if a
			specific card exists in the player's hand
Parameters:
            list1-a list conaining the cards from the player's hand
			card1-the card that needs to be validated
Return Value: true if the card exists in hand, false otherwise
Local Variables:
			none
Algorithm:
			1)if the first element if list1 = card1
				-return true
			2)else if list1 is empty
				-return false
			3)else
				-call isCardValid with the list minus the first card in the list (rest of the list)
Assistance Received: none.
********************************************************************* ||#
(defun isCardValid (list1 card1)
	(cond
		( (eq (first list1) card1)
			t
		)
		( (eq list1 nil)
			nil
		)
		( t 
			(isCardValid (rest list1) card1)
		)
	)
)

#|| *********************************************************************
Function Name: winner
Purpose: This function is responsible for determining the winner of the turn
		(lead/chase player)
Parameters:
            lead- a card holding the lead card of the turn
			chase- a card holding the chase	card of the turn
			trump- a card holding the trump card of the current round
Return Value: true if the card exists in hand, false otherwise
Local Variables:
			none
Algorithm:
			1)if lead is if trumps suit
				-if chase is of trump suit
					-if chase type index > lead type index
						return chase
					-else
						-return lead
				else
					-return lead
			2)else (lead is not of trump suit)
				-if lead and chase are of the same type
					-if chase type index > lead type index
						-return chase
					-else
						-return lead
				-if chase is of trump suit
					-return chase
				-else
					-return lead
Assistance Received: none.
********************************************************************* ||#
(defun winner (lead chase trump)
	(cond
		;if lead suit = trump suit
		( (eq (getSuit lead) (getSuit trump))
			(cond
				;if chase suit = trump suit
				( (eq (getSuit chase) (getSuit trump))
					(cond 
						;if chase's index is greater than lead's index
						( (> (getCardIndex chase) (getCardIndex lead))
							'chase)	;then chase wins
						;otherwise lead wins
						(t
							'lead
						)
					)
				)
				;if lead is trump but chase is not- lead wins
				( t
					'lead
				)
				
			)
		)
		;else (which means lead suit is not trump suit
		( t
			(cond
				;if chase suit = lead suit
				( (eq (getSuit lead) (getSuit chase))
					(cond 
						;if chase's index is greater than lead's index
						( (> (getCardIndex chase) (getCardIndex lead))
							'chase 				;then chase wins
						)
						;otherwise lead wins
						(t
							'lead
						)
					)
				)
				( t
					(cond 
						( (eq (getSuit chase) (getSuit trump)) ;if chase suit = trump suit
							'chase
						)
						( t
							'lead
						)
					)
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: saveToFile
Purpose: This function is responsible for saving the current game (game list) into a file
Parameters:
        fileName-the name of the file to save the game into
		gameList- the list of the game in which we are saving
Return Value: none
Local Variables:
			stream- the stream to write into the file
Algorithm:
			1)create a file with name fileName
			2)write all the information from gameList into that file in the
				appropriate format
Assistance Received: none.
********************************************************************* ||#
(defun saveToFile (fileName gameList)
	(with-open-file (stream fileName
                           :direction :output
                           :if-exists :supersede
						   :if-does-not-exist :create
                           :if-does-not-exist :create )
	
		(format stream "(~%")
		(format stream "   ; Round:~%")
		(format stream "   ~D~%" (getRoundNum gameList))
		
		(format stream "~%")
		
		(format stream "   ; Computer Score~%")
		(format stream "   ~D ~D~%" (getCGameScore gameList) (getCRoundScore gameList))
		
		(format stream "   ; Computer Hand~%")
		(cond
			((eq (getCHand gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getCHand gameList))
			)
		)
		
		(format stream "   ; Computer Capture Pile~%")
		(cond
			((eq (getCCapture gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getCCapture gameList))
			)
		)
		
		(format stream "   ; Computer Melds~%")
		(cond
			((eq (getCMeld gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getCMeld gameList))
			)
		)
		
		(format stream "~%")
		
		(format stream "   ; Human Score~%")
		(format stream "   ~D ~D~%" (getHGameScore gameList) (getHRoundScore gameList))
		
		(format stream "   ; Human Hand~%")
		(cond
			((eq (getHHand gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getHHand gameList))
			)
		)
		
		(format stream "   ; Human Capture Pile~%")
		(cond
			((eq (getHCapture gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getHCapture gameList))
			)
		)
		
		(format stream "   ; Human Melds~%")
		(cond
			((eq (getHMeld gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getHMeld gameList))
			)
		)
		
		(format stream "~%")
		
		(format stream "   ; Trump Card~%")
		(format stream "   ~A~%" (getTrump gameList))
		
		(format stream "~%")
		
		(format stream "   ; Stock~%")
		(cond
			((eq (getStock gameList) nil)
				(format stream "   ()~%")
			)
			( t
				(format stream "   ~A~%" (getStock gameList))
			)
		)
		
		(format stream "~%~%")
		
		(format stream "   ; Next Player~%")
		(format stream "   ~A~%" (getNextP gameList))
		
		(format stream ")")
	)
)

#|| *********************************************************************
Function Name: readFromFile
Purpose: This function is responsible for reading a game from a file into a list
Parameters:
        fileName-the name of the file that contains the game info
Return Value: the game list containing the information from the file
Local Variables:
			in- the stram to read from a file
Algorithm:
			1)open the file with name fileName
			2)read the information into a list (loadedGameList)
Assistance Received: none.
********************************************************************* ||#
(defun readFromFile (filename)
	(let* ((in (open filename))
		   (loadedGameList (read in)))
		(close in)
		loadedGameList
	)
)

#|| *********************************************************************
Function Name: loadAGame
Purpose: This function is responsible for reading a game from a file into a list
Parameters:
        none
Return Value: non
Local Variables:
			fileName-the name of the file from which we will read the game
Algorithm:
			1)read a valid filename into fileName
			2)load the game list into loadedList
			3)load a game using loadedList
Assistance Received: none.
********************************************************************* ||#
(defun loadAGame ()
	(let* ((fileName (validateFileName))
		   (loadedList (readFromFile fileName)))
			(loadedGame loadedList)
	)
)

#|| *********************************************************************
Function Name: validateFileName
Purpose: This function is responsible for validating the file name frovided by the user
Parameters:
        none
Return Value: a valid file name
Local Variables:
			fileName-the name of the file from which we will read the game
			doestFileExist- a variable that will contain nil if the file doesn't exist
					and some variation of the file's name if the file does exist
Algorithm:
			1)read a valid filename into fileName
			2)use doesFileExist to contain whether the file exist or not
			3)if doesFileExist = nil
				-call validateFileName recursively to fet a different file name
			4)else
				-return fileName
Assistance Received: none.
********************************************************************* ||#
(defun validateFileName ()
	(princ "Please enter a file name") (terpri)
	(princ '-->)
	(let* ((fileName (read))
		(doesFileExist (probe-file fileName)))
		(cond
			((eq doesFileExist nil)
				(princ "This file could not be found.") (terpri)
				(validateFileName)
			)
			( t
				fileName
			)
		)	
	)
)

;-------------------------------MAIN---------------------------------

(princ "To start a new game press 1")
(terpri)
(princ "To load a game press 2")
(terpri)
(princ '-->)

(cond
	( (= (vali(read)) 1)
		(game)
	)
	( t
		(loadAGame)
	)
)



