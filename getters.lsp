
#|| *********************************************************************
Function Name: getRoundNum
Purpose: This function is responsible returning the round number of the game
Parameters:
        gameList- a list containing all the information of the game
Return Value: a number representing the round number
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getRoundNum (gameList)
	(first gameList)
)

#|| *********************************************************************
Function Name: getCGameScore
Purpose: This function is responsible returning the computer's game score
Parameters:
        gameList- a list containing all the information of the game
Return Value: a number representing the computer's game score
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getCGameScore (gameList)
	(first (rest gameList))
)

#|| *********************************************************************
Function Name: getCRoundScore
Purpose: This function is responsible returning the computer's round score
Parameters:
        gameList- a list containing all the information of the game
Return Value: a number representing the computer's round score
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getCRoundScore (gameList)
	(first (rest (rest gameList)))
)

#|| *********************************************************************
Function Name: getCHand
Purpose: This function is responsible returning the computer's hand
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of cards representing the computer's current hand
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getCHand (gameList)
	(first (rest (rest (rest gameList))))
)

#|| *********************************************************************
Function Name: getCCapture
Purpose: This function is responsible returning the computer's capture pile
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of cards representing the computer's current capture pile
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getCCapture (gameList)
	(first (rest (rest (rest (rest gameList)))))
)

#|| *********************************************************************
Function Name: getCMeld
Purpose: This function is responsible returning the computer's previous melds pile
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of lists, each containing the cards of previous meld the computer performed
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getCMeld (gameList)
	(first (rest (rest (rest (rest (rest gameList))))))
)

#|| *********************************************************************
Function Name: getHGameScore
Purpose: This function is responsible returning the human's game score
Parameters:
        gameList- a list containing all the information of the game
Return Value: a number representing the human's game score
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getHGameScore (gameList)
	(first (rest (rest (rest (rest (rest (rest gameList)))))))
)

#|| *********************************************************************
Function Name: getHRoundScore
Purpose: This function is responsible returning the human's round score
Parameters:
        gameList- a list containing all the information of the game
Return Value: a number representing the human's round score
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getHRoundScore (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest gameList))))))))
)

#|| *********************************************************************
Function Name: getHHand
Purpose: This function is responsible returning the human's hand
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of cards representing the human's current hand
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getHHand (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))
)

#|| *********************************************************************
Function Name: getHCapture
Purpose: This function is responsible returning the human's capture pile
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of cards representing the human's current capture pile
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getHCapture (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList))))))))))
)

#|| *********************************************************************
Function Name: getHMeld
Purpose: This function is responsible returning the human's previous melds pile
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of lists, each containing the cards of previous melds that the human
			player performed
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getHMeld (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))))
)

#|| *********************************************************************
Function Name: getTrump
Purpose: This function is responsible returning the trump card of the current round
Parameters:
        gameList- a list containing all the information of the game
Return Value: a card that represents the trump card of the current round
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getTrump (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList))))))))))))
)

#|| *********************************************************************
Function Name: getStock
Purpose: This function is responsible for returning the stock of the current round
Parameters:
        gameList- a list containing all the information of the game
Return Value: a list of cards representing the stock of the current round
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getStock (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))))))
)

#|| *********************************************************************
Function Name: getNextP
Purpose: This function is responsible for returning next player (the player
		 to play first on the current turn)
Parameters:
        gameList- a list containing all the information of the game
Return Value: the name of the next player (human/computer)
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getNextP (gameList)
	(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList))))))))))))))
)

#|| *********************************************************************
Function Name: getType
Purpose: This function is responsible for returning the type of the card
Parameters:
        card- a string that represents a card
Return Value: a character that represents the type of the card
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getType (card)
	(char (string card) 0)
)

#|| *********************************************************************
Function Name: getSuit
Purpose: This function is responsible for returning the suit of the card
Parameters:
        card- a string that represents a card
Return Value: a character that represents the suit of the card
Local Variables:
			none
Algorithm:

Assistance Received: none.
********************************************************************* ||#
(defun getSuit (card)
	(cond
		((eq (length (string card)) 2)
				(char (string card) 1)
		)
		( t
			(char (string card) 0)
		)
	)
)

#|| *********************************************************************
Function Name: getCardIndex
Purpose: This function is responsible for returning the type index of the card
Parameters:
        card1- a string that represents a card
Return Value: a number that represents the type index of the card
Local Variables:
			none
Algorithm:
			1)type = the type of card1-
			2)if type = '9'
				return 0
			3)if type = 'j'
				return 1
			4)if type = 'q'
				return 2
			5)if type = 'k'
				return 3
			6)if type = 'x'
				return 4
			7)if type = 'a'
				return 5
Assistance Received: none.
********************************************************************* ||#
(defun getCardIndex (card1)
	(let ((type (getType card1)))
		(cond
			( (eq type '#\9 )
				0)
			( (or (eq type '#\j) (eq type '#\J))
				1)
			( (or (eq type '#\q) (eq type '#\Q))
				2)
			( (or (eq type '#\k) (eq type '#\K))
				3)
			( (or (eq type '#\x) (eq type '#\X))
				4)
			( (or (eq type '#\a) (eq type '#\A))
				5)	
		)
	)
)