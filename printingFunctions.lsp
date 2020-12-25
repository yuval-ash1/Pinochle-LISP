
#|| *********************************************************************
Function Name: printBeforeTurn
Purpose: This function is responsible printing all the round information nicely before
		the beginning of each turn
Parameters:
       gameList- a list containing all the information of the game
Return Value: none
Local Variables:
			none
Algorithm:
			1)go over gameList and print:
				-round number
				-trump card
				-deck
				-computer's hand
				-computer's capture pile
				-computer's previous melds
				-human's hand
				-human's capture pile
				-human's previous melds
			in a nice, readable format
Assistance Received: none
********************************************************************* ||#
(defun printBeforeTurn (gameList)
	(terpri)
	(princ '*----------------------------------------------------) (terpri)
	(princ "				Round number: ")
	(princ (getRoundNum gameList)) (terpri)
	(princ "Trump Card: ")
	(princ (getTrump gameList)) (terpri)
	(princ "Deck: ")
	(princ (getStock gameList)) (terpri) (terpri)
	
	(princ "Computer Hand: ")
	(princ (getCHand gameList)) (terpri)
	(princ "Computer Capture: ")
	(princ (getCCapture gameList)) (terpri)
	(princ "Computer Melds: ")
	(princ (getCMeld gameList)) (terpri)
	(princ "Computer Score: ")
	(princ (getCRoundScore gameList)) (terpri) (terpri)
	
	(princ "Human Hand: ")
	(princ (getHHand gameList)) (terpri)
	(princ "Human Capture: ")
	(princ (getHCapture gameList)) (terpri)
	(princ "Human Melds: ")
	(princ (getHMeld gameList)) (terpri)
	(princ "Human Score: ")
	(princ (getHRoundScore gameList)) (terpri)
	(princ '----------------------------------------------------*) (terpri)
)