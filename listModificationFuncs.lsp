(load 'getters.lsp)

#|| *********************************************************************
Function Name: dropCard
Purpose: This function is responsible for droping a card from a player's hand
Parameters:
        list1- a list of cards representing the player's hand
		card1- a string representing a card from the player's hand
Return Value: a list of card, the player's hand, without card1
Local Variables:
			none
Algorithm:
			1)if list1 is nil
				-return an empty list
			2)else if the first elemnt of the list = card
				-return the list without the first element
			3)else
				-combine the first element of list1 with a what is returned
					from recursive call to dropCard with list1 minus its 
					first element and card1
Assistance Received:
	-Professor Amruth Kumar- demonstrated this function in class
********************************************************************* ||#
(defun dropCard (list1 card1)
	(cond
		( (eq list1 nil)
			() )
		( (eq (first list1) card1)
			(rest list1)
		)
		( t
			(cons
				(first list1)
				(dropCard (rest list1) card1)
			)
		)
	)
)

#|| *********************************************************************
Function Name: setNextP
Purpose: This function is responsible for updating the next player in the gameList
Parameters:
        gameList- a list containing al the information of the game
		nextPlayer- a string containing the next player (which is also the winner of the current round)
Return Value: a list containing the game info with the updated next player
Local Variables:
			none
Algorithm:
			
Assistance Received: none
********************************************************************* ||#
(defun setNextP (gameList nextPlayer)
	(list 
		(first gameList)
		(first (rest gameList))
		(first (rest (rest gameList)))
		(first (rest (rest (rest gameList))))
		(first (rest (rest (rest (rest gameList)))))
		(first (rest (rest (rest (rest (rest gameList))))))
		(first (rest (rest (rest (rest (rest (rest gameList)))))))
		(first (rest (rest (rest (rest (rest (rest (rest gameList))))))))
		(first (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList))))))))))
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))))
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList))))))))))))
		(first (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest (rest gameList)))))))))))))
		nextPlayer
	)
)

#|| *********************************************************************
Function Name: setListForNewRound
Purpose: This function is responsible for setting all the necessary information 
	 	 from the previous round into the new game list
Parameters:
        prevRList- the list from the last round played
		newRlist-a new list conaining the new shuffled and player's hands
Return Value: a list containing the game info with the updated next player
Local Variables:
			none
Algorithm:
			
Assistance Received: none
********************************************************************* ||#
(defun setListForNewRound (prevRList newRlist)
	(list
		(+ (getRoundNum prevRList) 1)
		(+ (getCGameScore prevRList) (getCRoundScore prevRList))
		(getCRoundScore newRlist)
		(getCHand newRlist)
		(getCCapture newRlist)
		(getCMeld newRlist)
		
		(+ (getHGameScore prevRList) (getHRoundScore prevRList))
		(getHRoundScore newRlist)
		(getHHand newRlist)
		(getHCapture newRlist)
		(getHMeld newRlist)
		
		(getTrump newRlist)
		(getStock newRlist)
		(getNextP newRlist)
	)
)
