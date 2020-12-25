
#|| *********************************************************************
Function Name: getHighestNonT
Purpose: This function is responsible for finding the highest non trump card 
			in the player's hand
Parameters:
       hand- a list of cards which represents the hand of the player
	   trump- a string card which holds the trump card of the round
	   bestCard- a variable to eventually hold the card we are looking for
Return Value: a card (bestCard) which holds the highest non trump in hand (or nil if
					there are no non turmps in hand)
Local Variables:
			none
Algorithm:
			1)if bestCard is nil
				-if hand is not nil
					-if hand[0]is non trump
						-bestCard <- hand[0]
						- call getHighestNonT with rest of hand
					-else
						- call getHighestNonT with rest of hand
				-else
					-return bestCard
			2)else
				-if hand is empty
					-return bestCard
				-else
					-if hand[0] is non trump and its index is greater than the current bestCard
						-bestCard <- hand[0]
						- call getHighestNonT with rest of hand
					-else
						- call getHighestNonT with rest of hand
Assistance Received: none
********************************************************************* ||#
(defun getHighestNonT (hand trump bestCard)
	(cond
		((eq bestCard nil)
			(cond
				((not (eq (first hand) nil))
					(cond
						((not (eq (getSuit (first hand)) (getSuit trump)))
							(let ((newBest (first hand)))
								(getHighestNonT (rest hand) trump newBest)
							)
						)
						( t
							(getHighestNonT (rest hand) trump bestCard)
						)
					)
				)
				( t
					bestCard
				)
			)
		)
		( t
			(cond
				((eq (first hand) nil)
					bestCard
				)
				( t
					(cond
						((and (not (eq (getSuit (first hand)) (getSuit trump))) (> (getCardIndex (first hand)) (getCardIndex bestCard)))
							(let ((newBest (first hand)))
								(getHighestNonT (rest hand) trump newBest)
							)
						)
						( t
							(getHighestNonT (rest hand) trump bestCard)
						)
					)
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: getLowestTrump
Purpose: This function is responsible for finding the lowest trump card 
			in the player's hand
Parameters:
       hand- a list of cards which represents the hand of the player
	   trump- a string card which holds the trump card of the round
	   lowestTrump- a variable to eventually hold the card we are looking for
Return Value: a card (bestCard) which holds the lowest trump in hand (or nil if
					there are no turmps in hand)
Local Variables:
			none
Algorithm:
			1)if hand is not nil
				-if lowestTrump is not defined (nil)
					-if hand[0] is of trump suit
						-lowestTrump <- hand[0]
						-call getLowestTrump with rest of hand (and the lowestTrump, trump)
					-else
						-call getLowestTrump with rest of hand
				-else
					-if hand[0] is of trump suit and its type if of lower index the current lowestTrump
						-lowestTrump <-- hand[0]
						-call getLowestTrump with rest of hand
					-else
						-call getLowestTrump with rest of hand
			2)else
				-return lowestTrump
Assistance Received: none
********************************************************************* ||#
(defun getLowestTrump (hand trump lowestTrump)
	(cond
		((not (eq (first hand) nil))
			(cond
				((eq lowestTrump nil)
					(cond
						((eq (getSuit (first hand)) (getSuit trump))
							(let ((newLowestTrump (first hand)))
								(getLowestTrump (rest hand) trump newLowestTrump)
							)
						)
						( t
							(getLowestTrump (rest hand) trump lowestTrump)
						)
					)
				)
				( t
					(cond
						((and (eq (getSuit(first hand)) (getSuit trump)) 
							  (< (getCardIndex (first hand)) (getCardIndex lowestTrump)))
							
							(let ((newLowestTrump (first hand)))
								(getLowestTrump (rest hand) trump newLowestTrump)
							)
						)
						( t
							(getLowestTrump (rest hand) trump lowestTrump)
						)
					)
				)
			)
		)
		(
			lowestTrump
		)
	)
)

#|| *********************************************************************
Function Name: getLowestNonTrump
Purpose: This function is responsible for finding the lowest non trump card 
			in the player's hand
Parameters:
       hand- a list of cards which represents the hand of the player
	   trump- a string card which holds the trump card of the round
	   lowestNonTrump- a variable to eventually hold the card we are looking for
Return Value: a card (bestCard) which holds the lowest non trump card in hand (or nil if
					there are no non turmps in hand)
Local Variables:
			none
Algorithm:
			1)if hand is not nil
				-if lowestNonTrump is not defined (nil)
					-if hand[0] is of non-trump suit
						-lowestNonTrump <- hand[0]
						-call getLowestTrump with rest of hand (and the lowestNonTrump, trump)
					-else
						-call getLowestTrump with rest of hand
				-else
					-if hand[0] is of non-trump suit and its type if of lower index the current lowestNonTrump
						-lowestNonTrump <-- hand[0]
						-call getLowestTrump with rest of hand
					-else
						-call getLowestTrump with rest of hand
			2)else
				-return lowestNonTrump
Assistance Received: none
********************************************************************* ||#
(defun getLowestNonTrump (hand trump lowestNonTrump)
	(cond
		((not (eq (first hand) nil))
			(cond
				((eq lowestNonTrump nil)
					(cond
						((not (eq (getSuit (first hand)) (getSuit trump)))
							(let ((newLowestNonTrump (first hand)))
								(getLowestNonTrump (rest hand) trump newLowestNonTrump)
							)
						)
						( t
							(getLowestNonTrump (rest hand) trump lowestNonTrump)
						)
					)
				)
				( t
					(cond
						((and (not (eq (getSuit(first hand)) (getSuit trump))) 
							  (< (getCardIndex (first hand)) (getCardIndex lowestNonTrump)))
							
							(let ((newLowestNonTrump (first hand)))
								(getLowestNonTrump (rest hand) trump newLowestNonTrump)
							)
						)
						( t
							(getLowestNonTrump (rest hand) trump lowestNonTrump)
						)
					)
				)
			)
		)
		(
			lowestNonTrump
		)
	)
)


;Get lowest lead-suit card that is greater than lead
#|| *********************************************************************
Function Name: getLowestLeadGTLead
Purpose: This function is responsible for finding the lowest lead-suit
			card that is greater than lead
Parameters:
       hand- a list of cards which represents the hand of the player
	   lowestType- a variable to eventually hold the card we are looking for
	   lead- a string card which holds the lead card of the turn
Return Value: a card (bestCard) which holds the lowest lead-suit card in handthat
					is also greater than lead (or nil if there are no non turmps in hand)
Local Variables:
			none
Algorithm:
			1)if hand is not nil
				-if lowestType is not defined (nil)
					-if hand[0] is of lead suit and its type is greater than lead type
						-lowestType <-- hand[0]
						-call getLowestLeadGTLead (recursivly)
					-else
						-call getLowestLeadGTLead (recursivly)
				-else
					-if hand[0] is of lead suit and its type is greater than lead type
							and it is smaller than current lowestType
						-lowestType <-- hand[0]
						-call getLowestLeadGTLead (recursivly)
					-else
						-call getLowestLeadGTLead (recursivly)
			2)else
				-return lowestType
Assistance Received: none
********************************************************************* ||#
(defun getLowestLeadGTLead (hand lowestType lead)
	(cond
		((not (eq (first hand) nil))
			(cond
				((eq lowestType nil)
					(cond
						((and (eq (getSuit (first hand)) (getSuit lead)) 
							 (> (getCardIndex (first hand)) (getCardIndex lead)) )
						
							(let ((newLowestTrump (first hand)))
								(getLowestLeadGTLead (rest hand) newLowestTrump lead)
							)
						)
						( t
							(getLowestLeadGTLead (rest hand) lowestType lead)
						)		
					)
				)
				( t
					(cond
						((and (eq (getSuit (first hand)) (getSuit lead))
							  (> (getCardIndex (first hand)) (getCardIndex lead))
							  (< (getCardIndex (first hand)) (getCardIndex lowestType)) )
							
							(let ((newLowestTrump (first hand)))
								(getLowestLeadGTLead (rest hand) newLowestTrump lead)
							)
						)
						( t
							(getLowestLeadGTLead (rest hand) lowestType lead)
						)
					)
				)
			)
		)
		( t
			lowestType
		)
	)
)