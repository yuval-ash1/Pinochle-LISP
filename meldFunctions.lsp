
#|| *********************************************************************
Function Name: readMeldList
Purpose: This function is responsible reading the user's meld list
Parameters:
        l1- the list of cards the user is inputting
Return Value: a list containing the user's complete meld list
Local Variables:
			input- a card inputted by the user
Algorithm:
			1)input <-- user's input
			2)if input is not 'm'
				-call readMeldList with a "concatenation" of 's input and l1
			3)else
				-return l1
Assistance Received: none
********************************************************************* ||#
(defun readMeldList (l1)
	(let* ((input (read)))
		(cond 
			((not (eq input 'm))
				(readMeldList (cons input l1))
			)
			( t
				l1
			)
		)
	)
)

#|| *********************************************************************
Function Name: wouldYouMeld
Purpose: This function is responsible telling the player if it can perform a meld
Parameters:
        newRoundList- a list that contains all the round information
		meldNameCode- a number associated with the meld's name
		turnWinner- the name of the player that is making the meld (human/computer)
Return Value: a list containing the meld cards if a meld is possible, false otherwise 
Local Variables:
			meldList- a list of cards that holds the card's for the meld
Algorithm:
			1)call the meld function corresponding to meldNameCode
Assistance Received: none
********************************************************************* ||#
(defun wouldYouMeld (newRoundList meldNameCode turnWinner)
	(cond
		( (not(eq meldNameCode 0))
			;perform a meld
			(cond 
				((eq turnWinner 'computer)
					(let ((meldList (getCHand newRoundList)))
						;meld name code will hold the number associated with the meld
						;(princ "Please enter the number associated with the meld name: ")
						(cond
							( (eq meldNameCode 1)
								(tryFlush meldList (getTrump newRoundList) (getCMeld newRoundList))
							)
							( (eq meldNameCode 2)
								(tryFourA meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 3)
								(tryFourK meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 4)
								(tryFourQ meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 5)
								(tryRoyalMarriage meldList (getTrump newRoundList) (getCMeld newRoundList))
							)
							( (eq meldNameCode 6)
								(tryFourJ meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 7)
								(tryPinochle meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 8)
								(tryMarriage meldList (getCMeld newRoundList))
							)
							( (eq meldNameCode 9)
								(tryDix meldList (getTrump newRoundList) (getCMeld newRoundList))
							)
							( t
								nil
							)
						)
					)
				)
				( t
					(princ "Enter the meld cards, when you are done, enter 'm'") (terpri)
					(let ((meldList (readMeldList '())))
						;meld name code will hold the number associated with the meld
						;(princ "Please enter the number associated with the meld name: ")
						(cond
							( (eq meldNameCode 1)
								(tryFlush meldList (getTrump newRoundList) (getHMeld newRoundList))
							)
							( (eq meldNameCode 2)
								(tryFourA meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 3)
								(tryFourK meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 4)
								(tryFourQ meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 5)
								(tryRoyalMarriage meldList (getTrump newRoundList) (getHMeld newRoundList))
							)
							( (eq meldNameCode 6)
								(tryFourJ meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 7)
								(tryPinochle meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 8)
								(tryMarriage meldList (getHMeld newRoundList))
							)
							( (eq meldNameCode 9)
								(tryDix meldList (getTrump newRoundList) (getHMeld newRoundList))
							)
							( t
								nil
							)
						)
					)
				)
			)
		)
		( t
			;Do nothing here, player didn't want to meld
			;returning nil because no meld was performed
			nil
		)
	)
)

#|| *********************************************************************
Function Name: meldName
Purpose: This function is responsible for finding the best meld to perform
		using the computer's strategy.
Parameters:
        gameList- a list of lists and numbers that contain all the game infrmation
		turnWinner- the name of the player won the last turn and has the option of
			making a meld (human/computer)
Return Value: a number associated with the best meld the computer/human can perform
				(0 if no possible meld)
Local Variables:
			computerChoice- a number than will hold 1 if computer want to play a meld
						and 2 otherwise
			computerMeldNum-the number associated with the meld that the computer 
							wants to perform
			meldNum-the number associated with the meld that the computer 
							reccoments human to perform
Algorithm:
			1)if winner is computer
				-check if any meld is possible in order of points (high to low)
				-if yes- print out the meld name
			2) if winner is human
				- ask if human wants to play a meld
					-if yes:
						-ask if human wants help
							-if yes:
								-call computer function to return the best meld's number and print in 
										a nice format
								-read meld number from user
							else- read meld nuber from user
Assistance Received: none
********************************************************************* ||#
(defun meldName (gameList turnWinner)
	(cond 
		((eq turnWinner 'computer)
			(princ turnWinner)
			(princ ", if you want to play a meld press 1, otherwise press 2: ")
			(let ((computerChoice (computerPlayingMeld gameList)))
				(princ computerChoice) (terpri)
				(cond
					((eq computerChoice 1)
						(let ((computerMeldNum (computerMeldNumber (getCHand gameList) (getCMeld gameList) (getTrump gameList))))
							(princ "Please enter the number associated with the meld name: ") (terpri)
							(princ "1 - Flush Meld") (terpri)
							(princ "2 - Four Aces Meld") (terpri)
							(princ "3 - Four Kings Meld") (terpri)
							(princ "4 - Four Queens Meld") (terpri)
							(princ "5 - Royal Marriage Meld") (terpri)
							(princ "6 - Four Jacks Meld") (terpri)
							(princ "7 - Pinochle Meld") (terpri)
							(princ "8 - Marriage Meld") (terpri)
							(princ "9 - Dix Meld") (terpri)
							(princ computerMeldNum) (terpri)
							computerMeldNum
						)
					)
					( t
						0
					)
				)
			)
		)
		( t
			(princ turnWinner)
			
			(princ ", if you want to play a meld press 1, otherwise press 2: ")
			(cond
				((eq (vali(read)) 1)
					(princ "If you want help with the best meld press 1, otherwise press 2: ")
					(cond
						((eq (vali(read)) 1)
							(let ((meldNum (computerMeldNumber (getHHand gameList) (getHMeld gameList) (getTrump gameList))))
								(cond
									((eq meldNum 1)
										(princ "Computer recommends: Flush Meld") (terpri)
									)
									((eq meldNum 2)
										(princ "Computer recommends: Four Aces Meld") (terpri)
									)
									((eq meldNum 3)
										(princ "Computer recommends: Four Kings Meld") (terpri)
									)
									((eq meldNum 4)
										(princ "Computer recommends: Four Queens Meld") (terpri)
									)
									((eq meldNum 5)
										(princ "Computer recommends: Royal Marriage") (terpri)
									)
									((eq meldNum 6)
										(princ "Computer recommends: Four Jacks Meld") (terpri)
									)
									((eq meldNum 7)
										(princ "Computer recommends: Pinochle Meld") (terpri)
									)
									((eq meldNum 8)
										(princ "Computer recommends: Marriage Meld") (terpri)
									)
									((eq meldNum 9)
										(princ "Computer recommends: Dix Meld") (terpri)
									)
									( t 
										(princ "No possible melds at the moment") (terpri)
									)
								)
							
							)
						)
					)
					(princ "Please enter the number associated with the meld name: ") (terpri)
					(princ "1 - Flush Meld") (terpri)
					(princ "2 - Four Aces Meld") (terpri)
					(princ "3 - Four Kings Meld") (terpri)
					(princ "4 - Four Queens Meld") (terpri)
					(princ "5 - Royal Marriage Meld") (terpri)
					(princ "6 - Four Jacks Meld") (terpri)
					(princ "7 - Pinochle Meld") (terpri)
					(princ "8 - Marriage Meld") (terpri)
					(princ "9 - Dix Meld") (terpri)
					(validateMeldNumber(read))
				)
				( t
					0
				)
			)
		)
	)
)

#|| *********************************************************************
Function Name: computerPlayingMeld
Purpose: This function is responsible for checking whether any meld is possible
		or not with the computer's hand
Parameters:
        gameList- a list of lists and numbers that contain all the game infrmation
Return Value: a number representing whether or not the computer can perform a meld
			  (1 if it can, 2 if it can't)
Local Variables:
			none
Algorithm:
			1)for each meld function (by order of points):
				-if a meld function doesn't return an empty list
					-return 1
				
Assistance Received: none
********************************************************************* ||#
(defun computerPlayingMeld (gameList)

	;checking if any of these functions will be able to perform a meld with the computer's hand
	(cond 
		((not (eq (tryFlush (getCHand gameList) (getTrump gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryFourA (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryFourK (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryFourQ (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryRoyalMarriage (getCHand gameList) (getTrump gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryFourJ (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryPinochle (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryMarriage (getCHand gameList) (getCMeld gameList)) nil))
			1
		)
		((not (eq (tryDix (getCHand gameList) (getTrump gameList) (getCMeld gameList)) nil))
			1
		)
		( t
			2
		)
	)
)

#|| *********************************************************************
Function Name: computerMeldNumber
Purpose: This function is responsible for figuring out what is the best meld
		to perform with the player's hand
Parameters:
        hand- a list of cards representing the hand of the player that is trying to
				perform a meld
		meldList- a list of cards representing the cards for the user's meld
		trump- a string representing the trump card of the game
Return Value: a number associted with the best meld to perform
Local Variables:
			none
Algorithm:
			1)for each meld function (by order of points):
				-if a meld function doesn't return an empty list
					-return the number associated with the meld
Assistance Received: none
********************************************************************* ||#
(defun computerMeldNumber (hand meldList trump)
	(cond 
		((not (eq (tryFlush hand trump meldList) nil))
			1
		)
		((not (eq (tryFourA hand meldList) nil))
			2
		)
		((not (eq (tryFourK hand meldList) nil))
			3
		)
		((not (eq (tryFourQ hand meldList) nil))
			4
		)
		((not (eq (tryRoyalMarriage hand trump meldList) nil))
			5
		)
		((not (eq (tryFourJ hand meldList) nil))
			6
		)
		((not (eq (tryPinochle hand meldList) nil))
			7
		)
		((not (eq (tryMarriage hand meldList) nil))
			8
		)
		((not (eq (tryDix hand trump meldList) nil))
			9
		)
		;no need for default case in here because computer already verified it can perform a meld
	)
)


#|| *********************************************************************
Function Name: validateMeldNumber
Purpose: This function is responsible for validating the meld number (should
				be 1-9)
Parameters:
        num- the number to be validated
Return Value: a valid number (1-9)
Local Variables:
			none
Algorithm:
			1)if num is 1-9
				-return num
			2)else
				-prompt user for a valid number
				-call validateMeldNumber with the new number
Assistance Received: none
********************************************************************* ||#
(defun validateMeldNumber (num)
	(cond
		((eq num 1)
			1
		)
		((eq num 2)
			2
		)
		((eq num 3)
			3
		)
		((eq num 4)
			4
		)
		((eq num 5)
			5
		)
		((eq num 6)
			6
		)
		((eq num 7)
			7
		)
		((eq num 8)
			8
		)
		((eq num 9)
			9
		)
		( t 
			(princ "Input is not valid, please try again: ")
			(princ '-->)
			(validateMeldNumber (read))
		)
	)
)

#|| *********************************************************************
Function Name: pointsForMeld
Purpose: This function is responsible for evaluating the point for a performed meld
Parameters:
        meldList- a list of (string) cards, representing the cards used to perform
					the meld
		meldNameCode- a number associated with the name of the meld that was performed
Return Value: a nubmer representing the amount of point the player is getting for performing
				the meld
Local Variables:
			none
Algorithm:
			1)if meldList is not empty
				-identify the meld using meldNameCode and return the appropriate
						amount of points for that meld
			2)else-
				-return 0 (b/c no meld was performed)
Assistance Received: none
********************************************************************* ||#
(defun pointsForMeld (meldList meldNameCode)
	(cond
		((not (eq meldList nil))
			(cond
				((= meldNameCode 1)
					150
				)
				((= meldNameCode 2)
					100
				)
				((= meldNameCode 3)
					80
				)
				((= meldNameCode 4)
					60
				)
				((= meldNameCode 5)
					40
				)
				((= meldNameCode 6)
					40
				)
				((= meldNameCode 7)
					40
				)
				((= meldNameCode 8)
					20
				)
				((= meldNameCode 9)
					10
				)
				;No need for default condition in here because meldNameCode is validated
			)
		)
		( t
			;if meldList is nil- no meld has been performed- 0 points
			0
		)
	)
)

#|| *********************************************************************
Function Name: didPerform functions
Purpose: The purpose of these functions is to test whether or not the player
			has already performed that specific meld
Parameters:
        playerPreviousMelds- a list of lists, each inner list contains a list of cards,
					each representing a meld that player has performed. this is the previous
					melds list of the player
				
		trump- the trump card of the current round (only sent in the functions of a meld that
				depends on the trump card (ex. flush)
Return Value: true if the meld exists in the playerPreviousMelds list, false otherwise
Local Variables:
			currentCheck- a list of cards, each representing a meld list from playerPreviousMelds
Algorithm:
			1)playerPreviousMelds is empty
				-return false
			2)else
				-currentCheck <-- playerPreviousMelds[0]
				-check if the cards in the current list match the cards in
					the meld's list
					-if so- return true
					else- call the function recursively with playerPreviousMelds minus 
						  its first position
Assistance Received: none
********************************************************************* ||#
(defun didPerformFlush (trump playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond 
							((or (eq (getSuit trump) '#\H) (eq (getSuit trump) '#\h))
								(cond
									((and (eq (first currentCheck) 'ah)
										  (eq (first (rest currentCheck)) 'xh)
										  (eq (first (rest (rest currentCheck))) 'kh) 
										  (eq (first (rest (rest (rest currentCheck)))) 'qh)
										  (eq (first (rest (rest (rest (rest currentCheck))))) 'jh))
										
										t
									)
									( t
										(didPerformFlush trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\S) (eq (getSuit trump) '#\s))
								(cond
									((and (eq (first currentCheck) 'as)
										  (eq (first (rest currentCheck)) 'xs)
										  (eq (first (rest (rest currentCheck))) 'ks) 
										  (eq (first (rest (rest (rest currentCheck)))) 'qs)
										  (eq (first (rest (rest (rest (rest currentCheck))))) 'js))
									
										t
									)
									( t
										(didPerformFlush trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\C) (eq (getSuit trump) '#\c))
								(cond
									((and (eq (first currentCheck) 'ac)
										  (eq (first (rest currentCheck)) 'xc)
										  (eq (first (rest (rest currentCheck))) 'kc) 
										  (eq (first (rest (rest (rest currentCheck)))) 'qc)
										  (eq (first (rest (rest (rest (rest currentCheck))))) 'jc))
									
										t
									)
									( t
										(didPerformFlush trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\D) (eq (getSuit trump) '#\d))
								(cond
									((and (eq (first currentCheck) 'ad)
										  (eq (first (rest currentCheck)) 'xd)
										  (eq (first (rest (rest currentCheck))) 'kd) 
										  (eq (first (rest (rest (rest currentCheck)))) 'qd)
										  (eq (first (rest (rest (rest (rest currentCheck))))) 'jd))
									
										t
									)
									( t
										(didPerformFlush trump (rest playerPreviousMelds))
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

(defun didPerformFourA (playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond
							((and (eq (first currentCheck) 'ah)
								  (eq (first (rest currentCheck)) 'as)
								  (eq (first (rest (rest currentCheck))) 'ac) 
								  (eq (first (rest (rest (rest currentCheck)))) 'ad))
									
									t
							)
							( t
								(didPerformFourA (rest playerPreviousMelds))
							)
						)
					)	
				)
			)
		)
	)
)



(defun didPerformFourK (playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond
							((and (eq (first currentCheck) 'kh)
								  (eq (first (rest currentCheck)) 'ks)
								  (eq (first (rest (rest currentCheck))) 'kc) 
								  (eq (first (rest (rest (rest currentCheck)))) 'kd))
									
									t
							)
							( t
								(didPerformFourK (rest playerPreviousMelds))
							)
						)
					)	
				)
			)
		)
	)
)
(defun didPerformFourQ (playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t	
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond
							((and (eq (first currentCheck) 'qh)
								  (eq (first (rest currentCheck)) 'qs)
								  (eq (first (rest (rest currentCheck))) 'qc) 
								  (eq (first (rest (rest (rest currentCheck)))) 'qd))
									
									t
							)
							( t
								(didPerformFourQ (rest playerPreviousMelds))
							)
						)
					)	
				)
			)
		)
	)
)
(defun didPerformRoyalM (trump playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t	
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond 
							((or (eq (getSuit trump) '#\H) (eq (getSuit trump) '#\h))
								(cond
									((and (eq (first currentCheck) 'kh)
										  (eq (first (rest currentCheck)) 'qh))
										
										t
									)
									( t
										(didPerformRoyalM trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\S) (eq (getSuit trump) '#\s))
								(cond
									((and (eq (first currentCheck) 'ks)
										  (eq (first (rest currentCheck)) 'qs))
									
										t
									)
									( t
										(didPerformRoyalM trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\C) (eq (getSuit trump) '#\c))
								(cond
									((and (eq (first currentCheck) 'kc)
										  (eq (first (rest currentCheck)) 'qc))
									
										t
									)
									( t
										(didPerformRoyalM trump (rest playerPreviousMelds))
									)
								)
							)
							( t
								(cond
									((and (eq (first currentCheck) 'kd)
										  (eq (first (rest currentCheck)) 'qd))
									
										t
									)
									( t
										(didPerformRoyalM trump (rest playerPreviousMelds))
									)
								)
							)
						)
					)
				)
			)
		)
	)
)
(defun didPerformFourJ (playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond
							((and (eq (first currentCheck) 'jh)
								  (eq (first (rest currentCheck)) 'js)
								  (eq (first (rest (rest currentCheck))) 'jc) 
								  (eq (first (rest (rest (rest currentCheck)))) 'jd))
									
									t
							)
							( t
								(didPerformFourJ (rest playerPreviousMelds))
							)
						)
					)	
				)
			)
		)
	)
)
(defun didPerformPinochle (playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond
							((and (eq (first currentCheck) 'qs)
								  (eq (first (rest currentCheck)) 'jd))
									
									t
							)
							( t
								(didPerformPinochle (rest playerPreviousMelds))
							)
						)
					)	
				)
			)
		)
	)
)
(defun didPerformMarriage (playerPreviousMelds type)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t	
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond 
							((or (eq type 'H) (eq type 'h))
								(cond
									((and (eq (first currentCheck) 'kh)
										  (eq (first (rest currentCheck)) 'qh))
										
										t
									)
									( t
										(didPerformMarriage (rest playerPreviousMelds) type)
									)
								)
							)
							((or (eq type 'S) (eq type 's))
								(cond
									((and (eq (first currentCheck) 'ks)
										  (eq (first (rest currentCheck)) 'qs))
									
										t
									)
									( t
										(didPerformMarriage (rest playerPreviousMelds) type)
									)
								)
							)
							((or (eq type 'C) (eq type 'c))
								(cond
									((and (eq (first currentCheck) 'kc)
										  (eq (first (rest currentCheck)) 'qc))
									
										t
									)
									( t
										(didPerformMarriage (rest playerPreviousMelds) type)
									)
								)
							)
							( t
								(cond
									((and (eq (first currentCheck) 'kd)
										  (eq (first (rest currentCheck)) 'qd))
									
										t
									)
									( t
										(didPerformMarriage (rest playerPreviousMelds) type)
									)
								)
							)
						)
					)
				)
			)
		)
	)
)
(defun didPerformDix (trump playerPreviousMelds)
	(cond
		((eq playerPreviousMelds nil)
			nil
		)
		( t
			(let ((currentCheck (first playerPreviousMelds)))
				(cond
					((eq currentCheck nil)
						nil
					)
					( t
						(cond 
							((or (eq (getSuit trump) '#\H) (eq (getSuit trump) '#\h))
								(cond
									((eq (first currentCheck) '9h)
										t
									)
									( t
										(didPerformDix trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\S) (eq (getSuit trump) '#\s))
								(cond
									((eq (first currentCheck) '9s)
										t
									)
									( t
										(didPerformDix trump (rest playerPreviousMelds))
									)
								)
							)
							((or (eq (getSuit trump) '#\C) (eq (getSuit trump) '#\c))
								(cond
									((eq (first currentCheck) '9c)
									
										t
									)
									( t
										(didPerformDix trump (rest playerPreviousMelds))
									)
								)
							)
							( t
								(cond
									((eq (first currentCheck) '9d)
										t
									)
									( t
										(didPerformDix trump (rest playerPreviousMelds))
									)
								)
							)
						)
					)
				)
			)
		)
	)
)






#|| *********************************************************************
Function Name: try functions
Purpose: The purpose of these functions is to determine whether a meld is possible
			with a user provided meld list (list of cards)
Parameters:
       meldList- a list of string cards, holding the player's cards in which they are
				trying to perform the meld
	   trump- the trump card of the current round (only sent in the functions of a meld that
				depends on the trump card (ex. flush)
	   previousMelds- a list of lists, each inner list contains a list of cards,
					  each representing a meld that player has performed. This is the previous
					  melds list of the player 
Return Value: the meld list if the meld is doable with the meldList, an empty list otherwise
Local Variables:
			cards- in each function, there are card variables, each representing a card
				   neccessary for the meld (they will contain true if card exists in
				   meldList, flase if not)
Algorithm:
			1)create a vriable for each card and call the function associated with the
				card to determine if card exists in meldList
			2)if all neccessary card evaluated to true
				-return the meld list
			3)else
				-return an empty list
Assistance Received: none
********************************************************************* ||#
;-------------------------------------------------------------------------
; THESE ARE FUNCTIONS TO VERIFY IF A MELD IS POSSIBLE WITH THE GIVEN CARDS
;-------------------------------------------------------------------------
(defun tryFlush (meldList trump previousMelds)
	
	(let ((aceT (aceOfTrump meldList trump))
		  (tenT (tenOfTrump meldList trump))
		  (kingT (kingOfTrump meldList trump))
		  (queenT (queenOfTrump meldList trump))
		  (jackT (jackOfTrump meldList trump))
		  (wasPerformed (didPerformFlush trump previousMelds))
		  )
		(cond 
			((and (eq aceT t) (eq tenT t) (eq kingT t) (eq queenT t) (eq jackT t) (eq wasPerformed nil))
				(cond 
					((eq (getSuit trump) '#\H)
						(list 'ah 'xh 'kh 'qh 'jh)
					)
					((eq (getSuit trump) '#\S)
						(list 'as 'xs 'ks 'qs 'js)
					)
					((eq (getSuit trump) '#\C)
						(list 'ac 'xc 'kc 'qc 'jc)
					)
					( t 	; if suit is not any of the above- it has to be d- diamonds
						(list 'ad 'xd 'kd 'qd 'jd)
					)
				)
			)
			( t			;if not all the variables are true- this means player did not provide all necessary
						;cards for this meld
				nil
			)
		)
	)
)


(defun tryFourA (meldList previousMelds)
	(let ((aceH (aceOfHearts meldList))
		  (aceS (aceOfSpades meldList))
		  (aceC (aceOfClubs meldList))
		  (aceD (aceOfDiamonds meldList))
		  (wasPerformed (didPerformFourA previousMelds)))
	
		(cond 
			((and (eq aceH t) (eq aceS t) (eq aceC t) (eq aceD t) (eq wasPerformed nil))
				(list 'ah 'as 'ac 'ad)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryFourK (meldList previousMelds)
	
	(let ((kingH (kingOfHearts meldList))
		  (kingS (kingOfSpades meldList))
		  (kingC (kingOfClubs meldList))
		  (kingD (kingOfDiamonds meldList))
		  (wasPerformed (didPerformFourK previousMelds)))
		
		(cond 
			((and (eq kingH t) (eq kingS t) (eq kingC t) (eq kingD t) (eq wasPerformed nil))
				(list 'kh 'ks 'kc 'kd)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryFourQ (meldList previousMelds)
	
	(let ((queenH (queenOfHearts meldList))
		  (queenS (queenOfSpades meldList))
		  (queenC (queenOfClubs meldList))
		  (queenD (queenOfDiamonds meldList))
		  (wasPerformed (didPerformFourQ previousMelds)))
		
		(cond 
			((and (eq queenH t) (eq queenS t) (eq queenC t) (eq queenD t) (eq wasPerformed nil))
				(list 'qh 'qs 'qc 'qd)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryRoyalMarriage (meldList trump previousMelds)
	
	(let ((kingT (kingOfTrump meldList trump))
		  (queenT (queenOfTrump meldList trump))
		  (wasPerformed (didPerformRoyalM trump previousMelds)))
		
		(cond 
			((and (eq kingT t) (eq queenT t) (eq wasPerformed nil))
				(cond 
					((eq (getSuit trump) '#\H)
						(list 'kh 'qh)
					)
					((eq (getSuit trump) '#\S)
						(list 'ks 'qs)
					)
					((eq (getSuit trump) '#\C)
						(list 'kc 'qc)
					)
					( t 	; if suit is not any of the above- it has to be d- diamonds
						(list 'kd 'qd)
					)
				)
			)
			( t			;if not all the variables are true- this means player did not provide all necessary
						;cards for this meld
				nil
			)
		)
	)
)

(defun tryFourJ (meldList previousMelds)
	
	(let ((jackH (jackOfHearts meldList))
		  (jackS (jackOfSpades meldList))
		  (jackC (jackOfClubs meldList))
		  (jackD (jackOfDiamonds meldList))
		  (wasPerformed (didPerformFourJ previousMelds)))
		
		(cond 
			((and (eq jackH t) (eq jackS t) (eq jackC t) (eq jackD t) (eq wasPerformed nil))
				(list 'jh 'js 'jc 'jd)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryPinochle (meldList previousMelds)
	
	(let ((queenS (queenOfSpades meldList))
		  (jackD (jackOfDiamonds meldList))
		  (wasPerformed (didPerformPinochle previousMelds)))
		
		(cond 
			((and (eq queenS t) (eq jackD t) (eq wasPerformed nil))
				(list 'qs 'jd)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryMarriage (meldList previousMelds)
	
	(let ((kingH (kingOfHearts meldList))
		  (queenH (queenOfHearts meldList))
		  (kingS (kingOfSpades meldList))
		  (queenS (queenOfSpades meldList))
		  (kingC (kingOfClubs meldList))
		  (queenC (queenOfClubs meldList))
		  (kingD (kingOfDiamonds meldList))
		  (queenD (queenOfDiamonds meldList)))
		
		(cond 
			((and (eq kingH t) (eq queenH t) (eq (didPerformMarriage previousMelds 'h) nil))
				(list 'kh 'qh)
			)
			((and (eq kingS t) (eq queenS t) (eq (didPerformMarriage previousMelds 's) nil))
				(list 'ks 'qs)
			)
			((and (eq kingC t) (eq queenC t) (eq (didPerformMarriage previousMelds 'c) nil))
				(list 'kc 'qc)
			)
			((and (eq kingD t) (eq queenD t) (eq (didPerformMarriage previousMelds 'd) nil))
				(list 'kd 'qd)
			)
			( t 
				nil
			)
		)	
	)
)

(defun tryDix (meldList trump previousMelds)
	(let ((nineT (nineOfTrump meldList trump))
		  (wasPerformed (didPerformDix trump previousMelds)))
		
		(cond 
			((and (eq nineT t) (eq wasPerformed nil))
				(cond 
					((eq (getSuit trump) '#\H)
						(list '9h)
					)
					((eq (getSuit trump) '#\S)
						(list '9s)
					)
					((eq (getSuit trump) '#\C)
						(list '9c)
					)
					((eq (getSuit trump) '#\D) 	; if suit is not any of the above- it has to be d- diamonds
						(list '9d)
					)
				)
			)
			( t			;if not all the variables are true- this means player did not provide all necessary
						;cards for this meld
				nil
			)
		)
	)
)





#|| *********************************************************************
Function Name: card functions
Purpose: The purpose of these functions is to determine whether specific card exists in a list
			(the meld list)
Parameters:
       list1- a list of string cards, holding the player's cards in which they are
				trying to perform the meld with
	   trump- the trump card of the current round (only sent in the functions of a card that
				is a trump card (ex. aceOfTrump)
Return Value: the meld list if the meld is doable with the meldList, an empty list otherwise
Local Variables:
			none
Algorithm:
			1)if list[0] = the card we are looking for
				-return true
			2)if the rest of the list (list minus list[0]) is empty
				-return false
			3)else
				-call the function again with the rest of the list (list minus its first element)
Assistance Received: none
********************************************************************* ||#
;------------------------------------------------------------------
; THESE ARE CARD IDENTIFICATION FUNCTIONS (FOR VERIFYING MELDS)
;------------------------------------------------------------------
(defun aceOfHearts (list1)
	(cond
		((eq (first list1) 'ah)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(aceOfHearts (rest list1))
		)
	)
)

(defun aceOfSpades (list1)
	(cond
		((eq (first list1) 'as)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(aceOfSpades (rest list1))
		)
	)
)

(defun aceOfClubs (list1)
	(cond
		((eq (first list1) 'ac)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(aceOfClubs (rest list1))
		)
	)
)

(defun aceOfDiamonds (list1)
	(cond
		((eq (first list1) 'ad)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(aceOfDiamonds (rest list1))
		)
	)
)



(defun kingOfHearts (list1)
	(cond
		((eq (first list1) 'kh)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(kingOfHearts (rest list1))
		)
	)
)

(defun kingOfSpades (list1)
	(cond
		((eq (first list1) 'ks)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(kingOfSpades (rest list1))
		)
	)
)

(defun kingOfClubs (list1)
	(cond
		((eq (first list1) 'kc)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(kingOfClubs (rest list1))
		)
	)
)

(defun kingOfDiamonds (list1)
	(cond
		((eq (first list1) 'kd)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(kingOfDiamonds (rest list1))
		)
	)
)

(defun queenOfHearts (list1)
	(cond
		((eq (first list1) 'qh)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(queenOfHearts (rest list1))
		)
	)
)

(defun queenOfSpades (list1)
	(cond
		((eq (first list1) 'qs)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(queenOfSpades (rest list1))
		)
	)
)

(defun queenOfClubs (list1)
	(cond
		((eq (first list1) 'qc)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(queenOfClubs (rest list1))
		)
	)
)

(defun queenOfDiamonds (list1)
	(cond
		((eq (first list1) 'qd)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(queenOfDiamonds (rest list1))
		)
	)
)

(defun jackOfHearts (list1)
	(cond
		((eq (first list1) 'jh)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(jackOfHearts (rest list1))
		)
	)
)

(defun jackOfSpades (list1)
	(cond
		((eq (first list1) 'js)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(jackOfSpades (rest list1))
		)
	)
)

(defun jackOfClubs (list1)
	(cond
		((eq (first list1) 'jc)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(jackOfClubs (rest list1))
		)
	)
)

(defun jackOfDiamonds (list1)
	(cond
		((eq (first list1) 'jd)
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(jackOfDiamonds (rest list1))
		)
	)
)

(defun nineOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\9)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(nineOfTrump (rest list1) trump)
		)
	)
)

(defun jackOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\J)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(jackOfTrump (rest list1) trump)
		)
	)
)
(defun queenOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\Q)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(queenOfTrump (rest list1) trump)
		)
	)
)
(defun kingOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\K)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(kingOfTrump (rest list1) trump)
		)
	)
)
(defun tenOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\X)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(tenOfTrump (rest list1) trump)
		)
	)
)
(defun aceOfTrump (list1 trump)
	(cond
		((and (eq (getType (first list1)) '#\A)
			  (eq (getSuit (first list1)) (getSuit trump)))
			t
		)
		((eq (rest list1) nil)
			nil
		)
		( t
			(aceOfTrump (rest list1) trump)
		)
	)
)
