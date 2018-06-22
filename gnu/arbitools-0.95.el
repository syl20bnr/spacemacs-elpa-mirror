;;; arbitools.el --- Package for chess tournaments administration

;; Copyright 2016 Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 0.95
;; Package-Requires: ((cl-lib "0.5"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; REQUIRES:
;; ---------------------------
;; Some functions require the arbitools python package, written by myself
;; you can install it by: "pip3 install arbitools"
;;
;; "pdflatex" by Han The Thanh is necessary in case you want to get pdfs.
;;            It is distributed under a GPL license.
;;            https://www.tug.org/applications/pdftex/
;;
;; "bbpPairings.exe" by Bierema Boyz Programming is necessary to do the 
;;                   pairings. Copy the file to an executable folder, 
;;                   for example /usr/bin.
;;                   Find bbpPairings in
;;                   https://github.com/BieremaBoyzProgramming/bbpPairings
;;                   under GPL license.
;;
;; USAGE:
;; ---------------------------
;; arbitools.el is an interface for the python package "arbitools",
;; designed to manage chess tournament reports.  If you don't install the
;; python package you can still have the syntax colouring and some native
;; functions. In the future, all the functions will be translated to ELISP.
;;
;; FEATURES:
;; ----------------------------
;; - Syntax colouring for the official trf FIDE files.  This facilitates
;; manual edition of the files.
;;
;; - Updating the players ratings. - with python
;; 
;; - Adding players to an existing file. - with python
;;
;; - Getting standings from a tournament file. -with python
;; 
;; - Getting IT3 Tournament report form. - with python
;;
;; - Deleting a round. - Native
;;
;; - Insert result. - Native
;;
;; - Insert player. - Native
;;
;; - Insert bye. - Native
;;
;; - Get the pairing list or results of a round - Native
;;
;; - Get the list of the players - Native
;;
;; - Delete player. Adjust all rank numbers - Native
;;
;; - Adjust points for each player, according to results of rounds - Native
;;
;; - Print standings - Native
;;
;; - Do pairings - with bbpPairings.exe. In order for this to work,
;;                 remember to add XXR and XXCfields in the file with the number
;;                 of rounds of the tournament.
;;
;; TODO:
;; ---------------------------------
;;
;; - Write the add players function in ELISP.
;;
;; - Automatically purge all players who didn't play any games.
;;
;; - Insert results from a results file created with a pairing program.
;;   Add the date in the "132" line and the results in the "001" lines.
;;
;; - Add empty round. Ask for date create empty space in the players lines.
;;   Add the date in the "132" line.
;; 
;; - Add the rank number and the position automatically when adding players.
;;
;; - Add team.
;;
;; - Add player to team. Prompt for team and player number.
;;
;; - Generate pgn file for a round or the whole tournament.
;;
;; - Reorder the players list
;;
;; - Error handling
;;
;; - Calculate tyebreaks
;;
;; - Make the interface more friendly
;;
;; You will find more information in www.dggandara.eu/arbitools.htm

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun arbitools-do-pairings (round)
  "Use bbpPairings to do the pairings for the next round.
   You need a XXR section followed by the number of rounds.
   If you have any players that are not going to be paired, 
   insert 0000 - H in the column, for a half point bye and 
   0000 - F for full point bye. You can do that with
   arbitools-insert-bye. For the first round you will need a
   XXC section followed by white1 or black1, which will force
   the corresponding colour.
   If the program throws an error you will find it in the 
   Pairings-output buffer."
  ;; TODO: if there is no XXR entry, error and prompt to write one.
  ;; TODO: right now, the program writes "0" as an opponent for allocated bye:
  ;;       replace this with "0000 - U".
  (interactive "sWhich round do you need to generate the pairings for?: ")
  (save-excursion
     (with-current-buffer "Pairings-output"
        (erase-buffer)))
  (call-process "bbpPairings.exe" nil "Pairings-output" nil  "--dutch" buffer-file-name "-p")
    
  (let* ((actualround (arbitools-actual-round))
         (numberofrounds (arbitools-number-of-rounds))
         (numberoftables 0)
         (actualtable 0)
         (white 0)
         (black 0)
         (positiontowrite (+ 89 (* (- (string-to-number round) 1) 10)))
         (endoflinecolumn 0))
       
       (save-excursion
         (with-current-buffer "Pairings-output"
           (goto-char (point-min))
           (setq numberoftables (string-to-number (thing-at-point 'word)))))
       (while (<= actualtable numberoftables)
         (save-excursion
           (with-current-buffer "Pairings-output"
                 (forward-line)
                 (setq actualtable (+ actualtable 1))
                 (setq white (thing-at-point 'word))
                 (forward-word)
                 (forward-word)
                 (setq black (thing-at-point 'word))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward "^001" nil t)
             (forward-char 4) ;; go to rank number
             (when (string= white (thing-at-point 'word))
               (end-of-line)
               (setq endoflinecolumn (current-column))
               (beginning-of-line)
               (forward-char positiontowrite)
               (unless (= positiontowrite endoflinecolumn) ;; check if there is something and 
                 (delete-char (- endoflinecolumn positiontowrite)))   ;; erase it
               (insert "     ") ;; replace the first positions with spaces
               (cond ((= 2 (length black)) (backward-char 1));; make room for bigger numbers
                 ((= 3 (length black)) (backward-char 2)))
               (insert (format "%s w  " black))
               (cond ((= 2 (length black)) (delete-char 1));; adjust when numbers are longer
                 ((= 3 (length black)) (delete-char 2))))
             (when (string= black (thing-at-point 'word))
               (end-of-line)
               (setq endoflinecolumn (current-column))
               (beginning-of-line)
               (forward-char positiontowrite)
               (unless (= positiontowrite endoflinecolumn) ;; check if there is something and 
                 (delete-char (- endoflinecolumn positiontowrite)))   ;; erase it
               (insert "     ") ;; replace the first positions with spaces
               (cond ((= 2 (length white)) (backward-char 1)) ;; make room for bigger numbers
                 ((= 3 (length white)) (backward-char 2)))
               (insert (format "%s b  " white))
               (cond ((= 2 (length white)) (delete-char 1));; adjust when numbers are longer
                 ((= 3 (length white)) (delete-char 2)))))))))

(defun arbitools-prepare-file-DOS ()
  "Prepare file for DOS: add carriage return at the end of lines.
   For some administrators, like the ones in FEDA, the files need
   to be in this format or they will not allow them."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun arbitools-update (elolist)
  "Update the players ratings in a database file based on a elo list file."
  (interactive "selolist:")
  ;; FIXME: What if `list' is "foo; bar"?
  (call-process "arbitools-run.py" nil "Arbitools-output" nil "update" buffer-file-name "-l" elolist))

(defun arbitools-add (addfile)
  "Add players to an existing database file."
  (interactive "faddfile: ")
  ;; FIXME: What if `addlist' is "foo; bar"?
  (call-process "arbitools-add.py" nil "Arbitools-output" nil "-a" addfile "-i" buffer-file-name))

(defun arbitools-trim-left (s)
            "Remove whitespace at the beginning of S."
            (if (string-match "\\`[ \t\n\r]+" s)
              (replace-match "" t t s)
              s))
(defun arbitools-trim-right (s)
            "Remove whitespace at the end of S."
            (if (string-match "[ \t\n\r]+\\'" s)
              (replace-match "" t t s)
              s))

(defun arbitools-arpo-vega ()
  "Create userTB.txt file for vega based on ARPO results file. Use in crosstable.txt. 
   You need to open the ARPO file in another buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-line 7) ;; where the data starts in crosstable.txt
    (let* ((continue t)
	   (arpodata "data")
	   (arpopoint "point")
	   (name " ")
	   namesplit)

      (when (not (get-buffer "userTB.txt")) (generate-new-buffer "userTB.txt"))
      (save-excursion (with-current-buffer "userTB.txt" (erase-buffer) (insert "  User Tie-Break  ;")))
      (while continue ;; loop over crosstable.txt
        (beginning-of-line) (forward-word)
	(if (thing-at-point 'word)
	    ;; if statement
	    (progn
	      (clear-string name)
	      (setq name (substring-no-properties (thing-at-point 'line) 4 24)) ;; read the players name
	      (setq namesplit (split-string name ",")) ;; remove the comma, which is not in ARPO1
	      (setq name (mapconcat 'identity namesplit "" )) ;; remove the comma
	      (setq name (arbitools-trim-right name)) ;; remove the comma
	      
	      (save-excursion (with-current-buffer "ARPO1.txt"
		  (goto-char (point-min))
				
		  (if (search-forward name) ;; find the name from crosstable
		    ;; then
		    (progn
                      (end-of-line)(backward-word) ;; go to the end of line, where the ARPO is
		      (setq arpopoint (thing-at-point 'word))(backward-word) ;; get decimal figures
		      (setq arpodata (thing-at-point 'word)) ;;get integer part
	
		      (save-excursion (with-current-buffer "userTB.txt"
			(insert arpodata)(insert ".")
			(insert arpopoint) (insert ";")))) ;; insert the ARPO in userTB.txt
                    ;; else
		    (save-excursion (with-current-buffer "userTB.txt"
			(insert "0.0;")))))) ;; in case the player has not got an ARPO, write a 0
	    (next-line)) 
	    ;;else statement
	    (setq continue nil)))))) ;; if no more players, tell the while to stop


(defun arbitools-list-pairing (round)
  "Get the pairings and/or results of the given round. It will
   only work with the current round. Some player's names will be
   missing if you try a finished round, and the order of the tables
   will be wrong.
   You will find the pairings in the Pairing List buffer."
  ;; TODO: Fix the finished rounds issue.
  ;; TODO: There is an issue with the table number for floats.
  (interactive "sFor which round?: ")
  (arbitools-calculate-standings)
  (save-excursion
    (let* ((tournamentnamestring)
           (numberofplayers)
           (tablenumber 1)
           (tablenumberstring)
           (paired '())
           (saveposition)
           (savepositionstandings)
           (namestring nil)
           (playerlinestring nil)
           (opponentlinestring "- ")
           (opponentstring nil)
           (rankstring nil)
           (fideidstring nil)
           (opponent nil)
           (color nil)
           (result nil))
      
      
      (goto-char (point-min))
      (re-search-forward "^012" nil t)
      (setq tournamentnamestring (substring-no-properties (thing-at-point 'line) 4 (end-of-line)))
      (goto-char (point-min))
      (re-search-forward "^062" nil t)
      (setq numberofplayers (string-to-number (substring-no-properties 
        (thing-at-point 'line) 4 (end-of-line))))
      (with-current-buffer "Pairings List"
        (erase-buffer)
        (insert (format "%s" tournamentnamestring))
        (insert (format "Pairings for round %s\n\n" round)))
      (with-current-buffer "Standings" 
        (goto-char (point-min)) (forward-line 4) (setq savepositionstandings (point)))
      (while (>= numberofplayers 1)
         (with-current-buffer "Standings"
           (goto-char savepositionstandings)
           (forward-line 1)
           (setq fideidstring (arbitools-trim-left (substring-no-properties (thing-at-point 'line) 50 58)))
           (setq savepositionstandings (point)))
         (goto-char (point-min))
         (search-forward fideidstring)
         (setq rankstring (arbitools-trim-left (substring-no-properties (thing-at-point 'line) 4 8)))
         (setq namestring (substring-no-properties (thing-at-point 'line) 14 46))
         (setq opponent (arbitools-trim-left (substring-no-properties (thing-at-point 'line) 
                        (+ 91 (* (- (string-to-number round) 1)10 ))
                        (+ 95(* (- (string-to-number round) 1) 10 )))))
         (setq color (substring-no-properties (thing-at-point 'line) 
                        (+ 96 (* (- (string-to-number round) 1)10 ))
                        (+ 97(* (- (string-to-number round) 1) 10 ))))
         (setq result (substring-no-properties (thing-at-point 'line) 
                        (+ 98 (* (- (string-to-number round) 1)10 ))
                        (+ 99(* (- (string-to-number round) 1) 10 ))))

         (setq saveposition (point))
         (goto-char (point-min))
         (while (re-search-forward "^001" nil t)
           (forward-char 4)
           (when (string= (arbitools-trim-left opponent) (thing-at-point 'word))
             (setq opponentstring (substring-no-properties (thing-at-point 'line) 14 46))
             (with-current-buffer "Arbitools-output" (insert (format "%s" opponentstring)))))
         (goto-char saveposition)      
         (unless (or (member rankstring paired) (member opponent paired))
           (cl-pushnew rankstring paired :test #'equal)         
           (with-current-buffer "Pairings List"
             (setq tablenumberstring (number-to-string tablenumber))
             (when (< (length tablenumberstring)  2) 
               (setq tablenumberstring (concat " " tablenumberstring)))
             (when (< (length rankstring)  2) 
               (setq rankstring (concat rankstring " ")))
             (when (< (length opponent)  2) 
               (setq opponent (concat opponent " ")))
             (cond ((string= color "w")
                     (cond ((string= result "1")
                             (insert (format "%s. %s %s 1-0 %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))
                           ((string= result "0")
                             (insert (format "%s. %s %s 0-1 %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))
                           ((string= result "+")
                             (insert (format "%s. %s %s + - %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))
                           ((string= result "-")
                             (insert (format "%s. %s %s - + %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))
                           ((string= result " ")
                             (insert (format "%s. %s %s  -  %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))
                           ((string= result "=")
                             (insert (format "%s. %s %s 1/2 %s %s\n" tablenumberstring rankstring 
                              namestring opponent opponentstring)))))
                   ((string= color "b")
                     (cond ((string= result "1")
                           (insert (format "%s. %s %s 0-1 %s %s\n" tablenumberstring opponent opponentstring 
                            rankstring namestring)))
                          ((string= result "0")
                           (insert (format "%s. %s %s 1-0 %s %s\n" tablenumberstring opponent opponentstring 
                            rankstring namestring)))
                          ((string= result "+")
                           (insert (format "%s. %s %s - + %s %s\n" tablenumberstring opponent opponentstring 
                            rankstring namestring)))
                          ((string= result "-")
                           (insert (format "%s. %s %s + - %s %s\n" tablenumberstring opponent opponentstring 
                            rankstring namestring)))
                          ((string= result " ")
                           (insert (format "%s. %s %s  -  %s %s\n" tablenumberstring opponent opponentstring  
                            rankstring namestring)))
                          ((string= result "=")
                           (insert (format "%s. %s %s 1/2 %s %s\n" tablenumberstring opponent opponentstring 
                            rankstring namestring))))))))
         (setq tablenumber (+ tablenumber 1))
         (setq numberofplayers (- numberofplayers 1)))))
   (switch-to-buffer "Pairings List"))



(defun arbitools-standings ()
  "Get standings and report files from a tournament file."
  ;; TODO: Add tiebreaks
  (interactive)
  ;; (shell-command (concat (expand-file-name "arbitools-standings.py") " -i " buffer-file-name))) ;this is to use the actual path
  (call-process "arbitools-run.py" nil "Arbitools-output" nil "standings" buffer-file-name))

(defun arbitools-list-players ()
  "Put the list of players in two buffers, one in plain text and another 
   in a beautiful LaTeX.
   You will also find a list in the List of players buffer."
  ;; TODO: the beautiful LaTeX
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (with-current-buffer "List of players" (erase-buffer))
   (while (re-search-forward "^001" nil t)
     (let* ((linestring (thing-at-point 'line))
           (rankstring (substring linestring 5 8)))
       
       (with-current-buffer "List of players"
         (insert (format " %s " rankstring))))

     (let* ((linestring (thing-at-point 'line))
           (namestring (substring linestring 14 47)))
       
       (with-current-buffer "List of players"
         (insert (format "%s " namestring))))

     (let* ((linestring (thing-at-point 'line))
            (elostring (substring linestring 48 52)))
       
       (with-current-buffer "List of players"
         (insert (format "%s\n" elostring))))))
  (with-current-buffer "List of players"
    (remove-text-properties (point-min)(point-max) '(face nil))))

(defun arbitools-new-trf ()
  "Create an empty trf file"
  (interactive)
  (generate-new-buffer "New trf")
  (switch-to-buffer "New trf")
  (set-buffer "New trf")
  (arbitools-mode)
  (insert "012 NAME OF THE TOURNAMENT\n")
  (insert "022 PLACE\n")
  (insert "032 FEDERATION\n")
  (insert "042 STARTING DATE (YYYY/MM/DD)\n")
  (insert "052 ENDING DATE (YYYY/MM/DD)\n")
  (insert "062 NUMBER OF PLAYERS\n")
  (insert "072 NUMBER OF RATED PLAYERS\n")
  (insert "082 NUMBER OF TEAMS\n")
  (insert "092 TYPE OF TOURNAMENT\n")
  (insert "102 CHIEF ARBITER\n")
  (insert "112 DEPUTY CHIEF ARBITER\n")
  (insert "122 ALLOTED TIMES PER MOVE/GAME\n")
  (insert "XXC COLOR FOR THE FIRST ROUND (white1 or black1)\n")
  (insert "XXR NUMBER OF ROUNDS\n")
  (insert "132 DATES                                                                                  YY/MM/DD  YY/MM/DD\n")
  ;; (insert "001  000 GTIT NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN RAT. FED   0000000000 YYYY/MM/DD 00.0  RNK  0000 C R  0000 C R\n")
  ;; (insert "013 NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN  0000 0000\n")
)

 (defun arbitools-number-of-rounds ()
   "Get the number of rounds in the tournament. It has to be executed in the 
    principal buffer."
   (let* ((numberofrounds 0))
     
     (save-excursion
       (if (re-search-forward "^XXR" nil t)
         (progn 
            (beginning-of-line)
            (forward-char 5)
            (setq numberofrounds (string-to-number (thing-at-point 'word))))
       
         (goto-char (point-min))
         (re-search-forward "^132" nil t)
         (let* ((linestringrounds (thing-at-point 'line))
              (beginning-of-round 91)
              (end-of-round 99)
              (continue t))
            (while continue
              (if (< end-of-round (length linestringrounds))
                
                (progn
                   (setq numberofrounds (+ numberofrounds 1))
                   (setq beginning-of-round (+ beginning-of-round 10))
                   (setq end-of-round (+ end-of-round 10)))
                  
                   (setq continue nil))))))
     numberofrounds))

(defun arbitools-actual-round ()
  "Calculate the actual round. It has to be run on the principal buffer."
  (let* ((actualround 0))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^001" nil t)
      (end-of-line)
      (setq actualround (- (current-column) 89))
      ;; 89 is the position of the initial data
      (when (> (current-column) 89)
        (setq actualround (/ (current-column) 10)))
      (when (< actualround 0)
        (setq actualround 0)))
      ;;(save-excursion (with-current-buffer "Arbitools-output"  
      ;;    (insert (format "column: %d -" actualround))))
    actualround))

(defun arbitools-calculate-points (round)
  "Automatically calculate the points of each player and adjust the 
   corresponding column. 
   Don't use this function when the round doesn't include all the results."
  (interactive "sUp to which round?: ")
  (save-excursion
    (let ( (numberofrounds (arbitools-number-of-rounds))
           (points         0.0)
           (pointstosum    0.0)
           (roundcount     1))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (setq points 0.0)
        (setq roundcount 1)
        (while (<= roundcount (string-to-number round))
          (beginning-of-line)
	  (forward-char (+ 98 (* (- roundcount 1) 10))) ;; go to where the result is for each round
          (cond ((string= (thing-at-point 'symbol) "1") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) "+") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) "=") (setq pointstosum 0.5))
                ((string= (thing-at-point 'symbol) "0") (setq pointstosum 0.0))
                ((string= (thing-at-point 'symbol) "-") (setq pointstosum 0.0))
                ((string= (thing-at-point 'symbol) "F") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) "H") (setq pointstosum 0.5))
                ((string= (thing-at-point 'symbol) "Z") (setq pointstosum 0.0))
                ((string= (thing-at-point 'symbol) "U") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) nil) (setq pointstosum 0.0)))
          (setq points (+ points pointstosum))
          (setq roundcount (+ roundcount 1)))
        (beginning-of-line)
        (forward-char 84)
        (forward-char -3)
        (delete-char 3)
        (insert-char ?\s (- 3 (length (format "%s" points))))
        (insert (format "%s" points))))))

(defun arbitools-calculate-standings ()
  "Write the standings in the Standings buffer. Update the POS field in the 
   file.
   You might need to run arbitools-calculate-points before using this
   function."
  ;; TODO: Write tiebreaks. Write a new function that organize the standings according 
  ;; to a given tiebreak.
  ;; Also, make it possible to print standing for past rounds.
  (interactive)
  (save-excursion
    (with-current-buffer "Standings"
      (erase-buffer))
    (arbitools-list-players)
    (let* ((linestring (thing-at-point 'line))
          (tournamentnamestring (substring linestring 4))
          (numberofplayers 0)
          (round "round")
          (datachunk "")
          (newpos 0)
          (idfide "")
          (beg)
          (end))
      (goto-char (point-min))
      (re-search-forward "^062" nil t)
      (forward-char 1)
      (setq numberofplayers (thing-at-point 'word)) ;; get the number of players
      ;;(setq numberofplayers "27")
      (goto-char (point-min))
      (re-search-forward "^012" nil t) ;; Get the name of the tournament
      (with-current-buffer "Standings" ;; write the headings
        (erase-buffer)
        (insert (format "%s" tournamentnamestring))
        (insert (format "Standings for round %s\n\n" round))
        (insert (format "POS. PTS.   No. Name                              ID\n")))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t) ;; loop the players in the main buffer
        (beginning-of-line)
        (setq datachunk (substring-no-properties (thing-at-point 'line) 80 84)) ;; get points
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 4 (length datachunk)))
          (insert "  "))
        (setq datachunk (substring-no-properties (thing-at-point 'line) 4 8)) ;; get rank
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 4 (length datachunk)))
          (insert "  "))
        (setq datachunk (substring-no-properties (thing-at-point 'line) 14 47)) ;; get name
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 33 (length datachunk)))
          (insert " "))
        (beginning-of-line)
        (forward-char 67)
        (setq datachunk (thing-at-point 'word)) ;; get idfide 
        (with-current-buffer "Standings"
          (insert (format "%s" datachunk))
          (insert-char ?\s (- 10 (length datachunk)))
          (insert "\n")))
     (with-current-buffer "Standings" ;;  sorting
        (goto-char (point-min))
        (forward-line 4)
        (setq beg (point))
        (goto-char (point-max))
        (setq end (point))
        (reverse-region beg end) ;; make sure it respects the ranking
        (sort-numeric-fields 1 beg end)
        (goto-char (point-min))
        (forward-line 4)
        (while (>= (string-to-number numberofplayers) 1) ;; write the positions
          (insert (format "%s" numberofplayers))
          (insert-char ?\s (- 3 (length numberofplayers)))
          (insert " ")
          (setq numberofplayers (number-to-string(- (string-to-number numberofplayers) 1)))
          (forward-line 1))
        (goto-char (point-min))
        (forward-line 4)
        (setq beg (point))
        (goto-char (point-max))
        (setq end (point))
        (reverse-region beg end)) ;; get this to sort in reverse
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (beginning-of-line)
        (forward-char 68)
        (setq idfide (thing-at-point 'word))
        (with-current-buffer "Standings"
          (goto-char (point-min))
          (search-forward idfide nil t)
          (setq newpos (- (line-number-at-pos) 4))) ;; the number of gives as the pos field
                                              ;; minus 4 because of the first two lines
        (beginning-of-line)
        (forward-char 89) ;; go to POS field
        (forward-char -3)
        (delete-char 3)
        (insert-char ?\s (- 3 (length (format "%s" newpos))))
        (insert (format "%s" newpos))))))

(defun arbitools-delete-player (player)
   "Delete a player. Adjust all the rank numbers accordingly."
   (interactive "sInsert rank number of the player: ")
   (let ((numberofrounds 0)
         (elo            ""))
    
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^132" nil t)
        (let* ((linestringrounds   (thing-at-point 'line))
               ;; (actualround        " ")
               (beginning-of-round 91)
               (end-of-round       99)
               (continue           t))
           (while continue
             (if (< end-of-round (length linestringrounds))
               (progn
                  ;; (setq actualround (substring-no-properties linestringrounds beginning-of-round end-of-round))
                  (setq numberofrounds (+ numberofrounds 1))
                  (setq beginning-of-round (+ beginning-of-round 10))
                  (setq end-of-round (+ end-of-round 10)))   
               (setq continue nil)))))
    (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^001" nil t)
       (let* ((linestring (thing-at-point 'line))
              (rankstring (substring linestring 5 8)))
         (when (= (string-to-number rankstring) (string-to-number player))
           (forward-char 1)
           (delete-char 4)
           (insert " DEL")
           (setq elo (substring linestring 48 52))
           (with-current-buffer "Arbitools-output" (insert (format "%s" elo))))
         (when (> (string-to-number rankstring)(string-to-number player))
           (forward-char 1)
           (delete-char 4)
           (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
           (insert (format "%s" (- (string-to-number rankstring) 1)))
           (save-excursion
             (goto-char (point-min))
             (while (re-search-forward "^001" nil t)
               (let* ((roundcount          1))
                  (while (<= roundcount numberofrounds)
                    (beginning-of-line)
                    (forward-char (+ 95 (* (- roundcount 1) 10)))
                    (when (string= (format "%s" (string-to-number rankstring)) (thing-at-point 'word))
                      (forward-char -4) ;; go back to the beginning of the opponent's number
                      (delete-char 4) ;; remove the original opponent's number
                      (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
                      (insert (format "%s" (- (string-to-number rankstring) 1))))
                    (setq roundcount (+ roundcount 1))))
               ;;(condition-case nil ;; TODO: fix teams info
                 (save-excursion 
                   (while (re-search-forward "^013" nil t)
                    (let* ((linestringteam (thing-at-point 'line))
                          (integrantcount 0)
                          (members 0))

                        ;; to find the end of the line, the number is length -2, for some reason
                        (setq members (/ (- (- (length linestringteam) 2) 34) 5)) ;; calculate number of members

                      (while (< integrantcount members)
                       (beginning-of-line)
                       (forward-char (+ 40 (* (- integrantcount 1) 5)))
                       (when (string= (format "%s" (string-to-number rankstring)) (thing-at-point 'word))
                         (forward-char -4)
                         (delete-char 4)
                         (insert-char ?\s (- 4 (length (format "%s" (- (string-to-number rankstring) 1)))))
                         (insert (format "%s" (- (string-to-number rankstring) 1))))
                       (setq integrantcount (+ integrantcount 1))))))))))))
             
     (save-excursion  ;; Actually delete the player's line
       (goto-char (point-min))
       (while (re-search-forward "^001  DEL" nil t)
         (beginning-of-line)
         (let ((beg (point)))
           (forward-line 1)
           (delete-region beg (point)))))
     ;; TODO delete the rank from teams section
     ;; TODO change number of players and number of rated players
     (save-excursion
       (with-current-buffer "Arbitools-output" (insert (format "%s" elo)))
       (goto-char (point-min))
       (re-search-forward "^062 ")
       (let* ((linestring      (thing-at-point 'line))
              (numberofplayers (substring linestring 4))) 
        (delete-char (length numberofplayers))
        (setq numberofplayers (string-to-number numberofplayers))
        (setq numberofplayers (- numberofplayers 1))
        (insert (concat (number-to-string numberofplayers) "\n")))
       (re-search-forward "^072 ")
       (let* ((linestring           (thing-at-point 'line))
              (numberofratedplayers (substring linestring 4))) 
        (unless (< (length elo) 2) ;; if elo is 0 or nonexistent
          (delete-char (length numberofratedplayers))
          (setq numberofratedplayers (string-to-number numberofratedplayers))
          (setq numberofratedplayers (- numberofratedplayers 1))
          (insert (concat (number-to-string numberofratedplayers) "\n")))))))

(defun arbitools-delete-round (round)
   "Delete a round."
   ;; TODO: It seems that it doesn't delete a previous bye inserted. 
   (interactive "sround: ")
   (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^001" nil t)
     (forward-char (+ 88 (* (- (string-to-number round) 1) 10)))
     (delete-char 8)
     (insert "        "))))

(defun arbitools-insert-bye (player round type)
   "Insert bye for player. Types of byes are: H for Half point,
    F for Full point, Z for zero points of U for allocated by the system."
   (interactive "sRank number of the player: \nsround: \nstype (H, F, -, Z, U):")
   (let* ((pointtowrite (+ 89 (* (- (string-to-number round) 1) 10)))
       (positionendofline 0)
       (points 0.0))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^001" nil t)
         (forward-char 4) ;; go to rank number
         (when (string= player (thing-at-point 'word))
          (end-of-line)
          (setq positionendofline (current-column))
          ;; create space if needed
          (when (< positionendofline pointtowrite)
            (end-of-line)
            (insert-char 32 (- pointtowrite positionendofline)))
          (beginning-of-line)
          (forward-char 84)
          (forward-char -3)
          (setq points (string-to-number (thing-at-point 'word)))
          (cond ((string= type "H")(setq points (+ points 0.5)))
            ((string= type "F")(setq points (+ points 1.0)))
            ((string= type "U")(setq points (+ points 1.0)))
            ((string= type "Z")(setq points (+ points 0.0)))
            ((string= type "-")(setq points (+ points 0.0))))
          (delete-char 3)
          (insert-char ?\s (- 3 (length (format "%s" points)))) ;; write extra empty spaces
          (insert (format "%s" points)) ;; write the points
          (beginning-of-line)
          (forward-char pointtowrite) 
          (insert (format "  0000 - %s" type)))))))

(defun arbitools-replace-empty ()
   "Replace non played games with spaces."
   (interactive)
   (save-excursion
    (goto-char (point-min))
    (while (search-forward "0000 - 0" nil t)
      (replace-match "        "))))

(defun arbitools-insert-player (sex title name elo fed idfide year)
   "Insert a player. You will be prompted for the data."
   ;; TODO: automatically insert the player in a team
   (interactive "ssex: \nstitle: \nsname: \nselo: \nsfed: \nsidfide: \nsyear: ")
  (let ((playerlinelength nil)
        (thislinelength nil)) 
     (save-excursion
       (goto-char (point-min))
       (re-search-forward "^001 ")
       (let* ((linestring (thing-at-point 'line))) 
         (setq playerlinelength (length linestring))))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^001" nil t))
       (let* ((linestring (thing-at-point 'line))
              (rankstring (substring linestring 5 8)))

         (forward-line 1)
         (insert "\n")
         (forward-char -1)
         (insert (format "001 "))
         (insert-char ?\s (- 4 (length (format "%s" (+ (string-to-number rankstring) 1)))))
         (insert (format "%s" (+ (string-to-number rankstring) 1)))
         (insert (format " %s" sex))
         (when (= (length sex) 0) (insert " ")) ;; add extra space if the sex string is empty
         (insert-char ?\s (- 3 (length title)))
         (insert (format "%s " title))
         (insert (format "%s" name))
         (insert-char ?\s (- 34 (length name)))
         (when (= (length elo) 4) (insert (format "%s " elo)))
         (when (= (length elo) 0) (insert "     ")) ;; add extra space if the elo is empty
         (when (= (length elo) 1) (insert "   0 ")) ;; add extra space if the elo is a "0"
         (insert (format "%s" fed))
         (when (= (length fed) 0) (insert "   ")) ;; add extra space if fed is empty
         (insert-char ?\s (- 12 (length idfide)))
         (insert (format "%s " idfide))
         (insert (format "%s      " year))
         (when (= (length year) 0) (insert "    ")) ;; TODO: improve this to make it support different data formats
         (insert (format "  0.0 "))
         (insert-char ?\s (- 4 (length (format "%s" (+ (string-to-number rankstring) 1)))))
         (insert (format "%s" (+ (string-to-number rankstring) 1)))
         (setq thislinelength (length (thing-at-point 'line)))
         (insert-char ?\s (- playerlinelength thislinelength)))))   
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^062 ")
    (let* ((linestring (thing-at-point 'line))
           (numberofplayers (substring linestring 4))) 
      (delete-char (length numberofplayers))
      (setq numberofplayers (string-to-number numberofplayers))
      (setq numberofplayers (+ 1 numberofplayers))
      (insert (concat (number-to-string numberofplayers) "\n")))
    (re-search-forward "^072 ")
    (let* ((linestring (thing-at-point 'line))
        (numberofratedplayers (substring linestring 4))) 
      (unless (< (length elo) 2)
        (delete-char (length numberofratedplayers))
        (setq numberofratedplayers (string-to-number numberofratedplayers))
        (setq numberofratedplayers (+ 1 numberofratedplayers))
        (insert (concat (number-to-string numberofratedplayers) "\n"))))))

(defun arbitools-insert-result (round white black result)
   "Insert a result. You will be prompetd for the white and black players
    rank numbers and the result (1, 0, =, +, -)"
   ;; TODO: It erases everything at the end. Fix this.
   (interactive "sround: \nswhite player's rank number: \nsblack player's rank number: \nsresult (1, 0, =, +, -): ")
   (let* ((pointtowrite (+ 89 (* (- (string-to-number round) 1) 10)))
     (positionendofline 0))
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^001" nil t)
       (forward-char 4) ;; go to rank number
       (when (string= white (thing-at-point 'word))
         ;; go to first round taking into account the cursor is in the rank number
         (end-of-line)
         (setq positionendofline (current-column))
         (beginning-of-line)
         (forward-char pointtowrite)
         (unless (= pointtowrite positionendofline) ;; check if there is something and 
           (delete-char (- positionendofline pointtowrite)))   ;; erase it
         (insert "     ") ;; replace the first positions with spaces
         ;; make room for bigger numbers
         (cond ((= 2 (length black))
           (backward-char 1))
           ((= 3 (length black))
           (backward-char 2)))
         (insert (format "%s w %s" black result))) 
       (when (string= black (thing-at-point 'word))
         ;; go to first round taking into account the cursor is in the rank number
         (end-of-line)
         (setq positionendofline (current-column))
         (beginning-of-line)
         (forward-char pointtowrite)
         (unless (= pointtowrite positionendofline) ;; check if there is something and 
           (save-excursion (with-current-buffer "Arbitools-output" (insert "yes")))
           (delete-char (- positionendofline pointtowrite)))   ;; erase it
         (insert "     ") ;; replace the first positions with spaces
         ;; make room for bigger numbers
         (cond ((= 2 (length white)) (backward-char 1))
           ((= 3 (length white)) (backward-char 2)))
         (cond ((string= "1" result) (insert (format "%s b 0" white)))
           ((string= "=" result) (insert (format "%s b =" white)))
           ((string= "+" result) (insert (format "%s b -" white)))
           ((string= "-" result) (insert (format "%s b +" white)))
           ((string= "0" result) (insert (format "%s b 1" white)))))))))

(defun arbitools-it3 ()
   "Get the IT3 tournament report. You will get a .tex file, and a pdf
    if you have pdflatex installed."
   (interactive)
   (call-process "arbitools-run.py" nil "Arbitools-output" nil "it3" buffer-file-name))

;; TODO: New It3 function, usint it3.tex from home directory, replacing the data and pdflatex it

(defun arbitools-fedarating ()
   "Get the FEDA rating admin file."
   (interactive)
   (call-process "arbitools-run.py" nil "Arbitools-output" nil "fedarating" buffer-file-name))

(defvar arbitools-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c i") 'arbitools-insert-player)
    (define-key map (kbd "C-c r") 'arbitools-insert-result)
    (define-key map (kbd "C-c p") 'arbitools-do-pairing)
    (define-key map (kbd "C-c b") 'arbitools-insert-bye)
    map)
  "Keymap for Arbitools major mode.")


(easy-menu-define arbitools-mode-menu arbitools-mode-map
  "Menu for Arbitools mode"
  '("Arbitools"
    ["New Tournament header" arbitools-new-trf]
    "---"
    ["Insert Player" arbitools-insert-player]
    ["Delete Player" arbitools-delete-player]
    "---"
    ["Do Pairings" arbitools-do-pairings]
    ["Insert Result" arbitools-insert-result]
    ["Insert Bye" arbitools-insert-bye]
    ["Delete Round" arbitools-delete-round]
    "---"
    ["List Players" arbitools-list-players]
    ["List Pairings" arbitools-list-pairing]
    ["Recalculate Standings" arbitools-calculate-standings]
    ["Recalculate points" arbitools-calculate-points]
    "---"
    ["Print Standings to file" arbitools-standings]
    "---"
    ["Update Elo" arbitools-update]
    ["Get It3 form Report" arbitools-it3]
    ["Get FEDA Rating file" arbitools-fedarating]
    ["Prepare file for DOS" arbitools-prepare-file-DOS]
    ))


(defvar arbitools-highlights
 '(("^001" . font-lock-function-name-face) ; name of the tournament
    ("^012.*" . font-lock-comment-face)
    ("\\(^022\\|^032\\|^042\\|^052\\|^062\\|^072\\|^082\\|^092\\|^102\\|^112\\|^122\\).*" . font-lock-constant-face)
    ("^132.*" . font-lock-warning-face) ;dates
    ("^013" . font-lock-warning-face) ;teams
    ("\\(^013.\\{1\\}\\)\\(.\\{31\\}\\)" 2 font-lock-comment-face) ;; teams
    ;; (" [0-9]\\{6,\\} " . font-lock-variable-name-face) ;FIDE ID
    ("\\(^001.\\{11\\}\\)\\(.\\{32\\}\\)" 2 font-lock-string-face)  ;; Name of the player (by position)
    ("\\(^001.\\{55\\}\\)\\(.\\{10\\}\\)" 2 font-lock-function-name-face) ;; FIDE ID
    ("\\(^001.\\{88\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face) ;; round 1 opponent
    ;; ("\\(^132.\\{88\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face) ;; round 1 date line
    ("\\(^001.\\{93\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face) ;; round 1 colour
    ("\\(^001.\\{95\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face) ;; round 1 result
    ;; rest of rounds
    ("\\(^001.\\{98\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{98\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{103\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{105\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{108\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{108\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{113\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{115\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{118\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{118\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{123\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{125\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{128\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{128\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{133\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{135\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{138\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{138\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{143\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{145\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{148\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{148\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{153\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{155\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{158\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{158\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{163\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{165\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{168\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{168\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{173\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{175\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{178\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{178\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{183\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{185\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{188\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{188\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{193\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{195\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)
    ("\\(^001.\\{198\\}\\)\\(.\\{4\\}\\)" 2 font-lock-comment-face)
    ;; ("\\(^132.\\{198\\}\\)\\(.\\{8\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{203\\}\\)\\(.\\{1\\}\\)" 2 font-lock-string-face)
    ("\\(^001.\\{205\\}\\)\\(.\\{1\\}\\)" 2 font-lock-function-name-face)))

;;;###autoload
(define-derived-mode arbitools-mode
  fundamental-mode
  "Arbitools"
  "Major mode for Chess Tournament Management."
  ;(setq font-lock-defaults '(arbitools-highlights))
  (use-local-map arbitools-mode-map)
  (generate-new-buffer "Arbitools-output")
  (generate-new-buffer "List of players")
  (generate-new-buffer "Pairings List")
  (generate-new-buffer "Standings")
  (generate-new-buffer "Pairings-output")
  (column-number-mode)
  (set (make-local-variable 'font-lock-defaults) '(arbitools-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trf?\\'" . arbitools-mode))

;;;; ChangeLog:

;; 2018-06-22  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	 arbitools.el: added new function
;; 
;; 2018-01-10  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	arbitools.el: Improved functions, fixed bugs
;; 
;; 2017-12-31  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	*arbitools.el: Some functions improved
;; 
;; 2017-12-19  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* arbitools.el: added new functions, updated website
;; 
;; 2016-05-04  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Removed unused variables
;; 
;; 2016-05-03  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: minor fixes
;; 
;; 2016-04-24  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* arbitools/arbitools.el: Remove unused vars
;; 
;; 	* arbitools.el (arbitools-calculate-standings, arbitools-delete-player)
;; 	(arbitools-calculate-standings): Remove unused vars.
;; 
;; 2016-04-24  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Fixed some bugs
;; 
;; 2016-04-24  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Fixed some bugs
;; 
;; 2016-04-09  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Added new functions
;; 
;; 2016-03-27  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools.el: Applied suggestions, improved functions
;; 
;; 2016-03-25  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	packages/arbitools: Added new functions
;; 
;; 2016-03-01  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: Add functions and menus
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: added menu option
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: endoffile bug fixed
;; 
;; 2016-02-28  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools: added some new functions and menus
;; 
;; 2016-02-23  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools.el: fix coding issues
;; 
;; 2016-02-22  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* arbitools/arbitools.el: Fix checkdoc warnings and quoting problems
;; 
;; 	* arbitools/arbitools.el (arbitools-update, arbitools-add)
;; 	(arbitools-standings): Fix obvious quoting problems.  Add docstring.
;; 	(arbitools-mode): Use a more conventional mode-name.
;; 
;; 2016-02-22  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	* packages/arbitools.el: correct code syntax issues
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA]: new package: arbitools
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA]: new package: arbitools
;; 
;; 2016-02-21  David Gonzalez Gandara  <dggandara@member.fsf.org>
;; 
;; 	[ELPA] new package: arbitools
;; 


(provide 'arbitools)

;;; arbitools.el ends here
