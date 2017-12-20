;;; arbitools.el --- Package for chess tournaments administration

;; Copyright 2016 Free Software Foundation, Inc.

;; Author: David Gonzalez Gandara <dggandara@member.fsf.org>
;; Version: 0.91
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
;; - Get the pairing or results of a round - Native
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
;;                 remember to add a XXR field in the file with the number
;;                 of rounds of the tournament.
;;
;; TODO:
;; ---------------------------------
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
;; - Reorder the ranking
;;
;; - Reorder the players list
;;
;; - Error handling
;;
;; You will find more information in www.dggandara.eu/arbitools.htm

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun arbitools-do-pairings ()
  "Use bbpPairings to do the pairings for the next round."
  ;; TODO: if there is no XXR entry, error and prompt to write one.
  (interactive)
  (save-excursion
     (with-current-buffer "Pairings-output"
        (erase-buffer)))
    (call-process "bbpPairings.exe" nil "Pairings-output" nil  "--dutch" buffer-file-name "-p")
    
    (let* ((actualround (arbitools-actual-round))
         (numberofrounds (arbitools-number-of-rounds))
         (numberoftables 0)
         (actualtable 0)
         (white 0)
         (black 0))
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
                 (forward-char 4) ;; rank number
                 (when (string= white (thing-at-point 'word))
                   (forward-char (+ 85 (* actualround 10)))
                   (insert "  ") ;; replace the first positions with spaces
                   (delete-char 2)
                   (cond ((= 2 (length black)) (backward-char 1));; make room for bigger numbers
                     ((= 3 (length black)) (backward-char 2)))
                   (insert (format "%s w" black))
                   (delete-char 3)
                   (cond ((= 2 (length black)) (delete-char 1));; adjust when numbers are longer
                     ((= 3 (length black)) (delete-char 2))))
                (when (string= black (thing-at-point 'word))
                   (forward-char (+ 85 (* actualround 10)))
                   (insert "  ") ;; replace the first positions with spaces
                   (delete-char 2)
                   (cond ((= 2 (length white)) (backward-char 1)) ;; make room for bigger numbers
                     ((= 3 (length white)) (backward-char 2)))
                   (insert (format "%s b" white))
                   (delete-char 3)
                   (cond ((= 2 (length white)) (delete-char 1));; adjust when numbers are longer
                     ((= 3 (length white)) (delete-char 2)))))))))

(defun arbitools-prepare-feda ()
  "Prepare file to FEDA: add carriage return at the end of lines."
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

(defun arbitools-list-pairing (round)
  "Get the pairings and/or results of the given round"
  (interactive "sround: ")
  (goto-char (point-min))
  (arbitools-list-players)
  (save-excursion
    (re-search-forward "^012" nil t)
    (let* ((linestring (thing-at-point 'line))
          (tournamentnamestring (substring linestring 4)))
      (with-current-buffer "Pairings List"
        (erase-buffer)
        (insert (format "%s" tournamentnamestring)))))
  (with-current-buffer "Pairings List"
   (insert (format "Pairings for round %s\n\n" round)) )
  (let* ((paired '()))

    (while (re-search-forward "^001" nil t)
       (let* ((namestring nil)
             (linestring (thing-at-point 'line))
             (playerlinestring nil)
             (opponentlinestring nil)
             opponentstring
             (rankstring (substring linestring 4 8))
             (opponent (substring linestring (+ 91 (* (- (string-to-number round) 1)10 )) 
                       (+ 95(* (- (string-to-number round) 1)10 ))))
             (color (substring linestring (+ 96 (* (- (string-to-number round) 1)10 )) 
                       (+ 97(* (- (string-to-number round) 1)10 ))))
             (result (substring linestring (+ 98 (* (- (string-to-number round) 1)10 )) 
                       (+ 99(* (- (string-to-number round) 1)10 )))))
         (with-current-buffer "Arbitools-output"
           (insert (format "%s\n" paired))
           (insert (format "-%s-" rankstring))
           (insert (format "%s\n" (member "   1" paired))))
         (unless (or (member rankstring paired) (member opponent paired))
           (with-current-buffer "List of players"
               (goto-char (point-min))
               (re-search-forward (concat "^" (regexp-quote  rankstring)))
               (setq playerlinestring (thing-at-point 'line))
               (setq namestring (substring playerlinestring 4 37))
               (goto-char (point-min))
               (unless (or (string= opponent "0000") (string= opponent "    "))
                   (re-search-forward (concat "^" (regexp-quote opponent))))
               (setq opponentlinestring (thing-at-point 'line))
               (setq opponentstring (substring opponentlinestring 4 37))
               (when (or (string= opponent "0000")(string= opponent "    "))
                 (setq opponentstring "-"))
               (cl-pushnew rankstring paired :test #'equal))
           (with-current-buffer "Pairings List"
             (cond ((string= color "w") ;; TODO: change the ranknumber with the name
                     (cond ((string= result "1")
                           (insert (format "%s 1-0 %s\n" namestring opponentstring)))
                          ((string= result "0")
                           (insert (format "%s 0-1 %s\n" namestring opponentstring)))
                          ((string= result "+")
                           (insert (format "%s + - %s\n" namestring opponentstring)))
                          ((string= result "-")
                           (insert (format "%s - + %s\n" namestring opponentstring)))
                          ((string= result "=")
                           (insert (format "%s 1/2 %s\n" namestring opponentstring)))))
                   ((string= color "b")
                     (cond ((string= result "1")
                           (insert (format "%s 0-1 %s\n" opponentstring namestring)))
                          ((string= result "0")
                           (insert (format "%s 1-0 %s\n" opponentstring namestring)))
                          ((string= result "+")
                           (insert (format "%s - + %s\n" opponentstring namestring)))
                          ((string= result "-")
                           (insert (format "%s + - %s\n" opponentstring namestring)))
                          ((string= result "=")
                           (insert (format "%s 1/2 %s\n" opponentstring namestring))))))))))))


(defun arbitools-standings ()
  "Get standings and report files from a tournament file."
  (interactive)
  ;; (shell-command (concat (expand-file-name "arbitools-standings.py") " -i " buffer-file-name))) ;this is to use the actual path
  (call-process "arbitools-run.py" nil "Arbitools-output" nil "standings" buffer-file-name))

(defun arbitools-list-players ()
  "Put the list of players in two buffers, one in plain text and another in a beautiful LaTeX"
  ;; TODO: the beautiful LaTeX
  (interactive)
  (save-excursion
   (goto-char (point-min))
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
  (insert "XXR NUMBER OF ROUNDS\n")
  (insert "132 DATES                                                                                  YY/MM/DD  YY/MM/DD\n")
  (insert "XXR NUMBER OF ROUNDS\n")
  ;; (insert "001  000 GTIT NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN RAT. FED   0000000000 YYYY/MM/DD 00.0  RNK  0000 C R  0000 C R\n")
  ;; (insert "013 NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN  0000 0000\n")
)

 (defun arbitools-number-of-rounds ()
   "Get the number of rounds in the tournament. It has to be executed in the principal buffer."
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
  (let* (numberofrounds (arbitools-number-of-rounds)
        (actualround 0)
        (continue t))
    
    (save-excursion
      (re-search-forward "^001" nil t)
      (beginning-of-line)
      (while continue
        (forward-char (+ 93 (* actualround 10)))
        (unless (string= (thing-at-point 'word) nil)
          (setq actualround (+ actualround 1)))
        (when (string= (thing-at-point 'word) nil)
          (setq actualround (+ actualround 1))
          (setq continue nil))))
    actualround))

(defun arbitools-calculate-points ()
  "Automatically calculate the points of each player"
  (interactive)
  (save-excursion
    (let ( (numberofrounds (arbitools-number-of-rounds))
           (points         0.0)
           (pointstosum    0.0)
           (roundcount     1))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (setq points 0.0)
        (setq roundcount 1)
        (while (<= roundcount numberofrounds)
          (beginning-of-line)
	  (forward-char (+ 98 (* (- roundcount 1) 10))) ;; go to where the result is for each round
          (cond ((string= (thing-at-point 'symbol) "1") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) "+") (setq pointstosum 1.0))
                ((string= (thing-at-point 'symbol) "=") (setq pointstosum 0.5))
                ((string= (thing-at-point 'symbol) "0") (setq pointstosum 0.0))
                ((string= (thing-at-point 'symbol) "-") (setq pointstosum 0.0))
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
  "Write the standings in the Standings buffer"
  (interactive)
  (arbitools-calculate-points) ;; make sure the points of each player are correct
  (save-excursion
    (with-current-buffer "Standings"
      (erase-buffer))
    (let ((datachunk ""))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (let* ()
          (beginning-of-line)
          (forward-char 89) ;; get the POS field
          (setq datachunk (thing-at-point 'word))
          (with-current-buffer "Standings"
            (insert (format "%s" datachunk))
            (insert-char ?\s (- 3 (length datachunk)))
            (insert " "))
          (setq datachunk (substring-no-properties (thing-at-point 'line) 14 47)) ;; get name
          (with-current-buffer "Standings"
            (insert (format "%s " datachunk))
            (insert-char ?\s (- 33 (length datachunk))))
          (beginning-of-line)
          (forward-char 68)
          (setq datachunk (thing-at-point 'word)) ;; get idfide 
          (with-current-buffer "Standings"
            (insert (format "%s " datachunk))
            (insert-char ?\s (- 10 (length datachunk))))
          (setq datachunk (substring-no-properties (thing-at-point 'line) 80 84)) ;; get points
          (with-current-buffer "Standings"
            (insert (format "%s " datachunk))
            (insert-char ?\s (- 4 (length datachunk))))
          (with-current-buffer "Standings"
            (insert "\n")
            (sort-columns 1 49 (- (point-max) 1))))))
    (let ((newpos 0)
          (idfide ""))
      (goto-char (point-min))
      (while (re-search-forward "^001" nil t)
        (beginning-of-line)
        (forward-char 68)
        (setq idfide (thing-at-point 'word))
        (with-current-buffer "Standings"
          (goto-char (point-min))
          (search-forward idfide nil t)
          (setq newpos (line-number-at-pos))) ;; the POS is in the beginning of the line in Standings
        (with-current-buffer "Arbitools-output"
          (insert (format "%s" newpos))
          (insert "\n"))
        (beginning-of-line)
        (forward-char 89) ;; go to POS field
        (forward-char -3)
        (delete-char 3)
        (insert-char ?\s (- 3 (length (format "%s" newpos))))
        (insert (format "%s" newpos))))))

(defun arbitools-delete-player (player)
   "Delete a player. Adjust all the rank numbers accordingly."
   (interactive "splayer: ")
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
   (interactive "sround: ")
   (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^001" nil t)
     (forward-char (+ 88 (* (- (string-to-number round) 1) 10)))
     (delete-char 8)
     (insert "        "))))

(defun arbitools-replace-empty ()
   "Replace non played games with spaces"
   (interactive)
   (save-excursion
    (goto-char (point-min))
    (while (search-forward "0000 - 0" nil t)
      (replace-match "        "))))

(defun arbitools-insert-player (sex title name elo fed idfide year)
   "Insert a player"
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
   "Insert a result."
   (interactive "sround: \nswhite: \nsblack: \nsresult: ")
   (save-excursion
     (goto-char (point-min))
     (while (re-search-forward "^001" nil t)
       (forward-char 4) ;; rank number
       (when (string= white (thing-at-point 'word))
         ;;go to first round taking into account the cursor is in the rank number
         (forward-char (+ 85 (* (- (string-to-number round) 1) 10)))
         (insert "  ") ;; replace the first positions with spaces
         (delete-char 2) ;; delete the former characters
         ;; make room for bigger numbers
         (cond ((= 2 (length black))
           (backward-char 1))
           ((= 3 (length black))
           (backward-char 2)))
         (insert (format "%s w %s" black result))
         (delete-char 5) 
         ;; adjust when numbers are longer
         (cond ((= 2 (length black)) (delete-char 1))
           ((= 3 (length black)) (delete-char 2))))
       (when (string= black (thing-at-point 'word))
         ;; go to first round taking into account the cursor is in the rank number
         (forward-char (+ 85 (* (- (string-to-number round) 1) 10)))
         (insert "  ") ;; replace the first positions with spaces
         (delete-char 2) ;; delete the former characters
         ;; make room for bigger numbers
         (cond ((= 2 (length white)) (backward-char 1))
           ((= 3 (length white)) (backward-char 2)))
         (cond ((string= "1" result) (insert (format "%s b 0" white)))
           ((string= "=" result) (insert (format "%s b =" white)))
           ((string= "+" result) (insert (format "%s b +" white)))
           ((string= "-" result) (insert (format "%s b -" white)))
           ((string= "0" result) (insert (format "%s b 1" white))))
         (delete-char 5) 
         ;; adjust when numbers are longer
         (cond ((= 2 (length white)) (delete-char 1))
           ((= 3 (length white)) (delete-char 2)))))))

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
    (define-key map (kbd "C-c i") 'arbitools-it3)
    (define-key map (kbd "C-c r") 'arbitools-insert-result)
    (define-key map (kbd "C-c p") 'arbitools-insert-player)
    map)
  "Keymap for Arbitools major mode.")


(easy-menu-define arbitools-mode-menu arbitools-mode-map
  "Menu for Arbitools mode"
  '("Arbitools"
    ["New Tournament" arbitools-new-trf]
    "---"
    ["Insert Player" arbitools-insert-player]
    ["Delete Player" arbitools-delete-player]
    ["Do Pairings" arbitools-do-pairings]
    ["Insert Result" arbitools-insert-result]
    ["Delete Round" arbitools-delete-round]
    "---"
    ["List Players" arbitools-list-players]
    ["List Pairings" arbitools-list-pairing]
    "---"
    ["Update Elo" arbitools-update]
    ["Get It3 form Report" arbitools-it3]
    ["Get FEDA Rating file" arbitools-fedarating]
    "---"
    ["Prepare for FEDA" arbitools-prepare-feda]
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
