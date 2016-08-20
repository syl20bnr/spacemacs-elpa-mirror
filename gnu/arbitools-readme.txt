REQUIRES:
---------------------------
Some functions require the arbitools python package, you can install
it by: "pip3 install arbitools"
"pdflatex" is necessary in case you want to get pdfs.

USAGE:
---------------------------
arbitools.el is an interface for the python package "arbitools",
designed to manage chess tournament reports.  If you don't install the
python package you can still have the syntax colouring and some native
functions. In the future, all the functions will be translated to ELISP.

FEATURES:
----------------------------
- Syntax colouring for the official trf FIDE files.  This facilitates
manual edition of the files.

- Updating the players ratings. - with python

- Adding players to an existing file. - with python

- Getting standings from a tournament file. -with python

- Getting IT3 Tournament report form. - with python

- Deleting a round. - Native

- Insert result. - Native

- Insert player. - Native

- Get the pairing or results of a round - Native

- Get the list of the players - Native

- Delete player. Adjust all rank numbers - Native

- Adjust points for each player, according to results of rounds - Native

- Print standings - Native

TODO:
---------------------------------

- Automatically purge all players who didn't play any games.

- Insert results from a results file created with a pairing program.
  Add the date in the "132" line and the results in the "001" lines.

- Add empty round. Ask for date create empty space in the players lines.
  Add the date in the "132" line.

- Add the rank number and the position automatically when adding players.

- Add team.

- Add player to team. Prompt for team and player number.

- Generate pgn file for a round or the whole tournament.

- Reorder the ranking

- Reorder the players list

You will find more information in www.ourenxadrez.org/arbitools.htm