shell-split-string.el provides one function `shell-split-string'.
This function splits strings using shell-like syntax:
(shell-split-string "abc")  ->  '("abc")
(shell-split-string "abc  def")  ->  '("abc" "def")
(shell-split-string "abc \"def ghi\"")  ->  '("abc" "def ghi")
(shell-split-string "abc 'de f'ghi") -> '("abc" "de fghi")
(shell-split-string "abc \"d\\\"ef\"") -> '("abc" "d\"ef")
