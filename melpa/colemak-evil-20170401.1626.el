;;; colemak-evil.el --- Colemak-friendly keybindings for Evil.

;; Copyright 2012-2017 Patrick Brinich-Langlois <pbrinichlanglois@gmail.com>

;; Author: Patrick Brinich-Langlois <pbrinichlanglois@gmail.com>
;; Version: 2.0.0
;; Package-Version: 20170401.1626
;; URL: https://github.com/patbl/colemak-evil
;; Package-Requires: ((evil "20170323.1140"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Colemak Evil
;; ============
;;
;; Colemak Evil is a set of remappings that implements some of
;; Shai Coleman's awesome Vim remappings in Emacs
;; ([more information](http://forum.colemak.com/viewtopic.php?id=50)).
;;
;; Here are the main differences from Shai's mappings:
;;
;; * The only Vim mapping that works in insert mode is Esc (this avoids
;;   conflicts with Emacs's shortucts). Tab in insert mode doesn't take
;;   you into normal mode.
;; * Folding and several other features aren't implemented.
;;
;; Setup
;; -----
;;
;; You can install Colemak Evil (as `colemak-evil`) from the MELPA repository.
;; Once it's installed, add the following to your `.emacs` file:
;;
;;     (require 'colemak-evil)
;;
;; Tips
;; ----
;;
;; Type :hints (or just :h) to bring up the hint screen.
;;
;; Escape takes you into normal mode, but you may find that defining your
;; own key combination using
;; [Key Chord](http://www.emacswiki.org/emacs/key-chord.el) to be more
;; comfortable. The only adjacent home-row combinations that are
;; relatively uncommon in English "hn" and "td." If you find yourself
;; unintentionally entering normal mode when typing quickly, you might
;; try reducing the key delay:
;;
;;     (key-chord-define-global "td" 'evil-normal-state)
;;     (setq key-chord-two-keys-delay .01)
;;
;; If this doesn't work, you can use the spacebar as one of the keys:
;;
;;     (key-chord-define-global " e" 'evil-normal-state)
;;
;; There are also some Vim features that haven't yet been implemented in
;; Evil. You'll probably have to add quite a few of your own mappings to
;; get your setup where you want it. For insert-mode mappings, check out
;; [ErgoEmacs](http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html),
;; which provides saner alternatives to Emacs's mappings (there's a
;; Colemak version).
;;
;; An Alternative
;; --------------
;;
;; [Lalopmak Evil](https://github.com/lalopmak/lalopmak-evil), another
;; set of Emacs mappings based on Shai's Vim layout, "takes some of
;; Shai's ideas even further." If you're used to and happy with Shai's
;; mappings, you'll probably be satisfied with Colemak Evil. But if
;; you're just starting out or you're an efficiency fanatic, Lalopmak
;; Evil may be the better choice.
;;
;;; Code:
(require 'evil)

(defvar colemak-evil-hintstring "Hints for colemak-evil.  Accessed via: :hints, :h, :ars, or M-x colemak-evil-hints.

To dismiss: retype one of the above commands or press q in the buffer.

NOTE/CREDITS: These hints were originally created by DreymaR for golemak.vim (http://forum.colemak.com/viewtopic.php?pid=6789#p6789).
Though most should have been corrected, some may still not be valid for colemak-evil.el.

Normal mode:
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
|~ Case    |! ExtFlt> |@ PlyMcr· |#  <-=    |$  ->|    |% GoMatch |^  <--    |& Rep :s  |*  =->    |( |<-Sent |) Sent->| |_ LastLin |+ Next<-- |
|` Go Mk·  |1         |2         |3         |4         |5         |6         |7         |8         |9         |0  |<-    |- TopLine |= Format> |
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
|          |  Quit    |          |          |          |          |          |          |          |          |          |          |          |
|  NextTab |  =<C-v>  |  WinCmd  |  GUIFind |  =<Up>   |Abort cmd |          |   <--    |  ScrlUp  |   ->|    |          |          |          |
| <TAB>    |Q PlyMcrQ |W ChangeLn|F <-Find· |P <-Prch· |G ScrMid  |J JoinLine|L <-WORD  |U  5Up    |Y WORD->  |; z-Cmd·  |{ |<-Para |} Para->| |
| <TAB>    |  RecMcr· |  Change  |  Find·-> |  Prch·-> |  g-Cmd·  |  PgUp    |  <-word  |    Up    |  word->  |: z-Cmd·  |[ <-Misc· |] Misc·-> |
+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
 Meta----->|          |          |          |          |          |          |          |          |          |          |          |          |
 Ctrl----->|  AreaAll |  Redo    |  Search  |          |  DelWord |          |          |  ScrlDwn |          |          |          |          |
 Shift---->|A AreaLin |R RepMode |S |<-Ins  |T Att->|  |D Del->|  |H TopPage |N 5Left   |E  5Dn    |I 5Right  |O OpenUp  |\" SetReg· || GoCol1  |
 Normal--->|  Area    |  Replce· |  InSert  |  ATtach  |  Delete> |  PgDn    |   Left   |    Dn    |   Right  |  OpenDn  |' GoMk·|< |\\ (usr)·  |
           +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+
           |          |          |          |          |          |          |          |          |          |          |
  Ctrl+:   |          |          |          | VisBlock |          |  Digraph |  =<CR>   |          |          |          |    · = char arg.
Up/Dn scrl |Z Redo    |X <-Cut   |C CopyLin |V <-Paste |B RevFndCh|K <-Next§ |M ScrMid  |< Unindt> |> Indent> |? <-Find§ |    > = move arg.
PgUp/Dn HL |  Undo    |  Cut->   |  Copy >  |  Paste-> |  RepFndCh|  Next§-> |  Set Mk· |, (usr)·  |. Repeat  |/ Find§-> |
           +----------+----------+----------+----------+----------+----------+----------+----------+----------+----------+

====Commands====

Help:
:hints = shows/dismisses this prompt (M-x colemak-evil-hints)
:key = describes key (C-h k)
:fun = describes function (C-h f)

Shortcuts:
:comment = :c = M-x comment-or-uncomment-region
:git = M-x magit-status
:eval = :ev = Evaluates an elisp expression (C-:)

")

(defun colemak-evil-hints ()
  "Provides hints about this configuration, or closes said hints."
  (interactive)
  (let* ((hints-buffer-name "Colemak-Evil Hints")
	 (hints-buffer (get-buffer hints-buffer-name) ) )
    ;;if hints are currently visible, close them. Otherwise, display them.
    (if (and hints-buffer
	     (get-buffer-window hints-buffer))
	(progn (delete-windows-on hints-buffer-name)
	       (kill-buffer hints-buffer-name))
      (with-output-to-temp-buffer hints-buffer-name
	(princ colemak-evil-hintstring)))))


;; remove all keybindings from insert-state keymap
(setcdr evil-insert-state-map nil)
;; but [escape] should switch back to normal state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(defun set-in-evil-states (key def maps)
  (while maps
    (define-key (pop maps) key def)))

(defun set-in-navigation-evil-states (key def)
  (set-in-evil-states key def (list evil-motion-state-map
                                    evil-normal-state-map
                                    evil-visual-state-map)))

;;; No insert-state alt-navigation remappings (they would clobber
;;; Emacs shortcuts, and Emacs has its own navigation commands that
;;; you can use.

;;; Up/down/left/right
(set-in-navigation-evil-states "u" 'evil-previous-line)
(set-in-navigation-evil-states "e" 'evil-next-line)
(set-in-navigation-evil-states "n" 'evil-backward-char)
(set-in-navigation-evil-states "i" 'evil-forward-char)
(define-key evil-operator-state-map "i" 'evil-forward-char)

;;; Turbo navigation mode
(set-in-navigation-evil-states "I" '(lambda () (interactive) (evil-forward-char 5)))
(set-in-navigation-evil-states "N" '(lambda () (interactive) (evil-backward-char 5)))
(set-in-navigation-evil-states "E" '(lambda () (interactive) (evil-next-line 5)))
(set-in-navigation-evil-states "U" '(lambda () (interactive) (evil-previous-line 5)))

;;; Beginning/end of line (home/end)
;; Use back-to-indentation instead of evil-beginning-of-line so that
;; cursor ends up at the first non-whitespace character of a line. 0
;; can be used to go to real beginning of line
(set-in-navigation-evil-states "L" 'back-to-indentation)
(set-in-navigation-evil-states "Y" 'evil-end-of-line)

;;; Page up/page down
(define-key evil-motion-state-map (kbd "j") 'evil-scroll-page-up)
(define-key evil-motion-state-map (kbd "h") 'evil-scroll-page-down)

;;; Page halfway up/down
(set-in-navigation-evil-states "\C-u" 'evil-scroll-up)
(set-in-navigation-evil-states "\C-e" 'evil-scroll-down)

;;; Jump to line
;; Redundant with gg and G
;; (set-in-navigation-evil-states "-" 'evil-goto-first-line)
;; (set-in-navigation-evil-states "_" 'evil-goto-line)

;;; Words forward/backward
(set-in-navigation-evil-states "l" 'evil-backward-word-begin)
(set-in-navigation-evil-states "y" 'evil-forward-word-begin)
;;; WORD forward/backward
(set-in-navigation-evil-states (kbd "C-y") 'evil-forward-WORD-begin)
(set-in-navigation-evil-states (kbd "C-l") 'evil-backward-WORD-begin)

;;; inneR text objects
(define-key evil-visual-state-map "r" evil-inner-text-objects-map)
(define-key evil-operator-state-map "r" evil-inner-text-objects-map)
(define-key evil-inner-text-objects-map "y" 'evil-inner-word)
(define-key evil-inner-text-objects-map "Y" 'evil-inner-WORD)

;; Execute command: map : to ;
(define-key evil-motion-state-map ";" 'evil-ex)

;;; Word end forward/backward
;; (set-in-navigation-evil-states ";" 'evil-forward-word-end)
;; (set-in-navigation-evil-states "g;" 'evil-backward-word-end)

;;; Folds, etc.
;; (define-key evil-normal-state-map ",o" 'evil-open-fold)
;; (define-key evil-normal-state-map ",c" 'evil-close-fold)
;; (define-key evil-normal-state-map ",a" 'evil-toggle-fold)
;; (define-key evil-normal-state-map ",r" 'evil-open-folds)
;; (define-key evil-normal-state-map ",m" 'evil-close-folds)

;;; I'm not sure what this is
;; for virtualedit=onemore
;; set virtualedit=block,onemore

;;; Cut/copy/paste
(set-in-navigation-evil-states "x" 'evil-delete-char)
(set-in-navigation-evil-states "X" 'evil-delete-line)  ; delete to end of line; use dd to delete whole line
(set-in-navigation-evil-states "c" 'evil-yank)
(set-in-navigation-evil-states "C" 'evil-yank-line)
(set-in-navigation-evil-states "v" 'evil-paste-before)
(set-in-navigation-evil-states "V" 'evil-paste-after)

;;; Change
(set-in-navigation-evil-states "w" 'evil-change)
(set-in-navigation-evil-states "W" 'evil-change-line)

;;; Undo/redo
(define-key evil-normal-state-map "z" 'undo)
(when (fboundp 'undo-tree-undo)
  (define-key evil-normal-state-map "z" 'undo-tree-undo)
  (define-key evil-normal-state-map "Z" 'undo-tree-redo))

;;; Break undo chain
;; not sure what this is

;;; Cursor position jumplist
(set-in-navigation-evil-states "(" 'evil-jump-backward)
(set-in-navigation-evil-states ")" 'evil-jump-forward)

;;; Start/end of document
;; How is this different from gg/G?
;; C-h would interfere with the useful C-h f/v/k commands
;; (set-in-navigation-evil-states "\C-j" '(lambda () (interactive)
					     ;; (goto-char (point-min))))
;; (set-in-navigation-evil-states "\C-h" '(lambda () (interactive)
					     ;; (goto-char (point-max))))

;;; inSert/Replace/Append
(set-in-navigation-evil-states "s" 'evil-insert)
(set-in-navigation-evil-states "S" 'evil-insert-line)
(set-in-navigation-evil-states "t" 'evil-append)
(set-in-navigation-evil-states "T" 'evil-append-line)

;;; Make insert/add work also in visual line mode like in visual block mode
;; not sure what this means

;;; Visual mode
(set-in-navigation-evil-states "a" 'evil-visual-char)
(set-in-navigation-evil-states "A" 'evil-visual-line)
(set-in-navigation-evil-states "\C-a" 'mark-whole-buffer)

;;; visual Block mode
;; Since the system clipboard is accessible by Emacs through the
;; regular paste command (v), a separate C-v mapping isn't needed.
;; (define-key evil-motion-state-map "\C-b" 'evil-visual-block)

;;; Allow switching from visual line to visual block mode
;; not implemented

;;; Visual mode with mouse
;; not implemented
;;; Insert literal
;; not implemented

;;; Search
;; f unchanged
;; F unchanged
(set-in-navigation-evil-states "p" 'evil-find-char-to)
(set-in-navigation-evil-states "P" 'evil-find-char-to-backward)

;;; GUI search
;; not implemented

;;; Redraw screen
;; not implemented

;;; Tabs
;; Who needs tabs? Use iswitchb instead. Put (iswitchb-mode 1) in your
;; .emacs and use C-x b to search for the buffer you want. C-s and C-r
;; rotate through the listed buffers

;;; New/close/save
;; these might conflict with emacs mappings


(set-in-navigation-evil-states "J" 'evil-join)

(set-in-navigation-evil-states "r" 'evil-replace)
(set-in-navigation-evil-states "R" 'evil-replace-state)

(define-key evil-motion-state-map (kbd "C-e") 'evil-scroll-line-down)
(define-key evil-motion-state-map (kbd "C-f") 'evil-scroll-page-down)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)
(define-key evil-motion-state-map (kbd "C-y") 'evil-scroll-line-up)

;;; Scroll in place
(define-key evil-motion-state-map (kbd "C-<up>") 'evil-scroll-line-up)
(define-key evil-motion-state-map (kbd "C-<down>") 'evil-scroll-line-down)

;;; Live line reordering
;; not implemented

;;; Restore mappings
;;; Free mappings: ,/+/H

;;; Macros
(define-key evil-normal-state-map "Q" '(lambda ()
					 (interactive)
					 (evil-execute-macro 1 last-kbd-macro)))

;;; Duplicate line
;; not implemented
;; Use "CV" instead

;;; Misc overridden keys must be prefixed with g
;; not implemented

;;; Search
(define-key evil-motion-state-map "k" 'evil-search-next)
(define-key evil-motion-state-map "K" 'evil-search-previous)

;;; Folding
;; (define-key evil-normal-state-map "zo" 'evil-open-fold)
;; (define-key evil-normal-state-map "zc" 'evil-close-fold)
;; (define-key evil-normal-state-map "za" 'evil-toggle-fold)
;; (define-key evil-normal-state-map "zr" 'evil-open-folds)
;; (define-key evil-normal-state-map "zm" 'evil-close-folds)

;;; Make the space, return, and backspace keys work in normal mode
;; Backspace in normal mode doesn't work in the terminal.
(define-key evil-motion-state-map " " (lambda () (interactive) (insert " ")))
(define-key evil-motion-state-map (kbd "RET") (lambda () (interactive) (newline)))
(define-key evil-motion-state-map (kbd "<backspace>") 'delete-backward-char)

;;; Visual line navigation
;; In normal mode, use "ge" and "gu" when lines wrap.
(set-in-navigation-evil-states "ge" 'evil-next-visual-line)
(set-in-navigation-evil-states "gu" 'evil-previous-visual-line)

;;; Window handling
;; C-w (not C-r as in Shai's mappings) prefixes window commands
(define-key evil-window-map "n" 'evil-window-left)
(define-key evil-window-map "N" 'evil-window-move-far-left)
(define-key evil-window-map "e" 'evil-window-down)
(define-key evil-window-map "E" 'evil-window-move-very-bottom)
(define-key evil-window-map "u" 'evil-window-up)
(define-key evil-window-map "U" 'evil-window-move-very-top)
(define-key evil-window-map "i" 'evil-window-right)
(define-key evil-window-map "I" 'evil-window-move-far-right)
(define-key evil-window-map "k" 'evil-window-new)

(define-key evil-normal-state-map (kbd "TAB")  'evil-indent)

(set-in-navigation-evil-states "o" 'evil-open-below)
(set-in-navigation-evil-states "O" 'evil-open-above)


;;allows you to use ; as :
(define-key evil-motion-state-map ";" 'evil-ex-read-command)

;;hooks for hints
(evil-ex-define-cmd "hints" 'colemak-evil-hints)

;;git
(evil-ex-define-cmd "git" 'magit-status)

;;comment
(evil-ex-define-cmd "comment" 'comment-or-uncomment-region)
(evil-ex-define-cmd "c" "comment")

;;M-:
(evil-ex-define-cmd "eval" 'eval-expression)
(evil-ex-define-cmd "ev" "eval")

;;C-h k
(evil-ex-define-cmd "describe-key" 'describe-key)
(evil-ex-define-cmd "key" "describe-key")

;;C-h f
(evil-ex-define-cmd "describe-function" 'describe-function)
(evil-ex-define-cmd "function" "describe-function")
(evil-ex-define-cmd "fun" "describe-function")

;;FRAGILE
;;Redefines visual updates so as to update the primary, rather than the clipboard, with the selection
;;This also allows you to select a region, copy from outside, then paste into the region
(defun evil-visual-update-x-selection (&optional buffer)
  "Update the X selection with the current visual region."
  (let ((buf (or buffer (current-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (evil-visual-state-p)
                   (fboundp 'x-select-text)
                   (or (not (boundp 'ns-initialized))
                       (with-no-warnings ns-initialized))
                   (not (eq evil-visual-selection 'block)))
          (x-set-selection 'PRIMARY (buffer-substring-no-properties
                                     evil-visual-beginning
                                     evil-visual-end))
          (setq x-last-selected-text-primary ))))))

(provide 'colemak-evil)

;;; colemak-evil.el ends here
