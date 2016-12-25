   Extensions to `replace.el'.

 Commands defined here:

   `occur-unhighlight-visited-hits', `query-replace-w-options',
   `toggle-replace-w-completion',
   `toggle-search/replace-region-as-default'.

 Faces defined here:

   `occur-highlight-linenum', `replacep-msg-emphasis',
   `replacep-msg-emphasis2'.

 User options defined here:

   `replace-w-completion-flag',
   `search/replace-region-as-default-flag',
   `search/replace-2nd-sel-as-default-flag',
   `search/replace-default-fn'.

 Non-interactive functions defined here:

    `replacep-propertize', `replacep-remove-property',
    `replacep-string-match-p.', `search/replace-default',
    `usable-region'.

 Internal variable defined here:

   `occur-regexp', `occur-searched-buffers'.


 ***** NOTE: The following functions defined in `replace.el' have
             been REDEFINED or ADVISED HERE:

   `flush-lines' - (Not needed for Emacs 21)
                   1. The prompt mentions that only lines after
                      point are affected.
                   2. The default input is provided by
                      `search/replace-region-as-default-flag' or
                      `search/replace-2nd-sel-as-default-flag' or
                      `search/replace-default-fn', in that order.
                   3. An in-progress message has been added.
   `how-many' - (Not needed for Emacs 21)
                1. Prompt mentions tlines after point are affected.
                2. The default input is provided by
                   `search/replace-region-as-default-flag' or
                   `search/replace-2nd-sel-as-default-flag' or
                   `search/replace-default-fn', in that order.
                3. An in-progress message has been added.
   `keep-lines' - Same as `flush-lines'. (Not needed for Emacs 21)
   `occur' - Default from `search/replace-region-as-default-flag'
             or `search/replace-2nd-sel-as-default-flag'
             or `search/replace-default-fn' (Emacs 20 only)
   `occur', `multi-occur', `multi-occur-in-matching-buffers' -
             Regexp is saved as `occur-regexp' for use by
             `occur-mode-mouse-goto'
   `occur-engine' - Save list of searched buffers in
                    `occur-searched-buffers' (Emacs 22+)
   `occur-mode-goto-occurrence', `occur-mode-display-occurrence',
   `occur-mode-goto-occurrence-other-window',
   `occur-mode-mouse-goto' - Highlight regexp in source buffer
                             and visited linenum in occur buffer.
   `occur-read-primary-args' - (Emacs 21 only) Default regexps via
                               `search/replace-default'.
   `query-replace', `query-replace-regexp', `replace-string',
     `replace-regexp'        - No " in region" in prompt if
                               `*-region-as-default-flag'.
   `query-replace-read-args' - 1. Uses `completing-read' if
                                  `replace-w-completion-flag' is
                                  non-nil.
                               2. Default regexps are obtained via
                                  `search/replace-default'.
                               3. Deactivates region if
                                  `*-region-as-default-flag'.
   `query-replace-read-(from|to)' - Like `query-replace-read-args',
                                    but for Emacs 21+.
   `read-regexp' (Emacs 23-24.2) -
                       1. Allow DEFAULTS to be a list of strings.
                       2. Prepend DEFAULTS to the vanilla defaults.

   `replace-highlight' (Emacs 24.4+) - Highlight regexp groups, per
                     `isearchp-highlight-regexp-group-levels-flag'.
   `replace-dehighlight' (Emacs 24.4+) - Dehighlight regexp groups.


 This file should be loaded after loading the standard GNU file
 `replace.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "replace" '(progn (require 'replace+)))

 For Emacs releases prior to Emacs 22, these Emacs 22 key bindings
 are made here:

  (define-key occur-mode-map "o" 'occur-mode-goto-occurrence-other-window)
  (define-key occur-mode-map "\C-o" 'occur-mode-display-occurrence))

 Suggested additional key binding:

  (substitute-key-definition 'query-replace 'query-replace-w-options
                             global-map)

 If you want the highlighting of regexp matches in the searched
 buffers to be removed when you quit occur or multi-occur, then add
 function `occur-unhighlight-visited-hits' to an appropripate hook.
 For example, to have this happen when you kill the occur buffer,
 add it to `kill-buffer-hook':

   (add-hook 'kill-buffer-hook 'occur-unhighlight-visited-hits)
