Provides window-local back and forward stacks for point.
Each stack value contains buffer reference, point position,
and window scroll position.
Both stacks are local to current window, in other words, stacks in
different windows are not related.

To navigate the stacks, bind back and forward commands to some keys:
(global-set-key (kbd "C-M-,") 'point-stack-pop)
(global-set-key (kbd "C-M-.") 'point-stack-forward-stack-pop)

The recommended usage is to save locations automatically on navigation:
(point-stack-setup-advices)

To push values to stack manually instead, bind the push command:
(global-set-key [f5] 'point-stack-push)

Based on https://github.com/mattharrison/point-stack
which is in turn based on http://www.emacswiki.org/emacs/JohnConnors
