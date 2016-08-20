Bridge between anything and helm.

M-x `helm-anything-resume' replaces M-x `anything-resume' and M-x
`helm-resume'.

`helm-anything/funcall', `helm-anything/set' and
`helm-anything/get' handles helm or anything functions and
variables.

`helm-anything/define-key' binds key to `helm-map' and `anything-map'.

Installation:

Put helm-anything.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'helm-anything)
Replace helm-resume and anything-resume with helm-anything-resume
(helm-anything-set-keys)

No need more.
