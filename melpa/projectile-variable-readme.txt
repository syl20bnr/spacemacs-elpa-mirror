Store project local variables (property) using Projectile and Symbol Plists.
The name of this package has projectile- in the prefix, but it can now be executed without depending on it.


- https://github.com/bbatsov/projectile
- https://www.gnu.org/software/emacs/manual/html_node/elisp/Symbol-Plists.html

(projectile-variable-put 'foo-value 2) ;; Store property
(projectile-variable-get 'foo-value)   ;;=> 2

(projectile-variable-plist)        ;; Return all project local property list
(projectile-variable-plist "foo-") ;; Return project local property list filterd by prefix "foo-"
(projectile-variable-alist)        ;; Return all project local properties as association list (alist)
(projectile-variable-alist "foo-") ;; Return project local properties alist filterd by prefix "foo-"
