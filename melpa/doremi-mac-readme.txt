 A macro for defining Do Re Mi commands.

 Defines a Do Re Mi command and adds it to a `Do Re Mi' menu-bar
 menu.  See library `doremi.el'.


User options defined here:

   `define-doremi-after-hook', `define-doremi-before-hook'.

 Macro defined here:

   `define-doremi'.


 Add this to your initialization file (~/.emacs or ~/_emacs):

   (autoload 'define-doremi "doremi-mac"
     "Define a Do Re Mi command." nil 'macro)


 See also these other Do Re Mi libraries:

   `doremi-frm.el' - Incrementally adjust frame properties.
   `doremi-cmd.el' - Other Do Re Mi commands.


 Example test commands defined using the macro.

 1. Command `doremi-frame-height+' sets the frame height.

    (define-doremi frame-height+
      "Set frame height, changing it incrementally."   ; Doc string
      "Set Frame Height"                        ; Command menu name
      (lambda (new-val)                         ; Setter function
        (set-frame-height (selected-frame) new-val) new-val)
      (frame-height (selected-frame)))          ; Initial value

 2. Command `doremi-set-bg+' cycles through
    (x-defined-colors), setting the background color.

    (define-doremi set-bg+
      ;; Doc string
      "Set background color, choosing from a list of all colors."
      "Set Background Color"                    ; Command menu name
      ;; Setter function
      (lambda (newval) (set-background-color newval) newval)
      ;; Initial value
      (frame-parameter (selected-frame) 'background-color)
      nil                                       ; Ignored
      (x-defined-colors)                        ; Cycle enumeration
      t)    ; Add current color to enumeration if not there already

    Command `doremi-set-bg+' runs this hook after running `doremi':
(setq define-doremi-after-hook
      ;; Update the way faces display with new bg.
      (lambda () (frame-set-background-mode (selected-frame))))
