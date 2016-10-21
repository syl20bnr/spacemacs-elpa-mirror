;;; gobgen.el --- Generate GObject descendants using a detailed form -*- lexical-binding: nil; -*-

;; Copyright (C) 2015 Gergely Polonkai

;; Author: Gergely Polonkai <gergely@polonkai.eu>
;; Keywords: gobject, glib, gtk, helper, utilities
;; Package-Version: 20161020.823
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generator code is highly based on Gustavo Sverzut Barbieri's
;; gobject-class.el

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'subr-x)

(defvar-local gobgen-widget-name nil
  "Widget for the class name.")
(defvar-local gobgen-widget-prefix nil
  "Widget for the class prefix. It is auto-filled, but changeable.")
(defvar-local gobgen-widget-parent-name nil
  "Widget for the name of the parent class.")
(defvar-local gobgen-widget-parent-prefix nil
  "Widget for the prefix of the parent class. It is auto-filled, but
  changeable.")
(defvar-local gobgen-widget-recent nil
  "Checkbox field for the recent GLib option.")
(defvar-local gobgen-widget-private nil
  "Checkbox field for the private structure option.")

(defun gobject-get-prefix (class-name)
  "Guess the GObject prefix from CLASS-NAME."

  (car (split-string class-name "_")))

(defun gobgen-gen-header (class-full-name-upper
                          class-prefix-upper
                          class-name-upper
                          class-full-name-camel
                          func-prefix
                          parent-prefix-snake
                          parent-prefix-camel
                          parent-name-camel
                          parent-header
                          recent-glib
                          need-private)
  "Generate the contents of a GObject header file.

CLASS-FULL-NAME-UPPER is the full GObject name of the new class in
UPPER_SNAKE_CASE notation.

CLASS-PREFIX-UPPER is the GObject prefix of the new class in
UPPER_SNAKE_CASE notation.

CLASS-NAME-UPPER is the name of the new class without the
prefix in UPPER_SNAKE_CASE notation.

CLASS-FULL-NAME-CAMEL is the full name of the new object in
CamelCase notation.

FUNC-PREFIX is the function prefix for object methods.  Usually
it is the same as the full object name in lower_snake_case
notation.

PARENT-PREFIX-SNAKE is the prefix of the parent object in
snake_case notation.

PARENT-PREFIX-CAMEL is the prefix of the parent object in
CamelCase notation.

PARENT-NAME-CAMEL is the name of the parent class without the
prefix, in CamelCase notation.

PARENT-HEADER is the name of the header file to be included in
order to use the parent class.

If RECENT-GLIB is 't', some features available from GLib 2.38
will be used.

If NEED-PRIVATE is 't', a private struct will be added to the new
class."

  (concat
   "#ifndef __"
   class-full-name-upper
   "_H__\n"

   "#define __"
   class-full-name-upper
   "_H__\n"

   "\n"

   (if (string-equal "g" parent-prefix-snake)
       "#include <glib-object.h>"
     (if (string-equal "gtk" parent-prefix-snake)
         "#include <gtk/gtk.h>"
       (concat "// You might want to revise this\n"
               "#include <"
               parent-header
               ">")))
   "\n"

   "\n"

   "G_BEGIN_DECLS\n"

   "\n"

   "#define " class-prefix-upper "_TYPE_" class-name-upper "         (" func-prefix "_get_type())\n"

   "#define " class-full-name-upper "(o)           (G_TYPE_CHECK_INSTANCE_CAST((o), " class-prefix-upper "_TYPE_" class-name-upper ", " class-full-name-camel "))\n"

   "#define " class-full-name-upper "_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), " class-prefix-upper "_TYPE_" class-name-upper ", " class-full-name-camel "Class))\n"

   "#define " class-prefix-upper "_IS_" class-name-upper "(o)        (G_TYPE_CHECK_INSTANCE_TYPE((o), " class-prefix-upper "_TYPE_" class-name-upper "))\n"

   "#define " class-prefix-upper "_IS_" class-name-upper "_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE((k), " class-prefix-upper "_TYPE_" class-name-upper "))\n"

   "#define " class-full-name-upper"_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS((o), " class-prefix-upper "_TYPE_" class-name-upper ", " class-full-name-camel "Class))\n"

   "\n"

   "typedef struct _" class-full-name-camel "      " class-full-name-camel ";\n"

   "typedef struct _" class-full-name-camel "Class " class-full-name-camel "Class;\n"

   (if (and (not recent-glib) need-private)
       (concat "typedef struct _" class-full-name-camel "Private " class-full-name-camel "Private;\n"))

   "\n"

   "struct _" class-full-name-camel " {\n"

   "    /* Parent instance structure */\n"

   "    " parent-prefix-camel parent-name-camel " parent_instance;\n"

   "\n"

   "    /* Instance members */\n"

   (if (and (not recent-glib) need-private)
       (concat "\n"
               "    /*< private >*/\n"
               "    " class-full-name-camel "Private *priv;\n"))

   "};\n"

   "\n"

   "struct _" class-full-name-camel "Class {\n"

   "    " parent-prefix-camel parent-name-camel "Class parent_class;\n"

   "};\n"

   "\n"

   "GType " func-prefix "_get_type(void) G_GNUC_CONST;\n"

   "\n"

   "G_END_DECLS\n"

   "\n"

   "#endif /* __"
   class-full-name-upper
   "_H__ */\n"))

(defun gobgen-gen-code (class-full-name-upper
                        class-prefix-upper
                        class-name-upper
                        class-name-snake
                        class-full-name-camel
                        func-prefix
                        file-name-header
                        parent-prefix-upper
                        parent-name-upper
                        recent-glib
                        need-private)
  "Generate the contents of a GObject source file.

CLASS-FULL-NAME-UPPER is the full GObject name of the new class in
UPPER_SNAKE_CASE notation.

CLASS-PREFIX-UPPER is the GObject prefix of the new class in
UPPER_SNAKE_CASE notation.

CLASS-NAME-UPPER is the name of the new class without the
prefix in UPPER_SNAKE_CASE notation.

CLASS-NAME-SNAKE is the name of the new class without the prefix
in snake_case notation.

CLASS-FULL-NAME-CAMEL is the full name of the new object in
CamelCase notation.

FUNC-PREFIX is the function prefix for object methods.  Usually
it is the same as the full object name in lower_snake_case
notation.

FILE-NAME-HEADER is the name of the header file generated for the
new class.

PARENT-PREFIX-UPPER is the prefix of the parent object in
UPPER_SNAKE_CASE notation.

PARENT-NAME-UPPER is the name of the parent class without the
prefix in UPPER_SNAKE_CASE notation.

If RECENT-GLIB is 't', some features available from GLib 2.38
will be used.

If NEED-PRIVATE is 't', a private struct will be added to the new
class."

  (concat
   "#include \"" file-name-header "\"\n"

   "\n"

   (if need-private
       (concat
        (if (not recent-glib)
            (concat
             "#define " class-full-name-upper "_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE( \\\n"
             "            (o), \\\n"
             "            " class-prefix-upper "_TYPE_" class-name-upper ", \\\n"
             "            " class-full-name-camel "Private \\\n"
             "        ))\n"
             "\n"))

        (if recent-glib "typedef ")

        "struct _" class-full-name-camel "Private {\n"
        "    /* TODO: You must add something here, or GLib will produce warnings! */\n"
        "}"

        (if recent-glib
            (concat " " class-full-name-camel "Private"))

        ";\n"
        "\n"))

   "G_DEFINE_TYPE"

   (if (and recent-glib need-private)
       "_WITH_PRIVATE")

   "(" class-full-name-camel ", " func-prefix ", " parent-prefix-upper "_TYPE_" parent-name-upper ");\n"

   "\n"

   "static void\n"
   func-prefix "_finalize(GObject *gobject)\n"
   "{\n"
   "    g_signal_handlers_destroy(gobject);\n"
   "    G_OBJECT_CLASS(" func-prefix "_parent_class)->finalize(gobject);\n"
   "}\n"

   "\n"

   "static void\n"
   func-prefix "_class_init(" class-full-name-camel "Class *klass)\n"
   "{\n"
   "    GObjectClass *gobject_class = G_OBJECT_CLASS(klass);\n"
   "\n"

   (if (and (not recent-glib) need-private)
       (concat
        "    g_type_class_add_private(klass, sizeof(" class-full-name-camel "Private));\n"
        "\n"))

   "    gobject_class->finalize = " func-prefix "_finalize;\n"

   "}\n"

   "\n"

   "static void\n"
   func-prefix "_init(" class-full-name-camel " *" class-name-snake ")\n"
   "{\n"

   (if (and (not recent-glib) need-private)
       (concat
        "    " class-name-snake "->priv = " class-full-name-upper "_GET_PRIVATE(" class-name-snake ");\n"))

   "}\n"))

(defun gobgen-generator (class-prefix
                         class-name
                         parent-prefix
                         parent-name
                         recent-glib
                         need-private)
  "Generate the boilerplate of a new GObject derived class.

CLASS-PREFIX is the prefix of the new class.

CLASS-NAME is the name of the new class, without the prefix.

PARENT-PREFIX is the prefix of the parent class.

PARENT-NAME is the name of the parent class, without the prefix.

If RECENT-GLIB is 't', some features available from GLib 2.38
will be used.

If NEED-PRIVATE is 't', a private struct will be added to the new
class."

  (let* ((parent-prefix        (downcase parent-prefix))
         (parent-name          (downcase parent-name))
         (class-prefix         (downcase class-prefix))
         (class-name           (downcase class-name))
         (parent-prefix-length (length parent-prefix))
         (class-prefix-length  (length class-prefix)))

    (if (not (string-prefix-p (concat parent-prefix "_") parent-name))
        (message (concat "Parent (" parent-name ") and parent prefix (" parent-prefix ") don't match"))

      (if (not (string-prefix-p (concat class-prefix "_") class-name))
          (message (concat "Class (" class-name ") and class prefix (" class-prefix ") don't match"))

        (let* ((parent-name (substring parent-name (+ parent-prefix-length 1)))
               (class-name  (substring class-name (+ class-prefix-length 1)))
               (parent-prefix-pcs (split-string parent-prefix "_"))
               (parent-name-pcs   (split-string parent-name "_"))
               (class-prefix-pcs  (split-string class-prefix "_"))
               (class-name-pcs    (split-string class-name "_"))
               (parent-prefix-snake     (string-join parent-prefix-pcs "_"))
               (parent-prefix-camel      (mapconcat 'capitalize parent-prefix-pcs ""))
               (parent-prefix-upper     (upcase parent-prefix-snake))
               (parent_name       (string-join parent-name-pcs "_"))
               (parent-name-camel        (mapconcat 'capitalize parent-name-pcs ""))
               (parent-name-upper       (upcase parent_name))
               (class_prefix      (string-join class-prefix-pcs "_"))
               (ClassPrefix       (mapconcat 'capitalize class-prefix-pcs ""))
               (class-prefix-upper      (upcase class_prefix))
               (class-name-snake        (string-join class-name-pcs "_"))
               (ClassName         (mapconcat 'capitalize class-name-pcs ""))
               (class-name-upper        (upcase class-name-snake))
               (func-prefix       (concat class_prefix "_" class-name-snake))
               (class-full-name-camel     (concat ClassPrefix ClassName))
               (class-full-name-upper   (concat class-prefix-upper "_" class-name-upper))
               (parent-header     (concat (string-join (append parent-prefix-pcs parent-name-pcs) "-") ".h"))
               (file-name-base    (string-join (append class-prefix-pcs class-name-pcs) "-"))
               (file-name-code    (concat file-name-base ".c"))
               (file-name-header  (concat file-name-base ".h")))

          (delete-other-windows)
          (split-window-vertically)
          (other-window 1)
          (find-file file-name-header)
          (insert (gobgen-gen-header class-full-name-upper
                                     class-prefix-upper
                                     class-name-upper
                                     class-full-name-camel
                                     func-prefix
                                     parent-prefix-snake
                                     parent-prefix-camel
                                     parent-name-camel
                                     parent-header
                                     recent-glib
                                     need-private))

          (split-window-vertically)
          (other-window 1)
          (find-file file-name-code)
          (insert (gobgen-gen-code class-full-name-upper
                                   class-prefix-upper
                                   class-name-upper
                                   class-name-snake
                                   class-full-name-camel
                                   func-prefix
                                   file-name-header
                                   parent-prefix-upper
                                   parent-name-upper
                                   recent-glib
                                   need-private)))))))

;;;###autoload
(defun gobgen ()
  "Create widgets window for GObject creation."

  (interactive)

  (switch-to-buffer "*GObject Creator*")

  (kill-all-local-variables)

  (let ((inhibit-read-only t))
    (erase-buffer))

  (remove-overlays)

  (widget-insert "GObject Creator\n\n")

  (widget-insert "Generate a GObject class skeleton.\n\n")

  (setq gobgen-widget-name
        (widget-create 'editable-field
                       :size 25
                       :format "Name:   %v"
                       :notify (lambda (widget _child &optional event)
                                 (save-excursion
                                   (widget-value-set gobgen-widget-prefix
                                                     (gobject-get-prefix (widget-value widget)))))
                       :doc "The name of the new class, with its prefix included"
                       "gtk_example_object"))

  (widget-insert " ")

  (setq gobgen-widget-prefix
        (widget-create 'editable-field
                       :size 10
                       :format "Prefix: %v\n"
                       :doc "Prefix of the new class. It updates automatically based on the name, so unless you need a namespace that consists of multiple parts (like my_ns), you should not touch this."
                       "gtk"))

  (setq gobgen-widget-parent-name
        (widget-create 'editable-field
                       :size 25
                       :format "Parent: %v"
                       :notify (lambda (widget _child &optional event)
                                 (save-excursion
                                   (widget-value-set gobgen-widget-parent-prefix
                                                     (gobject-get-prefix (widget-value widget)))))
                       :doc "Name of the parent class. Use g_object if you don't want to derive from something specific."
                       "g_object"))

  (widget-insert " ")

  (setq gobgen-widget-parent-prefix
        (widget-create 'editable-field
                       :size 10
                       :format "Prefix: %v\n"
                       :doc "Prefix of the parent class. Its automatically set value should suffice most of the time"
                       "g"))

  (widget-insert "\n")

  (setq gobgen-widget-recent
        (widget-create 'checkbox
                       :doc "Use recent GLib's features, like defining a class with a private struct. Usually you would want this on."
                       t))

  (widget-insert " GLib >= 2.38\n")

  (setq gobgen-widget-private
        (widget-create 'checkbox
                       :doc "Add a private struct for the object."
                       nil))

  (widget-insert " Has private members\n")

  (widget-insert "\n\n")

  (widget-create 'push-button
                 :notify (lambda (widget _child &optional event)
                           (let ((class-name    (widget-value gobgen-widget-name))
                                 (class-prefix  (widget-value gobgen-widget-prefix))
                                 (parent-name   (widget-value gobgen-widget-parent-name))
                                 (parent-prefix (widget-value gobgen-widget-parent-prefix))
                                 (recent-glib   (widget-value gobgen-widget-recent))
                                 (need-private  (widget-value gobgen-widget-private)))
                             (gobgen-generator class-prefix
                                               class-name
                                               parent-prefix
                                               parent-name
                                               recent-glib
                                               need-private)))
                 "Generate")

  (widget-insert " ")

  (widget-create 'push-button
                 :notify (lambda (widget _child &optional event)
                           (widget-value-set gobgen-widget-name "gtk_example_object")
                           (widget-value-set gobgen-widget-prefix "gtx")
                           (widget-value-set gobgen-widget-parent-name "g_object")
                           (widget-value-set gobgen-widget-parent-prefix "g")
                           (widget-value-set gobgen-widget-recent t)
                           (widget-value-set gobgen-widget-private nil))
                 "Reset form")

  (widget-insert " ")

  (widget-create 'push-button
                 :notify (lambda (widget _child &optional event)
                           (kill-buffer "*GObject Creator*"))
                 "Close")

  (goto-char (point-min))

  (use-local-map widget-keymap)
  (widget-setup))

(provide 'gobgen)

;;; gobgen.el ends here
