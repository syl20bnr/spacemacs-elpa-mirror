;;; svg.el --- svg image creation functions

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Maintainer: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: image
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This pacakge allows creating SVG images in Emacs.  SVG images are
;; vector-based XML files, really, so you could create them directly
;; as XML.  However, that's really tedious, as there are some fiddly
;; bits.

;; In addition, the `svg-insert-image' function allows inserting an
;; SVG image into a buffer that's updated "on the fly" as you
;; add/alter elements to the image, which is useful when composing the
;; images.

;; Here are some usage examples:

;; Create the base image structure, add a gradient spec, and insert it
;; into the buffer:
;; (setq svg (svg-create 800 800 :stroke "orange" :stroke-width 5))
;; (svg-gradient svg "gradient" 'linear '(0 . "red") '(100 . "blue"))
;; (save-excursion (goto-char (point-max)) (svg-insert-image svg))

;; Then add various elements to the structure:
;; (svg-rectangle svg 100 100 500 500 :gradient "gradient" :id "rec1")
;; (svg-circle svg 500 500 100 :id "circle1")
;; (svg-ellipse svg 100 100 50 90 :stroke "red" :id "ellipse1")
;; (svg-line svg 100 190 50 100 :id "line1" :stroke "yellow")
;; (svg-polyline svg '((200 . 100) (500 . 450) (80 . 100))
;; 	      :stroke "green" :id "poly1")
;; (svg-polygon svg '((100 . 100) (200 . 150) (150 . 90))
;; 	     :stroke "blue" :fill "red" :id "gon1")

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'dom)

(defun svg-create (width height &rest args)
  "Create a new, empty SVG image with dimentions WIDTHxHEIGHT.
ARGS can be used to provide `stroke' and `stroke-width' parameters to
any further elements added."
  (dom-node 'svg
	    `((width . ,width)
	      (height . ,height)
	      (version . "1.1")
	      (xmlsn . "http://www.w3.org/2000/svg")
	      ,@(svg-arguments nil args))))

(defun svg-gradient (svg id type &rest stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `gradient'.  STOPS is a list of percentage/color
pairs."
  (svg-def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-color . ,(cdr stop)))))
     stops))))

(defun svg-rectangle (svg x y width height &rest args)
  "Create a rectangle on SVG."
  (svg-append
   svg
   (dom-node 'rect
	     `((width . ,width)
	       (height . ,height)
	       (x . ,x)
	       (y . ,y)
	       ,@(svg-arguments svg args)))))

(defun svg-circle (svg x y radius &rest args)
  "Create a circle of RADIUS on SVG.
X/Y denote the center of the circle."
  (svg-append
   svg
   (dom-node 'circle
	     `((cx . ,x)
	       (cy . ,y)
	       (r . ,radius)
	       ,@(svg-arguments svg args)))))

(defun svg-ellipse (svg x y x-radius y-radius &rest args)
  "Create an ellipse of X-RADIUS/Y-RADIUS on SVG.
X/Y denote the center of the ellipse."
  (svg-append
   svg
   (dom-node 'ellipse
	     `((cx . ,x)
	       (cy . ,y)
	       (rx . ,x-radius)
	       (ry . ,y-radius)
	       ,@(svg-arguments svg args)))))

(defun svg-line (svg x1 y1 x2 y2 &rest args)
  "Create a line of starting in X1/Y1, ending at X2/Y2 on SVG."
  (svg-append
   svg
   (dom-node 'line
	     `((x1 . ,x1)
	       (y1 . ,y1)
	       (x2 . ,x2)
	       (y2 . ,y2)
	       ,@(svg-arguments svg args)))))

(defun svg-polyline (svg points &rest args)
  "Create a polyline going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg-append
   svg
   (dom-node
    'polyline
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s %s" (car pair) (cdr pair)))
			    points
			    ", "))
      ,@(svg-arguments svg args)))))

(defun svg-polygon (svg points &rest args)
  "Create a polygon going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg-append
   svg
   (dom-node
    'polygon
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s %s" (car pair) (cdr pair)))
			    points
			    ", "))
      ,@(svg-arguments svg args)))))

(defun svg-append (svg node)
  (let ((old (and (dom-attr node 'id)
		  (dom-by-id svg (concat "\\`" (regexp-quote (dom-attr node 'id))
					 "\\'")))))
    (if old
	(dom-set-attributes old (dom-attributes node))
      (dom-append-child svg node)))
  (svg-possibly-update-image svg))

(defun svg-arguments (svg args)
  (let ((stroke-width (or (plist-get args :stroke-width)
			  (dom-attr svg 'stroke-width)))
	(stroke (or (plist-get args :stroke)
		    (dom-attr svg 'stroke)))
	attr)
    (when stroke-width
      (push (cons 'stroke-width stroke-width) attr))
    (when stroke
      (push (cons 'stroke stroke) attr))
    (when (plist-get args :gradient)
      (setq attr
	    (append
	     ;; We need a way to specify the gradient direction here...
	     `((x1 . 0)
	       (x2 . 0)
	       (y1 . 0)
	       (y2 . 1)
	       (fill . ,(format "url(#%s)"
				(plist-get args :gradient))))
	     attr)))
    (cl-loop for (key value) on args by #'cddr
	     unless (memq key '(:stroke :stroke-width :gradient))
	     ;; Drop the leading colon.
	     do (push (cons (intern (substring (symbol-name key) 1) obarray)
			    value)
		      attr))
    attr))

(defun svg-def (svg def)
  (dom-append-child
   (or (dom-by-tag svg 'defs)
       (let ((node (dom-node 'defs)))
	 (dom-add-child-before svg node)
	 node))
   def)
  svg)

(defun svg-image (svg)
  "Return an image object from SVG."
  (create-image
   (with-temp-buffer
     (svg-print svg)
     (buffer-string))
   'svg t))

(defun svg-insert-image (svg)
  "Insert SVG as an image at point.
If the SVG is later changed, the image will also be updated."
  (let ((image (svg-image svg))
	(marker (point-marker)))
    (insert-image image)
    (dom-set-attribute svg :image marker)))

(defun svg-possibly-update-image (svg)
  (let ((marker (dom-attr svg :image)))
    (when (and marker
	       (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
	(put-text-property marker (1+ marker) 'display (svg-image svg))))))

(defun svg-print (dom)
  "Convert DOM into a string containing the xml representation."
  (insert (format "<%s" (car dom)))
  (dolist (attr (nth 1 dom))
    ;; Ignore attributes that start with a colon.
    (unless (= (aref (format "%s" (car attr)) 0) ?:)
      (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
  (insert ">")
  (dolist (elem (nthcdr 2 dom))
    (insert " ")
    (svg-print elem))
  (insert (format "</%s>" (car dom))))

;;;; ChangeLog:

;; 2014-12-01  Ulf Jasper	<ulf.jasper@web.de>
;; 
;; 	svg.el: (svg-line): Fix scrambled coordinates.
;; 
;; 2014-12-01  Lars Magne Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Move the SVG examples to the svg.el file
;; 
;; 2014-11-30  Lars Magne Ingebrigtsen  <larsi@gnus.org>
;; 
;; 	Add svg.el, a package for creating SVG images
;; 


(provide 'svg)

;;; svg.el ends here
