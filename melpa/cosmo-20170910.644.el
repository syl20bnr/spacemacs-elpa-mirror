;;; cosmo.el --- Cosmological Calculator    -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Francesco Montanari
;;
;; Author: Francesco Montanari <fmnt@fmnt.info>
;; Created: 22 April 2017
;; Version: 0.1
;; Package-Version: 20170910.644
;; Package-Requires: ((emacs "24.4"))
;; Keywords: tools
;; URL: https://gitlab.com/montanari/cosmo-el
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a cosmological calculator for Lambda-CDM
;; models.  Such a framework describes a homogeneous and isotropic
;; universe containing a cosmological constant (Lambda) and a Cold
;; Dark Matter (CDM) component, besides ordinary species.  The
;; model is characterized by the following parameters:
;;
;; - H_0 :: Hubble parameter (expansion rate) today.
;; - Omega_m0 :: Matter density parameter today.
;; - Omega_Lambda :: Cosmological constant density parameter.
;; - Omega_r0 :: Relativistic species (e.g., photons plus
;;               neutrinos) density parameter today.
;; - Omega_k0 :: Curvature density parameter today.  This
;;               parameter is derived from the others above
;;               according to Friedmann's equation
;;               Omega_m0 + Omega_Lambda + Omega_r0 + Omega_k0 = 1.
;;
;; All cosmological quantities are computed at a given redshift
;; value:
;;
;; - redshift :: Gravitational redshift of photons frequency due to the
;;               expansion of the Universe.
;;
;; Definitions follow Hogg (1999)
;; <https://arxiv.org/abs/astro-ph/9905116>.
;;
;; Names with "--" are for functions and variables that are meant to
;; be for internal use only.

;;; Bugs:

;; - None known.

;;; Todo:

;; - Suggest default parameters when reading them with the related
;;   command; set the to default values if none is entered.
;;
;; - (Consider the following only if performance becomes critical.) At
;;   a fixed redshift, only cosmo-get-los-comoving-distance perform
;;   the integral. Other distances are defined in terms of this
;;   one. However, each time that other distances are required this
;;   distance is called once again. This is not necessary, it can be
;;   called just once for a given redshift (or use memoization).

;;; Code:

;;; Global variables.

(defgroup cosmo nil
  "Cosmological calculator."
  :group 'applications)

(defcustom cosmo-int-prec 1e-3
  "Fractional accuracy for numerical integrals."
  :group 'cosmo)

(defcustom cosmo-int-maxsteps 20
  "Maximum number of steps in the numerical integral algorithm."
  :group 'cosmo)

;; Hash table containing all independent cosmological parameters.
(defvar cosmo--params
  (let ((table (make-hash-table :test #'equal)))
    (puthash "H0 [Km/s/Mpc]" 70.0 table) ; Hubble today km/s/Mpc.
    (puthash "omatter" 0.3 table)        ; Matter density today.
    (puthash "olambda" 0.7 table)        ; Curvature density today.
    (puthash "orel" 0.0 table)           ; Relativistic density today.
    table)
  "Table containing Lambda-CDM cosmological parameters.")

;;; Handle input.

(defun cosmo--string-number-p (string)
  "Test whether STRING represents a number."
  (if (string-match "\\`[-+]?[0-9]+\\.?[0-9]*\\'" string)
      t
    nil))

(defun cosmo--read-param (name)
  "Read parameter NAME from minibuffer and convert it to a number."
  (let ((value (read-from-minibuffer (format "Enter %s: " name))))
    (if (cosmo--string-number-p value)
        (string-to-number value)
      (error "Error: parameter must be a number"))))

(defun cosmo--put-param (name)
  "Read parameter NAME from minibuffer and add it to the parameter table."
  (puthash name (cosmo--read-param name) cosmo--params))

(defun cosmo--check-param (name value)
  "Check the validity of NAME (a cosmological parameter) VALUE."
  (cond ((or (string= name "omatter")
             (string= name "olambda")
             (string= name "orel"))
         (unless (>= value 0.0)
           (error "Error: density parameter must be positive")))))

;;;###autoload
(defun cosmo-set-params ()
  "Change the values of cosmological parameters."
  (interactive)
  (maphash (lambda (key _value)
             (cosmo--put-param key)
             (cosmo--check-param key (gethash key cosmo--params)))
           cosmo--params))

;;; Numerical utilities.

(defun cosmo-sinh (x)
  "Hyperbolic sine of real arguments X."
  (* 0.5 (- (exp x) (exp (- x)))))

(defun cosmo-asinh (x)
  "Inverse hyperbolic sine of real arguments X."
  (log (+ x (sqrt (1+ (* x x))))))

(defun cosmo--trapzd (func lo hi sum niter)
  "Extended trapezoidal rule to integrate FUNC from LO to HI.
Compute the refinement to an old SUM result, previous to the
current NITER stage of refinement.  The function must be called
for NITER = 1, 2, 3, ... in order.  Boundaries LO and HI are
always converted to floats."
  (let ((lo (float lo))                 ; Always assume float boundaries.
        (hi (float hi)))
    ;; NITER=1 corresponds simply to integrate(func(x), x, lo, hi)
    (if (= niter 1)
        (* 0.5 (- hi lo) (+ (funcall func lo) (funcall func hi)))
      ;; Subsequent calls NITER = 2, 3, ... (in order) improve the
      ;; accuracy by adding 2^(NITER-2) interior points.
      (let* ((iter (expt 2 (- niter 2)))
             (step (/ (- hi lo) iter))
             (xval (+ lo (* 0.5 step)))
             (sumf 0.0)
             (j 0))
        (while (< j iter)
          (setq sumf (+ sumf (funcall func xval)))
          (setq xval (+ xval step))
          (setq j (1+ j)))
        (* 0.5 (+ sum (* (- hi lo) (/ sumf iter))))))))

(defun cosmo-qsimp (func lo hi &optional eps jmax)
  "Simpson's integration.
Argument FUNC integrand.
Argument LO lower bound.
Argument HI upper bound.
Optional argument EPS fractional accuracy.
Optional argument JMAX maximum number of steps."
  (let ((eps (or eps 1e-6))             ; Set defaults.
        (jmax (or jmax 20))
        (nsum 0.0)                      ; N-th iteration sum.
        (nsumt 0.0)
        (osum 0.0)                      ; Old sum.
        (osumt 0.0)
        (converged nil)
        (j 1))
    (while (and (<= j jmax) (not converged))
      (setq nsumt (cosmo--trapzd func lo hi nsum j))
      (setq nsum (/ (- (* 4.0 nsumt) osumt) 3.0))
      (if (and (> j 5)                  ; Avoid spurious early convergence.
               (or (< (abs (- nsum osum)) (* eps (abs osum)))
                   (and (= nsum 0.0) (= osum 0.0))))
          (setq converged t))
      (setq osum nsum)
      (setq osumt nsumt)
      (setq j (1+ j)))
    (if converged
        nsum
      (error "Error: the integral did not converge, try to
      increase the number of steps"))))

;;; Compute cosmological functions.

(defun cosmo-get-ocurvature ()
  "Get curvature density parameter today from Friedmann equations."
  (let ((omatter (gethash "omatter" cosmo--params))
        (olambda (gethash "olambda" cosmo--params))
        (orel (gethash "orel" cosmo--params)))
    (- 1.0 omatter olambda orel)))

(defun cosmo-efunc (redshift)
  "E(z) function at a given REDSHIFT."
  (let ((omatter (gethash "omatter" cosmo--params))
        (olambda (gethash "olambda" cosmo--params))
        (orel (gethash "orel" cosmo--params))
        (ocurvature (cosmo-get-ocurvature))
        (zp1 (+ 1 redshift)))
    (sqrt (+ (* orel (expt zp1 4.0))
             (* omatter (expt zp1 3.0))
             (* ocurvature (expt zp1 2.0))
             olambda))))

(defun cosmo-inv-efunc (redshift)
  "Inverse E(z) function at a given REDSHIFT."
  (/ 1.0 (cosmo-efunc redshift)))

(defun cosmo-get-hubble (redshift)
  "Hubble parameter [Km/s/Mpc] for Lambda-CDM at a given REDSHIFT."
  (let ((H0 (gethash "H0 [Km/s/Mpc]" cosmo--params)))
    (* H0 (cosmo-efunc redshift))))

;;;###autoload
(defun cosmo-hubble ()
  "Display Hubble parameter in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s km/s/Mpc" (cosmo-get-hubble z)))))

(defun cosmo-get-hubble-distance ()
  "Hubble distance c/H0 [Mpc] for Lambda-CDM."
  (let ((H0 (gethash "H0 [Km/s/Mpc]" cosmo--params)))
    (/ 3.0e5 H0)))

;;;###autoload
(defun cosmo-hubble-distance ()
  "Display Hubble distance c/H0 in mini-buffer."
  (interactive)
  (message (format "%s Mpc" (cosmo-get-hubble-distance))))

(defun cosmo-get-hubble-time ()
  "Hubble time 1/H0 [Gyr] for Lambda-CDM."
  (let ((H0 (gethash "H0 [Km/s/Mpc]" cosmo--params)))
    (/ 9.78e2 H0)))

;;;###autoload
(defun cosmo-hubble-time ()
  "Display Hubble time 1/H0 in mini-buffer."
  (interactive)
  (message (format "%s Gyr" (cosmo-get-hubble-time))))

(defun cosmo-get-los-comoving-distance (redshift)
  "Line-of-sight comoving distance [Mpc] for Lambda-CDM at a given REDSHIFT."
  (let ((DH (cosmo-get-hubble-distance))
        (int (cosmo-qsimp #'cosmo-inv-efunc 0.0 redshift
                          cosmo-int-prec cosmo-int-maxsteps)))
    (* DH int)))

;;;###autoload
(defun cosmo-los-comoving-distance ()
  "Display line-of-sight comoving distance in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc" (cosmo-get-los-comoving-distance z)))))

(defun cosmo-get-transverse-comoving-distance (redshift)
  "Line-of-sight comoving distance [Mpc] for Lambda-CDM at a given REDSHIFT."
  (let* ((DH (cosmo-get-hubble-distance))
         (DC (cosmo-get-los-comoving-distance redshift))
         (ocurvature (cosmo-get-ocurvature))
         (sqrt-ok (sqrt (abs (cosmo-get-ocurvature))))
         (DH-over-sqrtok (/ DH sqrt-ok)))
    (cond ((> ocurvature 0)
           (* DH-over-sqrtok (cosmo-sinh (/ DC DH-over-sqrtok))))
          ((= ocurvature 0)
           DC)
          ((< ocurvature 0)
           (* DH-over-sqrtok (sin (/ DC DH-over-sqrtok)))))))

;;;###autoload
(defun cosmo-transverse-comoving-distance ()
  "Display transverse comoving distance in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc"
                     (cosmo-get-transverse-comoving-distance z)))))

(defun cosmo-get-angular-diameter-distance (redshift)
  "Angular diameter distance [Mpc] for Lambda-CDM at a given REDSHIFT."
  (let* ((DM (cosmo-get-transverse-comoving-distance redshift)))
    (/ DM (1+ redshift))))

;;;###autoload
(defun cosmo-angular-diameter-distance ()
  "Display angular diameter distance in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc"
                     (cosmo-get-angular-diameter-distance z)))))

(defun cosmo-get-luminosity-distance (redshift)
  "Luminosity distance [Mpc] for Lambda-CDM at a given REDSHIFT."
  (let* ((DM (cosmo-get-transverse-comoving-distance redshift)))
    (* DM (1+ redshift))))

;;;###autoload
(defun cosmo-luminosity-distance ()
  "Display luminosity distance in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc"
                     (cosmo-get-luminosity-distance z)))))

(defun cosmo-get-parallax-distance (redshift)
  "Parallax distance [Mpc] for Lambda-CDM at a given REDSHIFT."
  (let* ((DH (cosmo-get-hubble-distance))
         (DM (cosmo-get-transverse-comoving-distance redshift))
         (dM (/ DM DH))
         (ocurvature (cosmo-get-ocurvature)))
    (/ DM (+ dM (sqrt (1+ (* ocurvature dM dM)))))))

;;;###autoload
(defun cosmo-parallax-distance ()
  "Display parallax distance in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc"
                     (cosmo-get-parallax-distance z)))))

(defun cosmo--get-comoving-volume-nonflat (DM DH ocurvature)
  "Return the comoving volume for non-vanishing curvature.
Argument DM comoving distance (transverse).
Argument DH Hubble distance.
Argument OCURVATURE curvature density parameter."
  (let* ((DM-over-DH (/ DM DH))
         (sqrt-ok (sqrt (abs (cosmo-get-ocurvature))))
         (pref (* 2.0 float-pi (/ (expt DH 3.0) ocurvature)))
         (func (cond ((> ocurvature 0.0)
                      #'cosmo-asinh)
                     ((< ocurvature 0.0)
                      #'asin)
                     (t (error "Error: wrong curvature parameter value"))))
         (term1 (* DM-over-DH
                   (sqrt (1+ (* ocurvature (expt DM-over-DH 2.0))))))
         (term2 (/ (funcall func (* sqrt-ok DM-over-DH)) sqrt-ok)))
    (* pref (- term1 term2))))

(defun cosmo-get-comoving-volume (redshift)
  "Comoving volume [Mpc^3] for Lambda-CDM at a given REDSHIFT."
  (let* ((DH (cosmo-get-hubble-distance))
         (DM (cosmo-get-transverse-comoving-distance redshift))
         (ocurvature (cosmo-get-ocurvature)))
    (if (= ocurvature 0.0)
        (* (/ 4.0 3.0) float-pi (expt DM 3.0))
        (cosmo--get-comoving-volume-nonflat DM DH ocurvature))))

;;;###autoload
(defun cosmo-comoving-volume ()
  "Display comoving volume in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Mpc^3"
                     (cosmo-get-comoving-volume z)))))

(defun cosmo--age-integrand (redshift)
  "Universe age integrand at a given REDSHIFT."
  (/ (cosmo-inv-efunc redshift) (1+ redshift)))

(defun cosmo-get-lookback-time (redshift)
  "Lookback time [Gyr] for Lambda-CDM at a given REDSHIFT."
  (let ((tH (cosmo-get-hubble-time))
        (int (cosmo-qsimp #'cosmo--age-integrand 0.0 redshift
                          cosmo-int-prec cosmo-int-maxsteps)))
    (* tH int)))

;;;###autoload
(defun cosmo-lookback-time ()
  "Display lookback time in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Gyr"
                     (cosmo-get-lookback-time z)))))

(defun cosmo-get-age (redshift)
  ;; This is much slower than other functions.
  "Age of the Universe [Gyr] for Lambda-CDM at a given REDSHIFT.
This is approximated as the age since equality redshift."
  (let* ((tH (cosmo-get-hubble-time))
         (omatter (gethash "omatter" cosmo--params))
         (H0 (gethash "H0 [Km/s/Mpc]" cosmo--params))
         (zeq                           ; arXiv:astro-ph/9709112
          (* 2.5 1e4 omatter (expt (/ H0 100.0) 2.0)))
         (int (cosmo-qsimp #'cosmo--age-integrand redshift zeq
                           cosmo-int-prec cosmo-int-maxsteps)))
    (* tH int)))

;;;###autoload
(defun cosmo-age ()
  "Display age of the Universe in mini-buffer."
  (interactive)
  (let ((z (cosmo--read-param "redshift")))
    (message (format "%s Gyr"
                     (cosmo-get-age z)))))

;;; Handle output.

(defun cosmo--write-calc-header ()
  "Write header for the cosmological calculator summary buffer."
  (let ((head "Cosmology calculator.\n\n")
        (help "(`q` to quite)\n\n"))
    (insert (propertize help 'font-lock-face 'italic))
    (insert head)))

(defun cosmo--write-calc (redshift H0 omatter olambda orel hubble
                                   los-dist transverse-dist
                                   luminosity-dist angular-dist
                                   parallax-dist comoving-vol
                                   lookback-time)
  "Format and insert cosmological table in buffer.
Argument REDSHIFT redshift.
Argument H0 Hubble parameter today.
Argument OMATTER matter density parameter.
Argument OLAMBDA cosmological constant density parameter.
Argument OREL density parameter.
Argument HUBBLE Hubble parameter at given redshift.
Argument LOS-DIST line-of-sight comoving distance at given redshift.
Argument TRANSVERSE-DIST transverse comoving distance at given redshift.
Argument LUMINOSITY-DIST luminosity distance at given redshift.
Argument ANGULAR-DIST angular diameter distance at given redshift.
Argument PARALLAX-DIST parallax distance at given redshift.
Argument COMOVING-VOL comoving volume at given redshift.
Argument LOOKBACK-TIME lookback time at given redshift."

  ;; Input parameters.
  (cosmo--write-calc-header)
  (insert "Input Parameters\n"
          "----------------\n"
          (format "- Redshift:                                 %s\n"
                  redshift)
          (format "- Hubble constant, now [km/s/Mpc]:          %s\n"
                  H0)
          (format "- Matter fractional density, now:           %s\n"
                  omatter)
          (format "- Cosmological constant fractional density: %s\n"
                  olambda)
          (format "- Relativistic fractional density, now:     %s\n"
                  orel)
          "\n")
  ;; Derived parameters.
  (insert "Derived parameters\n"
          "------------------\n"
          (format "- Curvature fractional density: %s\n"
                  (cosmo-get-ocurvature))
          (format "- Hubble distance [Mpc]:        %s\n"
                  (cosmo-get-hubble-distance))
          (format "- Hubble time [Gyr]:            %s\n"
                  (cosmo-get-hubble-time))
          "\n")
  ;; Cosmological functions.
  (insert "Cosmography at required redshift\n"
          "--------------------------------\n"
          (format "- Hubble parameter [km/s/Mpc]:             %s\n"
                  hubble)
          (format "- Comoving distance (line-of-sight) [Mpc]: %s\n"
                  los-dist)
          (format "- Comoving distance (transverse) [Mpc]:    %s\n"
                  transverse-dist)
          (format "- Angular diameter distance [Mpc]:         %s\n"
                  angular-dist)
          (format "- Luminosity distance [Mpc]:               %s\n"
                  luminosity-dist)
          (format "- Parallax distance [Mpc]:                 %s\n"
                  parallax-dist)
          (format "- Comoving volume [Mpc^3]:                 %s\n"
                  comoving-vol)
          (format "- Lookback time [Gyr]:                     %s\n"
                  lookback-time))
  nil)

;;;###autoload
(defun cosmo-calculator ()
  "Compute cosmology and display summary table in a new buffer."
  (interactive)
  (let* ((cosmo-buffer "*Cosmo*")
         (redshift (cosmo--read-param "redshift"))
         (omatter (gethash "omatter" cosmo--params))
         (olambda (gethash "olambda" cosmo--params))
         (orel (gethash "orel" cosmo--params))
         (H0 (gethash "H0 [Km/s/Mpc]" cosmo--params))
         (hubble (cosmo-get-hubble redshift))
         (los-dist (cosmo-get-los-comoving-distance redshift))
         (transverse-dist (cosmo-get-transverse-comoving-distance redshift))
         (luminosity-dist (cosmo-get-luminosity-distance redshift))
         (angular-dist (cosmo-get-angular-diameter-distance redshift))
         (parallax-dist (cosmo-get-parallax-distance redshift))
         (comoving-vol (cosmo-get-comoving-volume redshift))
         (lookback-time (cosmo-get-lookback-time redshift)))
   (with-output-to-temp-buffer cosmo-buffer
      (pop-to-buffer cosmo-buffer)
      (cosmo--write-calc redshift H0 omatter olambda orel hubble
                         los-dist transverse-dist luminosity-dist
                         angular-dist parallax-dist comoving-vol
                         lookback-time))))

(provide 'cosmo)

;;; cosmo.el ends here
