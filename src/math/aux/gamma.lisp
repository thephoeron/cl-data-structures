(in-package #:cl-ds.math.aux)


(defun gamma (xx)
  "Calculates value of gamma function with Lancoz optimization."
  (if (< (realpart xx) 1.0)
      (let ((z (- 1.0 xx)))
        (- (log (/ (* pi z) (sin (* pi z))))
           (gamma (+ 1.0 z))))
      (let* ((z (- xx 1.0))
             (tmp (+ z 5.0 0.5)))
        (+ (* (log tmp) (+ z 0.5))
           (- tmp)
           (log (sqrt (* 2 pi)))
           (log (+ 1.0
                   (/ 76.18009173d0 (+ z 1.0d0))
                   (/ -86.50532033d0 (+ z 2.0d0))
                   (/ 24.01409822d0 (+ z 3.0d0))
                   (/ -1.231739516d0 (+ z 4.0d0))
                   (/ 0.120858003d-2 (+ z 5.0d0))
                   (/ -0.536382d-5 (+ z 6.0d0))))))))
