(in-package :cl-user)
(defpackage rrb-vector-tests (:use :prove :cl))
(in-package :rrb-vector-tests)
(cl-ds.utils:import-all-package-symbols :cl-data-structures.sequences.rrb-vector :rrb-vector-tests)

(plan 19076)
(let* ((container (make-instance 'functional-rrb-vector))
       (cont1 (cl-ds:put container 1))
       (cont2 (cl-ds:put cont1 2)))
  (ok (cl:subtypep (type-of container) 'cl-ds:traversable))
  (is (cl-ds:at cont2 0) 1)
  (is (cl-ds:at cont2 1) 2)
  (setf container cont2)
  (iterate
    (for i from 3)
    (repeat cl-data-structures.common.rrb:+maximum-children-count+)
    (setf container (cl-ds:put container i)))
  (is (cl-ds:size container) 34)
  (iterate
    (for i from 3)
    (repeat cl-data-structures.common.rrb:+maximum-children-count+)
    (is (cl-ds:at container (1- i)) i))
  (let ((content cl:nil))
    (cl-ds:traverse (lambda (x) (push x content))
                    container)
    (is (reverse content) (alexandria:iota 34 :start 1)))
  (let ((range (cl-ds:whole-range container)))
    (iterate
      (for i from 0 below 34)
      (is (cl-ds:at range i) (1+ i)))
    (let ((i 1))
      (cl-ds:traverse (lambda (x) (is x i) (incf i))
                      range)))
  (let ((range (cl-ds:whole-range container)))
    (iterate
      (for (values value not-end) = (cl-ds:consume-front range))
      (for i from 1)
      (while not-end)
      (is value i)))
  (let ((range (cl-ds:whole-range container)))
    (iterate
      (for (values value not-end) = (cl-ds:consume-back range))
      (for i from 34 downto 0)
      (while not-end)
      (is value i)))
  (setf container (cl-ds:update container 10 'a))
  (is (cl-ds:at container 10) 'a)
  (setf container (cl-ds:take-out container))
  (is (cl-ds:size container) 33)
  (setf container (cl-ds:take-out container))
  (is (cl-ds:size container) 32)
  (setf container (cl-ds:take-out container))
  (is (cl-ds:size container) 31))


(let ((container (make-instance 'functional-rrb-vector)))
  (iterate
    (for i from 0 below 130)
    (is (cl-ds:size container) i)
    (for next = (cl-ds:put container i))
    (iterate
      (for j from 0 to i)
      (is (cl-ds:at next j) j))
    (setf container next))
  (iterate
    (for i from 129 downto 1)
    (for next = (cl-ds:take-out container))
    (is (cl-ds:size next) i)
    (iterate
      (for j from 0 below i)
      (is (cl-ds:at next j) j))
    (setf container next)))

(let ((container (make-instance 'mutable-rrb-vector)))
  (iterate
    (for i from 0 below 130)
    (is (cl-ds:size container) i)
    (cl-ds:put! container i))
  (iterate
    (for i from 0 below 130)
    (is (cl-ds:at container i) i))
  (iterate
    (for i from 0 below 130)
    (setf (cl-ds:at container i) (+ i 129)))
  (iterate
    (for i from 0 below 130)
    (is (cl-ds:at container i) (+ i 129)))
  (iterate
    (for i from 130 downto 1)
    (is (cl-ds:size container) i)
    (is (cl-ds:at container (1- i)) (+ 129 (1- i)))
    (for (values c status) = (cl-ds:take-out! container))
    (ok (cl-ds:found status))
    (is (cl-ds:value status) (+ 129 (1- i)))))

(let ((container (cl-ds:make-from-traversable 'functional-rrb-vector nil
                                              (cl-ds:xpr (:i 0)
                                                (when (< i 10)
                                                  (send-recur i :i (1+ i)))))))
  (iterate
    (for i from 0 below 10)
    (is (cl-ds:at container i) i)))

(let ((container (cl-ds:make-from-traversable 'transactional-rrb-vector nil
                                              (cl-ds:xpr (:i 0)
                                                (when (< i 64)
                                                  (send-recur i :i (1+ i)))))))
  (iterate
    (for i from 0 below 64)
    (is (cl-ds:at container i) i))
  (let ((new-container (cl-ds:become-transactional container)))
    (iterate
      (for i from 64 below 365)
      (cl-ds:put! new-container i))
    (iterate
      (for i from 64 below 365)
      (cl-ds:put! container (* i 2)))
    (iterate
      (for i from 64 below 365)
      (is (cl-ds:at container i) (* i 2)))
    (iterate
      (for i from 0 below 64)
      (is (cl-ds:at new-container i) i))
    (iterate
      (for i from 0 below 365)
      (is (cl-ds:at new-container i) i))))

(let ((container (cl-ds:make-from-traversable 'functional-rrb-vector nil
                                              (cl-ds:xpr (:i 0)
                                                (when (< i 64)
                                                  (send-recur i :i (1+ i)))))))
  (let ((range (cl-ds:whole-range container)))
    (cl-ds.common.rrb::init-rrb range container :from 5)
    (iterate
      (for (values value more) = (cl-ds:consume-front range))
      (while more)
      (for i from 5)
      (is value i))))

(finalize)
