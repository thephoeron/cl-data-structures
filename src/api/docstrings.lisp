(in-package #:cl-data-structures)


(set-documentation
 'position-modification <mechanics> <generic> *documentation*
 :syntax "position-modification operation container location more-args => container status"
 :arguments-and-values '(("OPERATION" "Instance of API function.")
                         ("CONTAINER" "Instance of container class")
                         ("LOCATION" "Where modification is supposed to happen?"))
 :returns '("Container (new or the same instance)"
            "Modification status")
 :description "A low level function used as de facto implementation point of all API modification functions (INSERT, ADD, UPDATE)."
 :notes "Implementations of this generic function are multimethods dispatched on the class of the OPERATION and on the CONTAINER.")


(set-documentation
 'functional-counterpart <mechanics> <generic> cl-ds:*documentation*
 :syntax "functional-counterpart operation => functional-operation"
 :arguments-and-values '(("OPERATION" "Instance of the modification API function."))
 :description "Low level function that returns an instance of the functional api modification function that serves the same purpose as the OPERATION. Will, for instance, return #'UPDATE when OPERATION is #'UPDATE! Will return original the OPERATION if OPERATION is already part of the functional API."
 :returns "The instance of api function."
 :notes "This function is low level, and therefore should be rarely (if ever) used by the user of this library.")


(set-documentation
 'destructive-counterpart <mechanics> <generic> cl-ds:*documentation*
 :syntax "destructive-counterpart operation => destructive-operation"
 :arguments-and-values '(("OPERATION" "Instance of the modification API function."))
 :description "Low level function that returns an instance of the destrutive api modification function that serves the same purpose as the OPERATION. Will, for instance, return #'UPDATE! when OPERATION is #'UPDATE Will return original the OPERATION if OPERATION is already part of the functional API."
 :returns "The instance of api function."
 :notes "This function is low level, and therefore should be rarely (if ever) used by the user of this library.")


(set-documentation
 'at <mechanics> <generic> *documentation*
 :syntax '("for dictionary containers: at dictionary key => value found"
           "for everything else: at sequence location => value")
 :arguments-and-values
 '(("CONTAINER" "Instance of subclass of fundamental-container."
    ("LOCATION" "Where are we looking at? Key in hashtable, index of vector, etc.")))

 :returns
 "In case of associative containers, second value informs if LOCATION was found in the CONTAINER (first value is NIL if element was not found).
 In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type ARGUMENT-OUT-OF-BOUNDS will be raised."

 :description
 "Obtain element stored at LOCATION in the CONTAINER."

 :examples '("(let ((table (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
                (prove:diag \"Testing example for AT function.\")
                (multiple-value-bind (value found) (cl-ds:at table 'a)
                  (prove:is value nil)
                  (prove:is found nil))
                (setf (cl-ds:at table 'a) 1)
                (multiple-value-bind (value found) (cl-ds:at table 'a)
                  (prove:is value 1)
                  (prove:is found t))))"))


(set-documentation
 'add <mechanics> <generic> *documentation*
 :syntax "add dictionary key value => new-dictionary-instance status"
 :arguments-and-values
 '(("CONTAINER" "Instance that we want to modify.")
   ("LOCATION" "Place where NEW-VALUE shall be added.")
   ("NEW-VALUE" "Value that we intend to add into returned instance."))

 :examples '("(let ((table (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
               (prove:diag \"Testing example for add function.\")
               (multiple-value-bind (value found) (cl-ds:at table 'a)
                 (prove:is value nil)
                 (prove:is found nil))
               (let ((next-table (cl-ds:add table 'a 1)))
                 (multiple-value-bind (value found) (cl-ds:at table 'a)
                   (prove:is value nil)
                   (prove:is found nil))
                 (multiple-value-bind (value found) (cl-ds:at next-table 'a)
                   (prove:is value 1)
                   (prove:is found t))
                 (cl-ds:mod-bind (next-next-table found value)
                                 (cl-ds:add next-table 'a 2)
                   (prove:ok found)
                   (prove:is value 1)
                   (prove:is next-next-table next-table)
                   (multiple-value-bind (value found) (cl-ds:at next-next-table 'a)
                     (prove:ok found)
                     (prove:is value 1)))))")

 :returns '("Instance of the same type as CONTAINER. If add took place it shall contain NEW-VALUE at LOCATION."
            "Modification status object.")

 :description "Functional API: attempts to non-destructively add NEW-VALUE into CONTAINER at LOCATION. Will not replace value at LOCATION if it was already occupied."

 :notes "This is functional counterpart to the ADD! function.")


(set-documentation
 'add! <mechanics> <generic> *documentation*
 :arguments-and-values
 '(("CONTAINER" "Instance that we intend to destructivly modify")
   ("LOCATION" "Place in the CONTAINER that we intend to change")
   ("NEW-VALUE" "Value that we intend to add into CONTAINER"))

 :description "Destructively add the NEW-VALUE into the CONTAINER at the LOCATION. Will not replace a value at LOCATION if it was already occupied."

 :returns '("CONTAINER"
            "Modification status object")

 :syntax "add! container location new-value => same-container status"

 :side-effects "If item was not found in the CONTAINER, destructivly transform CONTAINER."

 :notes "This is the destructive counterpart to the ADD function.")


(set-documentation
 'insert <mechanics> <generic> *documentation*
 :syntax "insert container location new-value => new-instance status"
 :description
 "Functional API: non-destructively insert the NEW-VALUE into the CONTAINER at the LOCATION. Will replace a value at the LOCATION if it was already occupied."

 :returns
 '("Instance of the same type as CONTAINER, with NEW-VALUE at LOCATION"
   "Modification status object.")

 :arguments-and-values
 '(("container" "Instance of container.")
   ("location" "Designates place in returned instance that will be changed")
   ("new-value" "Value that will be inserted into returned instance"))

 :notes "This is the functional counterpart to the (SETF AT) function."

 :examples
 '("(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
          (next-table (cl-ds:insert table 'a 5)))
     (prove:is (cl-ds:at next-table 'a) 5)
     (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql)))"))


(set-documentation
 'erase <mechanics> <generic> *documentation*
 :syntax "erase container location => new-instance status"
 :description
 "Functional API: non-destructively remove a element at the LOCATION from the CONTAINER."

 :returns
 '("Instance of the same type as CONTAINER, without value at LOCATION"
   "Modification status object.")

 :notes "This is the functional counterpart to the ERASE! function."

 :examples
 '("(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
           (next-table (cl-ds:insert table 'a 5)))
     (prove:diag \"Running example for ERASE\")
     (prove:is (cl-ds:at next-table 'a) 5)
     (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql))
     (cl-ds:mod-bind (erased-table found value) (cl-ds:erase next-table 'a)
       (prove:ok found)
       (prove:is value 5)
       (prove:is (cl-ds:at erased-table 'a) nil)
       (prove:is (cl-ds:at next-table 'a) 5)))")

 :arguments-and-values
 '(("CONTAINER" "Container that shall be modified.")
   ("LOCATION" "Designates place in returned instance that will be changed.")))


(set-documentation
 'erase-if <mechanics> <generic> *documentation*
 :syntax "erase-if container location condition => new-instance status"
 :description
 "Functional API: non-destructively remove element at LOCATION from the CONTAINER, only when CONDITION function returns true. CONDITION will be called with location that matches according to comparsion function used to construct container, and with a value."

 :returns
 '("Instance of the same type as CONTAINER, without value at LOCATION"
   "Modification status object.")

 :examples
 '("(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
           (next-table (cl-ds:insert (cl-ds:insert table 'a 5) 'b 6)))
     (prove:diag \"Running example for ERASE-IF\")
     (prove:is (cl-ds:at next-table 'a) 5)
     (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql))
     (cl-ds:mod-bind (erased-table found value) (cl-ds:erase-if next-table 'a (lambda (location value) (declare (ignore location)) (evenp value)))
       (prove:ok (null found))
       (prove:is value nil)
       (prove:is (cl-ds:at erased-table 'a) 5)
       (prove:is (cl-ds:at next-table 'a) 5))
    (cl-ds:mod-bind (erased-table found value) (cl-ds:erase-if next-table 'b (lambda (location value) (declare (ignore location)) (evenp value)))
       (prove:ok found)
       (prove:is value 6)
       (prove:is (cl-ds:at erased-table 'b) nil)
       (prove:is (cl-ds:at next-table 'b) 6)))")

 :arguments-and-values
 '(("CONTAINER" "Container that shall be modified.")
   ("LOCATION" "Designates place in returned instance that will be changed.")
   ("CONDITION" "Function of two arguments, should return boolean."))

 :notes "This is the functional counterpart to the ERASE-IF! function.")


(set-documentation
 'erase-if! <mechanics> <generic> *documentation*
 :syntax "erase-if! container location condition => same-instance status"
 :description
 "Functional API: destructively remove element at LOCATION from the CONTAINER, only when CONDITION function returns true. CONDITION will be called with location that matches according to comparsion function, and with value."

 :returns
 '("CONTAINER"
   "Modification status object.")

 :examples
 '("(let* ((table (cl-ds.dicts.hamt::make-mutable-hamt-dictionary #'sxhash #'eq)))
     (setf (cl-ds:at table 'a) 5
           (cl-ds:at table 'b) 6)
     (prove:diag \"Running example for ERASE-IF!\")
     (prove:is (cl-ds:at table 'a) 5)
     (cl-ds:mod-bind (erased-table found value) (cl-ds:erase-if! table 'a (lambda (location value) (declare (ignore location)) (evenp value)))
       (prove:ok (null found))
       (prove:is value nil)
       (prove:is erased-table table)
       (prove:is (cl-ds:at erased-table 'a) 5))
    (cl-ds:mod-bind (erased-table found value) (cl-ds:erase-if! table 'b (lambda (location value) (declare (ignore location)) (evenp value)))
       (prove:ok found)
       (prove:is value 6)
       (prove:is erased-table table)
       (prove:is (cl-ds:at erased-table 'b) nil)))")

 :arguments-and-values
 '(("CONTAINER" "Container that shall be modified.")
   ("LOCATION" "Designates place in returned instance that will be changed.")
   ("CONDITION" "Function of two arguments, should return boolean."))

 :notes "This is the destructive counterpart to the ERASE-IF function.")


(set-documentation
 'erase! <mechanics> <generic> *documentation*
 :description "Mutable API: destructively remove a element at the LOCATION from the CONTAINER."
 :syntax "erase! container location => same-instance status"
 :returns '("CONTAINER" "Modification status object")
 :arguments-and-values
 '(("container" "Instance that is intended to be destructivly modified.")
   ("location" "Location in the container that we want to modify by removing value."))
 :side-effects "If erase took place, destructivly transform CONTAINER."
 :notes "This is the destrucive counterpart to the ERASE function.")


(set-documentation
 'size <mechanics> <generic> *documentation*
 :syntax "size container => count"
 :description "How many elements the CONTAINER holds currently?"
 :arguments-and-values '(("container" "instance that will be checked."))
 :returns "The number of elements in the container."
 :examples '("(let ((container (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
               (prove:is (cl-ds:size container) 0)
               (setf (cl-ds:at container 'a) 1)
               (prove:is (cl-ds:size container) 1))"))


(set-documentation
 'update <mechanics> <generic> *documentation*
 :description
 "Functional API: if there is value at LOCATION in the CONTAINER return new instance with NEW-VALUE at LOCATION."

 :syntax
 "update container location new-value => new-instance status"

 :returns
 '("New container, with updated value at LOCATION if UPDATE took place"
   "Modification status object")

 :arguments-and-values
 '(("container" "The instance that shall be transformed.")
   ("location" "The location in the container that we want to transform."))

 :notes "This is the functional counterpart to the UPDATE! function.")


(set-documentation
 'update! <mechanics> <generic> *documentation*
 :description
 "Mutable API: If the LOCATION is taken in the CONTAINER, destructivly update it with the NEW-VALUE"

 :syntax
 "(update! container location new-value) -> same-container status"

 :returns
 '("CONTAINER"
   "Modification status object")

 :arguments-and-values
 '(("container" "Container that shall be modified.")
   ("location" "Location in the container that we want to transform."))

 :notes "This is the destructive counterpart to the UPDATE function.")

(set-documentation
 'become-functional <mechanics> <generic> *documentation*
 :description
 "Transforms CONTAINER into functional variant."

 :syntax
 "become-functional container => functional-container"

 :returns
 "A instance implementing functional API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into functional container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of a internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into a returned instance.")


(set-documentation
 'become-mutable <mechanics> <generic> *documentation*
 :description
 "Transforms the CONTAINER into a mutable variant."

 :syntax
 "become-mutable container => mutable-container"

 :returns
 "A instance implementing mutable API. Content of the returned instance is identical to the content of the input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into mutable container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of a internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance.")


(set-documentation
 'become-transactional <mechanics> <generic> *documentation*
 :description
 "Transforms CONTAINER into transactional variant."

 :syntax
 "become-transactional container => transactional-container"

 :returns
 "instance implementing mutable API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into transactional container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance.")


(set-documentation
 'become-lazy <mechanics> <generic> *documentation*
 :description
 "Transforms CONTAINER into lazy variant."

 :syntax
 "become-lazy container => lazy-container"

 :returns
 "instance implementing functional, lazy API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into lazy container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "All containers that implement become-transactional, also implement become-lazy")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance.")


(set-documentation
 'mutablep <mechanics> <generic> *documentation*
 :syntax '("mutablep mutable-container => t"
           "mutablep functional-container => nil")
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))

 :examples '("(progn (prove:diag \"Running example for mutablep.\")
                    (let ((mutable (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
                      (prove:ok (cl-ds:mutablep mutable))
                      (prove:ok (not (cl-ds:functionalp mutable))))
                    (let ((functional (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
                      (prove:ok (not (cl-ds:mutablep functional)))
                      (prove:ok (cl-ds:functionalp functional))))")

 :returns "T if CONTAINER exposes mutable API and NIL if not.")


(set-documentation
 'functionalp <mechanics> <generic> *documentation*
 :syntax '("(functionalp mutable-container) -> nil"
           "(functionalp functional-container) -> t")
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :examples '("(progn (prove:diag \"Running example for functionalp.\")
                    (let ((mutable (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
                      (prove:ok (cl-ds:mutablep mutable))
                      (prove:ok (not (cl-ds:functionalp mutable))))
                    (let ((functional (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
                      (prove:ok (not (cl-ds:mutablep functional)))
                      (prove:ok (cl-ds:functionalp functional))))")
 :returns "T if CONTAINER exposes functional API and NIL if not.")


(set-documentation
 'transactionalp <mechanics> <generic> *documentation*
 :syntax "transactionalp container => boolean"
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :examples '("(progn (prove:diag \"Running example for transactionalp.\")
                    (let ((container (cl-ds:become-transactional (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq))))
                      (prove:ok (cl-ds:mutablep container))
                      (prove:ok (cl-ds:transactionalp container))))")
 :returns "T if CONTAINER is transactional and NIL if it is not.")


(set-documentation
 'value <mechanics> <generic> *documentation*
 :syntax "value status => value"
 :arguments-and-values '((status "instance of modification status class."))
 :returns "Value that was present in the container at location before operation took place. Returns NIL if location was free.")


(set-documentation
 'found <mechanics> <generic> *documentation*
 :syntax "found status => boolean"
 :arguments-and-values '((status "instance of modification status class."))
 :returns "T if LOCATION was occupied before operation took place, NIL otherwise.")


(set-documentation
 'transaction <mechanics> <macro> *documentation*
 :syntax
 "transaction (binding instance) &body operations"

 :arguments-and-values
 '((binding "Symbol, will be bound to the transactionl instance.")
   (instance "Form that evaluates to container that will be changed in transactional way.")
   (operations "Body, containing operations that modify transactional instance"))

 :description
 "Utility macro. &body is executed in the lexical scope of transactional instance. After last operation, new instance is returned.")

(set-documentation
 '(setf at) <mechanics> <generic> *documentation*
 :description "Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."
 :arguments-and-values
 '((new-value "Value that shall be put in the container.")
   (container "Container that shall be modified.")
   (location "Location where container shall be modified."))
 :returns '("NEW-VALUE"
            "modification-status object as second value.")
 :notes "This is the destructive counterpart to the INSERT function.")

(set-documentation
 'mod-bind <mechanics> <macro> *documentation*
 :syntax
 "mod-bind (first &optional found value) values-form body"

 :arguments-and-values
 '((first "Symbol, will be bound to the first value returned by values-form.")
   (found "Symbol, this macro will construct symbol-macrolet that will expand to call (found status)")
   (value "Symbol, this macro will construct symbol-macrolet that will expand to call (value status)"))

 :description
 "This macro attempts to mimic multiple-value-bind syntax for modification operations performed on containers. All of those operations will return secondary object representing operation status that shall be bound in lexical environment and. Next, symbol-macrolets will be established, that inline found and value function calls on operation status (like with-accessors).")


(set-documentation
 'fundamental-container <mechanics> <class> *documentation*
 :description "Root class of containers.")


(set-documentation
 'fundamental-modification-operation-status <mechanics> <class> *documentation*
 :description "Base class of all fundamental modification status classes.")


(set-documentation
 'functional  <mechanics> <class> *documentation*
  :description "Object implements functional api.")


(set-documentation
 'mutable <mechanics> <class> *documentation*
  :description "Object implements mutable api.")


(set-documentation
 'transactional <mechanics> <class> *documentation*
 :description "Object implements mutable api in transactional way.")


(set-documentation
 'lazy <mechanics> <class> *documentation*
 :description "Functional object, with lazy implementation.")


(set-documentation
 'textual-error <mechanics> <error> *documentation*
 :description "Error with human readable text description.")


(set-documentation
 'invalid-argument <mechanics> <error> *documentation*
 :description "Error signaled if for some reason passed argument is invalid.")


(set-documentation
 'initialization-error <mechanics> <error> *documentation*
 :description "Error signaled when container can't be initialized.")


(set-documentation
 'argument-out-of-bounds <mechanics> <error> *documentation*
 :description "Error signaled when passed argument exceeds allowed bounds")


(set-documentation
 'initialization-out-of-bounds <mechanics> <error> *documentation*
 :description "Error signaled when container can't be initialized with value because value exceeds bounds.")


(set-documentation
 'not-implemented <mechanics> <error> *documentation*
 :description "Error signaled when not implemented functionality is accessed.")


(set-documentation
 'out-of-bounds <mechanics> <error> *documentation*
 :description "Error signaled when some value is out of expected bounds.")


(set-documentation
 'grow-bucket <mechanics> <generic> cl-ds:*documentation*
 :description "Generic, low level function used to grow all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
 :returns '("Bucket."
            "Instance of modification-operation-status."
            "Boolean. T if element was added into container, NIL otherwise. If NIL was returned, POSITION-MODIFICATION may assume that container was not changed, and therefore ignore returned bucket.")
 :notes "This function is not allowed to perform any side effects. See grow-bucket! for function that is allowed to do so.")


(set-documentation
 'shrink-bucket <mechanics> <generic> cl-ds:*documentation*
 :description "Generic, low level function used to shrink all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
 :returns '("Bucket. May also return nil to indicate empty bucket."
            "Instance of modification-operation-status."
            "Boolean. T if element was removed from container, NIL otherwise. If NIL was returned, POSITION-MODIFICATION may assume that container was not changed, and therefore ignore returned bucket.")
 :notes "This function is not allowed to perform any side effects. See shrink-bucket! for function that is allowed to do so.")


(set-documentation
 'make-bucket <mechanics> <generic> cl-ds:*documentation*
 :description "Generic, low level function used to create all sorts of buckets (like those in dictionaries). Buckets are modified by grow-bucket and shrink-bucket functions."
 :returns "New bucket. Precise type of bucket is not relevant to this level of abstraction and usually is defined in context of specific container."
 :notes "This function is not allowed to perform any side effects.")


(set-documentation
 'grow-bucket! <mechanics> <generic> cl-ds:*documentation*
 :description "Generic, low level function used to destructivly grow all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
 :returns '("New or passed bucket."
            "Instance of position modification status."
            "Boolean. T if returned bucket contains new elements, NIL otherwise. If NIL was returned, position-modification is free to ignore first value.")
 :side-effects "Can modify bucket.")


(set-documentation
 'shrink-bucket! <mechanics> <generic> cl-ds:*documentation*
 :description "Generic, low level function used to destructivly shrink all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
 :returns '("New or passed bucket. May return nil as empty bucket."
            "Instance of position modification status."
            "Boolean. T if elements were removed from BUCKET, NIL otherwise. If NIL was returned, position-modification is free to ignore first value.")
 :side-effects "Can modify bucket.")


(set-documentation
 'functional-function <mechanics> <class> cl-ds:*documentation*
 :description "Function that inherits this class is not allowed to perform any side effects.")


(set-documentation
 'destructive-function <mechanics> <class> cl-ds:*documentation*
 :description "Function that inherits this class is expected to perform side effects.")


(set-documentation
 'grow-function <mechanics> <class> cl-ds:*documentation*
 :description "Function will attempt to add new element to container.")


(set-documentation
 'shrink-function <mechanics> <class> cl-ds:*documentation*
 :description "Function will attempt to remove element from container.")


(set-documentation
 'insert-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of INSERT and (SETF AT).")


(set-documentation
 'update-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of UPDATE and UPDATE!.")


(set-documentation
 'add-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ADD and ADD!.")


(set-documentation
 'erase-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE and ERASE!.")


(set-documentation
 'erase-if-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE-IF and ERASE-IF!.")


(set-documentation
 'functional-insert-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of INSERT.")


(set-documentation
 'functional-update-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of UPDATE.")


(set-documentation
 'functional-add-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ADD.")


(set-documentation
 'functional-erase-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE.")


(set-documentation
 'insert!-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of INSERT.")


(set-documentation
 'update!-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of UPDATE.")


(set-documentation
 'add!-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ADD.")


(set-documentation
 'erase!-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE.")


(set-documentation
 'erase-if!-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE-IF!.")


(set-documentation
 'erase-if-function <mechanics> <class> cl-ds:*documentation*
 :description "Class of ERASE-IF.")
