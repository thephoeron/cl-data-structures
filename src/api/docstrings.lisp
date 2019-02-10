(in-package #:cl-data-structures)
(eval-always
  (scribble:configure-scribble :package :cl-data-structures)
  (named-readtables:in-readtable :scribble))

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function at
    (:syntax  ("for dictionary containers: at dictionary key => value found"
               "for everything else: at sequence location => value")
     :arguments
     (("CONTAINER" "Instance of subclass of fundamental-container.")
      ("LOCATION" "Where are we looking at? Key in hashtable, index of vector, etc."))

     :returns
     "In case of associative containers, second value informs if LOCATION was found in the CONTAINER (first value is NIL if element was not found).
 In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type ARGUMENT-OUT-OF-BOUNDS will be raised."

     :description
     "Obtain element stored at LOCATION in the CONTAINER."

     :examples
     [(let ((table (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
        (prove:diag "Testing example for AT function.")
        (multiple-value-bind (value found) (cl-ds:at table 'a)
          (prove:is value nil)
          (prove:is found nil))
        (setf (cl-ds:at table 'a) 1)
        (multiple-value-bind (value found) (cl-ds:at table 'a)
          (prove:is value 1)
          (prove:is found t)))]))

  (function dimensionality
    (:description "Number of dimensions in the object."))

  (function delay
    (:description "Simple lazy evaluated value. Used to eleminate costly creation of elements in the container by update-if."))

  (function make-from-traversable
    (:description "Creates container of CLASS with content from traversable."
     :returns "Instance of CLASS."
     :exceptional-situations "Varies, depending on the CLASS argument."
     :arguments ((class "Class of resulting container.")
                 (arguments "Arguments, as passed to usual make function.")
                 (traversable "Data that should be put in the result container."))))

  (function near
    (:description "Searches CONTAINER for elements that are at most MAXIMAL-DISTANCE away from item. Returns range of elements."
     :arguments ((container "Container searched for element.")
                 (item "Item to search around.")
                 (maximal-distance "Don't yield elements longer "))
     :examples [(let* ((data #(10 20 40 5 11 12 50 30 20 1 6 7 8 18 21 51 52 80 78))
                       (set (cl-ds:make-from-traversable
                             data
                             'cl-ds.ms.egnat:mutable-egnat-metric-set
                             #'= (alexandria:compose #'abs #'-) 'fixnum))
                       (near (cl-ds.alg:to-vector (cl-ds:near set 10 7))))
                  (prove:ok (every (lambda (x) (< 3 x 17)) near)))]))

  (function add
    (:description "Add NEW-VALUE into the CONTAINER at the LOCATION. Will not replace a value at LOCATION if it was already occupied."))

  (function add!
    (:arguments
     (("CONTAINER" "Instance that we intend to destructivly modify")
      ("LOCATION" "Place in the CONTAINER that we intend to change")
      ("NEW-VALUE" "Value that we intend to add into CONTAINER"))

     :description "Destructively add the NEW-VALUE into the CONTAINER at the LOCATION. Will not replace a value at LOCATION if it was already occupied."

     :returns ("CONTAINER"
               "Modification status object")

     :syntax "add! container location new-value => same-container status"

     :side-effects "If item was not found in the CONTAINER, destructivly transform CONTAINER."

     :notes "This is the destructive counterpart to the ADD function."))

  (function insert
    (:syntax "insert container location new-value => new-instance status"
     :description
     "Functional API: non-destructively insert the NEW-VALUE into the CONTAINER at the LOCATION. Will replace a value at the LOCATION if it was already occupied."

     :returns
     ("Instance of the same type as CONTAINER, with NEW-VALUE at LOCATION"
      "Modification status object.")

     :arguments
     (("container" "Instance of container.")
      ("location" "Designates place in returned instance that will be changed")
      ("new-value" "Value that will be inserted into returned instance"))

     :notes "This is the functional counterpart to the (SETF AT) function."

     :examples
     [(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
             (next-table (cl-ds:insert table 'a 5)))
        (prove:is (cl-ds:at next-table 'a) 5)
        (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql)))]))

  (function erase
    (:syntax "erase container location => new-instance status"
     :description
     "Functional API: non-destructively remove a element at the LOCATION from the CONTAINER."

     :returns
     ("Instance of the same type as CONTAINER, without value at LOCATION"
      "Modification status object.")

     :notes "This is the functional counterpart to the ERASE! function."

     :examples
     [(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
             (next-table (cl-ds:insert table 'a 5)))
        (prove:diag \"Running example for ERASE\")
        (prove:is (cl-ds:at next-table 'a) 5)
        (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql))
        (cl-ds:mod-bind (erased-table found value) (cl-ds:erase next-table 'a)
          (prove:ok found)
          (prove:is value 5)
          (prove:is (cl-ds:at erased-table 'a) nil)
          (prove:is (cl-ds:at next-table 'a) 5)))]

     :arguments
     (("CONTAINER" "Container that shall be modified.")
      ("LOCATION" "Designates place in returned instance that will be changed."))))

  (function erase-if
    (:syntax "erase-if container location condition => new-instance status"
     :description
     "Functional API: non-destructively remove element at LOCATION from the CONTAINER, only when CONDITION function returns true. CONDITION will be called with location that matches according to comparsion function used to construct container, and with a value."

     :returns
     ("Instance of the same type as CONTAINER, without value at LOCATION"
      "Modification status object.")

     :examples
     [(let* ((table (cl-ds.dicts.hamt::make-functional-hamt-dictionary #'sxhash #'eq))
             (next-table (cl-ds:insert (cl-ds:insert table 'a 5) 'b 6)))
        (prove:diag "Running example for ERASE-IF")
        (prove:is (cl-ds:at next-table 'a) 5)
        (prove:is (cl-ds:at table 'a) 5 :test (alexandria:compose #'null #'eql))
        (cl-ds:mod-bind (erased-table found value)
                        (cl-ds:erase-if next-table 'a
                                        (lambda (location value) (declare (ignore location))
                                          (evenp value)))
          (prove:ok (null found))
          (prove:is value nil)
          (prove:is (cl-ds:at erased-table 'a) 5)
          (prove:is (cl-ds:at next-table 'a) 5))
        (cl-ds:mod-bind (erased-table found value) (cl-ds:erase-if next-table 'b (lambda (location value) (declare (ignore location)) (evenp value)))
          (prove:ok found)
          (prove:is value 6)
          (prove:is (cl-ds:at erased-table 'b) nil)
          (prove:is (cl-ds:at next-table 'b) 6)))]

     :arguments
     (("CONTAINER" "Container that shall be modified.")
      ("LOCATION" "Designates place in returned instance that will be changed.")
      ("CONDITION" "Function of two arguments, should return boolean."))

     :notes "This is the functional counterpart to the ERASE-IF! function."))

  (function erase-if!
    (:syntax "erase-if! container location condition => same-instance status"
     :description
     "Functional API: destructively remove element at LOCATION from the CONTAINER, only when CONDITION function returns true. CONDITION will be called with location that matches according to comparsion function, and with value."

     :returns
     ("CONTAINER"
      "Modification status object.")

     :examples
     [(let* ((table (cl-ds.dicts.hamt::make-mutable-hamt-dictionary #'sxhash #'eq)))
        (setf (cl-ds:at table 'a) 5
              (cl-ds:at table 'b) 6)
        (prove:diag "Running example for ERASE-IF!")
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
          (prove:is (cl-ds:at erased-table 'b) nil)))]

     :arguments (("CONTAINER" "Container that shall be modified.")
                 ("LOCATION" "Designates place in returned instance that will be changed.")
                 ("CONDITION" "Function of two arguments, should return boolean."))

     :notes "This is the destructive counterpart to the ERASE-IF function."))

  (function erase!
    (:description "Mutable API: destructively remove a element at the LOCATION from the CONTAINER."
     :syntax "erase! container location => same-instance status"
     :returns ("CONTAINER" "Modification status object")
     :arguments
     (("container" "Instance that is intended to be destructivly modified.")
      ("location" "Location in the container that we want to modify by removing value."))
     :side-effects "If erase took place, destructivly transform CONTAINER."
     :notes "This is the destrucive counterpart to the ERASE function."))

  (function size
    (:syntax "size container => count"
     :description "How many elements the CONTAINER holds currently?"
     :arguments (("container" "instance that will be checked."))
     :returns "The number of elements in the container."
     :examples
     [(let ((container (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
        (prove:is (cl-ds:size container) 0)
        (setf (cl-ds:at container 'a) 1)
        (prove:is (cl-ds:size container) 1))]))

  (function update
    (:description
     "Functional API: if there is value at LOCATION in the CONTAINER return new instance with NEW-VALUE at LOCATION."

     :syntax
     "update container location new-value => new-instance status"

     :returns
     ("New container, with updated value at LOCATION if UPDATE took place"
      "Modification status object")

     :arguments
     (("container" "The instance that shall be transformed.")
      ("location" "The location in the container that we want to transform."))

     :notes "This is the functional counterpart to the UPDATE! function."))

  (function put
    (:arguments ((container "A subclass of fundamental-container")
                 (item "Object that should be added into CONTAINER"))
     :description "Functional API. Inserts new the ITEM into a new version of the CONTAINER. Relevant to sets and sequences."
     :returns "Modified container."
     :notes "This is the functional counterpart to the PUT! function."))

  (function put!
    (:arguments ((container "A subclass of fundamental-container")
                 (item "Object that should be added into CONTAINER"))
     :description "Destructive API. Inserts new the ITEM into the CONTAINER. Relevant to sets and sequences."
     :returns "CONTAINER"
     :notes "This is the destructive counterpart to the PUT function."))

  (function take-out
    (:description "Functional API. Removes one element from the CONTAINER. Relevant to sequences."
     :arguments ((container "Container that is about to be modified."))
     :returns ("New version of the container, without one element."
               "Modification status.")
     :notes "This is the functional counterpart to the TAKE-OUT! function."))

  (function take-out!
    (:description "Destructive API: removes one element from the CONTAINER. Relevant to sequences."
     :returns ("CONTAINER"
               "Modification status.")
     :arguments ((container "Container that is about to be modified"))
     :notes "This is the destructive counterpart to the TAKE-OUT function."))

  (function update-if
    (:description
     "Functional API: if there is value at LOCATION in the CONTAINER and supplied CONDITION-FN passes when called with present value return new instance with NEW-VALUE at LOCATION."

     :syntax
     "update-if container location new-value CONDITION-FN => new-instance status"

     :returns
     ("New container, with updated value at LOCATION if UPDATE took place"
      "Modification status object")

     :arguments
     ((container "The instance that shall be transformed.")
      (location "The location in the container that we want to transform.")
      (condition-fn "Function used to check if update should happen."))

     :notes "This is the functional counterpart to the UPDATE-IF! function."))

  (function update-if!
    (:description
     "Mutable API: if there is value at LOCATION in the CONTAINER and supplied CONDITION-FN passes when called with present value sets LOCATION and returns container."

     :syntax
     "update-if! container location new-value CONDITION-FN => container status"

     :returns
     ("CONTAINER"
      "Modification status object")

     :arguments
     ((container "The instance that shall be transformed.")
      (location "The location in the container that we want to transform.")
      (condition-fn "Function used to check if update should happen."))

     :notes "This is the destructive counterpart to the UPDATE-IF function."))

  (function update!
    (:description
     "Mutable API: If the LOCATION is taken in the CONTAINER, destructivly update it with the NEW-VALUE"

     :syntax
     "(update! container location new-value) -> same-container status"

     :returns
     ("CONTAINER"
      "Modification status object")

     :arguments
     (("container" "Container that shall be modified.")
      ("location" "Location in the container that we want to transform."))

     :notes "This is the destructive counterpart to the UPDATE function."))

  (function become-functional
    (:description
     "Transforms CONTAINER into functional variant."

     :syntax
     "become-functional container => functional-container"

     :returns
     "A instance implementing functional API. Content of returned instance is identical to the content of input CONTAINER."

     :arguments
     (("container" "Container that we want to transform into functional container."))

     :notes
     ("Side effects from destructive operations on CONTAINER may leak into returned instance."
      "Not all containers implement this function.")

     :side-effects
     "May vary, depending on type of the CONTAINER. Also, some (or all) parts of a internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into a returned instance."))

  (function chunked
    (:description "Returns either nil or forward range of sub-ranges that in total contain all elements of the RANGE. This function can be used to implement multithreaded functions, however, implementation of this is optional."))

  (function become-mutable
    (:description
     "Transforms the CONTAINER into a mutable variant."

     :syntax
     "become-mutable container => mutable-container"

     :returns
     "A instance implementing mutable API. Content of the returned instance is identical to the content of the input CONTAINER."

     :arguments
     (("container" "Container that we want to transform into mutable container."))

     :notes
     ("Side effects from destructive operations on CONTAINER may leak into returned instance."
      "Not all containers implement this function.")

     :side-effects
     "May vary, depending on type of the CONTAINER. Also, some (or all) parts of a internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance."))

  (function become-transactional
    (:description
     "Transforms CONTAINER into transactional variant."

     :syntax
     "become-transactional container => transactional-container"

     :returns
     "instance implementing mutable API. Content of returned instance is identical to the content of input CONTAINER."

     :arguments
     (("container" "Container that we want to transform into transactional container."))

     :notes
     ("Side effects from destructive operations on CONTAINER may leak into returned instance."
      "Not all containers implement this function.")

     :side-effects
     "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance."))

  (function become-lazy
    (:description
     "Transforms CONTAINER into lazy variant."

     :syntax
     "become-lazy container => lazy-container"

     :returns
     "instance implementing functional, lazy API. Content of returned instance is identical to the content of input CONTAINER."

     :arguments
     (("container" "Container that we want to transform into lazy container."))

     :notes
     ("Side effects from destructive operations on CONTAINER may leak into returned instance."
      "All containers that implement become-transactional, also implement become-lazy")

     :side-effects
     "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both the CONTAINER and a returned instance. Side effects from the mutable CONTAINER may leak into the returned instance."))

  (function mutablep
    (:syntax  ("mutablep mutable-container => t"
               "mutablep functional-container => nil")
     :arguments (("container" "Any subclass of fundamental-container"))

     :examples
     [(progn (prove:diag "Running example for mutablep.")
             (let ((mutable (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
               (prove:ok (cl-ds:mutablep mutable))
               (prove:ok (not (cl-ds:functionalp mutable))))
             (let ((functional (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
               (prove:ok (not (cl-ds:mutablep functional)))
               (prove:ok (cl-ds:functionalp functional))))]

     :returns "T if CONTAINER exposes mutable API and NIL if not."))

  (function functionalp
    (:syntax  ("(functionalp mutable-container) -> nil"
               "(functionalp functional-container) -> t")
     :arguments (("container" "Any subclass of fundamental-container"))
     :examples
     [(progn
        (prove:diag "Running example for functionalp.")
        (let ((mutable (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq)))
          (prove:ok (cl-ds:mutablep mutable))
          (prove:ok (not (cl-ds:functionalp mutable))))
        (let ((functional (cl-ds.dicts.hamt:make-functional-hamt-dictionary #'sxhash #'eq)))
          (prove:ok (not (cl-ds:mutablep functional)))
          (prove:ok (cl-ds:functionalp functional))))]
     :returns "T if CONTAINER exposes functional API and NIL if not."))

  (function transactionalp
    (:syntax "transactionalp container => boolean"
     :arguments (("container" "Any subclass of fundamental-container"))
     :examples
     [(progn
        (prove:diag "Running example for transactionalp.")
        (let ((container (cl-ds:become-transactional (cl-ds.dicts.hamt:make-mutable-hamt-dictionary #'sxhash #'eq))))
          (prove:ok (cl-ds:mutablep container))
          (prove:ok (cl-ds:transactionalp container))))]
     :returns "T if CONTAINER is transactional and NIL if it is not."))

  (function value
    (:syntax "value status => value"
     :arguments ((status "instance of modification status class."))
     :returns "Value that was present in the container at location before operation took place. Returns NIL if location was free."))

  (function changed
    (:syntax "changed status => boolean"
     :arguments ((status "instance of modification status class"))
     :returns "T if operation changed the container."))

  (function found
    (:syntax "found status => boolean"
     :arguments ((status "instance of modification status class."))
     :returns "T if LOCATION was occupied before operation took place, NIL otherwise."))

  (function transaction
    (:syntax
     "transaction (binding instance) &body operations"

     :arguments
     ((binding "Symbol, will be bound to the transactionl instance.")
      (instance "Form that evaluates to container that will be changed in transactional way.")
      (operations "Body, containing operations that modify transactional instance"))

     :description
     "Utility macro. &body is executed in the lexical scope of transactional instance. After last operation, new instance is returned."))

  (function (setf at)
    (:description "Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."
     :arguments ((new-value "Value that shall be put in the container.")
                 (container "Container that shall be modified.")
                 (location "Location where container shall be modified."))
     :returns ("NEW-VALUE"
               "modification-status object as second value.")
     :notes "This is the destructive counterpart to the INSERT function."))

  (function mod-bind
    (:syntax
     "mod-bind (first &optional found value) values-form body"

     :arguments
     ((first "Symbol, will be bound to the first value returned by values-form.")
      (found "Symbol, this macro will construct symbol-macrolet that will expand to call (found status)")
      (value "Symbol, this macro will construct symbol-macrolet that will expand to call (value status)"))

     :description
     "This macro attempts to mimic multiple-value-bind syntax for modification operations performed on containers. All of those operations will return secondary object representing operation status that shall be bound in lexical environment and. Next, symbol-macrolets will be established, that inline found and value function calls on operation status (like with-accessors)."))

  (type fundamental-container
    (:description "Root class of containers."))

  (type fundamental-modification-operation-status
    (:description "Base class of all fundamental modification status classes."))

  (type functional
    (:description "Object implements functional api."))

  (type mutable
    (:description "Object implements mutable api."))

  (type transactional
    (:description "Object implements mutable api in transactional way."))

  (type lazy
    (:description "Functional object, with lazy implementation."))

  (type textual-error
    (:description "Error with human readable text description."))

  (type invalid-argument
    (:description "Error signaled if for some reason passed argument is invalid."))

  (type initialization-error
    (:description "Error signaled when container can't be initialized."))

  (type argument-out-of-bounds
    (:description "Error signaled when passed argument exceeds allowed bounds"))

  (type initialization-out-of-bounds
    (:description "Error signaled when container can't be initialized with value because value exceeds bounds."))

  (type not-implemented
    (:description "Error signaled when not implemented functionality is accessed."))

  (type unexpected-argument
    (:description "Error signaled when passed argument was not expected."))

  (type out-of-bounds
    (:description "Error signaled when some value is out of expected bounds.")))
