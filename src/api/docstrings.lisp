(in-package #:cl-data-structures)


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
 "Obtain element stored at LOCATION in the CONTAINER.")


(set-documentation
 'add <mechanics> <generic> *documentation*
 :syntax "add dictionary key value => new-dictionary-instance status"
 :arguments-and-values
 '(("CONTAINER" "Instance that we want to modify.")
   ("LOCATION" "Place where NEW-VALUE shall be added.")
   ("NEW-VALUE" "Value that we intend to add into returned instance."))

 :returns '("Instance of the same type as CONTAINER. If add took place it shall contain NEW-VALUE at LOCATION."
            "Modification status object.")

 :description "Functional API: attempts to non-destructively add NEW-VALUE into CONTAINER at LOCATION. Will not replace value at LOCATION if it was already occupied.")


(set-documentation
 'add! <mechanics> <generic> *documentation*
 :arguments-and-values
 '(("CONTAINER" "Instance that we intend to destructivly modify")
   ("LOCATION" "Place in the CONTAINER that we intend to change")
   ("NEW-VALUE" "Value that we intend to add into CONTAINER"))

 :description "Destructively add NEW-VALUE into CONTAINER at LOCATION. Will not replace value at LOCATION if it was already occupied."

 :returns '("CONTAINER"
            "Modification status object")

 :syntax "add! container location new-value => same-container status"

 :side-effects "If item was not found in the CONTAINER, destructivly transform CONTAINER.")


(set-documentation
 'insert <mechanics> <generic> *documentation*
 :syntax "insert container location new-value => new-instance status"
 :description
 "Functional API: non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will replace element value at LOCATION if it was already occupied."

 :returns
 '("Instance of the same type as CONTAINER, with NEW-VALUE at LOCATION"
   "Modification status object.")

 :arguments-and-values
 '(("container" "TODO")
   ("location" "designates place in returned instance that will be changed")
   ("new-value" "Value that will be inserted into returned instance")))


(set-documentation
 'erase <mechanics> <generic> *documentation*
 :syntax "erase container location new-value => new-instance status"
 :description
 "Functional API: non-destructively remove element at LOCATION from the CONTAINER."

 :returns
 '("Instance of the same type as CONTAINER, without value at LOCATION"
   "Modification status object.")

 :arguments-and-values
 '(("CONTAINER" "TODO")
   ("LOCATION" "designates place in returned instance that will be changed.")))


(set-documentation
 'erase! <mechanics> <generic> *documentation*
 :description "Mutable API: destructively remove element at LOCATION from the CONTAINER."
 :syntax "erase! container location new-value => same-instance status"
 :returns '("CONTAINER" "Modification status object")
 :arguments-and-values
 '(("container" "Instance that is intended to be destructivly modified.")
   ("location" "Location in the container that we want to modify by removing value."))
 :side-effects "If erase took place, destructivly transform CONTAINER.")


(set-documentation
 'size <mechanics> <generic> *documentation*
 :syntax "size container => count"
 :description "How many elements CONTAINER holds currently?"
 :arguments-and-values '(("container" "instance that will be checked."))
 :returns "number of elements in the container.")


(set-documentation
 'update <mechanics> <generic> *documentation*
 :description
 "Functional API: if there is value at LOCATION in the CONTAINER, return new instance with NEW-VALUE at LOCATION."

 :syntax
 "update container location new-value => new-instance status"

 :returns
 '("New container, with updated value at LOCATION if UPDATE took place"
   "Modification status object")

 :arguments-and-values
 '(("container" "Container that shall be transformed.")
   ("location" "Location in the container that we want to transform.")))


(set-documentation
 'update! <mechanics> <generic> *documentation*
 :description
 "Mutable API: If LOCATION is taken in the CONTAINER, destructivly update it with NEW-VALUE"

 :syntax
 "(update! container location new-value) -> same-container status"

 :returns
 '("CONTAINER"
   "Modification status object")

 :arguments-and-values
 '(("container" "Container that shall be modified.")
   ("location" "Location in the container that we want to transform.")))


(set-documentation
 'become-functional <mechanics> <generic> *documentation*
 :description
 "Transforms CONTAINER into functional variant."

 :syntax
 "become-functional container => functional-container"

 :returns
 "instance implementing functional API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into functional container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into returned instance.")


(set-documentation
 'become-mutable <mechanics> <generic> *documentation*
 :description
 "Transforms CONTAINER into mutable variant."

 :syntax
 "become-mutable container => mutable-container"

 :returns
 "instance implementing mutable API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into mutable container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

 :side-effects
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into returned instance.")


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
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into returned instance.")


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
 "May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into returned instance.")


(set-documentation
 'mutablep <mechanics> <generic> *documentation*
 :syntax '("mutablep mutable-container => t"
           "mutablep functional-container => nil")
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :returns "T if CONTAINER exposes mutable API and NIL if not.")


(set-documentation
 'functionalp <mechanics> <generic> *documentation*
 :syntax '("(functionalp mutable-container) -> nil"
           "(functionalp functional-container) -> t")
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :returns "T if CONTAINER exposes functional API and NIL if not.")


(set-documentation
 'transactionalp <mechanics> <generic> *documentation*
 :syntax "transactionalp container => boolean"
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :returns "T if CONTAINER is transactional and NIL if it is not.")


(set-documentation
 'emptyp <mechanics> <generic> *documentation*
 :syntax "emptyp container => boolean"
 :arguments-and-values '(("container" "Any subclass of fundamental-container"))
 :returns "T if CONTAINER is empty and NIL if it is not.")


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
 :returns '('new-value
            "modification-status object as second value."))

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
 'textual-error <mechanics> <class> *documentation*
 :description "Error with human readable text description.")


(set-documentation
 'invalid-argument <mechanics> <class> *documentation*
 :description "Error signaled if for some reason passed argument is invalid.")


(set-documentation
 'initialization-error <mechanics> <class> *documentation*
 :description "Error signaled when container can't be initialized.")


(set-documentation
 'initialization-error <mechanics> <class> *documentation*
 :description "Error signaled when variable exceeds allowed bounds.")


(set-documentation
 'argument-out-of-bounds <mechanics> <class> *documentation*
 :description "Error signaled when passed argument exceeds allowed bounds")


(set-documentation
 'initialization-out-of-bounds <mechanics> <class> *documentation*
 :description "Error signaled when container can't be initialized with value because value exceeds bounds.")


(set-documentation
 'not-implemented <mechanics> <class> *documentation*
 :description "Error signaled when not implemented functionality is accessed.")


(set-documentation
 'out-of-bounds <mechanics> <class> *documentation*
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
            "Boolean. T if element was added into container, NIL otherwise. If NIL was returned, POSITION-MODIFICATION may assume that container was not changed, and therefore ignore returned bucket.")
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
            "Boolean. T if returned bucket contains new elements, NIL otherwise. If NIL was returned, position-modification is free to ignore first value.")
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
