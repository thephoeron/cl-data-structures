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
 In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type TODO will be raised."

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
 "become-functional container => mutable-container"

 :returns
 "instance implementing transactional API. Content of returned instance is identical to the content of input CONTAINER."

 :arguments-and-values
 '(("container" "Container that we want to transform into transactional container."))

 :notes
 '("Side effects from destructive operations on CONTAINER may leak into returned instance."
   "Not all containers implement this function.")

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
 'mod-bind <mechanics> <macro> *documentation*
 :syntax
 "mod-bind (first &optional found value) values-form body"

 :arguments-and-values
 '((first "Symbol, will be bound to the first value returned by values-form.")
   (found "Symbol, this macro will construct symbol-macrolet that will expand to call (found status)")
   (value "Symbol, this macro will construct symbol-macrolet that will expand to call (value status)"))

 :description
 "This macro attempts to mimic multiple-value-bind syntax for modification operations performed on containers. All of those operations will return secondary object representing operation status.")


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
