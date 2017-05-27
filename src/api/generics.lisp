(in-package #:cl-data-structures)


(defgeneric at (container location)
  (:documentation "Obtain element stored at LOCATION in the CONTAINER. This function will @b(return) one or two values, depending on the CONTAINER.
  In case of associative containers, second value informs if element was found (first value is nil if element was not found).
  In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type TODO will be raised.

  @b(Arguments and values:)
  @begin(list)
  @item(CONTAINER -- Instance of subclass of fundamental-container.)
  @item(LOCATION -- Where are we looking at? Key in hashtable, index of vector, etc.)
  @end(list)

  @b(Side effects:) None."))


(defgeneric (setf at) (new-value container location)
  (:documentation "@b(Mutable API:) Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."))


(defgeneric add (container location new-value)
  (:documentation "@b(Functional API:) Attempts to non-destructively add NEW-VALUE into CONTAINER at LOCATION. Will not replace value at LOCATION if it was already occupied. Will @b(return) three values:
  @begin(list)
  @item(first -- Instance of the same type as CONTAINER. If add took place it shall contain NEW-VALUE at LOCATION.)
  @item(second -- T if LOCATION was found in the container (in other words: nil if item was sucessfully added), NIL otherwise.)
  @item(third -- Value at LOCATION in the CONTAINER (or NIL, if it was not found).)
  @end(list)

  @b(Arguments and values:)
  @begin(list)
  @item(CONTAINER -- Instance that we intend to modify.)
  @item(LOCATION -- Place where NEW-VALUE shall be added.)
  @item(NEW-VALUE -- Value that we intend to add into returned instance.)
  @end(list)

  @b(Side effects:) None."))


(defgeneric add! (container location new-value)
  (:documentation "@b(Mutable API:) Destructively add NEW-VALUE into CONTAINER at LOCATION. Will not replace value at LOCATION if it was already occupied. Will @b(return) three values:
  @begin(list)
  @item(first -- CONTAINER.)
  @item(second -- Boolean informing if LOCATION was found in the container (in other words: NIL if item was sucessfully added).)
  @item(third -- Value in the CONTAINER at LOCATION before destructive modification took place (or nil, if LOCATION was not found).)
  @end(list)

  @b(Arguments and values:)
  @begin(list)
  @item(CONTAINER -- instance that we intend to destructivly modify)
  @item(LOCATION -- place in the CONTAINER that we intend to change)
  @item(NEW-VALUE -- value that we intend to add into CONTAINER)
  @end(list)

  @b(Side effects:) If item was not found in the container, destructivly transform CONTAINER."))


(defgeneric insert (container location new-value)
  (:documentation "@b(Functional API:) Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will replace element value at LOCATION if it was already occupied. Will @b(return) three values:

  @begin(list)
  @item(first -- instance of the same type as CONTAINER, with NEW-VALUE at LOCATION)
  @item(second -- T if LOCATION was found in the CONTAINER, NIL otherwise)
  @item(third -- value in the CONTAINER at LOCATION (or NIL, if LOCATION was not found))
  @end(list)

  @b(Arguments and values:)
  @begin(list)
  @item(CONTAINER -- TODO)
  @item(LOCATION -- designates place in container that will be changed)
  @item(NEW-VALUE -- value that will be inserted into returned instance)
  @end(list)

 @b(Side effects:) None"))


(defgeneric erase (container location)
  (:documentation "@b(Functional API:) Non-destructively remove element at LOCATION from the CONTAINER. Will @b(return) three values:
                   @begin(list)
                   @item(first -- instance of the same type as CONTAINER, without any item at LOCATION)
                   @item(second -- T if LOCATION was found in the CONTAINER (or: if erase took place), NIL otherwise)
                   @item(third -- value at LOCATION in the CONTAINER. (NIL if LOCATION was not found))
                   @end(list)

  @b(Side effects:) None."))


(defgeneric erase! (container location)
  (:documentation "@b(Mutable API:) Destructively remove element at LOCATION from the CONTAINER. Will @b(return) three values:
                   @begin(list)
                   @item(first -- CONTAINER)
                   @item(second -- T if LOCATION was found in the CONTAINER (or: if erase took place), NIL otherwise)
                   @item(third -- erased value at LOCATION in the CONTAINER (NIL if LOCATION was not found))
                   @end(list)

  @b(Arguments and values:)
  @begin(list)
  @item(CONTAINER -- container that is intended to be destructivly modified)
  @item(LOCATION -- place in the container that we intend to remove)
  @end(list)

                   @b(Side effects:) If erase took place, destructivly transform CONTAINER"))


(defgeneric size (container)
  (:documentation "How many elements CONTAINER holds currently?

  @b(Side effects:) None"))


(defgeneric update (container location new-value)
  (:documentation "@b(Functional API:) If there is value at LOCATION in the CONTAINER, return new instance with NEW-VALUE at LOCATION. @b(Returns) three values:
   @begin(list)
   @item(first -- new CONTAINER with updated value at LOCATION)
   @item(second -- t if update took place, nil otherwise)
   @item(third -- previous value found in the CONTAINER at LOCATION)
   @end(list)
   @b(Side effects:) None"))


(defgeneric update! (container location new-value)
  (:documentation "@b(Mutable API:) Destructive version of UPDATE. If LOCATION is taken in the CONTAINER, destructivly update it with NEW-VALUE. @b(Returns) three values:
   @begin(list)
   @item(first -- CONTAINER)
   @item(second -- t if update took place nil otherwise)
   @item(third -- previous value (or nil if LOCATION is not present in the CONTAINER))
   @end(list)

   @b(Side effects:) If update took place, destructivly transform CONTAINER"))


(defgeneric become-functional (container)
  (:method ((container functional)) container)
  (:documentation "@b(Returns) instance implementing functional API. Content of returned instance is identical to the content of input CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Note:) Side effects from destructive operations on CONTAINER may leak into returned instance.

  @b(Side effects:) May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into returned instance."))


(defgeneric become-mutable (container)
  (:method ((container mutable)) container)
  (:documentation "@b(Returns) instance implementing mutable API. Content of returned instance is identical to the content of input CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Side effects:) May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric become-transactional (container)
  (:method ((container transactional)) container)
  (:documentation "@b(Returns) transactional instance implementing mutable API. Operations performed on returned instance are guaranteed to not leak side effects outside of it. Content of returned instance is identical to the content of input CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Note:) Side effects from destructive operations on CONTAINER may leak into returned instance.

  @b(Side effects:) May vary, depending on type of the CONTAINER. Also, some (or all) parts of internal representation can be shared between both CONTAINER and returned instance. Therefore side effects from mutable CONTAINER may leak into returned instance."))


(defgeneric mutablep (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes mutable API and NIL if not.

  @b(Side effects:) None"))


(defgeneric functionalp (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes functional API and NIL if not.

   @b(Side effects:) None"))


(defgeneric transactionalp (container)
  (:method ((container transactional)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "@b(Returns) T if container is transactional and NIL if it is not.

  @b(Side effects:) None"))


(defgeneric emptyp (container)
  (:method ((container fundamental-container)) (zerop (size container)))
  (:documentation "@b(Returns) T if container is empty and @b(returns) NIL if there is something in it.

  @b(Side effects:) None"))
