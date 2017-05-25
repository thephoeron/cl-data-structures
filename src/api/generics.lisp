(in-package :cl-data-structures)


(defgeneric at (container location)
  (:documentation "Obtain element stored at LOCATION in the CONTAINER. This function will @b(return) one or two values, depending on the CONTAINER.
  In case of associative containers, second value informs if element was found (first value is nil if element was not found).
  In case of non-associtive containers (e.g. vectors), the function returns value under LOCATION if LOCATION is valid, otherwise condition of type TODO will be raised.

  @b(Arguments and values:)
  @begin(list)
  @item(container -- instance of subclass of fundamental-container)
  @item(location -- where are we looking at? Key in hashtable, index of vector, etc)
  @end(list)

  @b(Side effects:) None."))


(defgeneric (setf at) (new-value container location)
  (:documentation "@b(Mutable API:) Destructively insert/replace element in the CONTAINER at LOCATION with NEW-VALUE."))


(defgeneric add (container location new-value)
  (:documentation "@b(Functional API:) Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will return three values: first one is the new container, second one is boolean informing if item was already in the container, third is value found in the container (or nil).

  @b(Side effects:) None"))


(defgeneric add! (container location new-value)
  (:documentation "@b(Mutable API:) Destructively add NEW-VALUE into CONTAINER at LOCATION. This will do nothing if container already contains value under LOCATION. Will @b(return) three values:
  @begin(list)
  @item(first -- CONTAINER)
  @item(second -- boolean informing if item was found in the container (in other words: nil if item was sucessfully added))
  @item(third -- value found in the container (or nil, if it was not found))
  @end(list)
  @b(Side effencts:) If item was not found in the container, destructivly transform CONTAINER."))


(defgeneric insert (container location new-value)
  (:documentation "@b(Functional API:) Non-destructively insert NEW-VALUE into CONTAINER at LOCATION. Will replace element value at LOCATION if it was already occupied. Will return up to three values: new container, boolean to inform user if element was already in the container and old value AT location (or nil if it was not present). Essentially purely functional (SETF (AT CONTAINER) NEW-VALUE).

 @b(Side effects:) None"))


(defgeneric erase (container location)
  (:documentation "@b(Functional API:) Non-destructively remove element at LOCATION from the CONTAINER. Will @b(return) three values:
                   @begin(list)
                   @item(first -- instance of container)
                   @item(second -- if item was found (or: if erase took place))
                   @item(third -- old value assigned to the LOCATION in the container. NIL if LOCATION was not found)
                   @end(list)

  @b(Side effects:) None"))


(defgeneric erase! (container location)
  (:documentation "@b(Mutable API:) Destructively remove element at LOCATION from the CONTAINER. Will @b(return) three values:
                   @begin(list)
                   @item(first -- CONTAINER)
                   @item(second -- boolean informing if item was found (and erased) from container)
                   @item(third -- value that was assigned to the LOCATION (nil if it was not found))
                   @end(list)
                   @b(Side effects:) If erase took place, destructivly transform CONTAINER"))


(defgeneric size (container)
  (:documentation "How many elements CONTAINER holds currently?

  @b(Side effects:) None"))


(defgeneric update (container location new-value)
  (:documentation "@b(Functional API:) If LOCATION is taken in the CONTAINER, update it with NEW-VALUE. @b(Returns) three values:
   @begin(list)
   @item(first -- new CONTAINER with updated LOCATION)
   @item(second -- t if update took place nil otherwise)
   @item(third -- previous value).
   @end(list)
   @b(Side effects:) None"))


(defgeneric update! (container location new-value)
  (:documentation "@b(Mutable API:) Destructive version of UPDATE. If LOCATION is taken in the CONTAINER, destructivly update it with NEW-VALUE. @b(Returns) three values:
   @begin(list)
   @item(first -- CONTAINER)
   @item(second -- t if update took place nil otherwise)
   @item(third -- previous value).
   @end(list)
   @b(Side effects:) If update took place, destructivly transform CONTAINER"))


(defgeneric become-functional (container)
  (:method ((container functional)) container)
  (:documentation "@b(Returns) new CONTAINER presenting functional API of CONTAINER. Content of returned CONTAINER is identical to the input CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Side effects:) May vary, depending on type of CONTAINER. Also, some (or all) parts of internal representation are shared beetween both instances. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric become-mutable (container)
  (:method ((container mutable)) container)
  (:documentation "@b(Returns) new CONTAINER presenting mutable API of CONTAINER. Content of returned CONTAINER is identical to the input CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Side effects:) May vary, depending on type of CONTAINER. Also, some (or all) parts of internal representation are shared beetween both instances. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric become-transactional (container)
  (:method ((container transactional)) container)
  (:documentation "@b(Returns) transactional instance. Transactional instance exposes mutable api, but any side effects peformed on instance are isolated to the said instance.

  @b(Note:) This does not prevent destructive operations to leak informations into returned CONTAINER.

  @b(Note:) Not all containers implement this function.

  @b(Side effects:) May vary, depending on type of CONTAINER. Also, some (or all) parts of internal representation are shared beetween both instances. Side effects from mutable CONTAINER may leak into functional CONTAINER."))


(defgeneric mutable-p (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes mutable API.

  @b (Side effects:) None"))


(defgeneric functional-p (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "Will @b(return) T if CONTAINER exposes functional API.

                  @b(Side effects:) None"))

(defgeneric transactional-p (container)
  (:method ((container transactional)) t)
  (:method ((container fundamental-container)) nil)
  (:documentation "@b(Returns) T if container is transactional and @b(returns) nil if it is not.

  @b(Side effects:) None"))


(defgeneric empty-p (container)
  (:method ((container fundamental-container)) (zerop (size container)))
  (:documentation "@b(Returns) T if container is empty and @b(returns) NIL if there is something in it. All containers start as empty

  @b(Side effects:) None"))
