(in-package #:cl-data-structures.documentation)
(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.protocol
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)


(with-names (cl-lore.extensions.documentation.api:<documentation-names>)
  (chunk *cl-data-structures*
    (level [section] @label{cl-ds internals}
      @title{Internal details}
      [:text This section explains parts of system design and low-level elements that shouldn't bother user of the library.  However, they are critical for contributors, and some people just want to know and so shall be delivered.]
      (level [section]
        @title{Docstrings and docstample}
        [:text Majority of docstrings are constructed and set outside function definitions, in a separate file. This has been done in such way to not clutter the code with information that is easy to obtain by any SLIME user. Furthermore this allows to build doctsrings with the code itself. This makes easier to ensure common, uniform style of docstrings. It may also perform various operations on docstrings. To handle this, a separate project called docstample was created. Hopefully, this tool can be augmented in the future to handle tasks such as automatic validation of examples. This is in fact what is done in this project. Examples are written as unit tests, so they can be checked if they actually work (so users are not getting angry, hopefully). Also, building docstrings from structured input eliminates need for parsing (a language other than lisp itself) to build a nicely formatted output (which is nice).])

      (level [section]
        @title{POSITION-MODIFICATION metaprotocol}
        [:text Interestingly enough, generic functions performing modification on containers are in fact just wrappers around a low level position-modification function. This may seem odd, but has rather simple motivation. Consider building a nested data structures (sequence of dictionaries for instance). Performing destructive modifications is simple enough, however, when we assume that both top level and bottom level structures are purely functional, this becomes very tricky. One may be tempted to create a set of higher order functions that can be combined with each other to handle such structures. This way, the function combinations mimics structure combinations. In essence, the PERFORM-POSITION-MODIFICATION does exactly that. Because passing multiple callbacks is rather tiresome on the long run, instead, it dispatches its logic on the class of the function itself. In addition to the above, this approach reduces code duplication when implementing additional, convenience functions (like UPDATE-IF).]

        (sequence-graph
         '("User" "API function" "POSITION-MODIFICATION")
         (seq
          :block
          '(:axis-name "User")
          (seq
           :sync
           '(:axis-name "API function" :name "User modifies instance")
           (seq
            :block
            '(:axis-name "API function")
            (seq
             :sync
             '(:axis-name "POSITION-MODIFICATION" :name "Implementation of CL-DATA-STRUCTURES")
             (seq :block '(:axis-name "POSITION-MODIFICATION")))))))

        [:text This is made possible by the fact that Generic Functions in ,(lisp) are in fact objects of their own classes. By creating custom classes, ,(lisp) programmer may actually assign behavior as a method of function (as peculiar as it may sound). This essentially means that the INSERT function object satisfies protocol that allows the programmer to query it about itself (for example: is it modification or query?), explains how to handle existing key (see ADD and UPDATE for instance) and so one, without a need of additional object at all. In fact, some of the functions are implementations with rather complex class inheritance!]
        [:text Because understanding how these protocol works is useful when implementing generic and reusable algorithms, it is beneficial to explain it here, even it is not required for simple use cases.]

        (level [section]
          @title{How does it work?}
          @text{A function that is supposed to modify data structure calls the POSITION-MODIFICATION, passing itself as an argument. POSITION-MODIFICATION is a generic function with multiple implementations dispatched on the class of the passed function. For instance, INSERT is of class INSERT-FUNCTION which inherits the GROW-FUNCTION, while ERASE is of class ERASE-FUNCTION which in turns inherits the SHRINK-FUNCTION. Therefore, typically container implements POSITION-MODIFICATION generic function for GROW-FUNCTION and SHRINK-FUNCTION. Next, it is assumed that passed function fulfills a particular contract. For instance (to stay within the established boundaries) INSERT-FUNCTION should be applicable with methods GROW-BUCKET and MAKE-BUCKET while ERASE-FUNCTION needs to know how to SHRINK-BUCKET when used with dictionaries. A list of required functions is provided for every container, along with a description of the function itself in the API reference section.}
          @text{In a summary, generic functions like ADD can be considered high level. Direct classes of those functions can be considered medium level, while SHRINK-FUNCTION and GROW-FUNCTION are the lowest levels of the system. Adding new high level and medium level objects is encouraged and allowed, however, low-level functionality should be considered to be primitives.})))))
