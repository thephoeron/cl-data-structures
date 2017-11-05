(in-package #:cl-data-structures.documentation)
(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.protocol
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)


(with-names (cl-lore.extensions.documentation.api:<documentation-names>)
  (chunk *cl-data-structures*
    (level [section] @label{cl-ds intro}
      @title{Overview}
      [:text Cl-data-structures is a portable collection of data structures for ,(lisp). The design goals of this library are following:]

      (level [list]
        @item{Uniform -- Data structures that are used for specific task should have a common interface. The user should just know how to use dictionary, and not some specific implementation of it.}
        @item{Complete -- This package intends to be the definitive ,(lisp) data structures collection, containing both functional and mutable structures, for every use case possible.}
        @item{Universal -- There should be no limitations on when this library is useful.}
        @item{Stable -- The API should be backward compatible. Breaking existing software is not acceptable.})

      [:text To achieve these goals, the package cl-data-structures contains the common API. Various implementations of that API have their own, separate packages. Implementations are divided into few categories:]
      (level [list]
        [:item Dicts (short for dictionaries) -- Data structures that map keys to values. All in the package cl-ds.dicts.])

      (level [section]
        @title{Conventions}
        [:text Data structure types are not hidden under generic interface name (like "std::unordered_map") but instead are directly exposed to the user. Users are encouraged to read the implementation details section of this manual to decide what data structure implementation works best for the specific use case. Destructive (in the sense of capable of mutating data passed as an argument) functions follow scheme style of adding '!' as a suffix (so we have GF ADD! that is the destructive version of ADD). There are exceptions to this rule, namely SETF functions. According to above, there should be a generic function called INSERT!, but alas, that's not the case. Instead, there is the (SETF AT) API function that does the thing one would expect from INSERT!. In addition to this difference, SETF functions are expected to return value of modified place, and not the container itself. Therefore, that's what (SETF AT) does to maintain style cohesive with ,(lisp).])

      (level [section]
        @title{Key concepts}

        [:text Inspection of the CL-DATA-STRUCTURE source code may reveal a few interesting patterns. Listed below in their own sections.]

        (level [section]
          @title{Signaling errors}
          [:text The Cl-data-structures approach to signaling errors can be summarized with two points:]

          @begin{list}
          @item{Signal error, only if without a doubt, error has occurred.}
          @item{Signal only well structured and documented errors.}
          @end{list})

        [:text To fulfill those requirements, library defines it's own hierarchy of conditions, with each error signaled only in a very specific scenario. For instance, there is the INITIALIZATION-OUT-OF-BOUNDS error, that will be signaled only if the user attempts to initialize the class with a value that exceeds accepted bounds, as described in a relevant reference. Such error also usually points to documentation that describes why this error was signaled and provides information on what argument triggered signaling error, and what are an accepted bounds. This is done such way primarily to make both learning and debugging as easy as possible. In addition, it also makes automatic handling of errors actually possible. The user of this library is encouraged to take a look at the error hierarchies, as laid out in this manual API reference section.]
        [:text It is also important to point out, that CL-DATA-STRUCTURES attempts to explicitly document every possible error that can be raised by every function. If an unexpected error occurs, it may and should be considered bug of manual itself, and treated as such (namely: ,[:emph reported and fixed]).]

        (level [section]
          @title{Modification Status}
          [:text ,(lisp) standard says that GETHASH function returns two values: the first being value itself (or NIL if the key was not found in the hashtable), while the second is a boolean that is T if the key was found. This is reasonable approach also taken by the AT function. However, (SETF GETHASH) returns just one value. This is problematic because information about previous value is lost. To counter this problem, all modification functions return a MODIFICATION-STATUS object as a second value. This object grants access to information on container state (if previous value was found if a container has been changed) using reader functions. It also may be implemented in different ways, which is beneficial in situations when obtaining value in a strict way is not ideal (think about lazy evaluation). To simplify using this object, the MOD-BIND macro is introduced (syntactic sugar that mimics MULTIPLE-VALUE-BIND syntax).])

        (level [section]
          @title{Trait classes}
          [:text The class hierarchy of CL-DATA-STRUCTURES objects may appear to be complex, and somewhat convoluted, but there is a reason for that. CL-DATA-STRUCTURES defines multiple slotless classes, like the FUNCTIONAL. Those classes are used as a way to attach a set of information about the container contract. In case of functional containers, that would be: do not allow any sort of mutable operations, in case of dictionaries: mapping keys to values. Thanks to this programmer may write code that dispatches logic according to the behavior of the container. This manual contains a description of each trait, and container class documentation contains information about inherited classes.]
          [:text Some of the trait classes are used to represents variants.])

        (level [section]
          @title{Variants}
          [:text Most of the cl-data-structures containers are available in few variants. The purpose of those is to aid the programmer in avoiding errors that may occur when mixing functional and destructive operations, while still providing access to both. To understand the motivation behind this decision, consider other possible approaches that could be taken instead.]
          [:text You can just allow arbitrary changes happening on any level. This usually gives you the best raw performance, but at the high cost: a state that you are mutating can be shared in an arbitrary way. If execution of your code is interrupted, changes made in the container are preserved, even if they represent incoherent or invalid data. You need to clean it up yourself. Changes are also shared between threads, which means that you will need to also share some mutex to protect your data from races. This kind of containers are called ,[:emph mutable] in this library.]
          [:text ,[:emph Functional] containers do not suffer from the same problems. Every operation that would change the existing state in a mutable container will instead return new container, with changes visible only there. This, however, has another limitation: copying is costly. Although copying the whole structure is usually not required, we still need to copy at least parts of it.]
          [:text ,[:emph Transactional] containers represent a compromise between those two opposite approaches. Transactional containers implement mutable API in a distinct way: instead of performing destructive operations in an arbitrary way, we are trying to isolate changes so they will be visible only in the instance that we passed into the method. This allows us to achieve a compromise between safety, simplicity, and speed.]
          [:text All containers with transactional variant available can be also used as functional, lazy containers. Those containers reduce consing that troubles functional containers by grouping all modification operations and performing hidden, destructive modification of transactional containers in the last possible moment. Since all those fancy functional data structures are just trees with the copy on write semantics it improves performance a little bit.]
          [:text A container can be converted between ,[:emph functional], ,[:emph transactional] and ,[:emph mutable] variant using become methods. However, not every container is available in all three variants. It is also important to remember that become methods have a limited set of guarantees. For instance: BECOME-TRANSACTIONAL guaranties that changes in the returned instance won't leak outside of that instance, but not that destructive changes in the original instance can't leak into it. Same applies for the BECOME-FUNCTIONAL method. Be careful and keep this in mind.]))))

  (chunk *cl-data-structures*
    @begin{section} @label{cl-ds API}
    @title{API Reference}
    @text{This section contains reference of all the functions, classes and macros provided by this library to the user.}

    @begin{section}
    @title{Common API}
    @text{Following is the most abstract API of cl-data-structures library.}

    @begin{section}
    @title{Generic Functions}

    (level [section]
      @title{Query Functions}
      @text{Following generic functions check state of the container and are not allowed to change it.}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:at]
      @docgeneric['cl-ds:size]
      @docgeneric['cl-ds:mutablep]
      @docgeneric['cl-ds:transactionalp]
      @docgeneric['cl-ds:functionalp]

      @end{documentation})

    (level [section]
      @title{Functional modification API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:insert]
      @docgeneric['cl-ds:add]
      @docgeneric['cl-ds:update]
      @docgeneric['cl-ds:erase]
      @docgeneric['cl-ds:erase-if]
      @end{documentation})

    (level [section]
      @title{Mutable modification API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['(setf cl-ds:at)]
      @docgeneric['cl-ds:add!]
      @docgeneric['cl-ds:update!]
      @docgeneric['cl-ds:erase!]
      @docgeneric['cl-ds:erase-if!]
      @end{documentation})

    (level [section]
      @title{Variants API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:become-functional]
      @docgeneric['cl-ds:become-mutable]
      @docgeneric['cl-ds:become-transactional]
      @docgeneric['cl-ds:become-lazy]
      @end{documentation})

    @end{section}

    (level [section]
      @title{Macros}
      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docmacro['cl-ds:mod-bind]
      @end{documentation})

    (level [section]
      @title{Classes}
      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docclass['cl-ds:fundamental-container]
      @docclass['cl-ds:fundamental-modification-operation-status]
      @docclass['cl-ds:functional]
      @docclass['cl-ds:mutable]
      @docclass['cl-ds:transactional]
      @docclass['cl-ds:lazy]
      @end{documentation})

    (level [section]
      @title{Conditions}
      @text{Cl-data-structures tries to signal only the well structured errors that are possible to interpret. In order to achieve this, the hierarchy of condition classes is introduced. Below there is documentation explaining it.}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docerror['cl-ds:textual-error]
      @docerror['cl-ds:invalid-argument]
      @docerror['cl-ds:initialization-error]
      @docerror['cl-ds:out-of-bounds]
      @docerror['cl-ds:argument-out-of-bounds]
      @docerror['cl-ds:initialization-out-of-bounds]
      @docerror['cl-ds:not-implemented]
      @end{documentation})

    @end{section}

    @end{section})

  (chunk *cl-data-structures*
    @begin{section} @label{dicts}
    @title{Dictionary structures}
    [:text Dictionaries map values to unique keys. ,(lisp) standard already contains such structures (hash tables, alists, plists) and therefore idea should not be alien to a Lisp programmer. CL-DATA-STRUCTURES offers both functional and mutable dictionaries, with HAMT being the prime example of complete, feature rich implementation of the protocol. In practice, containers present in this module are either ordered containers (for instance binary search trees) or some sort of unordered hash table (either classiscal hashtable or some sort of hashing tree). In each case, overview of data structure is present in this document.]
    (level [section]
      @title{API}
      @text{To obtain value under key use following functions:}
      @begin{list}
      @item{AT}
      @end{list}
      @text{To change mapping use following purely functional functions:}
      @begin{list}
      @item{INSERT}
      @item{ADD}
      @item{UPDATE}
      @item{ERASE}
      @item{ERASE-IF}
      @end{list}
      @text{To change mapping in destructive way, use following functions:}
      @begin{list}
      @item{(SETF AT)}
      @item{ADD!}
      @item{UPDATE!}
      @item{ERASE!}
      @item{ERASE-IF!}
      @end{list}
      @text{This package adds set of another trait classes, specific to dictionaries.}
      @docclass['cl-ds.dicts:dictionary]
      @docclass['cl-ds.dicts:hashing-dictionary]
      @text{In addition to this, on this level, few additional functions are defined.}
      @docgeneric['cl-ds.dicts:find-content]
      @docgeneric['cl-ds.dicts:single-element-p])

    (level [section]
      @title{HAMT}
      @text{HAMT stands from hash array mapped trie. This data structure is used most commonly as functional dictionary in standard libraries of few recent languages (including Clojure and Scala). Cl-data-structures implementation offers also mutable and transactional variant of this structure. Although this container is not optimized for destructive modification, it is still faster then copying on write whole path. Since HAMT contains transactional implementation, lazy functional implementation is also present.}
      @text{CL-DATA-STRUCTURES implementation of this data structure is unusual, because presence of transactional implementation. Transactional in this sense means that destructive changes are isolated to the single instance of container (think: delayed copy). Thanks to this it is possible to implement fancy stuff like diff generation from changes applied during transformation in efficient way (usefull for creating eventual consistent systems).}
      @text{Dictionary implementation of HAMT is present in the system as a class.}
      @docclass['cl-ds.dicts.hamt:hamt-dictionary]
      @docclass['cl-ds.dicts.hamt:functional-hamt-dictionary]
      @docclass['cl-ds.dicts.hamt:mutable-hamt-dictionary]
      @text{As you can see, it inherits DICTIONARY trait class as well as lower level FUNDAMENTAL-HAMT-CONTAINER class. All instances of this class can be used with following functions:}
      @docfun['cl-ds.dicts.hamt:hamt-dictionary-at]
      @docfun['cl-ds.dicts.hamt:hamt-dictionary-size]
      @text{Functional dictionary is represented by the following class:}

      @text{There is no lazy-hamt-dictionary class, because lazy hamt dictionary is nothing more then a TRANSACTIONAL-HAMT-DICTIONARY inside LAZY-BOX.}

      (level [section]
        @title{Constructing}
        @text{To construct HAMT dictionary, use following functions.}
        @begin{documentation} @pack{CL-DATA-STRUCTURES.DICTS.HAMT}
        @docfun['cl-ds.dicts.hamt:make-functional-hamt-dictionary]
        @docfun['cl-ds.dicts.hamt:make-mutable-hamt-dictionary]
        @end{documentation})

      (level [section]
        @title{POSITION-MODIFICATION contracts}
        @text{Since HAMT is hashing container, many of the functions accept additional hash key argument with fixnum produced by the hashing function.}
        @text{SHRINK-BUCKET function must be defined in terms all functional shrink-functions and buckets. Will accept :hash.}
        @text{GROW-BUCKET function must be defined in terms all functional grow-functions and buckets. Will accept :hash.}
        @text{SHRINK-BUCKET! function must be defined in terms all mutable shrink-functions and buckets. Will accept :hash.}
        @text{GROW-BUCKET! function must be defined in terms all mutable grow-functions and buckets. Will accept :hash.}
        @text{MAKE-BUCKET function must be defined in terms of all grow-functions and will return list of hash-content-tuple as bucket. Will accept :hash}
        @text{Bucket must be usable with cl-ds.dicts:find-content. FIND-CONTENT function will accept hash as key argument.}))
    @end{section}))

