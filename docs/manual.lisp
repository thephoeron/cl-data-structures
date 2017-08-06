(in-package #:cl-data-structures.documentation)
(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.protocol
 cl-lore.extensions.documentation.api)

(defparameter *index* cl-data-structures:*documentation*)
(def-chunks *cl-data-structures*)

(with-names (cl-lore.extensions.documentation.api:<documentation-names>)
  (chunk *cl-data-structures*
    @begin{section} @label{cl-ds intro}
    @title{Overview}

    @text{cl-data-structures is a portable collection of data structures for common lisp. Design goals of this library are following:}

    @begin{list}
    @item{Uniform -- data structures that are used for specific task should have common interface. User should just know how to use dictionary, and not specific implementation of it.}
    @item{Complete -- this package intends to be definitive common lisp data structures collection, containing both functional and mutable structures, for every use case possible.}
    @item{Universal -- there should be no limitations on when this library is useful.}
    @end{list}

    @text{To achieve this goals, package cl-data-structures contains common API and various implementations of it in separate packages. Implementations are divided into few categories:}

    @begin{list}
    @item{Dicts -- short for dictionaries. Data structures that map locations to values. All in the package cl-ds.dicts}
    @end{list}

    @begin{section}

    @title{Conventions}

    @text{Data structure types are not hidden under generic interface name (like std::unordered_map) but instead names are directly exposed to the user. User is encouraged to read Implementation details section of this manual fo figure out which data structure implementation works best for his use case. Destructive functions follow scheme style of adding '!' as a suffix (so we have GF ADD! that is destructive version of ADD).

    Methods that implements API generic functions redirect logic to good, old fashioned functions named in convention data-structure-name-operation (for instance hamt-dictionary-at). If you @emph{really} want to squeeze all the performance you can get, you may want to skip generic function dispatch by using those functions directly.}

    @end{section}

    @begin{section}
    @title{Key concepts}

    @text{This section describes distinctive approach of cl-data-structure library in various areas.}

    @begin{section}
    @title{Signaling errors}

    @end{section}

    @begin{section}
    @title{Trait classes}

    @end{section}

    @begin{section}
    @title{Modification Status}

    @end{section}

    @begin{section}
    @title{Variants}

    @text{Most of cl-data-structures containers are available in few variants. Purpose of those is to aid programmer in avoidining errors by that may occur when mixing functional and destructive operations, while still providing access to both. To understand motivation behind this decission, consider other possible approaches that could be taken instead.}

    @text{You can just allow arbitrary changes happening on any level. This usual gives you the best raw performance, but at the high cost: state that you are mutating can be shared in arbitrary way. If execution of your code is interupted, changes made in the container are preserved, even if the represent incoherent or invalid data. You need to clean it yourself. Changes are also shared between threads, which means that you will need to also share some mutex to protect your data from races. This kind of containers are called @emph{mutable} in this library.}

    @text{@emph{Functional} containers do not suffer from the same problems. Every operation that would change existing state in the mutable container instead will return new container, with changes visible only there. This, however has another limitation: copying is costly. Although copying whole structure is usually not required, we still need to copy at least parts of it.}

    @text{@emph{Transactional} containers represent compromise between those two opposite approaches. Transactional containers implement @emph{mutable} api in distinct way: instead of performing destructive operations in arbitrary way, we are trying to @emph{isolate} changes so they will be visible only in the instance that we passed into method. This allows us to achieve compromise between safety, simplicity and speed.}

    @text{All containers with @emph{transactional} variant available can be also used as @emph{functional}, @emph{lazy} containers. Those containers reduce consing that troubles @emph{functional} containers by grouping all modification operations, and performing hidden, destructive modification of @emph{transactional} containers in the last possible moment.}

    @text{Containers can be converted between functional, transactional and mutable variants using @emph{become} methods. However, not every container is available in all three variants. It is also important to remember that @emph{become} methods have limited guaranties. For instance: @emph{BECOME-TRANSACTIONAL} guaranties that changes from returned instance won't leak outside of returned instance, but not that destructive changes from original instance can't leak into it. Same applies for @emph{BECOME-FUNCTIONAL} method. Be careful and keep this in mind.}

    @end{section}

    @end{section}

    @end{section})

  (chunk *cl-data-structures*
    @begin{section} @label{cl-ds API}
    @title{API Reference}
    @text{This section contains reference of all functions, classes and macros provided by this library to the user.}

    @begin{section}
    @title{Common API}
    @text{Following is the most abstract API of cl-data-structures library.}

    @begin{section}
    @title{Generic Functions}

    (progn
      @begin{section}
      @title{Query Functions}
      @text{Following generic functions check state of the container and are not allowed to change it.}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:at]
      @docgeneric['cl-ds:size]
      @docgeneric['cl-ds:mutablep]
      @docgeneric['cl-ds:transactionalp]
      @docgeneric['cl-ds:functionalp]

      @end{documentation}

      @end{section})
    (progn
      @begin{section}
      @title{Functional modification API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:insert]
      @docgeneric['cl-ds:add]
      @docgeneric['cl-ds:update]
      @docgeneric['cl-ds:erase]
      @end{documentation}

      @end{section})

    (progn
      @begin{section}
      @title{Mutable modification API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:add!]
      @docgeneric['cl-ds:update!]
      @docgeneric['cl-ds:erase!]
      @end{documentation}

      @end{section})

    (progn
      @begin{section}
      @title{Variants API}

      @begin{documentation}
      @pack{CL-DATA-STRUCTURES}
      @docgeneric['cl-ds:become-functional]
      @docgeneric['cl-ds:become-mutable]
      @docgeneric['cl-ds:become-transactional]
      @docgeneric['cl-ds:become-lazy]
      @end{documentation}

      @end{section})

    @end{section}

    @begin{section}
    @title{Classes}
    @begin{documentation}
    @pack{CL-DATA-STRUCTURES}
    @docclass['cl-ds:fundamental-container]
    @docclass['cl-ds:fundamental-modification-operation-status]
    @docclass['cl-ds:functional]
    @docclass['cl-ds:mutable]
    @docclass['cl-ds:transactional]
    @docclass['cl-ds:lazy]
    @end{documentation}
    @end{section}

    @begin{section}
    @title{Conditions}
    @begin{documentation}
    @pack{CL-DATA-STRUCTURES}
    @docclass['cl-ds:textual-error]
    @docclass['cl-ds:invalid-argument]
    @docclass['cl-ds:initialization-error]
    @docclass['cl-ds:out-of-bounds]
    @docclass['cl-ds:argument-out-of-bounds]
    @docclass['cl-ds:initialization-out-of-bounds]
    @docclass['cl-ds:not-implemented]
    @end{documentation}
    @end{section}

    @end{section}

    @end{section}))

(defun build-document ()
  (with-names (<documentation-names>)
    (document (cl-lore.mechanics:<mechanics-html-output-generator> out *cl-data-structures*
               :output-options (:css cl-lore.mechanics:*mechanics-html-style*))
      @title{CL-DATA-STRUCTURES}
      @include{cl-ds intro}
      @include{cl-ds API})))

(cl-lore.protocol.output::save-output #P"/home/shka/lore/" (build-document))
