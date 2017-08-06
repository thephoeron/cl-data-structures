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
    @text{Cl-data-structures makes use of few key concepts. It is important to understand those concepts as they will allow you to write better, safer and more efficient code. First of: there are different approaches to handling destructive operations.}
    @text{You can just allow arbitrary changes happening on any level. This usual gives you the best raw performance, but at the high cost: state that you are mutating can be shared in arbitrary way. If execution of your code is interupted, changes made in the container are preserved, even if the represent incoherent or invalid data. You need to clean it yourself. Changes are also shared between threads, which means that you will need to also share some mutex to protect your data from races. This kind of containers are called @emph{mutable} in this library.}

    @text{@emph{Functional} containers do not suffer from the same problems. Every operation that would change existing state in the mutable container instead will return new container, with changes visible only there. This, however has another limitation: copying is costly. Although copying whole structure is usually not required, we still need to copy at least parts of it.}

    @text{@emph{Transactional} containers represent compromise between those two opposite approaches. Transactional containers implement @emph{mutable} api in distinct way: instead of performing destructive operations in arbitrary way, we are trying to @emph{isolate} changes so they will be visible only in the instance that we passed into method. This allows us to achieve compromise between safety, simplicity and speed.}

    @text{All containers with @emph{transactional} variant available can be also used as @emph{functional}, @emph{lazy} containers. Those containers reduce consing that troubles @emph{functional} containers by grouping all modification operations, and performing hidden, destructive modification of @emph{transactional} containers in the last possible moment.}

    @text{Containers can be converted between functional, transactional and mutable variants using @emph{become} methods. However, not every container is available in all three variants. It is also important to remember that @emph{become} methods have limited guaranties. For instance: @emph{BECOME-TRANSACTIONAL} guaranties that changes from returned instance won't leak outside of returned instance, but not that destructive changes from original instance can't leak into it. Same applies for @emph{BECOME-FUNCTIONAL} method. Be careful and keep this in mind.}

    @end{section}

    @end{section}))

(defun build-document ()
  (with-names (<documentation-names>)
    (document (cl-lore.mechanics:<mechanics-html-output-generator> out *cl-data-structures*
               :output-options (:css cl-lore.mechanics:*mechanics-html-style*))
      @title{CL-DATA-STRUCTURES Documentation}
      @include{cl-ds intro})))

(cl-lore.protocol.output::save-output #P"/home/shka/lore/" (build-document))
