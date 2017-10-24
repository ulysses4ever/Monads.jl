Monads.jl --- Monadic Computation
=================================

.. image:: https://travis-ci.org/pao/Monads.jl.svg?branch=master
    :target: https://travis-ci.org/pao/Monads.jl

.. .. module:: Monads.jl
   :synopsis: Monadic computation primitives and combinators

Monads.jl provides a powerful, if relatively slow, implementation of monadic computation with several monads and combinators predefined. Monads.jl contains implementations of the identity, maybe, list, and state monads. It also offers Haskell-like syntactic sugar for chaining monadic computations with the ``@mdo`` macro.

-------
Example
-------

Using the list monad ``MList``, we can perform guarded list comprehensions::

    julia> @mdo MList begin
             a <- MList(1:3)
             b <- MList(1:3)
             guard(a!=b)
             return (a,b)
           end
    MList([(1,2), (1,3), (2,1), (2,3), (3,1), (3,2)])

This demonstrates the basic building blocks of a Julian monadic expression. ``@mdo`` opens the expression, followed by the name of the monad to compute in and a block with monadic computations. The ``<-`` operator is used to extract a value from a monad to use in a later step in the process. Combinators such as ``guard`` are automatically dispatched to the correct monad type, and the ``mreturn`` expression wraps the final result.

With the ``Maybe`` monad, we can immediately terminate a computation that produces ``nothing`` rather than throw an error.::

    julia> @mdo Maybe begin
             m <- Maybe(match(r"a", "bbbbac"))
             return m.offset 
           end
    Maybe{Int64}(5)

    julia> @mdo Maybe begin
             m <- Maybe(match(r"a", "bbbbc"))
             return m.offset
           end
    Maybe{Nothing}(nothing)

------
Macros
------

.. function:: @mdo(mtype, body)

    Perform the series of monadic computations defined by ``body`` in the monad ``mtype``. While monadic computations can be performed by directly calling monad combinators, it is often more convenient to represent them in imperative form. ``@mdo`` allows you to represent the computation in a sugared form which omits repeated type information needed to correctly dispatch monad combinators.

    Within ``@mdo`` blocks, ``mreturn``, ``mzero``, ``guard``, and ``liftM`` gain superpowers; specifically, their first argument (which is a type ``T<:Monad``) may be omitted. The ``<-`` operator and ``return`` expression are given alternate meanings which will be familiar to users of Haskell's ``do`` syntax. ``<-`` will extract the value of a monad to use in a further computation, and ``return`` becomes an alternate spelling for ``mreturn``.

-----
Types
-----

``Monad``
    The base monad type. New monads should be subtypes of this type, and must implement either ``fmap`` and ``mbind``, or ``join`` (the default ``mreturn`` is usually acceptable).

``Identity``
    The identity monad. While it isn't very interesting, it might be a good reference if you are implementing your own monads.

``Maybe{T}``
    The maybe monad. This monad can take either a non-``nothing`` value, or ``nothing``. When its value becomes ``nothing``, further computation will cease.

``State``
    The state monad.

``MonadPlus``
    The base for MonadPlus types. A new monad deriving from this type should meet the requirements of a ``Monad``, and also must implement ``mzero`` and ``mplus``.

``MList``
    The list monad. The constructor expects a ``Vector``.

-------
Methods
-------

.. function:: fmap(f, m)

    Map the function ``f`` over the monad ``m``. This will generally speaking apply ``f`` to the contents of ``m``. Equivalent to::

        mbind(m) do x
            mreturn(M, f(x))
        end

    where ``M`` is the type of the monad ``m``.

.. function:: mbind(f, m)

    The monad bind operation of the function ``f`` to the monad ``m``, equivalent to ``join(fmap(f, m))``.

.. function:: join(m)

    Join should flatten one level of monadic structure, ending in at least one monadic wrapper, equivalent to ``mbind(identity, m)``

.. function:: mreturn(M, val)

    Monadic return should wrap a value in a monad. Usually equivalent to ``M(val)``.

.. function:: mcomp(g, f)

    Composition of two monadic functions. Equivalent to ``x -> mbind(g, f(x))``.

.. function:: mthen(k, m)

    Sequencing of monadic actions. Equivalent to ``mbind(_ -> k, m)``. Can also be spelled with the infix operator ``>>``.

.. function:: mzero(M)

    The zero value of a MonadPlus ``M``. This should be the identity for ``mplus``. For instance, this is the empty list ``[]`` for ``MList``.

.. function:: mplus(m1, m2)

    The addition operation for a MonadPlus.

.. function:: guard(M, c)

    When ``M<:MonadPlus``, ``guard`` filters values based on the Boolean predicate ``c``.
    
--------------------
Implementing a monad
--------------------

To implement your own monad, you will need to create a new type that is a subtype of either ``Monad`` or ``MonadPlus`` and implement either ``mbind`` and ``mreturn``, or ``fmap`` and ``join``, each of which you will need to ``import`` from ``Monads``. The methods you define should obey the following rules::

    mbind(f, mreturn(M, a))) == f(a)

    mbind((x) -> mreturn(M, x), m) == m

    mbind(g, mbind(f, m)) == mbind((x) -> mbind(g, f(x)), m)

If your monad type is a subtype of ``MonadPlus``, it should also define the additional functions ``mplus``, which combines instances of the monad, and ``mzero``, which is the identity under ``mplus``. It should obey the following rules::

    mbind(f, mzero) = mzero
    mthen(mzero, v) = mzero

For more information, the definitive reference is the `Typeclassopedia <http://www.haskell.org/haskellwiki/Typeclassopedia>_`.

