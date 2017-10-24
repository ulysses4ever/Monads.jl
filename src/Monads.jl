module Monads

import Base.==
using Promisables

# types
export Monad, Identity, MList, Maybe, State, MPromise
# combinators
export mreturn, join, fmap, mbind, mcomp, mthen, (>>)
# utilities
export liftM
# do syntax
export @mdo
# MonadPlus
export MonadPlus, mzero, mplus, guard
# State
export runState, put, get, evalState, execState

abstract type Monad end
abstract type MonadPlus <: Monad end

## Buy two monad combinators, get the third free!
mreturn{M<:Monad}(::Type{M}, val) = M(val)
join(m::Monad) = mbind(identity, m)
fmap{M<:Monad}(f::Function, m::M) = mbind(m) do x
    mreturn(M, f(x))
end
mbind(f::Function, m::Monad) = join(fmap(f, m))

## Extra combinators
mcomp(g::Function, f::Function) = x -> mbind(g, f(x))
mthen(k::Monad, m::Monad) = mbind(_ -> k, m)
(>>)(m::Monad, k::Monad) = mthen(k, m)
(==)(m::Monad, k::Monad) = m.value == k.value

## A MonadPlus function
guard{M<:MonadPlus}(::Type{M}, c::Bool) = c ? mreturn(M, nothing) : mzero(M)

## Friendly monad blocks
macro mdo(mtype, body)
    esc(mdo_desugar(mdo_patch(mtype, body)))
end

## patch up functions to insert the right monad
mdo_patch(mtype, expr) = expr
function mdo_patch(mtype, expr::Expr)
    expr.args = map(arg->mdo_patch(mtype, arg), expr.args)
    if expr.head == :return
        expr.head = :call
        insert!(expr.args, 1, :mreturn)
    end
    if expr.head == :call && any(expr.args[1] .== [:mreturn, :mzero, :guard, :liftM])
        insert!(expr.args, 2, mtype)
    end
    expr
end

## desugaring mdo syntax is a right fold
mdo_desugar(exprIn) = foldr(mdo_desugar_helper, exprIn.args)
mdo_desugar_helper(rest, expr) = rest
function mdo_desugar_helper(expr::Expr, rest)
    if (expr.head == :call 
      && expr.args[1] == Symbol("<")
        && expr.args[3].args[1] == Symbol("-"))
        # replace "<-" with monadic binding
        newExpr = expr.args[3].args[2:end];
        quote
            mbind($(newExpr...)) do $(expr.args[2])
                $rest
            end
        end
    elseif expr.head == :(=)
        # replace assignment with let binding
        quote
            let
                $expr
                $rest
            end
        end
    elseif expr.head == :line
        rest
    elseif rest == :()
        expr
    else
        # replace with sequencing
        :(mthen($rest, $expr))
    end
end

## Function lifting
liftM{M<:Monad}(::Type{M}, f::Function) = m1 -> @mdo M begin
    x1 <- m1
    return f(x1)
end

## Starting slow: Identity
type Identity{T} <: Monad
    value :: T
end

mbind(f::Function, m::Identity) = f(m.value)

## List
type MList <: MonadPlus
    value :: Vector

    MList(x::Array) = new(vec(x))
    MList(x) = new([x])
end

function join(m::MList)
    if !isempty(m.value)
        val = nothing
        for arr in m.value[1:end]
            if !isempty(arr.value)
                if val === nothing
                    val = arr.value
                else
                    append!(val, arr.value)
                end
            end
        end
        if val === nothing
            mzero(MList)
        else
            mreturn(MList, val)
        end
    else
        mzero(MList)
    end
end
fmap(f::Function, m::MList) = MList(map(f, m.value))

mbind(f::Function, m::MList) = join(fmap(f, m))

# It's also a MonadPlus
mzero(::Type{MList}) = MList([])
mplus(m1::MList, m2::MList) = join(MList([m1, m2]))

type Nothing end

## Maybe
type Maybe{T} <: Monad
    value :: Union{T, Nothing}
end

mbind(f::Function, m::Maybe) = isa(m.value, Nothing) ? Maybe(nothing) : f(m.value)

## State
type State <: Monad
    runState :: Function # s -> (a, s)
end
state(f) = State(f)

runState(s::State) = s.runState
runState(s::State, st) = s.runState(st)

function mbind(f::Function, s::State)
      state(st -> begin
          (x, stp) = runState(s, st)
          runState(f(x), stp)
            end
            )
end
mreturn(::Type{State}, x) = state(st -> (x, st))

put(newState) = state(_ -> (nothing, newState))
get() = state(st -> (st, st))

evalState(s::State, st) = runState(s, st)[1]
execState(s::State, st) = runState(s, st)[2]

T1 = T where T <: Function
function mbind(f::T1, m::Promise)
  Then(f, m)
end
mreturn(::Type{Promise}, x) = Resolve(x);

end
