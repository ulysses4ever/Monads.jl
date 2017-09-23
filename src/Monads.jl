module Monads

# types
export Monad, Identity, MList, Maybe, State
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

abstract type Monad{T} end 
abstract type MonadPlus{T} <: Monad{T} end

## Buy two monad combinators, get the third free!
mreturn(::Type{M}, val :: T) where {T, M<:Monad} = M{T}(val)
join(m::Monad) = mbind(identity, m)
fmap{M<:Monad}(f::Function, m::M) = mbind(m) do x
    mreturn(M, f(x))
end
mbind(f::Function, m::Monad) = join(fmap(f, m))

## Extra combinators
mcomp(g::Function, f::Function) = x -> mbind(g, f(x))
mthen(k::Monad, m::Monad) = mbind(_ -> k, m)
(>>)(m::Monad, k::Monad) = mthen(k, m)

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
mdo_desugar(exprIn) = reduce(mdo_desugar_helper, :(), reverse(exprIn.args))
mdo_desugar_helper(rest, expr) = rest
function mdo_desugar_helper(rest, expr::Expr)
    if expr.head == :call && expr.args[1] == :(<|)
        # replace "<|" with monadic binding
        quote
            mbind($(expr.args[3])) do $(expr.args[2])
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
    x1 <| m1
    return f(x1)
end

## Starting slow: Identity
type Identity{T} <: Monad{T}
    value :: T
end

mbind(f::Function, m::Identity) = f(m.value)

## List
mutable struct MList{T} <: MonadPlus{T}
    value :: Vector{T}
    
    MList{T}(x :: Vector{T}) where T = new{T}(x)
    MList{T}(x :: T) where T = new{T}([x])
end

MList(x :: Vector{T}) where T = MList{T}(x)
MList(x :: T) where T = MList{T}([x])

function join(mm :: MList{MList{T}}) where T
    MList{T}(
     foldl(
       (v,m) -> vcat(v, m.value), 
       T[], 
       mm.value))
end
fmap(f::Function, m::MList) = MList(map(f, m.value))

# It's also a MonadPlus
mzero(::Type{MList}) = MList([])
mplus(m1::MList, m2::MList) = join(MList([m1, m2]))

import Base.==
==(l1 :: MList{T}, l2 :: MList{T}) where T =
    l1.value == l2.value

## Maybe
mutable struct Maybe{T} <: Monad{T}
    value :: Union{T, Void}
end

mbind(f::Function, m::Maybe{T}) where T = isa(m.value, Nothing) ? Maybe{T}(nothing) : f(m.value)

## State
type State{T} <: Monad{T}
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

end
