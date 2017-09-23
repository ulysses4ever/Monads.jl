module Test

# using Monads; while developing:
include("../src/Monads.jl")
using Test.Monads

using Base.Test

@testset "Tests for list monad (MList)" begin
    @test mbind(x -> MList([x, 3]), MList([1,2])) == MList([1,3,2,3])
    @test (@mdo MList begin
                a <| MList([1,2,3])
                return (a + 1)
              end) == MList([2,3,4])
    @test (@mdo MList begin
                a <| MList([1,2,3])
                b <| MList([1,2,3])
                guard(a!=b)
                return (a,b)
              end) == MList([(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)])

end

end
