module Test

using Monads
using Base.Test
using Promisables

@testset "Tests for list monad (MList)" begin
    @test mreturn(MList, 1) == MList([1])
    @test mbind(x -> MList([x, 3]), MList([1,2])) == MList([1,3,2,3])
    @test (@mdo MList begin
                a <- MList([1,2,3])
                return (a + 1)
              end) == MList([2,3,4])
    @test (@mdo MList begin
                a <- MList([1,2,3])
                b <- MList([1,2,3])
                guard(a!=b)
                return (a,b)
              end) == MList([(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)])

end

