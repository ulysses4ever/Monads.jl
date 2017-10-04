module Test

using Monads
using Base.Test

@testset "Tests for list monad (MList)" begin
    @test mreturn(MList, 1).value == MList([1]).value
    @test mbind(x -> MList([x, 3]), MList([1,2])).value == MList([1,3,2,3]).value
    @test (@mdo MList begin
                a <- MList([1,2,3])
                return (a + 1)
              end).value == MList([2,3,4]).value
    @test (@mdo MList begin
                a <- MList([1,2,3])
                b <- MList([1,2,3])
                guard(a!=b)
                return (a,b)
              end).value == MList([(1, 2), (1, 3), (2, 1), (2, 3), (3, 1), (3, 2)]).value

end

end
