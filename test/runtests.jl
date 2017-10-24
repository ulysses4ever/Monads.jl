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

@testset "A promise monad" begin
  @testset "must properly resolve and return a working promise" begin
    p0 = Promise()
    step1::Function = (x) -> Resolve(x+2);
    step2::Function = (x) -> Resolve(x+2);

    p = (@mdo Promise begin
      x <- p0
      y <- (step1(x))
      z <- (step2(y))
      return y + z
    end)

    Timer((t) -> begin
      Fulfill(p0, 0)
      close(t)
    end, 2)

    @pawait p
    @test p.value == 6
  end
end

end
