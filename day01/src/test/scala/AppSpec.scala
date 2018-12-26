import org.scalatest.FlatSpec

class AppSpec extends FlatSpec  {
  "+1 +1 +1" should " result in 3" in {
    assert(App.part1(List(1, 1, 1)) == 3)
  }
  "+1, +1, -2" should " result in 0" in {
    assert(App.part1(List(1, 1, -2)) == 0)
  }
  "-1, -2, -3" should " result in -6" in {
    assert(App.part1(List(-1, -2, -3)) == -6)
  }
  "-1, +2" should "fail" in {
    assert(App.part1(List(-1, 2)) != 0)
  }

  "+1, -1 " should " first reach 0 twice" in {
    assert(App.part2(List(1, -1)) == 0)
  }

  "+3, +3, +4, -2, -4 " should " first reach 10 twice" in {
    assert(App.part2(3 :: 3 :: 4 :: -2 :: -4 :: Nil) == 10)
  }

  "-6, +3, +8, +5, -6 " should " first reach 5 twice" in {
    assert(App.part2(-6 :: 3 :: 8 :: 5 :: -6 :: Nil) == 5)
  }

  "+7, +7, -2, -7, -4 " should " first reach 14 twice" in {
    assert(App.part2(7 :: 7 :: -2 :: -7 :: -4 :: Nil) == 14)
  }
}
