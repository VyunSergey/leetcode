package study.array

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RemoveElementTest extends AnyFlatSpec with Matchers {

  "removeElement" should "correctly remove 3" in {
    val nums = Array(3, 2, 2, 3)
    val `val` = 3
    val res = RemoveElement.removeElement(nums, `val`)
    res shouldBe 2
    nums.take(2).sameElements(Array(2, 2)) shouldBe true
  }

  it should "correctly remove 2" in {
    val nums = Array(0, 1, 2, 2, 3, 0, 4, 2)
    val `val` = 2
    val res = RemoveElement.removeElement(nums, `val`)
    res shouldBe 5
    nums.take(5).sameElements(Array(0, 1, 3, 0, 4)) shouldBe true
  }

  it should "correctly remove 1" in {
    val elements = List.fill(1000)(Random.nextInt(10))
    val nums = elements.toArray
    val `val` = 1
    val res = RemoveElement.removeElement(nums, `val`)
    res shouldBe elements.filterNot(_ == `val`).length
    nums.take(res).sameElements(elements.filterNot(_ == `val`)) shouldBe true
  }
}
