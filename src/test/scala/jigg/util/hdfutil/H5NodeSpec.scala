package jigg.util.hdfutil

/**
  * Created by kenta-yoshinaga on 2016/11/09.
  */

import org.scalatest._

class H5NodeSpec extends FlatSpec with Matchers{

  // Data structure
  //  1 2
  //  3 4
  val elem2d = H5DataSet(Seq(1,2,3,4), "dummy", 2, Seq(2,2))

  // Data structure
  // (0,:,:) |  (1,:,:)
  //  1 2    |   5 6
  //  3 4    |   7 8
  val elem3d = H5DataSet(Seq(1,2,3,4,5,6,7,8), "dummy", 3, Seq(2,2,2))

  // Data structure
  // (0,0,:,:)   |  (0,1,:,:)
  //  1  2       |   5  6
  //  3  4       |   7  8
  // (1,0,:,:)   |  (1,1,:,:)
  //  9  10      |   13 14
  //  11 12      |   15 16
  val elem4d = H5DataSet(Seq(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), "dummy", 4, Seq(2,2,2,2))

  "apply(y,x)" should "get the element by specified position" in{
    elem2d(0,0) should be (1)
    elem2d(0,1) should be (2)
    elem2d(1,0) should be (3)
    elem2d(1,1) should be (4)
  }

  "apply(y,x)" should "throw exception when invalid arguments input" in {
    an[IllegalArgumentException] should be thrownBy elem2d(0,-1)
    an[IllegalArgumentException] should be thrownBy elem2d(-1,0)
    an[IllegalArgumentException] should be thrownBy elem2d(2,0)
    an[IllegalArgumentException] should be thrownBy elem2d(0,2)
  }

  "apply(z,y,x)" should "get the element by specified position" in{
    elem3d(0, 0, 0) should be (1)
    elem3d(0, 0, 1) should be (2)
    elem3d(0, 1, 0) should be (3)
    elem3d(0, 1, 1) should be (4)
    elem3d(1, 0, 0) should be (5)
    elem3d(1, 0, 1) should be (6)
    elem3d(1, 1, 0) should be (7)
    elem3d(1, 1, 1) should be (8)
  }

  "apply(z,y,x)" should "throw exception when invalid arguments input" in {
    an[IllegalArgumentException] should be thrownBy elem3d(-1,0,0)
    an[IllegalArgumentException] should be thrownBy elem3d(0,-1,0)
    an[IllegalArgumentException] should be thrownBy elem3d(0,0,-1)
    an[IllegalArgumentException] should be thrownBy elem3d(2,0,0)
    an[IllegalArgumentException] should be thrownBy elem3d(0,2,0)
    an[IllegalArgumentException] should be thrownBy elem3d(0,0,2)
  }

  "apply(i,z,y,x)" should "get the element by specified position" in{
    elem4d(0, 0, 0, 0) should be (1)
    elem4d(0, 0, 0, 1) should be (2)
    elem4d(0, 0, 1, 0) should be (3)
    elem4d(0, 0, 1, 1) should be (4)
    elem4d(0, 1, 0, 0) should be (5)
    elem4d(0, 1, 0, 1) should be (6)
    elem4d(0, 1, 1, 0) should be (7)
    elem4d(0, 1, 1, 1) should be (8)
    elem4d(1, 0, 0, 0) should be (9)
    elem4d(1, 0, 0, 1) should be (10)
    elem4d(1, 0, 1, 0) should be (11)
    elem4d(1, 0, 1, 1) should be (12)
    elem4d(1, 1, 0, 0) should be (13)
    elem4d(1, 1, 0, 1) should be (14)
    elem4d(1, 1, 1, 0) should be (15)
    elem4d(1, 1, 1, 1) should be (16)
  }

  "apply(i,z,y,x)" should "throw exception when invalid arguments input" in {
    an[IllegalArgumentException] should be thrownBy elem4d(-1,0,0,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,-1,0,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,0,-1,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,0,0,-1)
    an[IllegalArgumentException] should be thrownBy elem4d(2,0,0,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,2,0,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,0,2,0)
    an[IllegalArgumentException] should be thrownBy elem4d(0,0,0,2)
  }
}
