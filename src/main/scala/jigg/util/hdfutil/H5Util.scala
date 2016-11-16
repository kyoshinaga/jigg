package jigg.util.hdfutil

/**
  * Created by kenta-yoshinaga on 2016/08/30.
  */

import java.io._

import hdf.hdf5lib.H5
import hdf.hdf5lib.HDF5Constants
import hdf.hdf5lib.callbacks.H5P_prp_delete_func_cb

import scala.collection.immutable.IndexedSeq

object H5Util {

  def loadData(filePath: String): H5Node = {
    val fid = openFile(filePath)
    val nameList = getNameTypeList(fid, fid, "/")
    val elemSeq = nameList.map(x => recursiveLoad(fid,x._1,x._2))
    val rootElem = H5Elem("ROOT", "Root", elemSeq: _*)
    rootElem
  }

  def loadData(fid: Int): H5Node = {
//    val fid = openFile(filePath)
    val nameList = getNameTypeList(fid, fid, "/")
    val elemSeq = nameList.map(x => recursiveLoad(fid,x._1,x._2))
    val rootElem = H5Elem("ROOT", "Root", elemSeq: _*)
    rootElem
  }

  private def recursiveLoad(locid: Int, name: String, dataType: Int): H5Node = dataType match{
    case HDF5Constants.H5O_TYPE_GROUP =>
      val gid = H5.H5Gopen(locid, name, HDF5Constants.H5P_DEFAULT)
      val gnameList = getNameTypeList(locid, gid, name)
      val elemSeq = gnameList.map(x => recursiveLoad(gid, x._1, x._2))
      H5Elem(name,"Group", elemSeq: _*)
    case HDF5Constants.H5O_TYPE_DATASET =>
      val did = H5.H5Dopen(locid, name, HDF5Constants.H5P_DEFAULT)
      val dsid = H5.H5Dget_space(did)
      val ndim = H5.H5Sget_simple_extent_ndims(dsid)
      val dims = new Array[Long](ndim)
      H5.H5Sget_simple_extent_dims(dsid, dims, null)
      val data = readData(did)
      if (name == "#TYPE")
        H5DataSet(data, name)
      else
        H5DataSet(data, name, ndim, dims)
  }

  def openFile(filePath:String): Int = {
    H5.H5Fopen(filePath, HDF5Constants.H5F_ACC_RDONLY,HDF5Constants.H5P_DEFAULT)
  }

  def closeFile(fid:Int): Unit = H5.H5Fclose(fid)

  private def readData(did: Int): Array[_ <: Any] = {
    val tid = H5.H5Dget_type(did)
    val dsid = H5.H5Dget_space(did)
    val npoints = H5.H5Sget_simple_extent_npoints(dsid)
    val isSimple = H5.H5Sis_simple(dsid)
    val size = H5.H5Tget_size(tid)

    H5.H5Tget_class(tid) match {
      case HDF5Constants.H5T_INTEGER =>
        val buff = new Array[Long](npoints.toInt)
        H5.H5Dread(did, tid, dsid, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, buff)
        buff.map(_.toInt)
      case HDF5Constants.H5T_FLOAT =>
        val buff = new Array[Float](npoints.toInt)
        H5.H5Dread(did, tid, dsid, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, buff)
        buff
      case HDF5Constants.H5T_STRING if npoints == 1=>
        val buff =  new Array[String](npoints.toInt)
        H5.H5Dread_string(did, tid, dsid, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, buff)
        buff
      case HDF5Constants.H5T_STRING =>
        val buff =  new Array[AnyRef](npoints.toInt)
        H5.H5DreadVL(did, tid, dsid, HDF5Constants.H5S_ALL, HDF5Constants.H5P_DEFAULT, buff)
        buff.map(_.toString)
    }
  }

  private def getNameTypeList(fid:Int, locid:Int, rootName: String): List[(String, Int)] = {
    val gInfo = H5.H5Gget_info(locid)
    val nlink = gInfo.nlinks.toInt
    val gnameList = for {
      i <- 0 until nlink
      gname = H5.H5Lget_name_by_idx(fid, rootName, HDF5Constants.H5_INDEX_NAME,
        HDF5Constants.H5_ITER_INC, i, HDF5Constants.H5P_DEFAULT)
      oi = H5.H5Oget_info_by_name(locid, gname, HDF5Constants.H5P_DEFAULT)
      nameAndType = gname -> oi.`type`
    } yield nameAndType
    gnameList.toList
  }
}
