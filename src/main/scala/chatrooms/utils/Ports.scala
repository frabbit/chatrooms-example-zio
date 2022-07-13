package chatrooms.utils

import zio.*
import java.net.DatagramSocket
import java.net.ServerSocket
import java.io.IOException



object Ports {
  // based on https://stackoverflow.com/a/435579
  val MIN_PORT_NUMBER = 1025
  val MAX_PORT_NUMBER = 65535
  def available(port:Int):ZIO[Any, Nothing, Boolean] =
    def check:Boolean = {
      if port < Ports.MIN_PORT_NUMBER || port > Ports.MAX_PORT_NUMBER then
        false
      else
        var ss:ServerSocket = null
        var ds:DatagramSocket = null
        val res =
          try
            ss = new ServerSocket(port)
            ss.setReuseAddress(true)
            ds = new DatagramSocket(port)
            ds.setReuseAddress(true)
            true
          catch
            case e:IOException => false
          finally
            if ds != null then
              ds.close()
            if ss != null then
              try
                ss.close()
              catch
                case e:IOException =>
        res
    }
    ZIO.succeed(check)
}