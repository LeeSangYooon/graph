package Interpreter
import VirtualMachine.VirtualMachine

import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import java.time.LocalDateTime
import javax.imageio.ImageIO
import javax.print.attribute.standard.DateTimeAtProcessing
case class Output (string: String = null, pixels: Array[Array[Double]] = null, func: (String, Int) = null) {
  private val lineColor: Color = Color.blue
  private val width: Int = 1024
  private val height: Int = 1024
  private val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)


  def show(vm: VirtualMachine = null): Unit = {
    print(Console.BLUE)
    if (string != null) {
      println(string)
    }
    if (func != null) {
      showGraph(vm)
    }
    print(Console.BLACK)

  }

  private def showGraph(vm: VirtualMachine): Unit = {
    val name = func._1
    val dimension = func._2
    val time = LocalDateTime.now()
    val title = f"${time.getYear}_${time.getMonthValue}_${time.getDayOfMonth}_${time.getHour}_${time.getMinute} ${name}"

    val range = 2f
    for (x <- 0 until width; y <- 0 until height) {
      val color = Color.white
      image.setRGB(x, y, color.getRGB)
    }
    drawLine(x = width / 2, size = 4, color = Color.darkGray)
    drawLine(y = height / 2, size = 4, color = Color.darkGray)

    if (dimension == 1) {

      for (x <- 0 until width) {
        val xValue = (x.toFloat / width - 0.5) * range * 2
        val prompt = Code(List(ByteCode.PUSH(xValue), ByteCode.Call(name)))
        val yValue = vm.run(prompt).memory.stack.last
        val y = ((-yValue / range + 1) / 2 * height).round.toInt
        //println(x, xValue, y, yValue)
        if(0 <= y && y < height) {
          drawCircle(x, y, 4, lineColor)
        }
      }
    }
    else if (dimension == 2) {
      for (x <- 0 until width; y <- 0 until height) {
        val xValue = (x.toFloat / width - 0.5) * range * 2
        val yValue = (-y.toFloat / width + 0.5) * range * 2
        val prompt = Code(List(
          ByteCode.PUSH(xValue),
          ByteCode.PUSH(yValue),
          ByteCode.Call(name)
        ))
        val value = vm.run(prompt).memory.stack.last
        val color = getColor(value)
        image.setRGB(x, y, color.getRGB)
      }
    }
    else {
      return
    }



    val outputFile = new File(s"outputs/${title}.png")
    ImageIO.write(image, "png", outputFile)
    println(s"Image generated and saved as ${title}.png")
  }

  private def getColor(double: Double): Color = {
    if (double >= 0) {
      val n = (Math.min(double, 2) / 2 * 255).toInt
      new Color(0, 0, n)
    } else {
      val n = (Math.min(-double, 2) / 2 * 255).toInt
      new Color(n, 0, 0)
    }
  }

  private def drawCircle(x: Int, y: Int, size: Int, color: Color): Unit = {
    for (a <- x - size / 2 to x + size / 2; b <- y - size / 2 to y + size / 2) {
      if (0 <= a && a < width && 0 <= b && b < height) {
        image.setRGB(a, b, color.getRGB)
      }
    }
  }
  private def drawLine(x: Int= -1, y: Int = -1, size: Int, color: Color): Unit = {
    if (x == -1 && y != -1) {
      // y = a
      for (a <- 0 until width; b <- y - size / 2 to y + size / 2) {
        if (0 <= b && b < height) {
          image.setRGB(a, b, color.getRGB)
        }
      }
    }
    else if (x != -1 && y == -1) {
      // x = a
      for (b <- 0 until width; a <- x - size / 2 to x + size / 2) {
        if (0 <= a && a < width) {
          image.setRGB(a, b, color.getRGB)
        }
      }
    }
    else {
      throw new Exception("Not a Line")
    }
  }
}
//hello