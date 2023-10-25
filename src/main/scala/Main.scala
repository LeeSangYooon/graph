import Interpreter.REPL

object Main {
  def main(args: Array[String]): Unit = {
    var repl = REPL.init
    while (true)
      repl = repl.prompt()
  }
}

/*
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File
import javax.imageio.ImageIO

object Main {
  def main(args: Array[String]): Unit = {
    val width = 1024
    val height = 1024
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for (x <- 0 until width; y <- 0 until height) {
      val color = calculatePixelColor(x, y)
      image.setRGB(x, y, color.getRGB)
    }

    val outputFile = new File("output.png")
    ImageIO.write(image, "png", outputFile)
    println("Image generated and saved as output.png")
  }

  def calculatePixelColor(x: Int, y: Int): Color = {
    // Replace this with your own pixel color calculation logic
    val red = (x % 256).toInt
    val green = (y % 256).toInt
    val blue = (x + y) % 256
    new Color(red, green, blue)
  }
}

* */