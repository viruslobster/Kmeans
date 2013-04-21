import scala.swing._
import java.awt.Color
import java.awt.RenderingHints
import scala.util.Random
import scala.collection.mutable.ListBuffer
import scala.swing.event.ButtonClicked
import scala.swing.event.ButtonClicked

object KmeansGui extends SimpleSwingApplication {
  
  val maxppc = 20; val range = 300; val sigma = 15.0; val maxc = 7; val r = new Random
  val colors = Array(Color.RED, Color.GREEN, Color.BLUE, Color.MAGENTA, Color.ORANGE, Color.PINK, Color.CYAN)
  var kmeans: Kmeans = _
  var clusters: IndexedSeq[(Int, Int)] = _
  var data: ListBuffer[Array[Double]] = _
  
  genData() 

  def genData() {
    kmeans = new Kmeans(r.nextInt(maxc - 1) + 2, 2)
    clusters = for (_ <- 0 to kmeans.getK - 1) yield (r.nextInt(range), r.nextInt(range))
    data = for (i <- ListBuffer.range(0 , kmeans.getK); j <- 0 to r.nextInt(maxppc) + 1)
      yield Array(clusters(i)._1 + r.nextGaussian * sigma, clusters(i)._2 + r.nextGaussian * sigma)

    kmeans.data = data

    kmeans.init()
    kmeans.assign()
  }

  def top = new MainFrame {
    title = "Kmeans Algorithm"
    minimumSize = new Dimension(500, 500)

    val panel = new Panel {
      override def paintComponent(g: Graphics2D) = {       
        
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        g.setColor(Color.BLACK)
        //kmeans.data.foreach((p: Array[Double]) => g.fillOval(20 + p(0).toInt, 20 + p(1).toInt, 4, 4))
        for (i <- 0 to kmeans.clustered_data.length - 1; k <- 0 to kmeans.clustered_data(i).length - 1) {
          g.setColor(colors(i))
          val p = kmeans.clustered_data(i)(k)
          g.fillOval(20 + p(0).toInt, 20 + p(1).toInt, 4, 4)
        }
        for (i <- 0 to kmeans.centriods.length - 1) {
          g.setColor(colors(i))
          g.fillOval(20 + kmeans.centriods(i)(0).toInt, 20 + kmeans.centriods(i)(1).toInt, 10, 10)
        }
      }
    }
    val updateButton = new Button { text = "Update" }
    val ranButton = new Button { text = "Randomize" }
    val genButton = new Button { text = "Generate data" }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += panel
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += updateButton
        contents += ranButton
        contents += genButton
      }
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }

    listenTo(updateButton, ranButton, genButton)
    reactions += {
      case ButtonClicked(`updateButton`) =>
        kmeans.update(); kmeans.assign(); repaint()
      case ButtonClicked(`ranButton`) => 
        kmeans.init(); kmeans.assign(); repaint()
      case ButtonClicked(`genButton`) =>
        genData(); kmeans.init(); kmeans.assign(); repaint()      
    }
  }
}