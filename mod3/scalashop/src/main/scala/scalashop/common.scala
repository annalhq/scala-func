package scalashop

import java.util.concurrent.*
import scala.util.DynamicVariable

import org.scalameter.*

type RGBA = Int

def red(c: RGBA): Int = (0xff000000 & c) >>> 24

def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

def rgba(r: Int, g: Int, b: Int, a: Int): RGBA =
  (r << 24) | (g << 16) | (b << 8) | (a << 0)

def clamp(v: Int, min: Int, max: Int): Int =
  if v < min then min
  else if v > max then max
  else v

class Img(val width: Int, val height: Int, private val data: Array[RGBA]):
  def this(w: Int, h: Int) = this(w, h, new Array(w * h))
  def apply(x: Int, y: Int): RGBA = data(y * width + x)
  def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c

def neighbouringPixels(x: Int, y: Int, w: Int, h: Int, radius: Int): List[(Int, Int)] =
  val neighbours = for
    i <- -radius to radius
    j <- -radius to radius
  yield (clamp(x + i, 0, w - 1), clamp(y + j, 0, h - 1))
  neighbours.distinct.toList


def rgbaToChannelValues(rgba: RGBA): List[Int] =
  List(red(rgba), green(rgba), blue(rgba), alpha(rgba))

def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA =
  val neighbours = neighbouringPixels(x, y, src.width, src.height, radius)
  val rgbaArr = neighbours
    .map((i, j) => rgbaToChannelValues(src(i, j)))
    .foldLeft(Array(0, 0, 0, 0))(
      (i, j) => i.zip(j).map((i,j) => i + j)
    ).map(x => x / neighbours.length)
  rgba(rgbaArr(0), rgbaArr(1), rgbaArr(2), rgbaArr(3))

val forkJoinPool = ForkJoinPool()

abstract class TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T]
  def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
    val right = task {
      taskB
    }
    val left = taskA
    (left, right.join())

class DefaultTaskScheduler extends TaskScheduler:
  def schedule[T](body: => T): ForkJoinTask[T] =
    val t = new RecursiveTask[T] {
      def compute = body
    }
    Thread.currentThread match
      case wt: ForkJoinWorkerThread =>
        t.fork()
      case _ =>
        forkJoinPool.execute(t)
    t

val scheduler =
  DynamicVariable[TaskScheduler](DefaultTaskScheduler())

def task[T](body: => T): ForkJoinTask[T] =
  scheduler.value.schedule(body)

def parallel[A, B](taskA: => A, taskB: => B): (A, B) =
  scheduler.value.parallel(taskA, taskB)

def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) =
  val ta = task { taskA }
  val tb = task { taskB }
  val tc = task { taskC }
  val td = taskD
  (ta.join(), tb.join(), tc.join(), td)