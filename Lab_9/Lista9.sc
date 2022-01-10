//Rafał Kruszyna

import javax.print.attribute.standard.PrinterMoreInfoManufacturer

//1
class Time(private var hourArg: Int):

  if hourArg < 0 then
    hourArg = 0

  def hour: Int = hourArg

  def hour_=(newHour: Int): Unit = {
    if newHour < 0 then hourArg = 0
    else hourArg = newHour
  }


class Time2(private var hour: Int, private var minute: Int):

  require(hour >= 0 && hour < 24)
  require(minute >= 0 && hour < 60)


  def h: Int = hour

  def m: Int = minute

  def h_=(newHour: Int): Unit = {
    require(newHour >= 0 && newHour < 24)
    hour = newHour
  }

  def m_=(newMinute: Int): Unit = {
    require(newMinute >= 0 && newMinute < 60)
    minute = newMinute
  }

  def before(other: Time2): Boolean = {
    if hour < other.h then true
    else if hour > other.h then false
    else minute < other.m
  }


//2b
class Time3(hour: Int, minute: Int):


  require(hour >= 0 && hour < 24)
  require(minute >= 0 && hour < 60)
  private var minutesAfterMidnight: Int = hour * 60 + minute

  def h: Int = hour

  def m: Int = minute

  def h_=(newHour: Int): Unit = {
    require(newHour >= 0 && newHour < 24)
    minutesAfterMidnight = newHour * 60 + minute
  }

  def m_=(newMinute: Int): Unit = {
    require(newMinute >= 0 && newMinute < 60)
    minutesAfterMidnight = hour + newMinute

  }

  def before(other: Time3): Boolean =
    minutesAfterMidnight < other.minutesAfterMidnight


//3
class Pojazd(val manufacturer: String, val model: String, val year: Int = -1, var registrationNumber: String = "") {

  def this(manufacturer: String, model: String, registrationNumber: String) =
    this(manufacturer, model, -1, registrationNumber)

}


object Lista9:
  def main(args: Array[String]): Unit =

    println("Time1")

    var time11 = new Time(12)
    println(time11.hour == 12)
    time11.hour = -5
    println(time11.hour == 0)

    var time22 = new Time(-7)
    println(time22.hour == 0)

    println("Time2")
    var time1 = new Time2(14, 32)
    var time2 = new Time2(14, 31)
    var time3 = new Time2(14, 33)
    var time4 = new Time2(13, 32)
    var time5 = new Time2(14, 32)
    var time6 = new Time2(15, 32)

    println(time1.before(time2) == false)
    println(time1.before(time3) == true)
    println(time1.before(time4) == false)
    println(time1.before(time5) == false)
    println(time1.before(time6) == true)

    println("Time3")
    var time13 = new Time3(14, 32)

    var time23 = new Time3(14, 31)
    var time33 = new Time3(14, 33)
    var time43 = new Time3(13, 32)
    var time53 = new Time3(14, 32)
    var time63 = new Time3(15, 32)

    println(time13.before(time23) == false)
    println(time13.before(time33) == true)
    println(time13.before(time43) == false)
    println(time13.before(time53) == false)
    println(time13.before(time63) == true)
    println()

    var pojazd1 = new Pojazd("Volkswagen", "Golf")
    var pojazd2 = new Pojazd("Seat", "Ibiza", 1999)
    var pojazd3 = new Pojazd("Ford", "Mustang", _, "K1 DIS")
    var pojazd4 = new Pojazd("Lamborghini", "Huracan", 2020, "DW 21902")



//object UzycieWyjatkow {
//  def main(args: Array[String]): Unit = {
//    try metoda1()
//    catch {
//      case e: Exception =>
//        System.err.println(e.getMessage + "\n")
//        e.printStackTrace()
//    }
//  }
//
//  @throws[Exception]
//  def metoda1(): Unit = {
//    metoda2()
//  }
//
//  @throws[Exception]
//  def metoda2(): Unit = {
//    metoda3()
//  }
//
//  @throws[Exception]
//  def metoda3(): Unit = {
//    throw new Exception("Wyjatek zgloszony w metoda3")
//  }
//}