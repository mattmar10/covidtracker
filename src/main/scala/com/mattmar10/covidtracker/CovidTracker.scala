package com.mattmar10.covidtracker

import org.scalajs.dom
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global

import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import dom.document

import scala.collection.mutable
import scala.concurrent.Future
import scala.scalajs.js
import js.JSConverters._
import scala.collection.immutable.ListMap
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("CovidTracker")
object CovidTracker {

  type CountyName = String

  def main(args: Array[String]): Unit = {

    /*
        fetchStats().map(data => {
          data.fold(err => {
            val parNode = document.createElement("p")
            val textNode = document.createTextNode(err.errorMsg)
            parNode.appendChild(textNode)
            document.body.appendChild(parNode)
          }, rawDataRows => {
            val calculated = calculate(rawDataRows)

            val mapped = calculated.map(dr => {
              js.Array(dr.date, dr.nationalCases, dr.oklahomaCases, dr.nationalDeaths, dr.oklahomaDeaths)
            }).toJSArray

            println(mapped)

            val parNode = document.createElement("p")
            val textNode = document.createTextNode(mapped.toString)
            parNode.appendChild(textNode)
            document.body.appendChild(parNode)
          })
        })

        fetchStats().map(data => {
          data.fold(err => {
            val parNode = document.createElement("p")
            val textNode = document.createTextNode(err.errorMsg)
            parNode.appendChild(textNode)
            document.body.appendChild(parNode)
          },
            res => {
              val parNode = document.createElement("p")
              val textNode = document.createTextNode(res)
              parNode.appendChild(textNode)
              document.body.appendChild(parNode)
            })
        })*/

  }

  @JSExport
  def fetchData() = {
    /*fetchStats()
      .map(e => {
        e.fold(
          _ => js.Array(),
          rows =>
            calculate((rows))
              .map(dr => {
                js.Array(
                  dr.date,
                  dr.nationalCases,
                  dr.oklahomaCases,
                  dr.nationalDeaths,
                  dr.oklahomaDeaths
                )
              })
              .toJSArray
        )
      })
      .toJSPromise

     */

    fetchStatsOklahoma()
      .map(e => {
        e.fold(
          _ => js.Array(),
          rows =>
            rows
              .map(rdr => {
                js.Array(rdr.date, rdr.cases, rdr.deaths)
              })
              .toJSArray
        )
      })
      .toJSPromise
  }

  @JSExport
  def fetchCountyData() = {

    fetchCountyStats()
      .map(e => {
        e.fold(_ => js.Array(), rows => {
          val countyStats = calculate(rows)
          countyStats
        })
      })
      .toJSPromise
  }

  def fetchStatsOklahoma(): Future[Either[DataError, Seq[RawDataRow]]] = {

    val request = HttpRequest(
      "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    )

    request
      .send()
      .map(res => {
        val parts =
          res.body
            .split("\n")
            .drop(1)
            .filter(line => line.contains("Oklahoma"))
            .map(line => {
              val parts = line.split(",")
              RawDataRow(parts(0), parts(1), parts(3).toInt, parts(4).toInt)
            })
        Right(parts.toSeq)
      })
      .recover { case err => Left(DataError(err.toString)) }

  }

  def fetchCountyStats(): Future[Either[DataError, Seq[RawCountyDataRow]]] = {
    val request = HttpRequest(
      "http://www.mattmartin.io/oklahoma-covid-data.csv"
    )

    request
      .send()
      .map(res => {
        val parts =
          res.body
            .split("\n")
            .drop(1)
            .map(line => {
              val pieces = line.split(",")
              RawCountyDataRow(
                pieces(0),
                pieces(1),
                pieces(2),
                pieces(3).toInt,
                pieces(4).toInt
              )
            })
        Right(parts.toSeq)
      })
      .recover { case err => Left(DataError(err.toString)) }
  }

  def fetchStats(): Future[Either[DataError, Seq[RawDataRow]]] = {

    val request = HttpRequest(
      "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    )

    val data = request
      .send()
      .map(res => {
        val parts =
          res.body
            .split("\n")
            .drop(1)
            .map(line => {
              val parts = line.split(",")
              RawDataRow(parts(0), parts(1), parts(3).toInt, parts(4).toInt)
            })

        Right(parts.toSeq)
      })
      .recover { case err => Left(DataError(err.toString)) }

    data

  }

  def calculate(rows: Seq[RawCountyDataRow]): js.Array[String] = {

    val dataMap: scala.collection.mutable.Map[String, scala.collection.mutable.Map[CountyName, CountyDataRow]] =
      new mutable.HashMap[String, scala.collection.mutable.Map[CountyName, CountyDataRow]]()

    val counties = rows.map(r => r.county).distinct.sorted


    rows
      .filter(
        rdr => !(rdr.date.contains("2020-01-") || rdr.date.contains("2020-02-"))
      )
      .foreach(rdr => {

        dataMap.get(rdr.date) match {
          case Some(countyMap) => {

            countyMap.get(rdr.county) match {
              case None => {
                countyMap.put(rdr.county, CountyDataRow(rdr.date, rdr.county, rdr.cases, rdr.deaths))
              }

              case Some(m) => {
                throw new IllegalStateException("duplicate data")
              }
            }


          }
          case None =>
            val newMap = new scala.collection.mutable.HashMap[String, CountyDataRow]()
            newMap.put(rdr.county, CountyDataRow(rdr.date, rdr.county, rdr.cases, rdr.deaths))

            dataMap.put(rdr.date, newMap)
        }

      })

    val sorted = ListMap(dataMap.toSeq.sortBy(_._1):_*)

    val resultData = sorted.map(m => {

      var line = ""
      for( i <- counties.indices) {

        m._2.get(counties(i)) match {
          case None => line = line + 0
          case Some(num) => line = line + num.cases
        }

        if(i != counties.length -1 ){
          line = line + ","
        }

      }

      line = m._1 + "," + line + "\n"

      line

    }).toSeq

    val header = s"Date,${counties.mkString(",")}\n"

    (Seq(header) ++ resultData).toJSArray



  }


}

case class DataError(errorMsg: String)

case class RawDataRow(date: String, state: String, cases: Int, deaths: Int)

case class RawCountyDataRow(date: String,
                            state: String,
                            county: String,
                            cases: Int,
                            deaths: Int)

case class CountyDataRow(date: String,
                   county: String,
                         cases: Int,
                         deaths: Int)


