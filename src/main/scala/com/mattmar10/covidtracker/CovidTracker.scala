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
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("CovidTracker")
object CovidTracker {

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
          rows.sortBy(rdr => rdr.county)

          rows
            .map(rdr => js.Array(rdr.date, rdr.county, rdr.cases, rdr.deaths))
            .toJSArray
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
      "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
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
              RawCountyDataRow(
                parts(0),
                parts(2),
                parts(1),
                parts(4).toInt,
                parts(5).toInt
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

  def calculate(rows: Seq[RawDataRow]): Seq[DataRow] = {

    val dataMap: scala.collection.mutable.Map[String, DataRow] =
      new mutable.HashMap[String, DataRow]()

    rows
      .filter(
        rdr => !(rdr.date.contains("2020-01-") || rdr.date.contains("2020-02-"))
      )
      .foreach(rdr => {

        dataMap.get(rdr.date) match {
          case Some(dr) => {
            rdr.state match {
              case "Oklahoma" => {
                dataMap.put(
                  rdr.date,
                  DataRow(
                    rdr.date,
                    rdr.cases + dr.nationalCases,
                    rdr.cases + dr.oklahomaCases,
                    rdr.deaths + dr.nationalDeaths,
                    rdr.deaths + dr.oklahomaDeaths
                  )
                )
              }
              case _ => {
                dataMap.put(
                  rdr.date,
                  dr.copy(
                    nationalCases = dr.nationalCases + rdr.cases,
                    nationalDeaths = dr.nationalDeaths + rdr.deaths
                  )
                )
              }
            }
          }
          case None =>
            rdr.state match {
              case "Oklahoma" =>
                dataMap.put(
                  rdr.date,
                  DataRow(
                    rdr.date,
                    rdr.cases,
                    rdr.cases,
                    rdr.deaths,
                    rdr.deaths
                  )
                )
              case _ =>
                dataMap
                  .put(rdr.date, DataRow(rdr.date, rdr.cases, 0, rdr.deaths, 0))
            }

        }

      })

    dataMap.values.toSeq.sortBy(dr => dr.date)
  }

}

case class DataError(errorMsg: String)

case class RawDataRow(date: String, state: String, cases: Int, deaths: Int)

case class RawCountyDataRow(date: String,
                            state: String,
                            county: String,
                            cases: Int,
                            deaths: Int)

case class DataRow(date: String,
                   nationalCases: Int,
                   oklahomaCases: Int,
                   nationalDeaths: Int,
                   oklahomaDeaths: Int)
