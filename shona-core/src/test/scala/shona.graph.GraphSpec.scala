//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package graph

import org.specs2.mutable._

import record._
import Property._

class GraphSpec extends Specification {
  val id = Sing("id")
  val name = Sing("name")
  val venue = Sing("venue")
  val address = Sing("address")
  val review = Sing("review")
  val venueId = Sing("venueId")
  val street = Sing("street")
  val city = Sing("city")
  val rating = Sing("rating")
  val comment = Sing("comment")

  val venueV = Vertex[venue.T] ~ (
    int   [id.T], 
    string[name.T]
  )

  val addressV = Vertex[address.T] ~ (
    int   [venueId.T], 
    string[street.T], 
    string[city.T]
  )

  val reviewV = Vertex[review.T] ~ (
    int   [id.T], 
    string[venueId.T], 
    string[rating.T], 
    string[comment.T]
  )

  val graph = Graph(venueV, addressV, reviewV)(
    Edge[address.T] ~ (venueV, addressV, Mapping.identity(int[id.T], int[venueId.T])), 
    Edge[review.T] ~ (venueV, reviewV, Mapping(int[id.T], string[venueId.T])("venue" + _.toString))
  )

  "Shona Graph" should {
    "support static lookup of vertex" in {
      Graph.get(graph)("venue") === venueV
      Graph.get(graph)("address") === addressV
    }
  }
}
