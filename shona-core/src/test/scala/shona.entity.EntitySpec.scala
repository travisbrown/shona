//  __..                                                                                   Copyright (c) 2013, Alois Cochard
// (__ |_  _ ._  _.
// .__)[ )(_)[ )(_]
//
// Licensed under the Apache License, Version 2.0

package shona
package entity

import org.specs2.mutable._

import record._

class EntitySpec extends Specification {
  val id = Sing("id"); import id._
  val email = Sing("email"); import email._

  val entity = Entity((Field[id.T] ~ 1000L, Field[email.T] ~ "alois.cochard@gmail.com"))

  "Shona Entity" should {
    "support view to access an entity fields" in {
      val view = View(entity)
      view.id === 1000L
      view.email === "alois.cochard@gmail.com"
      view.entity === entity
    }

    "not screw up the bytecode when the same view is requested multiple times" in {
      val view0 = View(entity)
      val view1 = View(entity)
      view0.entity === view1.entity
    }
  }
}
