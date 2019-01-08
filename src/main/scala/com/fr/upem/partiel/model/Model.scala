package com.fr.upem.partiel.model

class Model {

  trait Accout
  object  Accout {

    def apply: Accout = ???

    trait Transaction

    trait User

    trait Categorie
    object Categorie {

      final case object Salary extends Categorie

      final case object purchase extends Categorie

      final case object checkDeposit extends Categorie

      final case object checkPayment extends Categorie

      final case object withdrawal extends Categorie

    }

    def banckAccout(transaction : Transaction , categories : List[Categorie]): Accout = ???
    def user(id: Accout, name : String): User = ???
    def transaction(id: String, amount : String, date: String ) : Transaction = ???

  }
}
