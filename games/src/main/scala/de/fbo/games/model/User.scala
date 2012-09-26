package de.fbo.games {
  package model {

    import _root_.net.liftweb.mapper._
    import _root_.net.liftweb.util._
    import _root_.net.liftweb.common._
    import scala.xml.Text

    /**
     * The singleton that has methods for accessing the database
     */
    object User extends User with MetaMegaProtoUser[User] {
      override def dbTableName = "users" // define the DB table name
      override def screenWrap = Full(<lift:surround with="default" at="content">
                                       <lift:bind/>
                                     </lift:surround>)
      // define the order fields will appear in forms and output
      override def fieldOrder = List(id, firstName, email,
        password)

      // comment this line out to require email validations
      override def skipEmailValidation = true

      override def signupFields = List(firstName, email, password)

      override def editFields = List(firstName, email, password, locale)

    }

    /**
     * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
     */
    class User extends MegaProtoUser[User] {
      def getSingleton = User // what's the "meta" server

      override def firstNameDisplayName = "Nickname"

      def asSpieler: Spieler = Spieler(firstName)

    }

  }
}
