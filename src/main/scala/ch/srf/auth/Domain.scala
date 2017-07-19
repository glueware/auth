package ch.srf.auth

object Domain {

  object Role {

    private val superEditorString = "role_super_editor"
    private val webEditorString = "role_web_editor"

    def fromString(s: String): Option[Role] = s.toLowerCase match {
      case `superEditorString` => Some(RoleSuperEditor())
      case `webEditorString` => Some(RoleWebEditor())
      case _ => None
    }
  }

  object BU {


    def forName(s: String): Option[BU] = s.toLowerCase match {
      case "srf" => Some(SRF())
      case "rtr" => Some(RTR())
      case _ => None
    }

  }

  sealed abstract class BU(val name: String) {
    def fold[X](onSrf: SRF => X,
                onRtr: RTR => X) = this match {
      case SRF() => onSrf(SRF())
      case RTR() => onRtr(RTR())
    }

    final override def toString: String = name
  }

  case class RTR() extends BU("RTR")
  case class SRF() extends BU("SRF")

  sealed trait Role {
    import Role._

    private[Domain] def hierarchyLevel: Int

    final def fold[X](onSuperEditor: => X, onWebEditor: => X): X = this match {
      case RoleSuperEditor() => onSuperEditor
      case RoleWebEditor() => onWebEditor
    }

    final override def toString = fold(
      onSuperEditor = superEditorString,
      onWebEditor = webEditorString
    )
  }

  case class RoleSuperEditor() extends Role {
    private[Domain] val hierarchyLevel: Int  = 1
  }

  case class RoleWebEditor() extends Role {
    private[Domain] val hierarchyLevel: Int  = 2
  }

  //from db
  /**
    * @param username
    * @param role
    * @param bu
    * @param buOverride Which bu to use as a default setting for bu specific functionality
    */
  case class User(username: String, role: Role, bu: BU, buOverride: BU) {
    override def toString() = s"$username/$role/$bu"
  }

  case class UserWithoutRole(username: String, bu: BU) {
    def withRole(r: Role) = User(username = username, role = r, bu = bu, buOverride = bu)
  }

  //from client
  case class UserCredentials(username: String, password: String, bu: BU) {
    override def toString() = s"$username/$bu"
    def toUserWithoutRole = UserWithoutRole(username = username, bu = bu)
  }


}
