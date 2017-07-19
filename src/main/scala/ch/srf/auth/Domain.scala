package ch.srf.auth

import scalaz.{NonEmptyList, Validation, ValidationNel, \/}

object Domain {

  sealed trait AuthProvider

  case object AuthProviderLdap extends AuthProvider

  type AuthResult[X] = \/[AuthException, X]

//  object AuthResult {
//    def success[X](x: X): AuthResult[X] = Validation.success(x).toValidationNel
//
//    def failure[X](e: AuthException): AuthResult[X] = Validation.failureNel(e)
//
//    def failure[X](e: NonEmptyList[AuthException]): AuthResult[X] = Validation.failure(e)
//  }

  sealed trait AuthException {
    def fold[X](onBadCredentials: BadCredentials => X,
                onLdapNotReachable: LdapNotReachable => X,
                onLdapAuthenticationTimeout: LdapAuthenticationTimeout => X,
                onRolesNotFetchable: RolesNotFetchable => X,
                onNoRoleAssigned: NoRoleAssigned => X,
                onRoleTextNotParseable: RoleTextNotParseable => X,
                onRolesNotMappable: RolesNotMappable => X,
                onRoleFetchTimeout: RoleFetchTimeout => X,
                onUserDoesNotBelongToBU: UserDoesNotBelongToBu => X) = this match {
      case x: BadCredentials => onBadCredentials(x)
      case x: LdapNotReachable => onLdapNotReachable(x)
      case x: LdapAuthenticationTimeout => onLdapAuthenticationTimeout(x)
      case x: RolesNotFetchable => onRolesNotFetchable(x)
      case x: NoRoleAssigned => onNoRoleAssigned(x)
      case x: RoleTextNotParseable => onRoleTextNotParseable(x)
      case x: RolesNotMappable => onRolesNotMappable(x)
      case x: RoleFetchTimeout => onRoleFetchTimeout(x)
      case x: UserDoesNotBelongToBu => onUserDoesNotBelongToBU(x)
    }
  }

  case class BadCredentials(user: UserCredentials, provider: AuthProvider) extends AuthException

  case class LdapNotReachable(user: UserCredentials, ldapUrl: NonEmptyList[String], logicalName: String) extends AuthException

  case class LdapAuthenticationTimeout(user: UserCredentials) extends AuthException

  case class RolesNotFetchable(user: UserCredentials, underlying: Throwable, url: String) extends AuthException

  case class NoRoleAssigned(user: UserCredentials, url: String) extends AuthException

  case class RoleTextNotParseable(user: UserCredentials, text: String, details: String) extends AuthException

  case class RolesNotMappable(user: UserCredentials, roles: NonEmptyList[String]) extends AuthException

  case class RoleFetchTimeout(user: UserCredentials) extends AuthException

  case class UserDoesNotBelongToBu(user: UserCredentials) extends AuthException


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
