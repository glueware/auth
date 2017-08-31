package ch.srf.auth

import javax.naming.directory.DirContext

import ch.srf.auth.Config.LdapConfigWithBaseDn

import scala.collection.JavaConversions._
import ch.srf.auth.Domain._
import org.springframework.ldap.core.{AuthenticatedLdapEntryContextMapper, LdapEntryIdentification, LdapTemplate}
import org.springframework.ldap.query.{LdapQuery, LdapQueryBuilder}
import org.springframework.ldap.core.support.LdapContextSource

import scalaz.{-\/, \/, \/-}
import scalaz.concurrent.Task
import scala.concurrent.duration._


object Ldap {
  private val config: LdapConfigWithBaseDn = ???

  lazy val template: LdapTemplate = {
    val ldapContextSource = new LdapContextSource

    // apply ldap config
    ldapContextSource.setUrl(config.sub.url)
    ldapContextSource.setBase(config.baseDn)
    ldapContextSource.setUserDn(config.sub.userDn)
    ldapContextSource.setPassword(config.sub.password)

    // this is necessary, when the ldapContextSource is not set by a spring context
    // authenticationSource would be null otherwise
    ldapContextSource.afterPropertiesSet()

    new LdapTemplate(ldapContextSource)
  }

  private[auth] def authenticatedUserWithoutRoleT(credentials: UserCredentials): Task[AuthResult[UserWithoutRole]] = {
    // parameters

    lazy val query: LdapQuery =
      LdapQueryBuilder.query()
        .base("")
        .where(config.userNameAttr)
        .is(credentials.username)

    lazy val password = credentials.password

    lazy val username = credentials.username

    lazy val bu = credentials.bu

    lazy val mapper: AuthenticatedLdapEntryContextMapper[List[BU]] = new AuthenticatedLdapEntryContextMapper[List[BU]] {
      override def mapWithContext(ctx: DirContext, ldapEntryIdentification: LdapEntryIdentification): List[BU] = {
        ldapEntryIdentification.getAbsoluteName.getRdns.toList
          .filter(rdn => rdn.getType.toLowerCase == "ou")
          .map(rdn => rdn.getValue.toString.toLowerCase)
          .map(ou => BU.forName(ou)).flatten
      }
    }

    // this tries to do the work
    def authenticatedUserWithoutRole: AuthResult[UserWithoutRole] = {

      // this does the work
      def buList = template.authenticate(query, password, mapper)

      buList.find(_ == bu) match {
        case Some(b) => \/-(UserWithoutRole(username, b))
        case _ => -\/(BadCredentials(credentials, AuthProviderLdap))
      }
    }

    // Task[\/[Throwable, AuthResult[UserWithoutRole]]]
    val task: Task[AuthResult[UserWithoutRole]] = Task(authenticatedUserWithoutRole)

    def flatten(result: \/[Throwable, AuthResult[UserWithoutRole]]): AuthResult[UserWithoutRole] = result match {
      case -\/(_) => -\/(LdapAuthenticationTimeout(credentials))
      case \/-(r) => r
    }

    Task.fork(task).timed(2.seconds).attempt.map(flatten)
  }
}
