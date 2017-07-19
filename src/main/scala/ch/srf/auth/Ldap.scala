package ch.srf.auth

import javax.naming.directory.DirContext

import ch.srf.auth.Config.LdapConfigWithBaseDn

import scala.collection.JavaConversions._
import ch.srf.auth.Domain.{BU, UserCredentials, UserWithoutRole}
import org.springframework.ldap.core.{AuthenticatedLdapEntryContextMapper, LdapEntryIdentification, LdapTemplate}
import org.springframework.ldap.query.{LdapQuery, LdapQueryBuilder}
import org.springframework.ldap.core.support.LdapContextSource

import scalaz.{-\/, \/, \/-}

object Ldap {

  def template(config: LdapConfigWithBaseDn): LdapTemplate = {
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


  /**
    *
    * @param attribute
    * @param credentials
    * @return
    */
  def query(attribute: String, credentials: UserCredentials): LdapQuery =
    LdapQueryBuilder.query().base("").where(attribute).is(credentials.username)

  def password(credentials: UserCredentials): String = credentials.password

  def mapper: AuthenticatedLdapEntryContextMapper[List[BU]] = new AuthenticatedLdapEntryContextMapper[List[BU]] {
    override def mapWithContext(ctx: DirContext, ldapEntryIdentification: LdapEntryIdentification): List[BU] = {
      ldapEntryIdentification.getAbsoluteName.getRdns.toList
        .filter(rdn => rdn.getType.toLowerCase == "ou")
        .map(rdn => rdn.getValue.toString.toLowerCase)
        .map(ou => BU.forName(ou)).flatten
    }
  }


  // main target
  private[auth] def authenticatedUserWithoutRole(query: LdapQuery,
                                                 credentials: UserCredentials, // credentials instead of query and password, because username, bu is needed for errormessages
                                                 mapper: AuthenticatedLdapEntryContextMapper[List[BU]]): \/[Throwable, UserWithoutRole] = {

    def isAuthenticated(buList: List[BU]) =
      if (buList.nonEmpty) {
        \/-(())
      } else
        -\/(new RuntimeException("Ldap: User ${credentials.username} not authenticated for any bu"))

    def bu(buList: List[BU], credentials: UserCredentials) =
      buList.find(_ == credentials.bu) match {
        case Some(bu) => \/-(bu)
        case _ => -\/(new RuntimeException(s"Ldap: User ${credentials.username} not authenticated for $buList"))
      }


    val buList = template.authenticate(query, password(credentials), mapper)

    for {
      _ <- isAuthenticated(buList)
      bu <- bu(buList, credentials)
    } yield UserWithoutRole(credentials.username, bu)

  }
}
