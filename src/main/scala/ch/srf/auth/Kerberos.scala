package ch.srf.auth

import ch.srf.auth.Domain.{SRF, UserWithoutRole}
import org.springframework.security.core.{Authentication, GrantedAuthority}
import org.springframework.security.kerberos.authentication.{KerberosServiceAuthenticationProvider, KerberosServiceRequestToken}
import org.springframework.security.core.userdetails.{UserDetailsService, User => SpringUser}

import scala.collection.JavaConverters._
import scalaz.{-\/, \/, \/-}


object Kerberos {

  def kerberosServiceAuthenticationProvider: KerberosServiceAuthenticationProvider = ???

  def kerberosServiceRequestTokenFromAuthHeaderValue(authHeaderValue: String): \/[Throwable, KerberosServiceRequestToken] = {

    for {
      _ <-
      if (authHeaderValue.startsWith("Negotiate ") || authHeaderValue.startsWith("Kerberos ")) {
        \/-(())
      } else
        -\/(new RuntimeException("Something went wrong during the Kerberos authentication: Neither Negotiate Nor Kerberos in RequestHeader"))
      substring <- \/.fromTryCatchNonFatal(authHeaderValue.substring(authHeaderValue.indexOf(" ") + 1))
      base64Bytes <- \/.fromTryCatchNonFatal(substring.getBytes("UTF-8"))
    } yield new KerberosServiceRequestToken(base64Bytes)
  }


  private[auth] def authenticatedUserWithoutRole(kerberosServiceRequestToken: KerberosServiceRequestToken): \/[Throwable, UserWithoutRole] = {

    case class SpringUserDetailsDummy(username: String) extends SpringUser(username, "", List.empty[GrantedAuthority].asJava)

    def userWithoutRole(authentication: Authentication) = {
      val userDummy = authentication.getPrincipal.asInstanceOf[SpringUserDetailsDummy]
      UserWithoutRole(username = userDummy.username.takeWhile(_ != '@'), bu = SRF())
    }

    for {
      // main call to Kerberos
      authentication <- \/.fromTryCatchNonFatal(kerberosServiceAuthenticationProvider.authenticate(kerberosServiceRequestToken))
//      _ <-
//      if (authentication.isAuthenticated) {
//        \/-(())
//      } else
//        -\/(new RuntimeException("Kerberos: not authenticated"))
      user <- \/.fromTryCatchNonFatal(userWithoutRole(authentication))
    } yield user
  }
}
