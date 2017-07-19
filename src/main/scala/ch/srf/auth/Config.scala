package ch.srf.auth

object Config {
  /*
  What is this split good for? Some BUs have multiple baseDns.
   */
  case class LdapConfig(url: String, userDn: String, password: String)

  case class LdapConfigWithBaseDn(baseDn: String, userNameAttr: String, sub: LdapConfig)

}
