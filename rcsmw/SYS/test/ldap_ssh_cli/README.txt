Unless it's broken down, there is an ldap-server (slapd) running on:
ls13 ls13.rnd.ki.sw.ericsson.se 172.31.89.13
default ports ldap:389 ldaps:636
The ldap server is running with a minimal config (and probably without any
security, i.e. database is wide open) it can be used to test the RCS ldap
authentication/authorization with any of the (|not) supported methods.
o simple
o ssl
o starttls
o sasl
As far as I understand the only methods we get when plugging this into COM,
are simple and starttls (or maybe ssl instead of starttls but starttls should
be preferred).

Configure the ECIM ldap fragment with the following parameters to work with
the schema set in ls13 ldap-server:
bindDn: uid='%u',dc=mordor,dc=invalid
baseDn: dc=mordor,dc=invalid
filter: uid='%u'
type:   title

Note: the "'" around %u are literal - they must be there


ldap.sh
The ldap server on ls13 can be tried out with the ldap.sh program in this
directory, run it without params and there should be a brief usage.
This same script can also be used to populate the database with new users.

slapd.conf
The configuration used in the server on ls13
