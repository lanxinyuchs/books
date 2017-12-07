#!/bin/bash


export LDAPNOINIT
export LDAPTLS_CACERT=${LDAPTLS_CACERT:-$HOME/git/base/sys/config/ca-cert.pem}


LDAPSERVER=137.58.180.11
#LDAPSERVER=ls13
usage() {
    cat <<eot
    $0 dump - get everything from the server, useful to see if it running
    $0 add <User> <Password> <Role1> [<Role2>...] - create a new user
    $0 modify <User> <Role1> [<Role2>...] -  change the roles on existing user
    $0 delete <User>                              - remove an existing user
    $0 search_simple <User> <Password>
    $0 search_sasl <User> <Password>
    $0 search_starttls <User> <Password>
    $0 search_ssl <User> <Password>

    All the above also accepts -h <LDAP_SERVER_HOSTNAME> as an initial
    argument. Default is $LDAPSERVER, but actual RCS code may be using ls14
    (hardcoded).

    The various search_* thingies affect how the client authenticate and talk
    to the server: 
    simple - ldap on the wire, password and everything else in plain
    sasl   - a hash of the password is transferred 
    starttls - communication switches over to TLS
    ssl - uses alternate port number to run ssl (nonstandard)

    dump, add, delete, modify: these are maintenance commands implemented
    here to facilitate testing, they have no security and works only
    because the ldap server has been configured to allow this.

eot
}

if [ ! -r "$LDAPTLS_CACERT" ]; then
    echo No CA cert found, try setting the variable LDATLS_CACERT\
        to point to it if you need one
    echo There may be a cert in the ...git/base/sys/config -directory
fi

if [ "_$1" = _-h ];then
    LDAPSERVER="${2:?missing argument LDAP_SERVER_HOSTNAME}"
    shift 2
fi

case ${1:-noop} in
    -h)
        LDAPSERVER="${2:?missing argument LDAP_SERVER_HOSTNAME}"
        shift 2
        ;;
    #dump everything relevant (all users) + some more
    dump)
        ldapsearch -LLL -x -H ldap://$LDAPSERVER -D cn=king,dc=mordor,dc=invalid \
            -b dc=mordor,dc=invalid \
            -w hello
        ;;
    search_simple)
        ldapsearch -LLL -x -H ldap://$LDAPSERVER -D uid=${2:-*},dc=mordor,dc=invalid \
            -b dc=mordor,dc=invalid \
            -w ${3:?missing arg: password} uid=${2:-*} title
        ;;
    search_sasl)
        ldapsearch -LLL -Q -Y DIGEST-MD5 -H ldap://$LDAPSERVER \
            -U ${2:?missing arg: user} \
            -b dc=mordor,dc=invalid \
            -w $3 uid=${2:-*} title
            #realm not used by sasl in my environment -R ${2:?missing arg: user}@${4:?missing arg: realm}
        ;;
    search_starttls)
        ldapsearch -LLL -x -ZZ -H ldap://$LDAPSERVER \
            -D uid=${2:-*},dc=mordor,dc=invalid \
            -b dc=mordor,dc=invalid \
            -w ${3:?missing arg: password} uid=${2} title
        ;;
    search_ssl)
        ldapsearch -LLL -x -H ldaps://$LDAPSERVER \
            -D uid=${2:-*},dc=mordor,dc=invalid \
            -b dc=mordor,dc=invalid \
            -w ${3:?missing arg: password} uid=${2} title
        ;;
    delete)
        ldapdelete -v -x -w hello -H ldap://$LDAPSERVER \
            -D cn=king,dc=mordor,dc=invalid \
            uid=${2:?missin arg: user},dc=mordor,dc=invalid
        ;;
    add)
        User=${2:?missing arg: user}
        Pw=${3:?missing arg: password}
        shift 3
        Roles=$@
        ldapadd -v -x -w hello -H ldap://$LDAPSERVER \
             -D "cn=king,dc=mordor,dc=invalid" <<-eot
dn: uid=${User},dc=mordor,dc=invalid
objectClass: inetOrgPerson
cn: $User (common name)
sn: $User (shortname)
userPassword: ${Pw}
${Roles:+title: $Roles}
eot
;;
    modify)
        User=${2:?missing arg: user}
        shift 2
        Roles=${@:?missing arg: roles}
        ldapmodify -v -x -w hello -H ldap://$LDAPSERVER \
             -D "cn=king,dc=mordor,dc=invalid" <<-eot
dn: uid=${User},dc=mordor,dc=invalid
changetype: modify
replace: title
title: $Roles
eot
;;


#This will add the "root" stuff to a new database, normally needed only if
#everything is reinstalled.
    add_base)
        ldapadd -v -x -w hello -H ldap://$LDAPSERVER \
             -D "cn=king,dc=mordor,dc=invalid" <<-eot
dn: dc=mordor,dc=invalid
objectClass: dcObject
objectClass: inetOrgPerson
cn: kings domain
sn: kings yard
eot
;;
    noop|*)
        echo wrong or missing argument >&2
        usage
        exit 1
        ;;
esac

# sasl          -LLL -Y DIGEST-MD5 -Q -H ldap://<IP> -U User -R Realm -b searchbase -w Pw uid=User attribute
# simpe         -LLL -x     -H ldap://<IP> -D Binddn -b Searchbase -w Pw uid=User attribute
# ssl           -LLL -x     -H ldaps://<IP> -D Binddn -b Searchbase -w Pw uid=User attribute
# startls       -LLL -x -ZZ -H ldap://<IP> -D Binddn -b Searchbase -w Pw uid=User attribute
