/* confdefs.h */
#define PACKAGE_NAME "Haskell network package"
#define PACKAGE_TARNAME "network"
#define PACKAGE_VERSION "3.1.4.0"
#define PACKAGE_STRING "Haskell network package 3.1.4.0"
#define PACKAGE_BUGREPORT "libraries@haskell.org"
#define PACKAGE_URL ""
/* end confdefs.h.  */
#ifndef _WIN32
# define _GNU_SOURCE 1  /* for struct ucred on Linux */
#endif
#include <stdio.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif
#ifdef STDC_HEADERS
# include <stdlib.h>
# include <stddef.h>
#else
# ifdef HAVE_STDLIB_H
#  include <stdlib.h>
# endif
#endif
#ifdef HAVE_STRING_H
# if !defined STDC_HEADERS && defined HAVE_MEMORY_H
#  include <memory.h>
# endif
# include <string.h>
#endif
#ifdef HAVE_STRINGS_H
# include <strings.h>
#endif
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef _WIN32
# include <winsock2.h>
# include <ws2tcpip.h>
# define IPV6_V6ONLY 27
#endif

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#ifdef HAVE_SYS_UN_H
# include <sys/un.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_NET_IF_H
# include <net/if.h>
#endif
#ifdef HAVE_NETIOAPI_H
# include <netioapi.h>
#endif

int
main ()
{
static struct sockaddr ac_aggr;
if (ac_aggr.sa_len)
return 0;
  ;
  return 0;
}
