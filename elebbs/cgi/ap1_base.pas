unit ap1_base;
(*
**
** Copyright (C) 1999-2003 Maarten Bekers. All rights reserved.
**
** This file is part of EleBBS, EleXer and EleWEB.
**
** This file may be distributed under the terms of the Q Public License
** as defined by Trolltech AS of Norway and appearing in the file
** LICENSE.QPL included in the packaging of this file.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
*)
{$I COMPILER.INC}
(*
**
** Apache1_Base.
** Base definition file for EleWEB.
**
** Copyright (c) 1999-2003 by Maarten Bekers
**
**
** Created: 10-Feb-2003
** Last update : 10-Feb-2003
**
*)
{$mode delphi}

{$IFNDEF ISCGI}
  You need to define "ISCGI" in order to compile the CGIs
{$ENDIF}

(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)
 INTERFACE
(*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-+-*-*)

{$IFDEF Win32}
  uses WinSock;
{$ENDIF}

{$IFDEF ELEUNIX}
  uses SockDef, Unix;

  {$LINKLIB c}
  {$LINKLIB crypt}
  {$LINKLIB dl}
{$ENDIF}


{-- Config dir -------------------------------------------------------------}
{- Define this to be the default server home dir. Most things later in this }
{- file with a relative pathname will have this added.                      }
{- The target name of the installed Apache                                  }

{$IFDEF Win32}
const
   ApacheCore = 'ApacheCore.dll';
{$ENDIF}

{$IFDEF ELEUNIX}
const
   ApacheCore = 'libhttpd.so'; {do not localize}
{$ENDIF}

{$IFNDEF Win32}
type
  SOCKADDR_IN = tSockAddr;
{$ENDIF}

{-- Specify the magics we use - its up to the server to decide wether this --}
{-- is compatible with our version or not -----------------------------------}
const
  MODULE_MAGIC_COOKIE       = $41503133;                              { AP13 }
  MODULE_MAGIC_NUMBER_MAJOR = 19990320;
  MODULE_MAGIC_NUMBER_MINOR = 6;
  MODULE_MAGIC_NUMBER       = MODULE_MAGIC_NUMBER_MAJOR;

{- Numeric release version identifier: MMNNFFRBB: major minor fix final beta }
{- Always increases along the same track as the source branch.               }
{- For example, Apache 1.4.2 would be '10402100', 2.5b7 would be '20500007'. }

  APACHE_RELEASE            = 10309100;
  SERVER_PROTOCOL           = 'HTTP/1.1';

{-- These constants are used in the child_handler to signal to the server ---}
{-- what the handler is going to do with the output -------------------------}
const
  AP_OK       =  0;                         { Module has handled this stage. }
  AP_DECLINED = -1;                              { Module declines to handle }
  AP_DONE     = -2;              { Module has served the response completely }
                                  { - it's safe to die() with no more output }

{-- Constants for reading client input --------------------------------------}
const
  REQUEST_NO_BODY         = 0;
  REQUEST_CHUNKED_ERROR   = 1;
  REQUEST_CHUNKED_DECHUNK = 2;
  REQUEST_CHUNKED_PASS    = 3;

{-- Methods recognized (but not necessarily handled) by the server.          }
{-- These constants are used in bit shifting masks of size int, so it is     }
{-- unsafe to have more methods than bits in an int.  HEAD == M_GET.         }
const
  M_GET        = 0;
  M_PUT        = 1;
  M_POST       = 2;
  M_DELETE     = 3;
  M_CONNECT    = 4;
  M_OPTIONS    = 5;
  M_TRACE      = 6;
  M_PATCH      = 7;
  M_PROPFIND   = 8;
  M_PROPPATCH  = 9;
  M_MKCOL      = 10;
  M_COPY       = 11;
  M_MOVE       = 12;
  M_LOCK       = 13;
  M_UNLOCK     = 14;
  M_INVALID    = 15;

  METHODS      = 16;

{-- we now map the specific Apache constants to their equilavelent in Pascal -}
type
  ap_pchar = pchar;             // char *
  ap_ppchar = ^ap_pchar;        // char **
  ap_constpchar = pchar;        // const char *
  ap_punsignedchar = ^byte;     // unsigned char *
  ap_short = smallint;          // short
  ap_pshort = ^ap_short;        // short *
  ap_unsignedshort = word;      // unsigned short
  ap_int = integer;             // int
  ap_unsigned = cardinal;       // unsigned
  ap_long = longint;            // long
  ap_unsignedlong = longword;   // unsigned long
  ap_pFILE = pointer;           // FILE *
  ap_ppFILE = ^ap_pFILE;        // FILE **
  ap_pvoid = pointer;           // void *

  // pending ...
  Ppool = pointer;              // pool *
  Ptable = pointer;             // table *
  Pserver_addr_rec = pointer;   // server_addr_rec *
  Parray_header = pointer;      // array_header *
  Pregmatch_t = pointer;        // regmatch_t *
  Pchild_info = pointer;        // child_info *
  AP_PMD5_CTX = pointer;        // AP_MD5_CTX *
  Ptm = pointer;                // tm *
  uid_t = integer;              // system type
  gid_t = integer;              // system type
  time_t = longint;             // system type
  size_t = integer;             // system type

  ap_Ppool = Ppool;
  Phostent = pointer;

{--- The allowed locations for a configuration directive are the union of      }
{--- those indicated by each set bit in the req_override mask.                 }
{--- (req_override & RSRC_CONF)   => *.conf outside <Directory> or <Location>  }
{--- (req_override & ACCESS_CONF) => *.conf inside <Directory> or <Location>   }
{--- (req_override & OR_AUTHCFG)  => *.conf inside <Directory> or<Location>    }
{---                                 and .htaccess when AllowOverride AuthConfi}
{--- (req_override & OR_LIMIT)    => *.conf inside <Directory> or <Location>   }
{---                                 and .htaccess when AllowOverride Limit    }
{--- (req_override & OR_OPTIONS)  => *.conf anywhere                           }
{---                                 and .htaccess when AllowOverride Options  }
{--- (req_override & OR_FILEINFO) => *.conf anywhere                           }
{---                                 and .htaccess when AllowOverride FileInfo }
{--- (req_override & OR_INDEXES)  => *.conf anywhere                           }
{---                                 and .htaccess when AllowOverride Indexes  }
{------------------------------------------------------------------------------}
const
   OR_NONE     =   0;
   OR_LIMIT    =   1;
   OR_OPTIONS  =   2;
   OR_FILEINFO =   4;
   OR_AUTHCFG  =   8;
   OR_INDEXES  =  16;
   OR_UNSET    =  32;
   ACCESS_CONF =  64;
   RSRC_CONF   = 128;
   OR_ALL      = (OR_LIMIT or OR_OPTIONS or OR_FILEINFO or OR_AUTHCFG or OR_INDEXES);


type
  Pstat = ^stat;
  stat = packed record
    st_dev: Word;
    st_ino: Word;
    st_mode: Word;
    st_nlink: SmallInt;
    st_uid: SmallInt;
    st_gid: SmallInt;
    st_rdev: Word;
    st_size: Longint;
    st_atime: Longint;
    st_mtime: Longint;
    st_ctime: Longint;
  end;

{-- Apache data structures --------------------------------------------------}
type
  pURI_Components = ^uri_components;

  uri_components = packed record
    scheme: pchar;		///* scheme ("http"/"ftp"/...) */
    hostinfo: pchar;            ///* combined [user[:password]@]host[:port] */
    user: pchar;		///* user name, as in http://user:passwd@host:port/ */
    password: pchar;		///* password, as in http://user:passwd@host:port/ */
    hostname: pchar;		///* hostname from URI (or from Host: header) */
    port_str: pchar;		///* port string (integer representation is in "port") */
    path: pchar;		///* the request path (or "/" if only scheme://host was given) */
    query: pchar;		///* Everything after a '?' in the path, if present */
    fragment: pchar;		///* Trailing "#fragment" string, if present */

    hostent: Phostent;

    port: word;	                ///* The port number, numeric, valid only if port_str != NULL */

    is_initialized: cardinal;   // = 1;

    dns_looked_up: cardinal;    // = 1;
    dns_resolved: cardinal;     // = 1;
  end;

///* Things which may vary per file-lookup WITHIN a request ---
// * e.g., state of MIME config.  Basically, the name of an object, info
// * about the object, and any other info we may ahve which may need to
// * change as we go poking around looking for it (e.g., overridden by
// * .htaccess files).
// *
// * Note how the default state of almost all these things is properly
// * zero, so that allocating it with pcalloc does the right thing without
// * a whole lot of hairy initialization... so long as we are willing to
// * make the (fairly) portable assumption that the bit pattern of a NULL
// * pointer is, in fact, zero.
// */

///* This represents the result of calling htaccess; these are cached for
// * each request.
// */
  Phtaccess_result = ^htaccess_result;
  htaccess_result = packed record
    dir: ap_pchar;      	///* the directory to which this applies */
    override: ap_int;		///* the overrides allowed for the .htaccess file */
    htaccess: ap_pvoid;		///* the configuration directives */

    ///* the next one, or NULL if no more; N.B. never change this */
    next: Phtaccess_result;
  end;

  PSPerServer = ^SPerServer;
  SPerServer = packed record
    szServer: ap_pchar;
    szTag: ap_pchar;
  end;

  Pserver_rec = ^server_rec;
  server_rec = packed record
    next: Pserver_rec;

    ///* description of where the definition came from */
    defn_name: ap_constpchar;
    defn_line_number: ap_unsigned;

    ///* Full locations of server config info */

    srm_confname: ap_pchar;
    access_confname: ap_pchar;

    ///* Contact information */

    server_admin: ap_pchar;
    server_hostname: ap_pchar;
    port: ap_unsignedshort;	///* for redirects, etc. */

    ///* Log files --- note that transfer log is now in the modules... */

    error_fname: ap_pchar;
    error_log: ap_pFILE;
    loglevel: ap_int;

    ///* Module-specific configuration for server, and defaults... */

    is_virtual: ap_int;		///* true if this is the virtual server */
    module_config: ap_pvoid;	///* Config vector containing pointers to
				// * modules' per-server config structures.
				// */
    lookup_defaults: ap_pvoid;	///* MIME type info, etc., before we start
				// * checking per-directory info.
				// */
    ///* Transaction handling */

    addrs: Pserver_addr_rec;
    timeout: ap_int;		///* Timeout, in seconds, before we give up */
    keep_alive_timeout: ap_int;	///* Seconds we'll wait for another request */
    keep_alive_max: ap_int;	///* Maximum requests per connection */
    keep_alive: ap_int;		///* Use persistent connections? */
    send_buffer_size: ap_int;	///* size of TCP send buffer (in bytes) */

    path: ap_pchar;		///* Pathname for ServerPath */
    pathlen: ap_int;		///* Length of path */

    names: Parray_header;	///* Normal names for ServerAlias servers */
    wild_names: Parray_header;	///* Wildcarded names for ServerAlias servers */

    server_uid: uid_t;          ///* effective user id when calling exec wrapper */
    server_gid: gid_t;          ///* effective group id when calling exec wrapper */

    limit_req_line: ap_int;      ///* limit on size of the HTTP request line    */
    limit_req_fieldsize: ap_int; ///* limit on size of any request header field */
    limit_req_fields: ap_int;    ///* limit on number of request header fields  */
  end;

  PSPerDir = ^SPerDir;
  SPerDir = packed record
    szDir: ap_pchar;
    szTag: ap_pchar;
  end;

  PBUFF = ^BUFF;                // BUFF *
  PPBUFF = ^PBUFF;              // BUFF **
  BUFF = packed record
  end;

  Pconn_rec = ^conn_rec;
  conn_rec = packed record
    pool: ap_Ppool;
    server: Pserver_rec;
    base_server: Pserver_rec;	///* Physical vhost this conn come in on */
    vhost_lookup_data: ap_pvoid;	///* used by http_vhost.c */

    ///* Information about the connection itself */

    child_num: integer;		///* The number of the child handling conn_rec */
    client: PBUFF;		///* Connection to the guy */

    ///* Who is the client? */

    local_addr: sockaddr_in;   ///* local address */
    remote_addr: sockaddr_in;  ///* remote address */
    remote_ip: ap_pchar;       ///* Client's IP address */
    remote_host: ap_pchar;     ///* Client's DNS name, if known.
			       // * NULL if DNS hasn't been checked,
			       // * "" if it has and no address was found.
			       // * N.B. Only access this though
			       // * get_remote_host() */
    remote_logname: ap_pchar;  ///* Only ever set if doing rfc1413 lookups.
			       // * N.B. Only access this through
			       // * get_remote_logname() */
    user: ap_pchar;	       ///* If an authentication check was made,
			       // * this gets set to the user name.  We assume
			       // * that there's only one user per connection(!)
			       // */
    ap_auth_type: ap_pchar;    ///* Ditto. */

    flags: integer;
    		                ///* Are we still talking? */
    	                        ///* Are we using HTTP Keep-Alive?
				// * -1 fatal error, 0 undecided, 1 yes */
    	                        ///* Did we use HTTP Keep-Alive? */
                                ///* have we done double-reverse DNS?
				// * -1 yes/failure, 0 not yet, 1 yes/success */
    keepalives: integer;	///* How many times have we used it? */
    local_ip: ap_pchar;		///* server IP address */
    local_host: ap_pchar;	///* used for ap_get_server_name when
				// * UseCanonicalName is set to DNS
				// * (ignores setting of HostnameLookups) */
  end;

  Prequest_rec = ^request_rec;
  request_rec = packed record
    pool: ap_Ppool;
    connection: Pconn_rec;
    server: Pserver_rec;

    next: Prequest_rec;      ///* If we wind up getting redirected,
				// * pointer to the request we redirected to.
				// */
    prev: Prequest_rec; 	///* If this is an internal redirect,
				// * pointer to where we redirected *from*.
				// */

    main: Prequest_rec; 	///* If this is a sub_request (see request.h)
				// * pointer back to the main request.
				// */

    ///* Info about the request itself... we begin with stuff that only
    // * protocol.c should ever touch...
    // */

    the_request: ap_pchar;  	///* First line of request, so we can log it */
    backwards: ap_int; 	        ///* HTTP/0.9, "simple" request */
    proxyreq: ap_int;		///* A proxy request (calculated during
				// * post_read_request or translate_name) */
    header_only: ap_int; 	///* HEAD request, as opposed to GET */
    protocol: ap_pchar;		///* Protocol, as given to us, or HTTP/0.9 */
    proto_num: ap_int;		///* Number version of protocol; 1.1 = 1001 */
    hostname: ap_constpchar;	///* Host, as set by full URI or Host: */

    request_time: time_t;	///* When the request started */

    status_line: ap_constpchar; ///* Status line, if set by script */
    status: ap_int;		///* In any case */

    ///* Request method, two ways; also, protocol, etc..  Outside of protocol.c,
    // * look, but don't touch.
    // */

    method: ap_constpchar;      ///* GET, HEAD, POST, etc. */
    method_number: ap_int;	///* M_GET, M_POST, etc. */

    ///*
    //	allowed is a bitvector of the allowed methods.

    //	A handler must ensure that the request method is one that
    //	it is capable of handling.  Generally modules should DECLINE
    //	any request methods they do not handle.  Prior to aborting the
    //	handler like this the handler should set r->allowed to the list
    //	of methods that it is willing to handle.  This bitvector is used
    //	to construct the "Allow:" header required for OPTIONS requests,
    //	and METHOD_NOT_ALLOWED and NOT_IMPLEMENTED status codes.

    //	Since the default_handler deals with OPTIONS, all modules can
    //	usually decline to deal with OPTIONS.  TRACE is always allowed,
    //	modules don't need to set it explicitly.

    //	Since the default_handler will always handle a GET, a
    //	module which does *not* implement GET should probably return
    //	METHOD_NOT_ALLOWED.  Unfortunately this means that a Script GET
    //	handler can't be installed by mod_actions.
    //*/
    allowed: ap_int;		///* Allowed methods - for 405, OPTIONS, etc */

    sent_bodyct: ap_int;	///* byte count in stream is for body */
    bytes_sent: ap_long;        ///* body byte count, for easy access */
    mtime: time_t;		///* Time the resource was last modified */

    ///* HTTP/1.1 connection-level features */

    chunked: ap_int;		///* sending chunked transfer-coding */
    byterange: ap_int;		///* number of byte ranges */
    boundary: ap_pchar;		///* multipart/byteranges boundary */
    range: ap_constpchar; 	///* The Range: header */
    clength: ap_long;		///* The "real" content length */

    remaining: ap_long;		///* bytes left to read */
    read_length: ap_long;	///* bytes that have been read */
    read_body: ap_int;		///* how the request body should be read */
    read_chunked: ap_int;	///* reading chunked transfer-coding */
    expecting_100: ap_unsigned;	///* is client waiting for a 100 response? */

    ///* MIME header environments, in and out.  Also, an array containing
    // * environment variables to be passed to subprocesses, so people can
    // * write modules to add to that environment.
    // *
    // * The difference between headers_out and err_headers_out is that the
    // * latter are printed even on error, and persist across internal redirects
    // * (so the headers printed for ErrorDocument handlers will have them).
    // *
    // * The 'notes' table is for notes from one module to another, with no
    // */ other set purpose in mind...
    // */

    headers_in: Ptable;
    headers_out: Ptable;
    err_headers_out: Ptable;
    subprocess_env: Ptable;
    notes: Ptable;

    ///* content_type, handler, content_encoding, content_language, and all
    // * content_languages MUST be lowercased strings.  They may be pointers
    // * to static strings; they should not be modified in place.
    // */
    content_type: ap_constpchar; ///* Break these out --- we dispatch on 'em */
    handler: ap_constpchar;      ///* What we *really* dispatch on           */

    content_encoding: ap_constpchar;
    content_language: ap_constpchar;	///* for back-compat. only -- do not use */
    content_languages: Parray_header;   ///* array of (char*) */

    vlist_validator: ap_pchar;  ///* variant list validator (if negotiated) */

    no_cache: ap_int;
    no_local_copy: ap_int;

    ///* What object is being requested (either directly, or via include
    // * or content-negotiation mapping).
    // */

    unparsed_uri: ap_pchar;	///* the uri without any parsing performed */
    uri: ap_pchar;			///* the path portion of the URI */
    filename: ap_pchar;
    path_info: ap_pchar;
    args: ap_pchar;		///* QUERY_ARGS, if any */
    finfo: stat;		///* ST_MODE set to zero if no such file */
    parsed_uri: uri_components; ///* components of uri, dismantled */

    ///* Various other config info which may change with .htaccess files
    // * These are config vectors, with one void* pointer for each module
    // * (the thing pointed to being the module's business).
    // */

    per_dir_config: ap_pvoid;	///* Options set in config files, etc. */
    request_config: ap_pvoid;	///* Notes on *this* request */

///*
// * a linked list of the configuration directives in the .htaccess files
// * accessed by this request.
// * N.B. always add to the head of the list, _never_ to the end.
// * that way, a sub request's list can (temporarily) point to a parent's list
// */
    htaccess: Phtaccess_result; // pointer to constant. * gcm *

///* Things placed at the end of the record to avoid breaking binary
// * compatibility.  It would be nice to remember to reorder the entire
// * record to improve 64bit alignment the next time we need to break
// * binary compatibility for some other reason.
// */
  end;

///* Note that for all of these except RAW_ARGS, the config routine is
// * passed a freshly allocated string which can be modified or stored
// * or whatever... it's only necessary to do pstrdup() stuff with
// * RAW_ARGS.
// */

  cmd_how = (
    RAW_ARGS,			///* cmd_func parses command line itself */
    TAKE1,			///* one argument only */
    TAKE2,			///* two arguments only */
    ITERATE,			///* one argument, occuring multiple times
				// * (e.g., IndexIgnore)
				// */
    ITERATE2,			///* two arguments, 2nd occurs multiple times
				// * (e.g., AddIcon)
				// */
    FLAG,			///* One of 'On' or 'Off' */
    NO_ARGS,			///* No args at all, e.g. </Directory> */
    TAKE12,			///* one or two arguments */
    TAKE3,			///* three arguments only */
    TAKE23,			///* two or three arguments */
    TAKE123,			///* one, two or three arguments */
    TAKE13			///* one or three arguments */
);


  TCommandFunc = function : pchar;
  Pcommand_rec = ^command_rec;
  command_rec = packed record
    name: pchar;		///* Name of this command */
    func: TCommandFunc;	        ///* Function invoked */
    cmd_data: pointer;		///* Extra data, for functions which
                                // * implement multiple commands...
                                // */
    req_override: integer;	///* What overrides need to be allowed to
                                // * enable this command.
                                // */
    args_how: cmd_how;	        ///* What the command expects as arguments */
    errmsg: pchar;		///* 'usage' message, in case of syntax errors */
  end;

///* This structure records the existence of handlers in a module... */

  THandlerFunc = function (rr: Prequest_rec): integer; stdcall;
  Phandler_rec = ^handler_rec;
  handler_rec = packed record
    content_type: pchar;	///* MUST be all lower case */
    handler: THandlerFunc;
  end;

  Pmodule = ^module;
  module = packed record
    version: integer;  	        ///* API version, *not* module version;
                                // * check that module is compatible with this
                                // * version of the server.
                                // */
    minor_version: integer;     ///* API minor version. Provides API feature
                                // * milestones. Not checked during module init
                                // */
    module_index: integer;	///* Index to this modules structures in
                                // * config vectors.
                                // */

    name: pchar;
    dynamic_load_handle: pointer;
    next: Pmodule;
    magic: integer;             ///* Magic Cookie to identify a module structure;
                                // * It's mainly important for the DSO facility
                                // * (see also mod_so).
                                // */

    ///* init() occurs after config parsing, but before any children are
    // * forked.
    // * Modules should not rely on the order in which create_server_config
    // * and create_dir_config are called.
    // */

    init: procedure (s: Pserver_rec; p: Ppool); stdcall;
    create_dir_config: function (p: Ppool; dir: pchar): pointer; stdcall;
    merge_dir_config: function (p: Ppool; base_conf, new_conf: pointer): pointer; stdcall;
    create_server_config: function (p: Ppool; s: Pserver_rec): pointer; stdcall;
    merge_server_config: function (p: Ppool; base_conf, new_conf: pointer): pointer; stdcall;

    cmds: Pcommand_rec;
    handlers: Phandler_rec;

    ///* Hooks for getting into the middle of server ops...

    // * translate_handler --- translate URI to filename
    // * access_checker --- check access by host address, etc.   All of these
    // *                    run; if all decline, that's still OK.
    // * check_user_id --- get and validate user id from the HTTP request
    // * auth_checker --- see if the user (from check_user_id) is OK *here*.
    // *                  If all of *these* decline, the request is rejected
    // *                  (as a SERVER_ERROR, since the module which was
    // *                  supposed to handle this was configured wrong).
    // * type_checker --- Determine MIME type of the requested entity;
    // *                  sets content_type, _encoding and _language fields.
    // * logger --- log a transaction.
    // * post_read_request --- run right after read_request or internal_redirect,
    // *                  and not run during any subrequests.
    // */

    translate_handler: THandlerFunc;
    ap_check_user_id: THandlerFunc;
    auth_checker: THandlerFunc;
    access_checker: THandlerFunc;
    type_checker: THandlerFunc;
    fixer_upper: THandlerFunc;
    logger: THandlerFunc;
    header_parser: THandlerFunc;

    //* Regardless of the model the server uses for managing "units of
    // * execution", i.e. multi-process, multi-threaded, hybrids of those,
    // * there is the concept of a "heavy weight process".  That is, a
    // * process with its own memory space, file spaces, etc.  This method,
    // * child_init, is called once for each heavy-weight process before
    // * any requests are served.  Note that no provision is made yet for
    // * initialization per light-weight process (i.e. thread).  The
    // * parameters passed here are the same as those passed to the global
    // * init method above.
    // */
    child_init: procedure (s: Pserver_rec; p: Ppool); stdcall;
    child_exit: procedure (s: Pserver_rec; p: Ppool); stdcall;

    post_read_request: function (r: Prequest_rec): integer; stdcall;
  end;

  TCompFunc = function (rec: ap_pvoid; const key, value: ap_pchar): ap_int;
  TCleanupFunc = procedure (p: ap_pvoid);
  TSpawnFunc = procedure (p: ap_pvoid; ci: Pchild_info);
  TbSpawnFunc = function (p: ap_pvoid; ci: Pchild_info): ap_int;

  Pregex_t = ^regex_t;
  regex_t = packed record
    re_magic: ap_int;
    re_nsub: size_t;	        ///* number of parenthesized subexpressions */
    re_endp: ap_pchar;	        ///* end pointer for REG_PEND */
    re_g: pointer;	        ///* none of your business :-) */
  end;

  kill_conditions = (
    kill_never,			///* process is never sent any signals */
    kill_always,		///* process is sent SIGKILL on pool cleanup */
    kill_after_timeout,		///* SIGTERM, wait 3 seconds, SIGKILL */
    just_wait,			///* wait forever for the process to complete */
    kill_only_once		///* send SIGTERM and then wait */
);

///* Common structure for reading of config files / passwd files etc. */
Pconfigfile_t = ^configfile_t;
configfile_t = packed record
    ///* a getc()-like function */
    getch: function (param: ap_pvoid): integer;
    ///* a fgets()-like function */
    getstr: function (buf: ap_pvoid; bufsiz: size_t; param: ap_pvoid): ap_pvoid;
    ///* a close hander function */
    close: function (param: ap_pvoid): integer;
    param: ap_pvoid;		///* the argument passed to getch/getstr/close */
    filename: ap_constpchar;   	///* the filename / description */
    line_number: ap_unsigned;	///* current line number, starting at 1 */
end;

///*
// * This structure is passed to a command which is being invoked,
// * to carry a large variety of miscellaneous data which is all of
// * use to *somebody*...
// */

 type
   Pcmd_parms = ^cmd_parms;
   cmd_parms = record
     info: pointer;              ///* Argument to command from cmd_table */
     override: ap_int;                ///* Which allow-override bits are set */
     limited: ap_int;            ///* Which methods are <Limit>ed */

     config_file: Pconfigfile_t;  ///* Config file structure from pcfg_openfile() */

     pool: Ppool;                ///* Pool to allocate new storage in */
     temp_pool: Ppool;           ///* Pool for scratch memory; persists during
                                 // * configuration, but wiped before the first
                                 // * request is served...
                                 // */
     server: Pserver_rec;        ///* Server_rec being configured for */
     path: PChar;                ///* If configuring for a directory,
                                 // * pathname of that directory.
                                 // * NOPE!  That's what it meant previous to the
                                 // * existance of <Files>, <Location>  and regex
                                 // * matching.  Now the only usefulness that can
                                 // * be derived from this field is whether a command
                                 // * is being called in a server context (path == NULL)
                                 // * or being called in a dir context (path != NULL).
                                 // */
     cmd: Pcommand_rec;          ///* configuration command */
     end_token: PChar;           ///* end token required to end a nested section */
     context: Pointer;           ///* per_dir_config vector passed
                                 // * to handle_command */
   end;


// HTTP status info
function ap_is_HTTP_INFO(x: ap_int): boolean;
function ap_is_HTTP_SUCCESS(x: ap_int): boolean;
function ap_is_HTTP_REDIRECT(x: ap_int): boolean;
function ap_is_HTTP_ERROR(x: ap_int): boolean;
function ap_is_HTTP_CLIENT_ERROR(x: ap_int): boolean;
function ap_is_HTTP_SERVER_ERROR(x: ap_int): boolean;

// Pool functions

function ap_make_sub_pool(p: Ppool): Ppool; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_clear_pool(p: Ppool); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_destroy_pool(p: Ppool); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_bytes_in_pool(p: Ppool): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_bytes_in_free_blocks: ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_palloc(p: Ppool; size: ap_int): ap_pvoid; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pcalloc(p: Ppool; size: ap_int): ap_pvoid; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pstrdup(p: Ppool; const s: ap_pchar): ap_pchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pstrndup(p: Ppool; const s: ap_pchar; n: ap_int): ap_pchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
// function ap_pstrcat(p: Ppool; ..): ap_pchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Array functions

function ap_make_array(p: Ppool; nelts, elt_size: ap_int): Parray_header; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_push_array(arr: Parray_header): ap_pvoid; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_array_cat(dst: Parray_header; const src: Parray_header); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_copy_array(p: Ppool; const arr: Parray_header): Parray_header; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_copy_array_hdr(p: Ppool; const arr: Parray_header): Parray_header; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_append_arrays(p: Ppool; const first, second: Parray_header): Parray_header; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Table functions
function ap_make_table(p: Ppool; nelts: ap_int): Ptable; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_copy_table(p: Ppool; const t: Ptable): Ptable; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
// !!! function ap_table_elts(t: Ptable): Parray_header; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
// !!! function ap_is_empty_table(t: Ptable): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_set(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_setn(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_merge(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_mergen(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_add(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_addn(t: Ptable; const key, value: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_table_unset(t: Ptable; const key: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_table_get(t: Ptable; const key: pchar): ap_constpchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
{$IFNDEF VER1_0_6}
procedure ap_table_do(comp: TCompFunc; rec: ap_pvoid; const t: Ptable; tag: ap_pchar = nil); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
{$ENDIF}
function ap_overlay_tables(p: Ppool; const overlay, base: Ptable): Ptable; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_clear_table(t: Ptable); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Cleanup functions

procedure ap_register_cleanup(p: Ppool; data: ap_pvoid; plain_cleanup, child_cleanup: TCleanupFunc); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_kill_cleanup(p: Ppool; data: ap_pvoid; plain_cleanup: TCleanupFunc); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_cleanup_for_exec; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_note_cleanups_for_fd(p: Ppool; fd: ap_int); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_kill_cleanups_for_fd(p: Ppool; fd: ap_int); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_note_cleanups_for_socket(p: Ppool; fd: ap_int); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_kill_cleanups_for_socket(p: Ppool; fd: ap_int); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_note_cleanups_for_file(p: Ppool; f: ap_pFILE); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_run_cleanup(p: Ppool; data: ap_pvoid; cleanup: TCleanupFunc); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// File and socket functions

function ap_popenf(p: Ppool; const name: ap_pchar; flg, mode: ap_int): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pclosef(p: Ppool; fd: ap_int): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pfopen(p: Ppool; const name, mode: ap_pchar): ap_pFILE; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pfdopen(p: Ppool; fd: ap_int; const mode: ap_pchar): ap_pFILE; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pfclose(p: Ppool; fd: ap_pFILE): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_psocket(p: Ppool; domain, _type, protocol: ap_int): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pclosesocket(p: Ppool; sock: ap_int): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// TCP/IP and I/O functions

function ap_get_virthost_addr(const hostname: ap_pchar; ports: ap_pshort): ap_unsignedlong; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_get_local_host(p: Ppool): ap_constpchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_get_remote_host(conn: Pconn_rec; dir_config: ap_pvoid; _type: ap_int): ap_constpchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_send_fd(f: ap_pFILE; r: Prequest_rec): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
// !!! function ap_send_fd_length(f: ap_pFILE; r: Prequest_rec; length: ap_long): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_send_fb(fb: pBUFF; r: Prequest_rec): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
// !!! function ap_send_fb_length(f: pBUFF; r: Prequest_rec; length: ap_long): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_rwrite(var buf; n_byte: ap_int; r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_rputc(c: ap_int; r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_rputs(const s: pchar; r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_rflush(r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_setup_client_block(r: Prequest_rec; read_policy: ap_int): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_should_client_block(r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_get_client_block(r: Prequest_rec; buffer: ap_pchar; bufsiz: ap_int): ap_long; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_send_http_header(r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_send_size(size: size_t; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Request handling functions

function ap_sub_req_lookup_uri(const new_uri: ap_pchar; const r: Prequest_rec): Prequest_rec; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_sub_req_lookup_file(const new_file: ap_pchar; const r: Prequest_rec): Prequest_rec; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_run_sub_req(r: Prequest_rec): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_destroy_sub_req(r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_internal_redirect(const uri: ap_pchar; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_internal_redirect_handler(const uri: ap_pchar; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Timeout & Alarm functions

procedure ap_hard_timeout(name: ap_pchar; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_keepalive_timeout(name: ap_pchar; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_soft_timeout(name: ap_pchar; r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_reset_timeout(r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_kill_timeout(r: Prequest_rec); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_block_alarms; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_unblock_alarms; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_check_alarm; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// Logging functions

procedure ap_log_error(const filename: pchar; line, level: ap_int; const s: Pserver_rec; const fmt: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure ap_log_rerror(const filename: pchar; line, level: ap_int; const s: Prequest_rec; const fmt: pchar); {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// URI functions

function ap_parse_uri_components(p: Ppool; const uri: ap_pchar; uptr: Puri_components): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_parse_hostinfo_components(p: Ppool; const hostinfo: ap_pchar; uptr: Puri_components): ap_int; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_unparse_uri_components(p: Ppool; const uptr: Puri_components; flags: ap_unsigned): ap_pchar; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pgethostbyname(p: Ppool; const hostname: ap_pchar): Phostent; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}
function ap_pduphostent(p: Ppool; const hp: Phostent): Phostent; {$IFDEF Win32} stdcall; {$ELSE} cdecl; {$ENDIF}

// http_protocol.h

procedure ap_note_auth_failure(r: Prequest_rec);
procedure ap_note_basic_auth_failure(r: Prequest_rec);
procedure ap_note_digest_auth_failure(r: Prequest_rec);

implementation

function ap_is_HTTP_INFO(x: ap_int): boolean;
begin
  result :=  (x >= 100) and (x < 200);
end;

function ap_is_HTTP_SUCCESS(x: ap_int): boolean;
begin
  result :=  (x >= 200) and (x < 300);
end;

function ap_is_HTTP_REDIRECT(x: ap_int): boolean;
begin
  result :=  (x >= 300) and (x < 400);
end;

function ap_is_HTTP_ERROR(x: ap_int): boolean;
begin
  result :=  (x >= 400) and (x < 600);
end;

function ap_is_HTTP_CLIENT_ERROR(x: ap_int): boolean;
begin
  result :=  (x >= 400) and (x < 500);
end;

function ap_is_HTTP_SERVER_ERROR(x: ap_int): boolean;
begin
  result :=  (x >= 500) and (x < 600);
end;

// Pool functions
function ap_make_sub_pool;              external ApacheCore name 'ap_make_sub_pool';

procedure ap_clear_pool;                external ApacheCore name 'ap_clear_pool';
procedure ap_destroy_pool;              external ApacheCore name 'ap_destroy_pool';
function ap_bytes_in_pool;              external ApacheCore name 'ap_bytes_in_pool';
{ !!!! function ap_bytes_in_free_blocks;       external ApacheCore name 'ap_bytes_in_free_pool'; }
function ap_bytes_in_free_blocks;       external ApacheCore name 'ap_bytes_in_free_blocks';
function ap_palloc;                     external ApacheCore name 'ap_palloc';
function ap_pcalloc;                    external ApacheCore name 'ap_pcalloc';
function ap_pstrdup;                    external ApacheCore name 'ap_pstrdup';
function ap_pstrndup;                   external ApacheCore name 'ap_pstrndup';
// function ap_pstrcat; external ApacheCore name 'ap_pstrcat';

// Array functions

function ap_make_array;                 external ApacheCore name 'ap_make_array';
function ap_push_array;                 external ApacheCore name 'ap_push_array';
procedure ap_array_cat;                 external ApacheCore name 'ap_array_cat';
function ap_copy_array;                 external ApacheCore name 'ap_copy_array';
function ap_copy_array_hdr;             external ApacheCore name 'ap_copy_array_hdr';
function ap_append_arrays;              external ApacheCore name 'ap_append_arrays';

// Table functions

function ap_make_table;                 external ApacheCore name 'ap_make_table';
function ap_copy_table;                 external ApacheCore name 'ap_copy_table';
// !!! function ap_table_elts;                 external ApacheCore name 'ap_table_elts';
// !!! function ap_is_empty_table;             external ApacheCore name 'ap_is_empty_table';
procedure ap_table_set;                 external ApacheCore name 'ap_table_set';
procedure ap_table_setn;                external ApacheCore name 'ap_table_setn';
procedure ap_table_merge;               external ApacheCore name 'ap_table_merge';
procedure ap_table_mergen;              external ApacheCore name 'ap_table_mergen';
procedure ap_table_add;                 external ApacheCore name 'ap_table_add';
procedure ap_table_addn;                external ApacheCore name 'ap_table_addn';
procedure ap_table_unset;               external ApacheCore name 'ap_table_unset';
function ap_table_get;                  external ApacheCore name 'ap_table_get';
procedure ap_table_do;                  external ApacheCore name 'ap_table_do';
function ap_overlay_tables;             external ApacheCore name 'ap_overlay_tables';
procedure ap_clear_table;               external ApacheCore name 'ap_clear_table';

// Cleanup functions

procedure ap_register_cleanup;          external ApacheCore name 'ap_register_cleanup';
procedure ap_kill_cleanup;              external ApacheCore name 'ap_kill_cleanup';
procedure ap_cleanup_for_exec;          external ApacheCore name 'ap_cleanup_for_exec';
procedure ap_note_cleanups_for_fd;      external ApacheCore name 'ap_note_cleanups_for_fd';
procedure ap_kill_cleanups_for_fd;      external ApacheCore name 'ap_kill_cleanups_for_fd';
procedure ap_note_cleanups_for_socket;  external ApacheCore name 'ap_note_cleanups_for_socket';
procedure ap_kill_cleanups_for_socket;  external ApacheCore name 'ap_kill_cleanups_for_socket';
procedure ap_note_cleanups_for_file;    external ApacheCore name 'ap_note_cleanups_for_file';
procedure ap_run_cleanup;               external ApacheCore name 'ap_run_cleanup';

// File and socket functions

function ap_popenf;                     external ApacheCore name 'ap_popenf';
function ap_pclosef;                    external ApacheCore name 'ap_pclosef';
function ap_pfopen;                     external ApacheCore name 'ap_pfopen';
function ap_pfdopen;                    external ApacheCore name 'ap_pfdopen';
function ap_pfclose;                    external ApacheCore name 'ap_pfclose';
function ap_psocket;                    external ApacheCore name 'ap_psocket';
function ap_pclosesocket;               external ApacheCore name 'ap_pclosesocket';

// TCP/IP and I/O functions

function ap_get_virthost_addr;          external ApacheCore name 'ap_get_virthost_addr';
function ap_get_local_host;             external ApacheCore name 'ap_get_local_host';
function ap_get_remote_host;            external ApacheCore name 'ap_get_remote_host';
function ap_send_fd;                    external ApacheCore name 'ap_send_fd';
// !!! function ap_send_fd_length;             external ApacheCore name 'ap_send_fd_lentgh';
function ap_send_fb;                    external ApacheCore name 'ap_send_fb';
// !!! function ap_send_fb_length;             external ApacheCore name 'ap_send_fb_lentgh';
function ap_rwrite;                     external ApacheCore name 'ap_rwrite';
function ap_rputc;                      external ApacheCore name 'ap_rputc';
function ap_rputs;                      external ApacheCore name 'ap_rputs';
function ap_rflush;                     external ApacheCore name 'ap_rflush';
function ap_setup_client_block;         external ApacheCore name 'ap_setup_client_block';
function ap_should_client_block;        external ApacheCore name 'ap_should_client_block';
function ap_get_client_block;           external ApacheCore name 'ap_get_client_block';
procedure ap_send_http_header;          external ApacheCore name 'ap_send_http_header';
procedure ap_send_size;                 external ApacheCore name 'ap_send_size';

// Request handling functions

function ap_sub_req_lookup_uri;         external ApacheCore name 'ap_sub_req_lookup_uri';
function ap_sub_req_lookup_file;        external ApacheCore name 'ap_sub_req_lookup_uri';
function ap_run_sub_req;                external ApacheCore name 'ap_run_sub_req';
procedure ap_destroy_sub_req;           external ApacheCore name 'ap_destroy_sub_req';
procedure ap_internal_redirect;         external ApacheCore name 'ap_internal_redirect';
procedure ap_internal_redirect_handler; external ApacheCore name 'ap_internal_redirect_handler';

// Timeout & Alarm functions

procedure ap_hard_timeout;              external ApacheCore name 'ap_hard_timeout';
procedure ap_keepalive_timeout;         external ApacheCore name 'ap_keepalive_timeout';
procedure ap_soft_timeout;              external ApacheCore name 'ap_soft_timeout';
procedure ap_reset_timeout;             external ApacheCore name 'ap_reset_timeout';
procedure ap_kill_timeout;              external ApacheCore name 'ap_kill_timeout';
procedure ap_block_alarms;              external ApacheCore name 'ap_block_alarms';
procedure ap_unblock_alarms;            external ApacheCore name 'ap_unblock_alarms';
procedure ap_check_alarm;               external ApacheCore name 'ap_check_alarm';

// Logging functions

procedure ap_log_error;                 external ApacheCore name 'ap_log_error';
procedure ap_log_rerror;                external ApacheCore name 'ap_log_error';

// URI functions

function ap_parse_uri_components;       external ApacheCore name 'ap_parse_uri_components';
function ap_parse_hostinfo_components;  external ApacheCore name 'ap_parse_hostinfo_components';
function ap_unparse_uri_components;     external ApacheCore name 'ap_unparse_uri_components';
function ap_pgethostbyname;             external ApacheCore name 'ap_pgethostbyname';
function ap_pduphostent;                external ApacheCore name 'ap_pduphostent';

// http_protocol.h

procedure ap_note_auth_failure;         external ApacheCore name 'ap_note_auth_failure';
procedure ap_note_basic_auth_failure;   external ApacheCore name 'ap_note_basic_auth_failure';
procedure ap_note_digest_auth_failure;  external ApacheCore name 'ap_note_digest_auth_failure';

end.
