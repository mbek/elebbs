(*
**
** Auto generated file
** use the "compile_readmsg" script to generate
** a new file instead of editting this file
**
*)


procedure show_html_qrddata_tpl;
var Tmp: String;
begin
  Write(' <TR ALIGN=MIDDLE> <TD ALIGN=L'\
  'EFT class="tab_contents"> <A H'\
  'REF="/cgi-bin/eleweb.exe?actio'\
  'n=3&script=readmsg&sub-action='\
  '1&msgnum=', FStr(mb_GetMsgNumber), '"> ', mb_GetFromWho, ' </A> </TD>'\
  '  <TD class="tab_contents2" al'\
  'ign=left> ', mb_GetToWho, ' </TD>  <TD alig'\
  '=left class="tab_contents2"> ',
  mb_GetSubject,' </TD> </TR> ');;
end; { proc. show_html_qrddata_tpl }


procedure show_html_qrdflonl_tpl;
var Tmp: String;
begin
  Write(' <TR ALIGN=MIDDLE> <TD VALIGN='\
  'TOP class="tab_contents2"> <IM'\
  'G SRC="/images/lockfolder_on.g'\
  'if"></IMG> </TD>  <TD ALIGN=LE'\
  'FT class="frm_msgs_thread"> <A'\
  ' href="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=readmsg&sub-actio'\
  'n=9&areanum=', FStr(MessageInf.Areanum), '&msgnum=', FStr(mb_GetMsgNumber), '"> ',
  mb_GetSubject, ' </A> </TD>   <TD class="frm'\
  '_msgs_lastposter"> ', TopicInf.LastPoster, ' </TD> '\
  '<TD class="frm_msgs_replycount'\
  '"> ', FStr(TopicInf.TotalPosts -1), ' </td>  <TD class="frm_'\
  'sgs_lastpostdate" nowrap> ',  FormatDateString(Unix2Str(TopicInf.LastPostDate, 4), 8, 3), 
'&nbsp;&nbsp;, Unix2Str(TopicInf.LastPostDate, 0), '''\
  '</TD> </TR>  ');;
end; { proc. show_html_qrdflonl_tpl }


procedure show_html_qrdflon_tpl;
var Tmp: String;
begin
  Write(' <TR ALIGN=MIDDLE> <TD VALIGN='\
  'TOP class="tab_contents2"> <IM'\
  'G SRC="/images/folder_on.gif">'\
  '</IMG> </TD>  <TD ALIGN=LEFT c'\
  'lass="frm_msgs_thread"> <A hre'\
  'f="/cgi-bin/eleweb.exe?action='\
  '3&script=readmsg&sub-action=9&'\
  'areanum=', FStr(MessageInf.Areanum), '&msgnum=', FStr(mb_GetMsgNumber), '"> ', mb_GetSubject, ' '\
  '/A> </TD>   <TD class="frm_msg'\
  's_lastposter"> ', TopicInf.LastPoster, ' </TD>  <TD'\
  'class="frm_msgs_replycount"> '\
  'ZK </td>  <TD class="frm_msgs_'\
  'lastpostdate" nowrap> ',  FormatDateString(Unix2Str(TopicInf.LastPostDate, 4), 8, 3), 
'&nbsp;&nbsp;', Unix2Str(TopicInf.LastPostDate, 0), ' </T'\
  '> </TR>  ');;
end; { proc. show_html_qrdflon_tpl }


procedure show_html_qrdflofl_tpl;
var Tmp: String;
begin
  Write(' <TR ALIGN=MIDDLE> <TD VALIGN='\
  'TOP class="tab_contents2"> <IM'\
  'G SRC="/images/lockfolder_off.'\
  'gif"></IMG> </TD>   <TD ALIGN='\
  'LEFT class="frm_msgs_thread"> '\
  '<A href="/cgi-bin/eleweb.exe?a'\
  'ction=3&script=readmsg&sub-act'\
  'ion=9&areanum=', FStr(MessageInf.Areanum), '&msgnum=', FStr(mb_GetMsgNumber), '"'\
  ' ', mb_GetSubject, ' </A> </TD>   <TD class="'\
  'rm_msgs_lastposter"> ', TopicInf.LastPoster, ' </TD'\
  '  <TD class="frm_msgs_replycou'\
  'nt"> ', FStr(TopicInf.TotalPosts -1), ' </td>  <TD class="fr'\
  '_msgs_lastpostdate" nowrap> '\
  'H </TD> </TR>  ');;
end; { proc. show_html_qrdflofl_tpl }


procedure show_html_qrdfloff_tpl;
var Tmp: String;
begin
  Write(' <TR ALIGN=MIDDLE> <TD VALIGN='\
  'TOP class="tab_contents2"> <IM'\
  'G SRC="/images/folder_off.gif"'\
  '></IMG> </TD>  <TD ALIGN=LEFT '\
  'class="frm_msgs_thread"> <A hr'\
  'ef="/cgi-bin/eleweb.exe?action'\
  '=3&script=readmsg&sub-action=9'\
  '&areanum=', FStr(MessageInf.Areanum), '&msgnum=', FStr(mb_GetMsgNumber), '"> ', mb_GetSubject, ''\
  '</A> </TD>   <TD class="frm_ms'\
  'gs_lastposter"> ', TopicInf.LastPoster, ' </TD>  <T'\
  ' class="frm_msgs_replycount"> ',
  FStr(TopicInf.TotalPosts -1), ' </td>  <TD class="frm_msg'\
  '_lastpostdate" nowrap> ',  FormatDateString(Unix2Str(TopicInf.LastPostDate, 4), 8, 3), 
  '&nbsp;&nbsp;', Unix2Str(TopicInf.LastPostDate, 0), ' </'\
  'D> </TR>  ');;
end; { proc. show_html_qrdfloff_tpl }


procedure show_html_qrdhead_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - ', MessageInf.Name, ' - </TITLE> <link re'\
  '="Stylesheet" TYPE="text/css" '\
  'HREF="/eleweb.css"> </HEAD>  <'\
  'BODY>   <TABLE cellSpacing=1 c'\
  'ellPadding=4 width="70%" align'\
  '=center border=0 class="tab_bo'\
  'rder"> <TBODY> <TR ALIGN=MIDDL'\
  'E class="tab_header">  <TD ali'\
  'gn=left width="40%"> From </TD'\
  '>  <TD> To </TD>   <TD> Subjec'\
  't </TD> </TR> ');;
end; { proc. show_html_qrdhead_tpl }


procedure show_html_qrdhd2_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - ', MessageInf.Name, ' - </TITLE> <link re'\
  '="Stylesheet" TYPE="text/css" '\
  'HREF="/eleweb.css"> </HEAD>  <'\
  'BODY>   <TABLE CELLSPACING=0 C'\
  'ELLPADDING=10 WIDTH="100%" BOR'\
  'DER=0> <TBODY> <TR> <TD>   <TA'\
  'BLE CELLSPACING=0 CELLPADDING='\
  '2 WIDTH="100%" ALIGN=CENTER BO'\
  'RDER=0> <TBODY> <TR> <TD> <A H'\
  'REF="/cgi-bin/eleweb.exe?actio'\
  'n=3&script=readmsg&sub-action='\
  '15&areanum=1000">Top</A> > <A '\
  'HREF="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=lstareas&sub-actio'\
  'n=6">Forum list</A> >'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=readmsg&sub-ac'\
  'tion=14&areanum=', FStr(MessageInf.Areanum), '">', MessageInf.Name, '</A> '\
  '/TD> </TR> </TBODY> </TABLE>  '\
  ' </TD> </TR> </TBODY> </TABLE>'\
  '        <TABLE cellSpacing=1 c'\
  'ellPadding=4 width="70%" align'\
  '=center border=0 class="tab_bo'\
  'rder"> <TBODY> <TR ALIGN=MIDDL'\
  'E class="tab_header">  <TD> &n'\
  'bsp; </TD>  <TD align=left wid'\
  'th="40%"> Thread </TD>  <TD> L'\
  'ast poster </TD>   <TD> Replie'\
  's </TD>   <TD noWrap> Last Pos'\
  't </TD>  </TR> ');;
end; { proc. show_html_qrdhd2_tpl }


procedure show_html_qrdfoot_tpl;
var Tmp: String;
begin
  Write('   <TR> <TD class="tab_footer"'\
  '> <A HREF="/cgi-bin/eleweb.exe'\
  '?action=3&script=readmsg&sub-a'\
  'ction=3&areanum=', FStr(MessageInf.Areanum), '">Post new'\
  '/A> </TD>  <TD ALIGN="RIGHT" c'\
  'olspan=2 class="tab_footer"> <'\
  'A HREF="/cgi-bin/eleweb.exe?ac'\
  'tion=3&script=readmsg&msgnum='\
  'ZD">Next 10</A> </TD> </TR>   '\
  '</TBODY> </TABLE>   ');;
end; { proc. show_html_qrdfoot_tpl }


procedure show_html_qrdend_tpl;
var Tmp: String;
begin
  Write('   <TR> <TD class="tab_footer"'\
  ' colSpan=5> <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=rea'\
  'dmsg&sub-action=3&areanum=', FStr(MessageInf.Areanum), ''\
  '>Post new</A> </TD> </TR>   </'\
  'TBODY> </TABLE>   ');;
end; { proc. show_html_qrdend_tpl }


procedure show_html_qrdft2_tpl;
var Tmp: String;
begin
  Write('   <TR class="tab_footer"> <TD'\
  ' class="tab_footer" colSpan=5>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=readmsg&sub-ac'\
  'tion=3&areanum=', FStr(MessageInf.Areanum), '">Post new!'\
  '/A> </TD> </TR>   </TBODY> </T'\
  'ABLE>   <FORM ACTION="/cgi-bin'\
  '/eleweb.exe" METHOD="GET"> <IN'\
  'PUT TYPE="HIDDEN" NAME="action'\
  '" VALUE="3"> <INPUT TYPE="HIDD'\
  'EN" NAME="script" VALUE="readm'\
  'sg"> <INPUT TYPE="HIDDEN" NAME'\
  '="sub-action" VALUE="14"> <INP'\
  'UT TYPE="HIDDEN" NAME="areanum'\
  '" VALUE="', FStr(MessageInf.Areanum), '"> <SELECT NAME="'\
  'aysprune"> <OPTION value="1"  '\
  ' 001>Show topics from last'\
  'day'\
  ' <OPTION value="2"   002>S'\
  'ow topics from last 2 days <OP'\
  'TION value="5"   005>Show '\
  'opics from last 5 days <OPTION'\
  ' value="10"  010>Show topi'\
  's from last 10 days <OPTION va'\
  'lue="20"  020>Show topics '\
  'rom last 20 days <OPTION value'\
  '="30"  030>Show topics fro'\
  ' last 30 days <OPTION value="4'\
  '5"  045>Show topics from l'\
  'st 45 days <OPTION value="60" '\
  ' 060>Show topics from last'\
  '60 days <OPTION value="75"  '\
  'U075>Show topics from last 75 '\
  'days <OPTION value="100" 1'\
  '0>Show topics from last 100 da'\
  'ys'\
  ' <OPTION value="365" 365>S'\
  'ow topics from the last year <'\
  'OPTION value=""    000>Sho'\
  ' all topics </SELECT> <INPUT T'\
  'YPE="SUBMIT" Value="Show"> </F'\
  'ORM>  ');;
end; { proc. show_html_qrdft2_tpl }


procedure show_html_qrdend2_tpl;
var Tmp: String;
begin
  Write('   <TR class="tab_footer"> <TD'\
  ' colSpan=5> <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=rea'\
  'dmsg&sub-action=3&areanum=', FStr(MessageInf.Areanum), ''\
  '>Post new</A> </TD> </TR>   </'\
  'TBODY> </TABLE>   ');;
end; { proc. show_html_qrdend2_tpl }


procedure show_html_stfl_cmn_tpl;
var Tmp: String;
begin
  Write('  <TABLE cellspacing=0 cellpad'\
  'ding=0 border=0> <TBODY>  <TR '\
  'valign=top> <TD align=left cla'\
  'ss="story_table"><img src="/im'\
  'ages/pix.gif" alt="" width=1><'\
  '/TD>  <TD width="100%" class="'\
  'story_table">   <TABLE cellSpa'\
  'cing=0 cellPadding=2 width="10'\
  '0%" border=0> <TBODY> <TR> <TD'\
  ' class="story_table"> <FONT CL'\
  'ASS="story_title"> ', mb_GetSubject, ' </FONT'\
  ' </TD> </TR> </TBODY> </TABLE>'\
  '  </TD>  <TD align=right class'\
  '="story_table"><img src="/imag'\
  'es/pix.gif" alt="" width=1></T'\
  'D> </TR>  </TBODY> </TABLE>   '\
  '  <TABLE cellSpacing=0 cellPad'\
  'ding=0 border=0>'\
  ' <TBODY>  <TR> <TD align=left '\
  'class="story_table"><img src="'\
  '/images/pix.gif" alt="" width='\
  '1></TD>   <TD width="100%" cla'\
  'ss="tab_contents">  <TABLE cel'\
  'lSpacing=0 cellPadding=5 width'\
  '="100%" border=0 class="table_'\
  'contents"> <TBODY> <TR> <TD> <'\
  'FONT class="story_byhdr"> post'\
  'ed by ', mb_GetFromWho, ' on ', FormatDateString(mb_GetDateStr, 8, 3), ' ', mb_GetTimeStr, ' </FONT> '\
  '/TD> </TR> </TBODY> </TABLE>  '\
  '</TD>  <TD align=right class="'\
  'story_table"><img src="/images'\
  '/pix.gif" alt="" width=1></TD>'\
  ' </TR>  <TR> <TD bgColor=#0066'\
  '66 colSpan=3></TD></TR>   <TR>'\
  ' <TD align=right class="story_'\
  'table"><img src="/images/pix.g'\
  'if" alt="" width=1></TD>'\
  '  <TD width="100%" class="stor'\
  'y_text">   <TABLE cellSpacing='\
  '0 cellPadding=5 width="100%" b'\
  'order=0> <TBODY> <TR> <TD clas'\
  's="story_text"></TD> </TR>'\
  '</TBODY> </TABLE> </TD>  <TD a'\
  'lign=right class="story_table"'\
  '><img src="/images/pix.gif" al'\
  't="" width=1></TD> </TR>  <TR '\
  'class="story_table"> <TD colSp'\
  'an=3> <IMG height=1 src="/imag'\
  'es/pix.gif" alt="" width=1></T'\
  'D></TR> </TBODY> </TABLE>     '\
  '<TABLE cellSpacing=0 cellPaddi'\
  'ng=0 border=0> <TBODY> <TR> <T'\
  'D class="story_table"> <IMG he'\
  'ight=11 alt="" src="/images/pi'\
  'x.gif" alt="" width=1>'\
  ' </TD>  <TD width="100%"> <TAB'\
  'LE cellSpacing=0 cellPadding=5'\
  ' width="100%" border=0> <TBODY'\
  '> <TR> <TD class="story_table"'\
  '> <P> <A HREF="/cgi-bin/eleweb'\
  '.exe?action=3&script=readmsg&s'\
  'ub-action=5&msgnum=', FStr(mb_GetMsgNumber), '&areanu'\
  '=', FStr(MessageInf.Areanum), '&fpreply=', FStr(StartMsgNum), '"><IMG SRC="/'\
  'mages/quote.gif" ALT="Reply" B'\
  'ORDER=0>Reply</A> </P> </TD> <'\
  '/TR> </TBODY> </TABLE> </TD>  '\
  ' <TD align=right class="story_'\
  'table"><img src="/images/pix.g'\
  'if" alt="" width=1></TD> </TR>'\
  '   <TR> <TD class="story_table'\
  '" colSpan=3> <IMG height=1 src'\
  '="/images/pix.gif" alt="" widt'\
  'h=1></TD></TR>'\
  '  </TBODY> </TABLE>   <BR> ');;
end; { proc. show_html_stfl_cmn_tpl }


procedure show_html_trdhead_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - </TITLE> <link rel="Styl'\
  'esheet" TYPE="text/css" HREF="'\
  '/eleweb.css"> </HEAD>  <BODY> '\
  '<CENTER>       <TABLE cellSpac'\
  'ing=1 cellPadding=4 width="80%'\
  '" border=0 align=center class='\
  '"tab_border"> <TBODY>  <TR> <T'\
  'D align="center" class="tab_he'\
  'ader"> Threaded message list <'\
  '/TD> </TR>  <TR ALIGN=MIDDLE c'\
  'lass="tab_contents">  <TD alig'\
  'n=left class="tab_contents"> &'\
  'nbsp; </TD>  </TR>   <TR ALIGN'\
  '=MIDDLE>  <TD align=left class'\
  '="tab_contents">'\
  ' ');;
end; { proc. show_html_trdhead_tpl }


procedure show_html_trdfoot_tpl;
var Tmp: String;
begin
  Write('   <TR> <TD class="tab_footer"'\
  ' colSpan=2> <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=rea'\
  'dmsg&msgnum=', FStr(mb_GetMsgNumber), '&sub-action=7"'\
  'Next 100</A> </TD> </TR>   </T'\
  'BODY> </TABLE>  ');;
end; { proc. show_html_trdfoot_tpl }


procedure show_html_trdend_tpl;
var Tmp: String;
begin
  Write('   <TR> <TD class="tab_footer"'\
  ' colspan=2> <BR> </TD> </TR>  '\
  ' </TBODY> </TABLE>    ');;
end; { proc. show_html_trdend_tpl }


procedure show_html_stor_dt_tpl;
var Tmp: String;
begin
  Write('  <TABLE cellspacing=0 cellpad'\
  'ding=0 width="100%" border=0> '\
  '<TBODY>  <TR valign=top> <TD a'\
  'lign=left class="tab_header"><'\
  'img src="/images/pix.gif" alt='\
  '"" width=1></TD>  <TD width="1'\
  '00%" class="tab_header">   <TA'\
  'BLE cellSpacing=0 cellPadding='\
  '2 width="100%" border=0> <TBOD'\
  'Y> <TR> <TD class="tab_header"'\
  '> <FONT class="story_title"> ',
  mb_GetSubject, ' </FONT> </TD> </TR> </TBODY'\
  '> </TABLE>  </TD>  <TD align=r'\
  'ight class="tab_footer"><img s'\
  'rc="/images/pix.gif" alt="" wi'\
  'dth=1></TD> </TR>  </TBODY> </'\
  'TABLE>     <TABLE cellSpacing='\
  '0 cellPadding=0 border=0>'\
  ' <TBODY>  <TR> <TD align=left '\
  'class="tab_header"><img src="/'\
  'images/pix.gif" alt="" width=1'\
  '></TD>   <TD width="100%" clas'\
  's="tab_contents">  <TABLE cell'\
  'Spacing=0 cellPadding=5 width='\
  '"100%" border=0> <TBODY> <TR> '\
  '<TD> <FONT class="story_byhdr"'\
  '> posted by ', mb_GetFromWho, ' on ', FormatDateString(mb_GetDateStr, 8, 3), ' ', mb_GetTimeStr, ' </'\
  'ONT> &nbsp;&nbsp; <BR> </TD> <'\
  '/TR> </TBODY> </TABLE>  </TD> '\
  ' <TD align=right class="tab_he'\
  'ader"><img src="/images/pix.gi'\
  'f" alt="" width=1></TD> </TR> '\
  ' <TR> <TD bgColor=#006666 colS'\
  'pan=3></TD></TR>   <TR> <TD al'\
  'ign=right class="tab_header"><'\
  'img src="/images/pix.gif" alt='\
  '"" width=1></TD>'\
  '  <TD width="100%" class="stor'\
  'y_text">   <TABLE cellSpacing='\
  '0 cellPadding=5 width="100%" b'\
  'order=0> <TBODY> <TR> <TD CLAS'\
  'S="story_text">'); DisplayStoryExcerpt(Tmp); Write(Tmp);
  Write('</TD> </TR>'\
  '</TBODY> </TABLE> </TD>  <TD a'\
  'lign=right class="tab_header">'\
  '<img src="/images/pix.gif" alt'\
  '="" width=1></TD> </TR>  <TR c'\
  'lass="tab_header"> <TD colSpan'\
  '=3> <IMG height=1 src="/images'\
  '/pix.gif" alt="" width=1></TD>'\
  '</TR> </TBODY> </TABLE>     <T'\
  'ABLE cellSpacing=0 cellPadding'\
  '=0 border=0> <TBODY> <TR> <TD '\
  'class="tab_header"> <IMG heigh'\
  't=11 alt="" src="/images/pix.g'\
  'if" alt="" width=1>'\
  ' </TD>  <TD width="100%" class'\
  '="story_text"> <TABLE cellSpac'\
  'ing=0 cellPadding=5 width="100'\
  '%" border=0> <TBODY> <TR> <TD '\
  'class="tab_footer"> <P> <B> (<'\
  '/B> <A HREF="/cgi-bin/eleweb.e'\
  'xe?action=3&script=readmsg&sub'\
  '-action=16&msgnum=', FStr(TopicInf.MsgNum), '&areanum',
  FStr(MessageInf.Areanum), '">Read More</A> | ', FStr(TopicInf.TotalPosts -1), ' repl'\
  'es <B> ) </P> </TD> </TR> </TB'\
  'ODY> </TABLE> </TD>   <TD alig'\
  'n=right class="tab_footer"><im'\
  'g src="/images/pix.gif" alt=""'\
  ' width=1></TD> </TR>   <TR> <T'\
  'D class="tab_footer" colSpan=3'\
  '> <IMG height=1 src="/images/p'\
  'ix.gif" alt="" width=1></TD></'\
  'TR>'\
  '  </TBODY> </TABLE>      <BR><'\
  'BR>  ');
end; { proc. show_html_stor_dt_tpl }


procedure show_html_stfl_dt_tpl;
var Tmp: String;
begin
  Write('  <TABLE cellspacing=0 cellpad'\
  'ding=0 width="100%" border=0> '\
  '<TBODY>  <TR valign=top> <TD a'\
  'lign=left class="tab_header"><'\
  'img src="/images/pix.gif" alt='\
  '"" width=1></TD>  <TD width="1'\
  '00%" class="tab_header">   <TA'\
  'BLE cellSpacing=0 cellPadding='\
  '2 width="100%" border=0> <TBOD'\
  'Y> <TR> <TD class="tab_header"'\
  '> <FONT class="story_title"> ',
  mb_GetSubject, ' </FONT> </TD> </TR> </TBODY'\
  '> </TABLE>  </TD>  <TD align=r'\
  'ight class="tab_footer"><img s'\
  'rc="/images/pix.gif" alt="" wi'\
  'dth=1></TD> </TR>  </TBODY> </'\
  'TABLE>     <TABLE cellSpacing='\
  '0 cellPadding=0 border=0>'\
  ' <TBODY>  <TR> <TD align=left '\
  'class="tab_header"><img src="/'\
  'images/pix.gif" alt="" width=1'\
  '></TD>   <TD width="100%" clas'\
  's="tab_contents">  <TABLE cell'\
  'Spacing=0 cellPadding=5 width='\
  '"100%" border=0> <TBODY> <TR> '\
  '<TD> <FONT class="story_byhdr"'\
  '> posted by ', mb_GetFromWho, ' on ', FormatDateString(mb_GetDateStr, 8, 3), ' ', mb_GetTimeStr, ' </'\
  'ONT> &nbsp;&nbsp; <BR> </TD> <'\
  '/TR> </TBODY> </TABLE>  </TD> '\
  ' <TD align=right class="tab_he'\
  'ader"><img src="/images/pix.gi'\
  'f" alt="" width=1></TD> </TR> '\
  ' <TR> <TD bgColor=#006666 colS'\
  'pan=3></TD></TR>   <TR> <TD al'\
  'ign=right class="tab_header"><'\
  'img src="/images/pix.gif" alt='\
  '"" width=1></TD>'\
  '  <TD width="100%" class="stor'\
  'y_text">   <TABLE cellSpacing='\
  '0 cellPadding=5 width="100%" b'\
  'order=0> <TBODY> <TR> <TD CLAS'\
  'S="story_text">');
  Write('</TD> </TR>'\
  '</TBODY> </TABLE> </TD>  <TD a'\
  'lign=right class="tab_header">'\
  '<img src="/images/pix.gif" alt'\
  '="" width=1></TD> </TR>  <TR c'\
  'lass="tab_header"> <TD colSpan'\
  '=3> <IMG height=1 src="/images'\
  '/pix.gif" alt="" width=1></TD>'\
  '</TR> </TBODY> </TABLE>     <T'\
  'ABLE cellSpacing=0 cellPadding'\
  '=0 border=0> <TBODY> <TR> <TD '\
  'class="tab_header"> <IMG heigh'\
  't=11 alt="" src="/images/pix.g'\
  'if" alt="" width=1>'\
  ' </TD>  <TD width="100%" class'\
  '="story_text"> <TABLE cellSpac'\
  'ing=0 cellPadding=5 width="100'\
  '%" border=0> <TBODY> <TR> <TD '\
  'class="tab_footer"> <P> <FONT '\
  'face="verdana,helvetica,arial"'\
  ' size=2> <A HREF="/cgi-bin/ele'\
  'web.exe?action=3&script=readms'\
  'g&sub-action=5&msgnum=', FStr(mb_GetMsgNumber), '&are'\
  'num=', FStr(MessageInf.Areanum), '&threadreply=', FStr(TopicInf.MsgNum), '"><IMG'\
  'SRC="/images/quote.gif" ALT="R'\
  'eply" BORDER=0>Post new commen'\
  't</A> </FONT> </P> </TD> </TR>'\
  ' </TBODY> </TABLE> </TD>   <TD'\
  ' align=right class="tab_footer'\
  '"><img src="/images/pix.gif" a'\
  'lt="" width=1></TD> </TR>'\
  '   <TR> <TD class="tab_footer"'\
  ' colSpan=3> <IMG height=1 src='\
  '"/images/pix.gif" alt="" width'\
  '=1></TD></TR>  </TBODY> </TABL'\
  'E>      <BR><BR>  ');;
end; { proc. show_html_stfl_dt_tpl }


procedure show_html_stor_hd_tpl;
var Tmp: String;
begin
  Write(' <TABLE cellSpacing=0 cellPadd'\
  'ing=0 width="99%" align=center'\
  ' border=0> <TBODY> <TR> <TD vA'\
  'lign=top width=100 rowSpan=5> '\
  '  <TABLE cellspacing=0 cellpad'\
  'ding=0 width=120 border=0> <TB'\
  'ODY>  <TR> <TD class="story_ta'\
  'ble" colspan=3 align="center">'\
  ' Quick links </TD> </TR>   <TR'\
  '> <TD class="story_table"><img'\
  ' src="/images/pix.gif" width=1'\
  '> </TD>   <TD width="100%">   '\
  '<TABLE cellSpacing=0 cellPaddi'\
  'ng=5 width="100%" border=0> <T'\
  'BODY>  <TR> <TD class="story_t'\
  'able_text"> <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=edi'\
  'tprof">Edit profile</A><BR>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=uinfo&uname='\
  'S">Show profile</A><BR> <A HRE'\
  'F="/cgi-bin/eleweb.exe?action='\
  '2">Logout</A><BR> <HR noShade '\
  'SIZE=1> <B><A HREF="/cgi-bin/e'\
  'leweb.exe?action=3&script=lsta'\
  'reas&sub-action=6">Forums</A><'\
  '/B><BR> <HR noShade SIZE=1> <A'\
  ' HREF="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=listusr">Userlist'\
  '</A><BR> <A HREF="/cgi-bin/ele'\
  'web.exe?action=3&script=liston'\
  'l">Online</A><BR> <A HREF="/cg'\
  'i-bin/eleweb.exe?action=3&scri'\
  'pt=listlast">Last online</A><B'\
  'R> <HR noShade SIZE=1>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=lstareas&sub-a'\
  'ction=1">File categories</A><B'\
  'R> <A HREF="/cgi-bin/eleweb.ex'\
  'e?action=3&script=lstareas&sub'\
  '-action=0">Message categories<'\
  '/A><BR> <HR noShade SIZE=1> <A'\
  ' HREF="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=listfile&sub-acti'\
  'on=1">New files</A><BR> <A HRE'\
  'F="/cgi-bin/eleweb.exe?action='\
  '3&script=wall">The Wall</A><BR'\
  '> </TD> </TR> </TBODY> </TABLE'\
  '>  </TD>  <TD align=right clas'\
  's="story_table"><img src="/ima'\
  'ges/pix.gif" width=1></TD>  </'\
  'TR>  <TR class="story_table">'\
  ' <TD colSpan=3> &nbsp; </TD> <'\
  '/TR>   </TBODY> </TABLE>     <'\
  'P> <BR>  </TD>     <TD> &nbsp;'\
  '&nbsp; </TD>    <TD VALIGN=TOP'\
  ' ALIGN=LEFT>  ');;
end; { proc. show_html_stor_hd_tpl }


procedure show_html_stor_f2_tpl;
var Tmp: String;
begin
  Write('   </FONT> </TD>   <TD> &nbsp;'\
  ' </TD>   <TD vAlign=top align='\
  'middle width=170>   <TABLE cel'\
  'lspacing=0 cellpadding=0 width'\
  '=160 border=0> <TBODY>   <TR> '\
  '<TD class="story_table" colspa'\
  'n=3 align=center> <IMG height='\
  '1 src="/images/pix.gif" width='\
  '1> User </TD> </TR>   <TR> <TD'\
  ' class="story_table"> <IMG hei'\
  'ght=3 alt="" src="/images/pix.'\
  'gif" width=1> </TD>   <TD widt'\
  'h="100%" class="story_text">  '\
  ' <TABLE cellSpacing=0 cellPadd'\
  'ing=5 width="100%" border=0> <'\
  'TBODY>  <TR> <TD class="story_'\
  'table_text"> <FONT CLASS="tab_'\
  'notloggedin">You are not logge'\
  'd in</FONT><BR>'\
  ' <HR size=1 noShade> <FORM ACT'\
  'ION="/cgi-bin/eleweb.exe?actio'\
  'n=1" METHOD="POST"> Username<B'\
  'R> <INPUT NAME="ele_username" '\
  'TYPE="text" value=""><BR> Pass'\
  'word <INPUT NAME="ele_password'\
  '" TYPE="password" value=""><BR'\
  '><BR> <CENTER> <INPUT TYPE="su'\
  'bmit" value="Submit"> <INPUT T'\
  'YPE="reset" value="Reset"> </C'\
  'ENTER> <HR size=1 noShade> <A '\
  'HREF="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=new_one">New user?'\
  ' Click here</A> </FONT> </TD> '\
  '</TR> </TBODY> </TABLE>  </TD>'\
  '  <TD align=right class="story'\
  '_table"> <IMG height=3 alt="" '\
  'src="/images/pix.gif" width=1>'\
  ' </TD>  </TR>  <TR class="stor'\
  'y_table"> <TD colSpan=3> <IMG '\
  'height=1 src="/images/pix.gif"'\
  ' width=1> </TD> </TR>   </TBOD'\
  'Y> </TABLE>      <P> <BR>  </T'\
  'D>  </TR> </TBODY> </TABLE>  <'\
  'P>  </BODY></HTML> ');;
end; { proc. show_html_stor_f2_tpl }


procedure show_html_stor_ft_tpl;
var Tmp: String;
begin
  Write('   </FONT> </TD>   <TD> &nbsp;'\
  ' </TD>   <TD vAlign=top align='\
  'middle width=170>   <TABLE cel'\
  'lspacing=0 cellpadding=0 width'\
  '=160 border=0> <TBODY>  <TR> <'\
  'TD class="story_table" colspan'\
  '=3 align=center> <IMG height=1'\
  ' src="/images/pix.gif" width=1'\
  '> User </TD> </TR>   <TR> <TD '\
  'class="story_table"> <IMG heig'\
  'ht=3 alt="" src="/images/pix.g'\
  'if" width=1> </TD>   <TD width'\
  '="100%">   <TABLE cellSpacing='\
  '0 cellPadding=5 width="100%" b'\
  'order=0> <TBODY>  <TR class="s'\
  'tory_table_text"> <TD> Welcome'\
  ' back,<BR> |FA.<BR> <HR noSha'\
  'e SIZE=1>'\
  ' Last visit: |FF  <BR> Total '\
  'osts: |FM <BR> <HR size=1 noS'\
  'ade> <CENTER></CENTER> </F'\
  'NT> </TD> </TR> </TBODY> </TAB'\
  'LE>  </TD>  <TD align=right cl'\
  'ass="story_table"> <IMG height'\
  '=3 alt="" src="/images/pix.gif'\
  '" width=1> </TD>  </TR>  <TR c'\
  'lass="story_table"> <TD colSpa'\
  'n=3> <IMG height=1 src="/image'\
  's/pix.gif" width=1> </TD> </TR'\
  '>   </TBODY> </TABLE>     <P> '\
  '<BR>  </TD>  </TR> </TBODY> </'\
  'TABLE>  <P>  </BODY></HTML> ');
end; { proc. show_html_stor_ft_tpl }


procedure show_html_rdhead1_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - </TITLE> <link rel="Styl'\
  'esheet" TYPE="text/css" HREF="'\
  '/eleweb.css"> </HEAD>  <BODY> '\
  '     <TABLE cellSpacing=1 cell'\
  'Padding=4 width="80%" border=0'\
  ' align=center class="tab_borde'\
  'r"> <TBODY>  <TR class="tab_he'\
  'ader"> <TD colSpan=3 align="ce'\
  'nter" width="100%"> ', MessageInf.Name, ' - ', FStr(MessageInf.Areanum), ''\
  '</TD> </TR>   <TR> <TD ALIGN=L'\
  'EFT width="30%" class="tab_con'\
  'tents"> Date </FONT> </TD>  <T'\
  'D  colspan=2 class="tab_conten'\
  'ts2"> ', FormatDateString(mb_GetDateStr, 8, 3), ' &nbsp; &nbsp; ', mb_GetTimeStr, ''\
  ' </TD> </TR>  <TR> <TD ALIGN=L'\
  'EFT class="tab_contents"> From'\
  ' </TD>  <TD  colspan=2 class="'\
  'tab_contents2"> ', mb_GetFromWho, ' </TD> </T'\
  '>  <TR> <TD ALIGN=LEFT class="'\
  'tab_contents"> To </TD>  <TD  '\
  'colspan=2 class="tab_contents2'\
  '"> ', mb_GetToWho, ' </TD> </TR>  <TR> <TD '\
  'LIGN=LEFT class="tab_contents"'\
  '> Subject </TD>  <TD colspan=2'\
  ' class="tab_contents2"> ', mb_GetSubject, ' <'\
  'TD> </TR>  <TR class="tab_head'\
  'er"> ');;
end; { proc. show_html_rdhead1_tpl }


procedure show_html_rdhead2_tpl;
var Tmp: String;
begin
  Write(' <TD> Read original message (-'\
  ') </TD> ');;
end; { proc. show_html_rdhead2_tpl }


procedure show_html_rdhead3_tpl;
var Tmp: String;
begin
  Write(' <TD> <A HREF="/cgi-bin/eleweb'\
  '.exe?action=3&script=readmsg&s'\
  'ub-action=1&msgnum=', FStr(mb_GetReplyTo), '"> Read'\
  'original message (-) </A> </TD'\
  '> ');;
end; { proc. show_html_rdhead3_tpl }


procedure show_html_rdhead4_tpl;
var Tmp: String;
begin
  Write(' <TD> Get next reply (+) </TD>'\
  ' ');;
end; { proc. show_html_rdhead4_tpl }


procedure show_html_rdhead5_tpl;
var Tmp: String;
begin
  Write(' <TD> <A HREF="/cgi-bin/eleweb'\
  '.exe?action=3&script=readmsg&s'\
  'ub-action=1&msgnum=', FStr(mb_GetReplyFirst), '"> Get '\
  'ext reply (+) </A> </TD> ');;
end; { proc. show_html_rdhead5_tpl }


procedure show_html_rdhead6_tpl;
var Tmp: String;
begin
  Write(' <TD> Get next in thread (>) <'\
  '/TD> ');;
end; { proc. show_html_rdhead6_tpl }


procedure show_html_rdhead7_tpl;
var Tmp: String;
begin
  Write(' <TD> <A HREF="/cgi-bin/eleweb'\
  '.exe?action=3&script=readmsg&s'\
  'ub-action=1&msgnum=', FStr(mb_GetReplyNext), '"> Get '\
  'ext in thread (>) </A> </TD>  ');;
end; { proc. show_html_rdhead7_tpl }


procedure show_html_rdhead99_tpl;
var Tmp: String;
begin
  Write(' </TR> </TABLE>   <TABLE cellS'\
  'pacing=1 cellPadding=4 width="'\
  '80%" border=0 align=center cla'\
  'ss="tab_contents"> <TBODY>  <T'\
  'R class="tab_header"> <TD alig'\
  'n="center" width="100%"> ', GetAttributesStr, ' '\
  '/TD> </TR>  ');;
end; { proc. show_html_rdhead99_tpl }


procedure show_html_rdtextn_tpl;
var Tmp: String;
begin
  Write(' <TR class="tab_contents">  <T'\
  'D ALIGN="LEFT" WIDTH="100%"> <'\
  'FONT style=txt_normal">  <'\
  'FONT> </TD> </TR> ');;
end; { proc. show_html_rdtextn_tpl }


procedure show_html_rdtextq_tpl;
var Tmp: String;
begin
  Write(' <TR class="tab_contents"> <TD'\
  ' ALIGN="LEFT" WIDTH="100%"> <F'\
  'ONT style="txt_quote">  </'\
  'ONT> </TD> </TR> ');;
end; { proc. show_html_rdtextq_tpl }


procedure show_html_rdtextk_tpl;
var Tmp: String;
begin
  Write(' <TR class="tab_contents"> <TD'\
  ' ALIGN="LEFT" WIDTH="100%"> <f'\
  'ont class="txt_kludge"> </'\
  'ont> </TD> </TR> ');;
end; { proc. show_html_rdtextk_tpl }


procedure show_html_rdtextt_tpl;
var Tmp: String;
begin
  Write(' <TR class="tab_contents"> <TD'\
  ' ALIGN="LEFT" WIDTH="100%"> <F'\
  'ONT style=txt_tear">  </FO'\
  'T> </TD> </TR> ');;
end; { proc. show_html_rdtextt_tpl }


procedure show_html_rdfoot_tpl;
var Tmp: String;
begin
  Write('  <TR> <TD> &nbsp; </TD> </TR>'\
  ' </TABLE>   <TABLE cellSpacing'\
  '=1 cellPadding=4 width="80%" b'\
  'order=0 align=center class="ta'\
  'b_header"> <TBODY> <TR class="'\
  'tab_footer"> <TD> <B>  <A HREF'\
  '="/cgi-bin/eleweb.exe?action=3'\
  '&script=readmsg&sub-action=1&m'\
  'sgnum=', FStr(mb_GetMsgNumber + 1), ',">Next</A> <B> </TD>'\
  ' <TD> <B>  <A HREF="/cgi-bin/e'\
  'leweb.exe?action=3&script=read'\
  'msg&sub-action=1&msgnum=">'\
  'revious</A> <B> </TD>  <TD> <B'\
  '>  <A HREF="/cgi-bin/eleweb.ex'\
  'e?action=3&script=readmsg&sub-'\
  'action=5&msgnum=', FStr(mb_GetMsgNumber), '">Reply</A'\
  ' <B> </TD>  <TD> <B> '\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=readmsg&sub-ac'\
  'tion=3">Enter new message</A> '\
  '<B> </TD>  <TD> <B>  <A HREF="'\
  '/cgi-bin/eleweb.exe?action=3&s'\
  'cript=readmsg&sub-action=2&msg'\
  'num=', FStr(mb_GetMsgNumber), '">Delete</A> <B> </TD>'\
  ' <TD> <B>  <A HREF="/cgi-bin/e'\
  'leweb.exe?action=3&script=read'\
  'msg&sub-action=15&areanum=1000'\
  '">Main menu</A> <B> </TD> </TR'\
  '> </TABLE>  </BODY> </HTML>  ');;
end; { proc. show_html_rdfoot_tpl }


procedure show_html_rderror_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Error reading - </TITLE> <l'\
  'ink rel="Stylesheet" TYPE="tex'\
  't/css" HREF="/eleweb.css"> </H'\
  'EAD>  <BODY>     <TABLE cellSp'\
  'acing=1 cellPadding=4 width="4'\
  '0%" align=center border=0 CLAS'\
  'S="tab_border"> <TBODY> <TR AL'\
  'IGN=MIDDLE>  <TD CLASS="tab_he'\
  'ader"> Error reading this area'\
  ' </TD>  </TR>  <TR ALIGN=LEFT>'\
  ' <TD VALIGN=TOP ALIGN=CENTER C'\
  'LASS="tab_contents"> <BR> Ther'\
  'e was an error occurred readin'\
  'g this area. Perhaps you encou'\
  'ntered the end of this area.<B'\
  'R> <BR><BR> </TD> </TR>'\
  '  <TR ALIGN=MIDDLE>  <TD CLASS'\
  '="tab_footer"> <BR> </TD>  </T'\
  'R>    </TABLE>  </BODY> </HTML'\
  '> ');;
end; { proc. show_html_rderror_tpl }


procedure show_html_rd_flhd_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - ', MessageInf.Name, ' - </TITLE> <link re'\
  '="Stylesheet" TYPE="text/css" '\
  'HREF="/eleweb.css"> </HEAD>  <'\
  'BODY> <CENTER>    <TABLE CELLS'\
  'PACING=0 CELLPADDING=2 WIDTH="'\
  '100%" ALIGN=CENTER BORDER=0 CL'\
  'ASS="frm_tab_top"> <TR> <TD> <'\
  'FONT face="verdana, arial, hel'\
  'vetica" SIZE=2 CLASS="frm_font'\
  '_top"> <B> <A HREF="/cgi-bin/e'\
  'leweb.exe?action=3&script=read'\
  'msg&areanum=1000&sub-action=15'\
  '">Main menu</A> > <A HREF="/cg'\
  'i-bin/eleweb.exe?action=3&scri'\
  'pt=lstareas&sub-action=6">Foru'\
  'm list</A> >'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=readmsg&sub-ac'\
  'tion=14&areanum=', FStr(MessageInf.Areanum), '"> ', MessageInf.Name, ' </A'\
  ' > <A HREF="/cgi-bin/eleweb.ex'\
  'e?action=3&script=readmsg&sub-'\
  'action=9&areanum=', FStr(MessageInf.Areanum), '&msgnum='\
  'D"> ', mb_GetSubject, ' </A> </B> </FONT> </T'\
  '> </TR>  <TR> <TD> &nbsp; </TD'\
  '> </TR> </TABLE>     <TABLE CE'\
  'LLSPACING=0 CELLPADDING=4 WIDT'\
  'H="100%" BORDER=0 CLASS="frm_t'\
  'ab_hdr"> <TR CLASS="frm_td_hdr'\
  '" border=0 height="10"> <TD CO'\
  'LSPAN=2><FONT SIZE="0">&nbsp;<'\
  '/FONT></TD></TR>  <TR> <TD NOW'\
  'RAP WIDTH=175 CLASS="frm_td_hd'\
  'r"> <FONT class="frm_font_hdr"'\
  '>'\
  ' <B> Author </B> </FONT> </TD>'\
  '  <TD WIDTH="100%" CLASS="frm_'\
  'td_hdr"> <FONT class="frm_font'\
  '_hdr"> <B> Topic:&nbsp;&nbsp;&'\
  'nbsp;', mb_GetSubject, ' </B> </FONT> </TD>  '\
  'TR CLASS="frm_td_hdr" border=0'\
  ' height="10"> <TD COLSPAN=2><F'\
  'ONT SIZE="0">&nbsp;</FONT></TD'\
  '></TR>  </TR>  </TABLE>   ');;
end; { proc. show_html_rd_flhd_tpl }


procedure show_html_rd_flhd1_tpl;
var Tmp: String;
begin
  Write('   <TABLE CELLSPACING=0 CELLPA'\
  'DDING=0 WIDTH="100%" CLASS="ta'\
  'b_border" BORDER=0> <TBODY> <T'\
  'R> <TD WIDTH="100%">   <TABLE '\
  'CELLSPACING=0 CELLPADDING=0 WI'\
  'DTH="100%" ALIGN=CENTER BORDER'\
  '=0 CLASS="tab_border"> <TBODY>'\
  ' <TR> <TD>  <TABLE CELLSPACING'\
  '=1 CELLPADDING=4 WIDTH="100%" '\
  'BORDER=0> <TBODY> <TR>  <TD VA'\
  'LIGN=TOP NOWRAP WIDTH=175 clas'\
  's="tab_contents"> <A HREF="/cg'\
  'i-bin/eleweb.exe?action=3&scri'\
  'pt=uinfo&uname=', web_ConvertLink(mb_GetFromWho), '" TARGET="_'\
  'lank" class=cat>', mb_GetFromWho, '</A>  <BR>'\
  '    <BR> </TD>   <TD VALIG'\
  '=TOP WIDTH="100%" class="tab_c'\
  'ontents">'\
  ' <P> <FONT face="verdana, aria'\
  'l, helvetica" SIZE=2>  ');;
end; { proc. show_html_rd_flhd1_tpl }


procedure show_html_rd_flhd2_tpl;
var Tmp: String;
begin
  Write('   <TABLE CELLSPACING=0 CELLPA'\
  'DDING=0 WIDTH="100%" CLASS="ta'\
  'b_border" BORDER=0> <TBODY> <T'\
  'R> <TD WIDTH="100%">   <TABLE '\
  'CELLSPACING=0 CELLPADDING=0 WI'\
  'DTH="100%" ALIGN=CENTER BORDER'\
  '=0 CLASS="tab_border"> <TBODY>'\
  ' <TR> <TD>  <TABLE CELLSPACING'\
  '=1 CELLPADDING=4 WIDTH="100%" '\
  'BORDER=0> <TBODY> <TR>  <TD VA'\
  'LIGN=TOP NOWRAP WIDTH=175 clas'\
  's="tab_contents2"> <A HREF="/c'\
  'gi-bin/eleweb.exe?action=3&scr'\
  'ipt=uinfo&uname=', web_ConvertLink(mb_GetFromWho), '" TARGET="'\
  'blank" class=cat>', mb_GetFromWho, '</A>  <BR'\
  '     <BR> </TD>   <TD VALI'\
  'N=TOP WIDTH="100%" class="tab_'\
  'contents2">'\
  ' <P> <FONT face="verdana, aria'\
  'l, helvetica" SIZE=2>  ');;
end; { proc. show_html_rd_flhd2_tpl }


procedure show_html_rdfltxth_tpl;
var Tmp: String;
begin
  Write(' <TABLE CELLSPACING=1 CELLPADD'\
  'ING=4 WIDTH="100%" BORDER=0> <'\
  'TR VALIGN="bottom"> <TD> <FONT'\
  ' CLASS="frm_byhdr"> By ', mb_GetFromWho, ' '\
  ', ', mb_GetTimeStr, ' &nbsp; ', FormatDateString(mb_GetDateStr, 8, 3), ' </FONT> </TD'\
  ' <TD align=right> <FONT SIZE=1'\
  '> <A HREF="/cgi-bin/eleweb.exe'\
  '?action=3&script=readmsg&sub-a'\
  'ction=11&msgnum=', FStr(mb_GetMsgNumber), '&areanum='\
  'B"><IMG SRC="/images/edit.gif"'\
  ' ALT="Edit" BORDER=0></A> <A H'\
  'REF="/cgi-bin/eleweb.exe?actio'\
  'n=3&script=readmsg&sub-action='\
  '5&msgnum=', FStr(mb_GetMsgNumber), '&areanum=', FStr(MessageInf.Areanum), '&thre'\
  'dreply=', FStr(TopicInf.MsgNum), '"><IMG SRC="/images'\
  'quote.gif" ALT="Reply" BORDER='\
  '0></A>   <A HREF="/cgi-'\
  'in/eleweb.exe?action=3&script='\
  'uinfo&uname=', web_ConvertLink(mb_GetFromWho), '" TARGET="_bla'\
  'k"><IMG SRC="/images/profile.g'\
  'if" ALT="Profile" BORDER=0></A'\
  '>'\
  ' </FONT> </TD> </TR> </TABLE> '\
  '<HR> ');;
end; { proc. show_html_rdfltxth_tpl }


procedure show_html_rdfltxta_tpl;
var Tmp: String;
begin
  Write(' <TABLE CELLSPACING=1 CELLPADD'\
  'ING=4 WIDTH="100%" BORDER=0>  '\
  '<TR VALIGN="bottom"> <TD> <FON'\
  'T class="frm_byhdr"> By ', mb_GetFromWho, ' '\
  'N, ', mb_GetTimeStr, ' &nbsp; ', FormatDateString(mb_GetDateStr, 8, 3), ' </FONT> </T'\
  '> <TD align=right> <A HREF="/c'\
  'gi-bin/eleweb.exe?action=3&scr'\
  'ipt=readmsg&sub-action=13&msgn'\
  'um=', FStr(mb_GetMsgNumber), '&areanum=', FStr(MessageInf.Areanum), '&msgnum=', FStr(TopicInf.MsgNum), ''\
  'unlock=false"><IMG SRC="/image'\
  's/locktopic.gif" ALT="Lock thi'\
  's topic" BORDER=0></A> <A HREF'\
  '="/cgi-bin/eleweb.exe?action=3'\
  '&script=readmsg&sub-action=13&'\
  'msgnum=', FStr(mb_GetMsgNumber), '&areanum=', FStr(MessageInf.Areanum), '&msgnum',
  FStr(TopicInf.MsgNum), '&unlock=true"><IMG SRC="/i'\
  'ages/unlocktopic.gif" ALT="Unl'\
  'ock this topic" BORDER=0></A>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=readmsg&sub-ac'\
  'tion=2&msgnum=', FStr(mb_GetMsgNumber), '&areanum=', FStr(MessageInf.Areanum), ''\
  'threadreply=', FStr(TopicInf.MsgNum), '"><IMG SRC="/i'\
  'ages/delete.gif" ALT="Delete" '\
  'BORDER=0></A> <A HREF="/cgi-bi'\
  'n/eleweb.exe?action=3&script=r'\
  'eadmsg&sub-action=11&msgnum='\
  'D&areanum=', FStr(MessageInf.Areanum), '"><IMG SRC="/ima'\
  'es/edit.gif" ALT="Edit" BORDER'\
  '=0></A> <A HREF="/cgi-bin/elew'\
  'eb.exe?action=3&script=readmsg'\
  '&sub-action=5&msgnum=', FStr(mb_GetMsgNumber), '&area'\
  'um=', FStr(MessageInf.Areanum), '&threadreply=', FStr(TopicInf.MsgNum), '"><IMG '\
  'RC="/images/quote.gif" ALT="Re'\
  'ply" BORDER=0></A>   <A'\
  'HREF="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=uinfo&uname=', web_ConvertLink(mb_GetFromWho), '" '\
  'ARGET="_blank"><IMG SRC="/imag'\
  'es/profile.gif" ALT="Profile" '\
  'BORDER=0></A>'\
  ' </TD> </TR> </TABLE> <HR> ');;
end; { proc. show_html_rdfltxta_tpl }


procedure show_html_rdfltxt1_tpl;
var Tmp: String;
begin
  Write(' <FONT class=frm_normaltxt_odd'\
  '>  </FONT>');;
end; { proc. show_html_rdfltxt1_tpl }


procedure show_html_rd_flft1_tpl;
var Tmp: String;
begin
  Write(' </FONT> </P> </TD> </TR>  </T'\
  'ABLE>  </TD>  </TR> </TBODY>  '\
  ' </TABLE>  </TD>  </TR> </TBOD'\
  'Y> </TABLE>   ');;
end; { proc. show_html_rd_flft1_tpl }


procedure show_html_rdfltxt2_tpl;
var Tmp: String;
begin
  Write(' <font class=frm_normaltxt_non'\
  'odd> </FONT>');;
end; { proc. show_html_rdfltxt2_tpl }


procedure show_html_rd_flft2_tpl;
var Tmp: String;
begin
  Write(' </FONT> </P> </TD> </TR>  </T'\
  'ABLE>  </TD> </TR>   </TBODY> '\
  '  </TABLE>  </TD>  </TR> </TBO'\
  'DY> </TABLE>   ');;
end; { proc. show_html_rd_flft2_tpl }


procedure show_html_rd_flft_tpl;
var Tmp: String;
begin
  Write('   <TABLE CELLSPACING=1 CELLPA'\
  'DDING=4 WIDTH="100%" BORDER=0 '\
  'CLASS="tab_footer"> <TBODY> <T'\
  'R> <TD WIDTH=10> &nbsp; </TD> '\
  ' <TD WIDTH="100%">   <TABLE CE'\
  'LLSPACING=0 CELLPADDING=0 WIDT'\
  'H="100%" ALIGN=CENTER BORDER=0'\
  ' class="TAB_FOOTER"> <TBODY> <'\
  'TR> <TD>  <TABLE CELLSPACING=1'\
  ' CELLPADDING=4 WIDTH="100%" BO'\
  'RDER=0> <TBODY> <TR> <TD NOWRA'\
  'P WIDTH=175> &nbsp; </TD>  <TD'\
  ' WIDTH="100%">  <TABLE CELLSPA'\
  'CING=0 CELLPADDING=0 WIDTH="10'\
  '0%" BORDER=0> <TBODY> <TR> <TD'\
  ' WIDTH="100%" ALIGH="RIGHT"> <'\
  'A HREF="/cgi-bin/eleweb.exe?ac'\
  'tion=3&script=readmsg&sub-acti'\
  'on=5&msgnum=', FStr(mb_GetMsgNumber), '&areanum=', FStr(MessageInf.Areanum), '&t'\
  'readreply=', FStr(TopicInf.MsgNum), '&dontquotetext=t'\
  'ue">Post reply</A>'\
  ' </TD>  </TR> </TBODY> </TABLE'\
  '>  </TD> </TR> </TBODY> </TABL'\
  'E>  </TD> </TR> </TBODY> </TAB'\
  'LE>   </TD>   <TD WIDTH=10> &n'\
  'bsp; </TD>  </TR> </TBODY> </T'\
  'ABLE>   ');;
end; { proc. show_html_rd_flft_tpl }


procedure show_html_stfl_hd_tpl;
var Tmp: String;
begin
  Write(' <TABLE cellSpacing=0 cellPadd'\
  'ing=0 width="99%" align=center'\
  ' border=0> <TBODY> <TR> <TD vA'\
  'lign=top width=100 rowSpan=5> '\
  '  <TABLE cellspacing=0 cellpad'\
  'ding=0 width=120 border=0> <TB'\
  'ODY>  <TR> <TD class="story_ta'\
  'ble" colspan=3 align="center">'\
  ' Quick links </TD> </TR>   <TR'\
  '> <TD class="story_table"><img'\
  ' src="/images/pix.gif" width=1'\
  '> </TD>   <TD width="100%">   '\
  '<TABLE cellSpacing=0 cellPaddi'\
  'ng=5 width="100%" border=0> <T'\
  'BODY>  <TR> <TD class="story_t'\
  'able_text"> <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=edi'\
  'tprof">Edit profile</A><BR>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=uinfo&uname='\
  'S">Show profile</A><BR> <A HRE'\
  'F="/cgi-bin/eleweb.exe?action='\
  '2">Logout</A><BR> <HR noShade '\
  'SIZE=1> <B><A HREF="/cgi-bin/e'\
  'leweb.exe?action=3&script=lsta'\
  'reas&sub-action=6">Forums</A><'\
  '/B><BR> <HR noShade SIZE=1> <A'\
  ' HREF="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=listusr">Userlist'\
  '</A><BR> <A HREF="/cgi-bin/ele'\
  'web.exe?action=3&script=liston'\
  'l">Online</A><BR> <A HREF="/cg'\
  'i-bin/eleweb.exe?action=3&scri'\
  'pt=listlast">Last online</A><B'\
  'R> <HR noShade SIZE=1>'\
  ' <A HREF="/cgi-bin/eleweb.exe?'\
  'action=3&script=lstareas&sub-a'\
  'ction=1">File categories</A><B'\
  'R> <A HREF="/cgi-bin/eleweb.ex'\
  'e?action=3&script=lstareas&sub'\
  '-action=0">Message categories<'\
  '/A><BR> <HR noShade SIZE=1> <A'\
  ' HREF="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=listfile&sub-acti'\
  'on=1">New files</A><BR> <A HRE'\
  'F="/cgi-bin/eleweb.exe?action='\
  '3&script=wall">The Wall</A><BR'\
  '> </TD> </TR> </TBODY> </TABLE'\
  '>  </TD>  <TD align=right clas'\
  's="story_table"><img src="/ima'\
  'ges/pix.gif" width=1></TD>  </'\
  'TR>  <TR class="story_table">'\
  ' <TD colSpan=3> &nbsp; </TD> <'\
  '/TR>   </TBODY> </TABLE>     <'\
  'P> <BR>  </TD>     <TD> &nbsp;'\
  '&nbsp; </TD>    <TD VALIGN=TOP'\
  ' ALIGN=LEFT>  ');;
end; { proc. show_html_stfl_hd_tpl }


procedure show_html_stfl_f2_tpl;
var Tmp: String;
begin
  Write('   </FONT> </TD>   <TD> &nbsp;'\
  ' </TD>   <TD vAlign=top align='\
  'middle width=170>   <TABLE cel'\
  'lspacing=0 cellpadding=0 width'\
  '=160 border=0> <TBODY>   <TR> '\
  '<TD class="story_table" colspa'\
  'n=3 align=center> <IMG height='\
  '1 src="/images/pix.gif" width='\
  '1> User </TD> </TR>   <TR> <TD'\
  ' class="story_table"> <IMG hei'\
  'ght=3 alt="" src="/images/pix.'\
  'gif" width=1> </TD>   <TD widt'\
  'h="100%" class="story_text">  '\
  ' <TABLE cellSpacing=0 cellPadd'\
  'ing=5 width="100%" border=0> <'\
  'TBODY>  <TR class="story_table'\
  '_text"> <TD> <FONT CLASS="tab_'\
  'notloggedin">You are not logge'\
  'd in</FONT><BR>'\
  ' <HR size=1 noShade> <FORM ACT'\
  'ION="/cgi-bin/eleweb.exe?actio'\
  'n=1" METHOD="POST"> Username<B'\
  'R> <INPUT NAME="ele_username" '\
  'TYPE="text" value=""><BR> Pass'\
  'word <INPUT NAME="ele_password'\
  '" TYPE="password" value=""><BR'\
  '><BR> <CENTER> <INPUT TYPE="su'\
  'bmit" value="Submit"> <INPUT T'\
  'YPE="reset" value="Reset"> </C'\
  'ENTER> <HR size=1 noShade> <A '\
  'HREF="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=new_one">New user?'\
  ' Click here</A> </FONT> </TD> '\
  '</TR> </TBODY> </TABLE>  </TD>'\
  '  <TD align=right class="story'\
  '_table"> <IMG height=3 alt="" '\
  'src="/images/pix.gif" width=1>'\
  ' </TD>  </TR>  <TR class="stor'\
  'y_table"> <TD colSpan=3> <IMG '\
  'height=1 src="/images/pix.gif"'\
  ' width=1> </TD> </TR>   </TBOD'\
  'Y> </TABLE>      <P> <BR>  </T'\
  'D>  </TR> </TBODY> </TABLE>  <'\
  'P>  </BODY></HTML> ');;
end; { proc. show_html_stfl_f2_tpl }


procedure show_html_stfl_ft_tpl;
var Tmp: String;
begin
  Write('   </FONT> </TD>   <TD> &nbsp;'\
  ' </TD>   <TD vAlign=top align='\
  'middle width=170>   <TABLE cel'\
  'lspacing=0 cellpadding=0 width'\
  '=160 border=0> <TBODY>  <TR> <'\
  'TD class="story_table" colspan'\
  '=3 align=center> <IMG height=1'\
  ' src="/images/pix.gif" width=1'\
  '> User </TD> </TR>   <TR> <TD '\
  'class="story_table"> <IMG heig'\
  'ht=3 alt="" src="/images/pix.g'\
  'if" width=1> </TD>   <TD width'\
  '="100%">   <TABLE cellSpacing='\
  '0 cellPadding=5 width="100%" b'\
  'order=0> <TBODY>  <TR class="s'\
  'tory_table_text"> <TD> Welcome'\
  ' back,<BR> |FA.<BR> <HR noSha'\
  'e SIZE=1>'\
  ' Last visit: |FF  <BR> Total '\
  'osts: |FM <BR> <HR size=1 noS'\
  'ade> <CENTER></CENTER> </F'\
  'NT> </TD> </TR> </TBODY> </TAB'\
  'LE>  </TD>  <TD align=right cl'\
  'ass="story_table"> <IMG height'\
  '=3 alt="" src="/images/pix.gif'\
  '" width=1> </TD>  </TR>  <TR c'\
  'lass="story_table"> <TD colSpa'\
  'n=3> <IMG height=1 src="/image'\
  's/pix.gif" width=1> </TD> </TR'\
  '>   </TBODY> </TABLE>     <P> '\
  '<BR>  </TD>  </TR> </TBODY> </'\
  'TABLE>  <P>  </BODY></HTML> ');
end; { proc. show_html_stfl_ft_tpl }


procedure show_html_delerr2_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Delete message - </TITLE> <'\
  'link rel="Stylesheet" TYPE="te'\
  'xt/css" HREF="/eleweb.css"> </'\
  'HEAD>  <BODY>     <TABLE cellS'\
  'pacing=1 cellPadding=4 width="'\
  '40%" align=center border=0 CLA'\
  'SS="tab_border"> <TBODY> <TR A'\
  'LIGN=MIDDLE>  <TD CLASS="tab_h'\
  'eader"> Error deleting message'\
  ' </TD>  </TR>  <TR ALIGN=LEFT>'\
  ' <TD VALIGN=TOP ALIGN=CENTER C'\
  'LASS="tab_contents"> <BR> Cann'\
  'ot delete first message of a t'\
  'hread<BR> Click <A HREF="/cgi-'\
  'bin/eleweb.exe?action=3&script'\
  '=readmsg&sub-action=9&areanum=',
  FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to delete in.<BR> <'\
  'BR> </TD> </TR>  <TR ALIGN=MID'\
  'DLE>  <TD CLASS="tab_footer"> '\
  '<BR> </TD>  </TR>    </TABLE> '\
  ' </BODY> </HTML> ');;
end; { proc. show_html_delerr2_tpl }


procedure show_html_delsucc_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Delete message - </TITLE> <'\
  'link rel="Stylesheet" TYPE="te'\
  'xt/css" HREF="/eleweb.css"> </'\
  'HEAD>  <BODY>     <TABLE cellS'\
  'pacing=1 cellPadding=4 width="'\
  '40%" align=center border=0 CLA'\
  'SS="tab_border"> <TBODY> <TR A'\
  'LIGN=MIDDLE>  <TD CLASS="tab_h'\
  'eader"> Message deleted </TD> '\
  ' </TR>  <TR ALIGN=LEFT> <TD VA'\
  'LIGN=TOP ALIGN=CENTER CLASS="t'\
  'ab_contents"> <BR> Message del'\
  'eted succesfully<BR> Click <A '\
  'HREF="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=readmsg&sub-action'\
  '=9&areanum=', FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread<'\
  'BR> <BR> </TD> </TR>  <TR ALIG'\
  'N=MIDDLE>  <TD CLASS="tab_foot'\
  'er"> <BR> </TD>  </TR>    </TA'\
  'BLE>  </BODY> </HTML> ');;
end; { proc. show_html_delsucc_tpl }


procedure show_html_delerror_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Delete message - </TITLE> <'\
  'link rel="Stylesheet" TYPE="te'\
  'xt/css" HREF="/eleweb.css"> </'\
  'HEAD>  <BODY>     <TABLE cellS'\
  'pacing=1 cellPadding=4 width="'\
  '40%" align=center border=0 CLA'\
  'SS="tab_border"> <TBODY> <TR A'\
  'LIGN=MIDDLE>  <TD CLASS="tab_h'\
  'eader"> Error deleting message'\
  ' </TD>  </TR>  <TR ALIGN=LEFT>'\
  ' <TD VALIGN=TOP ALIGN=CENTER C'\
  'LASS="tab_contents"> <BR> Unab'\
  'le to delete specified message'\
  '.<BR> Click <A HREF="/cgi-bin/'\
  'eleweb.exe?action=3&script=rea'\
  'dmsg&sub-action=9&areanum=', FStr(MessageInf.Areanum),
  'msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to delete in.<BR> <'\
  'BR> </TD> </TR>  <TR ALIGN=MID'\
  'DLE>  <TD CLASS="tab_footer"> '\
  '<BR> </TD>  </TR>    </TABLE> '\
  ' </BODY> </HTML> ');;
end; { proc. show_html_delerror_tpl }


procedure show_html_wmsg_one_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>     <FORM '\
  'ACTION="/cgi-bin/eleweb.exe?ac'\
  'tion=3&script=readmsg&sub-acti'\
  'on=4" METHOD="POST">   <TABLE '\
  'cellSpacing=1 cellPadding=4 wi'\
  'dth="80%" align=center border='\
  '0 class="tab_border"> <TBODY> '\
  '<TR ALIGN=MIDDLE class="tab_he'\
  'ader">  <TD COLSPAN=2 class="t'\
  'ab_header"> Posting message </'\
  'TD>  </TR>  <TR ALIGN=LEFT cla'\
  'ss="tab_contents2">'\
  ' <TD VALIGN=TOP COLSPAN=2 ALIG'\
  'N="CENTER"> You are logged in '\
  'as |FA, posting message in "'\
  'A" </TD> </TR>   <TR> <TD ALIG'\
  'N="LEFT" CLASS="tab_contents2"'\
  ' VALIGN="CENTER" WIDTH="30%"> '\
  'From </TD>  <TD ALIGN="LEFT" c'\
  'lass="tab_header"> <INPUT NAME'\
  '="post_fromwho" TYPE="text" SI'\
  'ZE="100%" value=""> </TD> '\
  '/TR>    <TR> <TD ALIGN="LEFT" '\
  'class="tab_contents2" VALIGN="'\
  'CENTER"> To </TD>  <TD ALIGN="'\
  'LEFT" class="tab_header"> <INP'\
  'UT NAME="post_towho" TYPE="tex'\
  't" SIZE="100%" value=""> </TD>'\
  ' </TR>  <TR> <TD ALIGN="LEFT" '\
  'class="tab_contents2" ALIGN="C'\
  'ENTER">'\
  ' Subject </TD>  <TD ALIGN="LEF'\
  'T" class="tab_header"> <INPUT '\
  'NAME="post_subject" TYPE="text'\
  '" SIZE="100%" MAXLENGTH="100">'\
  ' </TD> </TR>  <TR> <TD ALIGN="'\
  'LEFT" class="tab_contents2" VA'\
  'LIGN="CENTER" COLSPAN=2> <INPU'\
  'T NAME="post_private" TYPE="ch'\
  'eckbox" VALUE="TRUE">&nbsp;&nb'\
  'sp;Private </TD>  </TR> </TABL'\
  'E>   <TABLE cellSpacing=1 cell'\
  'Padding=4 width="80%" align=ce'\
  'nter border=0 align="tab_borde'\
  'r"> <TBODY> <TR class="tab_con'\
  'tents2"> <TD ALIGN="CENTER" VA'\
  'LIGN="CENTER"> <TEXTAREA NAME='\
  '"post_msgtext" WRAP="HARD" COL'\
  'S="76" ROWS="18"></TEXTAREA>'\
  ' </TD> </TR>  <TR class="tab_f'\
  'ooter"> <TD colspan=2 align=ce'\
  'nter> <INPUT TYPE="submit" val'\
  'ue="Send"> </TD>  </TABLE>   <'\
  '/TABLE>   </FORM>  </BODY> </H'\
  'TML> ');
end; { proc. show_html_wmsg_one_tpl }


procedure show_html_wmsg_two_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>    <FORM A'\
  'CTION="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=readmsg&sub-actio'\
  'n=6&msgnum=', FStr(StartMsgNum), '" METHOD="POST"'\
  '   <TABLE cellSpacing=1 cellPa'\
  'dding=4 width="80%" align=cent'\
  'er border=0 class="tab_border"'\
  '> <TBODY> <TR ALIGN=MIDDLE cla'\
  'ss="tab_header">  <TD COLSPAN='\
  '2> Replying to message </TD> <'\
  '/TR>  <TR ALIGN=center class="'\
  'tab_contents2">'\
  ' <TD VALIGN=TOP COLSPAN=2> You'\
  ' are logged in as |FA, postin'\
  ' message in "', MessageInf.Name, '" </TD> </TR>'\
  '  <TR class="tab_contents2"> <'\
  'TD ALIGN="LEFT" VALIGN="CENTER'\
  '" WIDTH="30%"> From </TD>  <TD'\
  ' ALIGN="LEFT" class="tab_heade'\
  'r"> <INPUT NAME="post_fromwho"'\
  ' TYPE="text" SIZE="100%" value'\
  '=""> </TD> </TR>    <TR> <'\
  'D ALIGN="LEFT" class="tab_cont'\
  'ents2" VALIGN="CENTER"> To </T'\
  'D>  <TD ALIGN="LEFT" class="ta'\
  'b_header"> <INPUT NAME="post_t'\
  'owho" TYPE="text" SIZE="100%" '\
  'value="', mb_GetFromWho, '" READONLY> </TD> <'\
  'TR>  <TR> <TD ALIGN="LEFT" cla'\
  'ss="tab_contents2"  VALIGN="CE'\
  'NTER">'\
  ' Subject </TD>  <TD ALIGN="LEF'\
  'T" class="tab_header"> <INPUT '\
  'NAME="post_subject" TYPE="text'\
  '" SIZE="100%" VALUE="', mb_GetSubject, '" REA'\
  'ONLY  MAXLENGTH="100"> </TD> <'\
  '/TR>   <TR> <TD ALIGN="LEFT" c'\
  'lass="tab_contents2" VALIGN="C'\
  'ENTER" COLSPAN=2> <INPUT NAME='\
  '"post_private" TYPE="checkbox"'\
  ' VALUE="TRUE">&nbsp;&nbsp;Priv'\
  'ate </TD>  </TR> </TABLE>   <T'\
  'ABLE cellSpacing=1 cellPadding'\
  '=4 width="80%" align=center bo'\
  'rder=0 class="tab_border"> <TB'\
  'ODY> <TR class="tab_contents2"'\
  '> <TD ALIGN="CENTER" VALIGN="C'\
  'ENTER"> <TEXTAREA NAME="post_m'\
  'sgtext" WRAP="HARD" COLS="76" '\
  'ROWS="18"></TEXTAREA'\
  ' </TD> </TR>  <TR  class="tab_'\
  'footer"> <TD colspan=2 align=c'\
  'enter> <INPUT TYPE="submit" va'\
  'lue="Send"> </TD>  </TABLE>   '\
  '</TABLE>   </FORM>  </BODY> </'\
  'HTML>       <INPUT NAME="post_'\
  'reply_replyaddr" TYPE="hidden"'\
  ' VALUE="', reply_ReplyAddr, '"> <INPUT NAME="po'\
  't_reply_replyto" TYPE="hidden"'\
  ' VALUE="', reply_ReplyTo, '"> <INPUT NAME="po'\
  't_reply_replykludge" TYPE="hid'\
  'den" VALUE="', reply_ReplyKludge, '"> <INPUT NAME'\
  '"post_reply_address" TYPE="hid'\
  'den" VALUE="', reply_Address, '"> </TABLE>   '\
  '/FORM>  </BODY> </HTML> ');
end; { proc. show_html_wmsg_two_tpl }


procedure show_html_wmsg_tre_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>   <FORM AC'\
  'TION="/cgi-bin/eleweb.exe" MET'\
  'HOD="POST"> <INPUT NAME="post_'\
  'fromwho" TYPE="HIDDEN" value="'\
  '" READONLY> <INPUT NAME="p'\
  'st_towho" TYPE="HIDDEN" value='\
  '"', mb_GetFromWho, '" READONLY> <INPUT NAME="'\
  'ost_subject" TYPE=HIDDEN value'\
  '="', mb_GetSubject, '" READONLY> <INPUT NAME='\
  'post_private" TYPE="HIDDEN" VA'\
  'LUE="TRUE"> <INPUT NAME="sub-a'\
  'ction" TYPE="HIDDEN" VALUE="12'\
  '">'\
  ' <INPUT NAME="script" TYPE="HI'\
  'DDEN" VALUE="readmsg"> <INPUT '\
  'NAME="msgnum" TYPE="HIDDEN" VA'\
  'LUE="', FStr(StartMsgNum), '"> <INPUT NAME="actio'\
  '" TYPE="HIDDEN" VALUE="3"> <IN'\
  'PUT NAME="areanum" TYPE="HIDDE'\
  'N" VALUE="', FStr(MessageInf.Areanum), '">   <TABLE cell'\
  'pacing=1 cellPadding=4 width="'\
  '70%" align=center border=0 cla'\
  'ss="tab_border"> <TBODY> <TR A'\
  'LIGN=MIDDLE class="tab_header"'\
  '> <TD COLSPAN=2 class="tab_hea'\
  'der"> Change message </TD> </T'\
  'R>  <TR ALIGN=CENTER class="ta'\
  'b_contents2"> <TD COLSPAN=2 > '\
  'You are logged in as |FA, edi'\
  'ting message in "', MessageInf.Name,
  ' </TD> </TR>  <TR> <TD ALIGN="'\
  'CENTER" CLASS="tab_Contents2" '\
  'VALIGN="CENTER" COLSPAN=2> <TE'\
  'XTAREA NAME="post_msgtext" WRA'\
  'P="HARD" ROWS="18" COLS="112">'\
  '</TEXTAREA> </TD> </TR>  <'\
  'R class="tab_footer"> <TD COLS'\
  'PAN=2" align=center> <INPUT TY'\
  'PE="submit" value="Send"> </TD'\
  '> </TR>    </TABLE>  </BODY> <'\
  '/HTML> ');
end; { proc. show_html_wmsg_tre_tpl }


procedure show_html_wmsg_for_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>   <FORM AC'\
  'TION="/cgi-bin/eleweb.exe?acti'\
  'on=3&script=readmsg&sub-action'\
  '=4&areanum=', FStr(MessageInf.Areanum), '" METHOD="POST"'\
  ' <INPUT NAME="post_fromwho" TY'\
  'PE="HIDDEN" value="" READO'\
  'LY> <INPUT NAME="post_towho" T'\
  'YPE="HIDDEN" value="All" READO'\
  'NLY> <INPUT NAME="post_private'\
  '" TYPE="HIDDEN" VALUE="FALSE">'\
  '    <TABLE cellSpacing=1 cellP'\
  'adding=4 width="70%" align=cen'\
  'ter border=0 class="tab_border'\
  '">'\
  ' <TBODY> <TR ALIGN=MIDDLE>  <T'\
  'D colspan=2 class="tab_header"'\
  '> Posting message </TD>  </TR>'\
  '  <TR ALIGN=CENTER class="tab_'\
  'contents2"> <TD colspan=2> You'\
  ' are logged in as |FA, postin'\
  ' message in "', MessageInf.Name, '" </TD> </TR>'\
  '  <TR class="tab_header"> <TD '\
  'ALIGN="LEFT" VALIGN="CENTER" c'\
  'lass="tab_contents2"> Subject '\
  '</TD>  <TD ALIGN="CENTER" clas'\
  's="tab_header"> <INPUT NAME="p'\
  'ost_subject" TYPE="text" SIZE='\
  '"100%" MAXLENGTH="100"> </TD> '\
  '</TR>  <TR class="tab_contents'\
  '"> <TD ALIGN="CENTER" VALIGN="'\
  'CENTER" COLSPAN=2 CLASS="tab_c'\
  'ontents">'\
  ' <TEXTAREA NAME="post_msgtext"'\
  ' WRAP="virtual" ROWS="18" COLS'\
  '="112"></TEXTAREA> </TD> <'\
  'TR>  <TR> <TD ALIGN="CENTER" C'\
  'OLSPAN=2 class="tab_footer"> <'\
  'INPUT TYPE="submit" value="Sen'\
  'd"> </TD> </TR>   </TABLE>    '\
  '</BODY> </HTML> ');
end; { proc. show_html_wmsg_for_tpl }


procedure show_html_wmsg_fiv_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>    <FORM A'\
  'CTION="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=readmsg&sub-actio'\
  'n=6&msgnum=', FStr(StartMsgNum), '&areanum=', FStr(MessageInf.Areanum), '" M'\
  'THOD="POST"> <INPUT NAME="thre'\
  'adreply" TYPE="HIDDEN" value="'\
  '"> <INPUT NAME="post_fromw'\
  'o" TYPE="HIDDEN" value="">'\
  '<INPUT NAME="post_towho" TYPE='\
  '"HIDDEN" value="All"> <INPUT N'\
  'AME="post_subject" TYPE="HIDDE'\
  'N" value="', mb_GetSubject, '" READONLY'\
  '    <TABLE cellSpacing=1 cellP'\
  'adding=4 width="70%" align=cen'\
  'ter border=0 class="tab_border'\
  '"> <TBODY> <TR ALIGN=MIDDLE cl'\
  'ass="tab_header">  <TD COLSPAN'\
  '=2 class="tab_header"> Posting'\
  ' a reply </TD>  </TR>  <TR ALI'\
  'GN=CENTER class="tab_contents2'\
  '"> <TD> <BR> You are logged in'\
  ' as |FA, replying to message '\
  'n "', MessageInf.Name, '" <BR>&nbsp; </TD> </TR'\
  '   <TR> <TD ALIGN="CENTER" VAL'\
  'IGN="CENTER" COLSPAN=2 class="'\
  'tab_contents"> <TEXTAREA NAME='\
  '"post_msgtext" WRAP="virtual" '\
  'ROWS="18" COLS="112"></TEX'\
  'AREA> </TD> </TR>  <TR>'\
  ' <TD ALIGN="CENTER" COLSPAN=2 '\
  'class="tab_footer"> <INPUT TYP'\
  'E="submit" value="Send"> </TD>'\
  ' </TR>  </TABLE>  </BODY> </HT'\
  'ML>  ');
end; { proc. show_html_wmsg_fiv_tpl }


procedure show_html_wmsg_six_tpl;
var Tmp: String;
begin
  Write(' <!DOCTYPE HTML PUBLIC "-//W3C'\
  '//DTD HTML 4.0 Transitional//E'\
  'N"> <HTML> <HEAD>  <TITLE> Ele'\
  'WEB - Write new message - </TI'\
  'TLE> <link rel="Stylesheet" TY'\
  'PE="text/css" HREF="/eleweb.cs'\
  's"> </HEAD>  <BODY>    <FORM A'\
  'CTION="/cgi-bin/eleweb.exe?act'\
  'ion=3&script=readmsg&sub-actio'\
  'n=6&msgnum=', FStr(StartMsgNum), '&areanum=', FStr(MessageInf.Areanum), '" M'\
  'THOD="POST"> <INPUT NAME="thre'\
  'adreply" TYPE="HIDDEN" value="'\
  '"> <INPUT NAME="post_fromw'\
  'o" TYPE="HIDDEN" value="">'\
  '<INPUT NAME="post_towho" TYPE='\
  '"HIDDEN" value="All"> <INPUT N'\
  'AME="fpreply" TYPE="HIDDEN" va'\
  'lue="', web_GetFormData('fpreply'), '"'\
  '    <TABLE cellSpacing=1 cellP'\
  'adding=4 width="70%" align=cen'\
  'ter border=0 class="tab_border'\
  '"> <TBODY> <TR ALIGN=MIDDLE cl'\
  'ass="tab_header">  <TD COLSPAN'\
  '=2 class="tab_header"> Write a'\
  ' new comment </TD>  </TR>   <T'\
  'R ALIGN=center class="tab_cont'\
  'ents2"> <TD colspan=2> You are'\
  ' logged in as |FA, posting a '\
  'ew comment </TD> </TR>   <TR c'\
  'lass="tab_header"> <TD ALIGN="'\
  'LEFT" VALIGN="CENTER" class="t'\
  'ab_contents2"> Subject </TD>  '\
  '<TD ALIGN="CENTER" class="tab_'\
  'header"> <INPUT NAME="post_sub'\
  'ject" TYPE="text" SIZE="100%" '\
  'MAXLENGTH="100">'\
  ' </TD> </TR>  <TR class="tab_c'\
  'ontents"> <TD ALIGN="CENTER" V'\
  'ALIGN="CENTER" COLSPAN=2 CLASS'\
  '="tab_contents"> <TEXTAREA NAM'\
  'E="post_msgtext" WRAP="virtual'\
  '" ROWS="18" COLS="112"></TEXTA'\
  'REA> </TD> </TR>  <TR> <TD ALI'\
  'GN="CENTER" COLSPAN=2 class="t'\
  'ab_footer"> <INPUT TYPE="submi'\
  't" value="Send"> </TD> </TR>  '\
  ' </TABLE>    </BODY> </HTML> ');
end; { proc. show_html_wmsg_six_tpl }


procedure show_html_werror1_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> You do not have sufficient p'\
  'ermissions to post in this are'\
  'a.<BR> Click <A HREF="/cgi-bin'\
  '/eleweb.exe?action=3&script=re'\
  'admsg&sub-action=9&areanum=', FStr(MessageInf.Areanum),
  '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror1_tpl }


procedure show_html_werror2_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> The "From" field is a requir'\
  'ed field<BR> Click <A HREF="/c'\
  'gi-bin/eleweb.exe?action=3&scr'\
  'ipt=readmsg&sub-action=9&arean'\
  'um=', FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror2_tpl }


procedure show_html_werror3_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> The "To" field is a required'\
  ' field<BR> Click <A HREF="/cgi'\
  '-bin/eleweb.exe?action=3&scrip'\
  't=readmsg&sub-action=9&areanum'\
  '=', FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror3_tpl }


procedure show_html_werror4_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> The "Subject" field is a req'\
  'uired field.<BR> Click <A HREF'\
  '="/cgi-bin/eleweb.exe?action=3'\
  '&script=readmsg&sub-action=9&a'\
  'reanum=', FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror4_tpl }


procedure show_html_werror5_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> Messages addressed to "Sysop'\
  '" are not allowed in EchoMail '\
  'areas<BR> Click <A HREF="/cgi-'\
  'bin/eleweb.exe?action=3&script'\
  '=readmsg&sub-action=9&areanum=',
  FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror5_tpl }


procedure show_html_werror6_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> You cannot post to netmail a'\
  'reas from within this EleWEB.<'\
  'BR> Click <A HREF="/cgi-bin/el'\
  'eweb.exe?action=3&script=readm'\
  'sg&sub-action=9&areanum=', FStr(MessageInf.Areanum), '&m'\
  'gnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror6_tpl }


procedure show_html_werror7_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> The entered mail address is '\
  'invalid.<BR> Click <A HREF="/c'\
  'gi-bin/eleweb.exe?action=3&scr'\
  'ipt=readmsg&sub-action=9&arean'\
  'um=', FStr(MessageInf.Areanum), '&msgnum="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror7_tpl }


procedure show_html_werror8_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Editting message </'\
  'TD>  </TR>  <TR ALIGN=LEFT> <T'\
  'D VALIGN=TOP ALIGN=CENTER CLAS'\
  'S="tab_contents"> <BR> Edit of'\
  ' this message failed.<BR> Clic'\
  'k <A HREF="/cgi-bin/eleweb.exe'\
  '?action=3&script=readmsg&sub-a'\
  'ction=9&areanum=', FStr(MessageInf.Areanum), '&msgnum='\
  '">'\
  ' here</A> to return to thread '\
  'you wanted to edit<BR> <BR> </'\
  'TD> </TR>  <TR ALIGN=MIDDLE>  '\
  '<TD CLASS="tab_footer"> <BR> <'\
  '/TD>  </TR>    </TABLE>  </BOD'\
  'Y> </HTML> ');;
end; { proc. show_html_werror8_tpl }


procedure show_html_werror9_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Write New Message - </TITLE'\
  '> <link rel="Stylesheet" TYPE='\
  '"text/css" HREF="/eleweb.css">'\
  ' </HEAD>  <BODY>     <TABLE ce'\
  'llSpacing=1 cellPadding=4 widt'\
  'h="40%" align=center border=0 '\
  'CLASS="tab_border"> <TBODY> <T'\
  'R ALIGN=MIDDLE>  <TD CLASS="ta'\
  'b_header"> Error posting new m'\
  'essage </TD>  </TR>  <TR ALIGN'\
  '=LEFT> <TD VALIGN=TOP ALIGN=CE'\
  'NTER CLASS="tab_contents"> <BR'\
  '> This thread has been closed '\
  'by a moderator and cannot be p'\
  'osted or edited in anymore.<BR'\
  '> Click <A HREF="/cgi-bin/elew'\
  'eb.exe?action=3&script=readmsg'\
  '&sub-action=9&areanum=', FStr(MessageInf.Areanum), '&msg'\
  'um="'\
  ' here</A> to return to thread '\
  'you wanted to post in.<BR> <BR'\
  '> </TD> </TR>  <TR ALIGN=MIDDL'\
  'E>  <TD CLASS="tab_footer"> <B'\
  'R> </TD>  </TR>    </TABLE>  <'\
  '/BODY> </HTML> ');;
end; { proc. show_html_werror9_tpl }


procedure show_html_w2succ_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Message Updated - </TITLE> '\
  '<link rel="Stylesheet" TYPE="t'\
  'ext/css" HREF="/eleweb.css"> <'\
  '/HEAD>  <BODY>     <TABLE cell'\
  'Spacing=1 cellPadding=4 width='\
  '"40%" align=center border=0 CL'\
  'ASS="tab_border"> <TBODY> <TR '\
  'ALIGN=MIDDLE>  <TD CLASS="tab_'\
  'header"> Message </TD>  </TR> '\
  ' <TR ALIGN=LEFT> <TD VALIGN=TO'\
  'P ALIGN=CENTER CLASS="tab_cont'\
  'ents"> <BR> Message updated su'\
  'ccesfully.<BR> Click <A HREF="'\
  '/cgi-bin/eleweb.exe?action=3&s'\
  'cript=readmsg&sub-action=9&are'\
  'anum=', FStr(MessageInf.Areanum), '&msgnum=">here</A'\
  ' to return to the message. <BR'\
  '>&nbsp; </TD> </TR>  <TR ALIGN'\
  '=MIDDLE>  <TD CLASS="tab_foote'\
  'r"> <BR> </TD>  </TR>    </TAB'\
  'LE>  </BODY> </HTML> ');;
end; { proc. show_html_w2succ_tpl }


procedure show_html_wsucc_tpl;
var Tmp: String;
begin
  Write(' <HTML> <HEAD>  <TITLE> EleWEB'\
  ' - Message Updated - </TITLE> '\
  '<link rel="Stylesheet" TYPE="t'\
  'ext/css" HREF="/eleweb.css"> <'\
  '/HEAD>  <BODY>     <TABLE cell'\
  'Spacing=1 cellPadding=4 width='\
  '"40%" align=center border=0 CL'\
  'ASS="tab_border"> <TBODY> <TR '\
  'ALIGN=MIDDLE>  <TD CLASS="tab_'\
  'header"> Message </TD>  </TR> '\
  ' <TR ALIGN=LEFT> <TD VALIGN=TO'\
  'P ALIGN=CENTER CLASS="tab_cont'\
  'ents"> <BR> Your message has b'\
  'een posted.<BR> Click <A HREF='\
  '"/cgi-bin/eleweb.exe?action=3&'\
  'script=readmsg&sub-action=9&ar'\
  'eanum=', FStr(MessageInf.Areanum), '&msgnum=">here</A'\
  ' to return to this message thr'\
  'ead. <BR>&nbsp; </TD> </TR>  <'\
  'TR ALIGN=MIDDLE>  <TD CLASS="t'\
  'ab_footer"> <BR> </TD>  </TR> '\
  '   </TABLE>  </BODY> </HTML> ');;
end; { proc. show_html_wsucc_tpl }


