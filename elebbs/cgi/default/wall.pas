program Wall;
(*
**
** Wall.PAS - Wall like displayer and inputer.
** Originally written as program to test string performance of EleXer.
** Can also be used to display (specially crafted) IRC logs.
**
**
** Created: 30-Jun-2002
** Last update: 08-Jul-2002
**
**
** (c)2002 by Maarten Bekers.
**
*)



const
  MaxLines = 3450;                         { never show more than 1450 lines }
  DBName = 'test';
  TableName = 'ircdata';
  TopicTableName = 'irctopics';

  {-- Include standard type definitions -------------------------------------}
  {$I stdrec.inc}


type
  InfoRec  = record
              TextStr  : String;
              Nick     : String;
              UnixTime : Longint;
              IpAddr   : String;
              Agent    : String;
              IsDeleted: Boolean;
            end; { record }

  {-- Include smiley header files --------------------------------------------}
  {$I ubbrec.inc}

var
  InputStr       : String;
  LastStr        : String;       { Last string entered (used to filter dupls) }
  AddInf         : InfoRec;                        { Last record read/written }
  TmpInf         : InfoRec;
  GoingForward   : Boolean;             { Read all records forward or reverse }
  AllowToAdd     : Boolean;                               { Allow to add text }
  ShowNum        : Integer;                                    { Type of show }
  LinesToShow    : Integer;                         { Lines to show backwards }
  DoAdd          : Boolean;
  IsLocalHost    : Boolean;                   { is this for localhost we run? }
  RefreshSec     : Integer;                    { Number of seconds to refresh }
  LineCount      : Integer;      		{ Total lines actually showed }
  SelStr         : String;
  HighLightStr   : String;                      { Highlight string (optional) }
  BreezerMode    : Boolean;                             { uhm.. never mind :) }

  timing_StartTime : Real;
  timing_Finished  : Real;

  DbHandle       : Integer;                                 { DataBase Handle }
  SqlRes         : Integer;
  
  DoShowDate     : Boolean;                        { show date on irc listing }
  
  logStartDate,
  logStartTime   : String;
  
  logEndDate,
  logEndTime     : String;
  
  logStartStamp,
  logEndStamp    : Longint;
   

  {-- Include header and getuir routines -------------------------------------}
  {$I header.inc}
  {$i getuir.inc}

  {-- Include timing routines ------------------------------------------------}
  {$I timefnc.inc}

  {-- Include smiley header files --------------------------------------------}
  {$I ubbparse.inc}

function Unix2Str(Date, Fmt: Integer): String;
var Year,
    Month,
    Day,
    Hour,
    Min,
    Sec     : Integer;
begin
  Unix2Norm(Date, Year, Month, Day, Hour, Min, Sec);

  Case Fmt of
    0 : Unix2Str := LeadingZero(Hour, 2) + ':' + LeadingZero(Min, 2);
    1 : Unix2Str := LeadingZero(Hour, 2) + ':' + LeadingZero(Min, 2) + ':' +
                    LeadingZero(Sec, 2);
    2 : Unix2Str := LeadingZero(Day, 2) + '-' + LeadingZero(Month, 2) + '-' +
                    LeadingZero(Year, 4);
    else Unix2Str := '??';
  end; { case }
end; { func. Unix2Str }



function IsHighlighted(DataStr: String): Boolean;
var TmpPos  : Integer;
    TermStr : String;
    HighStr : String;
    OnePass : Boolean;
    TwoPass : Boolean;
begin
  {-- Initialize some variables --------------------------------------------}
  IsHighLighted := false;
  OnePass := false;
  TwoPass := false;
  TmpPos := Pos(HighlightStr, DataStr);
  TermStr := ' .,<>[]!?';                         { Characters to break off }

  {-- check if this sentence has been spoken -------------------------------}
  if TmpPos > 0 then
    begin
      {-- Get the actual string --------------------------------------------}
      HighStr := Copy(DataStr, TmpPos, Length(HighLightStr));

      {-- check the first character ----------------------------------------}
      if TmpPos = 1 then Onepass := true
        else begin
               OnePass := Pos(Copy(DataStr, TmpPos - 1, 1), TermStr) > 0;
             end; { if }

      {-- check the first character ----------------------------------------}
      if (TmpPos + Length(HighStr)) >= Length(DataStr) then Twopass := true
        else begin
               TwoPass := Pos(Copy(DataStr, TmpPos + Length(HighStr), 1),
                              TermStr) > 0;
             end; { if }
    end; { if }

  IsHighlighted := (OnePass) AND (TwoPass);
end; { func. IsHighLighted }

function BreezerString(TmpStr: String): String;
var Counter: Integer;
    TmpNew : String;
begin
  TmpNew := '';
  Randomize;

  for Counter := 01 to Length(TmpStr) do
    begin
      if (TmpStr[Counter] = 'g') OR (TmpStr[Counter] = 'l') OR
          (TmpStr[COunter] = 't') then
            begin
              TmpNew := TmpNew + TmpStr[Counter];
            end
              else begin
                    if Random(6) > 1 then
                      TmpNew := TmpNew + UpCase(TmpStr[Counter])
                        else TmpNew := TmpNew + SLowCase(TmpStr[Counter]);
                   end; { else }
    end; { for }

  BreezerString := TmpNew;
end; { func. BreezerString }

procedure OpenDataBase(DbName: String);
begin
  {-- Connect to the database ---------------------------------------------} 
  DbHandle := 0;
  DbHandle := mysql_connect(DbHandle, 'localhost', 'root', 'root');

  {-- select our database -------------------------------------------------}
  if mysql_select_db(DbHandle, DbName) > 0 then
    begin
      WriteLn('Couldn''t select database ', DbName);
      WriteLn(mysql_error(DbHandle));
      halt;
    end; { if }
end; { proc. OpenDataBase } 


procedure CloseDb;
begin
  if mysql_close(DbHandle) < 0 then ;
end; { proc. CloseDb }  


procedure DoShowTable(TableName: String; ChanNr: Integer);
var Error       : Integer;
    DataStr     : String;
    Inf         : InfoRec;
    MyAr        : StringArray;
    RowCount    : Integer;
    DateCriteria: String;
begin
  {-- Open the database ---------------------------------------------------}
  OpenDataBase(DbName);
  
  {-- now run our query ---------------------------------------------------}
  if NOT GoingForward then
    begin
      {-- now start the normal query --------------------------------------}
      if mysql_query(DbHandle, 'SELECT * FROM ' + TableName + ' ' +
                    'WHERE (ChannelNumber=0) ORDER BY id DESC LIMIT ' +
                    FStr(LinesToShow)) < 0 then
        begin
          WriteLn('Error executing query: ', mysql_error(DbHandle), '<br>');
          Halt;
        end; { if }
    end
      else begin
      	     {-- set a date critera ---------------------------------------}
      	     if logStartStamp <> 0 then
      	       begin
      	       	DateCriteria := ' AND TimeStamp < ' + FStr(logEndStamp) + ' AND ' + 
      	       	                'TimeStamp > ' + FStr(LogStartStamp);
      	       end { if }
      	         else DateCriteria := '';
      	       
             {-- first select the topic -----------------------------------}
             if mysql_query(DbHandle, 'SELECT * FROM ' + TopicTableName + ' ' +
                                      'WHERE (ChannelNumber=' + 
                                      FStr(ChanNr) + ');') < 0 then
               begin
                 WriteLn('Error executing query: ', mysql_error(DbHandle), '<br>');
      	         Halt;
               end; { if }
               	                                         
             {-- store the result -----------------------------------------}
             SqlRes := mysql_store_result(DbHandle);
             if mysql_fetch_row(SqlRes, MyAr) < 0 then ;
             
             {-- show the result ------------------------------------------}
             Writeln(MyAr[2], '<hr>');

             {-- get the row count -----------------------------------------}
             if mysql_query(DbHandle, 'SELECT COUNT(*) FROM ' + TableName + ' ' +
                            'WHERE (ChannelNumber= ' + FStr(ChanNr) + DateCriteria + 
                            ');') = 0 then
                begin
                   SqlRes := mysql_store_result(DbHandle); 
                   if mysql_Fetch_row(SqlRes, MyAr) = 0 then ; { now get teh actual count }
                   RowCount := FVal(myAr[0]);
                end; { if }

             if DateCriteria <> '' then { disable max lines if a date range is given }
               LinesToShow := 99999999;
               
              if LinesToShow > RowCount then
                 LinesToShow := RowCount;

             {-- now start the normal query ----------------------------------}
             if mysql_query(DbHandle, 'SELECT * FROM ' + TableName + ' ' +
                             'WHERE (ChannelNumber=' + FStr(ChanNr) +  DateCriteria + 
                             ') ORDER BY id ASC LIMIT ' +  FStr(RowCount - LinesToShow) +
                             ',-1') < 0 then
      	       begin
                 WriteLn('Error executing query: ', mysql_error(DbHandle), '<br>');
      	         Halt;
               end; { if }
           end; { else }

  {-- store the result ------------------------------------------------------}
  SqlRes := mysql_store_result(DbHandle);

  {-- display an error if we cant find this Data-file -----------------------}
  if mysql_num_rows(SqlRes) = 0 then
    begin
      WriteLn('This show number is not available, please try another<br>');
      Error := 99;
    end; { if }
    
  if mysql_num_rows(SqlRes) > 1000 then
    WriteLn('<hr><b>Displaying ', mysql_num_rows(SqlRes), ' rows, this can take a while</b><hr>');

  {-- and actually startup the loop -----------------------------------------}
  if (Error = 0) then
   begin
     {-- Loop through the whole data file -----------------------------------}
     while (mysql_fetch_row(SqlRes, MyAr) = 0) do
       begin
       	 LineCount := LineCount + 1;
       	 
         {-- and display it to the user -------------------------------------}
         if Error = 0 then
          if MyAr[6] = 'N' then                                { not deleted }
           begin
             {-- add the extra info -----------------------------------------}
             DataStr := MyAr[7];

             {-- Add the time -----------------------------------------------}
             if NOT DoShowDate then
               DataStr := '<font color="black">[' + Unix2Str(FVal(MyAr[3]), 0) + '] ' + DataStr
                 else DataStr := '<font color="black">[' + Unix2Str(FVal(MyAr[3]), 2) + ', ' + 
                        Unix2Str(FVal(MyAr[3]), 0) + '] ' + DataStr;

             {-- test for higlighting ---------------------------------------}
             if HighlightStr <> '' then
               begin
                 if IsHighLighted(DataStr) then
                   begin
                     DataStr := '<b>' + DataStr + '</b>';
                   end; { if }
               end; { if }

             {-- automatic breezerize ---------------------------------------}
             if BreezerMode then
               DataStr := BreezerString(DataStr);

             {-- display the actual string ----------------------------------}
             FixSmileys(DataStr);
             Write(DataStr);

             if ShowNum = 0 then
               begin
                 if Trim(MyAr[2]) <> '' then
                   begin
                     Write('<font color="green"> by <b>[</b>', MyAr[2], '<b>]</b></font>');
                   end
                     else Write('<font color="green"> by <b>[</b> (unknown) <b>]</b></font>');
               end; { if }


             if IsLocalHost then
              if ShowNum = 0 then
               begin
                 Write('&nbsp;<img src="/images/profile.gif" alt="');
                 Write('nick: ', MyAr[2], #13);
                 Write('ip: ', MyAr[4], #13);
                 Write('Agent: ', MyAr[5], #13);
                 Write('Date: ', Unix2Str(FVal(MyAr[3]), 2) + ' / ' + Unix2Str(FVal(MyAr[3]), 1));
                 Write('"></img>');
                 Write('&nbsp');
                 Write('<A HREF="/cgi-bin/eleweb.exe?action=3&script=wallnew&num=');
                 Write(FStr(ShowNum), '&act=delete&recnum=');
                 Write( MyAr[0] );
                 Write('"><IMG src="/images/delete.gif" border=0>');
                 Write('</img></a>');
               end; { if }

             WriteLn('<br>');
           end; { if }
       end; { while }
   end; { if }

  {-- and close the database ------------------------------------------------}
  CloseDb;
end; { proc. ShowDataFile }


procedure AddToFile;
const
  SqlSepChar = '''';
  
var DataStr   : String;
begin
  {-- Open the database -----------------------------------------------------}
  OpenDataBase(DbName);

  {-- ubb aprse this string -------------------------------------------------}
  Addinf.TextStr := UbbParse(AddInf.TextStr, AddInf.Nick);

  {-- and add this string ---------------------------------------------------}
  if mysql_query(DbHandle, 'INSERT INTO ' + TableName + ' ' + 
                    '(ChannelNumber,NickName,TimeStamp,IpAddress,AgentString,' +
                    'IsDeleted,MessageLine)' +
                    ' VALUES ' +
                    '(' +
                    SqlSepChar + FStr(0) + SqlSepChar + ',' +
                    SqlSepChar + mysql_escape_string(AddInf.Nick) + SqlSepChar + ',' +
                    SqlSepChar + FStr(AddInf.UnixTime) + SqlSepChar + ',' +
                    SqlSepChar + mysql_escape_string(AddInf.IpAddr) + SqlSepChar + ',' +
                    SqlSepChar + mysql_escape_string(AddInf.Agent) + SqlSepChar + ',' +
                    SqlSepChar + mysql_escape_string('N') + SqlSepChar + ',' +
                    SqlSepChar + mysql_escape_string(AddInf.TextStr) + SqlSepChar + ');') < 0 then
     begin
       WriteLn('Error inserting into DB: ', mysql_error(DbHandle));
       Halt;
     end; { if }

  {-- and close the database ------------------------------------------------}
  CloseDb;
end; { proc. AddToFile }


procedure DeleteRec(RecNr: Integer);
begin
  {-- Open the database -----------------------------------------------------}
  OpenDataBase(DbName);

  {-- and add this string ---------------------------------------------------}
  if mysql_query(DbHandle, 'DELETE FROM ' + TableName + ' ' + 
                    'WHERE (id = ' + FStr(RecNr) + ' AND ChannelNumber = 0);') < 0 then
     begin
       WriteLn('Error inserting into DB: ', mysql_error(DbHandle));
       Halt;
     end; { if }
                        
  
  {-- and close the database ------------------------------------------------}
  CloseDb;
end; { proc. DeleteRec }


begin
  {-- make sure we have something to show -----------------------------------}
  OpenOutput;

  {-- Save the current time -------------------------------------------------}
  timing_Initialize(timing_StartTime);

  {-- Initialize some variables ---------------------------------------------}
  GoingForward := false;
  AllowToAdd := true;
  BreezerMode := false;
  IsLocalHost := GetEnvironment('REMOTE_ADDR') = '127.0.0.1';
  Doshowdate := web_GetFormData('doshowdate') = 'true';

  {-- Initialize smileys table ----------------------------------------------}
  InitTable;
  InitUbbParser;

  {-- get an hilight string -------------------------------------------------}
  HighlightStr := Trim(web_GetFormData('highlight'));

  {-- Get the type of showing -----------------------------------------------}
  LinesToShow := FVal(web_GetFormData('lines'));
  if LinesToShow = 0 then
    begin
      LinesToShow := FVal(web_GetFormData('linesback'));
      
      if LinesToShow = 0 then
        LinesToShow := 20;
    end; { if }
    
  {-- initialize the nick ---------------------------------------------------}
  AddInf.Nick := Trim(EleCodeStr('|F3'));

  {-- Make sure anonymous logons cannot add anything -------------------------}
  if NOT web_IsLoggedOn then
    begin
      AllowToAdd := false;
      AddInf.Nick := '';
    end; { if }

  {-- get the start date and end time ---------------------------------------}
  logStartTime := web_GetFormData('starttime');
  logStartDate := web_GetFormData('startdate');
  logEndTime := web_GetFormData('endtime');
  logEndDate := web_GetFormData('enddate');
  
  if (logStartTime = '') OR (logEndTime = '') OR 
      (logStartDate = '') OR (logStartDate = '') then
        begin
          logStartStamp := 0;
          logEndStamp := 999999999;
        end
          else begin
          	  logStartStamp := Date2Unix(PackTimeStr(logStartDate, logStartTime));
          	  logEndStamp := Date2Unix(PackTimeStr(logEndDate, logEndTime));
               end; { else }

  {-- Get the type of showing -----------------------------------------------}
  ShowNum := FVal(web_GetFormData('num'));
  ReFreshSec := FVal(web_GetFormData('refreshsec'));
  if sLowCase(web_GetFormData('breezermode')) = 'true' then
    BreezerMode := true;

  {-- set the refresh timer -------------------------------------------------}
  if web_GetFormData('dorefresh') = 'true' then
    begin
      {-- fix the refresh seconds for illegal values ------------------------}
      if ReFreshSec = 0 then
        ReFreshSec := 30;

      if RefreshSec < 15 then
        ReFreshSec := 15;

      {-- and make sure we auto refresh -------------------------------------}
      WriteLn('<META http-equiv=REFRESH CONTENT=', ReFreshSec, ';>');
    end; { if }


  {-- we have different levels of what we will do ---------------------------}
  if ShowNum <> 0 then
    begin
      GoingForward := true;
      AllowToAdd := false;
    end
      else begin
              LinesToShow := 0;
           end; { if }

  {-- if we should delete, we delete ----------------------------------------}
  if IsLocalHost then
    begin
      if web_GetFormData('act') = 'delete' then
        begin
          {-- Show something ------------------------------------------------}
          WriteLn('<center><font color="red"><b>Deleted record ',
                  FVal(web_GetFormData('recnum')), '</b></font></center><hr>');

          {-- and actually delete it ----------------------------------------}
          DeleteRec(FVal(web_GetFormData('recnum')));
        end; { web }
    end; { if }

  {-- show a title ----------------------------------------------------------}
  WriteLn('<title>Wall - Powered by EleXer and MySQL</title><br><br>');

  {-- and add a text if necessary -------------------------------------------}
  if AllowToAdd then
    begin
      {-- Generate the output for the input! --------------------------------}
      WriteLn('<form action="/cgi-bin/eleweb.exe?action=3&script=wallnew&doadd=true" method="post">');

      Write('Write on the wall: <input type=text name=input> Nick: <input type=text ');
      WriteLn('name=nick value="' + AddInf.Nick + '"> <input type=submit value="submit">');
      WriteLn('</form>');

      WriteLn('<hr>');
      Write('<center> People before you, wrote... (<a href="/cgi-bin/eleweb.exe?action=3&script=wallnew');
      WriteLn('&nick='+web_ConvertLink(AddInf.Nick)+'">refresh</a>)</center>');
      WriteLn('<hr>');

      {-- Get input string if we should input something ---------------------}
      InputStr := web_EscToNonEsc(web_GetFormData('input'));
    end; { if }

  {-- make sure people dont enter the same thing twice (accidentally) -------}
  if InputStr <> '' then
   if AllowToAdd then
     begin
       AddInf.TextStr := InputStr;

       AddInf.UnixTime := NowAsUnixDate;
       AddInf.IpAddr := GetEnvironment('REMOTE_ADDR');
       AddInf.Agent := GetEnvironment('HTTP_USER_AGENT');
       AddInf.IsDeleted := false;

       if AllowToAdd then
        if web_GetFormData('doadd') = 'true' then
          begin
            DoAdd := true;

            if Trim(AddInf.Nick) = '' then
              begin
                DoAdd := false;
                WriteLn('<center><font color="red"><b>Please enter your nick</b></font></center><hr>');
              end; { if }

            if DoAdd then
              AddToFile;
          end; { if }
     end; { if }

  {-- And now show the datafile ---------------------------------------------}
  timing_Initialize(timing_StartTime);
  DoShowTable(TableName, ShowNum);

  {-- if we dont add --------------------------------------------------------}
  if NOT AllowToAdd then
    begin
      {-- Generate the output for the input! --------------------------------}
      WriteLn('<hr><font color="green">');
      Write('<form action="/cgi-bin/eleweb.exe" method="get">');

      Write('<INPUT name="action" value="3" type=HIDDEN>');
      Write('<INPUT name="script" value="wall" type=HIDDEN>');
      Write('<INPUT name="num" value="', ShowNum, '" type=HIDDEN>');
      WriteLn('<TABLE>');

      {-- wow, this code sucks ----------------------------------------------}
      Write('<TR> <TD> Show lines: </TD>');
      Write('<TD> <SELECT NAME="lines" SIZE=1>');
      if LinesToShow = 5 then SelStr := 'SELECTED';
      Write('   <OPTION VALUE="5" ', SelStr, '>5');
      if LinesToShow = 20 then SelStr := 'SELECTED' else SelStr := '';
      Write('   <OPTION VALUE="20" ', SelStr, '>20');
      if LinesToShow = 50 then SelStr := 'SELECTED' else SelStr := '';
      Write('   <OPTION VALUE="50" ', SelStr, '>50');
      if LinesToShow = 100 then SelStr := 'SELECTED' else SelStr := '';
      Write('   <OPTION VALUE="100" ', SelStr, '>100');
      if LinesToShow = 150 then SelStr := 'SELECTED' else SelStr := '';
      Write('   <OPTION VALUE="150" ', SelStr, '>150');
      Write('</SELECT> </TD> </TR> ');
      Write('<br>');
      Write('<TR> <TD> Auto Refresh each 30 seconds </TD>');
      Write('<TD> <INPUT type="checkbox" name="dorefresh" value="true"');

      if web_GetFormData('dorefresh') = 'true' then
        Write(' CHECKED> </td> </tr> <br>')
         else Write('></td> </tr> <br>');

      Write('<TR> <TD> Alternating CapS and NoCaps </TD>');
      Write('<TD> <INPUT type="checkbox" name="breezermode" value="true"');

      if web_GetFormData('breezermode') = 'true' then
        Write(' CHECKED> </td> </tr> <br>')
         else Write('></td> </tr> <br>');

      Write('<TR> <TD> Add dates to time </TD>');
      Write('<TD> <INPUT type="checkbox" name="doshowdate" value="true"');

      if web_GetFormData('doshowdate') = 'true' then
        Write(' CHECKED> </td> </tr> <br>')
         else Write('></td> </tr> <br>');

      Write('<TR> <TD> Text to highlight: </TD>');
      Write('<TD> <INPUT type=text value="');
      Write(HighLightStr, '" name="highlight"></TD></TR><br>');

      Write('<TR> <TD> <BR> <BR> </TD> </TR>');
      Write('<TR> <TD> Start date (mm-dd-yy): </TD>');
      Write('<TD> <INPUT type=text value="');
      Write(logStartDate, '" name="startdate"></TD></TR><br>');
      Write('<TR> <TD> Start time (hh:mm):    </TD>');
      Write('<TD> <INPUT type=text value="');
      Write(logStartTime, '" name="starttime"></TD></TR><br>');
      Write('<TR> <TD> End   date (mm-dd-yy): </TD>');
      Write('<TD> <INPUT type=text value="');
      Write(logEndDate, '" name="enddate"></TD></TR><br>');
      Write('<TR> <TD> End time (hh:mm):      </TD>');
      Write('<TD> <INPUT type=text value="');
      Write(logEndTime, '" name="endtime"></TD></TR><br>');
     
      Write('<tr><td><input type=submit value="submit"></td></tr>');
      Write('</table>');

      Write('</form>');
    end; { if }

  {-- Add the footer --------------------------------------------------------}
  WriteLn('<hr>');
  WriteLn('<center> <sup> Wall - (c) by ele');
  WriteLn('<center> <sup> Powered by <b>EleXer</b> and MySQL');
  WriteLn('<center> <sup> ', LineCount, ' lines</center>');

  {-- and stop the timer -----------------------------------------------------}
  timing_Done(timing_Finished);
  timing_ShowTimeFooter(timing_StartTime, timing_Finished);
end.
